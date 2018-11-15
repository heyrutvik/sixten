{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Elaboration.Match where

import Protolude

import Data.List.NonEmpty(NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vector
import Data.Vector(Vector)

import qualified Analysis.Simplify as Simplify
import qualified Builtin.Names as Builtin
import Driver.Query
import Effect
import Elaboration.Constraint
import Elaboration.MetaVar
import Elaboration.Monad
import Elaboration.TypeOf
import Syntax
import Syntax.Core
import Syntax.Core.Pattern
import TypedFreeVar
import Util
import VIX

type PatM = Pat CoreM FreeV
-- | An expression possibly containing a pattern-match failure variable
type ExprF = CoreM
type Clause =
  ( [PatM]
  , ExprF
  )

fatBar :: FreeV -> CoreM -> CoreM -> CoreM
fatBar failVar e e' = case filter (== failVar) $ toList e of
  _ | Simplify.duplicable e' -> dup
  [] -> e
  [_] -> dup
  _ -> Simplify.let_
    (const False)
    mempty
    (noSourceLoc "fatBar")
    (Lam mempty Explicit Builtin.UnitType $ abstractNone e')
    (Pi mempty Explicit Builtin.UnitType $ abstractNone $ varType failVar)
    $ abstract1 failVar
    $ substitute failVar (App (pure failVar) Explicit Builtin.MkUnit) e
  where
    dup = substitute failVar e' e

matchSingle
  :: CoreM
  -> PatM
  -> CoreM
  -> CoreM
  -> Elaborate ExprF
matchSingle expr pat innerExpr retType = do
  failVar <- forall "fail" Explicit retType
  result <- withVar failVar $ match failVar retType [expr] [([pat], innerExpr)] innerExpr
  return $ substitute failVar (Builtin.Fail retType) result

matchCase
  :: CoreM
  -> [(PatM, CoreM)]
  -> CoreM
  -> Elaborate ExprF
matchCase expr pats retType = do
  failVar <- forall "fail" Explicit retType
  result <- withVar failVar $ match failVar retType [expr] (first pure <$> pats) (pure failVar)
  return $ substitute failVar (Builtin.Fail retType) result

matchClauses
  :: [CoreM]
  -> [([PatM], CoreM)]
  -> CoreM
  -> Elaborate ExprF
matchClauses exprs pats retType = do
  failVar <- forall "fail" Explicit retType
  result <- withVar failVar $ match failVar retType exprs pats (pure failVar)
  return $ substitute failVar (Builtin.Fail retType) result

type Match
  = FreeV -- ^ Failure variable
  -> ExprF -- ^ Return type
  -> [CoreM] -- ^ Expressions to case on corresponding to the patterns in the clauses (usually variables)
  -> [Clause] -- ^ Clauses
  -> ExprF -- ^ The continuation for pattern match failure
  -> Elaborate ExprF

type NonEmptyMatch
  = FreeV -- ^ Failure variable
  -> ExprF -- ^ Return type
  -> [CoreM] -- ^ Expressions to case on corresponding to the patterns in the clauses (usually variables)
  -> NonEmpty Clause -- ^ Clauses
  -> ExprF -- ^ The continuation for pattern match failure
  -> Elaborate ExprF

-- | Desugar pattern matching clauses
match :: Match
match _ _ _ [] expr0 = return expr0
match failVar _ [] clauses expr0 = return $ foldr go expr0 clauses
  where
    go :: Clause -> ExprF -> ExprF
    go ([], s) x = fatBar failVar s x
    go _ _ = panic "match go"
match failVar retType xs clauses expr0
  = foldrM
    (matchMix failVar retType xs)
    expr0
  $ NonEmpty.groupBy ((==) `on` patternType . firstPattern) clauses

firstPattern :: ([c], b) -> c
firstPattern ([], _) = panic "Match.firstPattern"
firstPattern (c:_, _) = c

matchMix :: NonEmptyMatch
matchMix failVar retType (expr:exprs) clauses@(clause NonEmpty.:| _) expr0
  = f expr failVar retType exprs clauses expr0
  where
    f = case patternType $ firstPattern clause of
      VarPatType -> matchVar
      LitPatType -> matchLit
      ConPatType -> matchCon
      ViewPatType _ -> matchView
matchMix _ _ _ _ _ = panic "matchMix"

matchCon :: CoreM -> NonEmptyMatch
matchCon expr failVar retType exprs clauses expr0 = do
  let (QConstr typeName _) = firstCon $ NonEmpty.head clauses
  cs <- constructors typeName

  cbrs <- forM cs $ \c -> do
    let clausesStartingWithC = NonEmpty.filter ((== c) . firstCon) clauses
    params <- case clausesStartingWithC of
      firstClause:_ -> return $ typeParams $ firstPattern firstClause
      [] -> do
        typ <- typeOf expr
        typ' <- whnf typ
        let (_, params) = appsView typ'
        return $ Vector.fromList params
    ys <- conPatArgs c params

    let exprs' = (pure <$> Vector.toList ys) ++ exprs
    rest <- withVars ys $ match failVar retType exprs' (decon clausesStartingWithC) (pure failVar)
    return $ conBranchTyped c ys rest

  return $ fatBar failVar (Case expr (ConBranches cbrs) retType) expr0
  where
    firstCon (c:_, _) = constr c
    firstCon _ = panic "firstCon "
    typeParams (ConPat _ ps _) = ps
    typeParams _ = panic "match typeParams"
    constr (ConPat c _ _) = c
    constr _ = panic "match constr"
    constructors typeName = do
      def <- fetchDefinition typeName
      case def of
        DataDefinition (DataDef _ cs) _ ->
          return $ QConstr typeName . constrName <$> cs
        _ -> panic $ "constructors: not a data def " <> shower typeName

conPatArgs
  :: QConstr
  -> Vector (Plicitness, CoreM)
  -> Elaborate (Vector FreeV)
conPatArgs c params = do
  ctype <- fetchQConstructor c
  let (tele, _) = pisView ctype
      tele' = instantiatePrefix (snd <$> params) tele
  forTeleWithPrefixM tele' $ \h p s vs ->
    forall h p $ instantiateTele pure vs s

matchLit :: CoreM -> NonEmptyMatch
matchLit expr failVar retType exprs clauses expr0 = do
  let ls = NonEmpty.nub $ lit . firstPattern <$> clauses
  lbrs <- forM ls $ \l -> do
    let clausesStartingWithL = NonEmpty.filter ((== LitPat l) . firstPattern) clauses
    rest <- match failVar retType exprs (decon clausesStartingWithL) (pure failVar)
    return $ LitBranch l rest
  return $ Case expr (LitBranches lbrs expr0) retType
  where
    lit (LitPat l) = l
    lit _ = panic "match lit"

matchVar :: CoreM -> NonEmptyMatch
matchVar expr failVar retType exprs clauses expr0 = do
  let clauses' = go <$> clauses
  match failVar retType exprs (NonEmpty.toList clauses') expr0
  where
    go :: Clause -> Clause
    go (VarPat _ y:ps, e) = do
      let ps' = fmap (first $ substitute y expr) ps
      (ps', substitute y expr e)
    go _ = panic "match var"

matchView :: CoreM -> NonEmptyMatch
matchView expr failVar retType exprs clauses
  = match failVar retType (App f Explicit expr : exprs) $ NonEmpty.toList $ deview <$> clauses
  where
    f = case clauses of
      (ViewPat t _:_, _) NonEmpty.:| _ -> t
      _ -> panic "matchView f"
    deview :: Clause -> Clause
    deview (ViewPat _ p:ps, s) = (p : ps, s)
    deview _ = panic "matchView deview"

decon :: [Clause] -> [Clause]
decon clauses = [(unpat pat <> pats, b) | (pat:pats, b) <- clauses]
  where
    unpat (ConPat _ _ pats) = Vector.toList $ snd3 <$> pats
    unpat (LitPat _) = mempty
    unpat _ = panic "match unpat"
