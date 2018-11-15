{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Elaboration.Constructor where

import Protolude

import qualified Data.HashSet as HashSet
import Data.HashSet(HashSet)
import qualified Data.Text.Prettyprint.Doc as PP

import Driver.Query
import Effect
import Elaboration.Constraint
import Elaboration.Monad
import Syntax
import qualified Syntax.Core as Core
import TypedFreeVar
import Util
import VIX

resolveConstr
  :: HashSet QConstr
  -> Maybe Rhotype
  -> Elaborate QConstr
resolveConstr cs expected = do
  mExpectedTypeName <- expectedDataType

  let candidates
        = maybe
          cs
          (\e -> HashSet.filter ((== e) . qconstrTypeName) cs)
          mExpectedTypeName

  case (HashSet.toList candidates, mExpectedTypeName) of
    ([], Just expectedTypeName) ->
      err cs "Undefined constructor"
        [ dullGreen (pretty expectedTypeName)
        PP.<+> "doesn't define the constructor"
        PP.<+> constrDoc <> "."
        ]
    ([x], _) -> return x
    (xs, _) -> err candidates "Ambiguous constructor"
      [ "Unable to determine which constructor" PP.<+> constrDoc PP.<+> "refers to."
      , "Possible candidates:"
      PP.<+> prettyHumanList "and" (dullGreen . pretty <$> xs)
      <> "."
      ]
  where
    expectedDataType = join <$> traverse findExpectedDataType expected
    findExpectedDataType :: CoreM -> Elaborate (Maybe QName)
    findExpectedDataType typ = do
      typ' <- whnf typ
      case typ' of
        Core.Pi h p t s -> do
          v <- freeVar h p t
          findExpectedDataType $ Util.instantiate1 (pure v) s
        Core.App t1 _ _ -> findExpectedDataType t1
        Core.Global v -> do
          d <- fetchDefinition v
          return $ case d of
            DataDefinition _ _ -> Just v
            _ -> Nothing
        _ -> return Nothing
    err candidates heading docs = do
      reportLocated $ heading <> PP.line <> PP.vcat docs
      -- Assume it's the first candidate to be able to keep going
      return $ case HashSet.toList candidates of
        qc:_ -> qc
        _ -> panic "resolveConstr: empty constr list"
    constrDoc = case HashSet.toList cs of
      QConstr _ cname:_ -> red (pretty cname)
      _ -> panic "resolveConstr no constrs"

