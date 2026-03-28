-- EVALUATES QUERIES
-- LOAD GRAPHS
-- MATCH PATTERNS
-- JOIN BINDINGS
-- FILTER
-- GROUP
-- AGGREGATE
-- CONSTRUCT OUTPUT GRAPH

{-# LANGUAGE LambdaCase #-}
module Eval
  ( runQuery
  ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import Syntax
import Turtle (loadTurtleGraph)

runQuery :: Query -> IO [Triple]
runQuery q = do
  graphs <- mapM (loadTurtleGraph . (++ ".ttl")) (qSources q)
  let triples = concat graphs
      bindings0 = evalPatterns triples (qPatterns q)
      bindings1 = maybe bindings0 (filterBindings bindings0) (qFilter q)
      bindings2 = applyGroupingAndAggs (qGroupBy q) (qAggs q) bindings1
      out = concatMap (instantiateAll (qConstruct q)) bindings2
  pure out

evalPatterns :: [Triple] -> [Pattern] -> [Binding]
evalPatterns triples pats = foldl step [M.empty] pats
  where
    step bs pat = do
      b <- bs
      t <- triples
      Maybe.maybeToList (matchPattern b pat t)

matchPattern :: Binding -> Pattern -> Triple -> Maybe Binding
matchPattern b (Pattern a1 a2 a3) (Triple n1 n2 n3) = do
  b1 <- matchTerm b a1 n1
  b2 <- matchTerm b1 a2 n2
  matchTerm b2 a3 n3

matchTerm :: Binding -> Term -> Node -> Maybe Binding
matchTerm b (TNode n) actual
  | n == actual = Just b
  | otherwise   = Nothing
matchTerm b (TVar v) actual =
  case M.lookup v b of
    Nothing -> Just (M.insert v actual b)
    Just n  -> if n == actual then Just b else Nothing

filterBindings :: [Binding] -> BoolExpr -> [Binding]
filterBindings bs e = filter (`evalBool` e) bs

evalBool :: Binding -> BoolExpr -> Bool
evalBool b = \case
  BEq e1 e2  -> evalExpr b e1 == evalExpr b e2
  BNe e1 e2  -> evalExpr b e1 /= evalExpr b e2
  BGe e1 e2  -> intCmp (>=) e1 e2
  BGt e1 e2  -> intCmp (>)  e1 e2
  BLt e1 e2  -> intCmp (<)  e1 e2
  BAnd p q   -> evalBool b p && evalBool b q
  BOr p q    -> evalBool b p || evalBool b q
  BNot p     -> not (evalBool b p)
  where
    intCmp op e1 e2 =
      case (evalExpr b e1, evalExpr b e2) of
        (Just (LitInt x), Just (LitInt y)) -> op x y
        _                                  -> False

evalExpr :: Binding -> Expr -> Maybe Node
evalExpr b = \case
  ENode n -> Just n
  EVar v  -> M.lookup v b

applyGroupingAndAggs :: [Var] -> [AggBind] -> [Binding] -> [Binding]
applyGroupingAndAggs [] [] bs = bs
applyGroupingAndAggs gs aggs bs = map (applyAggs aggs) (groupByVars gs bs)

applyAggs :: [AggBind] -> [Binding] -> Binding
applyAggs _ [] = error "Internal error: empty group"
applyAggs aggs grp@(base:_) = foldl (addAgg grp) base aggs
  where
    addAgg grp' acc (AggBind v AggMax e) =
      let vals = [i | b <- grp', Just (LitInt i) <- [evalExpr b e]]
      in M.insert v (LitInt (maximum vals)) acc

    addAgg grp' acc (AggBind v AggCount e) =
      let vals = [() | b <- grp', Just _ <- [evalExpr b e]]
      in M.insert v (LitInt (length vals)) acc

groupByVars :: [Var] -> [Binding] -> [[Binding]]
groupByVars [] bs = map (:[]) bs
groupByVars vs bs =
  let keyOf b = map (lookupMust b) vs
      sorted = L.sortOn keyOf bs
  in L.groupBy (\x y -> keyOf x == keyOf y) sorted

lookupMust :: Binding -> Var -> Node
lookupMust b v =
  case M.lookup v b of
    Just n  -> n
    Nothing -> error ("Missing grouped variable: " ++ show v)

instantiateAll :: [TripleTemplate] -> Binding -> [Triple]
instantiateAll ts b = Maybe.mapMaybe (`instantiateTemplate` b) ts

instantiateTemplate :: TripleTemplate -> Binding -> Maybe Triple
instantiateTemplate (TripleTemplate a1 a2 a3) b = do
  n1 <- termNode a1 b
  n2 <- termNode a2 b
  n3 <- termNode a3 b
  pure (Triple n1 n2 n3)

termNode :: Term -> Binding -> Maybe Node
termNode (TNode n) _ = Just n
termNode (TVar v) b  = M.lookup v b