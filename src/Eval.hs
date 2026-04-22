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
import Turtle (loadTurtleGr)

runQuery :: Query -> IO [Triple]
runQuery q = do
  env <- ldGrphs (qSources q)
  let bindings0 = evalPat env (qPatterns q)
      bindings1 = maybe bindings0 (filterBindings env bindings0) (qFilter q)
      bindings2 = applyGroupingNAggs (qGroupBy q) (qAggs q) bindings1
      out = concatMap (instantiateEverything (qConstruct q)) bindings2
  pure out

evalPat :: GrphEnv -> [Pattern] -> [Binding]
evalPat env pats = foldl step [M.empty] pats
  where
    step bs pat = do
      b <- bs
      t <- triplesForPttrn env pat
      Maybe.maybeToList (matchPat b pat t)

triplesForPttrn :: GrphEnv -> Pattern -> [Triple]
triplesForPttrn env pat =
  case patSource pat of
    Nothing -> concat (M.elems env)
    Just alias -> M.findWithDefault [] alias env

matchPat :: Binding -> Pattern -> Triple -> Maybe Binding
matchPat b pat (Triple n1 n2 n3) = do
  b1 <- matchTerm b (patSubj pat) n1
  b2 <- matchTerm b1 (patPred pat) n2
  matchTerm b2 (patObj pat) n3

matchTerm :: Binding -> Term -> Node -> Maybe Binding
matchTerm b (TNode n) actual
  | n == actual = Just b
  | otherwise   = Nothing
matchTerm b (TVar v) actual =
  case M.lookup v b of
    Nothing -> Just (M.insert v actual b)
    Just n  -> if n == actual then Just b else Nothing

filterBindings :: GrphEnv -> [Binding] -> BoolExpr -> [Binding]
filterBindings env bs e = filter (\b -> evalBool env b e) bs

evalBool :: GrphEnv -> Binding -> BoolExpr -> Bool
evalBool env b = \case
  BEq e1 e2  -> evalExpr b e1 == evalExpr b e2
  BNe e1 e2  -> evalExpr b e1 /= evalExpr b e2
  BGe e1 e2  -> intCmp (>=) e1 e2
  BGt e1 e2  -> intCmp (>)  e1 e2
  BLt e1 e2  -> intCmp (<)  e1 e2
  BAnd p q   -> evalBool env b p && evalBool env b q
  BOr p q    -> evalBool env b p || evalBool env b q
  BNot p     -> not (evalBool env b p)
  BExists a p -> existsMch env b a p
  where
    intCmp op e1 e2 =
      case (evalExpr b e1, evalExpr b e2) of
        (Just (LitInt x), Just (LitInt y)) -> op x y
        _                                  -> False

existsMch :: GrphEnv -> Binding -> SrcAlias -> Pattern -> Bool
existsMch env b alias pat =
  any (mchsWBinding b pat) triples
  where
   triples = M.findWithDefault [] alias env

mchsWBinding :: Binding -> Pattern -> Triple -> Bool
mchsWBinding b pat (Triple n1 n2 n3) =
  case matchTerm b (patSubj pat) n1 of
    Nothing -> False
    Just b1 ->
      case matchTerm b1 (patPred pat) n2 of
        Nothing -> False
        Just b2 ->
          case matchTerm b2 (patObj pat) n3 of
            Nothing -> False
            Just _ -> True

        
evalExpr :: Binding -> Expr -> Maybe Node
evalExpr b = \case
  ENode n -> Just n
  EVar v  -> M.lookup v b

applyGroupingNAggs :: [Var] -> [AggBind] -> [Binding] -> [Binding]
applyGroupingNAggs [] [] bs = bs
applyGroupingNAggs gs aggs bs = map (applyAggs aggs) (groupByVars gs bs)

applyAggs :: [AggBind] -> [Binding] -> Binding
applyAggs _ [] = error "Internal error: empty group"
applyAggs aggs grp@(base:_) = foldl (addAgg grp) base aggs
  where
    addAgg grp' acc (AggBind v AggMax e) =
      let vals = [i | b <- grp', Just (LitInt i) <- [evalExpr b e]]
      in if null vals
       then acc
       else M.insert v (LitInt (maximum vals)) acc

    addAgg grp' acc (AggBind v AggMin e) =
      let vals = [i | b <- grp', Just (LitInt i) <- [evalExpr b e]]
      in if null vals
       then acc
       else M.insert v (LitInt (minimum vals)) acc

    addAgg grp' acc (AggBind v AggCount e) =
      let count = length [() | b <- grp', Just _ <- [evalExpr b e]]
      in M.insert v (LitInt count) acc

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

instantiateEverything :: [TripleTemplate] -> Binding -> [Triple]
instantiateEverything ts b = Maybe.mapMaybe (`instantiateTemplate` b) ts

instantiateTemplate :: TripleTemplate -> Binding -> Maybe Triple
instantiateTemplate (TripleTemplate a1 a2 a3) b = do
  n1 <- termNode a1 b
  n2 <- termNode a2 b
  n3 <- termNode a3 b
  pure (Triple n1 n2 n3)


type GrphEnv = M.Map SrcAlias [Triple]

ldGrphs :: [SrcSpec] -> IO GrphEnv
ldGrphs specs = do
  pairs <- mapM loadOne specs
  pure (M.fromList pairs)
  where 
    loadOne (SrcSpec file alias) = do
      triples <- loadTurtleGr (file ++ ".ttl")
      pure (alias, triples)

termNode :: Term -> Binding -> Maybe Node
termNode (TNode n) _ = Just n
termNode (TVar v) b  = M.lookup v b