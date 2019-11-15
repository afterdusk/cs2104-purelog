module Engine where

import AST
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Debug.Trace 

-- Goal is made of subs and list of relations
data Goal = Goal Subs [Rel] deriving(Show)
-- Tree is made of goal and list of subtrees
data Tree = Tree Goal [Tree] Bool deriving (Show)
 {- this represent the resolution tree -}
type Subs = M.Map String Term
 {- this represents a substitution -}
data Result = Success Subs | Failure deriving (Show)

{- an initial resolution tree -}
initTree :: Rel -> Tree
-- initialise tree with empty subs and relations
initTree rel = Tree (Goal M.empty [rel]) [] False

{- unifying two terms -}
unify :: Term -> Term -> Subs -> Maybe Subs
unify (Atom a1) (Atom a2) subs = if a1 == a2 then Just subs else Nothing
--  returns list of subs after unification
unify (Func n1 t1) (Func n2 t2) subs =
  if n1 == n2
    --  (zipWith unify t1 t2): unifies list of rels t1 and t2
    --  (>=>): Just => List transformation
    -- foldl <reducer> <accumulated> <to-reduce>
    -- foldl <(>=>)> <Just (zipWith unify t1 t2)> <subs>
    -- reduction: \subs -> (Just (zipWith unify t1 t2)) >=> subs
    then foldl (>=>) Just (zipWith unify t1 t2) subs
    else Nothing

unify (Var v) term subs =
  -- (subs M.!? v) finds v in the map subs
  case subs M.!? v of
    Just term' -> unify term' term subs -- if term' found, unify term' with term
    Nothing -> Just (M.insert v term subs) -- if Nothing, insert v=term into subs
unify term (var@(Var v)) subs = unify var term subs
unify _ _ _ = Nothing

{- renaming a clause -}
rename :: Rule -> Int -> Rule
-- note the double map: first map to iterate over all rel conjuctions in body, second to iterate over
-- all disjunctions of rels in each conjunction
rename (Rule h body) height = Rule (renameRel h) (map (map renameRel) body)
  where
    -- Rel => Functor => rename... => Rel
    renameRel rel@(Rel _ _) = (toRel . renameTerm . toFunctor) rel
    renameRel Cut = Cut
    -- toRel :: Func -> Rel
    toRel (Func name terms) = Rel name terms
    -- renamed Var is its string + height
    renameTerm (Var v)            = Var (v ++ show height)
    -- renamed Func has its terms renamed
    renameTerm (Func name terms)  = Func name (map renameTerm terms)
    -- renamed atom is atom
    renameTerm atom               = atom

toFunctor :: Rel -> Term
toFunctor (Rel name terms) = Func name terms

match :: Rel -> Rule -> Bool
match (Rel name terms) (Rule (Rel name' terms') _) =
  name == name' && length terms == length terms'

  -- data Goal = Goal Subs [Rel]
  -- data Tree = Tree Goal [Tree] Bool
  -- data Rule = Rule Rel [[Rel]] deriving Show

  -- data Rel = Rel String [Term] 
  --        | Cut deriving Show

  -- type Program = Program [Rule] deriving Show

searchAll :: Program -> Tree -> [Subs]
searchAll (Program rules) tree =
  let searchResult = map fst (search tree 0)
      nonFailure = [ x | Success x <- searchResult ]
        -- foldr (\x lst-> case x of {Success k -> k:lst; Failure -> lst}) [] searchResult
  in nonFailure
  
  where
    
    -- input: matching rule
    -- output: singleton list containing (new tree generated from matching rule, number of cuts)
    expand:: Rule -> Int -> [Rel] -> Subs -> [(Tree, Int)]
    expand rule height rels@(headRels:tailRels) subs =
      -- rename matching rules
      let renamedRule@(Rule renamedHead renamedRels) = rename rule height
          -- get new goal rels, which are query rels + renamed matching rels
          -- assume singleton relationships by using concat -- TODO: is this correct?
          newGoal = (concat renamedRels) ++ tailRels 
          -- unify with head to get new subs
          maybeSubs = trace ((show headRels) ++ (show renamedHead)) (unify (toFunctor headRels) (toFunctor renamedHead) subs)
          -- get the number of cuts
          cutCount = countCuts renamedRels
      -- create child trees
      in case maybeSubs of
          Just newSubs -> {--trace (show (Goal newSubs newGoal))--} ([(Tree (Goal newSubs newGoal) [] False, cutCount)])
          Nothing -> [] 

    countCuts rels = foldr (\x y -> 
      case x of 
        Cut -> 1 + y
        Rel _ _ -> 0 + y) 0 (join rels)

    -- referenced from: https://stackoverflow.com/questions/22472536/does-haskell-have-a-takeuntil-function
    takeUntil :: (a -> Bool) -> [a] -> [a]
    takeUntil _ [] = []
    takeUntil cond (x:xs) 
      | cond x = x : takeUntil cond xs
      | otherwise = [x]

    -- hasCutInRel [] = False
    -- hasCutInRel (rel:rels) = 
    --   case rel of
    --     Cut -> True
    --     Rel _ _ -> hasCutInRel rels 

    -- input: tree node
    -- output: list of (Result, cutsPassed) 
    search:: Tree -> Int -> [(Result, Int)]
    search tree height =
      case tree of
        (Tree (Goal subs []) trees _) -> 
          [(Success subs,0)]
        -- (Tree (Goal subs rels) trees True) -> do
        --   childTree <- trees
        --   search childTree (height + 1)
        (Tree (Goal subs rels@(headRels:tailRels)) _ False) ->
          -- -- unpack list of child trees
          -- expandedTree <-
            case headRels of
              (Rel _ _) -> trace ("searching: " ++ show rels) (
                -- get all matching rules
                let matchingRules = trace ("matchingRules: " ++ (show $ filter (match headRels) rules)) (filter (match headRels) rules)
                    childTrees = concatMap (\rule -> expand rule height rels subs) matchingRules
                    -- cutCounts = trace ("cutCounts: " ++ (show $ map countCuts matchingRules)) (map countCuts matchingRules)
                    searchTree (tree, cutCount) = (map (\pairedSubs -> (pairedSubs, cutCount)) (search tree (height + 1)))
                    childSubs = map searchTree childTrees
                    isNotPruned [] = True
                    isNotPruned (((subs, cutsPassed), cutCount):lst) = trace (show height ++ " isNotPruned: " ++ (show cutsPassed) ++ " " ++ (show cutCount)) (cutsPassed == 0 && isNotPruned lst) 
                    unprunedChildSubs = join (takeUntil isNotPruned childSubs) -- (zip childSubs cutCounts)
                    adjustedChildSubs = trace (show height ++ " unprunedChildSubs: " ++ show unprunedChildSubs) (map (\((subs, cutsPassed), cutCount) -> (subs, max 0 (cutsPassed - cutCount))) unprunedChildSubs)

                -- merge lists of subs as they each indicate successful mappings
                in case childTrees of
                  [] -> [(Failure, 0)]
                  _ -> trace (show height ++ " adjustedChildSubs: " ++ show adjustedChildSubs) adjustedChildSubs)
                -- rule@(Rule matchingHead matchingRels) <- filter (match headRels) rules
      
              Cut -> 
                let childTree = Tree (Goal subs tailRels) [] False
                    childSubs = search childTree (height + 1)
                    adjustedChildSubs = map (\(childSub, cutsPassed) -> (childSub, cutsPassed + 1)) childSubs
                in trace (show height ++ " cut adjustedChildSubs: " ++ show adjustedChildSubs) adjustedChildSubs

              --   -- get all matching rules
              --   rule@(Rule matchingHead matchingRels) <- filter (match headRels) rules
              --   -- rename matching rules
              --   let renamedRule@(Rule renamedHead renamedRels) = rename rule height
              --   -- get new goal rels, which are query rels + renamed matching rels
              --   -- assume singleton relationships by using concat -- TODO: is this correct?
              --   let newGoal = (concat renamedRels) ++ tailRels 
              --   -- unify with head to get new subs
              --   let maybeSubs = trace ((show headRels) ++ (show renamedHead)) (unify (toFunctor headRels) (toFunctor renamedHead) subs)
              --   -- create child trees
              --   case maybeSubs of
              --       Just newSubs -> trace (show (Goal newSubs newGoal)) (return (Tree (Goal newSubs newGoal) [] False))
              --       Nothing -> []

          -- return list of subs by searching all child trees
          -- search expandedTree (height + 1)
          

{- returns all variables in a relation -}
variables :: Rel -> [Term]
variables rel = S.toList . S.fromList . aux . toFunctor $ rel
  where
    aux v@(Var x) = [v]
    aux (Func _ terms) = terms >>= aux
    aux (Atom _) = []

{- apply substitution to a term -}
resolve :: Subs -> Term -> Term
resolve subs (Var x) =
  -- find variable in subs
  case M.lookup x subs of
    -- if previous substitution already performed, resolve with prev substitution
    Just term -> resolve subs term
    Nothing -> Var x

-- resolve each term with subs
resolve subs (Func name terms) = Func name (map (resolve subs) terms)
-- atom resolution is just the atom
resolve subs a@(Atom _) = a