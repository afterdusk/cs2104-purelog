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
    renameRel = toRel . renameTerm . toFunctor
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
searchAll (Program rules) tree = search tree 0
  where
    
    -- populates children of a tree node by returning list of Trees
    expandTree (Tree (Goal subs rels) _ _) height = do
      -- get all matching rules
      rule@(Rule matchingHead matchingRels) <- filter (match (head rels)) rules
      -- rename matching rules
      let renamedRule@(Rule renamedHead renamedRels) = rename rule height
      -- get new goal rels, which are query rels + renamed matching rels
      -- assume singleton relationships by using concat -- TODO: is this correct?
      let newGoal = (concat renamedRels) ++ (tail rels) 
      -- unify with head to get new subs
      let maybeSubs = trace ((show (head rels)) ++ (show renamedHead)) (unify (toFunctor (head rels)) (toFunctor renamedHead) subs)
      -- create child trees
      case maybeSubs of
          Just newSubs -> trace (show (Goal newSubs newGoal)) (return (Tree (Goal newSubs newGoal) [] False))
          Nothing -> []
    
    -- input: node
    -- output: list of subs 
    search tree height =
      case tree of
        (Tree (Goal subs []) trees _) -> 
          return subs
        (Tree (Goal subs rels) trees True) -> do
          childTree <- trees
          search childTree (height + 1)
        childTree@(Tree goal _ False) -> do 
          -- unpack list of child trees
          expandedTree <- expandTree childTree (height + 1)
          -- return list of subs
          search expandedTree (height + 1)
          

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