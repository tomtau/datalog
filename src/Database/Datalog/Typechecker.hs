module Database.Datalog.Typechecker ( typecheck ) where

import           Data.List                        (nub)

import qualified Control.Monad.Catch              as E
import           Control.Monad.Trans.State.Strict
import           Data.Hashable
import           Data.HashMap.Strict              (HashMap)
import           Data.Text                        (pack)
import           Database.Datalog.Database        (Database)
import           Database.Datalog.Relation        (Relation (..))
import           Database.Datalog.Rules

data ConstraintRule a = ConstraintRule {
    left  :: Literal Clause a
  , right :: [Literal Clause a]
                                       }

type ArgPos = Int
type RuleId = Int

data TypeExp a = UnaryPredicate Relation
  | ArgPredicateType Relation ArgPos
  | VarRule RuleId (Term a) deriving (Show, Eq)

data Inclusion a = SimpleInclusion (TypeExp a) (TypeExp a)
  | DisInclusion (TypeExp a) [TypeExp a]
  | ConInclusion [TypeExp a] (TypeExp a) deriving (Show, Eq)

negateLit :: Literal AdornedClause a -> Literal AdornedClause a
negateLit (Literal c) = NegatedLiteral c
negateLit (NegatedLiteral c) = Literal c
negateLit _ = error "unexpected body clause literal"

--convertConstraint :: ConstraintRule a -> Rule a
--convertConstraint (ConstraintRule l r rlm) =
--  Rule nullary (l : (map negateLit r)) rlm

nullary :: Clause a
nullary = Clause (Relation (pack "fail()")) []

bodycheck :: (Literal Clause a, RuleId) -> [Inclusion a]
bodycheck (l, rid) = map toInclusion parts
  where
    toInclusion (t,ti) = SimpleInclusion (VarRule rid t) (ArgPredicateType r ti)
    (r,parts) = case l of
      Literal (Clause rel terms) -> (rel,getTerms terms)
      NegatedLiteral (Clause rel terms) -> (rel,getTerms terms)
      _ -> error "conditional clause in bodycheck"

isLogicVar :: Term a -> Bool
isLogicVar (LogicVar _) = True
isLogicVar _ = False

getTerms :: [Term a] -> [(Term a, ArgPos)]
getTerms t = filter (isLogicVar . fst) $ zip t [1..length t]


headcheck :: Clause a -> [RuleId] -> [Inclusion a]
headcheck h rs = map toInclusion parts
  where
   toInclusion (t,ti) = DisInclusion (ArgPredicateType r ti) (map (\i -> VarRule i t) rs)
   (r,parts) = case h of
      (Clause rel terms) -> (rel,getTerms terms)



typecheck :: (E.MonadThrow m, Eq a, Hashable a)
             => QueryBuilder m a (Query a) -> Database a -> m ([(Clause a, [Literal Clause a])],[Inclusion a])
typecheck qm idb = do
  (q, QueryState _ _ rs) <- runStateT qm (QueryState idb 0 [])
  let rids = zip rs [1..length rs]
      bodies = concatMap (\x -> map (\y -> (y,snd x)) (snd $ fst x)) rids
      heads = map (\x -> (fst $ fst x, snd x)) rids
      cheads = nub $ map fst heads
      hincl = concatMap (\x -> headcheck x $ map snd $ filter (\y -> x == fst y) heads) cheads
  return (rs, concatMap bodycheck bodies ++ hincl)

{-
db1 :: Maybe (Database Text)
db1 = makeDatabase $ do
  person <- addRelation "person" 1
  gender <- addRelation "gender" 1
  parent <- addRelation "parent" 2
  hasGender <- addRelation "hasGender" 2
  mapM_ (assertFact gender) ([["Male"], ["Female"]] :: [[Text]])


t1 = do
  let Just db = db1
  res <- queryDatabase db q1
  putStrLn (show res)

db = fromJust db1

q1 = do
      person <- relationPredicateFromName "person"
      gender <- relationPredicateFromName "gender"
      parent <- relationPredicateFromName "parent"
      hasGender <- relationPredicateFromName "hasGender"
      father <- inferencePredicate "father"
      let x = LogicVar "x"
          y = LogicVar "y"
      (father, [x, y]) |- [ lit parent [x, y], lit hasGender [x, Atom "Male"] ]
      issueQuery father [ ]
-}
