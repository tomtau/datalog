module Database.Datalog.Typechecker ( typecheck ) where

--import Database.Datalog.Database
import qualified Control.Monad.Catch              as E
import           Control.Monad.Trans.State.Strict
import           Data.Hashable
import           Data.HashMap.Strict              (HashMap)
import           Data.Text                        (pack)
import           Database.Datalog.Database        (Database)
import           Database.Datalog.Relation        (Relation (..))
import           Database.Datalog.Rules

data ConstraintRule a = ConstraintRule {
    left            :: Literal AdornedClause a
  , right           :: [Literal AdornedClause a]
  , ruleVariableMap :: HashMap (Term a) Int
                                       }

data TypeExp a = UnaryPredicate Relation
  | ArgPredicateType Relation Int
  | VarRule () (Term a)

data Inclusion a = SimpleInclusion (TypeExp a) (TypeExp a)

negateLit :: Literal AdornedClause a -> Literal AdornedClause a
negateLit (Literal c) = NegatedLiteral c
negateLit (NegatedLiteral c) = Literal c
negateLit _ = error "unexpected body clause literal"

convertConstraint :: ConstraintRule a -> Rule a
convertConstraint (ConstraintRule l r rlm) =
  Rule nullary (l : (map negateLit r)) rlm

nullary :: AdornedClause a
nullary = AdornedClause (Relation (pack "fail()")) []

bodycheck :: Literal Clause a -> [Inclusion a]
bodycheck l = map toInclusion parts
  where
    isLogicVar (LogicVar _) = True
    isLogicVar _ = False
    getTerms t = filter (isLogicVar . fst) $ zip t [1..length t]
    toInclusion (t,ti) = SimpleInclusion (VarRule () t) (ArgPredicateType r ti)
    (r,parts) = case l of
      Literal (Clause rel terms) -> (rel,getTerms terms)
      NegatedLiteral (Clause rel terms) -> (rel,getTerms terms)
      _ -> error "conditional clause in bodycheck"


typecheck :: (E.MonadThrow m, Eq a, Hashable a)
             => QueryBuilder m a (Query a) -> Database a -> m [Inclusion a]
typecheck qm idb = do
             (q, QueryState _ _ rs) <- runStateT qm (QueryState idb 0 [])
             return $ concatMap bodycheck (concatMap snd rs)

