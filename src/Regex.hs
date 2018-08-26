module Regex where

import           Data.Maybe      (fromMaybe)

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

data Regex
    = SymRegex Sym
    | Regex :+ Regex
    | Regex :| Regex
    | Repeat Regex
    | Eps
    deriving Show

data Sym
    = Fixed Char
    | Any
    deriving (Show, Eq, Ord)

infixl 3 :|
infixl 4 :+

char :: Char -> Regex
char = SymRegex . Fixed

string :: String -> Regex
string = foldr (:+) Eps . map char

newtype State = State { getState :: Int }
    deriving (Show, Eq, Ord)

data Transition
    = SymTransition Sym
    | EpsTransition
    deriving (Show, Eq, Ord)

type Transitions = M.Map (State, Transition) (S.Set State)

data NFA = NFA
    { startState     :: State
    , terminalStates :: S.Set State
    , transitions    :: Transitions
    }
    deriving Show

mergeTransitions :: Transitions -> Transitions -> Transitions
mergeTransitions = M.unionWith mappend

compile' :: Int -> Regex -> (Int, NFA)
compile' nextState (SymRegex sym) =
    ( nextState + 2
    , NFA
        { startState     = state1
        , terminalStates = S.singleton state2
        , transitions    = M.singleton (state1, SymTransition sym) (S.singleton state2)
        }
    )
  where
    state1 = State nextState
    state2 = State (nextState + 1)
compile' nextState (l :+ r) =
    ( nextState2
    , NFA
        { startState     = startState lNFA
        , terminalStates = terminalStates rNFA
        , transitions    = newTransitions
            `mergeTransitions` transitions lNFA `mergeTransitions` transitions rNFA
        }
    )
  where
    (nextState1, lNFA) = compile' nextState l
    (nextState2, rNFA) = compile' nextState1 r
    newTransitions = M.fromList
        $ map (\st -> ((st, EpsTransition), S.singleton (startState rNFA)))
        $ S.toList (terminalStates lNFA)
compile' nextState (l :| r) =
    ( nextState2 + 1
    , NFA
        { startState     = state1
        , terminalStates = S.union (terminalStates lNFA) (terminalStates rNFA)
        , transitions    = newTransitions
            `mergeTransitions` transitions lNFA `mergeTransitions` transitions rNFA
        }
    )
  where
    (nextState1, lNFA) = compile' nextState l
    (nextState2, rNFA) = compile' nextState1 r
    newTransitions = M.singleton (state1, EpsTransition)
        (S.fromList [startState lNFA, startState rNFA])
    state1 = State nextState2
compile' nextState (Repeat x) =
    ( nextState1 + 1
    , NFA
        { startState     = state1
        , terminalStates = S.singleton state1
        , transitions    = mergeTransitions (transitions xNFA) newTransitions
        }
    )
  where
    (nextState1, xNFA) = compile' nextState x
    state1 = State nextState1
    newTransitions = M.fromList
        $ (:) ((state1, EpsTransition), S.singleton (startState xNFA))
        $ map (\st -> ((st, EpsTransition), S.singleton state1))
        $ S.toList
        $ terminalStates xNFA
compile' nextState Eps =
    ( nextState + 1
    , NFA
        { startState     = state1
        , terminalStates = S.singleton state1
        , transitions    = mempty
        }
    )
  where
    state1 = State nextState

compile :: Regex -> NFA
compile = snd . compile' 0

lookupTransition :: (State, Transition) -> Transitions -> S.Set State
lookupTransition a b = fromMaybe S.empty (M.lookup a b)

runChar :: NFA -> Char -> State -> S.Set State
runChar nfa c st = S.union
    (lookupTransition (st, SymTransition (Fixed c)) (transitions nfa))
    (lookupTransition (st, SymTransition Any)       (transitions nfa))

runEps :: NFA -> State -> S.Set State
runEps nfa st = S.insert st (lookupTransition (st, EpsTransition) (transitions nfa))

runStep :: NFA -> Char -> S.Set State -> S.Set State
runStep nfa c = flatRun (runEps nfa) . flatRun (runChar nfa c)
  where
    flatRun :: (State -> S.Set State) -> S.Set State -> S.Set State
    flatRun f = mconcat . map f . S.toList

run :: NFA -> String -> Bool
run nfa = any (`S.member` terminalStates nfa)
    . S.toList
    . foldl (\st c -> runStep nfa c st) (runEps nfa (startState nfa))

checkRegex :: Regex -> String -> Bool
checkRegex = run . compile
