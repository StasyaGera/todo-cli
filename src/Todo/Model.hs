module Todo.Model
    ( Todo (..)
    , State (..)
    , Title (..)
    , Priority (..)
    , Tag (..)
    , Deadline (..)
    , Tags
    , Todos
    , defTodo
    , getByState
    , getTopPrio
    , getByTag
    , tagStatistics
    ) where

import Data.List (group, sort, sortBy)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Ord (comparing)
import Data.Ratio (Ratio)
import Numeric.Natural (Natural)

data State = TODO | DONE | FAIL deriving (Eq, Show)
newtype Title = Title (NonEmpty Char) deriving (Eq, Show)
-- the higher the priority value, the more urgent the task
newtype Priority = Priority (Ratio Natural) deriving (Eq, Show, Ord)
newtype Tag = Tag (NonEmpty Char) deriving (Eq, Show, Ord)
newtype Deadline = Deadline Int deriving (Eq, Show, Ord)
type Tags = NonEmpty Tag
type Todos = [Todo]

data Todo = Todo
    { todoState       :: State
    , todoTitle       :: Title
    , todoPriority    :: Priority
    , todoTags        :: Tags
    , todoSubtasks    :: Todos
    , todoDeadline    :: Maybe Deadline
    , todoDescription :: Maybe String
    } deriving (Eq, Show)

defTodo :: Title -> Priority -> Tags -> Todo
defTodo todoTitle todoPriority todoTags = Todo { todoState = TODO
                                               , todoSubtasks = []
                                               , todoDeadline = Nothing
                                               , todoDescription = Nothing
                                               , .. }

getByState :: State -> Todos -> Todos
getByState state = filter $ (state ==) . todoState

getByTag :: Tag -> Todos -> Todos
getByTag tag = filter $ elem tag . todoTags

getTopPrio :: Int -> Todos -> Todos
getTopPrio n = take n . sortBy (flip $ comparing todoPriority)

tagStatistics :: Todos -> [(Tag, Int)]
tagStatistics = count . concatMap (toList . todoTags)
  where
    -- from Data.List.Unique
    count :: Ord a => [a] -> [(a, Int)]
    count = map (\x -> (head x, length x)) . group . sort
