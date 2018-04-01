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

import           Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Ratio         as R
import qualified Numeric.Natural    as N
import qualified Data.List          as List
import qualified Data.Ord           as Ord

data State = TODO | DONE | FAIL deriving (Eq, Show)
newtype Title = Title (NonEmpty Char) deriving (Eq, Show)
-- the higher the priority value, the more urgent the task
newtype Priority = Priority (R.Ratio N.Natural) deriving (Eq, Show, Ord)
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
getTopPrio n lst = take n (List.sortBy (flip $ Ord.comparing todoPriority) lst)

tagStatistics :: Todos -> [(Tag, Int)]
tagStatistics = count . concatMap (toList . todoTags)
  where 
    -- from Data.List.Unique
    count :: Ord a => [a] -> [(a, Int)]
    count = map (\x -> (head x, length x)) . List.group . List.sort
