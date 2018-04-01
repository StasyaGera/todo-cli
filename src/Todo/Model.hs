module Todo.Model
    ( Todo (..)
    , State (..)
    , Title (..)
    , Priority (..)
    , Tag (..)
    , Deadline (..)
    , Tags
    , Todos
    , TodoDB (..)
    , addTodo
    , addTag
    , deleteTag
    , editTag
    , defTodo
    , getByState
    , getTopPrio
    , getByTag
    , tagStatistics
    ) where

import           Data.List.NonEmpty (NonEmpty (..), toList, fromList)
import qualified Data.Ratio         as R
import qualified Numeric.Natural    as N
import qualified Data.List          as List

data State = TODO | DONE | FAIL deriving (Eq, Show)
newtype Title = Title (NonEmpty Char) deriving (Eq, Show)
-- the lower the priority value, the more urgent the task
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

data TodoDB = TodoDB 
    { tododbTodos :: Todos
    , tododbTags  :: [Tag]
    } deriving (Eq, Show)


addTodo :: Todo -> TodoDB -> TodoDB
addTodo todo TodoDB { .. } = TodoDB { tododbTodos = todo : tododbTodos, .. }

addTag :: Tag -> TodoDB -> TodoDB
addTag tag TodoDB { .. } = TodoDB { tododbTags = tag : tododbTags, .. }

deleteTag :: Tag -> TodoDB -> TodoDB
deleteTag tag db@TodoDB { .. } = case List.find ((tag :| [] ==) . todoTags) tododbTodos of 
    Just _  -> db
    Nothing -> TodoDB { tododbTodos = map (\Todo{ .. } -> 
        Todo { todoTags = fromList $ List.delete tag . toList $ todoTags, .. }) tododbTodos
                      , tododbTags = List.delete tag tododbTags }

editTag :: Tag -> Tag -> TodoDB -> TodoDB
editTag old new TodoDB{ .. } = 
    TodoDB { tododbTodos = map (\Todo{ .. } -> 
        Todo { todoTags = fromList $ replace $ toList todoTags, .. }) tododbTodos
           , tododbTags = new : List.delete old tododbTags } 
  where 
    replace :: [Tag] -> [Tag]
    replace []      = []
    replace (t:ts) 
        | t == old  = new : ts 
        | otherwise = t : replace ts


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
getTopPrio n lst = take n (List.sortBy ((. todoPriority) . compare . todoPriority) lst)

tagStatistics :: Todos -> [(Tag, Int)]
tagStatistics = count . concatMap (toList . todoTags)
  where 
    -- from Data.List.Unique
    count :: Ord a => [a] -> [(a, Int)]
    count = map (\x -> (head x, length x)) . List.group . List.sort
