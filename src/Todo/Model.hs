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

import Data.Ratio (Ratio)
import Numeric.Natural (Natural)
import Data.Ord (comparing)
import Data.List (sortBy, foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable (toList)
import GHC.Generics (Generic)
import Data.Hashable (Hashable (..))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.Text (Text)
import qualified Data.Text as T

import Data.NESet (NonEmptySet (..))
import qualified Data.NESet as NESet

data State = TODO | DONE | FAIL deriving (Eq, Show, Generic)
newtype Title = Title Text deriving (Eq, Show, Hashable)
-- the higher the priority value, the more urgent the task
newtype Priority = Priority (Ratio Natural) deriving (Eq, Show, Ord, Hashable)
newtype Tag = Tag Text deriving (Eq, Show, Ord, Hashable)
newtype Deadline = Deadline Int deriving (Eq, Show, Ord, Hashable)
type Tags = NonEmptySet Tag
type Todos = HashSet Todo

instance Hashable State

data Todo = Todo
    { todoState       :: State
    , todoTitle       :: Title
    , todoPriority    :: Priority
    , todoTags        :: Tags
    , todoSubtasks    :: Todos
    , todoDeadline    :: Maybe Deadline
    , todoDescription :: Maybe Text
    } deriving (Eq, Show, Generic)

instance Hashable Todo 

defTodo :: Title -> Priority -> Tags -> Todo
defTodo todoTitle todoPriority todoTags = Todo { todoState = TODO
                                               , todoSubtasks = mempty
                                               , todoDeadline = Nothing
                                               , todoDescription = Nothing
                                               , .. }

getByState :: State -> Todos -> Todos
getByState state = HSet.filter $ (state ==) . todoState

getByTag :: Tag -> Todos -> Todos
getByTag tag = HSet.filter $ NESet.member tag . todoTags

getTopPrio :: Int -> Todos -> Todos
getTopPrio n = HSet.fromList . take n . sortBy (flip $ comparing todoPriority) . toList

tagStatistics :: Todos -> HashMap Tag Int
tagStatistics = count . concatMap (toList . todoTags)
  where
    count :: (Eq a, Hashable a) => [a] -> HashMap a Int
    count = foldl' (\dict x -> HMap.insertWith (+) x 1 dict) mempty
