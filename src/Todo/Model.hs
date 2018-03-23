module Todo.Model
    ( Todo (..)
    , State (..)
    , Title (..)
    , Priority (..)
    , Tag (..)
    , Deadline (..)
    , Tags
    , Todos
    ) where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Ratio         as R
import qualified Numeric.Natural    as N

data State = TODO | DONE | FAIL deriving (Eq, Show)

newtype Title = Title (NonEmpty Char) deriving (Eq, Show)
-- the lower the priority value, the more urgent the task
newtype Priority = Priority (R.Ratio N.Natural) deriving (Eq, Show, Ord)
newtype Tag = Tag (NonEmpty Char) deriving (Eq, Show)
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
