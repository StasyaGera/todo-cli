module Todo.Model where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Ratio         as R
import qualified Numeric.Natural    as N

data State = TODO | DONE | FAIL deriving (Eq, Show)

newtype Title = Title (NonEmpty Char) deriving (Eq, Show)
newtype Priority = Priority (R.Ratio N.Natural) deriving (Eq, Show)
newtype Tag = Tag (NonEmpty Char) deriving (Eq, Show)
newtype Deadline = Deadline Int deriving (Eq, Show)
type Tags = NonEmpty Tag

data Todo = Todo
    { todoState       :: State
    , todoTitle       :: Title
    , todoPriority    :: Priority
    , todoTags        :: Tags
    , todoSubtasks    :: [Todo]
    , todoDeadline    :: Maybe Deadline
    , todoDescription :: Maybe String
    } deriving (Eq, Show)

type Todos = [Todo]
