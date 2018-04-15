module Todo.DB
    ( TodoDB (..)
    , DBError (..)
    , addTodo
    , addTag
    , deleteTag
    , editTag
    ) where

import Data.HashSet (HashSet)
import Data.Foldable (toList)
import qualified Data.HashSet as HSet

import Todo.Model (Todo (..), Tag (..))
import Data.NESet (NonEmptySet (..))
import qualified Data.NESet as NESet

data TodoDB = TodoDB
    { dbTodos :: HashSet Todo
    , dbTags  :: HashSet Tag
    } deriving (Eq, Show)

data DBError = SingleTaggedTodo | NotRegisteredTags deriving (Eq, Show)

addTodo :: Todo -> TodoDB -> Either DBError TodoDB
addTodo todo@Todo {..} TodoDB {..} 
    | HSet.null missing = Right TodoDB { dbTodos = HSet.insert todo dbTodos, .. }
    | otherwise         = Left NotRegisteredTags
  where
    missing = NESet.filter (\t -> not $ HSet.member t dbTags) todoTags

addTag :: Tag -> TodoDB -> TodoDB
addTag tag TodoDB {..} = TodoDB { dbTags = HSet.insert tag dbTags, .. }

deleteTag :: Tag -> TodoDB -> Either DBError TodoDB
deleteTag tag TodoDB {..} = case mapM removeTag $ toList dbTodos of
    Nothing    -> Left SingleTaggedTodo
    Just todos -> Right TodoDB { dbTodos = HSet.fromList todos, .. }
  where
    removeTag :: Todo -> Maybe Todo
    removeTag Todo {..} = do
        tags <- NESet.fromSet $ NESet.delete tag todoTags
        Just Todo { todoTags = tags, .. }

editTag :: Tag -> Tag -> TodoDB -> TodoDB
editTag old new TodoDB {..} = TodoDB { dbTodos = HSet.map renameTag dbTodos
                                     , dbTags = HSet.insert new (HSet.delete old dbTags)
                                     }
  where
    renameTag :: Todo -> Todo
    renameTag Todo { todoTags = (t :< ts), .. } = Todo { todoTags = tags, .. } 
      where
        tags
            | t == old  = new :< ts 
            | otherwise = t :< HSet.insert new (HSet.delete old ts)
