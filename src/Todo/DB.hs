module Todo.DB
    ( TodoDB (..)
    , DBError (..)
    , addTodo
    , addTag
    , deleteTag
    , editTag
    ) where

import           Todo.Model
import           Data.List.NonEmpty (NonEmpty (..), fromList, toList)
import qualified Data.List as List

data TodoDB = TodoDB
    { dbTodos :: Todos
    , dbTags  :: [Tag]
    } deriving (Eq, Show)

data DBError = SingleTaggedTodo  deriving (Eq, Show)

addTodo :: Todo -> TodoDB -> TodoDB
addTodo todo TodoDB { .. } = TodoDB { dbTodos = todo : dbTodos, .. }

addTag :: Tag -> TodoDB -> TodoDB
addTag tag TodoDB { .. } = TodoDB { dbTags = tag : dbTags, .. }

deleteTag :: Tag -> TodoDB -> Either DBError TodoDB
deleteTag tag TodoDB { .. } = case List.find ((tag :| [] ==) . todoTags) dbTodos of
    Just _  -> Left SingleTaggedTodo
    Nothing -> Right $ TodoDB { dbTodos = map removeTag dbTodos
                              , dbTags = List.delete tag dbTags 
                              }
  where
    removeTag :: Todo -> Todo
    removeTag Todo { .. } = Todo { todoTags = fromList $ List.delete tag . toList $ todoTags, .. }

editTag :: Tag -> Tag -> TodoDB -> TodoDB
editTag old new TodoDB { .. } = TodoDB { dbTodos = map renameTag dbTodos
                                       , dbTags = new : List.delete old dbTags 
                                       }
  where
    renameTag :: Todo -> Todo
    renameTag Todo { .. } = Todo { todoTags = fromList $ replace $ toList todoTags, .. }

    replace :: [Tag] -> [Tag]
    replace []      = []
    replace (t:ts)
        | t == old  = new : ts
        | otherwise = t : replace ts
