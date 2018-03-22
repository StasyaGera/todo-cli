{-# LANGUAGE RecordWildCards #-}

module Lib where

import           Data.List.NonEmpty

data State = TODO | DONE | FAIL deriving (Eq, Show)

newtype Title = Title (NonEmpty Char) deriving (Eq, Show)
newtype Priority = Priority Int deriving (Eq, Show)
newtype Tag = Tag String deriving (Eq, Show)
newtype Tags = Tags (NonEmpty Tag) deriving (Eq, Show)
newtype Deadline = Deadline Int deriving (Eq, Show)

data Todo = Todo
    { state       :: State
    , title       :: Title
    , priority    :: Priority
    , tags        :: Tags
    , subtasks    :: Maybe (NonEmpty Todo)
    , deadline    :: Maybe Deadline
    , sescription :: Maybe String
    } deriving (Eq, Show)
    
data TodoList = TodoList  deriving (Eq, Show)
