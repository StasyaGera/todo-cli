# TODO console app 

## Fields

A typical TODO item should have:
 - title (non empty)
 - priority
 - tags (at least one)
 - subtasks    (optional)
 - deadline    (optional)
 - description (optional)

Note that TODOs with identical titles will be distinguished by the time of creation.

## States

A TODO item exists in one of three states:
 1. TODO
 2. DONE (successfully)
 3. FAIL (for both cancelled and actually failed tasks) 

## Operations

Supported operations with TODO items:
 - create a new item
    - set priority (by specifying a number manually or interactively by comparing to other tasks)
 - edit an exiting one
 - change state
 - show editing history

Overall operations:
 - get a list of items marked as done 
 - get N items with highest priority
 - get everything by a specific tag 
 - get list of all tags with the amount of issues on each
 - create / delete / edit tags
