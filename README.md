# TODO console app 

## Fields

A typical TODO item should have:
 - title
 - priority
 - tags (at least one)
 - subtasks (optional)
 - deadline (optional)
 - description (optional)

## States

A TODO item exists in one of three states:
 1. TODO
 2. DONE (successfully)
 3. FAIL (for both cancelled and actually failed tasks) 

## Operations

A TODO item should support following operations:
 - create a new item
    - set priority (by specifying a number manually or interactively by comparing to other tasks)
 - edit an exiting one
 - change state
 - show editing history

Overall operations should be:
 - get N items with highest priority
 - get a list of items marked as done 
 - get everything by a specific tag 
 - get list of all tags with the amount of issues on each
 - create / delete / edit tags
