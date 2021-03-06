## svn $Id: comments.conf.R 574 2010-05-11 02:07:15Z john $
##                                                         
##  COMMENTS   PLUGIN  CONFIG
##                                                         
##  These are parameters for the radR plugin comments      
##                                                         
##  DO NOT EDIT THIS FILE WHILE RUNNING radR, because      
##  it is overwritten with new values when you exit        
##  radR.                                                  
##                                                         
##  Lines beginning with "#" and blank lines are preserved 
##  but comments at the ends of lines are not.             
##                                                         
##  Make sure to include a comma at the end of each item   
##  in a list.  A list definition should look like this:   
##                                                         
##     what.ever = list (                                  
##        something = 45,                                  
##        something.else = 55                              
##     )                                                   
##                                                         
##  The first and last lines must not have any actual list 
##  items on them, and all items must be named             
##  (i.e. NAME = THING, instead of just THING)             


## Is this plugin enabled on loading?

enabled = TRUE

## Do we display comments in a popup window?

show.comments = TRUE

## How long do comments persist (in seconds)

comment.ttl = 3

## types of comment; the user can at any time
## generate a comment which will be added to the
## scan.info for the scan where the user initiated
## the comment, or to the next scan processed.

## This list provides a category which can be
## selected by the user when recording a comment,
## in addition to any comment text.  This can be
## used to automate subsequent comment processing.

builtin.comments = c ("rain starts", "rain ends")

## The default comment string.  This lets the user
## easily save a commonly used comment.

default.user.comment = "(enter a comment)"

