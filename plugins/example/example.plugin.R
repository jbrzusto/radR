##  svn $Id: example.plugin.R 134 2009-01-20 15:04:53Z john $
##
##  radR : an R-based platform for acquisition and analysis of radar data
##  Copyright (C) 2006-2009 John Brzustowski
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##

##      The EXAMPLE radR plugin
##
## After a plugin is loaded, it is assigned to the global environment
## using its upper case name.
##
## e.g. this plugin will be installed as EXAMPLE in .GlobalEnv

MYCLASS="example"

about = function() {
  ## a function returning a description and the current state of the plugin.
  return("This function describes the plugin.")
}

## This function returns a set list of plugin-specified menus.
## The menu is only installed after the plugin's load() function is called.
## The menus types are "main", "sources", and "sinks" and get attached to the
## right-click plot-window menu, the player source menu, and the player sink menu, respectively.

get.menus = function() {
  list(
       ## to put additional items in in the standard plugin menu,
       ## use the 'plugin' item, which gives a list-defined menu
       plugin = list("I'm a heading in the plugin menu"),

       ## for other menus, we need both the list-defined menu and
       ## the titles, so we put these in a list.
       
       main = list(
         ## The cascading list of menu headings under which the plugin's menu is installed.
         
         titles = c("Example plugin", "See how menus cascade?"),

         ## The menu itself.
         ##
         ## Each non-empty name of a list item becomes a menu label.
         ##
         ## The items in the list can be of the following types:
         ##
         ## function:  selecting that menu item calls the R function
         ##
         ## "---":  this quoted string generates a menu separator
         ##         
         ## list(type=TYPE, ...): an entry or entries or submenu according to TYPE:
         ##
         ##   TYPE == "choose.one": the entry is a set of mutually exclusive options
         ##           and ... has these possibilities:
         ##           set.or.get = NAME: the name of an R function which will be used to
         ##              get or set the current option.
         ##              NAME() will return the current option, from 1 ... num options
         ##              NAME(x) will set the current option
         ##           on.set = FUNCTION: a function which will be called with the current
         ##              option number (1 .. num options) as its only parameter
         ##           NAME=TRUE: the string which is the selected option for this group
         ##           NAMES: any other strings which name options for this group.
         ##           For unselected options, the items of the list will be used if they are
         ##           not of class logical; i.e. you can say either "My option" = FALSE, or just "My option",
         ##
         ##   TYPE == "choose.any": the entry is a set of independent options
         ##           and ... has these possibilities:
         ##           set.or.get = NAME: the name of an R function which will be created at
         ##              the global level to  get or set the current option.
         ##              NAME(i) will return the TRUE/FALSE state of option i, with i from 1 ... num options
         ##              NAME(i, state) will set the state of option i to state (TRUE or FALSE)
         ##           on.set = FUNCTION: a function which will be called with an 
         ##              option number (1 .. num options) and its new state as its two parameters
         ##           NAME = TRUE: for each option which is currently selected
         ##           NAME = FALSE: for each option which is currently unselected
         ##           For unselected options, the items of the list will be used if they are
         ##           not of class logical; i.e. you can say either "My option" = FALSE, or just "My option",
         ##
         ##   TYPE == "file": when selected, the entry will present the user with a file
         ##           selection dialog and ... has these possibilities:
         ##           type = "open.one", "open.many", "save", "open.dir"
         ##              according to whether the user is being prompted to open one or
         ##              multiple existing files, or to save a file, or to choose a directory
         ##              Defaults to "open.one"
         ##           on.set = FUNCTION: a function which will be called with the file/directory name(s)
         ##              selected by the user (unless the selection dialog was cancelled)
         ##           title = the title for the dialog box
         ##           init.dir = function returning initial directory
         ##           init.file = function returning initial file
         ##           file.types = list of file types:  given as "ext" = "Description"
         ##                                             e.g. ".dat" = "Archive files"
         ##
         ##   TYPE == "do.one": the entry is a set of commands all of which just call
         ##           the same function with a different index corresponding to which
         ##           entry was invoked
         ##           and ... has these possibilities:
         ##           on.set = FUNCTION: a function which will be called with the current
         ##              option number (1 .. num options) as its only parameter
         ##           NAMES: any other strings which are command label for this group.
         ##
         ##   TYPE == "menu": this is a cascading menu; the ... follow the same syntax
         ##           as the top level list.
         ##
         ## e.g. `Do something` = list( function() { do.something()})
         ## adds a menu entry labelled `Do something` which, when invoked, calls
         ## a function which in turn calls do.something()

         menu = list(
           "Increment plugin var" = function() {
             my.plugin.var <<- my.plugin.var + 1
             gui.print.cons(my.plugin.var)
           },
           
           "Decrement plugin var" = function() {
             my.plugin.var <<- my.plugin.var - 1
             gui.print.cons(my.plugin.var)
           },
           "---",
           c(list(
                  type = "choose.one",
                  on.set = function(i) gui.print.cons("You chose " %:% my.plugin.options[i])
                  ),
             my.plugin.options)
           )
         )
       )
}


load = function() {
  ## You can do initializations here.
  my.plugin.var <<- 25
}

unload = function(save.config) {
  ## do whatever is required to shut down this
  ## plugin and free resources
  ## Changes to it have no effect, and the return
  ## value of this function is ignored.
}

enable = function(enab) {
  ## enable or disable the plugin, according to the value of "enable"
  ## This function illustrates the default behaviour.  If your plugin
  ## does only this, you can omit the definition of "enable".

  enabled <<- enab
}

hooks = list(
  ## a list of hooks.
  ## Names in the list are hook types and must be in names(RSS$hooks)
  ## each item in the list is a list with these fields:
  ## $enabled:  the hook is enabled at plugin load time
  ## $read.only: the hook only reads radR data, but doesn't modify it
  ## $f: the actual hook function, which receives the parameters
  ##     appropriate to its type.  FIXME: document where these are defined
  
  ONPLAY = list( enabled = FALSE, read.only = TRUE,
    f = function() {
      ## save the information for this scan
      gui.print.cons("Got to the example ONPLAY hook.")
      gui.print.cons(my.plugin.var)
      my.plugin.var <<- 100
    }),
  
  ONSTOP = list( enabled = FALSE, read.only = TRUE,
    f = function() {
      ## save the information for this scan
      gui.print.cons("Got to the example ONSTOP hook.")
      gui.print.cons(my.plugin.var)
      my.plugin.var <<- 50
    })
  )

globals = list(
  ## any variables you wish to install in the Global environment can be included
  ## in this list.  These are installed before the plugin's load() function is called.
  my.global.var = 1:100
  )

## any additional state variables required by the plugin should be added here
## These can be referred to by name within plugin functions.  To assign
## values to them from within plugin functions, use the parent scope
## assignment operator; i.e. use "<<-" instead of "<-"

my.plugin.var = NULL
my.plugin.options = c("option A", "option B", "plan C")  

