##  svn $Id: comments.plugin.R 574 2010-05-11 02:07:15Z john $
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

## a radR plugin for accepting user comments during live recording

about = function() {
  return(plugin.label %:% "\n\nVersion " %:% version %:% 
         "\n\nThis plugin lets you enter a comment, and associate it with a particular scan\nThe comment might actually be recorded with the following scan.\n" )
}

enable = function(enab) {
  ## the START_SOURCE hook is used to quietly disable the plugin if 
  ## the current source is not a radar; so that hook is always left enabled
  rss.enable.hook("GET_SCAN_INFO", enab)
  rss.enable.hook("SCAN_INFO", enab & show.comments)
}

get.menus = function() {

  ## return the list of menus
  list(
       plugin = list(
         ## the standard comments
         list("choose.any",
              on.set = function(n, v) {
                show.comments <<- v
                rss.enable.hook("SCAN_INFO", enabled & show.comments)
              },
              "Show existing comments in a popup window" = show.comments
              ),
         list (ttl = "gauge",
               label = "how long a comment popup window is displayed (seconds):", 
               range = c(0, 1000),
               increment = 1,
               value = comment.ttl,
               on.set = function(x) { comment.ttl <<- x; }
               ),
        "Record a comment" = list("menu",
           "Custom comment..." =                 
           function() {
             if (!is.allowed()) return()
             entry <- rss.gui(POPUP_DIALOG,
                              title = "radR scan comment entry",
                              msg = "Enter a comment for this scan",
                              entry = TRUE,
                              default.entry = default.user.comment,
                              buttons = c("Ok", "Cancel")
                              )
             if (entry[[1]] == 1) {
               comment <<- entry[[2]]
               override <<- TRUE
             }
           },
           c(list("do.one",
                  on.do = function(n) {
                    if (!is.allowed())
                      return()
                    comment <<- builtin.comments[n]
                    override <<- TRUE
                  }),
             builtin.comments
             )
           )
         )
       )
}


## save the current scan as the checkpoint

is.allowed = function() {
  if (!enabled)
    return(FALSE)
  if (!is.null(RSS$source) && !is.null(RSS$sink) && inherits(RSS$sink, "blipmovie")) {
    return(TRUE)
  } else {
    rss.gui(POPUP_DIALOG,
            title="Comments are not available right now",
            msg = "You need to be recording to a blipmovie in order to register a comment for a scan.")
    return(FALSE)
  }
}

hooks = list (
##   MARK_PLOT_POINT = list (enabled=TRUE, read.only=TRUE,
##     f = function(coords) {
##       ## do something with the coordinates at which the pointer was
##       ## when the user chose to hit mark
##     }),

  GET_SCAN_INFO = list (enabled=TRUE, read.only=TRUE,
    f = function(si) {
      cmt <- comment
      ## unless this is a preview scan, in which case the comment would be lost
      ## when hitting play or play1, erase it since it has been returned once
      ## in scan.info, and reset override to FALSE
      
      if (!RSS$previewing && cmt != "")
        comment <<- ""
      if (override || length(si$user.comment) == 0 || sum(nchar(si$user.comment)) == 0) {
        if (!RSS$previewing)
          override <<- FALSE
        si$user.comment <- cmt
      }
      return(si)
    }),
  
  SCAN_INFO = list (enabled=TRUE, read.only=TRUE,
    f = function(si) {
      if (nchar(as.character(si$user.comment)) > 0)
        rss.gui(POPUP_MESSAGEBOX, "Comment", paste(format(si$timestamp, GUI$plot.title.date.format), ":  ", si$user.comment, sep=""), time.to.live = comment.ttl)
    })
      
  )  ## end of hooks list

## additional plugin variables

scan.index = NULL   ## a place to save the current scan index when a comment button is invoked,
## so the user's comment is attached to the correct scan even if it takes
## several scans to enter the comment

comment = ""        ## value of the comment string; reset to "" after each call to the GET_SCAN_INFO hook

override = FALSE    ## should an existing comment be overridden with the current one?  Set when the user
## generates a comment
