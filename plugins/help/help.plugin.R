######################################################################
###
### help.plugin.radR
###
### the radR help plugin
###
######################################################################

about = function() {
  ## a function returning a description and the current state of the plugin.
  return("This plugin provides pointers to online resources for radR.\nYou can customize it by editting plugins/help.conf.R")
}

## This function returns a set list of plugin-specified menus.
## The menu is only installed after the plugin's load() function is called.
## The menus types are "main", "sources", and "sinks" and get attached to the
## right-click plot-window menu, the player source menu, and the player sink menu, respectively.

get.menus = function() {
  list(
       main = list(
         ## The cascading list of menu headings under which the plugin's menu is installed.
         
         titles = c("Help"),
         menu = list(
           c(list(
                  "do.one",
                  on.do = function(i) {
                    url <- web.resources[[i]]
                    if (length(grep("^http:|file:|ftp:|mailto:", url)) == 0)
                      ## if no protocol is specified, assume the url is a file relative to
                      ## the radR path
                      url <- file.path(getwd(), url)
                    browseURL(url)
                  }),
             names(web.resources)
             )
           )
         )
       )
}
