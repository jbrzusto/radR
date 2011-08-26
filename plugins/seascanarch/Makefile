## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.  To make this plugin, do 
##
## cd .. (the main radR folder)
## make seascanarch
##

$(eval $(AT_MAKEFILE_START))

PLUGIN_LIB_EXTRA_DEPS = $(addprefix ../seascan/, IDLCONST.h myIDLTYPES.h) ../../include/wintypes.h
PLUGIN_LIB_EXTRA_OPTIONS = -I../seascan

$(eval $(AT_MAKEFILE_END))
