## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.  To make a particular plugin, do 
##
##     cd .. (the radR plugins folder)
##     make plugins/PLUGIN
##
##
## Set the names of extra plugin files here:
## (the standard files are 
##  PLUGIN.desc.R PLUGIN.conf.R PLUGIN.plugin.R, PLUGIN$(SHLIB_SUFFIX)
## )
##
## These will be copied to the plugin's installation folder.  Any which end in .R will
## be checked for syntax.

PLUGINS_TO_EXCLUDE := iris xenex usrp tilter

PLUGIN_NAMES := $(filter-out $(PLUGINS_TO_EXCLUDE),$(shell $(FIND) . -maxdepth 1 -type d -regex '^\./[a-zA-Z0-9_]+' -printf '%f\n'))

plugins: $(PLUGIN_NAMES)

.PHONY: plugins $(PLUGIN_NAMES)

$(PLUGIN_NAMES):
	$(MYMAKE) $@

clean:
	$(foreach PLUGIN,$(PLUGIN_NAMES),$(MYMAKE) $(PLUGIN) plugin_clean; )
	rm -rf $(RADR_INSTALL_DIR)/plugins/*
