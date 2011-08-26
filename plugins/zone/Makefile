## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.

$(eval $(AT_MAKEFILE_START))

PLUGIN_LIB_EXTRA_DEPS = $(RADR_INCLUDE_DIR)/patchify.h

## R file dependencies (due to ###.include statements)
.zone.plugin.R.stripped: zone.R

$(eval $(AT_MAKEFILE_END))
