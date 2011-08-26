## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.

$(eval $(AT_MAKEFILE_START))

PLUGIN_LIB_EXTRA_SOURCE = intpack.c
PLUGIN_LIB_EXTRA_DEPS = intpack.h intpack.c $(RADR_INCLUDE_DIR)/patchify.h
PLUGIN_EXTRA_R_FILES = fixraw.R

## kludge: for windows, locate zlib correctly by using ../
ifeq ("$(TARGET_PLATFORM)", "windows")

PLUGIN_LIB_EXTRA_OPTIONS =  $(LIBZ_INC) ../$(LIBZ_LIB)

else

PLUGIN_LIB_EXTRA_OPTIONS =  $(LIBZ_INC) $(LIBZ_LIB)

endif

$(eval $(AT_MAKEFILE_END))
