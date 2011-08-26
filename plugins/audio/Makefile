## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.

ifeq ("$(TARGET_PLATFORM)", "windows")

$(eval $(AT_MAKEFILE_START))

PLUGIN_LIB_EXTRA_OPTIONS = -lwinmm

$(eval $(AT_MAKEFILE_END))

else
$(info Skipping the audio plugin; it is currently available only for Windows.)
all:

plugin_clean:

endif
