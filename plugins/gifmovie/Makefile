## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.

$(eval $(AT_MAKEFILE_START))

PLUGIN_EXTRA_R_FILES := gifmovie.windows.conf.R gifmovie.unix.conf.R
PLUGIN_EXTRA_TARGETS := gifsicle-patch.txt
ifeq ("$(TARGET_PLATFORM)", "windows")
PLUGIN_EXTRA_TARGETS := $(PLUGIN_EXTRA_TARGETS) gifsicle.exe win2rgb.exe
endif
$(eval $(AT_MAKEFILE_END))
