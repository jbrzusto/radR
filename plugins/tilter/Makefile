## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.  To make this plugin, do "make tilter"
## from the radR toplevel folder.

TILTMINDER_DIR := $(PLUGIN_DIR)/tilter/tiltminder
$(eval $(AT_MAKEFILE_START))

.PHONY: FORCE

$(TILTMINDER_DIR)/tiltserv1.1s$(EXE_SUFFIX): FORCE
ifeq ("$(TARGET_PLATFORM)", "windows")
	$(MYMAKE) tiltminder -f $(TILTMINDER_DIR)/Makefile.mingw
else
	$(MYMAKE) tiltminder -f $(TILTMINDER_DIR)/Makefile
endif

PLUGIN_EXTRA_TARGETS = notes.txt 
PLUGIN_EXTRA_R_FILES = tilter.windows.conf.R tilter.unix.conf.R blipvolplot.R coldvolplot.R
PLUGIN_LIB_EXTRA_DEPS = $(wildcard $(RADR_LIB_DIR)/pthreadGC2*.dll) $(RADR_INCLUDE_DIR)/threadutil.h
PLUGIN_LIB_EXTRA_OPTIONS = $(PLUGIN_LIB_EXTRA_DEPS)

## copy the server invocation script
$(eval $(call DEF_INST_ALIAS,tiltserv$(SHELL_SCRIPT_SUFFIX),tiltminder/tiltserv$(SHELL_SCRIPT_SUFFIX)))

## copy the statically-linked server executable
$(eval $(call DEF_INST_ALIAS,tiltserv1.1s$(EXE_SUFFIX),tiltminder/tiltserv1.1s$(EXE_SUFFIX)))

## how to make the tiltserver
tiltminder/tiltserv1.1s$(EXE_SUFFIX): tiltminder/minder.cc

$(eval $(AT_MAKEFILE_END))



