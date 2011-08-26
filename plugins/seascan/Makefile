## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.  To make this plugin, do 

## for making the (so far) Windows-specific seascan shared library:

ifeq ("$(TARGET_PLATFORM)", "windows")

$(eval $(AT_MAKEFILE_START))

## path to the client library that comes with SeaScan
ifeq ("$(BUILD_PLATFORM)", "windows")
SEASCAN_PATH = c:/Program\ Files/Rutter/Sigma\ S6/SeaScanRT
else
SEASCAN_PATH = /media/sda1/Program\ Files/Rutter/Sigma\ S6/SeaScanRT
endif
SEASCAN_CLIENT_LIBRARY = $(SEASCAN_PATH)/Client.dll
## include the pthreads library

PLUGIN_LIB_EXTRA_DEPS = $(wildcard $(RADR_LIB_DIR)/pthreadGC2*.dll) $(RADR_INCLUDE_DIR)/threadutil.h
PLUGIN_LIB_EXTRA_OPTIONS = $(PLUGIN_LIB_EXTRA_DEPS) $(SEASCAN_CLIENT_LIBRARY)

PLUGIN_EXTRA_TARGETS = SeaScan.bat SeaScan0.ini SeaScan0.bin
PLUGIN_EXTRA_R_FILES = furuno1954C-BB.radar.R

$(eval $(AT_MAKEFILE_END))

else
$(info Skipping the seascan plugin; it is currently available only for Windows.)
all:

plugin_clean:

endif



