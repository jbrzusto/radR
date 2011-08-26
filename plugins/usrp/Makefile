## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.  To make this plugin, do 'make usrp' 
## from the radR top-level directory.

## for making the (so far) Linux-specific usrp:

PLUGIN_EXTRA_R_FILES := usrp.windows.conf.R usrp.unix.conf.R

ifeq ("$(TARGET_PLATFORM)", "unix")

USRP_RADR_PLUGIN_DIR:=/usr/src/gnuradio/usrp/host/apps
USRP_INCLUDE_PATH:=/usr/src/gnuradio/usrp/host/include

$(eval $(AT_MAKEFILE_START))

PLUGIN_LIB_EXTRA_DEPS=  $(USRP_RADR_PLUGIN_DIR)/usrp_radR_plugin.so  $(USRP_RADR_PLUGIN_DIR)/usrp_radR_plugin.h $(shell $(FIND) . -type f -regex "^.*\\.antenna\\.R$$" -printf '%p\n')
PLUGIN_LIB_EXTRA_OPTIONS= -I$(USRP_RADR_PLUGIN_DIR) -I$(USRP_INCLUDE_PATH) $(USRP_RADR_PLUGIN_DIR)/usrp_radR_plugin.so
PLUGIN_EXTRA_DIRS=antenna_db

$(eval $(AT_MAKEFILE_END))

else
$(info Skipping the usrp plugin; it is currently available only for Linux and not $(TARGET_PLATFORM).)

all:

plugin_clean:

endif
