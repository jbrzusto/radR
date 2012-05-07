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

#USRP_RADR_PLUGIN_DIR:=/c/cygwin/usr/src/gnuradio_bbprx/usrp/host/apps
#USRP_INCLUDE_PATH:=/c/cygwin/usr/src/gnuradio_bbprx/usrp/host/include
USRP_RADR_PLUGIN_DIR:=.
USRP_INCLUDE_PATH:=.

$(eval $(AT_MAKEFILE_START))

PLUGIN_LIB_EXTRA_DEPS=$(USRP_RADR_PLUGIN_DIR)/usrp_radR_plugin.dll  $(USRP_RADR_PLUGIN_DIR)/usrp_radR_plugin.h $(shell $(FIND) . -type f -regex "^.*\\.antenna\\.R$$" -printf '%p\n')
PLUGIN_LIB_EXTRA_OPTIONS= -I$(USRP_RADR_PLUGIN_DIR) -I$(USRP_INCLUDE_PATH) $(USRP_RADR_PLUGIN_DIR)/usrp_radR_plugin.dll
PLUGIN_EXTRA_DIRS=antenna_db rev4 windows_drivers windows_drivers/amd64 windows_drivers/ia64 windows_drivers/license/libusb-win32/installer_license.txt windows_drivers/x86
## FIXME: can't we statically link these:  need to change make_usrp_radR_plugin_mingw;
## these are listed in the order they should be dynamically loaded in

PLUGIN_EXTRA_TARGETS=libgcc_s_dw2-1.dll libstdc++-6.dll libboost_thread-mgw45-mt-1_46_1.dll libboost_program_options-mgw45-mt-1_46_1.dll libusrp-3-3-0-0.dll usrp_radR_plugin.dll test_usrp_bbprx.exe test_usrp_pulse_buffer.exe

$(eval $(AT_MAKEFILE_END))
endif
