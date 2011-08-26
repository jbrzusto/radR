## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.  To make this plugin, do 
##   /bin/make simrad
## from the radR directory.

$(eval $(AT_MAKEFILE_START))

PLUGIN_EXTRA_R_FILES = simrad.windows.conf.R simrad.unix.conf.R

ifeq ("$(TARGET_PLATFORM)", "windows")
## for making the multicast receiver:

PLUGIN_EXTRA_TARGETS = mcreceive.exe

mcreceive.exe: mcreceive.c 
	gcc -O2 -o mcreceive.exe mcreceive.c -lWS2_32

endif

$(eval $(AT_MAKEFILE_END))
