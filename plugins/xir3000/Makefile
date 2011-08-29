## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.  To make this plugin, do 

## for making the (so far) Windows-specific xir3000 shared library:

ifeq ("$(TARGET_PLATFORM)", "windows")

$(eval $(AT_MAKEFILE_START))

## version of the RTI SDK
RTI_SDK_VERSION:=1.5.0.012

ifeq ("$(BUILD_PLATFORM)", "windows")
## path to the RTI SDK on the windows build host
RTI_SDK_DIR=/cygdrive/c/RTI\ SDK\ $(RTI_SDK_VERSION)
MINGW_RTI_SDK_DIR=c:/RTI\ SDK\ $(RTI_SDK_VERSION)
else
## path to the RTI SDK on the unix build host
RTI_SDK_DIR=/home/john/rti/RTI SDK $(RTI_SDK_VERSION)
MINGW_RTI_SDK_DIR=$(RTI_SDK_DIR)
endif

## minimal subset of files from the SDK required for use of
## the XIR3000 with radR, and which we may redistribute

REQUIRED_SDK_FILES = usbvp* \
		     FW/* \
	             CANStar.ant \
                     CANRib.dll \
                     NMEA.PRO \
                     Arpa.dll \
                     arpa.cfg

XIR3000_INC_DIR=$(MINGW_RTI_SDK_DIR)/Include

## use the debug or development version of the library as appropriate
ifeq ("$(VERSION)", "DEBUG")
XIR3000_RUN_DIR=$(MINGW_RTI_SDK_DIR)/Debug
else
XIR3000_RUN_DIR=$(MINGW_RTI_SDK_DIR)/Release
endif

XIR3000_DYNLIB=SDK/CANRib.dll
ORIG_CANSTAR_FILE=$(RTI_SDK_DIR)/Include/CANSTAR.H
MINGW_ORIG_CANSTAR_FILE=$(MINGW_RTI_SDK_DIR)/Include/CANSTAR.H
CANSTAR_IMPLIB=$(RTI_SDK_DIR)/Lib/CANRib.lib
CANSTAR_NAMEFILE=CANRib_names.txt

## for making the (so far) Windows-specific xir3000 shared library:

xir3000$(SHLIB_SUFFIX): xir3000.c xir3000.h myCANSTAR.h MAKE_SDK_DIR

#	$(CC) -I../../include -I. -I$(MINGW_XIR3000_INC_DIR) $(CCOPTS) $(RINC) -o xir3000$(SHLIB_SUFFIX) xir3000.c -Wl,--enable-stdcall-fixup -Wl,--enable-auto-import  -lm $(RLIBDIR) -lR $(LIBOPTS)

## generate a gcc-compatible version of the include file by
## adding attributes to alias the decorated name and to declare functions 'stdcall'

myCANSTAR.h: $(ORIG_CANSTAR_FILE) $(CANSTAR_NAMEFILE)
	$(CP) $(MINGW_ORIG_CANSTAR_FILE) CANSTAR.h
## the following is needed for now to fix a couple of problems with the original file
	patch < myCANSTAR.h_patches
	gawk -f makeMyCANSTAR.awk "$(CANSTAR_NAMEFILE)" CANSTAR.h > $@

## compile a translation table of undecorated to decorated names, by dumping the import library with "nm"
$(CANSTAR_NAMEFILE): $(ORIG_CANSTAR_FILE) $(CANSTAR_IMPLIB)
	$(NM) $(CANSTAR_IMPLIB)  | grep " T _CSAPI" | gawk '{x=gensub("@.*","", "g", $$3); printf "%s\t%s\n", $$3, substr(x, 2)}' > $@


## Include the pthreads library

PTHREADS_LIB = $(wildcard $(RADR_LIB_DIR)/pthreadGC2*.dll)

PLUGIN_LIB_EXTRA_DEPS= $(PTHREADS_LIB) myCANSTAR.h
PLUGIN_LIB_EXTRA_OPTIONS=-I$(XIR3000_INC_DIR) $(PTHREADS_LIB) $(XIR3000_DYNLIB)
PLUGIN_EXTRA_DIRS := SDK SDK/FW

## Make the minimal SDK subfolder

.PHONY: FORCE

MAKE_SDK_DIR: FORCE
	TARGDIR=$$(PWD); \
	cd $(RTI_SDK_DIR)/Release; \
	for f in $(REQUIRED_SDK_FILES); do $(CP) "$$f" "$$TARGDIR/SDK/$$f"; done

$(eval $(AT_MAKEFILE_END))

else
$(info Skipping the xir3000 plugin; it is currently available only for Windows and not $(TARGET_PLATFORM).)

all:

plugin_clean:

endif
