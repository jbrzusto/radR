## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.  To make this plugin, do 

## for making the (so far) Windows-specific seascan shared library:

ifeq ("$(TARGET_PLATFORM)", "windows")

$(eval $(AT_MAKEFILE_START))

ifeq ("$(BUILD_PLATFORM)", "windows")
## path to the Xenex SDK on the windows build host
XENEX_SDK_DIR=/cygdrive/c/RTI\ SDK\ 1.5.0.008
MINGW_XENEX_SDK_DIR=c:/RTI\ SDK\ 1.5.0.008
else
## path to the Xenex SDK on the unix build host
XENEX_SDK_DIR=/home/john/Desktop/CurrentSDK
endif

XENEX_INC_DIR=$(MINGW_XENEX_SDK_DIR)/Include

## use the debug or development version of the library as appropriate
ifeq ("$(VERSION)", "DEBUG")
XENEX_RUN_DIR=$(MINGW_XENEX_SDK_DIR)/Debug
else
XENEX_RUN_DIR=$(MINGW_XENEX_SDK_DIR)/Release
endif

XENEX_DYNLIB=$(XENEX_RUN_DIR)/CANRib.dll
ORIG_CANSTAR_FILE=$(XENEX_SDK_DIR)/Include/CANSTAR.H
CANSTAR_IMPLIB=$(XENEX_SDK_DIR)/Lib/CANRib.lib
CANSTAR_NAMEFILE=CANRib_names.txt

## for making the (so far) Windows-specific xenex shared library:

xenex$(SHLIB_SUFFIX): xenex.c xenex.h myCANSTAR.h

#	$(CC) -I../../include -I. -I$(MINGW_XENEX_INC_DIR) $(CCOPTS) $(RINC) -o xenex$(SHLIB_SUFFIX) xenex.c -Wl,--enable-stdcall-fixup -Wl,--enable-auto-import  -lm $(RLIBDIR) -lR $(LIBOPTS)

## generate a gcc-compatible version of the include file by
## adding attributes to alias the decorated name and to declare functions 'stdcall'

myCANSTAR.h: $(ORIG_CANSTAR_FILE) $(CANSTAR_NAMEFILE)
	gawk -f makeMyCANSTAR.awk $(CANSTAR_NAMEFILE) $(ORIG_CANSTAR_FILE) > $@

## compile a translation table of undecorated to decorated names, by dumping the import library with "nm"
$(CANSTAR_NAMEFILE): $(ORIG_CANSTAR_FILE) $(CANSTAR_IMPLIB)
	$(NM) $(CANSTAR_IMPLIB)  | grep " T _CSAPI" | gawk '{x=gensub("@.*","", "g", $$3); printf "%s\t%s\n", $$3, substr(x, 2)}' > $@


## Include the pthreads library

PTHREADS_LIB = $(wildcard $(RADR_LIB_DIR)/pthreadGC2*.dll)
PLUGIN_LIB_EXTRA_DEPS= $(PTHREADS_LIB) myCANSTAR.h
PLUGIN_LIB_EXTRA_OPTIONS=-I$(XENEX_INC_DIR) $(PTHREADS_LIB) $(XENEX_DYNLIB)

## $(eval $(call DEF_EXTRA_LIB, xenexutil, xenexutil.c, xenexutil.c, $(PLUGIN_LIB_EXTRA_OPTIONS)))

$(eval $(AT_MAKEFILE_END))


else
$(info Skipping the xenex plugin; it is currently available only for Windows.)

all:

plugin_clean:

endif



