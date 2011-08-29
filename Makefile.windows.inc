###       (tell emacs this is a Makefile: -*- Makefile -*-  )
### 
### Windows-specific Makefile for radR
###
###


#PATH:=/mingw/bin:$(PATH)
CP = /usr/bin/cp
HEAD = /bin/head
GUNZIP = /usr/bin/gunzip
LOCAL_INSTALL_DIR=~/Desktop/radR
RADR_FORK = radR/trunk
PC_PROJECT_DIR = john@pc:projects/radar/$(RADR_FORK)/
PC_PACKAGE_DIR = $(PC_PROJECT_DIR)/packages
XENEX_SDK_DIR = "c:/Documents and Settings/john/Desktop/xenexSDK/Xenex SDK 1.1.0.018 ARPA"
XENEX_INC_DIR = $(XENEX_SDK_DIR)/Include
XENEX_RUN_DIR = $(XENEX_SDK_DIR)/Debug
## versions of R for which we generate radR_X_Y_Z.bat files

SUPPORTED_R_VERSIONS := 2.5.1

PLATFORM_TARGETS = $(RADR_BINARY_PACKAGES)

# FIXME: include mkshortcut.exe in the distribution so the user can easily
# get shortcuts on her machine.
#
# Don't do this: it creates shortcuts to the path on the build machine!
# 
# $(R_BATCH_FILE_SHORTCUTS): radR.bat $(R_BATCH_FILES) run
# 	for v in $(SUPPORTED_R_VERSIONS); do\
# 	  mkshortcut --icon radR.ico --name radR$$v --show=min --desc="radR for R version $$v" ./radR$$v.bat; \
# 	done

PROGFILES:=$(shell if [ -d "/cygdrive/c/Program Files (x86)" ]; then echo "Program Files (x86)"; else echo "Program Files"; fi)
## the version of R against which to link the .dlls
R_VERSION = 2.5.1
RINC = "-I/$(PROGFILES)/R/R-$(R_VERSION)/include" 
RLIB = "-L/$(PROGFILES)/R/R-$(R_VERSION)/bin" "-L/$(PROGFILES)/R/R-$(R_VERSION)/src/gnuwin32/" -lR
NM = /usr/bin/nm
RBINDIR = "/cygdrive/c/$(PROGFILES)/R/R-$(R_VERSION)/bin"
RSRCBINDIR = /usr/src/R-$(R_VERSION)/bin
R = $(RBINDIR)/Rterm.exe
RCMD = $(RBINDIR)/Rcmd.exe

TCLTK_INC = -Ic:/cygwin/usr/src/tcl8.4.15/generic -Ic:/cygwin/usr/src/tk8.4.15/generic -Ic:/cygwin/usr/src/tcl8.4.15/win -Ic:/cygwin/usr/src/tk8.4.15/win
TCLTK_LIB = "/$(PROGFILES)/R/R-$(R_VERSION)/Tcl/bin/tk84.dll" "/$(PROGFILES)/R/R-$(R_VERSION)/Tcl/bin/tcl84.dll"
TCLTK_LIB_PATH = "/$(PROGFILES)/R/R-$(R_VERSION)/Tcl/bin"
## the following path must be given to compile tcl/tk headers
## it is included using the 'idirafter' option so that only the
## X11 subdirectory is used
X11_INC_PATH=c:/cygwin/usr/include
BUILD_PLATFORM_LIBOPTS = -Wl,-rpath,../libs,--kill-at,--enable-stdcall-fixup,--enable-auto-import -L$(RADR_PACKAGE_INSTALL_DIR)/extmat/libs/ -lextmat
## the final -I is to bring in zlib.h from the cygwin headers
BUILD_PLATFORM_CCOPTS = -DWin32 -idirafter $(X11_INC_PATH) "-I../include"

LIBZ_INC = -I /cygwin/usr/include
LIBZ_LIB = ../libs/zlib1.dll
## on windows, we need to link against shared libraries explicitly
BUILD_PLATFORM_DEP = $(RADR_PACKAGE_INSTALL_DIR)/extmat/libs/extmat$(SHLIB_SUFFIX)
TARGET_PLATFORM_CONF_FILES = radR.windows.conf.R gui.windows.conf.R
TARGET_PLATFORM_RADR_FILES = radR.ico \
    $(R_BATCH_FILES) \
    startup.windows.R \
    plugins/seascan$(SHLIB_SUFFIX) \
    $(RADR_LIB_DIR)/pthreadGC2$(SHLIB_SUFFIX) \
    $(RADR_LIB_DIR)/pthreadGC2$(SHLIB_SUFFIX) \

TAGS: 
	scp $(PC_PROJECT_DIR)/TAGS .
