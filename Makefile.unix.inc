###      (tell emacs this is a makefile script:     -*- Makefile -*- )
### 
### Unix-specific Makefile for radR
###
### You need a source tree in which you have compiled the version of R which you
### will be using to run radR.
###
### You also need these packages (on Debian based systems):
###
###   libX11-dev
###   libXext-dev
###   libtk-dev
###   libtcl-dev
###   zlib-dev
###

SHLIB_SUFFIX_unix = .so
EXE_SUFFIX_unix = 
CP = /bin/cp
HEAD = /bin/head
## version of R you are building against
R_MAJOR_VERSION = 2
R_MINOR_VERSION = 5
R_MINOR_SUBVERSION = 1
R_VERSION = $(R_MAJOR_VERSION).$(R_MINOR_VERSION).$(R_MINOR_SUBVERSION)

R = /usr/bin/R-$(R_MAJOR_VERSION).$(R_MINOR_VERSION).$(R_MINOR_SUBVERSION)
RCMD = $(R) CMD
BUILD_PLATFORM_LIBOPTS = 
LOCAL_INSTALL_DIR=~/Desktop/radR
RINC =-I/usr/src/R-$(R_VERSION)/include -I/usr/src/R-$(R_VERSION)/src/include

BUILD_PLATFORM_CCOPTS = -march=k8
BUILD_PLATFORM_DEP = 
TARGET_PLATFORM_CONF_FILES = radR.unix.conf.R gui.unix.conf.R
TARGET_PLATFORM_RADR_FILES = radR.xbm startup.unix.R

ifeq ($(TARGET_PLATFORM),windows)
TARGET_PLATFORM_CCOPTS=-I$(GUI_DIR)/tcltk -DWin32
TARGET_PLATFORM_LIBOPTS=
TCLTK_LIB = $(R_WINDOWS_HOME)/Tcl/bin/tcl84.dll $(R_WINDOWS_HOME)/Tcl/bin/tk84.dll $(R_WINDOWS_HOME)/Tcl/bin/tcl84.dll $(R_WINDOWS_HOME)/Tcl/bin/tk84.dll
TCLTK_INC = -I/usr/src/tk8.4.15/generic -I/usr/src/tk8.4.15/win -I /usr/src/tcl8.4.15/generic -I /usr/src/tcl8.4.15/win
RLIB = $(R_WINDOWS_HOME)/bin/R.dll
NM = /usr/bin/i586-mingw32msvc-nm 
else
TCLTK_LIB = -L/usr/lib -ltcl8.4 -ltk8.4
TCLTK_INC = -I/usr/include/tcl8.4 -I/usr/include/tcl8.4/tk-private/generic  -I/usr/include/tcl8.4/tcl-private/generic
RLIB = -L/usr/src/R-$(R_VERSION)/lib -lR
NM = /usr/bin/nm
LIBZ_LIB = -L/usr/lib -lz -Wl,-rpath=/usr/lib
endif


PLATFORM_TARGETS = $(RADR_BINARY_PACKAGES)

## for linking with xlib and its extensions
XLIB := -L/usr/X11R6/lib -lX11 -lXext

## FIXME: For now, we build strictenv only on unix (it has no C code)
## The build on windows copies it over from the PC

%.Rout.save : %.R
	R --vanilla < $< > $@

TAGS: FORCE
	rm -f TAGS
	etags.emacs22 --declarations --members include/*.h main/*.[ch] gui/*.[ch] plugins/*/*.[ch] packages/*/src/*.[ch] fragments/*.[ch]
	build-scripts/rtags --append main/*.R gui/*.R plugins/*/*.R packages/*/R/*.R fragments/*.R

weathergif$(SHLIB_SUFFIX): weathergif.c $(RADR_INC_FILES)  $(MAKEFILE_DEP)
	$(CC) $(CCOPTS) $(RINC) $(TCLTK_INC) -o weathergif$(SHLIB_SUFFIX) weathergif.c $(TCLTK_LIB) -Wl,-rpath,. -Wl,-rpath,extmat/libs -Wl,-rpath,$(TCLTK_LIB) -lm -L. $(RLIB)  $(LIBOPTS)


