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
R_MINOR_VERSION = 15
R_MINOR_SUBVERSION = 2
R_VERSION = $(R_MAJOR_VERSION).$(R_MINOR_VERSION).$(R_MINOR_SUBVERSION)

R = /usr/bin/R
RCMD = $(R) CMD
BUILD_PLATFORM_LIBOPTS = 
LOCAL_INSTALL_DIR=~/Desktop/radR
RINC =-I/home/src/R-$(R_VERSION)/include -I/home/src/R-$(R_VERSION)/src/include

BUILD_PLATFORM_CCOPTS = -fPIC
BUILD_PLATFORM_DEP = 
TARGET_PLATFORM_CONF_FILES = radR.unix.conf.R gui.unix.conf.R
TARGET_PLATFORM_RADR_FILES = radR.xbm startup.unix.R

ifeq ($(TARGET_PLATFORM),windows)
TARGET_PLATFORM_CCOPTS=-I$(GUI_DIR)/tcltk -DWin32
TARGET_PLATFORM_LIBOPTS=
TCLTK_LIB = $(R_WINDOWS_HOME)/Tcl/bin/tcl84.dll $(R_WINDOWS_HOME)/Tcl/bin/tk84.dll $(R_WINDOWS_HOME)/Tcl/bin/tcl84.dll $(R_WINDOWS_HOME)/Tcl/bin/tk84.dll
TCLTK_INC = -I/home/src/tk8.4.15/generic -I/home/src/tk8.4.15/win -I /home/src/tcl8.4.15/generic -I /home/src/tcl8.4.15/win
RLIB = $(R_WINDOWS_HOME)/bin/R.dll
NM = /usr/bin/i586-mingw32msvc-nm 
else
TCLTK_LIB = -L/usr/lib -ltcl8.5 -ltk8.5
TCLTK_INC = -I/usr/include/tcl8.5 -I/usr/include/tcl8.5/tk-private/generic  -I/usr/include/tcl8.5/tcl-private/generic
RLIB = -L/home/src/R-$(R_VERSION)/lib -lR
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


