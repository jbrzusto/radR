###
### Makefile for radR - requires gnu make version at least 3.80 
###
### *** To build on Windows, make sure to use the Rtools package, available
###     from the web, and the cygwin environment, which has the required >= 3.80
###     version of gnu make.
###
###     Your path should look like:
###     PATH=/cygdrive/c/Rtools/bin:/cygdrive/c/Rtools/perl/bin:/cygdrive/c/Rtools/MinGW/bin:
###          /cygdrive/c/Program Files/R/R-X.Y.Z/bin:/cygdrive/c/Program Files/R/R-X.Y.Z/Tcl/bin:/usr/local/bin:/usr/bin:/bin
###
###     and you should use /bin/make to get make version >= 3.80
###

### Determine the git revision for this build

RADR_REVISION := $(shell git rev-parse --abbrev-ref HEAD)-$(shell git log --format=format:%ci | gawk '{print gensub("-","","g",$$1) gensub(":","","g",$$2);exit(0);}')

### Determine the platform.  You can specify the linux->windows cross compilation
### by invoking like this:
###
###    make TARGET_PLATFORM=windows
###
### on unix, in which case the installation directory will be install_windows


### Figure out the build platform
BUILD_PLATFORM=$(if $(findstring Windows_NT,$(OS)),windows,unix)
BUILD_PLATFORM_MAKEFILE = Makefile.$(BUILD_PLATFORM).inc

### unless the user has specified a target platform, it is the same as the build platform
TARGET_PLATFORM:=$(if $(TARGET_PLATFORM),$(TARGET_PLATFORM),$(BUILD_PLATFORM))

SRC_DIST_NAME := radR-$(RADR_REVISION)
SRC_DIST_FILE := $(SRC_DIST_NAME)-src.tar
SRC_DIST_FILE_COMP := $(SRC_DIST_FILE).gz
BIN_DIST_NAME := radR-$(RADR_REVISION)-$(TARGET_PLATFORM)
BIN_DIST_FILE := $(BIN_DIST_NAME).zip

### For building under windows/mingw, we need to prefix
### the toplevel path:

ifeq ($(BUILD_PLATFORM),windows)
## Note: there must be a symbolic link from /cygwin to / as seen from the cygwin file
## tree, so that the following will work in both mingw and cygwin file spaces, as it must.
## (FIXME: get this Makefile working with the mingw Make, so that all filenames are 
## in the mingw file space!)
TOPLEVEL_PATH_PREFIX = /cygwin
else
TOPLEVEL_PATH_PREFIX = 
endif

#############################################################
###                                                       ###
###     START OF CUSTOMIZATION SECTION                    ###
###                                                       ###
#############################################################

## Choose which version to build.  The "_BCHECK" versions, currently
## available only on unix, use a bounds-checking version of gcc.  See:
## http://sourceforge.net/projects/boundschecking/ Running a
## bounds-checking version of radR is much faster than using valgrind,
## and can detect incorrect pointers when they are created.  However,
## valgrind can detect errors in external code compiled without bounds
## checking, so both methods should be used for testing.
## Unfortunately, both are available only for unix, so the seascan
## module has not been fully tested this way.  The seascanarch module
## is available on unix and has been tested.
##
##
## To run radR with valgrind:
##
##    R -d "valgrind --tool=memcheck --leak-check=full" --vanilla
##    (... long wait with many messages...)
##    source("radR.R")
##    (... long wait with many messages...)
##


#
# One of the following should be specified on the command line
# e.g. make VERSION=DEBUG
#
# The default is VERSION=PRODUCTION
#
# VERSION=DEBUG
# VERSION=PRODUCTION
# VERSION=BCHECK_DEBUG
# VERSION=BCHECK_PRODUCTION
# VERSION=PROFILE

VERSION := $(if $(VERSION),$(VERSION),PRODUCTION)

# One of the following should be specified on the command line
# e.g. make BITS_PER_SAMPLE=8
# 
# The default is BITS_PER_SAMPLE=12
#
# BITS_PER_SAMPLE = 8
# BITS_PER_SAMPLE = 12
# BITS_PER_SAMPLE = 16

## force make to export all variables to recursive makes used to build plugins
.EXPORT_ALL_VARIABLES:

BITS_PER_SAMPLE := $(if $(BITS_PER_SAMPLE),$(BITS_PER_SAMPLE),12)

RADR_TOPLEVEL_DIR := $(TOPLEVEL_PATH_PREFIX)$(shell pwd)
RADR_PACKAGE_DIR := $(RADR_TOPLEVEL_DIR)/packages
RADR_INSTALL_DIR := $(RADR_TOPLEVEL_DIR)/install

ifeq ($(BUILD_PLATFORM),windows)
CC_TO_USE=gcc
STRIP=strip
else
 ifeq ($(TARGET_PLATFORM),unix)
CC_TO_USE=gcc
STRIP=/usr/bin/strip
else
## target platform is windows, build platform is unix
CC_TO_USE=i586-mingw32msvc-gcc -b i586-mingw32msvc
RADR_INSTALL_DIR := $(RADR_TOPLEVEL_DIR)/install_windows
STRIP=/usr/bin/i586-mingw32msvc-strip
endif
endif

RADR_PACKAGE_INSTALL_DIR := $(RADR_INSTALL_DIR)/packages

CC_DEBUG=$(CC_TO_USE)
CC_PRODUCTION=$(CC_DEBUG)
CC_BCHECK_DEBUG=/usr/src/gcc-3.4.4-bounds-checking/gcc/xgcc
CC_BCHECK_PRODUCTION=$(CC_BCHECK_DEBUG)
CC_PROFILE=$(CC_DEBUG)

R_WINDOWS_HOME=/home/john/.wine/drive_c/Program\ Files/R/R-2.5.1

## commands
CP      = /bin/cp
CHMOD   = /bin/chmod
FIND	= /usr/bin/find
GREP	= /bin/grep
INSTALL	= /usr/bin/install
MV	= /bin/mv
MYMAKE 	= $(MAKE) $(SUBMAKEOPTS)  --no-builtin-rules -C
PROTO	= /usr/bin/cproto   # don't call this CPROTO, because cproto reads options from an environment symbol of that name
RM	= /bin/rm
SCP     = scp
SED     = /bin/sed
SSH     = ssh
TAR	= /bin/tar
ZIPPROG = /usr/bin/zip  # don't call this ZIP, since it is exported to the shell, and the zip program uses it as a variable of options
UNZIPPROG = /usr/bin/unzip

R_FILE_CHECKER=$(BUILDSCRIPTS_DIR)/filecheck.R
RPP=$(BUILDSCRIPTS_DIR)/Rpp

PLUGIN_DIR		= $(RADR_TOPLEVEL_DIR)/plugins
PLUGIN_INSTALL_DIR	= $(RADR_INSTALL_DIR)/plugins
PALETTE_DIR		= $(RADR_TOPLEVEL_DIR)/palettes
PALETTE_INSTALL_DIR	= $(RADR_INSTALL_DIR)/palettes
ANTENNA_DIR		= $(RADR_TOPLEVEL_DIR)/antennas
ANTENNA_INSTALL_DIR	= $(RADR_INSTALL_DIR)/antennas
ZONE_DIR		= $(RADR_TOPLEVEL_DIR)/zones
ZONE_INSTALL_DIR	= $(RADR_INSTALL_DIR)/zones
GUI_DIR			= $(RADR_TOPLEVEL_DIR)/gui
RADR_INCLUDE_DIR	= $(RADR_TOPLEVEL_DIR)/include
RADR_MAIN_DIR           = $(RADR_TOPLEVEL_DIR)/main
BUILDSCRIPTS_DIR	= $(RADR_TOPLEVEL_DIR)/build-scripts
RADR_LIB_DIR            = $(RADR_TOPLEVEL_DIR)/libs

## makefiles in subdirectories should begin with the line $(eval $(AT_MAKEFILE_START))
AT_MAKEFILE_START = include $(RADR_TOPLEVEL_DIR)/Makefile.inc
CCOPTS_COMMON=-DBITS_PER_SAMPLE=$(BITS_PER_SAMPLE) -Wall
CCOPTS_DEBUG=$(CCOPTS_COMMON) -g3 -DRADR_DEBUG
CCOPTS_PRODUCTION=$(CCOPTS_COMMON) -O3 -ffast-math -fgcse-sm -fgcse-las -fgcse-after-reload -march=pentium4 -mfpmath=sse -msse2
CCOPTS_BCHECK=$(CCOPTS_COMMON) -B/usr/src/gcc-3.4.4-bounds-checking/gcc/ -fbounds-checking /usr/src/gcc-3.4.4-bounds-checking/gcc/bounds/lib/libboundscheck.a
CCOPTS_BCHECK_DEBUG= $(CCOPTS_BCHECK) $(CCOPTS_DEBUG)
CCOPTS_BCHECK_PRODUCTION=$(CCOPTS_BCHECK) $(CCOPTS_PRODUCTION)
CCOPTS_PROFILE=$(CCOPTS_COMMON) -g3 -O3 -ffast-math -fgcse-sm -fgcse-las -fgcse-after-reload -march=pentium4 -mfpmath=sse -msse2
LIBOPTS_BCHECK=-L/usr/src/gcc-3.4.4-bounds-checking/gcc/bounds/lib/ -lboundscheck
LIBOPTS_BCHECK_DEBUG = $(LIBOPTS_BCHECK)
LIBOPTS_BCHECK_PRODUCTION = $(LIBOPTS_BCHECK)
SCP_UPLOAD_USER=john@discovery.acadiau.ca
SCP_BACKUP_DEST=$(SCP_UPLOAD_USER):
SCP_UPLOAD_DIR=/home/www/html/radR/
SCP_UPLOAD_DEST=$(SCP_UPLOAD_USER):$(SCP_UPLOAD_DIR)
RPP_OPTS_DEBUG='DEBUG=1'
RPP_OPTS_PRODUCTION='DEBUG=0'
RPP_OPTS_BCHECK_DEBUG='DEBUG=1'
RPP_OPTS_BCHECK_PRODUCTION='DEBUG=0'

#############################################################
###                                                       ###
###     END OF CUSTOMIZATION SECTION                      ###
###                                                       ###
#############################################################
##MAKEFILE_DEP = Makefile $(BUILD_PLATFORM_MAKEFILE)
MAKEFILE_DEP = 

$(eval $(AT_MAKEFILE_START))

all: radR_all radR_toplevel

## toplevel targets to place in the install directory

radR_toplevel: $(RADR_INSTALL_DIR)/LICENSE.TXT $(RADR_INSTALL_DIR)/VERSION.TXT $(RADR_INSTALL_DIR)/00README.TXT

$(RADR_INSTALL_DIR)/LICENSE.TXT: LICENSE.TXT
	$(CP) -f $< $@

$(RADR_INSTALL_DIR)/VERSION.TXT: FORCE
	echo "This is revision $(RADR_REVISION) of radR." > $@
	git log --format=short | $(HEAD) -5l >> $@
	echo "Built for R version $(R_VERSION)" >> $@

$(RADR_INSTALL_DIR)/00README.TXT: 00README.TXT
	$(CP) -f $< $@

include $(BUILD_PLATFORM_MAKEFILE)

ifneq ($(BUILD_PLATFORM),$(TARGET_PLATFORM))
## include the cross-compilation Makefile
## include Makefile.xc.inc
endif


ifneq ($(TARGET_PLATFORM),unix)
binary:
	$(RM) -f $(BIN_DIST_FILE) ; \
	cd $(RADR_INSTALL_DIR)       ; \
	$(ZIPPROG) -r ../$(BIN_DIST_FILE) . -x ".git*"
else
binary:

endif

local_install: radR_all radR_toplevel
	mkdir -p $(LOCAL_INSTALL_DIR); \
	$(FIND) $(LOCAL_INSTALL_DIR) -iname "*.Rdata" -delete; \
	(cd $(RADR_INSTALL_DIR); $(TAR) -cf - .) | (cd $(LOCAL_INSTALL_DIR); $(TAR) -xpf -)

### Determine names of plugins for generating extra targets:

PLUGIN_NAMES := $(shell $(FIND) plugins -maxdepth 1 -type d -regex '^plugins/[a-zA-Z0-9_]+' -printf '%f\n')

define create_plugin_target
$(1): 
	$(MYMAKE) plugins $(1)
endef

$(foreach plugin, $(PLUGIN_NAMES), $(eval $(call create_plugin_target, $(plugin))))

### Determine names of packages for generating extra targets:

PACKAGE_NAMES := $(shell $(FIND) packages -maxdepth 1 -type d -regex '^packages/[a-zA-Z0-9_]+' -printf '%f\n')

define create_package_targets
$(1): 
	$(MYMAKE) packages $(addprefix $(RADR_PACKAGE_INSTALL_DIR)/, $(1))

$(1)_clean: 
	$(MYMAKE) packages $(1)_clean
endef

$(foreach package, $(PACKAGE_NAMES), $(eval $(call create_package_targets, $(package))))


PROTOS = $(RADR_INCLUDE_DIR)/radRprot.h

protos:
	$(MYMAKE) main $(PROTOS)

#############################################################
###                                                       ###
###    platform-dependent variables                       ###
###                                                       ###
#############################################################

## unix
SHLIB_SUFFIX_unix = .so
EXE_SUFFIX_unix = 
SHELL_SCRIPT_SUFFIX_unix = .sh

## windows 
SHLIB_SUFFIX_windows = .dll
EXE_SUFFIX_windows = .exe
SHELL_SCRIPT_SUFFIX_windows = .bat

## current target platform
SHLIB_SUFFIX=$(SHLIB_SUFFIX_$(TARGET_PLATFORM))
EXE_SUFFIX=$(EXE_SUFFIX_$(TARGET_PLATFORM))
SHELL_SCRIPT_SUFFIX=$(SHELL_SCRIPT_SUFFIX_$(TARGET_PLATFORM))

.PHONY: FORCE

INCLUDE_DIRS = $(addprefix -I, $(RADR_INCLUDE_DIR) $(wildcard $(RADR_PACKAGE_DIR)/*/src))
CC=$(CC_$(VERSION))
CCOPTS = $(CCOPTS_$(VERSION)) $(BUILD_PLATFORM_CCOPTS) $(TARGET_PLATFORM_CCOPTS) $(INCLUDE_DIRS)
LIBOPTS = -shared $(LIBOPTS_$(VERSION)) $(BUILD_PLATFORM_LIBOPTS) $(TARGET_PLATFORM_LIBOPTS)

RADR_BINARY_PACKAGES = $(addsuffix $(BINARY_PACKAGE_SUFFIX),$(RADR_PACKAGES))

SUBDIRS = main gui libs plugins antennas palettes scripts zones # utils
SUBDIRS_CLEAN = $(addsuffix _clean,$(SUBDIRS))

RADR_INC_FILES = $(addprefix $(RADR_INCLUDE_DIR)/, patchify.h radR.h radRshared.h extmatimg.h) \
	         $(addprefix $(RADR_PACKAGE_DIR)/, extmat/src/extmat.h)


.PHONY: subdirs $(SUBDIRS) subdirs_clean packages

subdirs: $(SUBDIRS)

subdirs_clean: main_clean $(SUBDIRS_CLEAN)

$(SUBDIRS):
	$(MYMAKE) $@

packages:
	@echo 'Making packages'
	$(MYMAKE) packages

packages_clean:
	$(MYMAKE) packages clean

radR_all:packages $(TARGET_PLATFORM_TARGETS) subdirs

clean:	install_clean subdirs_clean

clean_all: clean packages_clean

conf_clean: 
	rm -f $(RADR_INSTALL_DIR)/main/*.conf.update.R
	rm -f $(RADR_INSTALL_DIR)/main/*.conf.R
	rm -f $(RADR_INSTALL_DIR)/main/*.RData               ## unused so far
	rm -f $(RADR_INSTALL_DIR)/*.RData
	rm -f $(RADR_INSTALL_DIR)/gui/*.conf.update.R
	rm -f $(RADR_INSTALL_DIR)/main/*.conf.R
	rm -f $(RADR_INSTALL_DIR)/gui/*.RData                ## unused so far
	rm -f $(RADR_INSTALL_DIR)/plugins/*/*.conf.update.R
	rm -f $(RADR_INSTALL_DIR)/plugins/*/*.conf.R
	rm -f $(RADR_INSTALL_DIR)/plugins/*/*.RData
	rm -f $(RADR_INSTALL_DIR)/.Rprofile

install_clean: FORCE
	rm -rf $(RADR_INSTALL_DIR)/*

reinstall:  FORCE
	make install_clean
	make


$(SUBDIRS_CLEAN):
	$(MYMAKE) $(subst _clean,,$@) clean

# bkup_wiki: 
# 	wget -nH -P wikibackup -r -np -R "index.php?*,Special:*,Template:*,User:*" -X "editor,attachments,skins" http://radr.wiki.com

CHANGELOG: FORCE
	git log --format=medium > CHANGELOG

source_dist: CHANGELOG
	git archive --format=tar --prefix=$(SRC_DIST_NAME)/ HEAD > $(SRC_DIST_FILE)
	tar -rf$(SRC_DIST_FILE) CHANGELOG
	gzip -f < $(SRC_DIST_FILE) > $(SRC_DIST_FILE_COMP)
	rm -f $(SRC_DIST_FILE)

upload: binary source_dist
	$(SCP) $(SRC_DIST_FILE_COMP) $(BIN_DIST_FILE) $(SCP_UPLOAD_DEST)
	rm -f *LATEST_VERSION_IS*
	echo "Go back and click on the actual .tgz or .zip file..." > LATEST_VERSION_IS_$(RADR_REVISION)
	$(SSH) $(SCP_UPLOAD_USER) /bin/rm -f $(SCP_UPLOAD_DIR)/LATEST_VERSION_IS*
	$(SCP) CHANGELOG 00README.TXT TODO LATEST_VERSION_IS* $(SCP_UPLOAD_DEST)

upload_stable: binary source_dist CHANGELOG_STABLE
	rm -f *LATEST_STABLE_VERSION_IS*; \
	export TODAY=`date +_%Y_%m_%d`; \
	echo "Go back and click on the actual .tgz or .zip file..." > LATEST_STABLE_VERSION_IS$${TODAY}; \
	$(SCP) radR$(RADR_REVISION)windows.zip $(SCP_UPLOAD_DEST)/radRstablewindows$${TODAY}.zip; \
	$(SCP) radR$(RADR_REVISION)source.tgz $(SCP_UPLOAD_DEST)/radRstablesource$${TODAY}.tgz;	\
	$(SSH) $(SCP_UPLOAD_USER) /bin/rm -f $(SCP_UPLOAD_DIR)/LATEST_STABLE_VERSION_IS*; \
	$(SCP) CHANGELOG_STABLE 00README.TXT LATEST_STABLE_VERSION_IS* $(SCP_UPLOAD_DEST)

utils/bmdebug$(EXE_SUFFIX): utils/bmdebug.c 
	$(CC) $(CCOPTS) utils/bmdebug.c -o utils/bmdebug$(EXE_SUFFIX) $(RINC) -lm

# sscan$(EXE_SUFFIX): sscan.c sscan.h
# 	$(CC) $(CCOPTS) -I plugins sscan.c -o sscan$(EXE_SUFFIX) -lm


# ######## what follows is for testing and is not part of radR

# ## In order to create unix structs matching the 
# ## MS-Windows versions of the seascan-related structures, we create
# ## the file myIDLTYPES.H using the findoffsets.awk script and
# ## the findoffsets.c stub
# ##
# ##   NOTE!! the following two cases are not dealt with automatically
# ##   (the first is possibly a bug in the offsetof operator,
# ##   the second is due to my own laziness in not having findoffsets deal 
# ##   correctly with end-of-struct padding), and must be edited
# ##   in the file myIDLTYPES.H to look like this:
# ##
# ## typedef struct _ARCHIVE_LABEL { 
# ##   char SystemName[80]; // set as Version 3 - MRI data tape, see below 
# ##   time_t TimeStamp;   // initial time stamp, used for recognition 
# ##   MS_STRUCT_FILLER(4b, 4);    <--- insert this line manually
# ##   LARGE_INTEGER directoryPosition; // used for disk recording to locate directory 
# ## } MS_STRUCT  ARCHIVE_LABEL;  
# ##
# ## typedef struct _DISK_DIRECTORY_ENTRY {
# ##   LARGE_INTEGER Position; // queried from storage device
# ##   time_t TimeStamp;    // copied from DataHeader.Time in current image
# ##   MS_STRUCT_FILLER(10, 4);  <--- insert this line manually
# ## } MS_STRUCT  DISK_DIRECTORY_ENTRY; 
# ##
# # myIDLTYPES.H: findoffsets.exe
# # 	./findoffsets.exe > myIDLTYPES.H

# # findoffsets.exe: findoffsets.awk findoffsets.c IDLTYPES.H
# # 	gawk -f findoffsets.awk IDLTYPES.H > findoffsets.h
# # 	$(CC) -o findoffsets.exe findoffsets.c

# # testpatch$(EXE_SUFFIX): testpatch.c patchify.c patchify.h $(MAKEFILE_DEP)
# # 	$(CC) -Wall -g3 $(RINC) -o testpatch$(EXE_SUFFIX) testpatch.c patchify.c -lgdi32 $(RLIB)

# # testseascan: testseascan.c seascan.c seascan.h $(RADR_INC_FILES) IDLCONST.H IDLTYPES.H $(MAKEFILE_DEP)
# # 	$(CC) $(CCOPTS) $(RINC) -o seascan$(SHLIB_SUFFIX) testseascan.c seascan.c  -L. -lClient12 $(RLIB)

# # testwingui: windowsgui.c windowsgui.h $(MAKEFILE_DEP)
# # 	$(CC) $(CCOPTS) $(RINC) -o testwingui.o windowsgui.c radRgui.c $(RLIB) -lgdi32

# # rs$(EXE_SUFFIX):	rs.c libClient12.a
# # 	$(CC) -o rs$(EXE_SUFFIX) -g rs.c -L. -lClient12 $(RLIB) "-Ic:/Program Files/R/R-2.3.0/include"

# # zarch$(EXE_SUFFIX): zarch.c
# # 	$(CC) -g -o zarch$(EXE_SUFFIX) zarch.c -I. -L. -lz

# # idwin$(EXE_SUFFIX): idwin.c
# # 	$(CC) -o idwin$(EXE_SUFFIX) idwin.c -lgdi32

# # arch$(EXE_SUFFIX): arch.c libClient12.a
# # 	$(CC) -Wall  -g   -o arch$(EXE_SUFFIX) arch.c -L. -lClient12 -lgdi32 $(RLIB) "-Ic:/Program Files/R/R-2.3.0/include"

# # test$(SHLIB_SUFFIX): test.c $(RADR_INC_FILES) 
# # 	$(CC) $(CCOPTS) $(RINC) $(TCLTK_INC) -o test$(SHLIB_SUFFIX) test.c  -Wl,-rpath,. -Wl,-rpath,extmat/libs -Wl,-rpath,$(TCLTK_LIB) -lm -L. $(RLIB)  $(LIBOPTS)


