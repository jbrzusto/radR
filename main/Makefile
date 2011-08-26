##
## Makefile for the main radR components
##


ifeq ($(TARGET_PLATFORM), windows)
R_BATCH_FILES := $(addprefix $(RADR_INSTALL_DIR)/,$(patsubst %,../radR%.bat,$(SUPPORTED_R_VERSIONS)) radRcommon.bat)
endif

main: all $(R_BATCH_FILES)

$(eval $(AT_MAKEFILE_START))

RADR_SOURCE_FILES :=            \
	extmatimg.c  		\
	patchify.c 		\
	pulses.c                \
	radR.c 			\
	radRvars.c 		\
	random.c 		\
	scancvt.c               \
	statefun.c

TKIMGPNG_SOURCE_FILES :=        \
	tkImgPNG.c              \
	tkImgPNGInit.c          \
	R_tkImgPNG.c

R_FILES := 			\
	batchparm.R		\
	process_one.R		\
	radR.conf.R             \
	radRmain.R 		\
	radR.R 			\
	radR.Rprofile.R 	\
	radR.unix.conf.R 	\
	radRutil.R 		\
	radR.windows.conf.R 	\
	startup.unix.R 		\
	startup.windows.R 	

RADR_LIB     := radR$(SHLIB_SUFFIX)
TKIMGPNG_LIB := R_tkImgPNG$(SHLIB_SUFFIX)

EXTRA_TARGETS = $(RADR_LIB) $(TKIMGPNG_LIB) 
EXTRA_PRECIOUS_TARGETS = rbatch rbatch.bat

$(eval $(call DEF_INST_ALIAS,radRshared.h,../include/radRshared.h))
$(eval $(call DEF_INST_ALIAS,../rbatch,rbatch))
$(eval $(call DEF_INST_ALIAS,../rbatch.bat,rbatch.bat))

# radRprot.h is only made on unix, since cproto doesn't seem available in cygwin
NON_INSTALLED_TARGETS_unix = $(PROTOS)

## R file dependencies (due to ###.include statements)
.radRutil.R.stripped: layers.R pulses.R

$(R_BATCH_FILES): radR.bat radRcommon.bat
	for v in $(SUPPORTED_R_VERSIONS); do\
	  $(SED) -b -e "s/__RVERSION__/$$v/g" radR.bat > $(RADR_INSTALL_DIR)/radR$$v.bat; \
	  $(CHMOD) oug+x $(RADR_INSTALL_DIR)/radR$$v.bat; \
	done
	$(CP) -f radRcommon.bat $(RADR_INSTALL_DIR)/radRcommon.bat
	$(CHMOD) oug+x $(RADR_INSTALL_DIR)/radRcommon.bat; \

## rules for targets:

$(RADR_LIB): $(RADR_INC_FILES) $(RADR_SOURCE_FILES) ../include/scancvt.h $(MAKEFILE_DEP)
	$(CC) $(CCOPTS) $(RINC) $(TCLTK_INC) -o $@ $(RADR_SOURCE_FILES) $(TCLTK_LIB) -Wl,-rpath,. -Wl,-rpath,../packages/extmat/libs -Wl,-rpath,$(TCLTK_LIB) -lm -L. $(RLIB)  $(LIBOPTS)

$(TKIMGPNG_LIB): $(TKIMGPNG_SOURCE_FILES)
	$(CC) $(CCOPTS) $(RINC) $(TCLTK_INC) -DPACKAGE_VERSION='"8.4.13"' -o $@ $(TKIMGPNG_SOURCE_FILES) $(LIBOPTS) $(TCLTK_LIB) -Wl,-rpath,$(TCLTK_LIB) $(LIBZ_INC) $(LIBZ_LIB) $(RLIB)

$(PROTOS): $(RADR_SOURCE_FILES)
	$(PROTO) $(INCLUDE_DIRS) $(TCLTK_INC) $(RINC) -D DOING_CPROTO $^ > $@

## We maintain radR's .Rprofile as radR.Rprofile.R, so that it's visible
## in the source directory.

$(eval $(call DEF_INST_R_ALIAS,../.Rprofile,radR.Rprofile.R))

## We move the template batch parameter file into the toplevel directory

$(eval $(call DEF_INST_R_ALIAS,../batchparm.R,batchparm.R))

$(eval $(AT_MAKEFILE_END))

