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

R_FILES := 			\
	batchparm.R		\
	batchparminrad.R	\
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

EXTRA_TARGETS = $(RADR_LIB)
EXTRA_PRECIOUS_TARGETS = rbatch rbatch.bat

$(eval $(call DEF_INST_ALIAS,radRshared.h,../include/radRshared.h))
$(eval $(call DEF_INST_ALIAS,../rbatch,rbatch))
$(eval $(call DEF_INST_ALIAS,../rbatch.bat,rbatch.bat))

# radRprot.h is only made on unix, since cproto doesn't seem available in cygwin
NON_INSTALLED_TARGETS_unix = $(PROTOS)

## R file dependencies (due to ###.include statements)
.radRutil.R.stripped: layers.R pulses.R

## Note: we create a file called simply "rbatch.bat" which is for the last version in SUPPORTED_R_VERSIONS

$(R_BATCH_FILES): radR.bat rbatch.bat radRcommon.bat
	for v in $(SUPPORTED_R_VERSIONS); do\
	  $(SED) -b -e "s/__RVERSION__/$$v/g" radR.bat > $(RADR_INSTALL_DIR)/radR$$v.bat; \
	  $(CHMOD) oug+x $(RADR_INSTALL_DIR)/radR$$v.bat; \
	  $(SED) -b -e "s/__RVERSION__/$$v/g" rbatch.bat > $(RADR_INSTALL_DIR)/rbatch$$v.bat; \
	  $(SED) -b -e "s/__RVERSION__/$$v/g" rbatch.bat > $(RADR_INSTALL_DIR)/rbatch.bat; \
	  $(CHMOD) oug+x $(RADR_INSTALL_DIR)/rbatch.bat; \
	  $(CHMOD) oug+x $(RADR_INSTALL_DIR)/rbatch$$v.bat; \
	done
	$(CP) -f radRcommon.bat $(RADR_INSTALL_DIR)/radRcommon.bat
	$(CHMOD) oug+x $(RADR_INSTALL_DIR)/radRcommon.bat; \

## rules for targets:

$(RADR_LIB): $(RADR_INC_FILES) $(RADR_SOURCE_FILES) ../include/scancvt.h $(MAKEFILE_DEP)
	$(CC) $(CCOPTS) $(RINC) $(TCLTK_INC) -o $@ $(RADR_SOURCE_FILES) $(TCLTK_LIB) -Wl,-rpath,. -Wl,-rpath,../packages/extmat/libs -Wl,-rpath,$(TCLTK_LIB) -lm -L. $(RLIB)  $(LIBOPTS)

$(PROTOS): $(RADR_SOURCE_FILES)
	$(PROTO) $(INCLUDE_DIRS) $(TCLTK_INC) $(RINC) -D DOING_CPROTO $^ > $@

## We maintain radR's .Rprofile as radR.Rprofile.R, so that it's visible
## in the source directory.

$(eval $(call DEF_INST_R_ALIAS,../.Rprofile,radR.Rprofile.R))

## We move the template batch parameter file into the toplevel directory

$(eval $(call DEF_INST_R_ALIAS,../batchparm.R,batchparm.R))
$(eval $(call DEF_INST_R_ALIAS,../batchparminrad.R,batchparminrad.R))

$(eval $(AT_MAKEFILE_END))

