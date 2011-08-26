## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.  To make this plugin, do 
##
## cd .. (the main radR folder)
## make tracker
##

$(eval $(AT_MAKEFILE_START))

MODELS = multiframecorr nearestneighbour
PLUGIN_EXTRA_R_FILES = $(addsuffix .model.R, $(MODELS)) $(addsuffix .conf.R, $(MODELS))

$(eval $(call DEF_EXTRA_LIB, maxmatch, maxmatch.c, maxmatch.c))
$(eval $(call DEF_EXTRA_LIB, maxpathcover, maxpathcover.c maxpathcover.h, maxpathcover.c))
$(eval $(call DEF_EXTRA_LIB, multiframecorr, multiframecorr.h multiframecorr.c maxpathcover$(SHLIB_SUFFIX), multiframecorr.c maxpathcover$(SHLIB_SUFFIX), -Wl$(comma)-rpath$(comma)plugins/tracker$(comma)-rpath$(comma)packages/extmat/libs ))


$(eval $(AT_MAKEFILE_END))


# maxmatch$(SHLIB_SUFFIX): maxmatch.c $(RADR_INC_FILES) $(MAKEFILE_DEP)
# 	$(CC) $(CCOPTS) $(RINC) $(TCLTK_INC) -o $@ $< -Wl,-rpath,. -lm -L. $(RLIBDIR) -lR  $(LIBOPTS)

# maxpathcover$(SHLIB_SUFFIX): maxpathcover.c maxpathcover.h $(RADR_INC_FILES) $(MAKEFILE_DEP)
# 	$(CC) $(CCOPTS) $(RINC) $(TCLTK_INC) -o $@ $< -Wl,-rpath,. -lm -L. $(RLIBDIR) -lR  $(LIBOPTS)

# multiframecorr$(SHLIB_SUFFIX): multiframecorr.c multiframecorr.h maxpathcover.h $(RADR_INC_FILES) $(MAKEFILE_DEP)
# 	$(CC) $(CCOPTS) $(RINC) $(TCLTK_INC) -o $@ $< -Wl,-rpath,.. -Wl,-rpath,. -Wl,-rpath,../../extmat/libs ./radR$(SHLIB_SUFFIX) ./plugins/maxpathcover$(SHLIB_SUFFIX) -lm -L. $(RLIBDIR) -lR  $(LIBOPTS)

