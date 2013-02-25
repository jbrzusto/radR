##
## Makefile for the radR "R" packages
##

$(eval $(AT_MAKEFILE_START))

RADR_PACKAGES := $(shell $(FIND) . -maxdepth 1 -regex '^\./Rinst[0-9]+' -prune -o  -type d -regex '^\./[a-zA-Z0-9_]+$$' -printf '%f\n')

## FIXME: this includes the package version number, which should vary with the package!
ifeq (unix,$(BUILD_PLATFORM))
ifeq (unix,$(TARGET_PLATFORM))
BINARY_PACKAGE_SUFFIX = _1.0_R_unix.tar.gz
else
BINARY_PACKAGE_SUFFIX = _1.0_R_i586-mingw32msvc.tar.gz
endif
else
BINARY_PACKAGE_SUFFIX = _1.0.zip
endif

SOURCE_PACKAGE_SUFFIX = _1.0.tar.gz

INSTALLED_RADR_PACKAGES := $(addprefix $(RADR_PACKAGE_INSTALL_DIR)/,$(RADR_PACKAGES))

all: $(INSTALLED_RADR_PACKAGES)

FORCE:

## function to get the names of all source files in an R package
##
## call this as $(call GET_R_PACKAGE_FILENAMES,PACKAGE_PATH)
##

define GET_R_PACKAGE_FILENAMES
	$(subst $$,$$$$,$(shell find $(1) -iname "*.svn" -prune -o -iname "*~" -o -iname "*.o" -o -iname "*$(SHLIB_SUFFIX)" -o -type f -print))
endef


## rule for making a binary package file from the source directory, and for cleaning it

define BINARY_BUILD_RULE
$$(addprefix $(RADR_PACKAGE_DIR)/,$$(addsuffix $$(BINARY_PACKAGE_SUFFIX),$(1))): $$(call GET_R_PACKAGE_FILENAMES, $(1))
	export R_PLATFORM=$(TARGET_PLATFORM) TARGET_PLATFORM=$(TARGET_PLATFORM); $(RCMD) build "$(1)"
ifeq (unix,$(TARGET_PLATFORM))
	$(RCMD) check $(addsuffix $(SOURCE_PACKAGE_SUFFIX),$(1))
	$(RM) -rf $(1).Rcheck
endif
	export R_PLATFORM=$(TARGET_PLATFORM) TARGET_PLATFORM=$(TARGET_PLATFORM); $(RCMD) build --binary $(1)

$(1)_clean:
	$(RM) -rf $(1).Rcheck $(addprefix $(RADR_PACKAGE_DIR)/,$(addsuffix $(BINARY_PACKAGE_SUFFIX), $(1))) \
	$(addprefix $(RADR_PACKAGE_DIR)/,$(addsuffix $(SOURCE_PACKAGE_SUFFIX), $(1))) \
	$(addprefix $(RADR_PACKAGE_INSTALL_DIR)/,$(1))

clean: $(1)_clean

endef

ifdef DEBUG_MAKEFILE
	$(foreach PKG,$(RADR_PACKAGES),$(info $(call BINARY_BUILD_RULE,$(PKG))\n))
endif

$(foreach PKG,$(RADR_PACKAGES),$(eval $(call BINARY_BUILD_RULE,$(PKG))))

## rule for making the installed packages from its binary archive

ifeq (unix,$(BUILD_PLATFORM)) 
define INSTALL_PACKAGE_RULE
$$(addprefix $(RADR_PACKAGE_INSTALL_DIR)/,$(1)): $$(addprefix $(RADR_PACKAGE_DIR)/, $$(addsuffix $(BINARY_PACKAGE_SUFFIX),$(1)))
	-mkdir -p $(RADR_PACKAGE_INSTALL_DIR); \
	$(RM) -rf $$(addprefix $(RADR_PACKAGE_INSTALL_DIR)/, $(1)) Rinst[0-9]*
	cd $(RADR_PACKAGE_INSTALL_DIR); \
	$(TAR) -xzf $$(addprefix $(RADR_PACKAGE_DIR)/, $$(addsuffix $(BINARY_PACKAGE_SUFFIX), $(1)))

endef
else
 ifeq (xc,$(BUILD_PLATFORM)) 
define INSTALL_PACKAGE_RULE
$$(addprefix $(RADR_PACKAGE_INSTALL_DIR)/,$(1)): $$(addprefix $(RADR_PACKAGE_DIR)/, $$(addsuffix $(BINARY_PACKAGE_SUFFIX),$(1)))
	-mkdir -p $(RADR_PACKAGE_INSTALL_DIR); \
	$(RM) -rf $$(addprefix $(RADR_PACKAGE_INSTALL_DIR)/, $(1)) Rinst[0-9]*
	cd $(RADR_PACKAGE_INSTALL_DIR); \
	$(TAR) -xzf $$(addprefix $(RADR_PACKAGE_DIR)/, $$(addsuffix $(BINARY_PACKAGE_SUFFIX), $(1)))

endef
else
define INSTALL_PACKAGE_RULE
$$(addprefix $(RADR_PACKAGE_INSTALL_DIR)/,$(1)): $$(addprefix $(RADR_PACKAGE_DIR)/, $$(addsuffix $(BINARY_PACKAGE_SUFFIX),$(1)))
	-mkdir -p $(RADR_PACKAGE_INSTALL_DIR); \
	$(RM) -rf $$(addprefix $(RADR_PACKAGE_INSTALL_DIR)/, $(1)) Rinst[0-9]*
	cd $(RADR_PACKAGE_INSTALL_DIR); \
	$(UNZIPPROG) $$(addprefix $(RADR_PACKAGE_DIR)/,$$(addsuffix $(BINARY_PACKAGE_SUFFIX),$(1)))
## Note that we need to set execute permission on shared libs, as zip files do not maintain this.
	-$(CHMOD) oug+x $$(addprefix $(RADR_PACKAGE_INSTALL_DIR)/, $(1))/libs/*$(SHLIB_SUFFIX)
## Touch the directory so it looks up to date
	touch $$(addprefix $(RADR_PACKAGE_INSTALL_DIR)/, $(1))
endef
endif
endif

ifdef DEBUG_MAKEFILE
	$(foreach PKG,$(RADR_PACKAGES),$(info $(call INSTALL_PACKAGE_RULE,$(PKG))\n))
endif

$(foreach PKG,$(RADR_PACKAGES),$(eval $(call INSTALL_PACKAGE_RULE,$(PKG))))

#clean:
#	rm -rf $(RADR_BINARY_PACKAGES) $(addsuffix /*.o,$(RADR_PACKAGES)) \
#	    $(addsuffix /*$(SHLIB_SUFFIX_unix),$(RADR_PACKAGES)) \
#	    $(addsuffix /*$(SHLIB_SUFFIX_windows),$(RADR_PACKAGES))
#	rm -rf $(INSTALLED_RADR_PACKAGES) $(


