###
### Makefile.inc: macros and rules for radR Makefiles
###

## force a second expansion because of rules and targets
## defined in macros

.SECONDEXPANSION:


## macro needed to allow commas in arguments to $(call ...)
comma:= ,
empty:=
space:= $(empty) $(empty)

## R files are processed in two stages
## First, a simple R preprocesser (Rpp) is run on them
## Then, R is invoked to parse them for syntax errors.
## We hide the two outputs from these stages.  The output
## of the checking stage is to simply touch an empty
## file.  The name of the unstripped file is passed to the
## checker so that error messages can be reported relative
## to the real source file.

.%.R.stripped:%.R
	$(RPP) --lineinfo $@.lineinfo $(RPP_OPTS_$(VERSION)) < $< > $@

.%.R.checked:.%.R.stripped
	$(R) --slave --vanilla --args $< $*.R < $(R_FILE_CHECKER) >$@

MY_INSTALL_DIR := $(RADR_INSTALL_DIR)/$(subst $(RADR_TOPLEVEL_DIR)/,,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath .)))

STRIPPED_R_FILES = $(addprefix .,$(addsuffix .stripped,$(R_FILES)))
CHECKED_R_FILES = $(addprefix .,$(addsuffix .checked,$(R_FILES)))
LINEINFO_R_FILES = $(addsuffix .lineinfo,$(STRIPPED_R_FILES))
INSTALLED_TARGETS = $(patsubst $(RADR_TOPLEVEL_DIR)/%,$(RADR_INSTALL_DIR)/%,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(TARGETS) $(COPY_AS_IS))))
INSTALLED_R_FILES = $(filter-out %.conf.R,$(patsubst $(RADR_TOPLEVEL_DIR)/%,$(RADR_INSTALL_DIR)/%,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(R_FILES)))))
INSTALLED_R_CONF_FILES = $(patsubst %.conf.R, %.conf.update.R, $(filter %.conf.R,$(patsubst $(RADR_TOPLEVEL_DIR)/%,$(RADR_INSTALL_DIR)/%,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(R_FILES))))))

## a "function" to allow a file to be installed under an alias
## first arg is alias name; 2nd arg is (relative) filename
## The 2nd arg should name a file which is already being installed.
## This function will cause it to be treated as an .INTERMEDIATE file,
## to be renamed to the 1st arg.
##
## To use this, do 
##
##    $(eval $(call DEF_INST_ALIAS,alias,existing_installed_file))
## 
## and include MAKE_ALIASES in the dependencies of all:
##
##    all: ... MAKE_ALIASES
##

define DEF_INST_ALIAS
MAKE_ALIASES: $(patsubst $(RADR_TOPLEVEL_DIR)/%,$(RADR_INSTALL_DIR)/%,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(1))))

INSTALLED_ALIASES += $(patsubst $(RADR_TOPLEVEL_DIR)/%,$(RADR_INSTALL_DIR)/%,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(1))))

## remove this aliased target from the regular install targets
INSTALLED_TARGETS := $(filter-out $(patsubst $(RADR_TOPLEVEL_DIR)/%,$(RADR_INSTALL_DIR)/%,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(2)))),$(INSTALLED_TARGETS))

$(patsubst $(RADR_TOPLEVEL_DIR)/%,$(RADR_INSTALL_DIR)/%,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(1)))) : $(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(2)))
	$$(INSTALL) -D  $$< $$@
	chmod --reference=$$< $$@
endef

## An alias function for R files.   This takes care of preprocessing and syntax checking
## the R file, if necessary.  This function should not be used for configuration files.

define DEF_INST_R_ALIAS
MAKE_ALIASES: $(patsubst $(RADR_TOPLEVEL_DIR)/%,$(RADR_INSTALL_DIR)/%,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(1))))

INSTALLED_ALIASES += $(patsubst $(RADR_TOPLEVEL_DIR)/%,$(RADR_INSTALL_DIR)/%,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(1))))

## remove this aliased target from the regular install targets
INSTALLED_R_FILES := $(filter-out $(patsubst $(RADR_TOPLEVEL_DIR)/%,$(RADR_INSTALL_DIR)/%,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(2)))),$(INSTALLED_R_FILES))

$(patsubst $(RADR_TOPLEVEL_DIR)/%,$(RADR_INSTALL_DIR)/%,$(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath $(1)))) : $(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath .$(2).checked))  $(addprefix $(TOPLEVEL_PATH_PREFIX),$(abspath .$(2).stripped))
	$$(INSTALL) -D -m 644 .$(2).stripped $$@
endef


## definitions which must be deferred until
## after macros have been defined.
##
## A makefile including this file should have the line
## $(eval $(AT_MAKEFILE_END))
## as its last line.

define AT_MAKEFILE_END
$$(INSTALLED_R_FILES): $$(MY_INSTALL_DIR)/%.R: .%.R.checked
	$$(INSTALL) -D -m 644 .$$*.R.stripped $$@

$$(INSTALLED_R_CONF_FILES): $$(MY_INSTALL_DIR)/%conf.update.R: .%conf.R.checked
	$$(INSTALL) -D -m 644 .$$*conf.R.stripped $$@

$$(INSTALLED_TARGETS): $$(MY_INSTALL_DIR)/%: %
	$$(INSTALL) -D  $$< $$@
	chmod --reference=$$< $$@

$$(PLUGIN_LIB) : $$(PLUGIN_LIB_DEPS)
	$$(PLUGIN_BUILD_LIB_CMD)

$$(PLUGIN_EXTRA_DIRS) : $$(PLUGIN_LIB)
	-for d in $$(PLUGIN_EXTRA_DIRS); do $$(INSTALL) -d $$$$d $$(MY_INSTALL_DIR)/$$$$d; $$(INSTALL) -D $$$$d/* $$(MY_INSTALL_DIR)/$$$$d; done
endef

## mark stripped R files as precious so they are not deleted.
.SECONDARY: $$(STRIPPED_R_FILES)

all: $$(INSTALLED_TARGETS) $$(INSTALLED_R_FILES) $$(INSTALLED_R_CONF_FILES) MAKE_ALIASES $$(NON_INSTALLED_TARGETS) $$(NON_INSTALLED_TARGETS_$(PLATFORM)) $$(PLUGIN_EXTRA_DIRS)

MAKE_ALIASES:



## definitions for plugins

PLUGIN_NAME = $(notdir $(shell pwd))
PLUGIN_STANDARD_R_FILES = $(wildcard $(PLUGIN_NAME).desc.R $(PLUGIN_NAME).conf.R $(PLUGIN_NAME).plugin.R)
ifneq ("$(wildcard $(PLUGIN_NAME).c)", "")
PLUGIN_STANDARD_FILES = $(PLUGIN_NAME)$(SHLIB_SUFFIX)
PLUGIN_LIB = $(PLUGIN_NAME)$(SHLIB_SUFFIX)
PLUGIN_LIB_DEPS = $(wildcard $(PLUGIN_NAME).c) $(wildcard $(PLUGIN_NAME).h) $(RADR_INCLUDE_DIR)/radRmodule.h $(PLUGIN_LIB_EXTRA_DEPS) $(add-prefix $(RADR_INCLUDE_DIR)/, $(RADR_INC_FILES))
PLUGIN_LIB_SOURCE = $(wildcard $(PLUGIN_NAME).c) $(PLUGIN_LIB_EXTRA_SOURCE)
PLUGIN_LIB_OPTIONS = -I ../.. $(PLUGIN_LIB_EXTRA_OPTIONS)
PLUGIN_BUILD_LIB_CMD = $(CC) $(CCOPTS) $(RINC) -o $(PLUGIN_LIB) $(PLUGIN_LIB_SOURCE) $(PLUGIN_LIB_OPTIONS) -lm $(RLIB) $(LIBOPTS)
endif

TARGETS = $(filter-out %.R, $(PLUGIN_STANDARD_FILES) $(PLUGIN_EXTRA_TARGETS) $(EXTRA_TARGETS) $(EXTRA_PRECIOUS_TARGETS))
R_FILES = $(filter %.R, $(PLUGIN_STANDARD_R_FILES) $(PLUGIN_EXTRA_R_FILES) $(EXTRA_TARGETS) $(EXTRA_PRECIOUS_TARGETS)) 

## a function to build additional plugin libs
## called as:
## 
## $(eval $(call LIB_NAME, LIB_DEPS, LIB_SOURCE, LIB_BUILD_OPTIONS))
##
## where
##
##   LIB_NAME: the name of the shared library to be built, without a suffix (i.e. no ".dll")
##   LIB_DEPS: the targets (e.g. files) on which the library depends
##   LIB_SOURCE: the source files to be compiled into the library
##   LIB_BUILD_OPTIONS: additional options for the build command
##

define DEF_EXTRA_LIB
$(1)$(SHLIB_SUFFIX) : $(2)
	$(CC) $(CCOPTS) $(RINC) -o $(1)$(SHLIB_SUFFIX) $(3) $(4)  -lm $(RLIB) $(LIBOPTS)
PLUGIN_EXTRA_TARGETS += $(1)$(SHLIB_SUFFIX)
PLUGIN_EXTRA_CLEANUPS += $(1)$(SHLIB_SUFFIX)
endef

plugin_clean: 
	rm -rf $(PLUGIN_LIB) $(PLUGIN_EXTRA_CLEANUPS)

clean:
	$(RM) -f $(filter-out $(EXTRA_PRECIOUS_TARGETS),$(TARGETS)) $(STRIPPED_R_FILES) $(LINEINFO_R_FILES) $(CHECKED_R_FILES) $(INSTALLED_TARGETS) $(INSTALLED_R_FILES) $(INSTALLED_ALIASES)

