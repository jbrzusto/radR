## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.

$(eval $(AT_MAKEFILE_START))

R_FILES := $(shell $(FIND) . -type f -regex "^.*\\.R$$" -printf '%f\n')
PRECIOUS_EXTRA_TARGETS := README.TXT

$(eval $(AT_MAKEFILE_END))
