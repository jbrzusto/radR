## This Makefile is called recursively from the Makefile in the parent directory,
## and requires the variables defined there.

$(eval $(AT_MAKEFILE_START))

R_FILES := $(shell $(FIND) . -type f -regex "^.*\\.antenna\\.R$$" -printf '%f\n')

$(eval $(AT_MAKEFILE_END))
