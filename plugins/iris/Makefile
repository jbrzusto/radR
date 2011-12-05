## Makefile for building an R interface to Iris Radar raw product files using SWIG.
## (With some modifications to SWIG by me).

$(eval $(AT_MAKEFILE_START))

$(eval $(call DEF_EXTRA_LIB, iris_radR, iris_radR.c, iris_radR.c))

iris$(SHLIB_SUFFIX): iris.c

#iris.c: iris.i iris.h
#	/usr/src/swig/swig -o iris.c -DLINUX -D__GNUC__=4 -s3 -I/usr/include -I/usr/include/linux -importall iris.i 

$(eval $(AT_MAKEFILE_END))
