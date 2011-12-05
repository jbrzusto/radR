## Makefile for building an R interface to Iris Radar raw product files using SWIG.
## (With some modifications to SWIG by me).

$(eval $(AT_MAKEFILE_START))

all:	iris_radR$(SHLIB_SUFFIX) 

#$(eval $(call DEF_EXTRA_LIB, iris_radR, iris_radR.c, iris_radR.c))

iris$(SHLIB_SUFFIX): iris.c
	gcc -DBITS_PER_SAMPLE=12 -Wall -O3 -ffast-math -fgcse-sm -fgcse-las -fgcse-after-reload -march=pentium4 -mfpmath=sse -msse2 -DWin32 -idirafter c:/cygwin/usr/include "-I../include"  -I/cygwin/home/john/radR/include -I/cygwin/home/john/radR/packages/bigframe/src -I/cygwin/home/john/radR/packages/biglist/src -I/cygwin/home/john/radR/packages/extmat/src "-I/Program Files (x86)/R/R-2.5.1/include"  -o iris.dll iris.c  -I ../..  /cygwin/home/john/radR/libs/pthreadGC2.dll -Wl,-rpath,.,-rpath,main,-rpath,libs /cygwin/home/john/radR/main/radR.dll -lm "-L/Program Files (x86)/R/R-2.5.1/bin" "-L/Program Files (x86)/R/R-2.5.1/src/gnuwin32/" -lR -shared  -Wl,-rpath,../libs,--kill-at,--enable-stdcall-fixup,--enable-auto-import -L/cygwin/home/john/radR/install/packages/extmat/libs/ -lextmat


iris_radR$(SHLIB_SUFFIX): iris_radR.c
	gcc -DBITS_PER_SAMPLE=12 -Wall -O3 -ffast-math -fgcse-sm -fgcse-las -fgcse-after-reload -march=pentium4 -mfpmath=sse -msse2 -DWin32 -idirafter c:/cygwin/usr/include "-I../include"  -I/cygwin/home/john/radR/include -I/cygwin/home/john/radR/packages/bigframe/src -I/cygwin/home/john/radR/packages/biglist/src -I/cygwin/home/john/radR/packages/extmat/src "-I/Program Files (x86)/R/R-2.5.1/include"  -o iris_radR.dll iris_radR.c  -I ../..  /cygwin/home/john/radR/libs/pthreadGC2.dll -Wl,-rpath,.,-rpath,main,-rpath,libs /cygwin/home/john/radR/main/radR.dll -lm "-L/Program Files (x86)/R/R-2.5.1/bin" "-L/Program Files (x86)/R/R-2.5.1/src/gnuwin32/" -lR -shared  -Wl,-rpath,../libs,--kill-at,--enable-stdcall-fixup,--enable-auto-import -L/cygwin/home/john/radR/install/packages/extmat/libs/ -lextmat

#iris.c: iris.i iris.h
#	/usr/src/swig/swig -o iris.c -DLINUX -D__GNUC__=4 -s3 -I/usr/include -I/usr/include/linux -importall iris.i 

$(eval $(AT_MAKEFILE_END))
