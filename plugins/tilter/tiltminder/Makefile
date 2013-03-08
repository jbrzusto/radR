# svn: $Id: Makefile 785 2011-05-05 17:26:03Z john $
#
# for building  under linux
#

## The following must be DEBUG or PRODUCTION
VERSION := PRODUCTION
#VERSION := DEBUG

## A filter for wrapping g++ to get simpler C++ error messages
GPP := gfilt

## The GCC version:
GCC_VERSION := 4.3.2

GCC_OPTS_DEBUG := -Wall -g3 -fPIC
GCC_OPTS_PRODUCTION := -Wall -O2 -fPIC
GCC_OPTS := $(GCC_OPTS_$(VERSION))

## The Boost version:
BOOST_VERSION := 1.41.0

## Path to Boost (this uses an uninstalled source tree)
BOOST_PATH := /home/src/boost_$(subst .,_,$(BOOST_VERSION))
BOOST_INC_PATH := $(BOOST_PATH)
##BOOST_LIB_PATH := $(BOOST_PATH)/bin.v2/libs/thread/build/gcc-$(GCC_VERSION)/release/threading-multi
BOOST_LIB_PATH := $(BOOST_PATH)/stage/lib

STD_LIBS := -lstdc++ -lm

LIBS :=  -L$(BOOST_LIB_PATH) -lboost_date_time -lboost_thread $(STD_LIBS)
SERVER_LIBS :=  -L$(BOOST_LIB_PATH) -lboost_system -lboost_date_time -lboost_thread $(STD_LIBS)
INC := -I$(BOOST_INC_PATH)

MINDER_FILES := minder.cc minderutil.hpp minderutil.cc mindertypedefs.hpp

all: dynamic static

dynamic:runtilt1.1 tiltserv1.1
## dynamic:  runtilt1.0 tiltserv1.0 

static: runtilt1.1s tiltserv1.1s
##static: runtilt1.0s tiltserv1.0s runtilt1.1s tiltserv1.1s

## the shared library

# tilter1.0.so: tilter1.0.cc tilter1.0.h $(MINDER_FILES)
# 	$(GPP) $(GCC_OPTS) $(INC) -shared -o $@ $< minderutil.cc $(LIBS)

# ## runner linked to shared library

# runtilt1.0: runtilt1.0.c tilter1.0.so
# 	$(GPP) $(GCC_OPTS) $(INC) -o $@ $^ $(LIBS)

# ## runner linked statically with tilter1.0 

# runtilt1.0s: runtilt1.0.c tilter1.0.cc tilter1.0.h $(MINDER_FILES)
# 	$(GPP) $(GCC_OPTS) $(INC) -o $@ -x c runtilt1.0.c -x c++ tilter1.0.cc -x c++ minderutil.cc $(LIBS)

# ## build a server that acts like runtilt but listens on a socket

# tiltserv1.0: tiltserv1.0.cc tilter1.0.so
# 	$(GPP) $(GCC_OPTS) $(INC) -o $@ $^ $(SERVER_LIBS) 

# ## static version of server

# tiltserv1.0s: tiltserv1.0.cc tilter1.0.cc tilter1.0.h $(MINDER_FILES)
# 	$(GPP) $(GCC_OPTS) $(INC) -o $@ tiltserv1.0.cc tilter1.0.cc minderutil.cc $(SERVER_LIBS)

tilter1.1.so: tilter1.1.cc tilter1.1.h $(MINDER_FILES)
	$(GPP) $(GCC_OPTS) $(INC) -shared -o $@ $< minderutil.cc $(LIBS)

## runner linked to shared library

runtilt1.1: runtilt1.1.c tilter1.1.so
	$(GPP) $(GCC_OPTS) $(INC) -o $@ $^ $(LIBS)

## runner linked statically with tilter1.1 

runtilt1.1s: runtilt1.1.c tilter1.1.cc tilter1.1.h $(MINDER_FILES)
	$(GPP) $(GCC_OPTS) $(INC) -o $@ -x c runtilt1.1.c -x c++ tilter1.1.cc -x c++ minderutil.cc $(LIBS)

## build a server that acts like runtilt but listens on a socket

tiltserv1.1: tiltserv1.1.cc tilter1.1.so
	$(GPP) $(GCC_OPTS) $(INC) -o $@ $^ $(SERVER_LIBS) 

## static version of server

tiltserv1.1s: tiltserv1.1.cc tilter1.1.cc tilter1.1.h $(MINDER_FILES)
	$(GPP) $(GCC_OPTS) $(INC) -o $@ tiltserv1.1.cc tilter1.1.cc minderutil.cc $(SERVER_LIBS)

# strip:
# 	-strip runtilt1.0 runtilt1.0s tilter1.0.so tiltserv1.0 tiltserv1.0s

# clean:
# 	rm -f runtilt1.0 runtilt1.0s tilter1.0.so tiltserv1.0 tiltserv1.0s

strip:
	-strip runtilt1.1 runtilt1.1s tilter1.1.so tiltserv1.1 tiltserv1.1s

clean:
	rm -f runtilt1.1 runtilt1.1s tilter1.1.so tiltserv1.1 tiltserv1.1s

