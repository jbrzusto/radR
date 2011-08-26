##
## Makefile for the radR gui components
##

gui: all

$(eval $(AT_MAKEFILE_START))

UNIXGUI_SOURCE_FILES :=         \
	unixgui.c

UNIXGUI_DEPS :=                 \
	unixgui.h

WINDOWSGUI_SOURCE_FILES :=      \
	windowsgui.c

WINDOWSGUI_DEPS :=              \
	windowsgui.h

XCGUI_SOURCE_FILES :=      \
	windowsgui.c

XCGUI_DEPS :=              \
	windowsgui.h

TCL_FILES := \
	guicanvas.tcl \
	datepick.tcl

R_FILES := 			\
	epal.R                  \
	gui.R 			\
	gui.bindings.conf.R     \
	gui.conf.R              \
	gui.$(TARGET_PLATFORM).conf.R  \
	guiutil.R               \
	guicanvas.R             \
	blip3d.R

PIX := 	pause.gif 		\
	play1.gif 		\
	play.gif 		\
	radR.ico                \
	radR.xbm                \
	record.gif 		\
	stop.gif                \
	tostart.gif             \
	toend.gif


UNIXGUILIB := unixgui$(SHLIB_SUFFIX)
WINDOWSGUILIB := windowsgui$(SHLIB_SUFFIX)

GUI_LIB := $(TARGET_PLATFORM)gui$(SHLIB_SUFFIX)

TARGETS := $(GUI_LIB) 

COPY_AS_IS := $(PIX) $(TCL_FILES)

## rules for targets:


# the X11 gui lib

$(UNIXGUILIB): $(UNIXGUI_SOURCE_FILES) $(UNIXGUI_DEPS) $(RADR_LIB) $(RADR_INC_FILES) $(MAKEFILE_DEP)
	$(CC) $(CCOPTS) $(RINC) $(TCLTK_INC) -o $@ $(UNIXGUI_SOURCE_FILES) -Wl,-rpath,. $(RADR_LIB) -lm $(RLIB) $(XLIB) $(LIBOPTS) $(TCLTK_LIB)

## the windows gui lib

$(WINDOWSGUILIB): $(WINDOWSGUI_SOURCE_FILES) $(WINDOWSGUI_DEPS) $(RADR_INC_FILES) $(MAKEFILE_DEP)
	$(CC) $(CCOPTS) $(RINC) $(TCLTK_INC) -o $@ $(WINDOWSGUI_SOURCE_FILES) -lm $(RLIB) -lgdi32 $(LIBOPTS) $(TCLTK_LIB) $(PLATFORM_LIBOPTS)

$(eval $(AT_MAKEFILE_END))

