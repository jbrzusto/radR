
all: bmdebug$(EXE_SUFFIX)

$(eval $(AT_MAKEFILE_START))

bmdebug$(EXE_SUFFIX): bmdebug.c 
	$(CC) $(CCOPTS) bmdebug.c -o bmdebug$(EXE_SUFFIX) $(RINC) -lm


clean: 
	rm -f bmdebug$(EXE_SUFFIX)
