all: randomsound tests/shannon

SRCS := randomsound.c bitbuffer.c debias.c micfill.c
HDRS := bitbuffer.h debias.h micfill.h

LINK := gcc
CC := gcc
CFLAGS :=-g 

OBJS := $(SRCS:.c=.o)
DATAFILE := $(shell date +'tests/data-%F-%H-%M.bin')

check: test_bitbuffer

randomsound: $(OBJS)
	$(LINK) -o $@ $^ -lasound
	$(MAKE) --no-print-directory entropy_test

$(DATAFILE): randomsound
	./randomsound -v -v -v -d 512 -M 512 -T $@

entropy_test: $(DATAFILE) tests/shannon
	./tests/shannon $(DATAFILE)

clean:
	$(RM) randomsound $(OBJS)
	$(RM) *~ test_bitbuffer
	$(RM) tests/*.o tests/*.hi tests/shannon
	@echo "WARNING: Leaving data files for reference, to remove them: rm tests/*.bin"

.PHONY: clean entropy_test all

test_%: %.c
	$(CC) -DTEST -o $@ $<
	./$@


randomsound.o: bitbuffer.h debias.h micfill.h
bitbuffer.o: bitbuffer.h micfill.h
debias.o: debias.h bitbuffer.h
micfill.o: micfill.h

tests/shannon: tests/shannon.hs
	ghc -O2 -o tests/shannon tests/shannon.hs
