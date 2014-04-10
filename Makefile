all: randomsound

SRCS := randomsound.c bitbuffer.c debias.c micfill.c
HDRS := bitbuffer.h debias.h micfill.h

LINK := gcc
CC := gcc
CFLAGS :=-g 

OBJS := $(SRCS:.c=.o)

check: test_bitbuffer

randomsound: $(OBJS)
	$(LINK) -o $@ $^ -lasound

clean:
	$(RM) randomsound $(OBJS)
	$(RM) *~ test_bitbuffer

test_%: %.c
	$(CC) -DTEST -o $@ $<
	./$@

randomsound.o: bitbuffer.h debias.h micfill.h
bitbuffer.o: bitbuffer.h micfill.h
debias.o: debias.h bitbuffer.h
micfill.o: micfill.h
