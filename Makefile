all: randomsound

SRCS := randomsound.c bitbuffer.c debias.c asoundrunner.c
HDRS := bitbuffer.h debias.h asoundrunner.h

LINK := gcc
CC := gcc

OBJS := $(SRCS:.c=.o)

check: test_bitbuffer

randomsound: $(OBJS)
	$(LINK) -o $@ $^

clean:
	$(RM) randomsound $(OBJS)
	$(RM) *~ test_bitbuffer

test_%: %.c
	$(CC) -DTEST -o $@ $<
	./$@

randomsound.o: bitbuffer.h debias.h asoundrunner.h
bitbuffer.o: bitbuffer.h
debias.o: debias.h bitbuffer.h
asoundrunner.o: asoundrunner.h bitbuffer.h
