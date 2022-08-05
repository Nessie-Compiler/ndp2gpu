OBJS = oclcheck.o
TARGET = oclcheck

CC = g++
CFLAGS = -g -Wall
OS := $(shell uname -s)
ifeq ($(OS),Darwin)
LIBFLAGS=-framework OpenCL
else
LIBFLAGS=-lOpenCL
endif

build : $(TARGET)

clean :
	rm -f $(OBJS) $(TARGET)

%.o: %.c 
	$(CC) -c $(CFLAGS) -I . -o $@ $<

$(TARGET) : $(OBJS)
	$(CC) $(CFLAGS) $(LIBFLAGS) -L. -o $@ $^
