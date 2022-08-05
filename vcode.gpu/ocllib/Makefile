OBJS = scan.o stack.o vector.o
TESTOBJS = main.o
SMALLOBJS = smallnesting.vcode.o

TARGETLIB = libndpocl.a
TARGET = test
SMALLNESTING = smallnesting

CC = g++
CFLAGS = -g -Wall

build : $(TARGETLIB) $(TARGET) $(SMALLNESTING)

clean :
	rm -f $(OBJS) $(TARGETLIB) $(TARGET) $(SMALLNESTING) $(TESTOBJS) $(SMALLOBJS)

%.o: %.c ndp2gpu.h
	$(CC) -c $(CFLAGS) -I . -o $@ $<

$(TARGETLIB) : $(OBJS)
	ar -r $@ $^

$(TARGET) : $(TESTOBJS) $(TARGETLIB)
	$(CC) $(CFLAGS) -framework OpenCL -L. -lndpocl -o $@ $^

$(SMALLNESTING) : $(TARGETLIB) $(SMALLOBJS)
	$(CC) $(CFLAGS) -framework OpenCL -L. -lndpocl -o $@ $^

