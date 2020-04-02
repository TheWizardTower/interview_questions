CXX=g++
RM=rm -f
CPPFLAGS=-g -Wall -Werror -Wpedantic $(shell root-config --cflags) -I$(shell pwd)/../../lib/CPP/cxxopts/include
LDFLAGS=-g $(shell root-config --ldflags)
LDLIBS=$(shell root-config --libs)

SRCS=main.cpp trie.cpp
OBJS=$(subst .cpp,.o,$(SRCS))
OBJS+=boggle

all: boggle

test:
	echo "OBJ: " $(OBJS)
	echo "CPPFLAGS: " $(CPPFLAGS)
	echo "LDFLAGS: " $(LDFLAGS)
	echo "LDLIBS: " $(LDLIBS)

boggle: main.cpp
	g++ $(CPPFLAGS) $(SRCS) -o boggle

clean:
	$(RM) $(OBJS)
