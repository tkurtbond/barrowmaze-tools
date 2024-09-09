include .what-sys.dat

ifeq ($(OPSYS),UNIX)
EXE=
OBJ=o
else
EXE=.exe
# Warning: using gcc!
OBJ=o
endif

ifndef CSC
CSC=csc
endif

BINDIR=$(HOME)/local/bin

ADV_GENS=
INSTALL_PROGRAMS=bmaze-random-tavern-patrons
PROGRAMS=$(INSTALL_PROGRAMS:%=build/%$(EXE))

all: build $(PROGRAMS)

build:
	mkdir build

build/%$(EXE) : %.scm
	$(CSC) $(CSCFLAGS) -o $@ $^


$(BINDIR)/% : build/%
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	cp $< $@

install: all $(foreach e,$(PROGRAMS:%=%$(EXE)),$(BINDIR)/$(notdir $(e)))

clean: 
	-rm build/*.so build/*.link $(PROGRAMS:%=%$(EXE))

realclean: clean
	-rm .what-sys.dat

.what-sys.dat:
	what-sys
