F90=gfortran
FFLAGS=-g
LDFLAGS=

ALL_LIBS=template simple_timing dl_timer

.PHONY: default all $(ALL_LIBS) clean

default: all

all: $(ALL_LIBS)

simple_timing:
	$(MAKE) -C simple_timing

dl_timer:
	$(MAKE) -C dl_timer

template:
	$(MAKE) -C template

clean:
	$(foreach lib, $(ALL_LIBS), $(MAKE) -C $(lib) clean; )

