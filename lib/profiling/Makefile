F90=gfortran
FFLAGS=-g
LDFLAGS=

ALL_LIBS=template simple_timing dl_timer drhook

.PHONY: default all $(ALL_LIBS) clean

default: all

all: $(ALL_LIBS)

template:
	$(MAKE) -C template

simple_timing:
	$(MAKE) -C simple_timing

dl_timer:
	$(MAKE) -C dl_timer

drhook:
	$(MAKE) -C drhook

clean:
	$(foreach lib, $(ALL_LIBS), $(MAKE) -C $(lib) clean; )

