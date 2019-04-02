F90?=gfortran
FFLAGS?=-g

name=simple_timing
libname:=lib$(name).a

.PHONY: default

default: $(libname)

$(libname): $(name).f90
	$(F90) -c $(FFLAGS) $<
	ar rs $@ $(name).o

clean:
	rm -f $(libname) $(name).o profile_mod.mod

