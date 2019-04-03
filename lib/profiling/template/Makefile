F90?=gfortran
FFLAGS?=-g

default: libdummy.a

libdummy.a: dummy_lib.f90
	$(F90) -c $(FFLAGS) dummy_lib.f90
	ar rs libdummy.a dummy_lib.o

clean:
	rm -f libdummy.a dummy_lib.o profile_mod.mod

