# Assume that apeg-dl_timer is 'next' to psyclone
DRHOOK_ROOT?=../../../../drhook

DRHOOK_INCLUDE?=$(DRHOOK_ROOT)/drhook/include
DRHOOK_MODULES?=$(DRHOOK_ROOT)/build/module

F90?=gfortran
FFLAGS?=-g
FFLAGS += -I$(DRHOOK_INCLUDE) -I$(DRHOOK_MODULES)

name=drhook
libname=lib$(name).a

default: $(libname)

$(libname): $(name).f90
	$(F90) -c $(FFLAGS) $<
	ar rs $(libname) $(name).o

clean:
	rm -f $(libname) $(name).o profile_mod.mod

