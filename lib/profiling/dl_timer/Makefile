# Assume that apeg-dl_timer is 'next' to psyclone
DL_TIMER_ROOT?=../../../../apeg-dl_timer

DL_TIMER_INCLUDE?=$(DL_TIMER_ROOT)/src

F90?=gfortran
FFLAGS?=-g
FFLAGS += -I$(DL_TIMER_INCLUDE)

name=dl_timer
libname=lib$(name).a

default: $(libname)

$(libname): $(name).f90
	$(F90) -c $(FFLAGS) $<
	ar rs $(libname) $(name).o

clean:
	rm -f $(libname) $(name).o profile_mod.mod

