#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Makefile for LFRIC test code. May be that we want to replace with FCM make or
# somesuch.
#
# To compile:
#   make ARCH=[platform]
#
# Platforms are:
#   * linux_gfortran
#   * linux_ifort
#   * ibm_power7
#
# For GNU make.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Distribution specific paths
#-------------------------------------------------------------------------------

GUNGHOSRC=../../../GungHo/src

#-------------------------------------------------------------------------------
# Defaults
#-------------------------------------------------------------------------------

# Default to linux_gfortran
ARCH=linux_gfortran

#-------------------------------------------------------------------------------
# Conditionals
#-------------------------------------------------------------------------------

# Set the compiler options
ifeq ($(ARCH), linux_gfortran)
  FC=gfortran
  LD=$(FC)
  FFLAGS  = -g -O0 -std=f2003 -Wuninitialized -Wall -Wextra -fbounds-check
  LDFLAGS = -g
endif
ifeq ($(ARCH), linux_ifort)
  FC=ifort
  LD=$(FC)
  FFLAGS  = -g -O0 -std03
  LDFLAGS = -g
endif
ifeq ($(ARCH), ibm_power7)
  FC=xlf2003_r
  LD=$(FC)
  FFLAGS  = -g -O0 -qlanglvl=2003std
  LDFLAGS = -g
endif

#-------------------------------------------------------------------------------
# Object list
#-------------------------------------------------------------------------------

OBJ = integrate_one_module.o integrate_one.o

LIBS=-L${GUNGHOSRC}/utils -L${GUNGHOSRC}/kernels -llfric -lgungho
FFLAGS += -I${GUNGHOSRC}/utils -I${GUNGHOSRC}/kernels
EXE=integrate_one
#-------------------------------------------------------------------------------
# Targets
#-------------------------------------------------------------------------------

all: $(EXE)

integrate_one:
	cd ${GUNGHOSRC}/utils;make
	cd ${GUNGHOSRC}/kernels;make
	python ../../src/generator.py -oalg integrate_one_alg.F90 -opsy integrate_one_psy.F90 integrate_one.F90
	make integrate_one_generated

integrate_one_generated: integrate_one_module.o integrate_one_psy.o integrate_one_alg.o
	$(LD) $(FFLAGS) $(LDFLAGS) -o $@ integrate_one_module.o integrate_one_psy.o integrate_one_alg.o $(LIBS)

clean:
	rm -f *.o *.mod *.out $(EXE) *_alg.F90 *_psy.F90 fparser.log *_generated

.PHONY:
	clean

#-------------------------------------------------------------------------------
# Patterns
#-------------------------------------------------------------------------------

%.o:%.F90
	$(FC) -c $(FFLAGS) $<
%.o:%.f90
	$(FC) -c $(FFLAGS) $<
