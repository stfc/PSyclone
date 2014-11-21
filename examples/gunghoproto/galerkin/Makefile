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
-fcheck=all
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

OBJ = manual_invoke_populate_rhs.o manual_invoke_galerkin_rhs_kernel.o

LIBS=-L../../utils -llfric 
FFLAGS += -I../../utils -I../../kernels
EXE=galerkin
#-------------------------------------------------------------------------------
# Targets
#-------------------------------------------------------------------------------

all: $(EXE)

galerkin: manual_invoke_galerkin_rhs_kernel.o manual_invoke_populate_rhs.o manual_galerkin.F90
	$(LD) $(FFLAGS) $(LDFLAGS) -o $@ manual_galerkin.F90 manual_invoke_populate_rhs.o manual_invoke_galerkin_rhs_kernel.o $(LIBS)

gen:
	python ../../src/generator.py -oalg galerkin_alg.F90 -opsy galerkin_psy.F90 galerkin.F90
	make galerkin_generated

galerkin_generated: galerkin_module.o galerkin_psy.o galerkin_alg.o
	$(LD) $(FFLAGS) $(LDFLAGS) -o $@ galerkin_module.o galerkin_psy.o galerkin_alg.o $(LIBS)

clean:
	rm -f *.o *.mod *.out $(EXE) *_alg.F90 *_psy.F90 fparser.log *_generated

.PHONY:
	clean

#-------------------------------------------------------------------------------
# Patterns
#-------------------------------------------------------------------------------

%.o:%.F90
	$(FC) -c $(FFLAGS)  -fdefault-real-8 $<
%.o:%.f90
	$(FC) -c $(FFLAGS) $<

#-------------------------------------------------------------------------------
# Dependencies
#-------------------------------------------------------------------------------
$(EXE): manual_galerkin.F90

