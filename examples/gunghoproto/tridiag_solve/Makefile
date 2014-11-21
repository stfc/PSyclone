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
  FFLAGS  = -g -O0 -std=f2003 -Wuninitialized -Wall -Wextra -fbounds-check -fcheck=all
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

OBJ = 

LIBS=-L${GUNGHOSRC}/utils -llfric 
FFLAGS += -I${GUNGHOSRC}/utils -I${GUNGHOSRC}/kernels
EXE=integrate_one galerkin tridiag_solve
#-------------------------------------------------------------------------------
# Targets
#-------------------------------------------------------------------------------

all: $(EXE)
tridiag_solve: \
	manual_invoke_tridiag_solve_kernel.o \
	manual_invoke_tridiag_testdata_kernel.o \
	manual_invoke_tridiag_verify_kernel.o \
	manual_tridiag_solve.o
	$(LD) $(FFLAGS) $(LDFLAGS) -o $@ manual_tridiag_solve.o manual_invoke_tridiag_solve_kernel.o manual_invoke_tridiag_testdata_kernel.o manual_invoke_tridiag_verify_kernel.o $(LIBS)

gen: tridiag_solve_generate

tridiag_solve_generate:
	python ../../src/generator.py -oalg tridiag_solve_alg.F90 -opsy tridiag_solve_psy.F90 tridiag_solve.F90
	make tridiag_solve_generated

tridiag_solve_generated: tridiag_solve_module.o tridiag_solve_psy.o tridiag_solve_alg.o
	$(LD) $(FFLAGS) $(LDFLAGS) -o $@ tridiag_solve_module.o tridiag_solve_psy.o tridiag_solve_alg.o $(LIBS)

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

#-------------------------------------------------------------------------------
# Dependencies
#-------------------------------------------------------------------------------
manual_invoke_tridiag_solve_kernel.o: manual_invoke_tridiag_solve_kernel.F90
manual_invoke_tridiag_testdata_kernel.o: manual_invoke_tridiag_testdata_kernel.F90
manual_invoke_tridiag_verify_kernel.o: manual_invoke_tridiag_verify_kernel.F90
$(EXE):  manual_tridiag_solve.F90

