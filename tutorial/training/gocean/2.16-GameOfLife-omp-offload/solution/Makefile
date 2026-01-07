EXE = gol
API = gocean

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o

# Use nvfortran with managed memory - the scripts currently only support
# managed memory.
F90 ?= nvfortran
F90FLAGS ?= -O3 -mp=gpu -gpu=managed -Minfo=all

include ../../common.mk

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# PSyclone: create alg and psy layer files:
%.f90: %.x90 Makefile omp_offload.py
	$(PSYCLONE) -s ./omp_offload.py -oalg $*.f90 -opsy $*_psy.f90 $<
