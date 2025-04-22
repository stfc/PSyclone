EXE = gol
API = gocean

OBJ = output_field_mod.o time_step_alg_mod.o time_step_alg_mod_psy.o

# Use nvfortran by default
F90 ?= nvfortran
F90FLAGS ?= -fopenmp

include ../../common.mk

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# PSyclone: create alg and psy layer files:
%.f90: %.x90 Makefile omp_offload.py
	$(PSYCLONE) -s ./omp_offload.py -oalg $*.f90 -opsy $*_psy.f90 $<
