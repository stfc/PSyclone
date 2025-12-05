EXE = gol
API = gocean

OBJ = output_field_mod.o time_step_alg_mod.o time_step_alg_mod_psy.o

# Use nvfortran by default
F90 ?= nvfortran
F90FLAGS ?= -O3 -mp=gpu -gpu=managed -Minfo=all

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

# We can't simply use the default test, since it will require to recompile
# the infrastructure files. So only test the transformation here.
# If compilation is to be tested, use `make test-default`
TS=time_step_alg_mod
test:
	$(PSYCLONE) -s ./omp_offload.py -oalg $(TS).f90 -opsy $(TS)_psy.f90 $(TS).x90
