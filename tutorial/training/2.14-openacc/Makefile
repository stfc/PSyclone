EXE = gol
API = gocean1.0

OBJ = output_field_mod.o time_step_alg_mod.o time_step_alg_mod_psy.o

# Use nvfortran by default
F90 ?= nvfortran
F90FLAGS ?= -O3 -acc -Minfo=all

include ../common.mk

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)
test:
	make run | tail -n 12 | head -n 12 | diff - ../gol-lib/glider.correct

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# PSyclone: create alg and psy layer files:
%.f90: %.x90 Makefile openacc.py
	$(PSYCLONE) -s ./openacc.py -oalg $*.f90 -opsy $*_psy.f90 $<
