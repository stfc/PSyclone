EXE = gol
API = gocean

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o

include ../../common.mk

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

TS=time_step_alg_mod
test: test-default
	$(PSYCLONE) -s ./full_inline_loops.py -oalg $(TS).f90 -opsy $(TS)_psy.f90 $(TS).x90

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# PSyclone: create alg and psy layer files:
%.f90: %.x90 Makefile fuse_loops.py
	$(PSYCLONE) -s ./fuse_loops.py -oalg $*.f90 \
		        -opsy $*_psy.f90 $<
