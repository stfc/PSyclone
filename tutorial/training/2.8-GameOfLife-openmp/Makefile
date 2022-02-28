EXE = gol
API = gocean

include ../common.mk

PSY_SRC = time_step_alg_mod.x90 

OBJ = $(PSY_SRC:%.x90=%.o) $(PSY_SRC:%.x90=%_psy.o)

.PHONY: clean run test

$(EXE): $(INF_LIB) $(GOL_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

run: $(EXE)
	./$(EXE) config.glider

test: $(EXE)
	./$(EXE) config.glider | tail -n 12 | diff - ./glider.correct

%.f90: %.x90 Makefile
	$(PSYCLONE) -api gocean1.0 -s ./fuse_loops.py -oalg $*.f90 -opsy $*_psy.f90 $<

# Dependencies:
# -------------
time_step_alg_mod.o: time_step_alg_mod_psy.o

.precious: time_step_alg_mod.f90 time_step_alg_mod_psy.f90

%.f90: %.x90 openmp.py Makefile
	$(PSYCLONE) -s ./openmp.py -api gocean1.0 -oalg $*.f90 -opsy $*_psy.f90 $<
	#$(PSYCLONE) -s ./task.py -api gocean1.0 -oalg $*.f90 -opsy $*_psy.f90 $<

# Clean
# -----

clean:
	rm -f $(OBJ) $(EXE) *.mod time_step_alg_mod.f90 time_step_alg_mod_psy.f90
