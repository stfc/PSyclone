my_default:
	MPI=yes F90=mpif90 $(MAKE) MPI=yes default

EXE = gol
API = gocean
MPI = yes

include ../common.mk

SRC = gol.f90 read_config_mod.f90
PSY_SRC = time_step_alg_mod.x90 

OBJ = $(SRC:%.f90=%.o) $(PSY_SRC:%.x90=%.o) $(PSY_SRC:%.x90=%_psy.o)

.PHONY: run test clean

$(EXE): $(INF_LIB) $(GOL_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

run: $(EXE)
	mpirun -np 4 ./$(EXE) config.glider

test: $(EXE)
	mpirun -np 4 ./$(EXE) config.glider | tail -n 12 | diff - ./glider.correct

# Dependencies:
# -------------
gol.o: read_config_mod.o time_step_alg_mod.o
time_step_alg_mod.o: time_step_alg_mod_psy.o


.precious: time_step_alg_mod.f90 time_step_alg_mod_psy.f90

#%.f90: %.x90
#	$(PSYCLONE) -api gocean1.0 -oalg $*.f90 -opsy $*_psy.f90 $<

time_step_alg_mod.f90: time_step_alg_mod.x90
	$(PSYCLONE) -api gocean1.0 -oalg time_step_alg_mod.f90 \
							   -opsy time_step_alg_mod_psy.f90 $<

# Clean
# -----

clean:
	rm -f $(OBJ) $(EXE) *.mod time_step_alg_mod.f90 time_step_alg_mod_psy.f90
