#MPI=yes F90=mpif90 $(MAKE) MPI=yes default

EXE = gol
API = gocean
MPI = yes
F90 = mpif90

SRC = read_config_mod.f90
PSY_SRC = time_step_alg_mod.x90 

OBJ = $(SRC:%.f90=%.o) $(PSY_SRC:%.x90=%.o) $(PSY_SRC:%.x90=%_psy.o)

include ../common.mk

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

run: $(EXE)
	mpirun -np 4 ./$(EXE) config.glider

test: $(EXE)
	mpirun -np 4 ./$(EXE) config.glider | tail -n 12 | diff - ./glider.correct

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

time_step_alg_mod.f90: time_step_alg_mod.x90
	$(PSYCLONE) -api gocean1.0 -oalg time_step_alg_mod.f90 \
							   -opsy time_step_alg_mod_psy.f90 $<