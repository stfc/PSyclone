EXE = gol
API = gocean1.0

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o

include ../common.mk

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

run: $(EXE)
	./$(EXE) config.glider

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# PSyclone: create alg and psy layer files:
time_step_alg_mod.f90: time_step_alg_mod.x90
	$(PSYCLONE) -api gocean1.0 -oalg time_step_alg_mod.f90     \
						       -opsy time_step_alg_mod_psy.f90 \
								     time_step_alg_mod.x90
