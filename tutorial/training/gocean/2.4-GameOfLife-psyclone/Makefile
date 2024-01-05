EXE = gol
API = gocean1.0
IGNORE_GOL_LIB = yes

SRC = gol.f90 read_config_mod.f90 compute_die_mod.f90               \
          compute_born_mod.f90 combine_mod.f90 count_neighbours_mod.f90 \
          output_field_mod.f90 time_step_alg_mod.f90 time_step_alg_mod_psy.f90

OBJ = $(SRC:%.f90=%.o)

include ../common.mk

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90

gol.o: read_config_mod.o time_step_alg_mod.o
time_step_alg_mod.o:time_step_alg_mod_psy.o output_field_mod.o
time_step_alg_mod_psy.o: combine_mod.o compute_born_mod.o compute_die_mod.o \
        count_neighbours_mod.o

# PSyclone: create alg and psy layer files:
time_step_alg_mod.f90: time_step_alg_mod.x90
	$(PSYCLONE) -api gocean1.0 -oalg time_step_alg_mod.f90     \
						       -opsy time_step_alg_mod_psy.f90 \
								     time_step_alg_mod.x90
