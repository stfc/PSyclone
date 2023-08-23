EXE = gol
API = gocean1.0

# Don't include gol-lib at link time, it is not required
IGNORE_GOL_LIB=yes
include ../common.mk

SRC = gol.f90 read_config_mod.f90 time_step_mod.f90 compute_die_mod.f90 \
	  compute_born_mod.f90 combine_mod.f90 count_neighbours_mod.f90     \
	  output_field_mod.f90
OBJ = $(SRC:%.f90=%.o)

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

run: $(INF_LIB) $(EXE)
	./$(EXE) config.glider

test: $(EXE)
	./$(EXE) config.glider | tail -n 12 | diff - ./glider.correct

# Dependencies:
# -------------
$(OBJ): $(INF_LIB)
gol.o: read_config_mod.o time_step_mod.o
time_step_mod.o: combine_mod.o compute_born_mod.o compute_die_mod.o \
	count_neighbours_mod.o output_field_mod.o
