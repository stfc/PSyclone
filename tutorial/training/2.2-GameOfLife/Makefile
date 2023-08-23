EXE = gol
API = gocean

include ../common.mk

SRC = gol.f90 read_config_mod.f90 time_step_mod.f90 compute_die_mod.f90 \
	  compute_born_mod.f90 combine_mod.f90 count_neighbours_mod.f90     \
	  output_field_mod.f90
OBJ = $(SRC:%.f90=%.o)

.PHONY: allclean clean

$(EXE): $(INF_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

run: $(EXE)
	./$(EXE) config.glider

test: $(EXE)
	./$(EXE) config.glider | tail -n 12 | diff - ./glider.correct

# Dependencies:
# -------------
$(OBJ): $(INF_LIB)
gol.o: read_config_mod.o time_step_mod.o
time_step_mod.o: combine_mod.o compute_born_mod.o compute_die_mod.o \
	count_neighbours_mod.o output_field_mod.o
