EXE = gol

TRANS ?= omp_trans.py
include ../common.mk

SRC_X90 = compute_die_mod.x90 compute_born_mod.x90 combine_mod.x90 \
		  count_neighbours_mod.x90 time_step_mod.x90
SRC_F90 = gol.f90 read_config_mod.f90 output_field_mod.f90

OBJ = $(SRC_F90:%.f90=%.o) $(SRC_X90:%.x90=%.o)


$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

# Generic compilation rule
# ------------------------
.precious: $(SRC_X90:%.x90=%.f90)

# This will need an additional flag
%.f90: %.x90 $(TRANS)
	$(PSYCLONE) -s ./$(TRANS) -o $*.f90 $<

%.o: %.f90
	$(F90) -c $(F90FLAGS) $<


# Dependencies:
# -------------
$(OBJ): $(INF_LIB)
gol.o: read_config_mod.o time_step_mod.o
time_step_mod.o: combine_mod.o compute_born_mod.o compute_die_mod.o \
	count_neighbours_mod.o output_field_mod.o

clean:
	rm -f $(OBJ) $(EXE) *.mod $(SRC_X90:%.x90=%.f90)
