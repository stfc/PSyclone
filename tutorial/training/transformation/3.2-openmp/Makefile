EXE = gol

SCRIPT ?= omp_trans.py
include ../common.mk

PSY_SRC = compute_die_mod.f90 compute_born_mod.f90 combine_mod.f90 \
		  count_neighbours_mod.f90 time_step_mod.f90
PROCESSED_SRC = $(PSY_SRC:%.f90=%.processed.f90)

SRC_F90 = gol.f90 read_config_mod.f90 output_field_mod.f90

OBJ = $(SRC_F90:%.f90=%.o) $(PROCESSED_SRC:%.f90=%.o)

# Default target: run all transformations
transform: $(PROCESSED_SRC)

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

# Generic compilation rule
# ------------------------
.precious: $(SRC_X90:%.x90=%.f90)

# This will need an additional flag when using omp_parallel.py
# to disable backend validation (see README.md)
$(PROCESSED_SRC): %.processed.f90: %.f90 $(SCRIPT)
	$(PSYCLONE) --backend-disable-validation -s ./$(SCRIPT) -o $*.processed.f90 $<

%.o: %.f90
	$(F90) -c $(F90FLAGS) $<

# Dependencies:
# -------------
gol.o: read_config_mod.o time_step_mod.processed.o
time_step_mod.processed.o: combine_mod.processed.o compute_born_mod.processed.o compute_die_mod.processed.o \
	count_neighbours_mod.processed.o output_field_mod.o

clean:
	rm -f $(OBJ) $(EXE) *.mod $(PROCESSED_SRC)
