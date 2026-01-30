EXE = gol

include ../common.mk

PSY_SRC = time_step_mod.f90
PROCESSED_SRC = $(PSY_SRC:%.f90=%.processed.f90)

SRC_F90 = gol.f90 read_config_mod.f90 output_field_mod.f90 compute_die_mod.f90 \
		  compute_born_mod.f90 combine_mod.f90 count_neighbours_mod.f90

OBJ = $(SRC_F90:%.f90=%.o) $(PROCESSED_SRC:%.f90=%.o)

transform: $(PROCESSED_SRC)

test: transform
	@# Check that omp statements and tiling loops are present:
	cat time_step_mod.processed.f90 | grep "omp *parallel *default" >/dev/null
	cat time_step_mod.processed.f90 | grep "omp *do *schedule" >/dev/null
	cat time_step_mod.processed.f90 | grep "do j_out_var" >/dev/null
	cat time_step_mod.processed.f90 | grep "do i_out_var" >/dev/null
	cat time_step_mod.processed.f90 | grep "do j = j_out_var" >/dev/null
	cat time_step_mod.processed.f90 | grep "do i = i_out_var" >/dev/null

# Run all tests first, and the default (checking output KGO),
# then ensure we did indeed compile with threads:
test_run: test test_run-default
	OMP_NUM_THREADS=2 $(MAKE) run | grep "Using *2 *threads" > /dev/null

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

# Generic compilation rule
# ------------------------
.precious: $(PROCESSED_SRC)

time_step_mod.processed.f90: time_step_mod.f90 tiling_with_openmp.py
	$(PSYCLONE) -s ./tiling_with_openmp.py -o time_step_mod.processed.f90 time_step_mod.f90

%.o: %.f90
	$(F90) -c $(F90FLAGS) $<

# Dependencies:
# -------------
$(OBJ): $(INF_LIB)
gol.o: read_config_mod.o time_step_mod.processed.o
time_step_mod.processed.o: combine_mod.o compute_born_mod.o compute_die_mod.o \
	count_neighbours_mod.o output_field_mod.o

clean:
	rm -f $(OBJ) $(EXE) *.mod $(PROCESSED_SRC)
