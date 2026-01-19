EXE = gol

SCRIPT ?= omp_trans.py
include ../../common.mk

PSY_SRC = compute_die_mod.f90 compute_born_mod.f90 combine_mod.f90 \
		  count_neighbours_mod.f90 time_step_mod.f90
PROCESSED_SRC = $(PSY_SRC:%.f90=%.processed.f90)

SRC_F90 = gol.f90 read_config_mod.f90 output_field_mod.f90

OBJ = $(SRC_F90:%.f90=%.o) $(PROCESSED_SRC:%.f90=%.o)

# Default target: run all transformations
transform: $(PROCESSED_SRC)

test:
	$(MAKE) clean
	SCRIPT=omp_trans.py $(MAKE) compute_die_mod.processed.f90
	cat compute_die_mod.processed.f90 | grep "omp *parallel *do *default" > /dev/null
	$(MAKE) clean
	SCRIPT=omp_parallel.py $(MAKE) time_step_mod.processed.f90
	cat time_step_mod.processed.f90 | grep "omp *parallel *default" > /dev/null
	SCRIPT=omp_parallel.py $(MAKE) compute_die_mod.processed.f90
	cat compute_die_mod.processed.f90 | grep "omp *do *schedule" > /dev/null

test_run: 
	@# We need to be careful not to mix files (i.e. some files transformed
	@# with omp_parallel, others with omp_trans). So make sure we always have
	@# a clean start:
	$(MAKE) clean
	$(MAKE) test
	$(MAKE) clean
	@# Check stdout for correctness
	SCRIPT=omp_trans.py OMP_NUM_THREADS=2 $(MAKE) test_run-default
	@# Check that threading was indeed enabled
	SCRIPT=omp_trans.py OMP_NUM_THREADS=2 $(MAKE) run | grep "Using *2 *threads" > /dev/null

	$(MAKE) clean
	@# Check stdout for correctness
	SCRIPT=omp_parallel.py OMP_NUM_THREADS=2 $(MAKE) test_run-default
	@# Check that threading was indeed enabled
	SCRIPT=omp_parallel.py OMP_NUM_THREADS=2 $(MAKE) run | grep "Using *2 *threads" > /dev/null

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

# Generic compilation rule
# ------------------------
.precious: $(SRC_X90:%.x90=%.f90)

ifeq ($(SCRIPT), omp_parallel.py)
$(PROCESSED_SRC): %.processed.f90: %.f90 $(SCRIPT)
	$(PSYCLONE) --backend-disable-validation -s ./$(SCRIPT) -o $*.processed.f90 $<
else
$(PROCESSED_SRC): %.processed.f90: %.f90 $(SCRIPT)
	$(PSYCLONE) -s ./$(SCRIPT) -o $*.processed.f90 $<
endif

%.o: %.f90
	$(F90) -c $(F90FLAGS) $<

# Dependencies:
# -------------
$(OBJ): $(INF_LIB)
gol.o: read_config_mod.o time_step_mod.processed.o
time_step_mod.processed.o: combine_mod.processed.o compute_born_mod.processed.o compute_die_mod.processed.o \
	count_neighbours_mod.processed.o output_field_mod.o

clean:
	rm -f $(OBJ) $(EXE) *.mod $(PROCESSED_SRC)
