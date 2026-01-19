EXE = gol

SCRIPT ?= omp_trans.py
include ../../common.mk

SRC_X90 = compute_die_mod.x90 compute_born_mod.x90 combine_mod.x90 \
		  count_neighbours_mod.x90 time_step_mod.x90
SRC_F90 = gol.f90 read_config_mod.f90 output_field_mod.f90

OBJ = $(SRC_F90:%.f90=%.o) $(SRC_X90:%.x90=%.o)

transform: $(SRC_X90:%.x90=%.f90)

test:
	$(MAKE) clean
	SCRIPT=omp_trans.py $(MAKE) compute_die_mod.f90
	cat compute_die_mod.f90 | grep "omp *parallel *do *default" > /dev/null
	$(MAKE) clean
	SCRIPT=omp_parallel.py $(MAKE) time_step_mod.f90
	cat time_step_mod.f90 | grep "omp *parallel *default" > /dev/null
	SCRIPT=omp_parallel.py $(MAKE) compute_die_mod.f90
	cat compute_die_mod.f90 | grep "omp *do *schedule" > /dev/null

test_run: test $(EXE)
	$(MAKE) clean
	SCRIPT=omp_trans.py OMP_NUM_THREADS=2 $(MAKE) run | grep "Using *2 *threads" > /dev/null
	$(MAKE) clean
	SCRIPT=omp_parallel.py OMP_NUM_THREADS=2 $(MAKE) run | grep "Using *2 *threads" > /dev/null


$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

# Generic compilation rule
# ------------------------
.precious: $(SRC_X90:%.x90=%.f90)

ifeq ($(SCRIPT), omp_parallel.py)
%.f90: %.x90 $(SCRIPT)
	$(PSYCLONE) --backend-disable-validation -s ./$(SCRIPT) -o $*.f90 $<
else
%.f90: %.x90 $(SCRIPT)
	$(PSYCLONE) -s ./$(SCRIPT) -o $*.f90 $<
endif

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
