EXE = gol
API = gocean

# We need to compile and link the PSyData libraries:
PSY_DATA_DIR = $(ROOT_DIR)/lib/profiling/simple_timing
PSY_DATA_LIB = $(PSY_DATA_DIR)/libsimple_timing.a
PSY_DATA_INCLUDE = -I $(PSY_DATA_DIR)

F90FLAGS += $(PSY_DATA_INCLUDE) -DPSYDATA_PROFILE

OBJ = gol.o time_step_alg_mod.o time_step_alg_mod_psy.o

include ../common.mk

$(EXE): $(INF_LIB) $(PSY_DATA_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(PSY_DATA_LIB) $(LDFLAGS) -o $(EXE)

test: $(EXE)
	@# Due to the additional profiling output, we need to take a different
	@# section of the output file:
	make run | tail -n 21  | head -n 12| diff - ../gol-lib/glider.correct

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# External libs
# -------------
$(PSY_DATA_LIB): $(INF_LIB)
	$(MAKE) -C $(PSY_DATA_DIR)

%.f90: %.x90 Makefile
	$(PSYCLONE) --profile kernels -oalg $*.f90 -opsy $*_psy.f90 $<

allclean:
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_INC) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(GOL_DIR) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(PSY_DATA_DIR) clean
