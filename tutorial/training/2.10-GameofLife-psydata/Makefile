EXE = gol
API = gocean1.0


# We need to compile and link the PSyData libraries:
PSY_DATA_DIR = $(ROOT_DIR)/lib/profiling/simple_timing
PSY_DATA_LIB = $(PSY_DATA_DIR)/libsimple_timing.a
PSY_DATA_INCLUDE = -I $(PSY_DATA_DIR)

F90FLAGS += $(PSY_DATA_INCLUDE)

OBJ = gol.o time_step_alg_mod.o time_step_alg_mod_psy.o

include ../common.mk

$(EXE): $(PSY_DATA_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(PSY_DATA_LIB) $(LDFLAGS) -o $(EXE)

run: $(EXE)
	./$(EXE) config.glider

test: $(EXE)
	@# Due to the additional debug output, we need to take a different
	@# section of the output file:
	make run | tail -n 19  | head -n 12| diff - ./glider.correct

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# External libs
# -------------
$(PSY_DATA_LIB): $(INF_LIB)
	$(MAKE) -C $(PSY_DATA_DIR)

%.f90: %.x90
	$(PSYCLONE) --profile invokes -oalg $*.f90 -opsy $*_psy.f90 $<
