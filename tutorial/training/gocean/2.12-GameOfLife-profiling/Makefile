EXE = gol
API = gocean

# We need to compile and link the PSyData libraries:
PSY_DATA_DIR = $(ROOT_DIR)/lib/profiling/simple_timing
PSY_DATA_LIB = $(PSY_DATA_DIR)/libsimple_timing.a
PSY_DATA_INCLUDE = -I $(PSY_DATA_DIR)

# We need to compile our own gol.F90, since it contains
# the PSydata init and shutdown calls (which the simple
# timer library needs).
OBJ = gol.o time_step_alg_mod.o time_step_alg_mod_psy.o

ADD_PSYCLONE_OPTIONS = --profile kernels

include ../common.mk
F90FLAGS += $(PSY_DATA_INCLUDE) -DPSYDATA_PROFILE
LDFLAGS += $(PSY_DATA_LIB)

test: test-default
	@# Check that we have the profile_psy_data object in the PSy-layer
	cat time_step_alg_mod_psy.f90 | grep profile_psy_data >/dev/null

test_run: test $(EXE)
	@# Due to the additional profiling output, we need to take a different
	@# section of the output file. Additionally, the white space is compiler
	@# dependent. The sed command cuts off the profiling output, then we
	@# cut the last state
	$(MAKE) --no-print-directory run | sed '/===========/q' \
		| tail -n 14 | head -n 12| diff -w - ../../gol-lib/glider.correct
	@# Also check that we get the output from the simple timer lib
	@# Again, line wrapping is compiler dependent, so test for
	@# profile headers individually
	$(MAKE) run | grep "average" >/dev/null
	$(MAKE) run | grep "min" >/dev/null
	@# The output contains a 'max' and 'count', adding a white space here
	@# (for the tab in the output) makes sure we test for the right one
	$(MAKE) run | grep "[[:space:]]count"  >/dev/null
	$(MAKE) run | grep "[[:space:]]max" >/dev/null

# External libs
# -------------
$(OBJ): $(PSY_DATA_LIB)

$(PSY_DATA_LIB): $(INF_LIB)
	$(MAKE) -C $(PSY_DATA_DIR)

allclean: clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_INC) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(GOL_DIR) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(PSY_DATA_DIR) clean
