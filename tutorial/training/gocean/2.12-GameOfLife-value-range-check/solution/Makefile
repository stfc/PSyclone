EXE = gol
API = gocean

# We need to compile and link the PSyData libraries:
PSY_DATA_DIR = $(ROOT_DIR)/lib/value_range_check/dl_esm_inf
PSY_DATA_LIB = $(PSY_DATA_DIR)/lib_value_range_check.a
PSY_DATA_INCLUDE = -I $(PSY_DATA_DIR)

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o compute_born_mod.o

SCRIPT = ./value_range_check.py
include ../../common.mk
F90FLAGS += $(PSY_DATA_INCLUDE)
LDFLAGS += $(PSY_DATA_LIB)

test: test-default
	@# Check that we have the value_range_check object in the PSy-layer
	cat time_step_alg_mod_psy.f90 | grep value_range_check_psy_data >/dev/null

test_run: test $(EXE)
	@# Since we introduce Infinity values, the output is
	@# not correct. Check instead for the warnings:
	$(MAKE) --no-print-directory run 2>&1 | grep "has the invalid value 'Inf'">/dev/null
	@# Check that we can specify a value range for a variable. Note that
	@# values for current are out of [0,1] because of errors introduced in
	@# the kernel on purpose! The error will have the value -1.0, so provide
	@# a flexible regex to identify a floating point number
	PSY_VALUE_RANGE="current%data=0:1" $(MAKE) --no-print-directory run 2>&1 \
		|grep "Variable 'current%data' has the value '[-0-9+.]\+' at index/indices 2 3" \
		>/dev/null

# External libs
# -------------
$(OBJ): $(PSY_DATA_LIB)

$(PSY_DATA_LIB): $(INF_LIB)
	$(MAKE) -C $(PSY_DATA_DIR)

allclean: clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_INC) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(GOL_DIR) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(PSY_DATA_DIR) clean
