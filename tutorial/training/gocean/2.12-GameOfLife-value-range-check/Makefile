EXE = gol
API = gocean

# We need to compile and link the PSyData libraries:
PSY_DATA_DIR = $(ROOT_DIR)/lib/value_range_check/dl_esm_inf
PSY_DATA_LIB = $(PSY_DATA_DIR)/lib_value_range_check.a
PSY_DATA_INCLUDE = -I $(PSY_DATA_DIR)

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o compute_born_mod.o

SCRIPT = ./value_range_check.py
include ../common.mk
F90FLAGS += $(PSY_DATA_INCLUDE)
LDFLAGS += $(PSY_DATA_LIB)

# External libs
# -------------
$(OBJ): $(PSY_DATA_LIB)

$(PSY_DATA_LIB): $(INF_LIB)
	$(MAKE) -C $(PSY_DATA_DIR)

allclean:
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_INC) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(GOL_DIR) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(PSY_DATA_DIR) clean
