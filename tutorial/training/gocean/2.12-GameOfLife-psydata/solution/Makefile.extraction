EXE = gol.extraction
API = gocean

# We need to compile and link the PSyData libraries:
PSY_DATA_DIR = $(ROOT_DIR)/lib/extract/netcdf/dl_esm_inf
PSY_DATA_LIB = $(PSY_DATA_DIR)/lib_extract.a
PSY_DATA_INCLUDE = -I $(PSY_DATA_DIR)

F90FLAGS += $(PSY_DATA_INCLUDE) -DPSYDATA_EXTRACT $$(nf-config --fflags)

OBJ = gol.o time_step_alg_mod.o time_step_alg_mod_psy.o
DRIVER_NAME = driver-psy_time_step_alg_mod-invoke_compute-r0

include ../../common.mk

$(EXE): $(INF_LIB) $(PSY_DATA_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) $(PSY_DATA_LIB) $$(nf-config --flibs) -o $(EXE)

driver: F90FLAGS += $(PSY_DATA_INCLUDE)/..
driver: LDFLAGS +=  $$(nf-config --flibs)
driver: $(DRIVER_NAME).o
	$(F90) $(F90FLAGS) $? $(LDFLAGS) -o $@

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# External libs
# -------------
$(PSY_DATA_LIB): $(INF_LIB)
	$(MAKE) -C $(PSY_DATA_DIR)


.precious: time_step_alg_mod.f90 time_step_alg_mod_psy.f90

# When creating the driver, we need to specify the paths to all module directories
# (which PSyclone will search recursively). Unfortunately, dl_esm_info contains
# two different files with the same module definition (one for MPI, one without),
# which PSyclone will detect and then abort. So we added a copy of the (non)-MPI
# do the local dl_no_mpi directory, and put this path first in the search list,
# so PSyclone will only detect this version.
# The inlclude paths are:
# - the gol-library directory (which is added automatically the Makefile, i.e.#
#   it is part of $(PSYCLONE)
# - the dl_no_mpi directory (containing only parallel_utils_stub_mod)
# - the dl_esm_inf (for all other dl_esm_inf files)
# - the extraction module (from which modules for reading the data and for
#   comparing the results are used):
%.f90: %.x90 Makefile
	$(PSYCLONE) -s ./extraction.py -api gocean1.0 -oalg $*.f90 \
	-d ../dl_no_mpi                                             \
	-d $(ROOT_DIR)/external/dl_esm_inf/finite_difference/src/  \
	-d $(ROOT_DIR)/lib/extract/netcdf/dl_esm_inf/              \
		-opsy $*_psy.f90 $<

# Clean
# -----

clean:
	rm -f $(OBJ) $(EXE) *.mod time_step_alg_mod.f90 time_step_alg_mod_psy.f90
	rm -f $(DRIVER_NAME) $(DRIVER_NAME).mod $(DRIVER_NAME).F90
	rm -f psy_time_step_alg_mod-invoke_compute-r0.nc

allclean:
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_INC) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(GOL_DIR) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(PSY_DATA_DIR) clean
