# Makefile to Generate PSyclone versions of NEMO, Compile, and Run them.
# It supports the targets: serial (only compile), openmp-cpu, openmp-gpu, openacc-kernels
# - To generate the psycloned versions use: `make <target>` 
# - To compile that version use: `make compile-<target>`
# - To run that version use: `make run-<target>`

# ---- Start of the configurable part of the Makefile ----

# - Specify NEMO directory
# NEMO_DIR ?=
# - Specify location of PSyclone transformation scripts
PSYCLONE_NEMO_DIR ?= ${WORKSPACE}/PSyclone/examples/nemo/scripts
# - Specify location of preprocessed files - you need to call 'make compile-serial' once first)
# ROOT_SRC ?=
# Where to find the MPI header files
# MPI_INC_DIR ?=
# - Specify NEMO test case
TEST_CASE ?= SPITZ12
# - Specify compilation options
# COMPILER_ARCH ?= 
# ADD_KEYS ?= 
# DEL_KEYS ?=
# - Specify input files
# MODEL_DIR ?=
# NAMELISTS_DIR ?=

# Example Config for MetOffice - eORCA1_GO8 - nvidia
NEMO_DIR ?= ${WORKSPACE}/NEMO
ROOT_SRC ?= ${NEMO_DIR}/cfgs/SPITZ12_serial/BLD/ppsrc/nemo/
MPI_INC_DIR ?= ${NVHPC_ROOT}/comm_libs/mpi/include
COMPILER_ARCH ?= linux_nvidia_omp_gpu
ADD_KEYS ?= "IEEE_IS_NAN=ieee_is_nan key_nosignedzero"
DEL_KEYS ?= "key_iomput key_mpp_mpi key_si3"
MODEL_DIR ?= /home/aporter/NEMO/orca1_inputs/
NAMELISTS_DIR ?= ${NEMO_DIR}/

# Example Config for ECMWF - eORCA1_GO8_Z75 (with MPI and SI3) - intel compiler
# NEMO_DIR ?= ${WORKSPACE}/ecmwf_nemo/nemo/NEMOGCM_V40/
# ROOT_SRC ?= ${NEMO_DIR}/cfgs/SPITZ12_serial/BLD/ppsrc/nemo/
# MPI_INC_DIR ?= ${I_MPI_ROOT}/include
# COMPILER_ARCH ?= linux_intel
# ADD_KEYS ?= "IEEE_IS_NAN=ieee_is_nan key_nosignedzero"
# DEL_KEYS ?= "key_iomput"
# MODEL_DIR ?= /archive/ssiso/ecmwf_eORCA1_GO8/
# NAMELISTS_DIR ?= ${WORKSPACE}/ecmwf_nemo/nemo/testscripts_V40/output/openmp_outer_V40_eORCA1_GO8_Z75_20170906_cray_dp_1x1/

# ---- End of configuration section - do not edit below this point ----

TARGETS := passthrough openmp_cpu openmp_gpu openacc_kernels openacc_loops
SRC_FILES := $(wildcard ${ROOT_SRC}/*.f90)
OUTPUT_FOLDERS := $(addprefix psycloned-, ${TARGETS})

.PHONY: clean

# Generate PSycloned folders
$(OUTPUT_FOLDERS):
	@echo "Creating folder $@"
	mkdir $@

# PSyclone targets (process all the f90 files in ${ROOT_SRC})
$(TARGETS): % : $(addprefix psycloned-%/, $(notdir $(SRC_FILES)))
	@echo "Finished generating $@"

# PSyclone instructions for each target type.

# We perform a 'passthrough' test by simply not providing a transformation
# script.
psycloned-passthrough/%.f90: ${ROOT_SRC}%.f90 psycloned-passthrough
	${PSYCLONE_NEMO_DIR}/process_nemo.py -I ${ROOT_SRC} -I ${MPI_INC_DIR} -o psycloned-passthrough $<

psycloned-openmp_cpu/%.f90: ${ROOT_SRC}%.f90 psycloned-openmp_cpu
	${PSYCLONE_NEMO_DIR}/process_nemo.py -s ${PSYCLONE_NEMO_DIR}/omp_cpu_trans.py -I ${ROOT_SRC} -I ${MPI_INC_DIR} -o psycloned-openmp_cpu $<

psycloned-openmp_gpu/%.f90: ${ROOT_SRC}%.f90 psycloned-openmp_gpu
	${PSYCLONE_NEMO_DIR}/process_nemo.py -s ${PSYCLONE_NEMO_DIR}/omp_gpu_trans.py -I ${ROOT_SRC} -I ${MPI_INC_DIR} -o psycloned-openmp_gpu $<

psycloned-openacc_kernels/%.f90: ${ROOT_SRC}%.f90 psycloned-openacc_kernels
	${PSYCLONE_NEMO_DIR}/process_nemo.py -s ${PSYCLONE_NEMO_DIR}/acc_kernels_trans.py -I ${ROOT_SRC} -I ${MPI_INC_DIR} -o psycloned-openacc_kernels $<

psycloned-openacc_loops/%.f90: ${ROOT_SRC}%.f90 psycloned-openacc_loops
	${PSYCLONE_NEMO_DIR}/process_nemo.py -s ${PSYCLONE_NEMO_DIR}/acc_loops_trans.py -I ${ROOT_SRC} -o psycloned-openacc_loops $<

# Get the number of Makefile parallel jobs to pass it to the makenemo
MAKE_PID := $(shell echo $$PPID)
JOBS := $(shell ps T | sed -n 's/.*$(MAKE_PID).*$(MAKE).* \(-j\|--jobs=\) *\([0-9][0-9]*\).*/\2/p')
JOBS := $(if ${JOBS}, ${JOBS}, 4)  # If none were given, default to 4

# Compile NEMO
compile-%:
	@test -s psycloned-$(lastword $(subst -, ,$@)) || { \
		echo "The psycloned-$(lastword $(subst -, ,$@)) folder does not exist!"; \
		echo "You may need to execute 'make $(lastword $(subst -, ,$@))' first."; \
		echo "Exiting..."; exit 1; }
	cd $(NEMO_DIR) ; \
	./makenemo -n ${TEST_CASE}_$(lastword $(subst -, ,$@)) -r ${TEST_CASE} \
		-e ${PWD}/psycloned-$(lastword $(subst -, ,$@)) -m ${COMPILER_ARCH} -j ${JOBS} \
		add_key ${ADD_KEYS} del_key ${DEL_KEYS}

# The compile-serial is a special case
compile-serial:
	cd $(NEMO_DIR) ; \
	./makenemo -n ${TEST_CASE}_serial -r ${TEST_CASE} \
		-m ${COMPILER_ARCH} -j ${JOBS} add_key ${ADD_KEYS} del_key ${DEL_KEYS}

# Run NEMO
run-%:
	 ln -sf ${MODEL_DIR}/*.nc ${NEMO_DIR}/cfgs/${TEST_CASE}_$(lastword $(subst -, ,$@))/EXP00/.
	 cp ${NAMELISTS_DIR}namelist_* ${NEMO_DIR}/cfgs/${TEST_CASE}_$(lastword $(subst -, ,$@))/EXP00/.
	 cd ${NEMO_DIR}/cfgs/${TEST_CASE}_$(lastword $(subst -, ,$@))/EXP00; ./nemo
	 cd ${NEMO_DIR}/cfgs/${TEST_CASE}_$(lastword $(subst -, ,$@))/EXP00; cat timing.output; cat run.stat

# Run NEMO with NVPROF
nvprof-%:
	 ln -sf ${MODEL_DIR}/*.nc ${NEMO_DIR}/cfgs/${TEST_CASE}_$(lastword $(subst -, ,$@))/EXP00/.
	 cp ${NAMELISTS_DIR}namelist_* ${NEMO_DIR}/cfgs/${TEST_CASE}_$(lastword $(subst -, ,$@))/EXP00/.
	 cd ${NEMO_DIR}/cfgs/${TEST_CASE}_$(lastword $(subst -, ,$@))/EXP00; nsys profile ./nemo

# Clean (only psycloned- folders)
clean:
	rm -rf $(OUTPUT_FOLDERS)
