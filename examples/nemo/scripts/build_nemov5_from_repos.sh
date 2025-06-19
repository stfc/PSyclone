#!/usr/bin/env bash

# This script provides an example of using upstream psyclone to process
# unmodified NEMO (taking the source and data directly from the NEMO forge
# and jasmin repositories). It uses the 'psyclonefc' compiler wrapper, which
# for each compiler invocation it first calls psyclone and then the selected
# compiler. This method is an alternative to the 'makenemo -p', therefore it
# bypasses all the 'mk/sct_psyclone.sh' infrastructure.

# Set up target directories and arch file
export NEMO_V5_DIR=${PWD}/nemov5
export ORCA2_INPUTS=/archive/NEMO_INPUTS/NEMOv5/SETTE_INPUTS/ORCA2_ICE
export PSYCLONE_NEMO_EXAMPLES_DIR=$( dirname -- "$( readlink -f -- "$0"; )"; )
export ARCH_FILE=$PSYCLONE_NEMO_EXAMPLES_DIR/KGOs/arch-linux_spack.fcm

# Set up compiler options: these can also be encoded in the arch-file, but in
# our case we want to them explict in the script, our arch-file is parameterised
# with the following environenment variables:
# Chose the compiler:
export MPIF90=mpif90

# Chose the flags between:
# - Do the serial transformations but do not parallelise
# export PARALLEL_DIRECTIVES=""
# export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g"

# - OpenMP CPU threading parallelism
# export PARALLEL_DIRECTIVES="omp_threading"
# export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g -mp"

# - OpenMP GPU offloading with reproducible build flags
# export PARALLEL_DIRECTIVES="omp_offloading"
# export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g -mp=gpu -gpu=mem:managed,math_uniform"
# export REPRODUCIBLE=1

# - OpenACC GPU offloading with reproducible build flags
export PARALLEL_DIRECTIVES="acc_offloading"
export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g -acc -gpu=mem:managed,math_uniform"
export REPRODUCIBLE=1

# - Fast GPU build flags 
# unset REPRODUCIBLE
# export PARALLEL_DIRECTIVES="omp_offloading+omp_threading"
# export FCFLAGS="-i4 -Mr8 -O3 -mp=gpu -gpu=mem:managed"

# === END OF SET UP section ===

# Download the necessary files
if [ ! -d ${NEMO_V5_DIR} ] ; then
  echo "Downloading NEMO v5.0 repo ..."
  git clone https://forge.nemo-ocean.eu/nemo/nemo.git ${NEMO_V5_DIR} --branch 5.0 --single-branch
fi
if [ ! -d ${ORCA2_INPUTS} ] ; then
  echo "Downloading ORCA2 inputs ..."
  mkdir -p ${ORCA2_INPUTS}
  cd ${ORCA2_INPUTS}
  wget https://gws-access.jasmin.ac.uk/public/nemo/sette_inputs/r5.0.0/ORCA2_ICE_v5.0.0.tar.gz
  tar -xzf ORCA2_ICE_v5.0.0.tar.gz
fi

# Clean up previous build
cd $NEMO_V5_DIR
cp $ARCH_FILE arch/arch-linux_test.fcm
# rm -rf cfgs/ORCA2_ICE_PISCES_psycloned

# Use psyclonefc to intercept the MPIF90 compiler
export PSYCLONE_COMPILER=$MPIF90
export MPIF90=psyclonefc
export PSYCLONE_OPTS="-l output -s ${PSYCLONE_NEMO_EXAMPLES_DIR}/insert_loop_parallelism.py"

./makenemo -r ORCA2_ICE_PISCES -m linux_test -n ORCA2_ICE_PISCES_psycloned del_key "key_xios key_top" -j 6 -v 1

ln -sf ${ORCA2_INPUTS}/ORCA2_ICE_v5.0.0/* cfgs/ORCA2_ICE_PISCES_psycloned/EXP00/.
# Reduce num of iterations and add timing/runstat
sed -i "s/nn_itend.*/nn_itend = 10/" cfgs/ORCA2_ICE_PISCES_psycloned/EXP00/namelist_cfg
sed -i "s/ln_icebergs.*/ln_icebergs = .false./" cfgs/ORCA2_ICE_PISCES_psycloned/EXP00/namelist_cfg
sed -i "s/\&namctl.*/\&namctl\n ln_timing   = .true. \n sn_cfctl%l_runstat = .true.\n/" cfgs/ORCA2_ICE_PISCES_psycloned/EXP00/namelist_cfg
cd cfgs/ORCA2_ICE_PISCES_psycloned/EXP00
OMP_NUM_THREADS=4 mpirun -n 1 ./nemo
diff ${PSYCLONE_NEMO_EXAMPLES_DIR}/KGOs/run.stat.orca_ice_pisces.nvhpc.10steps run.stat
grep -r "Elapsed" -A 3 timing.output

