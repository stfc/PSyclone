<!--
BSD 3-Clause License

Copyright (c) 2025, Science and Technology Facilities Council.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

Author S. Siso, STFC Daresbury Lab

-->

# PSyclone NEMO Examples

This directory contains various examples of the use of PSyclone to transform
source code from the NEMO ocean model.

> [!Important]
> The NEMO build system, `makenemo`, has the ability to apply psyclone
> scripts that come with the NEMO repository (see
> [the NEMO user guide](https://sites.nemo-ocean.io/user-guide/psyclone.html))
> but these are pinned to a particular release of PSyclone. By contrast,
> the process presented in this README uses the experimenatal `psyclonefc`
> command to intercept any compilation command and wrap it with a psyclone
> code-transformation step, and therefore bypasses the makenemo `-p` option.
> This is the recommended way to apply upstream psyclone transformations, as it
> is not contrained by the file-exclusions and backward compatibility guarantees
> of the `makenemo` scripts.

## Downloading the NEMO source and data files


```bash
git clone https://forge.nemo-ocean.eu/nemo/nemo.git --branch 5.0 --single-branch
wget https://gws-access.jasmin.ac.uk/public/nemo/sette_inputs/r5.0.0/ORCA2_ICE_v5.0.0.tar.gz
tar -xzf ORCA2_ICE_v5.0.0.tar.gz
``` 

## Set up environment variables

The code that psyclone produces, the compiler that reads it and the flags that
the build system uses should match in order to produce a successful run. In this
example we use the `KGO/arch-linux_spack.fcm` and the `insert_loop_parallelism.py`
transformation script. Both contain environment variables 

First, the arch file MPIF90 needs to be set to `psyclonefc`, this is a command line utility
that can be used instead of a call to the compiler that first processes the given
source file (using the options give to PSYCLONE_OPTS) and then send the output to
a compiler (given by PSYCLONE_COMPILER).



```bash
export MPIF90=psyclonefc
export PSYCLONE_COMPILER=mpif90
export PSYCLONE_OPTS="-l output -s ${PSYCLONE_NEMO_EXAMPLES_DIR}/insert_loop_parallelism.py"
```

Then the injected parallel directives  

Chose the flags between:
- Example of serial transformations with no parallel directives
```bash
export PARALLEL_DIRECTIVES=""
export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g"
```

- Example of inserting OpenMP CPU threading parallelism
```bash
export PARALLEL_DIRECTIVES="omp_threading"
export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g -mp"
```

- Example of inserting OpenMP GPU offloading with reproducible build flags
```bash
export PARALLEL_DIRECTIVES="omp_offloading"
export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g -mp=gpu -gpu=mem:managed,math_uniform"
export REPRODUCIBLE=1
```

- Example of inserting OpenACC GPU offloading with reproducible build flags (-mp=gpu is needed for reproducibility)
```bash
export PARALLEL_DIRECTIVES="acc_offloading"
export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g -acc=gpu -mp=gpu -gpu=mem:managed,math_uniform"
export REPRODUCIBLE=1
```

- Example of fast GPU build flags 
```bash
unset REPRODUCIBLE
export PARALLEL_DIRECTIVES="omp_offloading+omp_threading"
export FCFLAGS="-i4 -Mr8 -O3 -mp=gpu -gpu=mem:managed"
```

## Compile and Run NEMO with psyclone processing

```bash
./makenemo -r ORCA2_ICE_PISCES -m linux_test -n ORCA2_psycloned del_key "key_xios key_top" -j 6 -v 1
```

```bash
# Prepare problem
ln -sf ${ORCA2_INPUTS}/ORCA2_ICE_v5.0.0/* cfgs/ORCA2_psycloned/EXP00/.
cd cfgs/ORCA2_ICE_PISCES_psycloned/EXP00
# Reduce num of iterations and add timing/runstat
sed -i "s/nn_itend.*/nn_itend = 10/" namelist_cfg
sed -i "s/ln_icebergs.*/ln_icebergs = .false./" namelist_cfg
sed -i "s/\&namctl.*/\&namctl\n ln_timing   = .true. \n sn_cfctl%l_runstat = .true.\n/" namelist_cfg

# Run problem
OMP_NUM_THREADS=4 CUDA_VISIBLE_DEVICES=1,2 mpirun -n 2 ./nemo
diff ${PSYCLONE_NEMO_EXAMPLES_DIR}/KGOs/run.stat.orca_ice_pisces.nvhpc.10steps run.stat

```

