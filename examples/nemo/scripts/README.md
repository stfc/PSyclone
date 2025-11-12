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
> scripts that come with the NEMO repository with the `-p` flag (see
> [the NEMO user guide](https://sites.nemo-ocean.io/user-guide/psyclone.html)),
> but these are pinned to a particular release of PSyclone and have constrains
> defined in `mk/sct_psyclone.sh` script. By contrast, the process presented in
> this README uses the experimental `psyclonefc` compiler wrapper command which
> bypases the `makenemo -p` and instead intercepts any compilation command and
> wraps it with a psyclone call followed by a compiler call.
> This is the recommended way to apply upstream psyclone transformations, as it
> is not constrained by the file-exclusions and backward compatibility guarantees
> of the scripts inside the NEMO repository.

## Downloading the NEMO source and data files

To test the examples you can download NEMO and its input data as follows:
```bash
git clone https://forge.nemo-ocean.eu/nemo/nemo.git --branch 5.0 --single-branch
wget https://gws-access.jasmin.ac.uk/public/nemo/sette_inputs/r5.0.0/ORCA2_ICE_v5.0.0.tar.gz
tar -xzf ORCA2_ICE_v5.0.0.tar.gz
```

The examples have been tested with NEMOv4.0.2 (SPLITZ configuration) and
NEMOv5.0 (BENCH and ORCA_ICE_PISCIES configuration), but we aim to support
any version of NEMO. If you encounter any issue applying these examples
please report to the authors.


## Set up environment variables

In order to provide a flexible system that works with different directives and
compilers we provide a parameterised transformation script
`insert_loop_parallelism.py` and an example NEMO arch file `KGO/arch-linux_spack.fcm`
with multiple environment variables. These, together with the `psyclonefc`
environment variables have to be set up appropriately depending on the desired
output.

First of all, the arch file has a `MPIF90` to choose the compiler, this
needs to be set to `psyclonefc`. This is a compiler wrapper utility that
substitutes its call with: an invocation to psyclone to process the given
source file (using the options provided in `PSYCLONE_OPTS`) and then send the
output to a compiler (provided by `PSYCLONE_COMPILER`).

For example, to apply the `insert_loop_parallelism.py` and compile it with
`mpif90` we can use the following set up:

```bash
export MPIF90=psyclonefc
export PSYCLONE_COMPILER=mpif90
export PSYCLONE_OPTS="-l output -s ${PSYCLONE_NEMO_EXAMPLES_DIR}/insert_loop_parallelism.py"
```

As mentioned, the transformation script is parameterised with a `PARALLEL_DIRECTIVES`
variable that have to be consistent with the chosen Fortran flags.

For instance, for the `nvfortran` compiler, you can choose between:
- Serial transformations with no parallel directives
```bash
export PARALLEL_DIRECTIVES=""
export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g"
```

- Inserting OpenMP CPU threading parallelism
```bash
export PARALLEL_DIRECTIVES="omp_threading"
export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g -mp"
```

- Inserting OpenMP GPU offloading with reproducible build flags
```bash
export PARALLEL_DIRECTIVES="omp_offloading"
export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g -mp=gpu -gpu=mem:managed,math_uniform"
export REPRODUCIBLE=1
```

- Inserting OpenACC GPU offloading with reproducible build flags (-mp=gpu is needed for reproducibility)
```bash
export PARALLEL_DIRECTIVES="acc_offloading"
export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g -acc=gpu -mp=gpu -gpu=mem:managed,math_uniform"
export REPRODUCIBLE=1
```

- A fast GPU build flags
```bash
unset REPRODUCIBLE
export PARALLEL_DIRECTIVES="omp_offloading+omp_threading"
export FCFLAGS="-i4 -Mr8 -O3 -mp=gpu -gpu=mem:managed"
```

> [!Note]
> Currently, NEMOv4 and NEMOv5 take different optimisation paths, so it is
> imporant to also set:
>
> ```bash
> export NEMOv4=1
> ```
> when applying the transformations to NEMOv4.

TODO: Mention `ASYNC_PARALLEL`, `ENABLE_INLINING`, `PROFILING`

## Compiling and running the application

Once the environment variables are set, use the `makenemo` command with
the desired NEMO configuration and keys. For example:

```bash
./makenemo -r ORCA2_ICE_PISCES -m arch-linux_spack -n ORCA2_psycloned ...
```

If everything worked you can see the generated files in the
`<configuration>/BLD/tmp` directory. And you can run the binary from the
EXP00 directory. For example, for a hybrid MPI+OMP offloading+OMP threading
we can do:

```bash
# Prepare problem
ln -sf ${ORCA2_INPUTS}/ORCA2_ICE_v5.0.0/* cfgs/ORCA2_psycloned/EXP00/.
cd cfgs/ORCA2_psycloned/EXP00
# Reduce num of iterations and add timing/runstat
sed -i "s/nn_itend.*/nn_itend = 10/" namelist_cfg
sed -i "s/ln_icebergs.*/ln_icebergs = .false./" namelist_cfg
sed -i "s/\&namctl.*/\&namctl\n ln_timing   = .true. \n sn_cfctl%l_runstat = .true.\n/" namelist_cfg
# Run problem
OMP_NUM_THREADS=4 CUDA_VISIBLE_DEVICES=1,2 mpirun -n 2 ./nemo
```

## Identifying the cause of issues

A difficulty of working with code-transformation scripts is that it is possible
to incorrect transform a file semantics while still creating valid Fortran.
This means that the transformation will succeed and the generated code will
compile, but the results will diverge. This gets more complicated with parallel
programming because certain operations like reductions or atomics are not
always reproducible. For NEMO we typically compare the generated `run.stat` field
values. To do that we recommend:

- Starting building NEMO without `psyclonefc` and conservative optimisation flags
  and ru it serially. Then store the generated `run.stat`.
- Then switch to using `psyclonefc` with the `PSYCLONE_OTPS="-s passthrough.py"`,
  this will make all files pass through psyclone but without applying any
  transformations. Check if the results still match.
- Then build it with `PARALLEL_DIRECTIVES="" PSYCLONE_OTPS="-s insert_loop_parallelism.py"`
  and check if the results still match
- Then run it `REPRODUCIBLE=1 PARALLEL_DIRECTIVES="omp_threading" PSYCLONE_OTPS="-s insert_loop_parallelism.py"`
  and see if the results still match.
- Finally, run it with `REPRODUCIBLE=1 PARALLEL_DIRECTIVES="omp_offloading" PSYCLONE_OTPS="-s insert_loop_parallelism.py"`

Orthogonally to finding which step is causing the divergence we may want to find
which file/s are causing it. This folder also contains a `do_file_by_file.sh`
script that build NEMO many times, each with only one file being transformed,
and compares the results with the stores `run.stat`


## Tuning the generated implementation

Since this is now a two-step process, there are two locations where you can modify
files that will alter the output result. First is manually modifying the original
source code. For this we recommend using the built-in `makenemo` functionality
that allow to point to a directory with patched source files:

```bash
./makenemo -e <directory> ...
```

In addition to the source, you can also modify the recipe that psyclone uses to
transform the code. In this example you can do so by changing any detail of the
`insert_loop_parallelism.py` transformation script, but the `FILES_TO_SKIP`
global variable is particularly relevant as it allows psyclone skip processing
the listed files. If modifying a particular file is known to cause problems or
performance regressions, include it in this list.

You can also do both. For example if you want to provide a modified file that
already includes directives, you need to reference it with the `-e <path>`
and in the FILES_TO_SKIP (otherwise Psyclone would ignore the given directives
and try to insert its own). This is currently the optimal approach for `seaice`
and `lbclnk.f90` GPU offloading.
