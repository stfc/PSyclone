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

This directory contains various examples showing how to use PSyclone to
transform the source code of the NEMO ocean model.

> [!Important]
> The NEMO build system, `makenemo`, has the ability to apply PSyclone
> scripts that come with the NEMO repository with the `-p` flag (see
> [the NEMO user guide](https://sites.nemo-ocean.io/user-guide/psyclone.html)),
> but these are pinned to a particular release of PSyclone and have constraints
> defined in `mk/sct_psyclone.sh` script. By contrast, the process presented in
> this README uses the experimental `psyclonefc` compiler wrapper command which
> bypasses the `makenemo -p` and instead intercepts any compilation command and
> wraps it with a PSyclone call followed by a compiler call.
> This is the recommended way to apply upstream PSyclone transformations, as it
> is not constrained by the sct_psyclone file-exclusions and is compatible with
> older versions of NEMO before the `-p` flag was introduced.

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

In order to support multiple compilers and target devices we provide a parameterised
transformation script `insert_loop_parallelism.py` and a parameterised NEMO arch file
`KGO/arch-linux_spack.fcm`, both contain multiple environment variables that need
to be adjusted depending on your desired optimisation target.

First of all, the arch file has a `MPIF90` to choose the compiler, this
needs to be set to `psyclonefc`. This is a compiler wrapper utility that
substitutes its calls with: an invocation to PSyclone to process the given
source file (using the options provided in `PSYCLONE_OPTS`) followed by an
invocation to a compiler (provided by `PSYCLONE_COMPILER`).

For example, to apply the `insert_loop_parallelism.py` and compile it with
`mpif90` we can use the following set up:

```bash
export MPIF90=psyclonefc
export PSYCLONE_COMPILER=mpif90
export PSYCLONE_OPTS="-l output -s ${PSYCLONE_NEMO_EXAMPLES_DIR}/insert_loop_parallelism.py"
```

This transformation script looks at the environment variable `PARALLEL_DIRECTIVES`
to decide which directives to inject. Additionally the `REPRODUCIBLE` environemnt varaible
specifies if the parallelisation has to produce bit-reproducible results to the serial version.
Both options have to be consistent with the flags used by the NEMO arch file, in our case these
are set by the `FCFLAGS` environemnt variable.

For example, using the `nvfortran` compiler, you can choose between:
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

- Inserting OpenMP GPU offloading with bit-reporducible build flags
```bash
export PARALLEL_DIRECTIVES="omp_offloading"
export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g -mp=gpu -gpu=mem:managed,math_uniform"
export REPRODUCIBLE=1
```

- Inserting OpenACC GPU offloading with bit-reproducible build flags (-mp=gpu is needed for reproducibility)
```bash
export PARALLEL_DIRECTIVES="acc_offloading"
export FCFLAGS="-i4 -Mr8 -O2 -Mnofma -Mnovect -g -acc=gpu -mp=gpu -gpu=mem:managed,math_uniform"
export REPRODUCIBLE=1
```

- Hybrid directives (what cannot be offloaded fallsback to threading) and fast GPU flags
```bash
unset REPRODUCIBLE
export PARALLEL_DIRECTIVES="omp_offloading+omp_threading"
export FCFLAGS="-i4 -Mr8 -O3 -mp=gpu -gpu=mem:managed"
```

> [!Note]
> Currently, NEMOv4 and NEMOv5 take different optimisation paths, so it is
> important to also set:
>
> ```bash
> export NEMOv4=1
> ```
> when applying the transformations to NEMOv4.

<!---
TODO #3445: Mention `ASYNC_PARALLEL`, `ENABLE_INLINING`, `PROFILING`
--->

## Compiling and running the application

Once the environment variables are set, use the `makenemo` command with
the desired NEMO configuration and keys. For example:

```bash
./makenemo -r ORCA2_ICE_PISCES -m arch-linux_spack -n ORCA2_psycloned ...
```

If everything worked you will see that psyclone generated files in the
`<configuration>/BLD/tmp` directory and there is a final binary in the
`<configuration>/EXP00` directory.

You can run this binary using the appropriate command from the configuration
and inserted programming model. For example, for a hybrid
MPI+OMP offloading+OMP threading you can do:

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
to incorrectly transform a file semantics while still creating valid Fortran.

This means that the transformation will succeed and the generated code will
compile, but the results will diverge. This gets more complicated with parallel
programming because certain operations like reductions or atomics are not
always reproducible. Therefore, to understand what causes the results divergence
it is useful to apply the transformations step-by-step while checking if the
`run.stat` values change. Some useful steps are:

1) Build NEMO *without* `psyclonefc` and with conservative optimisation flags
(O2, no vectorisation, no-fma), run it serially and store the generated `run.stat`.

2) Build NEMO with `psyclonefc` and `PSYCLONE_OPTS="-s passthrough.py"`,
this will make PSyclone process all files but without applying any
transformation. Check if the results still match.

3) Build NEMO with `PSYCLONE_OPTS="-s insert_loop_parallelism.py"` but keeping
`PARALLEL_DIRECTIVES=""` empty. This will apply serial transformations but
won't add directives yet. Check if the results still match.

4) Build NEMO with `REPRODUCIBLE=1 PARALLEL_DIRECTIVES="omp_threading" PSYCLONE_OPTS="-s insert_loop_parallelism.py"`
  and see if the results still match.

5) Build NEMO with `REPRODUCIBLE=1 PARALLEL_DIRECTIVES="omp_offloading" PSYCLONE_OPTS="-s insert_loop_parallelism.py"`

<!---
TODO #3445: Introduce/exlain file_by_file.sh scripts
Alongside finding which step is causing the divergence we may want to find
which file/s are causing it. This folder also contains a `do_file_by_file.sh`
script that build NEMO many times, each with only one file being transformed,
and compares the results with the stored `run.stat`
--->


## Tuning the generated implementation

Since this is now a two-step process, there are two locations where you can modify
files that will alter the output result:

1) If you want to modify the original source-code before psyclone is applied (but
still let the modified file go through the psyclone transformation) we recommend
using the built-in `-e` flag to point to a directory with updated source files:

```bash
./makenemo -e <directory> ...
```

2) If you want to modify the transformation itself (or skip it altogether in some
files), you can edit the `insert_loop_parallelism.py` transformation script (and
add files to `FILES_TO_SKIP` to bypass those file).

You can also do both. For example if you want to provide a modified file that
already includes directives, you need to reference it with the `-e <path>`
and list it in the FILES_TO_SKIP (otherwise PSyclone would ignore the code
directives and try to insert its own).
