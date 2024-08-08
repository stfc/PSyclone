# Experimental xDSL PSyclone backend

The experimental xDSL backend generates MLIR / LLVM-IR code for compilation using the LLVM toolkit. 

## Dependencies

The xDSL backend has the following dependencies:

1. The [MLIR toolkit](https://mlir.llvm.org/) and [LLVM](https://www.llvm.org/) ([version 18](https://github.com/llvm/llvm-project/releases/tag/llvmorg-18.1.6))
    - Including `clang`, `flang-new` and `mlir-opt` tools - these can be specified when building LLVM from source e.g. 
        ```bash
        user@local:~$ git clone https://github.com/llvm/llvm-project.git
        user@local:~$ cd llvm-project
        user@local:~$ git checkout --track origin/release/18.x
        user@local:~$ mkdir build
        user@local:~$ cd build
        user@local:~$ cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release \
            -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ \
            -DLLVM_ENABLE_PROJECTS="clang;flang;mlir;openmp" ../llvm
        user@local:~$ make install -j 12  # Where 12 is the number of cores on the machine
        ```

2. The xDSL framework is required - it is recommended to use the branch [`maurice/flang-to-std-dialects`](https://github.com/xdslproject/xdsl/tree/maurice/flang-to-std-dialects).

    > See the [xDSL training](https://github.com/xdslproject/training-intro/blob/main/practical/general/local.md) for more information on building and installing the required MLIR and LLVM tool.

3. The PSyclone xDSL dialects and transformations [`psy`](https://github.com/xdslproject/psy) are also required, using the branch [`xdsl-updates`](https://github.com/xdslproject/psy/tree/xdsl-updates).
4. The PSyclone xDSL Fortran support functionality [`ftn`](https://github.com/xdslproject/ftn) 
5. Once you have installed the above dependencies, you will need to set your `PYTHONPATH` environment variable to include the following directories:

    1. The `ftn` repository directory 
    2. The `psy` repository direcotry 
    3. The `xdsl` subdirectory of the xDSL respository directory

    So, for example, we might have a `build` subdirectory in our home directory that contains the `ftn`, `psy` and `xdsl` repositories and we would set the `PYTHONPATH` to:
    ```bash
    user@local:~$ export PYTHONPATH=$PYTHONPATH:$HOME/build/ftn:$HOME/build/psy:$HOME/build/xdsl
    ```

## Usage / pipeline

The xDSL backend is invoked using the `xdsl_backends_transform.py` script in the `examples/xdsl` directory e.g.

```bash
user@local:~$ psyclone -s ./xdsl_backends_transform.py pw_advection/advection_mpi.F90
```

This will generate the `psy_output.mlir` file that we then progressively `lower` until we compile and generate a binary. For this example, we next perform stencil analysis and extraction and can use the example `stencil_lower.py` script:

```bash
user@local:~$ python3 stencil_lower.py
```

Alternatively, the individual steps are as follows:
1. Extract the stencils:     

    ```bash
    user@local:~$ psy-opt -p apply-stencil-analysis,lower-psy-ir,lower-mpi,extract-stencil,rewrite-fir-to-standard psy_output.mlir --output-module-files
    ```

    This pass will create a new `generated` subdirectory containing the extracted stencil file (`module_0.mlir`) and the 'main' driver file (`module_1.mlir`).

2. We now use the xDSL experimental distributed memory dialect and transformations to generate the 'halo exchanges' for the stencils in `module_0.mlir` and lower them to MPI calls:

    ```bash
    user@local:~$ psy-opt -p "dmp-to-mpi{mpi_init=false},convert-stencil-to-ll-mlir,dmp-to-mpi{mpi_init=false},lower-mpi" generated/module_0.mlir -o stencil.mlir
    ```

3. Next, we optimise the MPI stencil and lower the MLIR to LLVM-IR: 

    ```bash
    mlir-opt --pass-pipeline="builtin.module(canonicalize, cse, loop-invariant-code-motion, canonicalize, cse, loop-invariant-code-motion,cse,canonicalize,expand-strided-metadata,fold-memref-alias-ops,lower-affine,finalize-memref-to-llvm,loop-invariant-code-motion,canonicalize,cse,convert-scf-to-openmp,finalize-memref-to-llvm,convert-scf-to-cf,convert-openmp-to-llvm,convert-math-to-llvm,convert-func-to-llvm,reconcile-unrealized-casts,canonicalize,cse)" stencil.mlir | mlir-translate --mlir-to-llvmir -o stencil.bc
    ```

We then compile the stencil and main driver files:

```bash
clang -g -c stencil.bc ; flang-new -fc1 -emit-obj generated/module_1.mlir ; flang-new -o stencil stencil.o generated/module_1.o -I/opt/homebrew/Cellar/mpich/4.2.1/include -L/opt/homebrew/Cellar/mpich/4.2.1/lib -L /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib -lmpi -lpmpi -lomp -lmlir_c_runner_utils
```

> NOTE: The correct MPI include file and library directories will need to be substituted in place of the Hombrew MPICH paths in the example command above - these can be obtained by using the following commands:
>
> For MPICH:
> `mpicc -compile_info`
>
> or
>
> For Open MPI:
> `mpicc --showall`


