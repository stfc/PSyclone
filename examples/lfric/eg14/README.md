# Stand-alone LFRic Kernel OpenACC Example

This directory contains a runnable example of an LFRic mini-app that
uses OpenACC. The framework for this stand-alone example is explained in
more details in the directory
``<PSYCLONEHOME>/examples/lfric/eg17/full_example``.

The script ``acc_parallel_dm.py`` applies various OpenACC transformations
to all kernels. See the PSyclone User Guide for [details](https://psyclone.readthedocs.io/en/stable/examples.html#example-14-openacc).

## Compilation

Note that due to #1724 compilation will currently fail. A temporary workaround
is to edit the generated Alg file (``main_alg.f90``) and remove the
``use testkern_w0_kernel_mod, only: ...`` line.

A simple Makefile is provided to compile the example. It needs:
- the infrastructure library ``liblfric.a`` provided in
  ``<PSYCLONEHOME>/src/psyclone/tests/test_files/dynamo0p3/infrastructure``

The infrastructure library will be compiled if it is not available.

The following environment variables can be set to define the compiler
you want to use:
```shell
export F90=gfortran
export F90FLAGS="-Wall -g -fopenacc"
make
```

## Running

The binary can be executed using ``example_openacc`` without additional parameters:
```shell
./example_openacc
 Mesh has           5 layers.
20210318131720.135+1100:INFO : Min/max minmax of field1 =   0.10000000E+01  0.80000000E+01
```

If you are using NVIDIA hardware, you can specify NV_ACC_NOTIFY=3
when running in order to see GPU-related activity.
