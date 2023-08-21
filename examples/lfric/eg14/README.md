# Stand-alone LFRic Kernel OpenACC Example

This directory contains a runnable example of an LFRic mini-app that
uses OpenACC. The framework for this stand-alone example is explained in
more detail in the directory
``<PSYCLONEHOME>/examples/lfric/eg17/full_example``.

The script ``acc_parallel.py`` applies various OpenACC transformations
to all kernels. See the PSyclone User Guide for [details](https://psyclone.readthedocs.io/en/stable/examples.html#example-14-openacc).

## Compilation

A simple Makefile is provided to compile the example. It needs:
- the infrastructure library ``liblfric.a`` provided in
  ``<PSYCLONEHOME>/src/psyclone/tests/test_files/dynamo0p3/infrastructure``

The infrastructure library will be compiled if it is not available.

The ``F90`` and ``F90FLAGS`` environment variables can be set to define the
compiler you want to use. For instance, to use NVIDIA's nvfortran with OpenACC
enabled:

```shell
F90=nvfortran F90FLAGS="-acc -Minfo=all" make compile
```

## Running

The binary can be executed using ``example_openacc`` without additional parameters:
```shell
./example_openacc
 Mesh has           5 layers.
 profile_PSyDataInit called
 PreStart called for module 'main_psy' region 'invoke_initialise_fields:r0'
 PostEnd called for module 'main_psy' region 'invoke_initialise_fields:r0'
 PreStart called for module 'main_psy' region '
 invoke_testkern_w0:testkern_w0_code:r1'
 PostEnd called for module 'main_psy' region '
 invoke_testkern_w0:testkern_w0_code:r1'
 ...
20230807214504.374+0100:INFO : Min/max minmax of field1 =   0.30084014E+00  0.17067212E+01
20230807214504.374+0100:INFO : Min/max minmax of field2 =   0.21098316E-03  0.21098316E-03
```

If you are using NVIDIA hardware, you can specify NV_ACC_NOTIFY=3
when running in order to see GPU-related activity.
