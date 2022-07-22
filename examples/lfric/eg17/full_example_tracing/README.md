# Standalone LFRic Kernel Extraction Example

This directory contains a runnable example of array tracing
with LFRic. The main code is explained in more details in the
directory ``../full_example``.

It applies the PSyData tracing transformation to the
user-supplied kernels (due to #637 we cannot instrument builtins
at the moment).

## Compilation

A simple makefile is provided to compile the example. It needs:
- the infrastructure library ``liblfric.a`` provided in
  ``<PSYCLONEHOME>/src/psyclone/tests/test_files/dynamo0p3/infrastructure``,
  and
- the LFRic PSyData wrapper library ``lib_tracing`` from
  ``<PSYCLONEHOME>/lib/tracing/lfric``

The infrastructure and PSyData wrapper libraries will be compiled
if they are not available.

The following environment variables can be set to define the compiler
you want to use:
```shell
export F90=gfortran
export F90FLAGS="-Wall -g -fcheck=bound"
make
```

The location of the PSyData wrapper library can be set with
the environment variable ``TRACING_DIR``, and the location
of the LFRic infrastructure libraries is set using
``LFRIC_DIR`` - both defaulting to the versions included in
PSyclone.

## Running

The binary can be executed using ``tracing`` without additional parameters:
```shell
./tracing
 Mesh has           5 layers.
 Tracing write:field1       94364333967680  module main_psy region invoke_initialise_fields:setval_c:r0
 Tracing write:field2       94364333972000  module main_psy region invoke_initialise_fields:setval_c:r1
 Tracing read :field1       94364333967680  module main_psy region invoke_testkern_w0:testkern_w0_code:r2
 Tracing read :field2       94364333972000  module main_psy region invoke_testkern_w0:testkern_w0_code:r2
 Tracing read :map_w0       94364333963356  module main_psy region invoke_testkern_w0:testkern_w0_code:r2
 Tracing write:field1       94364333967680  module main_psy region invoke_testkern_w0:testkern_w0_code:r2
20220722120553.662+1000:INFO : Min/max minmax of field1 =   0.10000000E+01  0.80000000E+01
```

The output shows the traces of all reads and writes to arrays, including the
address of the first array element. The address can be used to identify an
array, since its name might be changed if it is passed as a parameter to a
subroutine. The output above shows for example that the array
`field1` is written in in the module `main_psy`, in
`invoke_initialise_fields:setval_c:r0`, and then read and written again in
`invoke_testkern_w0:testkern_w0_code:r2`.
This can be used during debugging: if invalid values are found in an array, this
output can be used to identify where this variable was written.