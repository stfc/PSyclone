# Simple PSyclone Application

This directory contains a very trivial example code that is based on
the LFRic infrastructure and PSyclone. It uses the simplified infrastructure
files used in PSyclone testing, and as such can be compiled and run without
the need to install LFRic.

The script creates two fields on a 3x3 mesh with 5 layers. One is initialised
with 0, the other with 1. Then the kernel `testkern_w3_kernel_type` is called,
which adds the second field to the first one (there is a builtin called
`inc_X_plus_Y` which would do this without the need for implementing a kernel
for this).

To create the required PSy-layer, use the following command:

    psyclone -nodm -l output -opsy main_alg_psy.f90 -oalg main_alg.f90 main.x90

The same command can be triggered by `make transform`. This will create
two new output files, `main_alg.f90`, the rewritten algorithm layer `main_alg.x90`,
and `main_alg_psy.f90`.

You can even compile and execute the script: `make compile` will create a binary for
you (including compilation of the required LFRic infrastructure if required).
Executing the binary with `./example` will print:

     Mesh has           5 layers.
    20230829165343.079+1000:INFO : Min/max minmax of field1 =   0.10000000E+01  0.10000000E+01
The minimum and maximum of the field is as expected now 1.

1. Look at the files. You don't need to try to understand the details, since PSyclone
   is creating quite a bit of code, but identify how the main program, the algorithm
   layer, calls the PSylayer, and how the PSy-layer calls the kernel in a loop. Focus
   on the first two invokes:

       call field2%initialise( vector_space = vector_space_ptr, name="field2" )
       call invoke( name = 'Initialise fields',        &
                    setval_c( field1,     0.0_r_def ), &
                    setval_c( field2,     1.0_r_def )  &
                    )
       call invoke( name = 'testkern_w3', testkern_w3_kernel_type(field1, field2) )

   Can you find the builtins in the PSy-layer file `main_alg_psy.f90`? And
   the call to the testkern_w3 kernel?

2. Apply the `omp.py` optimisation script, which will do some loop fusion, and
   also applies OpenMP parallelisation. You have to specify the full name of
   the script including path with the `-s` command line options, e.g.
   `-s ./omp.py` (or use `make omp`). Compare the PSy-layer files with
   the previously created files.

# Solution
1. The file `main_alg.f90` contains two calls to a PSy layer:
       CALL invoke_initialise_fields(field1, field2)
       CALL invoke_testkern_w3(field1, field2)

   In turn the `main_alg_psy.f90` files contains these two subroutines. The first
   one contains the implementation of the builtins, i.e. the code is inlined:

       ! Call our kernels
       !
       DO df=loop0_start,loop0_stop
         field1_proxy%data(df) = 0.0_r_def
       END DO
       DO df=loop1_start,loop1_stop
         field2_proxy%data(df) = 1.0_r_def
       END DO
   The second subroutine contains the call of the test kernel:

       DO cell=loop0_start,loop0_stop
         !
         CALL testkern_w3_code(nlayers, field1_proxy%data, field2_proxy%data, ndf_w3, undf_w3, map_w3(:,cell))
       END DO
   Note that PSyclone will automatically provide additional required parameters to
   the kernel.




This directory contains a stand-alone LFRic code example. PSyclone
comes with a minimum set of (slightly modified) infrastructure files
required to create a field, and call a simple kernel on this field.
The following steps are required for this (using simplified code examples):

1) A global mesh is created using an existing unit-testing functionality:
    ```fortran
    global_mesh = global_mesh_type()
    ```
   
2) A 1x1 planar partition for one process is created:
    ```fortran
    partitioner_ptr => partitioner_planar
    partition = partition_type(global_mesh_ptr, &
                               partitioner_ptr, &
                               1, 1,  &    ! # procs in x and y direction
                               0,     &    ! max stencil size
                               0,     &    ! local rank
                               1)          ! number of proceses
    ```

3) Create a uniform extrusion:
    ```fortran
    extrusion = uniform_extrusion_type(0.0d0, 100.0d0, 5)
    extrusion_ptr => extrusion
    ```

4) Create a mesh using the global mesh, partition and extrusion:
    ```fortran
    mesh = mesh_type(global_mesh_ptr, partition, extrusion_ptr)
    ```

5) Create a function/vector space:
    ```fortran
    vector_space = function_space_type( mesh,          &
                                        element_order, &
                                        lfric_fs,      &
                                        ndata_sz)
    ```

6) Create two fields for the test kernel:
    ```fortran
    call field1%initialise( vector_space = vector_space_ptr, name="field1" )
    call field2%initialise( vector_space = vector_space_ptr, name="field2" )
    ```

7) Call some built-ins:
    ```fortran
    call invoke( name = 'Initialise fields',        &
                 setval_c( field1,     0.0_r_def ), &
                 setval_c( field2,     1.0_r_def )  &
                 )
    ```

8) Call a user-defined kernel:
    ```fortran
    call invoke( name = 'testkern_w0', testkern_w0_type(field1, field2) )
    ```

## Compilation

A simple makefile is provided to compile the example. It needs the
infrastructure library ``liblfric.a`` provided in
``<PSYCLONEHOME>/src/psyclone/tests/test_files/dynamo0p3/infrastructure``.
If this library is not available, it will be automatically compiled.

The following environment variables can be set to define the compiler
you want to use:
```shell
export F90=gfortran
export F90FLAGS="-Wall -g -fcheck=bound"
make compile
```

## Running

The binary ``example`` can be executed without any parameters:
```shell
./example
 Mesh has           5 layers.
20210318121302.432+1100:INFO : Min/max minmax of field1 =   0.10000000E+01  0.80000000E+01
```
