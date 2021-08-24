# Standalone LFRic Example

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
