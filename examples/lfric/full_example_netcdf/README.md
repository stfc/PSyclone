# Standalone LFRic Example with NetCDF

This directory contains a stand-alone LFRic code example.
It is just a proof-of-concept to evaluate the feasibility and
required effort for a stand-alone LFRic example. This
examples adds support for reading in the global mesh from a
NetCDF file.

This example contains the minimum infrastructure required to create
a field, and call a simple kernel on this field. The following 
steps are required for this (using simplified code examples):

1) A global mesh is created from a NetCDF file:
    ```fortran
    global_mesh = global_mesh_type("mesh_BiP128x16-400x400.nc", "dynamics")
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

Significant refactoring was required in order to achieve that. This
proof-of-concept implementation did not try a proper refactoring of
code to reduce dependencies; in many case unnecessary dependencies
were just removed from the source. Some examples:
- XIOS was just removed (i.e. code that used it simply removed).
- YAXT/MPI was completely removed (code that used it simply removed).
- some dependencies were removed (for example the global_mesh module
  also provides functionality for managing collections/maps of 
  global meshes. This needlessly increase the number of files required).
- Other support was just removed (e.g. NetCDF/UGRID, ...).

A better designed stand-alone mini-example should
- Refactor existing LFRic code to reduce dependencies
  (e.g. instead of an object adding itself to a global data structure,
  which makes the global handling a dependency for the object,
  the global data structure can create a new object. This way
  it is possible to create the object without dependencies).
- For XIOS/YAXT NULL wrapper should be produced - i.e. they allow
  compilation and linking, but offer no functionality. This removes
  the need to remove significant amount of XIOS/YAXT code.
- Other classes might be split into two classes/files. For example
  there is no need that the unit-test functionality of a global
  mesh is in the same file as the code that reads a mesh, allowing
  the creation of a simple mesh by unit-testing code without
  dependency to UGRID.

Still, it is expected that there will be a porting effort involved
when updating the stand-alone LFRic example from LFRic, but the goal
is to minimise it, and automate it if possible.

## Compilation
A simple makefile is provided to compile the example. It needs 
a full installation of NetCDF, since it is using ``nf-config`` to
query the required compiler and linker flags, and the
infrastructure library ``liblfric_netcdf.a`` provided in
``<PSYCLONEHOME>/src/psyclone/tests/test_files/dynamo0p3/infrastructure``.
If the latter is not available, it will be automatically compiled.

The following environment variables can be set to define the compiler
you want to use:
```shell
export F90=gfortran
export F90FLAGS="-Wall -g -fcheck=bound $(nf-config --fflags)"

make
```