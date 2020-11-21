# Tutorial 1: Create and run simple kernels

In this tutorial we will learn how to

* Create simple functional kernels to update LFRic fields;

* Call the created kernels from an algorithm.

There are two parts of this tutorial and each should be done in turn.

* In [Part 1](part1/README.md) we will create and call kernels that
  update a field on a specific function space.

* In [Part 2](part2/README.md) we will create and call generic kernels
  that update a field on any function space.

*Note:* For more information on the supported LFRic function spaces in
PSyclone please refer to this PSyclone [user guide section](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#supported-function-spaces).

Each kernel in this tutorial performs one simple mathematical operation
in order to learn how to work with the basic building blocks of an
[LFRic kernel](LFRic_kernel_structure.md):

* [Kernel metadata](LFRic_kernel_structure.md#kernel-metadata) and

* [Kernel subroutine](LFRic_kernel_structure.md#kernel-subroutine) including
  - [Argument list and declarations](
    LFRic_kernel_structure.md#argument-list-and-declarations) and
  - [Loops](LFRic_kernel_structure.md#loops) to update LFRic field objects.

## Supporting source and scripts

In [Part 1](part1) we will use the following modules as templates to create
and call the kernels:

* [`setval_field_w0_kernel_mod.f90`](part1/setval_field_w0_kernel_mod.f90),
  a stub of an LFRic kernel that updates a field on the `W0` function space.
  The kernels needs to be completed and used as a template to create
  a similar kernel on the `W3` function space;

* [`simple_kernels_alg_mod.x90`](part1/simple_kernels_alg_mod.x90), an
  example of an LFRic [algorithm](
  ../background/LFRic_structure.md#algorithm-layer) that sets up fields
  and operates on them via the `invoke` calls to the kernels created in
  each part of this tutorial (the `invoke` calls need to be completed).

In [Part 2](part2) will use the following modules as templates to create and
call the kernels:

* [`setval_field_any_kernel_mod.f90`](part2/setval_field_any_kernel_mod.f90),
  a stub of an LFRic kernel that updates a field on a generic function space.
  The kernels needs to be completed and used as a template to create
  a kernel that adds two fields on a generic function space;

* [`simple_kernels_alg_mod.x90`](part2/simple_kernels_alg_mod.x90), to set
  up fields and operate on them via `invoke` calls as above.

Specific information on how to complete and use the above kernels and
algorithm code is given in the specific tasks in [Part 1](part1/README.md)
and [Part 2](part2/README.md) of this tutorial. The
[*Algorithm structure*](#algorithm-structure) section below outlines the
role of the algorithm layer in this tutorial.

We will also use the following utilities to build and run the code (provided
for each part of this tutorial):

* `simple_kernels_driver.f90`, an example of an LFRic-like
  [main program (driver)](
  ../background/LFRic_structure.md#driver-layer) that creates the
  required LFRic objects and calls the algorithm code in this example;

* `Makefile` script that builds the executable program for each part of
  this tutorial.

These utilities do not need to be modified. The
[*Driver structure*](#driver-structure) section below outlines the role
of the driver layer in this tutorial.

### Driver structure

As outlined in the [overview of the LFRic driver layer](
../background/LFRic_structure.md#driver-layer), the
`simple_kernels_driver.f90` sets up LFRic object stack required to
define field objects. The related Fortran calls are explained below.

**Note:** The order of creating objects, from the global 2D mesh to the
local 3D mesh is the same as in the full LFRic model, however the specific
calls are likely to be different due to the [reduced version](
../README.md#lfric-code-support) of the LFRic infrastructure used
in this example.

1) Create a global 2D mesh object, allocate the data from the unit-test
   planar mesh constructor and return a pointer to the global mesh:
   ```fortran
   global_mesh = global_mesh_type()
   global_mesh_ptr => global_mesh
   ```

2) Create a 1x1 (`xproc`, `yproc`) planar partition object
   ```fortran
   partitioner_ptr => partitioner_planar
   partition = partition_type( global_mesh_ptr,   &
                               partitioner_ptr,   &
                               xproc,             &
                               yproc,             &
                               max_stencil_depth, &
                               local_rank,        &
                               total_ranks )
    ```
   for one process (`total_ranks`) as we are running in serial. Note that
   we need to point to the global mesh (`global_mesh_ptr`) and the
   implementation of the `partitioner` class for a planar mesh
   (`partitioner_ptr => partitioner_planar`).

3) Create a uniform vertical extrusion object
   ```fortran
   extrusion = uniform_extrusion_type( 0.0_r_def, domain_top, number_of_layers )
   extrusion_ptr => extrusion
   ```
   with the specified atmosphere height (`domain_top`) and number of layers
   for the full 3D mesh (the ground level is set at the scalar literal
   `0.0_r_def` where `r_def` is an LFRc-defined Fortran `kind` for the
   `real`-valued literals). Return a pointer to it (`extrusion_ptr`).

4) Create a full 3D partitioned mesh object mesh using the global mesh,
   partition and extrusion information:
   ```fortran
   mesh = mesh_type( global_mesh_ptr, partition, extrusion_ptr )
   ```

### Algorithm structure

The [algorithm layer](
../background/LFRic_structure.md#algorithm-layer) in this tutorial
creates function space and field objects with the input mesh and
finite-element order information from the driver layer. The related
Fortran calls are explained below on the example of the
[`simple_kernels_alg_mod.x90` in Part 1](
part1/simple_kernels_alg_mod.x90) of this tutorial for one of
the "(function space, field) pairs" (as explained [above](#driver-structure),
the order of creating objects is the same as in the full LFRic model
however the specific calls may be different).

1) Create a `W0` function space object with single-valued field data
   points (`ndata_sz`) and initialise a pointer to it
   ```fortran
   fs_w0 = function_space_type( mesh, element_order, W0, ndata_sz )
   fs_w0_ptr => fs_w0
   ```

2) Create a field on the `W0` function space
   ```fortran
   call field_w0%initialise( vector_space = fs_w0_ptr, name = "field_w0" )
   ```
   by providing a mandatory pointer to the specified function space object
   (`fs_w0_ptr`) and an optional string for the field name (`"field_w0"`).

3) Operate on field objects by `invoke` calls to the specified kernels
   (to be completed in each part of this tutorial). The kernel objects
   must be made accessible to an algorithm through `use` statements, e.g.
   ```fortran
     use setval_field_w0_kernel_mod, only : setval_field_w0_kernel_type
   ```

The algorithm also calls one of the LFRic field class procedures,
`log_minmax`, to check the minimum and maximum values of the fields after
calling the kernels that update them.

#### `use` statements and encapsulation

As can be seen from this [algorithm example](
part1/simple_kernels_alg_mod.x90), the `use` statements in algorithms
mainly serve to access the LFRic infrastructure objects (e.g. mesh,
function space, field).

Unlike for [kernels](LFRic_kernel_structure.md), there is no inheritance
from an abstract base type. Similar to kernels, however, the general
encapsulation principle of keeping data and procedures `private`
unless they are called by other objects still holds.
