# Tutorial 1: Create and run simple kernels

In this tutorial we will learn how to

* Create simple functional kernels to update LFRic fields;
* Call the created kernels from an algorithm.

Each kernel in this tutorial performs one simple mathematical operation
in order to learn how to work with the basic building blocks of an
[LFRic kernel](LFRic_kernel_structure.md):

* Kernel metadata (LFRic_kernel_structure.md#kernel-metadata) and
* [Kernel subroutine](LFRic_kernel_structure.md#kernel-subroutine) including
  - [Argument list and declarations](
    LFRic_kernel_structure.md#argument-list-and-declarations) and
  - [Loops](LFRic_kernel_structure.md#loops) to update LFRic field objects.

The tutorial is further subdivided into two parts in order to learn how to

1. Create and call kernels that update a field on the specific
   [LFRic function spaces](
   https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#supported-function-spaces);
2. Create and call general-purpose kernels that update a field on any
   function space.

## Supporting source and scripts

We will use the following modules as templates to create and call the
kernels in this tutorial:

* [`setval_field_w0_kernel_mod.f90`](setval_field_w0_kernel_mod.f90), a
  stub of an LFRic kernel to be completed and used as a template in each
  part of this tutorial;

* `simple_kernels_alg_mod.x90`, an example of an LFRic [algorithm](
  ../background/LFRic_structure.md#algorithm-layer) that sets up fields
  and operates on them via the `invoke` calls to the kernels created in
  each part of this tutorial (the `invoke` calls need to be completed).

Specific information on how to complete and use the above kernel and
algorithm code is given in the specific tasks in [Part 1](part1/README.md)
and [Part 2](part2/README.md) of this tutorial. The
[*Algorithm structure*](#algorithm-structure) section below outlines the
role of the algorithm layer in this tutorial.

We will also use the following utilities to build and run the code:

* [`simple_kernels_driver.f90`](simple_kernels_driver.f90), an example
  of an LFRic-like [main program (driver)](
  ../background/LFRic_structure.md#driver-layer) that creates the required
  LFRic objects and calls the algorithm code in this example;
* `Makefile` script that builds the executable program for each part of
  this tutorial.

These utilities do not need to be modified. The
[*Driver structure*](#driver-structure) section below outlines the role
of the driver layer in this tutorial.

## Driver structure

As outlined in the [overview of the LFRic driver layer](
../background/LFRic_structure.md#driver-layer), the
`simple_kernels_driver.f90` sets up LFRic object stack required to
define field objects. The related Fortran calls are explained below.

*NOTE:* The order of creating objects, from the global 2D mesh to the
local 3D mesh is the same as in the full LFRic model, however the specific
calls are likely to be different due to the [reduced version](
../README.md#lfric-code-support) of the LFRic infrastructure used
in this example.

1) Create a global 2D mesh object and allocate the data from the
   unit-test planar mesh constructor:
   ```fortran
   global_mesh = global_mesh_type()
   global_mesh_ptr => global_mesh
   ```

2) Create a 1x1 (`xproc`, `yproc`) planar partition object:
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
   for one process (`total_ranks`) as we are running in serial.

3) Create a uniform vertical extrusion object
   ```fortran
   extrusion = uniform_extrusion_type( 0.0_r_def, domain_top, number_of_layers )
   extrusion_ptr => extrusion
   ```
   with the specified atmosphere height (`domain_top`) and number of layers
   for the full 3D mesh.

4) Create a full 3D partitioned mesh object mesh using the global mesh,
   partition and extrusion information:
   ```fortran
   mesh = mesh_type( global_mesh_ptr, partition, extrusion_ptr )
   ```

## Algorithm structure

The [algorithm layer](
../background/LFRic_structure.md#algorithm-layer) in this tutorial
creates function space and field objects with the help of input data from
the driver layer. The related Fortran calls are explained below on the
example of the [`simple_kernels_alg_mod.x90` in Part 1](
part1/simple_kernels_alg_mod.x90) of this tutorial for one of
the "(function space, field) pairs" (as explained [above](#driver-structure),
the order of creating objects is the same as in the full LFRic model
however the specific calls may be different).

1) Create the `W0` function space with single-valued field data points
   ```fortran
   fs_w0 = function_space_type( mesh, element_order, W0, ndata_sz )
   fs_w0_ptr => fs_w0
   ```

2) Create a field on this function space:
   ```fortran
   call field_w0%initialise( vector_space = fs_w0_ptr, name = "field_w0" )
   ```

3) Operate on field objects by `invoke` calls to the specified kernels
   (to be completed in each part of this tutorial). The kernel objects
   must be made accessible to an algorithm through `use` statements, e.g.
   ```fortran
     use setval_field_w0_kernel_mod, only : setval_field_w0_kernel_type
   ```

The algorithm also calls one of the LFRic field class procedures,
`log_minmax`, to check the minimum and maximum values of the fields after
calling the kernels that update them.

### `use` statements and encapsulation

As can be seen from this [algorithm example](
part1/simple_kernels_alg_mod.x90), the `use` statements in algorithms
mainly serve to access the LFRic infrastructure objects (e.g. mesh,
function space, field).

Unlike for [kernels](LFRic_kernel_structure.md), there is no inheritance
from an abstract base type. Similar to kernels, however, the general
encapsulation principle of keeping data and procedures `private`
unless they are called by other objects still holds.
