# Example 1: Create and run simple kernels

## Exercise

### Part 1

Use PSyclone kernel stub generator to create argument list and
declarations for two kernels, one that assigns a value to a field on
continuous `W0` function space and another on a discontinuous `W3`
function space. Modify the supplied algorithm `simple_kernels_alg_mod.90`
to call these kernels from.

The stub for the first kernel with the required metadata can be found
in the file `setval_field_w0_kernel_mod.f90`. Declarations and argument
list code can be created by running PSyclone kernel stub generator:

```bash
genkernelstub setval_field_w0_kernel_mod.f90
```

The kernel code can be completed by referring to the [*Loops* section](
../background/LFRic_kernel.md#loops) of the LFRic kernel documentation in
this tutorial.

The `W3` kernel can be created using the completed
`setval_field_w0_kernel_mod.f90` as a template and changing the function
space and code unit names accordingly. Information about the correct
access modes for fields on continuous and discontinuous function spaces
in PSyclone LFRic (Dynamo 0.3) API can be found [here.](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#valid-access-modes)

#### [Solutions](solutions/part1)

To check for the correct results, navigate to the `solutions/part1`
directory and run `make` to build the executable.

### Part 2

Create a kernel `add_fields_w0_kernel_mod.f90` that adds two fields on
`W0` space and stores the result in another field on the same space. Use
the existing `setval_field_w0_kernel_mod.f90`
* To initialise the resulting field to `0` and the fields being added
  to a constant value each;
* As a template for the new kernel that adds fields.

As in Part 1, use the kernel stub generator to create argument list
and declarations for the new kernel. Modify the supplied algorithm
`simple_kernels_alg_mod.90` to call these kernels from.

*Tips:*
* `W3` function space and the related fields and kernels are no
  longer required;
* Group kernel calls into a single `invoke`;
* Explore naming of `invoke` call.

#### [Solutions](solutions/part2)

To check for the correct results, navigate to the `solutions/part2`
directory and run `make` to build the executable.

### Part 3

Use the kernels `setval_fields_w0_kernel_mod.f90` and
`add_fields_w0_kernel_mod.f90` from Part 2 as templates that can set and
add field values for fields on any function space. Modify the supplied
algorithm `simple_kernels_alg_mod.90` to call these kernels from. Explore other
continuous and discontinuous function spaces in the algorithm (see the
summary table listing the function space continuity in section
[*Supported Function Spaces*](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#supported-function-spaces)
of the PSyclone LFRic (Dynamo 0.3) API documentation). The spaces used in
the solution here are `W2` and `Wtheta`.

*Tips:*

* Modify the kernel metadata (perhaps also rename the kernels);
* Metadata for `ANY_SPACE` and `ANY_DISCONTINUOUS_SPACE` spaces are
  in `argument_mod` in LFRic infrastructure;
* Try out the kernel stub generator.

#### [Solutions](solutions/part3)

To check for the correct results, navigate to the `solutions/part3`
directory and run `make` to build the executable.

## Supporting materials

The following modules need to be modified:

* `simple_kernels_alg_mod.x90` - an example of LFRic algorithm that sets up
  fields and operates on them via `invoke` calls to the kernels created
  in parts 1-3 of this example (`invoke` calls need to be completed);
* `setval_field_w0_kernel_mod.f90` - a stub of an LFRic kernel to be
  completed as described in Part 1 and used as a template.

Utilities to build and run the code are (do not need to be modified):

* `Makefile` - builds the executable program `simple_kernels` (does not
  need to be modified). Run `make` to build the completed example and
  `./simple_kernels` to run it;
* `simple_kernels_driver.f90` - an example of LFRic-like main program that
  creates the required LFRic objects and calls the algorithm code in
  this example (does not need to be modified).

### Driver and algorithm structure

`simple_kernels_driver.f90` follows the order of setting up LFRic object
stack outlined in [this full LFRic example](
../../../../../examples/lfric/full_example/README.md) and very close to
the general LFRic principle: **global 2D mesh** -> **partition** ->
**local 3D mesh** -> **function space** -> **field**. In this example
the last two steps of creating function space and field objects are
done in `simple_kernels_alg_mod.x90` with the mesh and finite element order
information as input. In LFRic the driver and algorithm layer can
create fields, with the globally used fields set up in the driver and
passed to algorithms. The set up of mesh and model configuration,
however, is always done in the driver layer. Similarly, operations on
fields using `invoke`s are exclusive to the algorithm layer.

LFRic supports and usually creates objects as collections, such as
**mesh collection**, **function space collection**. This method is
not supported in this tutorial and adapted LFRic infrastructure in
PSyclone.
