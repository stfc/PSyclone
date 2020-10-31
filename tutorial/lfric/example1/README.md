# Example 1: Create and run simple kernels

Accompanying materials:

* `Makefile` to build the code;
* `example1_driver.f90` - an example of LFRic-lite main program that
  creates the required LFRic objects and calls the algorithm code in
  this example (does not need to be modified);
* `example1_alg_mod.x90` - an example of LFRic algorithm that sets up
  fields and operates on them via `invoke` calls to the kernels created
  in parts 1-3 of this Example (`invoke` calls need to be completed);
* `setval_field_w0_kernel_mod.f90` - a stub of an LFRic kernel to be
  completed as described in Part 1 and used as a template.

## Part 1

Use PSyclone kernel stub generator to create argument list and
declarations for two kernels, one that assigns a value to a field on
continuous `W0` function space and another on a discontinuous `W3`
function space. Modify the supplied algorithm `example1_alg_mod.90`
to call these kernels from.

The stub for the first kernel with the required metadata can be found
in the file `setval_field_w0_kernel_mod.f90`. Declarations and argument
list code can be created by running PSyclone kernel stub generator:

```bash
genkernelstub setval_field_w0_kernel_mod.f90
```

The kernel code can be completed by referring to the *Loops* section of
the [LFRic kernel documentation](../background/LFRic_kernel.md) in this
tutorial.

The `W3` kernel can be created using the completed
`setval_field_w0_kernel_mod.f90` as a template and changing the function
space and code unit names accordingly. Information about the correct
access modes for fields on continuous and discontinuous function spaces
in PSyclone LFRic (Dynamo 0.3) API can be found [here.](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#valid-access-modes)

[Link to solutions](solutions/part1)

## Part 2

Create a kernel `add_fields_w0_kernel_mod.f90` that adds two fields on
`W0` space and stores the result in another field on the same space. Use
the existing `setval_field_w0_kernel_mod.f90`
* To initialise the resulting field to `0` and the fields being added
  to a constant value each;
* As a template for the new kernel that adds fields.

As in Part 1, use the kernel stub generator to create argument list
and declarations for the new kernel. Modify the supplied algorithm
`example1_alg_mod.90` to call these kernels from.

*Tips:*
* `W3` function space and the related fields and kernels are no
  longer required;
* Group kernel calls into a single `invoke`;
* Explore naming of `invoke` call.

[Link to solutions](solutions/part2)

## Part 3

Use the kernels `setval_fields_w0_kernel_mod.f90` and
`add_fields_w0_kernel_mod.f90` from Part 2 as templates that can set and
add field values for fields on any function space. Modify the supplied
algorithm `example1_alg_mod.90` to call these kernels from.

*Tips:*
* Modify the kernel metadata (perhaps also rename the kernels);
* Metadata for `ANY_SPACE` and `ANY_DISCONTINUOUS_SPACE` spaces are
  in `argument_mod` in LFRic infrastructure;
* Try out the kernel stub generator;
* Try out other function spaces in the provided algorithm.

[Link to solutions](solutions/part3)
