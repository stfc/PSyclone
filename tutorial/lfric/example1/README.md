# Exercise 1: Create and run simple kernels

Accompanying materials:
* [Quick intro to LFRic](../background/LFRic_intro.md);
* `Makefile` to build the code.
* `example1_driver.f90`, driver that creates
* 
* `setval_field_w0_kernel_mod.f90`

## Part 1

Use PSyclone kernel stub generator to create argument list and declarations
for two kernels, one that assigns a value to a field on continuous `W0`
function space and another on a discontinuous `W3` function space.
Call these kernels from an algorithm.

The stub for the first kernel with the required metadata can be found in
the file `setval_field_w0_kernel_mod.f90`. Declarations and argument list
code can be created by running PSyclone kernel stub generator:
```bash
genkernelstub setval_field_w0_kernel_mod.f90
```

The `W3` kernel can be created using this stub as a template and changing
the function space and code unit names accordingly (be careful about the
correct access modes for continuous and discontinuous function spaces).

## Part 2

Create a kernel `add_fields_W0_kernel_mod.f90` that adds two fields on
`W0` space and stores the result in another field on the same space. Use
the existing `setval_field_w0_kernel_mod.f90`
* To initialise each field to `0`;
* As a template for the new kernel that adds fields.

Call these kernels from an algorithm.

## Part 3

Use the kernels `setval_fields_W0_kernel_mod.f90` and
`add_fields_W0_kernel_mod.f90` from part 2 as templates that can set and
add field values for fields on any function space.

Call these kernels from an algorithm.
