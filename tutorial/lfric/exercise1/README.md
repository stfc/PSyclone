# Exercise 1: Create and run simple kernels

Use PSyclone kernel stub generator to create argument list and declarations
for two kernels using the LFRic `k`-first loop structure. One kernel assigns
a value to a field on continouous `W0` space and another on a discontinuous
`W3` space.

The header for the first kernel can be found in the file
`setval_field_W0_kernel_mod.f90` and the code created by running
```
genkernelstub setval_field_W0_kernel_mod.f90
```
The `W3` kernel can be created using this example as a template.
