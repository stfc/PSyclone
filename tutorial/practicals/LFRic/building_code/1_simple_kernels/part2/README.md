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

We will use the following modules as templates to create and call kernels
in this tutorial:

* [`setval_field_w0_kernel_mod.f90`](setval_field_w0_kernel_mod.f90) - a
  stub of an LFRic kernel to be completed and used as a template;

* [`simple_kernels_alg_mod.x90`](simple_kernels_alg_mod.x90) - an example
  of an LFRic algorithm that sets up fields and operates on them via the
  `invoke` calls to the kernels created in parts of this example (`invoke`
  calls need to be completed).

Specific information on how to complete and use the above kernel and
algorithm code is given in the specific tasks in [Part 1](part1/README.md)
and [Part 2](part2/README.md) of this tutorial.

We will also use the following utilities to build and run the code that
do not need to be modified:

* [`simple_kernels_driver.f90`](simple_kernels_driver.f90) - an example
  of an LFRic-like main program that creates the required LFRic objects and
  calls the algorithm code in this example;
* [`Makefile`](Makefile) - a script that builds the executable program
 `simple_kernels`. As outlined in the [top-level introduction](../README.md),
  this script can 
  `make` to build the completed
  example and `./simple_kernels` to run it;


* `make test` calls PSyclone with the prescribed command-line options
  to generate the processed [Algorithm](
  background/LFRic_structure.md#algorithm-layer) and [PSy](
  background/LFRic_structure.md#psy-layer) layer;
* `make` or `make build` builds the tutorial executable from the
  [LFRic pared-down infrastructure](#lfric-code-support) and the
  PSyclone-generated source code above;
* `make clean` removes the generated source and the compiled objects
  and libraries.




Both parts of this tutorial use the following supporting materials:

* [`Makefile`] that builds the executable program `simple_kernels` and does
  (does not need to be modified). Run `make` to build the completed
  example and `./simple_kernels` to run it;
* [`simple_kernels_driver.f90`](simple_kernels_driver.f90) - an example
  of LFRic-like main program that creates the required LFRic objects and
  calls the algorithm code in this example.




Every part of this tutorial uses 
Finally, we will use the supplied [`Makefile`](Makefile) and PSyclone and the LFRic infrastructure library
to build the code and ?????????







* In [Part 1](#part-1) we will 
* In [Part 2](#part-2) we will create and call general-purpose kernels
  that can operate on any function space.






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





## Part 1

In this part of the tutorial we will use the PSyclone
[kernel stub generator](
https://psyclone.readthedocs.io/en/stable/stub_gen.html) to
create the argument list and declarations for two kernels, one that assigns
a value to a field on a continuous `W0` function space and another on a
discontinuous `W3` function space. For this we will use the supplied
kernel stub file [`setval_field_w0_kernel_mod.f90`](
setval_field_w0_kernel_mod.f90) and the provided documentation on the
[LFRic kernel structure](LFRic_kernel_structure.md).

We will then modify the supplied algorithm
[`simple_kernels_alg_mod.x90`](simple_kernels_alg_mod.x90) to call
these kernels.




### Task 1: Complete the `setval_field_w0_kernel_mod.f90` kernel

Open the supplied [`setval_field_w0_kernel_mod.f90`](
setval_field_w0_kernel_mod.f90) code in an editor. 

The stub for the first kernel with the required metadata can be found
in the file [`setval_field_w0_kernel_mod.f90`](
setval_field_w0_kernel_mod.f90). The code for the declarations and argument list 
can be created by running the PSyclone kernel stub generator:

```bash
genkernelstub setval_field_w0_kernel_mod.f90
```

You now need to complete the implementation of the kernel so that it sets elements of the supplied field to the specified scalar value. This can be done by

The kernel code can be completed by referring to the [*Loops* section](
../background/LFRic_kernel.md#loops) of the LFRic kernel documentation in
this tutorial.

The `W3` kernel can be created using the completed
`setval_field_w0_kernel_mod.f90` as a template and changing the function
space and code unit names accordingly. Information about the correct
access modes for fields on continuous and discontinuous function spaces
in PSyclone LFRic (Dynamo 0.3) API can be found [here.](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#valid-access-modes)

The tutorial is further divided into three parts:

* Part 1 introduces  




For the first task we will use the supplied [`setval_field_w0_kernel_mod.f90`] with the required metadata, subroutine
  argument list and loops 




 using the
  supplied `setval_field_w0_kernel_mod.f90` kernel stub as a template;
* Complete the supplied `simple_kernels_alg_mod.x90` algorithm to call the
  created kernels from.




#### [Solutions](solutions/part1)

To check for the correct results, navigate to the `solutions/part1`
directory and run `make` to build the `simple_kernels_part1` executable.

### Part 2





Create a kernel `add_fields_w0_kernel_mod.f90` that adds two fields on
`W0` space and stores the result in another field on the same space. Use
the existing `setval_field_w0_kernel_mod.f90`
* To initialise the resulting field to `0` and the fields being added
  to a constant value each;
* As a template for the new kernel that adds fields.

As in Part 1, use the kernel stub generator to create argument list
and declarations for the new kernel. Modify the supplied algorithm
`simple_kernels_alg_mod.x90` to call these kernels from.

*Tips:*
* `W3` function space and the related fields and kernels are no
  longer required;
* Group kernel calls into a single `invoke`;
* Explore naming of `invoke` call.

#### [Solutions](solutions/part2)

To check for the correct results, navigate to the `solutions/part2`
directory and run `make` to build the `simple_kernels_part2` executable.

### Part 3

Use the kernels `setval_fields_w0_kernel_mod.f90` and
`add_fields_w0_kernel_mod.f90` from Part 2 as templates that can set and
add field values for fields on any function space. Modify the supplied
algorithm `simple_kernels_alg_mod.x90` to call these kernels from. Explore
other continuous and discontinuous function spaces in the algorithm (see
the summary table listing the function space continuity in section
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
directory and run `make` to build the `simple_kernels_part3` executable.

