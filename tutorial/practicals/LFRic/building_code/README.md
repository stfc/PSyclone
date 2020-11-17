# Tutorials on building LFRic code

The tutorials in this directory give an overview of building LFRic code,
with PSyclone as an integral part of this process. There are four
subdirectories, one with the
[introduction to LFRic](background) (including the overview of LFRic
code structure and how PSyclone is used in LFRic) and three hands-on
tutorials described below.

The tutorials build on each other so it is important to work through
them in order. The recommended background sections are highlighted and
linked as needed in each tutorial.

### [Simple kernels tutorial](1_simple_kernels)

This tutorial shows how to create and use simple LFRic kernels to
perform mathematical operations on LFRic field data. It starts with
different kernels for different LFRic finite-element [function spaces](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#supported-function-spaces)
and uses them as a template to write general-purpose kernels that can
operate on any function space.

The mathematical operations in this tutorial are quite simple (initialisation
of a field to a prescribed value and adding fields) as the primary purpose of
the tutorial is to create functional kernels with the required metadata,
subroutine argument list and loops that update an LFRic field.

### [Built-ins tutorial](2_built_ins)

This tutorial shows how to use the [PSyclone LFRic (Dynamo 0.3 API) built-ins](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#built-ins)
instead of kernels for simple linear algebra operations on fields.

### [Time-evolution tutorial](3_time_evolution)

This tutorial illustrates timestepping in LFRic through propagation of a
field on a planar mesh (more elaborate example of LFRic code). This requires
information to set the model initialisation and run parameters that needs to
be used throughout the model, including the kernels. The information in this
tutorial is read from an input namelist file and accessed by kernels and
algorithms that need to be completed to propagate the field.

## Hands-on tutorial structure

In order to illustrate the [*separation of concerns*](
background/LFRic_intro.md#separation-of-concerns) in LFRic, each tutorial
has the main program (driver) that calls one or more algorithms. The
algorithms, in turn, contain `invoke` calls to one or more [kernels](
background/LFRic_kernel.md) and/or built-ins.

The driver in each tutorial provides the framework to build an executable
program through the set-up of the LFRic infrastructure objects and calls
to algorithms (see [*Driver* section](??LINK) for general and tutorials
documentation for specific details). They do not need to be modified.

The algorithm code in each tutorial needs to be completed with the
appropriate `invoke` calls to kernels (the [first](1_simple_kernels)
tutorial), built-ins (the [second](2_built_ins) tutorial) or both (the
[third](3_time_evolution) tutorial). The kernel code in the
[first](1_simple_kernels) and the [third](3_time_evolution) tutorial
needs to either be completed or created (the [second](2_built_ins)
tutorial does not require kernels).

Each tutorial directory contains `Makefile`s to build and run the
code once that the kernel and algorithm code is completed. There are
tree targets to the `make` process:

* `make test` calls PSyclone with the prescribed command-line options
  to generate the processed [Algorithm](??LINK) and [PSy](??LINK) layer;
* `make` or `make build` builds the tutorial executable from the
  [LFRic pared-down infrastructure](#lfric-code-support) and the
  PSyclone-generated source code above;
* `make clean` removes the generated source and the compiled objects
  and libraries.

---
**NOTE**
It is advisable to run `make test` whilst completing the kernel and
and algorithm code to ensure that the code is correct. PSyclone checks
that the source is syntactically correct and that it abides by the
[LFRIC API](https://psyclone.readthedocs.io/en/stable/dynamo0p3.html)
rules.

---

The completed solutions are provided in the `solutions`
subdirectories of each tutorial, together with `Makefile`s to build
and run the code.

Each of these tutorials also has a `solutions` subdirectory with the
completed code that can be built and run as a reference.

## LFRic code support

All the tutorials here use a pared-down version of the LFRic infrastructure
stored in the [`src/psyclone/tests/test_files/dynamo0p3/infrastructure`](
../../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure/README.md)
directory of the PSyclone repository. For more information on LFRic code
structure and functionality see the [*LFRic code structure* section](
background/LFRic_structure.md).

### Distributed memory

The pared-down LFRic infrastructure used in these tutorials does not
have the support for distributed memory (done via the [YAXT library](
https://www.dkrz.de/redmine/projects/yaxt) in LFRic). Also, none of
the [PSyclone transformations](
https://psyclone.readthedocs.io/en/stable/transformations.html) are
applied here so the code is generated and run in serial.
