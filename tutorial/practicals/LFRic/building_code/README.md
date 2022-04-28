# Tutorials on building LFRic code

The tutorials in this directory give an overview of building LFRic code,
with PSyclone as an integral part of this process. There are four
subdirectories, one with the
[introduction to LFRic](background/LFRic_intro.md) (including the
overview of the [LFRic code structure](background/LFRic_structure.md)
and how PSyclone is used in LFRic) and three hands-on tutorials briefly
described below.

The tutorials build on each other so it is important to work through
them in order. The recommended background sections are highlighted and
linked as needed in each tutorial.

### [Tutorial 1: Simple kernels](1_simple_kernels)

This tutorial shows how to create and use simple LFRic kernels to
perform mathematical operations on the LFRic field data. It starts with
different kernels for different LFRic finite-element [function spaces](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#supported-function-spaces)
and uses them as a template to write generic kernels that can
operate on any function space.

The mathematical operations in this tutorial are quite simple (initialisation
of a field to a scalar value and adding fields) as the primary purpose of
the tutorial is to create functional kernels with the required metadata,
subroutine argument list and loops that update an LFRic field.

### [Tutorial 2: Built-ins](2_built_ins)

This tutorial shows how to use the [PSyclone LFRic (Dynamo 0.3 API) built-ins](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#built-ins)
instead of kernels for simple linear algebra operations on fields.

### [Tutorial 3: Time evolution](3_time_evolution)

This tutorial illustrates time-stepping in LFRic through propagation of a
field on a planar mesh (more elaborate example of LFRic code). This requires
information to set the model initialisation and run parameters that needs to
be used throughout the model, including the kernels. The information in this
tutorial is read from an input namelist file and accessed by kernels and
algorithms that need to be completed to propagate the field.

### [Tutorial 4: Using PSyData transformation](4_psydata)

This tutorial uses the previous [Time Evolution](#tutorial-3-time-evolution)
example to show the usage of various
PSyData transformations. [Kernel Extraction](
https://psyclone.readthedocs.io/en/latest/psyke.html)
is the first transformation
introduced, followed by [NAN verification](
https://psyclone.readthedocs.io/en/latest/psy_data.html#psydata-nan-test)
and [read-only-variable verification](
https://psyclone.readthedocs.io/en/latest/psy_data.html#read-only-verification).
All these examples are executable, and incorrect code can be uncommented to
trigger the error checks enabled by some PSyData transformations.

## Hands-on tutorial structure

In order to illustrate the [*separation of concerns*](
background/LFRic_intro.md#separation-of-concerns) in LFRic, each tutorial
has the [main program (driver)](background/LFRic_structure.md#driver-layer)
that calls one or more [algorithms](
background/LFRic_structure.md#algorithm-layer). The
algorithms, in turn, contain `invoke` calls to one or more [kernels](
background/LFRic_structure.md#kernel-layer) and/or [PSyclone built-ins](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html#built-ins).

The driver in each tutorial provides the framework to build an executable
program through the set-up of the LFRic infrastructure objects and calls
to algorithms (see the [*Driver* section](
background/LFRic_structure.md#driver-layer) for general information and
the individual tutorials documentation for the specific details). The
provided drivers do not need to be modified.

The algorithm code in each tutorial needs to be completed with the
appropriate `invoke` calls to kernels (the [first](1_simple_kernels)
tutorial), built-ins (the [second](2_built_ins) tutorial) or both (the
[third](3_time_evolution) tutorial). The kernel code in the
[first](1_simple_kernels) and the [third](3_time_evolution) tutorial
needs to either be completed or created (the second tutorial does not
require kernels).

Each tutorial directory contains `Makefile`s to build and run the
code once that the kernel and algorithm code is completed. There are
tree targets to the `make` process:

* `make transform` calls PSyclone with the prescribed command-line options
  to generate the processed [Algorithm](
  background/LFRic_structure.md#algorithm-layer) and [PSy](
  background/LFRic_structure.md#psy-layer) layer;
* `make` or `make compile` builds the tutorial executable from the
  [LFRic pared-down infrastructure](#lfric-code-support) and the
  PSyclone-generated source code above;
* `make clean` removes the generated source and the compiled objects
  and libraries.

---
**NOTE**

It is advisable to run `make transform` whilst completing the kernel and
and algorithm code to ensure that the code is correct. PSyclone checks
that the source is syntactically correct and that it abides by the
PSyclone [LFRIC (Dynamo 0.3) API](
https://psyclone.readthedocs.io/en/stable/dynamo0p3.html) rules.

---

The completed solutions are provided in the `solutions`
subdirectories of each tutorial, together with `Makefile`s to build
and run the code.

### Use of PSyclone to process the algorithm code

All algorithm modules in these tutorials have the `x90` extension. This
is the LFRic custom that indicates to the LFRic build system that the
source code needs to be processed by PSyclone prior to compilation.

As outlined in the [*Algorithm layer* section](
background/LFRic_structure.md#algorithm-layer), the full file name of an
algorithm module can be summarised as `<base_name>_alg_mod.x90`. The
processed algorithm code is saved as `<base_name>_alg_mod.f90` and the
generated PSy-layer code is saved as `<base_name>_alg_mod_psy.f90`.

This process, set as the `make transform` target (see above), is mimicked
in all tutorials. See, for instance, this code from the
[built-ins example `Makefile`](2_built_ins/Makefile)

```make
%_psy.f90: %.x90
    psyclone $(PSYCLONE_CMD) --config $(PSYCLONE_RELPATH)/config/psyclone.cfg \
    -opsy $*_psy.f90 -oalg $*.f90 $<
```

and [here](
https://psyclone.readthedocs.io/en/stable/psyclone_command.html) for more
information on running the `psyclone` script.

As in LFRic, the generated algorithm and PSy-layer source is not kept in
this tutorial repository.

## LFRic code support

All the tutorials here use a pared-down version of the LFRic infrastructure
stored in the [`src/psyclone/tests/test_files/dynamo0p3/infrastructure`](
../../../../src/psyclone/tests/test_files/dynamo0p3/infrastructure/README.md)
directory of the PSyclone repository. For more information on LFRic code
structure and functionality see the [*LFRic code structure* section](
background/LFRic_structure.md).

### Parallel code support

The pared-down LFRic infrastructure used in these tutorials does not
have the support for [distributed memory](../distributed_memory) (done
via the [YAXT library](
https://dkrz-sw.gitlab-pages.dkrz.de/yaxt) in LFRic). Also, none of
the [PSyclone transformations](
https://psyclone.readthedocs.io/en/stable/transformations.html) for
the [shared memory](../single_node) support are applied here so the
code is generated and run in serial.
