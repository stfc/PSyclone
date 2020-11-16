# Tutorial on building LFRic code

This tutorial gives an overview of building LFRic code, with PSyclone
as an integral part of this process. It is structured as four
subdirectories, one with the
[introduction to LFRic](background) (including the overview of LFRic
code structure and how PSyclone is used in LFRic) and three examples.

## Examples

1. [**Simple kernels example**](1_simple_kernels) shows how to build
   and use simple LFRic kernels;

2. [**Built-ins example**](2_built_ins) shows the use of PSyclone built-ins;

3. [**Time-evolution example**](3_time_evolution) shows propagation of a
   field on a planar mesh (more elaborate example of LFRic code).

In order to mimic the LFRic code structure, an example algorithm and
main program (driver) are provided in all Examples. In LFRic,
[kernels](background/LFRic_kernel.md) are *invoke*d from
[algorithms](background/LFRic_algorithm.md), which are in turn called
from the top-level driver code (see the [*Separation of concerns* section](
background/LFRic_intro.md#separation-of-concerns) in the [LFRic intro](
background/LFRic_intro.md)).

The kernel and algorithm code in the top-level example directories
is incomplete. The completed solutions are provided in the `solutions`
subdirectories of each example, together with `Makefile`s to build
and run the code.

Each of these examples has a `solutions` subdirectory with the
completed code that can be built and run. It is also possible to build
the executables of all solutions from this directory by running `make`.

## LFRic code support

All the examples here use a pared-down version of the LFRic infrastructure
stored in the `src/psyclone/tests/test_files/dynamo0p3/infrastructure`
directory of the PSyclone repository. For more information on LFRic code
structure and functionality see [*LFRic repository structure* section](
background/LFRic_intro.md#lfric-repository-structure) of the
[introduction to LFRic](background/LFRic_intro.md).

### Distributed memory

The pared-down LFRic infrastructure used in these examples does not
have the support for distributed memory (done via the [YAXT library](
https://www.dkrz.de/redmine/projects/yaxt) in LFRic). Also, none of
the [PSyclone transformations](
https://psyclone.readthedocs.io/en/stable/transformations.html) are
applied here so the code is generated and run in serial.
