# Transforming Existing Fortran Code

This directory contains a copy of the very first
version of the Game of Life, written in standard Fortran
without any DSL-specific features. It does follow the
GOcean DSL design closely though.

In this exercise you will first add OpenMP directives
around the four compute kernels. These files have been
renamed to have the .x90 extension, but this is done
to keep the Makefile similar between exercises, you can
easily use the standard .f90 extension (but then you have
to create new output files for the processed files, and
these names then need to be used in the Makefile).

## Using OMP Parallel Do

Similarly to exercise 2.8, you can start with just adding
OpenMP Parallel Do statements. The script ``omp_trans.py``
is available, but you need to specify the loop-type inference
rules. Then you can use the loop type to only parallel
latitudinal (or outer) loops.

In case of using PSyclone's transformation only usage,
the command line options are slightly different, more
aligned to typical options used by a compiler:

    psyclone -l output -nodm -s ./omp_trans.py -o count_neighbours_mod.f90 \
              count_neighbours_mod.x90


## Using OMP Parallel and OpenMP Do

Again, using a single OpenMP parallel statement, and include
several OMP Do loops should in general result in better performance,
since the overhead of starting and synchronising threads is
reduced. This was easy to achieve in the GOcean approach of the
Game of Life, since all loop were contained in the same subroutine.
But with this application, the calls to the kernels are actually
in ``time_step_mod.x90``, but the loops are in the individual
subroutines. Using the script ``omp_parallel.py`` as a
starting point, add an OpenMP parallel around all calls in the
time stepping loop, and individiual ``omp do`` statements around
any loop.

When running PSyclone on combine_mod.x90, you have to use:

    psyclone -l output -nodm  -s ./omp_parallel.py -o combine_mod.f90 \
    combine_mod.x90

Otherwise PSyclone will detect the usage of OMP do without an
enclosing parallel:

    psyclone.errors.GenerationError: Generation Error: OMPDoDirective must be inside an OMP parallel region but could not find an ancestor OMPParallelDirective node
