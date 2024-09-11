# Transforming Existing Fortran Code
This directory contains a copy of the very first
version of the Game of Life, written in standard Fortran
without any DSL-specific features (though it is still
using dl_esm_inf).

In this exercise you will first add OpenMP directives
around the four compute kernels. These files have been
renamed to have the .x90 extension, but this is done
to keep the Makefile similar between exercises, you can
easily use the standard .f90 extension (but then you have
to create new output files for the processed files, and
these names then need to be used in the Makefile).





### Running the program
You can run the program using ``gol ./config.glider``. This will print
the current state to the screen. The configuration is a so called glider,
a structure that moves continuously to the lower right. The last state
looks like this:
```
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.1.0.0.0.0.0.0.
0.0.0.0.0.0.0.1.1.0.0.0.0.
0.0.0.0.0.0.1.1.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
```
There are also two handy makefile targets:
1. ``make run``, which will compile and then run the glider example.
2. ``make test``, which will compile and run the example, and check that the
    final output is correct.

### Summary
This tutorial showed how to initialise ``dl_esm_inf``, create fields on
the grid, and do kernel computations with the grid.
