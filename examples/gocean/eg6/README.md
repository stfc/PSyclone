# PSyclone GOcean Example 6

**Authors** J. Henrichs, Bureau of Meteorology

## Introduction

This is a very simple test that shows how to use the kernel data extraction
support in PSyclone. It is a stand alone program that can be compiled
and run.

## Compilation
You have to compile dl_esm_inf (which is included in external/dl_esm_inf)
and one of the extraction libraries in lib/extract. 
The documentation assumes that lib/extract/netcdf is used.
Instructions for those are are given in the corresponding subdirectories.
To create and compile the example, modify the Makefile if required
and type ``make``.

PSyclone is invoked with the script ``extract_transform`` which will 
add extract regions around its invokes.

## Running
When running the program, you should see:
```
parallel_init: Not running with MPI
go_decompose: using grid of   1x  1
Tile width =    3, tile height =    3
...
Allocating C-T field with bounds: (1: 12,1:  6)
Internal region is:(2:  4,2:  4)
Grid has bounds:  (1: 12,1:  6)
   15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000     
```

Two netcdf files will be produced, one for each of the two invokes
instrumented with an extract region.
