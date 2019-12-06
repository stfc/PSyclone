# PSyclone GOcean Example 6

**Authors:** J. Henrichs, Bureau of Meteorology

## Introduction

This is a very simple test that shows how to use the kernel data extraction
support in PSyclone. It is a stand alone program that can be compiled
and run.

## Compilation
You have to compile dl_esm_inf (which is included in external/dl_esm_inf)
and one of the extraction libraries in lib/extract. 
The documentation assumes that lib/extract/netcdf is used.
Instructions for those are are given in the corresponding subdirectories.

The makefile here will invoke psyclone with the ``--profile invokes``
flag, which will add profiling around both invokes.

## Running
When running the program, you should see:
```
 ProfileInit called
 PreStart called for module 'init_field_mod' region 'init_field_code'
 PostEnd called for module 'init_field_mod' region 'init_field_code'
 PreStart called for module 'update_field_mod' region 'update_field_code'
 PostEnd called for module 'update_field_mod' region 'update_field_code'
   15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000        15.000000000000000     
```
