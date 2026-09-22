# PSyclone GOcean PSyData Value Range Check Example

## Introduction

This is a simple example that shows how to use the value range check
support in PSyclone. It is a stand-alone program that can be compiled
and run. 

## Compilation
A makefile is provided to compile this example. If required,
it will compile the dl_esm_inf library and the value_range_check
wrapper library. By default, the compilation uses the version
of the dl_esm_inf library provided as a git submodule (under
``../../../external/dl_esm_inf/finite_difference``- see
https://psyclone.readthedocs.io/en/latest/developer_guide/working_practises.html)
within the PSyclone repository. You can set the environment variable
``INF_DIR`` for the ``make`` command to pick a different version.

The makefile here invokes psyclone with the script
``value_range_check_transformation.py.py``.
This script uses PSyclone's ``ValueRangeCheck`` transformation to
instrument the two invokes in the ``test.x90`` source file.

The source code computes divisions by 0 on the diagonals, resulting in
invalid numbers (Infinity).

## Running
In order to activate the value range checking, you need to
specify the value range for variables as outlined here:
https://psyclone.readthedocs.io/en/latest/user_guide/psy_data.html#value-range-check

```
$ PSY_VALUE_RANGE="main.init.b_fld%data=2:3"    ./value_range_check
...
Allocating C-T field with bounds: (1:   6,1:   6), internal region is (2:   4,2:   4)
PSyData: Variable 'b_fld%data' has the value 0.0000000000000000 at index/indices 6 1 in module 'main', region 'init', which is not between '2.0000000000000000' and '3.0000000000000000'.
...
PSyData: Variable 'a_fld%data' has the invalid value 'Inf' at index/indices 1 1 in module 'main', region 'update'.
...

```
Several warnings will be printed - the first set caused by having values not between
2 and 3 in the `init` kernel, then the warnings about Infinity being the result of
the kernel computations.

Note that you do not need to specify a kernel name and module name if your variable
name is unique. You can remove the module and kernel name:
```
$ PSY_VALUE_RANGE="b_fld%data=2:3"    ./value_range_check

PSyData: Variable 'b_fld%data' has the value 0.0000000000000000 at index/indices 6 1 in module 'main' region 'init', which is not between '2.0000000000000000' and '3.0000000000000000'.
...
PSyData: Variable 'b_fld%data' has the value 0.0000000000000000 at index/indices 6 1 in module 'main' rqegion 'update', which is not between '2.0000000000000000' and '3.0000000000000000'.
...
```
Now that the kernel and module names are not being specified, warnings are also printed
for the `update` kernel.
