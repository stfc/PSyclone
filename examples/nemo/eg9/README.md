# PSyclone NEMO Example 9 - Value Range Check

This example demonstrates the use of PSyclone to add code to verify
variable value ranges, i.e. ensuring that variables before and after
an instrumented region have values in a user-specified range.

 
```sh
psyclone -l all -s ./value_range_check_transformation.py ../code/tra_adv.F90
```

Executing this will output the transformed Fortran code with the 
value range code added. Note that some of the lines in this
Fortran code will exceed the 132-character limit. This may be remedied
by supplying the `-l all` flag to PSyclone (as is done in the Makefile).


The generic value range check library in
``../../../lib/value_range_check/generic`` is used. The binary
instrumented for range_check will be called ``traadv.exe``.
To create and compile the example, type ``make compile``.

`tra_adv.F90` is a stand-alone version of one of the tracer-advection
routines from the NEMO ocean model. It was originally extracted by
Silvia Mocavero of CMCC. The code can be found in the `../code`
directory.

## Compiling and Execution

This example can be compiled and executed. At execution time, the
environment variable `PSY_VALUE_RANGE` *must* be specified.
Example:

```sh
    PSY_VALUE_RANGE="umask=0.0:0.9" IT=2 JPI=10 JPJ=10 JPK=5 ./traadv.exe  
    PSyData: Variable 'umask' has the value '1.0000000000000000' at index/indices 10 10 5 in module 'tra_adv', region 'r0', which is not between '0.0000000000000000' and '0.90000000000000002'.

```
Note that umask is expected to be between 0 and 1, the range was only specified
to be between 0 and 0.9 to show the warning message that would be printed.
