# PSyclone NEMO Example 6 - Read-only Verification

This example demonstrates the use of PSyclone to add code that checks
if variables that are only read are actually modified (e.g. because
of memory overwrite).

Once you have installed PSyclone, you can transform the file using:

```sh
psyclone -s ./read_only_check.py dummy.f90
```

Executing this will output the transformed Fortran code with the 
read-only-verification code added. 

The generic read-only verification library in
``../../../lib/read_only/generic`` is used, and will also be
automatically compiled if required.

## Compiling and Execution

To create and compile the example, type ``make compile``.
This example can be compiled and executed. It will report nothing,
since no read-only variable is overwritten. But you can verify that
the variables are checked by setting ``PSYDATA_VERBOSE=2``:

```sh
$ PSYDATA_VERBOSE=2 ./dummy 
 PSyData: PreStart dummy r0
 PSyData: DeclareScalarChar: dummy r0: char_var
 PSyData: DeclareScalarLogical: dummy r0: logical_var
 PSyData: DeclareScalarChar: dummy r0: char_var
 PSyData: DeclareScalarLogical: dummy r0: logical_var
 PSyData: checked variable char_var
 PSyData: checked variable logical_var
 PSyData: PostEnd dummy r0
   3.00000000     F
```

If you copy the lines 68 and 69 from ``dummy.f90`` into ``psy.f90``,
the code will modify ``logical_var`` (by using out-of-bound array accesses.
Or you could just manually set ``logical_var = .true.``). If you then
compile again (using `make compile`, otherwise the original file would
get processed again, overwriting your changes), an error will be produced.

```sh
$ ./dummy 
 ------------- PSyData -------------------------
 Logical(kind=4) variable logical_var has been modified in dummy : r0
 Original value:  F
 New value:       T
 ------------- PSyData -------------------------
   3.00000000     T
```

Note that adding the assignment to ``logical_var`` as above to the original
``dummy.f90`` file would mean that ``logical_var`` is not a read-only variable
anymore, so no test and therefore no error will be produced for this variable.
The code commented out can also not be processed by PSyclone (missing support
for ``loc`` and ``sizeof``, which are non-standard Fortran extensions).
