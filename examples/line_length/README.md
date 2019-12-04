# PSyclone Line Length Option

This example demonstrates the use of the line length option. This
option limits the line length of the generated code to a maximum of
132 characters as this is the maximum line length supported by free
format fortran.

This option should be safe for generated code, however there is a
possibility that it will incorrectly line break long lines for
existing code. In particular it does not know about the rules for line
breaking strings. This could be a problem for algorithm code and
module inlined kernel code.

In the example in this directory the call to the `testkern_qr_code`
Kernel subroutine from the generated PSy layer is longer than 132
characters so is line wrapped.

To run from the command line simply type the following

```sh
python ../../src/psyclone/generator.py -l -d \
  ../../src/psyclone/tests/test_files/dynamo0p3 longlines.f90
```

To see how to run interactively, look at (and run) the "runme.py"
script

```sh
python ./runme.py
```

pytest tests for the -l option can be found in
[../../src/psyclone/tests/line_length_test.py](../../src/psyclone/tests/line_length_test.py).
