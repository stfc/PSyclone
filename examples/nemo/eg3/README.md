# PSyclone NEMO Example 3

This directory contains a relatively simple example transformation script,
`kernels_trans.py`.  This demonstrates the use of PSyclone to add OpenACC
Kernel and Data directives to NEMO code. Note, the transformations are
indicative of what could be done - no claim is made as to the performance of
the resulting code.

A more sophisticated `acc_kernels_trans.py` script is provided in the
`../scripts` directory. This is designed to work with the NVIDIA (PGI)
compiler's 'managed memory' option (`-gpu=mem:managed`) and therefore makes
no attempt to control data movement to/from the GPU. It also adds
profiling instrumentation around those parts of the code that have
not been enclosed within an OpenACC KERNELS region.

Once you have installed PSyclone, either script may be supplied to
PSyclone via the -s option, e.g.:

```sh
psyclone -s ./kernels_trans.py ../code/tra_adv.F90
```

Executing this will output 1) the PSyclone invokes found in the code,
2) PSyclone's Schedule view of the original code, 3) PSyclone's
Schedule view of the code after adding OpenACC Kernels directives, 4)
PSyclone's Schedule view of the code after adding OpenACC Kernels and
Data directives, and 5) the transformed Fortran code with the OpenACC
directives added. Note that some of the lines in this Fortran code will
exceed the 132-character limit. This may be remedied by supplying the
`-l all` flag to PSyclone (as is done in the Makefile).

Running PSyclone with the `../scripts/acc_kernels_trans.py` script will
produce similar output but the Schedule will contain `Profile` nodes
and there will be no Data directives.

`tra_adv.F90` is a stand-alone version of one of the tracer-advection
routines from the NEMO ocean model. It was originally extracted by
Silvia Mocavero of CMCC. The code can be found in the `../code`
directory.

## Compiling and Execution

If desired this example may be compiled and executed on a GPU device
provided a suitable compiler with OpenACC support is available. Note
that this example is only provided to demonstrate how one adds OpenACC
directives using PSyclone with the NEMO API. It is not intended to
demonstrate how to obtain good performance.

Since `tra_adv.F90` is instrumented for use with the dl_timer library,
this library is also required. It is available on
[github](https://github.com/stfc/dl_timer).

Once dl_timer has been downloaded, the supplied Makefile must be
edited to supply the location of the library. The compiler and flags
must be specified via the F90 and F90FLAGS environment variables, e.g.
to use the NVIDIA compiler and OpenACC:

```sh
export F90=nvfortran
export F90FLAGS="-O1 -acc -gpu=cc70 -Minfo=all"
export LDFLAGS="-acc -gpu=cc70"
```

The size of domain and number of time-steps are also picked-up from
environment variables. Some example settings are provided in the
`domain_setup.sh` file.
