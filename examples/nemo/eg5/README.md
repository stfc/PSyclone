# PSyclone NEMO Example 5 - Kernel Data Extraction

This example demonstrates the use of PSyclone to add code for kernel
extraction, i.e. writing input- and output-data of a kernel into a data file.

Once you have installed PSyclone, either script may be supplied to
PSyclone via the -s option, e.g.:

```sh
psyclone -l all -s ./extract_kernels.py ../code/tra_adv.F90
```

Executing this will output the transformed Fortran code with the 
kernel extraction code added. Note that some of the lines in this
Fortran code will exceed the 132-character limit. This may be remedied
by supplying the `-l all` flag to PSyclone (as is done in the Makefile).


The stand-alone extraction library in
``../../../lib/extract/binary/generic`` is used as default, and
will also be automatically compiled. You can also use the NetCDF based
or ASCII extraction library by setting the environment variable
`TYPE` to `netcdf` or `ascii` correspondingly when calling `make`, e.g.:

    $ TYPE=netcdf make compile

The NetCDF version obviously requires NetCDF to be available (including
``nf-config`` to detect
installation-specific paths). The NetCDF-based extraction library in
``../../../../lib/extract/netcdf/generic``
will also be automatically compiled.
Similarly, the ASCII-based extraction library in
``../../../../lib/extract/ascii/generic``
will also be automatically compiled and used if ASCII output is selected.

The binary  instrumented for extraction will either be called
``traadv-binary.exe`` or ``traadv-netcdf.exe``.
More details on compiling these libraries are in the corresponding
subdirectories. To create and compile the example, type ``make compile``.

`tra_adv.F90` is a stand-alone version of one of the tracer-advection
routines from the NEMO ocean model. It was originally extracted by
Silvia Mocavero of CMCC. The code can be found in the `../code`
directory.

## Compiling and Execution

This example can be compiled and executed, resulting in several
data files created. The size of domain and number of time-steps are also
picked-up from environment variables. Some example settings are provided
in the `domain_setup.sh` file.

Note that driver creation is not yet supported in NEMO, see issue #2058.
