# Using PSyclone's PSyData Plugin Structure - Kernel Extraction

This examples shows PSyclone's kernel extraction and driver
creation functionality (called PSyKE). It utilises the PSyData
interface.

## Kernel Data Extraction
Kernel data extraction describes a very useful and new ability
of PSyclone. Applying the kernel extraction transformation will
insert PSyData calls around kernels or invokes. The runtime library
uses this to create a NetCDF file at runtime, which stores all
input- and output-parameters of the included kernel calls. This
transformation can also create a stand-alone driver - a program
which will read the written NetCDF file, executes the kernel(s),
and then compares the result. This allows you to automatically
develop unit tests, or to optimise the implementation of a single
kernel, without having to rerun the full application every time.

The transformation `GOceanExtractTrans` is used to add the data
extraction code to a region of code and to create the driver.
It takes two optional parameters that can be specified in the
options argument - if a driver should be created (default is
False), and a tuple containing a module and region name. While
PSyclone will create names for you, they are often not intuitive,
so you have an option to specify a module and region name to use.

The following line enables the creation of a driver program, and also
defines the module- and region-name:

    ... apply(..., options={"create_driver": True,
                            "region_name": ("timestep", "myinvoke")})
                            })

Typically you might want to extract more than one kernel at a time
(I often use it for any kernel in an application, e.g. for LFRic's
gungho application over 1500 kernels are created, though only around
700 are executed when running the example configuration), so naming
all kernels becomes impossible. PSyclone will add region and invoke
numbers if required to make sure unique names are created. Initially,
do not specify a name to get used of the automatic PSyclone naming
scheme. 

In the PSyclone script `extraction.py`, add the required parameter
in the options dictionary when calling `apply`. The code can be
compiled by using `Makefile.extraction` (`make -f Makefile.extraction`).
After compilation, run the binary `gol.extraction ../gol-lib/config.glider`.

You should now see that a new file `psy_time_step_alg_mod-invoke_compute-r0.nc`
has been created. The added `r0` is a region number added automatically by
PSyclone (in case that you instrument more than one kernel).

You can use 
`ncdump psy_time_step_alg_mod-invoke_compute-r0.nc | less`
to look at the content of this file. You can see that the dimensions
of each field are specified, as well as the data of each field.
Fields that input- and output-parameters will actually have
two entries in that file. An example is the field `current`: The
first entry is called `current`, the other one `current_post`. This field
is an input-parameter, because it contains the initial state of the 
grid, and it is an output-parameter, since its status is being updated
at the end. Some parameters are (maybe unexpected) output parameters:
`die` and `born` for example. While technically they are only temporary
work arrays, they are written first (so they are not input parameters),
and any written variable is considered an output, and so will be stored
in the file. As they do not need to have input values, only the
output values are stored in the fields `die_post` and `born_post`.

The output values of any variable (i.e. the ones with a `post` attached
to the names) will be used to verify the computation of the kernel.

The driver program is called `driver-psy_time_step_alg_mod-invoke_compute-r0.F90`
(reflecting the name of the created NetCDF file)
and you can have a look at the code created by PSyclone. After
a sequence of reading in the data from the file, you will see:

    ...
    do j = current_internal_ystart, current_internal_ystop, 1
      do i = current_internal_xstart, current_internal_xstop, 1
        call combine_code(i, j, current, die, born)
      enddo
    enddo
    call compare_init(6)
    call compare('born', born, born_post)
    call compare('current', current, current_post)
    call compare('die', die, die_post)
    call compare('i', i, i_post)
    call compare('j', j, j_post)
    call compare('neighbours', neighbours, neighbours_post)
    call compare_summary()


The nested loops are the psy-layer code that calls the kernel. Notice
that the driver is not using any field data types, it is using plain
Fortran arrays and non-structured scalar types, so it can be used
stand-alone without dependencies to a infrastructure library. The kernel
call is followed by a sequence of correctness tests as shown above. For
now all results must be bitwise identical.

You can compile and link the driver (which needs the original kernel
object files) using `make -f Makefile.extraction driver`. If you
execute the binary, it should print:

    current correct
    i correct
    j correct

While it might be surprising that it also checks the loop variables
`i` and `j`, from PSyclones point of view these are variables
that are modified by the code (which includes the loops), and as such
need to be tested.

Note that the extraction transformation can be applied to a list of
kernels at the same time (as long as they are consecutive), and the
driver will call all kernels. Result checking though will only happen
after all kernels have been called, not after each individual kernel.

There are two additional restrictions:
1) If a kernel is called multiple times, the NetCDF file will be
   overwritten each time the kernel is called, only the data of the
   last run will be preserved. We are looking at supporting to only
   store the data for a specific invocation (e.g. say only store the
   file after the 10th call).

2) Distributed memory (MPI) is not yet supported. Each process will
   write to the same file. We will be supporting distributed memory
   to some degree: each file will get a process rank attached, meaning
   that you can test a kernel for each rank.
