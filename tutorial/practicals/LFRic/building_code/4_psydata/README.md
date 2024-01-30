# Example 4: Using PSyData Transformations

In this example you will use some of the available PSyData
transformations to instrument the previous LFRic example.
Initially we will be using kernel extraction as an example
but a list of other PSyData applications is provided at the end.
The ``solutions`` directory contains working versions of the
transformation scripts and a ``Makefile``. If you are having problems,
you can look at the scripts and various makefiles in that directory.
You can also directly invoke them using for example:

    make -f solutions/Makefile.extract_one


## Step 1: Create a transformation script
The transformation script will be used as a parameter
to PSyclone when building the application.  The ``trans()``
function in this script is called by PSyclone before
any code is created and can be used to modify the PSyIR
trans, and this is were you can add transformations to a code.

In this exercise you will create a script that will apply
PSyclone's existing kernel extraction transformation to an invoke.
This will insert code that writes the input- and output-data
of a kernel to a file. Initially we will only transform one kernel,
the one that propagates the perturbation. Open the file
``extract_one_transform.py`` in an editor and follow these steps:


### Step 1.1: Creating the transformations
The kernel extraction transformation for LFRic is called
``LFRicExtractTrans`` and can be imported from
``psyclone.domain.lfric.transformations``. Add the import statement
and then create an instance of the transformation (see line 59
and 60 of the script).

### Step 1.2: Get the invoke object to be transformed
The easiest way of getting this invoke in the transformation script
is to use the name that was given to the invoke call. Look at the
file ``time_evolution_alg_mod.x90`` and find the name given to the
invoke statement that propagates the perturbation.

PSyclone adds ``invoke_`` as a prefix to the name given to an invoke.
This name can then be used to get the invoke object from
``psy.invokes``. Use this name in the script (see line 68).

### Step 1.3: Apply the transformation to the schedule
Next you need to apply the transformation to the schedule.
See lines 76 - call the ``apply`` method of the transformation
object with the schedule as parameter.


## Step 2: Modify the makefile so that PSyclone invokes the script

There is a set of makefiles provided in this directory, one for
each of the available PSyData transformations. They all include
``Makefile.inc`` which defines all of the required rules. The PSyData-specific
makefiles just supply different settings for compiling and linking
with the PSyData libraries, and they contain a special rule to run
PSyclone on the file ``time_evolution_alg_mod.x90``.

For this part of the exercise add the ``-s`` flag to the ``psyclone``
invocation in ``Makefile.extract_one`` and provide the path to your script
so that PSyclone will invoke it. It must start with ``./``, otherwise
Python will not find our file.

Once this is done, you can then create your application using:

    make -f Makefile.extract_one


You need the NetCDF development package installed, the makefiles
will be using ``nf-config`` to get the appropriate compiler and
linker flags. Additionally, compiler and compiler flags can be provided
using the environment variables F90 and F90FLAGS:

    F90=ifort F90flags="-O2 -traceback" make -f Makefile.extract_one

By default gfortran will be used. The makefile will automatically
compile the required PSyData library as well.

If you should get an error message that your script is not found,
it is possible that it contains a syntax error. You can quickly
test this by using:

    python ./your_transformation_script.py

If the python command does not return anything, your script is at least
syntactically correct, otherwise Python will print out a hopefully
useful error message.


## Step 3: Examine the generated PSy layer
Have a quick look at the produced code:

    less  time_evolution_alg_mod_psy.f90

and notice the added PSyData calls, e.g.:

```fortran
    TYPE(extract_PSyDataType), target, save :: extract_psy_data
    ....
    CALL extract_psy_data%PreStart("time_evolution_alg_mod_psy", "invoke_propagate_perturbation:prop_perturbation_code:r0", 7, 2)
    CALL extract_psy_data%PreStart("main", "update", 7, 2)
    CALL extract_psy_data%PreDeclareVariable("nlayers", nlayers)
    CALL extract_psy_data%PreDeclareVariable("perturbation", perturbation)
    CALL extract_psy_data%PreDeclareVariable("perturbation_post", perturbation)
    CALL extract_psy_data%PreEndDeclaration
    CALL extract_psy_data%ProvideVariable("nlayers", nlayers)
    CALL extract_psy_data%ProvideVariable("perturbation", perturbation)
    CALL extract_psy_data%PreEnd
    ! ... kernel call here
    CALL extract_psy_data%PostStart
    CALL extract_psy_data%ProvideVariable("cell_post", cell)
    CALL extract_psy_data%ProvideVariable("perturbation_post", perturbation)
    CALL extract_psy_data%PostEnd
```

Notice that the variable 'perturbation' is provided once before the kernel call
with the name ``perturbation``, but then again after the invocation with
the name ``perturbation_post``. This variable is an input- and output-variable,
so its values are stored twice, just with a different name.


## Step 4: Run the application
After running the application using:

    ./time_evolution

you should get a new NetCDF file called
``time_evolution_alg_mod_psy-invoke_propagate_perturbation:prop_perturbation_code:r0.nc``.
This name is just the concatenation of the two parameters to the ``PreStart`` function.
It is rather long and convoluted because PSyclone automatically creates a unique module 
and local name. 

# Step 5: Provide a more user friendly name
You can provide a more user friendly name by providing a
``region_name`` as an option to the transformation. The region name is a pair of strings,
the first one being a module name, the second a region name. You can use them in any
way you like. The options are provided as an additional dictionary argument to the ``apply``
function of a transformation, with "region_name" as key, and then a tuple containing first
a module name, then a local name. You could use:

    {"region_name": ("time_evolution", "propagate")}

Add this dictionary to your transformation script, and rebuild your application. You
have to do a

    make clean

since the makefile does not have a dependency on your script (though it would be
recommended to add this to the makefile so that a change in your script automatically
triggers running PSyclone again).

You should now see that the psy-layer contains:

```fortran
    CALL extract_psy_data%PreStart("time_evolution", "propagate", 7, 2)
```

I.e. it uses the names you have provided, and running the binary will now create
a NetCDF file called ``time_evolution-propagate.nc``.


## Step 6: Examining the NetCDF output file

The NetCDF command ``ncdump`` can be used to check the content of the created file:

```bash
    ncdump time_evolution-propagate.nc | less
```

You will see that the dimensions are defined for the fields used. Some variables
are listed with the postfix ``post``, e.g. ``perturbation_post``. These are the
values of the variables *after* the kernel call, so it allows the NetCDF file
to store perturbation as input parameter (just ``perturbation``), and its
values after the kernel (``perturbation_post``).

At this stage no driver is created that can read in the file.


## Step 7: Instrument more than one invoke

The transformation script can be slightly changed to instrument all invokes
in a file. While this might not be useful for kernel extraction, it is
essential for e.g. parameter verification, where you typically want to
check all invokes contained in a program. Use the template ``extract_all_transform.py``
to apply the extraction transformation to all invokes in a file. This script
actually requires less changes than the``extract_one_transform.py`` template
(since it works on all invokes):

1. Import the required transformation and create an instance (line 59).
2. In the loop over all invokes apply the transformation (line 75).

 Then modify ``Makefile.extract_all`` to supply your
``extract_all_transform.py`` script to PSyclone.
Following the same process as above will result in a binary that creates
two NetCDF files at run time - one for each invoke in the file.


## Step 8: Try other PSyData libraries
The following set of PSyData libraries is available and can be tested.
Corresponding makefiles are provided.

### Readonly Verification
This library makes sure that kernel arguments that are declared as read-only
(in the kernel metadata) are not modified. The compiler
should make sure that a variable declared with ``intent(in)`` is not explicitly
overwritten, but a memory overwrite because of an out-of-bound access to a
different array is always possible. This library will catch this kind of
memory overwrite. To import the transformation use:

```python
    from psyclone.psyir.transformations import ReadOnlyVerifyTrans
```
Then create a transformation script based on ``readonly_all_transform.py``
script to instrument your application. Modify ``Makefile.readonly_all``
to use your transformation script (using the ``-s`` flag). Do a

    make clean

to make sure PSyclone will get invoked again, and recompile your application
using

    make -f makefile.readonly_all

If you run the created binary, nothing seems to happen.
To see that the verification is actually working, set
the environment variable ``PSYDATA_VERBOSE`` to either 1 or 2 - the latter
will provide more output including each variable that is checked:

```bash
    PSYDATA_VERBOSE=2 ./time_evolution
```

If you are really daring you can modify the file
``prop_perturbation_kernel_mod.f90``.
It contains some commented out code at the bottom that will overwrite
a read-only field by using out-of-bounds array accesses. Look for the
comment:

    ! FOR READONLY VERIFICATION

in the file, Obviously you need make sure not to enable any array bounds
checking in the compiler. After recompiling and running (even without
setting ``PSYDATA_VERBOSE``), you will see:

    20201204115251.691+1100:INFO : time_evolution_alg_step: Propagating perturbation field at time-step 1
     ------------- PSyData -------------------------
     Double precision field chi(1) has been modified in time_evolution : invoke_propagate_perturbation
     Original checksum:   4657399313863802880
     New checksum:       -5811961289819291648
     ------------- PSyData -------------------------
    20201204115251.712+1100:INFO : Min/max perturbation =   0.00000000E+00  0.32369708E+04

Note that this error is only printed in the first time step. After the first modification
the modified value is not changed again in the application, so no change to the read-only
field is detected.

### NAN Verification
This library verifies that all input- and output-parameters of a kernel are
neither NAN nor an infinite number. If such a value is detected, a message
like this will be printed:

     PSyData: Variable perturbation has the invalid value
                           NaN  at index/indices           11


The transformation ``NanTestTrans`` is imported from ``psyclone.psyir.transformations``.
You can use the template ``nan_all_transform.py`` for your script, and ``Makefile.nan_all``
for the makefile to use.

This example by itself will not print any message (since there is no invalid floating
point number), so you have to use ``PSYDATA_VERBOSE`` and set it to 1 or 2 to see that
tests are actually happening. Alternatively, the file
``prop_perturbation_kernel_mod.f90`` contains code that you can uncomment that will
introduce a NAN into the result field. Search for the comment

    ! FOR NAN VERIFICATION:

in this file and uncomment the indicated lines. Then recompile and run your application.
When you run this application again, a significant number of NANs will be reported (over 500k).
While we only overwrite one entry in a field in the kernel, this LFRic kernel is called once
for each column. And after the first time this is reported as an invalid input parameter,
and then as an invalid output value.

### Stand-alone Kernel Extraction
PSyclone provides two different kernel extraction libraries. Besides the
NetCDF based one, which was used above, there is also a stand-alone library
which only uses Fortran IO and has no other external dependencies. This
library is ideal if your application does not already have a NetCDF dependency.
The downside is that you can't easily inspect the binary output file.
There is no other change to the script or the application required, and you
don't need to run PSyclone again if you have already processed the files,
but you need to recompile the files that use kernel extraction.

The extraction makefiles are all set up so you can easily switch to use
the stand-alone library instead of the NetCDF one. Just set the
makefile variable `TYPE` to `standalone` when invoking `make`:

    make TYPE=standalone -f solutions/Makefile.extract_one

After running the instrumented `time_evolution` binary, a new output file
`time_evolution-propagate.binary` is created.
