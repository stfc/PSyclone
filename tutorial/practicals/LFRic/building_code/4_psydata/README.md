# Example 4: Using PSyData Transformations

In this example you will use some of the available PSyData
transformation to instrument the previous LFRic example.
Initially we will be using kernel extraction as an example,
but a list of other PSyData applications is provided at the end.


## Step 1: Compile all required PSyData libraries
Change directory to PSyclone's ``lib`` directory, and trigger
compilation of all PSyData libraries.

    cd $PSYCLONEHOME/lib
    make all

You need the NetCDF development package installed, the makefiles
will be using ``nf-config`` to get the appropriate compiler and
linker flags. Additionally, compiler and compiler flags can be provided
using the environment variables F90 and F90FLAGS:

    F90=ifort F90flags="-O2 -traceback" make all

By default gfortran will be used.


## Step 2: Create a transformation script
This transformation script will be used as a parameter
to PSyclone when building the application. The script will apply
transformation to insert
code to extract input- and output-data of a kernel. Initially
we will only transform the kernel in ``time_evolution_alg_mod.fx0``.
Use the simple script ``transform_one.py`` as a template
to create a new transformation script that applies the
LFRic-specific kernel extraction transformation. The transformation
can be imported from ``psyclone.domain.lfric.transformations``
and is called ``LFRicExtractTrans``.

The ``apply()`` function in this script is called by PSyclone before
any code is created. Inside ``apply`` create an instance of the
extraction transformation and apply it to the schedule for
"invoke_propagate_perturbation". The template file ``transform_one.py``
contains most of the required calls, you only need to fill in the
details.

## Step 3: Modify the makefile so that PSyclone invokes the script

There is a set of makefiles provided in this directory, one for
each of the available PSyData transformation. They all include
``Makefile.inc`` which defines all of the rules. The PSyData-specific
makefiles just supply different settings for compiling and linking
with the PSyData libraries, and they contain the rule to run PSyclone
on the file ``time_evolution_alg_mod.x90``.

For this part of the exercise add the ``-s`` flag to the ``psyclone``
invocation in ``Makefile.extract`` and provide the path to your script
so that PSyclone will invoke it. You have start with ``./``, otherwise
Python will not find our file (unless you modify ``PYTHONPATH``).
You can then create your application using:

    make -f Makefile.extract

If you should get an error message that your script is not found,
it is possible that it contains a syntax error. You can quickly
test this by using:

    python ./your_transformation_script.py

If the python command does not return anything, your script is at least
syntactically correct, otherwise Python will print out a hopefully
useful error message.


## Step 4: Look at the produces code for the PSY layer
Have a quick look at the produced code:

    less  time_evolution_alg_mod_psy.f90

and notice the added PSyData calls, e.g.:

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

Notice that the variable 'perturbation' is provided once before the kernel call
with the name ``perturbation``, but then again after the invocation with
the name ``perturbation_post``. This variable is an input- and output-variable,
so its values are stored twice, just with a different name.


## Step 5: Run the application
After running the application using:

    ./time_evolution

you should get a new NetCDF file called
``time_evolution_alg_mod_psy-invoke_propagate_perturbation:prop_perturbation_code:r0.nc``.
This is just the concatenation of the two parameters to the ``PreStart`` function.
The long and convoluted name is caused by PSyclone automatically creating a unique module 
and local name. 

# Step 6: Provide a more user friendly name
You can provide a more user friendly name by providing a
``region_name`` as an option to the transformation. The region name is a pair of strings,
the first one being a module name, the second a region name. You can use them in any
way you like. The options are provided as an additional dictionary to the apply function
with "region" as key, and first a module name, then a local name. You could use:

    {"region_name": ("time_evolution", "propagate")}

Add this dictionary to your transformation script, and rebuild your application. You
have to do a

    make -f Makefile.extract clean

since the Makefile does not have a dependency on your script (though it would be
recommended to add this to the Makefile so that a change in your script automatically
triggers running PSyclone again).

You should now see that the psy-layer contains:

    CALL extract_psy_data%PreStart("time_evolution", "propagate", 7, 2)

I.e. it uses the names you have provided, and running the binary will now create
a NetCDF file called ``time_evolution-propagate.nc``.

## Step 7: Examining the output

The NetCDF command ``ncdump`` can be used to check the content of the created file:

    ncdump time_evolution-propagate.nc | less

You will see that the dimension are defined for the fields used. Some variables
are listed with the prefix ``post``, e.g. ``perturbation_post``. These are the
values of the variable after the kernel call, so it allows the NetCDF file
to store perturbation as input parameter (just ``perturbation``), and its
values after the kernel (``perturbation_post``).

At this stage no driver is created that can read in the file.


## Step 8: Instrument more than one invoke

The transformation script can be slightly changed to instrument all invokes
in a file. While this might not be useful for kernel extraction, it is
essential for e.g. parameter verification, where you typically want to
check all invokes contained in a program. Use the template ``instrument_all.py``
to apply the extraction transformation to all invokes. This script actually
requires less changes than the ``transform_one.py`` template (since it
works on all )
Following the same process, just using this more general transformation script,
will result in a binary that creates two NetCDF files at run time.


## Step 9: Try other PSyData libraries
The following set of PSyData libraries is available and can be tested.
Corresponding Makefiles are provided.

### Readonly Verification
This library makes sure that read-only parameter are not modified. The compiler
should make sure that a variable declared with ``intent(in)`` is not explicitly
overwritten, but a memory overwrite because of an out-of-bound access to a
different array is always possible. To import the transformation use:

    from psyclone.psyir.transformations import ReadOnlyVerifyTrans

Then create a transformation script based on ``transform_all.py`` script to
instrument your application. If you run this script, nothing seems to happen.
To see that the verification is actually working, set
the environment variable ``PSYDATA_VERBOSE`` to either 1 or 2 - the latter
will provide more output including each variable that is checked:

   PSYDATA_VERBOSE=2 ./time_evolution


If you are really daring you can modify ``prop_perturbation_kernel_mod.f90``.
It contains some commented out code at the bottom that will overwrite
a read-only field by using out-of-bounds array accesses (obviously you
need make sure the compiler is not doing array bounds checking). After
recompiling and running (even without setting ``PSYDATA_VERBOSE``), you will
see:



