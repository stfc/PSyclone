# Example 4: Using PSyData Transformations

In this example you will use some of the available PSyData
transformation to instrument the previous LFRic example.
Initially we will be using kernel extraction as an example,
but a list of other applications is provided.


## Step 1: Compile all required PSyData libraries
Change directory to PSyclone's ``lib`` directory, and trigger
compilation of all PSyData libraries.

    cd $PSYCLONEHOME/lib
    make all

You need the NetCDF development package installed, the makefiles
will be using ``nf-config`` to get the appropriate compiler and
linker flags. Additionalompiler and compiler flags can be provided
using the environment variables F90 and F90FLAGS:

    F90=ifort F90flags="-O2 -traceback" make all

By default gfortran will be used.


## Step 2: Create a transformation script
This transformation script will be used as a parameter
to PSyclone when building the application. It will insert
code to extract input- and output-data of a kernel. Initially
we will only transform the kernel in ``time_evolution_alg_mod.f90``.
Use the simple script ``transform_one.py`` as a template
to create a new transformation script that applies the
LFRic-specific kernel extraction transformation. The class
is contained in ``psyclone.domain.lfric.transformations``
and called ``LFRicExtractTrans``.

The ``apply()`` function in this script is called by PSyclone before
code is created. Inside ``apply`` create an instance of the
extraction transformation and apply it to the schedule for 
"invoke_propagate_perturbation". The template contains
most of the required calls, you only need to fill in the details.

## Step 3: Modify the makefile to use the script

There is a set of makefiles provided in this directory, one for
each of the available PSyData transformation. They all include
``Makefile.inc`` which defines all of the rules, the PSyData-specific
makefiles just supply different settings for compiling and linking
with the PSyData libraries.

Each of the makefile already contains a separate rule for the file
``time_evolution_alg_mod.x90``. For this part of the exercise,
add the ``-s`` flag to the ``psycline`` invocation in
``Makefile.extract`` and provide the name of your script so
that PSyclone will invoke it. You can the create your application
using:

    make -f Makefile.extract



## Step 4: Compile

Hopefully this is as simple as typing:

    make

If you should get an error message that your script is not found,
it is possible that it contains a syntax error. You can quickly
test this by using:

    python ./your_transformation_script.py

If this command does not return anything, your script is at least
syntactically correct. 

## Step 5: Look at the produces code for the PSY layer
Have a quick look at the produced code:

    less  time_evolution_alg_mod_psy.f90

and notice the added PSyData calls, e.g.:

    TYPE(extract_PSyDataType), target, save :: extract_psy_data
    ....
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

## Step 6: Run the application and examine the output
After running the application using:

    ./time_evolution

you should get a new NetCDF file called.

## Step 7: Optional: use other PSyData libraries
The following wrapper can be used:

...

