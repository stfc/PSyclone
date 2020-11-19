# Example 4: Using PSyData Transformations

In this example you will use some of the available PSyData
transformation to instrument the previous LFRic example.

## Step 1: Compile all required PSyData libraries
Change directory to PSyclone's lib directory, and trigger
all compilations

    cd $PYTHONHOME/lib
    make all

## Step 2: Create a transformation script
This transformation script will be passed in as a parameter
to PSyclone when building the application. Initially we
will only transform the kernel in time_evolution_alg_mod.f90.
Use the simple script ``transform_one.py`` as a template
to create a new transformation script that applies one of
the following PSyData transformations;

    from psyclone.domain.lfric.transformations import LFRicExtractTrans

    from psylone.psyir.transformations import ProfileTrans

    from psyclone.psyir.transformations import ReadOnlyVerificationTrans
    
    from psyclone.psyir.transformations import NanTestTrans

The ``apply()`` function in this script is called by PSyclone.
Inside ``apply`` create an instance of the transformation you have picked,
and apply it to the schedule for "invoke_propagate_perturbation", which
you can get inside of ``apply`` using:

    invoke = psy.invokes.get("invoke_prop"agate_perturbation")
    schedule = invoke.schedule

## Step 3: Modify the makefile to use the script

The makefile already contains a separate rule for the file
time_evolution_alg_mod.x90. You need to add the ``-s`` flag
to PSyclone and provide the name of your script.

You then also need to provide the include path to the corresponding
wrapper library in F90FLAGS, and the location and name of the library
to link with.

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

