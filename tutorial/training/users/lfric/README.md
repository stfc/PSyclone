# Simple PSyclone Application

This directory contains a very trivial example code that is based on
the LFRic infrastructure and PSyclone. It uses the simplified infrastructure
files used in PSyclone testing, and as such can be compiled and run without
the need to install LFRic.

There is no need to fully understand the program, but as a quick explanation:
the program creates two fields on a 3x3 mesh with 5 layers. The field `field_0`
is on the vertices of the finite element (W0 function space), and is initialised
with 1. The field `field_3` is on the W3 function space and represents the actual
element, it is initialised with 0. Then the kernel `testkern_w3_kernel_type` is called,
which adds the 8 neighbouring vertices of an element up (resulting in 8 for all
the finite elements).

## Using PSyclone
In this example we use PSyclone to process the algorithm file and create the new
algorithm layer and PSY-layer file.

To create the required PSy-layer, use the following command:

    psyclone -nodm -l output -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90

The same command can be triggered by `make transform`. This will create
two new output files, `main_alg.f90`, the rewritten algorithm layer `main_alg.x90`,
and `main_alg_psy.f90`, the created PSy-layer.

Look at the two created  files. You don't need to try to understand the details, since
PSyclone is creating quite a bit of code. Identify how the main program, the algorithm
layer, calls the PSylayer, and how the PSy-layer calls the kernel in a loop. Focus
on the first two invokes in `main_alg.x90`:

    call invoke( name = 'Initialise fields',        &
                 setval_c( field1,     0.0_r_def ), &
                 setval_c( field2,     1.0_r_def )  &
                 )
    call invoke( name = 'testkern_w3', testkern_w3_kernel_type(field1, field2) )

How are these lines rewritten in the algorithm file `main_alg.f90`? Then check the
PSy-layer file `main_alg_psy.f90` to find the two subroutines called from the
algorithm file. Can you find the builtins in the PSy-layer file `main_alg_psy.f90`? And
check the loop over all columns that will then  call `testkern_w3 kernel` for
each column.

You can even compile and execute the script: `make compile` will create a binary for
you (including compilation of the required LFRic infrastructure, which can take some
time). Executing the binary with `./example` will print:

     Mesh has           5 layers.
    20230905101141.609+1000:INFO : Min/max minmax of field_3 =   0.80000000E+01  0.80000000E+01

The minimum and maximum of `field1` are printed, and they are as expected 8.

The solution and explanation can be found [here](#solution-for-using-psyclone).


## Using MPI
As explained, PSyclone has the ability to support MPI parallelisation of the code. This
is simply done by using the command line option `-dm` (instead of `-nodm`):

    psyclone -dm -l output -opsy main_mpi_psy.f90 -oalg main_mpi_alg.f90 main_alg.x90

(or use `make mpi`). Use this option, and look at the PSy-layer. Some of the setup code
has changed (which is related to getting the loop sizes based on a distributed field),
and additionally there is now code that marks if the value of an array has changed,
called `dirty`. This indicates that the neighbouring processes might not have the correct
values, and these needs to be updated before they are used. The `set_dirty` calls
themselves do not trigger any communication. This is only done when the modified values
are actually used. In this example this happens in the user supplied kernel, which reads
the data of `field_w0`. Look for the code that will trigger an update of the values of
`field_w0`.

A small caveat: while this program could now be started with `mpirun`, there is
no domain-decomposition done! So each copy would run the same code (each one
computing the full field), since the fields are not distributed. This requires
additional setup, which is beyond the scope of this tutorial.

The solution and explanation can be found [here](#solution-for-using-mpi).

## Applying OpenMP
In this example you will add transformation script to the PSyclone command line.
This script will apply OpenMP transformation to the loops. Add the option
`-s omp.py` to the PSyclone command, i.e.:

    psyclone -s ./omp.py -nodm -l output -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90

(or use `make omp`). The script will combine the two loops for the two `setval_c`
calls into a single loop, and then apply OpenMP parallelisation to all loops.
Compare the PSy-layer files with the previously created files. What has changed?

The solution and explanation can be found [here](#solution-for-applying-openmp).

## Error in Algorithm Layer
Now let's have a look at some typical errors. Ideally they should not happen
for a user of a stable LFRic release, but if you for example should select
an untested set of options some of these problems could still happen. The
first example `main_err1_alg.x90` contains an invalid PSyclone builtin name,
though of course PSyclone cannot know what exactly the user meant.
Use:

    psyclone -nodm -l output -opsy main_err1_psy.f90 -oalg main_err1_alg.f90 main_err1_alg.x90

(or `make error1`). Does PSyclone's error message make sense?

The solution and explanation can be found [here](#solution-for-error-in-algorithm-layer).

## Missing Parameter
Fix the above error by modifying `main_err1_alg.x90` and putting the correct
name of the builtins in (`setval_c`, i.e. just remove the 'no_'). Run PSyclone
again (with the same parameter as above):

    psyclone -nodm -l output -opsy main_err1_psy.f90 -oalg main_err1_alg.f90 main_err1_alg.x90

(or `make error1`). What happens now?

The solution and explanation can be found [here](#solution-for-missing-parameter).

## Invalid Transformation
Now use the file `main_error2.x90`, and try to apply the `omp.py` script,
i.e. add the paramter `-s ./omp.py` to the PSyclone command line:

    psyclone -s ./omp.py -nodm -l output -opsy main_err1_psy.f90 -oalg main_err2_alg.f90 main_err2_alg.x90

(or use `make error2`). This kernel is very similar to the test kernel used originally,
but it operates on a different function space, and as a result it cannot be
be parallelised by simply applying OpenMP directives it would lead to a race
condition. What is PSyclone's behaviour? Note that LFRic provides a more
sophisticated version of the `omp.py` script, which will change the single
loop into a nested loop using a transformation called coluring. This in turn
then allows PSyclone to apply OpenMP parallelisation. PSyclone will always
internally verify if it is safe to apply a certain transformation, to make
sure it does not create incorrect code.

The solution and explanation can be found [here](#solution-for-invalid-transformation).

# Solutions
This section contains the solutions and explanations for all hands-on tasks.

## Solution for Using PSyclone
The file `main_alg.f90` contains two calls to a PSy layer:
    CALL invoke_initialise_fields(field1, field2)
    CALL invoke_testkern_w3(field1, field2)

In turn the `main_alg_psy.f90` files contains these two subroutines. The first
one contains the implementation of the builtins, i.e. the code is inlined:

    ! Call our kernels
    !
    DO df=loop0_start,loop0_stop
      field1_proxy%data(df) = 0.0_r_def
    END DO
    DO df=loop1_start,loop1_stop
      field2_proxy%data(df) = 1.0_r_def
    END DO
The second subroutine contains the call of the test kernel:

    DO cell=loop0_start,loop0_stop
      !
      CALL testkern_w3_code(nlayers, field1_proxy%data, field2_proxy%data, ndf_w3, undf_w3, map_w3(:,cell))
    END DO
Note that PSyclone will automatically provide additional required parameters to
the kernel.

## Solution for Using MPI
After initialising a field, it is marked to be modified (or 'dirty'):

    !
    DO df=loop0_start,loop0_stop
      field1_proxy%data(df) = 0.0_r_def
    END DO
    !
    ! Set halos dirty/clean for fields modified in the above loop
    !
    CALL field1_proxy%set_dirty()
Next time the fields are read, we need to get the newly computed values which
might be on neighbouring processes. So the second subroutine in the PSy-layer
contains:

    !
    ! Call kernels and communication routines
    !
    IF (field_0_proxy%is_dirty(depth=1)) THEN
       CALL field_0_proxy%halo_exchange(depth=1)
    END IF
    !
    DO cell=loop0_start,loop0_stop
    !
       CALL testkern_w3_code(nlayers, field_3_proxy%data, field_0_proxy%data, ndf_w3, undf_w3, &
                             map_w3(:,cell), ndf_w0, undf_w0, map_w0(:,cell))
    END DO

The halo-exchange uses an if-statement so that the communication functions are
only called if the values of a field have actually changed. While in our case this is
always be the case, in a more complex program it happens frequently that a field
has not been updated since the last time the halo-values were exchanged. So this
is an automatically applied optimisation that can significantly reduce the number of MPI
calls required.

## Solution for Applying OpenMP
For the first invoke, you should see that the two separate loops (see
[above](#solution-for-using-psyclone)) are now fused into a single loop:

    DO df=loop0_start,loop0_stop
      field1_proxy%data(df) = 0.0_r_def
      field2_proxy%data(df) = 1.0_r_def
    END DO

Additionally, OpenMP parallelisation is applied to all loops (including the builtins):

    $omp parallel do default(shared), private(cell), schedule(static)
    DO cell=loop0_start,loop0_stop
    !
       CALL testkern_w3_code(nlayers, field1_proxy%data, field2_proxy%data, ndf_w3, undf_w3, map_w3(:,cell))
    END DO
    !$omp end parallel do
 
## Solution for Error in Algorithm Layer

PSylone will print the following error message (or a variation of it, since depending
on version the list of builtins might change):

    Parse Error: kernel call 'no_setval_c' must either be named in a use statement (found
    ['global_mesh_base_mod', 'mesh_mod', 'mesh_mod', 'partition_mod', 'partition_mod', 
    'partition_mod', 'extrusion_mod', 'function_space_mod', 'fs_continuity_mod',
    'field_mod', 'testkern_w3_kernel_mod', 'constants_mod', 'constants_mod', 'log_mod'])
    or be a recognised built-in (one of '['x_plus_y', 'inc_x_plus_y',..., 'int_inc_min_ax', 
    'real_x']' for this API)
PSyclone cannot know if `no_setval_c` is supposed to be a builtin (for which no `use` statement
would be required), or if it is supposed to be a user-defined kernel (which requires
a `use` statement).

## Solution for Missing Parameter
PSyclone will detect that a parameter is missing for the kernel:

    Parse Error: Kernel 'testkern_w3_kernel_type' called from the algorithm layer with an
    insufficient number of arguments as specified by the metadata. Expected at least '2'
    but found '1'.

## Solution for Invalid Transformation
PSyclone internally verifies transformation to make sure it will always create valid
code. In this case, it will recognise that the kernel cannot simply be parallelised.
It would need an additional transformation (called colouring) in order to allow
threading-based parallelisation:

    Generation Error: generator: specified PSyclone transformation module 'omp'
    raised the following exception during execution...
    {
         File "/home/joerg/work/psyclone/tutorial/training/users/lfric/./omp.py", line 64, 
       in trans otrans.apply(loop)
         File "/home/joerg/work/psyclone/src/psyclone/transformations.py", line 676, in apply
       raise TransformationError(
       psyclone.psyir.transformations.transformation_error.TransformationError:
       Transformation Error: Error in DynamoOMPParallelLoopTrans transformation. The kernel
       has an argument with INC access. Colouring is required.
    }
    please check your script
This error should be reported to the developers of the optimisation script.
  
