# Simple PSyclone Application

This directory contains a very trivial example code that is based on
the LFRic infrastructure and PSyclone. It uses the simplified infrastructure
files used in PSyclone testing, and as such can be compiled and run without
the need to install LFRic.

The script creates two fields on a 3x3 mesh with 5 layers. One is initialised
with 0, the other with 1. Then the kernel `testkern_w3_kernel_type` is called,
which adds the second field to the first one (there is a builtin called
`inc_X_plus_Y` which would do this without the need for implementing a kernel
for this).

To create the required PSy-layer, use the following command:

    psyclone -nodm -l output -opsy main_alg_psy.f90 -oalg main_alg.f90 main.x90

The same command can be triggered by `make transform`. This will create
two new output files, `main_alg.f90`, the rewritten algorithm layer `main_alg.x90`,
and `main_alg_psy.f90`.

You can even compile and execute the script: `make compile` will create a binary for
you (including compilation of the required LFRic infrastructure if required).
Executing the binary with `./example` will print:

     Mesh has           5 layers.
    20230829165343.079+1000:INFO : Min/max minmax of field1 =   0.10000000E+01  0.10000000E+01
The minimum and maximum of the field is as expected now 1.

1. Look at the files. You don't need to try to understand the details, since PSyclone
   is creating quite a bit of code, but identify how the main program, the algorithm
   layer, calls the PSylayer, and how the PSy-layer calls the kernel in a loop. Focus
   on the first two invokes:

       call field2%initialise( vector_space = vector_space_ptr, name="field2" )
       call invoke( name = 'Initialise fields',        &
                    setval_c( field1,     0.0_r_def ), &
                    setval_c( field2,     1.0_r_def )  &
                    )
       call invoke( name = 'testkern_w3', testkern_w3_kernel_type(field1, field2) )

   Can you find the builtins in the PSy-layer file `main_alg_psy.f90`? And
   the call to the testkern_w3 kernel?

2. Apply the `omp.py` optimisation script, which will do some loop fusion, and
   also applies OpenMP parallelisation. You have to specify the full name of
   the script including path with the `-s` command line options, e.g.
   `-s ./omp.py` (or use `make omp`). Compare the PSy-layer files with
   the previously created files. What has changed?

# Solution
1. The file `main_alg.f90` contains two calls to a PSy layer:
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

2. For the first invoke, you should see that two separate loops (see solution for 1 above)
   are now fused into a single loop:

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
 