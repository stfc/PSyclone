# Adding MPI support to a PSyclone Program

In this session we will add support for distributed memory
parallelisation using MPI.

Adding MPI support does not require any transformation to be applied,
MPI support is added by a combination of infrastructure functionality, here
`dl_esm_inf`, and the way PSyclone creates the PSy-layer.

## MPI Support in the Infrastructure Library
1. Parallel Initialisation
The infrastructure can be compiled with MPI support. This library
is then called `lib_dm_fd.a` (instead of `lib_df.a`). The API
for the user application is identical, but it will use MPI at
initialisation time. The distributed memory version of this library
will recognise if it was started with more than one process,
and in this case compute a suitable domain decomposition for the
given number of processes. It will also add a halo region for each
field, which is used to store the field values from neighbouring
processes. It is possible for the user to overwrite these defaults
(e.g. by manually specifying the domain decomposition), but in this
example we will keep the defaults, which will result in a domain
decomposition to be as close to a square as possible.
Similarly, when finalising, it will internally call `MPI_Finalize()`.

2. Distributing Data
When creating a field and providing it with a pointer to initial
data, only the locally required data is copied into the field.
This is not very efficient for large application (since each
process needs to read in the global field), but given the very
simple and short input file size this is acceptable for this application.

3. Gathering Local Data
The field object provides a method to gather the local data from
all processes into a 2d array on the master. This is used for
the simple output functionality.

4. Halo Exchanges
The field object also provides a function for a halo exchange.


The first three items mean that there is no change required in
startup, distributing the input data and printing the output data.
All these calls will transparently work with MPI, if the distributed
memory version `lib_dm_fd` is linked in.

## Adding Halo Exchanges
The only required change is adding halo exchanges, since counting
the number of neighbours means that updated information from neighbouring
processes is required. And luckily, this error-prone step can be
done automatically by PSYclone.

PSyclone relies on the meta-data provided for each kernel. Especially
in `count_neighbours_mod` the declaration is:

       type(go_arg), dimension(2) :: meta_args =         &
            (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),    & ! field
               go_arg(GO_READ,  GO_CT, GO_STENCIL(111,   &
                                                  101,   &
                                                  111))  & ! field
             /)

This indicates that the second argument, the current information
about empty and filled cells, is accessed using a stencil-operation
of depth 1. The infrastructure library will do the required halo
exchange, acquiring the current data from all eight neighbours.
And PSyclone will automatically insert calls to halo-exchanges
for any required field.

To add MPI support, use the PSyclone command line flag `-dm` (for
distributed memory):

    psyclone -l output -dm -d /home/joerg/work/psyclone/tutorial/training/gol-lib -api gocean1.0 \
             -oalg time_step_alg_mod.f90 -opsy time_step_alg_mod_psy.f90 time_step_alg_mod.x90


Inspecting the PSy-layer output file `time_step_alg_mod_psy.f90` shows:

    SUBROUTINE invoke_0_count_neighbours(neighbours, current)
      USE count_neighbours_mod, ONLY: count_neighbours_code
      ...
      CALL current%halo_exchange(1)
      DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
        DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
          CALL count_neighbours_code(i, j, neighbours%data, current%data)
        END DO
      END DO
    END SUBROUTINE invoke_0_count_neighbours

You see that immediately before the kernel that counts the neighbours
the halo exchange is automatically triggered.

> Note that at this stage the infrastructure library (and therefore also
> PSyclone) only supports halo exchanges of depth 1.

## Combination with other Transformations
MPI can obviously work together with other transformations. For example,
in order to fuse loops and apply OpenMP, the script from example 2.8
can be used. This results in the following PSy-layer:

      CALL current%halo_exchange(1)
      !$omp parallel default(shared), private(i,j)
      !$omp do schedule(dynamic)
      DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
        DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
          CALL count_neighbours_code(i, j, neighbours%data, current%data)
          CALL compute_born_code(i, j, born%data, current%data, neighbours%data)
          CALL compute_die_code(i, j, die%data, current%data, neighbours%data)
        END DO
      END DO
      !$omp end do
      !$omp do schedule(dynamic)
      DO j = current%internal%ystart, current%internal%ystop, 1
        DO i = current%internal%xstart, current%internal%xstop, 1
          CALL combine_code(i, j, current%data, die%data, born%data)
        END DO
      END DO
      !$omp end do
      !$omp end parallel

The whole computation is included in a single `OpenMP parallel` section, the loops
over rows are parallelised using OpenMP, and as many loops as possible are fused.
Remember that `combine_code` cannot be called before all previous computations
have been finished (otherwise the current state is updated before the counting
for its neighbours to the right and bottom have been updated.
