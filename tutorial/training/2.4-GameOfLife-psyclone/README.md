# Game of Life with PSyclone

The goal of this exercise is to re-implement the 
Game of Life using PSyclone. The implementation
will closely follow the design of the previous
session: we use `dl_esm_inf` and especially the
field type provided to implement the 2d arrays.
And again we compute first the number of neighbours,
then based on the current state and number of
neighbours we compute if a cell dies, or a new
one is born, before combining these fields. But
these functions will now become kernels that operate
on a single element, and PSyclone will create the
loop structures automatically.

A word about coding style: we will follow the 
LFRic coding style, which is documented on
https://code.metoffice.gov.uk/trac/lfric/wiki/LFRicTechnical/FortranCodingStandards
(login required). This requires that any module
name is the same as the filename (and typically
has a suffix of `_mod`). Any file that needs to
be processed by PSyclone, i.e. it contains `invoke`
statements, uses the extensions `.x90`. Additionally,
the filename should contains `_alg`. For example, the time
stepping module in the Game of Life will be called
`time_step_alg_mod.x90`. 

> This is in no way required or enforced by PSyclone
> itself (all filename need to be specified with extensions),
> and you can define your own coding style.

This tutorial starts by implementing the various kernels
before implementing the actual invoke calls. Note that
the kernels need to be declared (i.e. at least their
meta-data needs to be available) before you can run
PSyclone, even for simple tests.

> Note that it is not possible to verify kernel meta-data.
> Only when PSyclone is running on the algorithm layer
> with the invoke statements will the kernel declarations
> be read and interpreted.

## Implement `compute_born` kernel
We start with implementing the `compute_born` kernel. It
is contained the `compute_born_mod.f90` file. Note that
only the algorithm layers are processed by PSyclone, so
there is no need for the kernel files to use the `.x90`
extension. PSyclone will read the kernel files when creating
the algorithm layer.

This file already contains the meta-data information for
the kernel:

    private
    public compute_die, compute_die_code
    type, extends(kernel_type) :: compute_die
       type(go_arg), dimension(3) :: meta_args =         &
            (/ go_arg(GO_WRITE, GO_CT, GO_POINTWISE),    & ! field
               go_arg(GO_READ,  GO_CT, GO_POINTWISE),    & ! field
               go_arg(GO_READ,  GO_CT, GO_POINTWISE)     & ! field
             /)
       !> This kernel writes to all points of the
       !! simulation domain.
       integer :: ITERATES_OVER = GO_INTERNAL_PTS
       integer :: index_offset = GO_OFFSET_SW

       contains
         procedure, nopass :: code => compute_die_code
       end type compute_die

As explained, PSyclone will read this type information that
specified the meta-data for the kernel. It looks like a
standard Fortran type declaration, and as such can be compiled
with any compiler, but the type itself is not used, it is only
used to provide the meta-data to PSyclone. Still, the type
and the kernel function need to be declared to be public,
otherwise a compilation error will be triggered.

As a a reminder, here the details of this declaration:

1. The kernel is called `compute_die` based on
     the line `type, extends(kernel_type) :: compute_die`.
2. The kernel takes three arguments (`dimension(3)`). The first
    argument will be the field containing cells that die, the
    second argument the current status of all cells, and the
    last the number of neighbours.
3. The first argument is written, and it is a field on the
    CT-space and is used pointwise (i.e. only using the
    coordinates `(i,j)`).
4. The second and third arguments are both read-only, on the
    CT-space and used pointwise.
5. It iterates over the internal points, i.e. does not handle
    any points on a halo or boundary layer.
6. Index offset is South-West.
7. The subroutine that contains the kernel is called
    `compute_die_code`. Note that 

There is no need to modify this meta-data, it has been correctly
filled out for you. But the body of
the actual kernel code (`compute_die_code`) is missing. 
Implement this code, and remember that only need to write
code for the single element `(i,j)` (`i` and `j` are
parameters passed in by PSyclone). Remember that the
output field needs to be initialised with zero here
(since it will contain the result from the previous
iteration otherwise).


## Implement the `compute_born` kernel
As you might remember from the exercise 2.2, this kernel
takes three arguments, first an output field that stores
which entries will get a new, alive cell, and two input
parameters specifying the current state and the number
of neighbours for each cell.

In the template for this function, you not only need to
implement the actual kernel code, but also add the declaration
of the parameters in the meta data. You can use the previous
kernel as template.

## Implement the `combine` kernel
This template file is nearly empty. You need to add the full
meta-data declaration (make sure to declare the type and
the actual function called to be public), and the kernel code.
Again, feel free to use the previous kernel as example.

## Implement the `count_neighbours` kernel
This is the last kernel that needs to be implemented. While
the kernel takes only one output parameter (number of neighbours)
and one input parameter (current state), there is one big
difference compared with any other kernel: this kernel accesses
the current state as a stencil, i.e. it does not (only) read
`current(i,j)`, but also accesses the neighbours. The meta-data
for this kernel must declare this access (which will later important
in case of a distributed memory implementation at a later stage).
So the access for the input field is not `GO_POINTWISE`, but
it has to be declared as `GO_STENCIL`, and the parameter specifying
the extend in each direction in which neighbouring elements are
accessed. The stencil declaration (which is used instead of
`GO_POINTWISE`) looks like this:

    STENCIL(010,   &
            010,   &
            010))

for a field that accesses elements one row above and below.
Adjust this declaration to the access pattern used when counting
the neighbours.

The template for `count_neighbours` contains most of the meta-data
and the full implementation. All you need to do is to insert the
right stencil declaration.

> Note that at this stage PSyclone does not verify the stencil
> declaration with the actual code, but it is on the todo list
> of the PSyclone developers.


## Update `time_step_mod`
Now that all kernels have been implemented, we can update the
time step function `time_step_alg_mod.x90`. Insert the missing
invoke statements and the required parameters (you might need
to check with the kernel meta-data to make sure you have the
parameters in the right order).

## Use PSyclone
Now you run PSyclone to see if it correctly processes
all your kernels. You can use the following command line:

    psyclone -nodm -api gocean1.0 -oalg time_step_alg_mod.f90 \
             -opsy time_step_alg_mod_psy.f90 time_step_alg_mod.x90

The flag `-nodm` disables distributes memory processing. We need
to specify which PSyclone API we are using (`gocean1.0`, which is
the 2d finite difference one based on dl_esm_inf). Then the output
filename for the algorithm layer (`-oalg`) and the psy layer files
(`-opsy`) and the input filename.

The provided Makefile already contains a stand-alone target to do
this for you:

    make time_step_alg_mod.f90

If you get an error message, check the syntax of the invoke statement
and the kernels. It can sometimes be useful to compile the algorithm
or kernel file with a compiler, which might give easier to understand
error messages, for example:

    gfortran -I ../external/PSyclone/external/dl_esm_inf/finite_difference/src/
             -c ./combine_mod.f90

Once PSyclone has successfully processed all the files, look at
the algorithm- and psy-layer file PSyclone has created, i.e.
`time_step_alg_mod.f90` and `time_step_alg_mod_psy.f90`.

Here the changes that PSyclone has applied to the file
`time_step_alg_mod.x90` in the newly created algorithm-layer
file `time_step_alg_mod.f90`. Note that his file will not
contain any comments, and the layout of the code will be different.

    USE psy_time_step_alg_mod, ONLY: invoke_3_combine
    USE psy_time_step_alg_mod, ONLY: invoke_2_compute_die
    USE psy_time_step_alg_mod, ONLY: invoke_1_compute_born
    USE psy_time_step_alg_mod, ONLY: invoke_0_count_neighbours

It has added `use` statements for the subroutines in the psy-layer
file. Then it has also replaced the invoke statements with
normal Fortran calls:

    DO time = 1, time_steps
      CALL invoke_0_count_neighbours(neighbours, current)
      CALL invoke_1_compute_born(born, current, neighbours)
      CALL invoke_2_compute_die(die, current, neighbours)
      CALL invoke_3_combine(current, die, born)
      IF (time_steps <= 20) CALL output_field(current)
    END DO

You can find these newly created subroutine in the psy-layer file
`time_step_alg_mod_psy.f90`. For example, it contains the
subroutine to count the neighbours:

    MODULE psy_time_step_alg_mod
      USE field_mod
      USE kind_params_mod
      IMPLICIT NONE
      CONTAINS
      SUBROUTINE invoke_0_count_neighbours(neighbours, current)
        TYPE(r2d_field), intent(inout) :: neighbours
        TYPE(r2d_field), intent(inout) :: current
        INTEGER j
        INTEGER i
  
        DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
          DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
            CALL count_neighbours_code(i, j, neighbours%data, current%data)
          END DO
        END DO
  
      END SUBROUTINE invoke_0_count_neighbours

The subroutine contains the nested loop across all inner points,
and calls the corresponding kernel function `count_neighbour_code`.
It adds the parameters `i` and `j` for the element to use, and
passes the plain 2d-Fortran arrays (and not the full fields) to
the kernel by accessing the `%data` member of the field parameters.

If everything is done correctly, you can compile this example using:

    make

and a binary `gol` will be created. You can run this with:

    ./gol ./config.glider

This is a small configuration with just 20 time steps, and the time
stepping loop will display the grid after each step:

    1.0.0.0.0.0.0.0.0.0.
    0.1.1.0.0.0.0.0.0.0.
    1.1.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    
    ...
    
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.1.0.0.0.0.
    0.0.0.0.0.0.1.1.0.0.
    0.0.0.0.0.1.1.0.0.0.
    0.0.0.0.0.0.0.0.0.0.
    0.0.0.0.0.0.0.0.0.0.

You can see that the glider configuration is moving to the bottom
right. To test if your implementation is correct, you can use

     make test

This will run the small test, and compare the output of each time step
with a known-good outcome contained in the file `glider.correct`:

    ./gol config.glider | tail -n 12 | diff - ./glider.correct

If no output is shown the results are correct, otherwise you will
see the output from diff, e.g.:

    9c9
    < 0.0.0.0.0.1.1.0.0.0.
    ---
    > 0.0.0.0.1.1.1.0.0.0.

If you measure the performance of this code, it will run significant
slower than the original non-PSyclone version, especially if you are
using gfortran. We will look at the performance in the next tutorial.

## Compacting the invoke calls
PSyclone can call more than one kernel in a invoke statement by just
specifying more than one kernel call, each separated by commas:

    call invoke(count_neighbours(neighbours, current),   &
                compute_born(born, current, neighbours), ...

Modify your `time_step_alg_mod.x90` file to use only one
invoke statement, and call all four kernels. Then run PSyclone
again, and look at the created algorithm file `time_step_alg_mod.f90`.

Combining several kernel calls into one invoke means that the
loops are all in the same subroutine, which will allow code
optimisations both automatically by the compiler, as well as using
PSyclone.

## Performance Check
Run the large configuration (which is a 1000x1000 grid with 2000
time steps), and compare the performance with the previous version
that did not combine the calls, and with the original non-PSyclone
version of the code.


