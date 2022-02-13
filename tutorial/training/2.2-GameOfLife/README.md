# Introduction to ``dl_esm_inf``

This tutorial introduces the infrastructure library
``dl_esm_inf``, which is used by the PSyclone
GOcean API. As example we will implement Conway's
'Game of Life'. This directory contains templates for
the functions, but you need to add code to various files.
Work through this README, which will guide you through
all required changes.

## Recap Game of Life
Game of Life is a cellular automaton developed by John
Conway in 1970. It is a set of four simple rules which
determine how cells develop on a simple 2d grid.
Given is an initial state defining which cells in the
grid are alive. Then the following rules are applied:

1. If a live cell has less than 2 live
   neighbours, it dies (underpopulation).
2. If a live cell has more than 3 live
   neighbours, it dies (overpopulation).
3. Any dead cell with exactly three live neighbours 
   becomes a live cell (reproduction).
4. Any life or dead cell for which none of the
   previous rules applies, will stay the way it is
   (i.e. either live or dead).

Neighbour includes the diagonals, so a cell has eight
neighbours.

There is a huge variety of cellular automatons, some of
which are easy to analyse, some of which are more complex.
The _Game of Life_ has been shown to be a _Universal Turing
Machine_ (UTM) meaning that you can create initial conditions
that allows you to do any computations just by following
these rules (see the wikipedia entries for
[Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) and
[UTM](https://en.wikipedia.org/wiki/Universal_Turing_machine) for
precise definitions).


## Implementation
In the spirit of numerical analysis the grid is represented
by a 2d double precision array (main reason for using double
precision is that the infrastructure library at this stage
only provides double precision fields). Each live cell is
stored as a '1', each dead cell as a '0'. We use the following
algorithm:

1. Compute a field ``neighbour`` which stores the number of live
   neighbours for each cell. Note that the halo is used to store
   the boundary conditions.
2. Compute a field ``die`` based on the ``current`` cell state
   and number of neighbours, and stores a '1' for each live cell
   that dies due to under- or over-population.
3. Compute a field ``born`` based on the ``current`` cell state
   and number of neighbours, which stores a '1' for each dead
   cell that will create a new cell due to reproduction.
3. Update the ``current`` state by adding the ``born`` field
   and subtracting the ``die`` field.

This is then repeated for the number of required time steps.

A configuration file is used to store the initial configuration. It
has the following format (see ``config.glider``):

    # 10x10 grid, 20 time steps
    10 10 20
    1 1
    1 3

The first line is a comment line and is always ignored. The second
line contains the number of columns, number of rows, and number
of steps. This is then followed by a list of coordinates of cells
that are alive - column row format.

The following sections describes the required changes in the files.
The subdirectory ``solution`` contains 

### Reading in the configuration: ``read_config.f90``
This subroutines takes the name of the configuration file to use
from the first and only command line parameter. It will read
the grid size from the configuration file. The grid size is then
used to initialise the ``dl_esm_inf`` grid object. This happens
in three steps, which you will need to add:
1. Initialise the parallelisation subsystem with a call to 
   ``parallel_init``, which needs to be imported from 
   ``parallel_mod``. This call takes no parameter (domain
   decomposition etc is defined later).
2. Create an Arakawa-C grid by constructing the grid object
   with the ``grid_type`` constructor. The constructor is
   imported from ``grid_mod`` (together with ``grid_init``, see
   below). Use the following import line:
   ```Fortran
        USE grid_mod, only : grid_type, grid_init, GO_ARAKAWA_C,     &
                             GO_BC_PERIODIC, GO_BC_NONE, GO_OFFSET_SW
   ```
   The ``grid_type`` constructors takes three parameters: the first
   one the grid type (``GO_ARAKAWA_C``, the only one currently supported
   by ``dl_esm_inf`` and PSyclone), a 3d array indicating the
   boundary conditions for each dimension - use ``GO_BC_PERIODIC``
   (all other boundary conditions require a T-mask), and the offset
   used (see *******, use ``GO_OFFSET_SW``)

3. Now the domain decomposition can be specified. Call the ``decompose``
   method of the grid object constructed in the previous step. Provide
   the number of points in x- and y-direction. For now we are not using
   MPI, so just provide ``ndomains=1`` as parameter, and ``halo_width=1``
   (TODO halo is used to store BC????)

4. Finally the grid initialisation can be called, providing the
   distance between grid points. We do not need this information,
   so just call ``grid_init`` with the grid parameter, and
   ``dxarg=1.0, dy_arg=1.0``.

Once the grid has been properly created and initialised, it can be
used to create fields, based on using T-points (TODO):

```Fortran
    initial = r2d_field(grid, GO_T_POINTS)
```


### The main program: ``gol.f90``
As is obvious from the previous step, the main program does not have to do
much with ``dl_esm_inf`` - it just passes the grid that is initialised
in ``read_config.f90`` to the function ``time_step.f90``. So there are only
two steps to do in this file:

1. Import ``grid_type`` and ``r2d_field`` (see above for details if required).
2. Declare a variable called ``grid`` and ``initial`` (which stores the initial
   condition read in from the configuration file). Note that the ``grid`` must
   be declared with the Fortran ``target`` attribute, since each field will
   store a pointer to the grid.

### Time-stepping: ``time_step_mod.f90``
The time stepping function receives the initial state of the cells from as
parameters, and uses it as current state. Then inside of the time stepping
loop it calls functions to:
1. Count neighbours based on current state
2. Computes newly born cells based on number of neighbours and current state
   (the latter is required since only empty cells can be born).
3. Computes dying cells based on number of neighbours and current state (again
   the current state is required since only live cells can die).
4. Computes the new state based on the current state, newly born cells and
   dying cells.

There is only a small change required here:
1. Import the ``r2d_field`` and ``go_t_points`` from ``field_mod``.
2. Declare three variables ``neighbours``, ``die``, ``born`` as ``r2d_fields``.
3. Construct these three variables as ``go_t_points``.

### Compute dying cells: ``compute_die_mod.f90``
This functions computes which cells will die. Live cells can die either because
they do not have enough neighbours (i.e. less than 2 - underpopulation), or
because they have too many neighbours (more than 3 - overpopulation). The function
already contains code to extract the loop boundaries from the grid object
(see ``xstart``, ``xstop`` etc), and the nested loop structure which iterates
over all cells.
1. Add code that tests if a cell is live. You get access to the field data by
   using ``field%data(...)``, so in order to test if a cell is alive you
   use ``if (current%data(i,j)>0.0)``. If a cell is not live, it obviously
   cannot die, so you must set the output ``die`` to 0 (since it might still
   have a value from a previous iteration).
2. If a cell is live, you need to test if it has less than 2 or more than 3
   neighbours using the ``neighbours`` field: ``if ( neighbours%data(i,j)...``.

### Remaining functions
The remaining functions are already completed for you, but it would be useful
to check their implementation as well. They closely follow the design of
``compute_die_mod.f90`` above: take the loop bounds from the grid, have
a double nested loop to handle all cells, and then compute the output. The
``combine_mod.f90`` function adds the newly born cells to the current state,
and subtracts the number of dying cells to compute the new state.

### Running the program
You can run the program using ``gol ./config.glider``. This will print
the current state to the screen. The configuration is a so called glider,
a structure that moves continuously to the lower right. The last state
looks like this:
```
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.1.0.0.0.0.0.0.
0.0.0.0.0.0.0.1.1.0.0.0.0.
0.0.0.0.0.0.1.1.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
0.0.0.0.0.0.0.0.0.0.0.0.0.
```
There is also a handy makefile target:
``make run``, which will compile and then run the glider example.

### Summary
This tutorial showed how to initialise ``dl_esm_inf``, create fields on
the grid, and do kernel computations with the grid.
