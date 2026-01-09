# Introduction to ``dl_esm_inf``

This tutorial introduces the infrastructure library
[``dl_esm_inf``](https://psyclone.readthedocs.io/en/latest/user_guide/gocean1p0.html#the-gocean-infrastructure-library-dl-esm-inf), 
which is used by the PSyclone
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
4. Any live or dead cell for which none of the
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
by a 2D double precision array (main reason for using double
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
has the following format (see ``../gol-lib/config.glider``):

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
in four steps:
1. Initialise the parallelisation subsystem with a call to 
   ``parallel_init``.
2. Create an Arakawa-C grid by constructing the grid object
   with the ``grid_type`` constructor.
   ```
   The ``grid_type`` constructors takes three parameters: the first
   one the grid type (``GO_ARAKAWA_C``, the only one currently supported
   by ``dl_esm_inf`` and PSyclone), a 3D array indicating the
   boundary conditions for each dimension - use ``GO_BC_EXTERNAL``, and
   the offset as ``GO_OFFSET_SW``

3. Now the domain decomposition can be specified. The ``decompose``
   method of the grid object constructed in the previous step can just take
   the number of domains, which is 1 in this case. Also use a halo region
   of 1, which will be used to store 0 and avoid tests when counting
   neighbours.

4. Finally the grid initialisation can be called, providing the
   distance between grid points. We do not need this information,
   so just call ``grid_init`` with the grid parameter, and
   ``dxarg=1.0, dy_arg=1.0``.

Once the grid has been properly created and initialised, we read in the
config file (``get_initial_state``), which will store the initial state
from the config file in the Fortran array ``initial_state``. This Fortran
array can then be used to initialise the actual field with this value.

While this could be combined in one step, this implementation will later
allow us to use distributed memory without additional change, since
the ``r2d_field`` constructor will take care of the data distribution.


### The main program: ``gol.f90``
As is obvious from the previous step, the main program does not have to do
much with ``dl_esm_inf`` - it just passes the grid that is initialised
in ``read_config.f90`` to the function ``time_step.f90``.

### Time-stepping: ``time_step_mod.f90``
The time stepping function receives the initial state of the cells as
parameters and uses it as current state. The function needs three additional
fields - one to count the neighbours, one to compute which cells are born,
and one for cells that are dying. After this, the field is printed if there
are 20 time steps or less (since larger and longer runs are meant for timing,
output is suppressed).

TODO:
1. Declare two fields ``die`` and ``born`` - similar to the way ``neighbours``
   is set up.
2. Pass the right fields as parameters to the four function called. You need
   to check with the code to find out the order (and if you are not sure
   which parameters are required).
3. Finish all four compute kernels.

### Compute dying cells: ``compute_die_mod.f90``
This functions computes which cells will die. Live cells can die either because
they do not have enough neighbours (i.e. less than 2 - underpopulation), or
because they have too many neighbours (more than 3 - overpopulation). The function
already contains code to extract the loop boundaries from the grid object
(see ``xstart``, ``xstop`` etc), and the nested loop structure which iterates
over all cells.
1. Add code that tests if a cell is alive. You get access to the field data by
   using ``field%data(...)``, so in order to test if a cell is alive you
   use ``if (current%data(i,j)>0.0)``. Note that ``die`` has to be initialised
   with 0, since it will contain values from a previous iteration.

2. If a cell is live, you need to test if it has less than 2 or more than 3
   neighbours using the ``neighbours`` field: ``if ( neighbours%data(i,j)...``.

### Remaining functions
The remaining functions need to be finished similarly. The template will
contain comments with details.

### Running the program
You can run the program using ``gol ../gol-lib/config.glider``. This will print
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
There are also two handy makefile targets:
1. ``make run``, which will compile and then run the glider example.
2. ``make test``, which will compile and run the example, and check that the
    final output is correct.

### Summary
This tutorial showed how to initialise ``dl_esm_inf``, create fields on
the grid, and do kernel computations with the grid.
