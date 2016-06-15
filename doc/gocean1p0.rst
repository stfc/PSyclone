.. _gocean1.0-api:

GOcean1.0 API
=============

.. highlight:: fortran

.. _gocean1.0-intro:

Introduction
------------

The GOcean 1.0 application programming interface (API) was originally
designed to support ocean models that use the finite-difference scheme
for two-dimensional domains.  However, the approach is not specific to
ocean models and can potentially be applied to any finite-difference
code.

As with all PSyclone API's, the GOcean 1.0 API specifies how a user
must write the Algorithm Layer and the Kernel Layer to allow PSyclone
to generate the PSy Layer. These Algorithm and Kernel API's are
discussed separately in the sections below. Before these we
describe the functionality provided by the GOcean Library.

.. _gocean1.0-library:

The GOcean Library
------------------

The use of PSyclone and the GOcean 1.0 API implies the use of a
standard set of data types and associated infrastructure. This is
provided by version 1.0 of the GOcean Library (GOLib v.1.0).
Currently this library is distributed separately from PSyclone and is
available from http://puma.nerc.ac.uk/trac/GOcean.

.. _gocean1.0-grid:

Grid
++++

The GOLib contains a ``grid_mod`` module which defines a ``grid_type``
and associated constructor:

::

  use grid_mod
  ...
  !> The grid on which our fields are defined
  type(grid_type), target :: model_grid
  ...
  ! Create the model grid
  model_grid = grid_type(ARAKAWA_C,                           &
                         (/BC_EXTERNAL,BC_EXTERNAL,BC_NONE/), &
                         OFFSET_NE)

.. note::
  The grid object itself must be declared with the ``target``
  attribute. This is because each field object will contain a pointer to
  it.

The ``grid_type`` constructor takes three arguments:

 1. The type of grid (only ARAKAWA_C is currently supported)
 2. The boundary conditions on the domain for the *x*, *y* and *z* dimensions (see below). The value for the *z* dimension is currently ignored.
 3. The 'index offset' - the convention used for indexing into offset fields.

Three types of boundary condition are currently supported:

.. tabularcolumns:: |l|L|

============  =========================================
Name          Description
============  =========================================
BC_NONE       No boundary conditions are applied.
BC_EXTERNAL   Some external forcing is applied. This must be implemented by a kernel. The domain must be defined with a T-point mask (see :ref:`gocean1.0-grid-init`).
BC_PERIODIC   Periodic boundary conditions are applied.
============  =========================================

The infrastructure requires this information in order to determine the
extent of the model grid.

The index offset is required because a model (kernel) developer has
choice in how they actually implement the staggering of variables on a
grid. This comes down to a choice of which grid points in the vicinity
of a given T point have the same array (*i*, *j*) indices. In
the diagram below, the image on the left corresponds to choosing those
points to the South and West of a T point to have the same (*i*, *j*)
index. That on the right corresponds to choosing those points to the
North and East of the T point (this is the offset scheme used in the
NEMO ocean model):

.. image:: grid_offset_choices.png

The GOcean 1.0 API supports these two different offset schemes, which
we term ``OFFSET_SW`` and ``OFFSET_NE``.

Note that the constructor does not specify the extent of the model
grid. This is because this information is normally obtained by reading
a file (a namelist file, a netcdf file etc.) which is specific to an
application.  Once this information has been obtained, a second
routine, ``grid_init``, is provided with which to 'load' a grid object
with state. This is discussed below.

.. _gocean1.0-grid-init:

The ``grid_init`` Routine
#########################

Once an application has determined the details of the model
configuration, it must use this information to populate the grid
object. This is done via a call to the ``grid_init`` subroutine:

::

  subroutine grid_init(grid, m, n, dxarg, dyarg, tmask)
    !> The grid object to configure
    type(grid_type), intent(inout) :: grid
    !> Dimensions of the model grid
    integer,         intent(in)    :: m, n
    !> The (constant) grid spacing in x and y (m)
    real(wp),        intent(in)    :: dxarg, dyarg
    !> Optional T-point mask specifying whether each grid point is
    !! wet (1), dry (0) or external (-1).
    integer, dimension(m,n), intent(in), optional :: tmask

If no T-mask is supplied then this routine configures the grid
appropriately for an all-wet domain with periodic boundary conditions
in both the *x*- and *y*-dimensions. It should also be noted that
currently only grids with constant resolution in *x* and *y* are
supported by this routine.

.. _gocean1.0-fields:

Fields
++++++

Once a model has a grid defined it will require one or more
fields. The GOLib contains a ``field_mod`` module which defines an
``r2d_field`` type (real, 2-dimensional field) and associated
constructor:

::

  use field_mod
  ...
  !> Current ('now') sea-surface height at different grid points
  type(r2d_field) :: sshn_u_fld, sshn_v_fld, sshn_t_fld
  ...

  ! Sea-surface height now (current time step)
  sshn_u = r2d_field(model_grid, U_POINTS)
  sshn_v = r2d_field(model_grid, V_POINTS)
  sshn_t = r2d_field(model_grid, T_POINTS)

The constructor takes two arguments:

 1. The grid on which the field exists
 2. The type of grid point at which the field is defined
    (``U_POINTS``, ``V_POINTS``, ``T_POINTS`` or ``F_POINTS``)

Note that the grid object need not have been fully configured (by a
call to ``grid_init`` for instance) before it is passed into this
constructor.


Example
+++++++

PSyclone is distributed with a full example of the use of the
GOcean Library. See ``<PSYCLONEHOME>/examples/gocean/shallow_alg.f90``.  In what
follows we will walk through a slightly cut-down example for a
different program.

The following code illustrates the use of the GOLib in constructing an
application:

::
   
   program gocean2d
     use grid_mod  ! From GOLib
     use field_mod ! From GOLib
     use model_mod
     use boundary_conditions_mod

     !> The grid on which our fields are defined. Must have the 'target'
     !! attribute because each field object contains a pointer to it.
     type(grid_type), target :: model_grid

     !> Current ('now') velocity component fields
     type(r2d_field) :: un_fld, vn_fld
     !> 'After' velocity component fields
     type(r2d_field) :: ua_fld, va_fld
     ...

     ! time stepping index
     integer :: istp 

     ! Create the model grid. We use a NE offset (i.e. the U, V and F
     ! points immediately to the North and East of a T point all have the
     ! same i,j index).  This is the same offset scheme as used by NEMO.
     model_grid = grid_type(ARAKAWA_C,                          &
                           (/BC_EXTERNAL,BC_EXTERNAL,BC_NONE/), &
                            OFFSET_NE)

     !! read in model parameters and configure the model grid 
     CALL model_init(model_grid)

     ! Create fields on this grid

     ! Velocity components now (current time step)
     un_fld = r2d_field(model_grid, U_POINTS)
     vn_fld = r2d_field(model_grid, V_POINTS)

     ! Velocity components 'after' (next time step)
     ua_fld = r2d_field(model_grid, U_POINTS)
     va_fld = r2d_field(model_grid, V_POINTS)

     ...
     
     !! time stepping 
     do istp = nit000, nitend, 1

       call step(istp,                               &
                 ua_fld, va_fld, un_fld, vn_fld,     &
                 ...)
     end do
     ...
   end program gocean2d

The ``model_init`` routine is application specific since it must
determine details of the model configuration being run, *e.g.* by
reading a namelist file. An example might look something like:

::

   subroutine model_init(grid)
     type(grid_type), intent(inout) :: grid

     !> Problem size, read from namelist
     integer :: jpiglo, jpjglo
     real(wp) :: dx, dy
     integer, dimension(:,:), allocatable :: tmask

     ! Read model configuration from namelist
     call read_namelist(jpiglo, jpjglo, dx, dy, &
                        nit000, nitend, irecord, &
                        jphgr_msh, dep_const, rdt, cbfr, visc)

     ! Set-up the T mask. This defines the model domain.
     allocate(tmask(jpiglo,jpjglo))

     call setup_tpoints_mask(jpiglo, jpjglo, tmask)

     ! Having specified the T points mask, we can set up mesh parameters
     call grid_init(grid, jpiglo, jpjglo, dx, dy, tmask)

     ! Clean-up. T-mask has been copied into the grid object.
     deallocate(tmask)

   end subroutine model_init

Here, only ``grid_type`` and the ``grid_init`` routine come from the
GOLib. The remaining code is all application specific.

Once the grid object is fully configured and all fields have been
constructed, a simulation will proceed by performing calculations with
those fields.  In the example program given above, this calculation is
performed in the time-stepping loop within the ``step``
subroutine. The way in which this routine uses Invoke calls is
described in the :ref:`gocean1.0-invokes` Section.


.. _gocean1.0-api-algorithm:

Algorithm
---------

The Algorithm is the top-level specification of the natural science
implemented in the software. Essentially it consists of mesh setup,
field declarations, initialisation of fields and (a series of) Kernel
calls. Infrastructure to support these tasks is provided in
version 1.0 of the GOcean library (see :ref:`gocean1.0-library`).

.. _gocean1.0-invokes:

Invokes
+++++++

The Kernels to call are specified through the use of Invokes, e.g.:

::

  call invoke( kernel1(field1, field2),                      &
               kernel2(field1, field3)                       &
             )

The location and number of these ``call invoke(...)`` statements
within the source code is entirely up to the user. The only
requirement is that PSyclone must be run on every source file that
contains one or more Invokes.
The body of each Invoke specifies the kernels to be called, the order
in which they are to be applied and the fields (and scalars) that they
work with.

Note that the kernel names specified in an Invoke are the names of the
corresponding kernel *types* defined in the kernel meta-data (see the
:ref:`gocean1.0-kernels` Section). These are not the same as the names
of the Fortran subroutines which contain the actual kernel code.
The kernel arguments are typically field objects, as described in the
:ref:`gocean1.0-fields` Section, but they may also be scalar
quantities (real or integer).

In the example ``gocean2d`` program shown earlier, there is only one
Invoke call and it is contained within the ``step`` subroutine:


::

   subroutine step(istp,                   &
                   ua, va, un, vn,         &
                   sshn_t, sshn_u, sshn_v, &
                   ssha_t, ssha_u, ssha_v, &
                   hu, hv, ht)
     use kind_params_mod  ! From GOLib
     use grid_mod         ! From GOLib
     use field_mod        ! From GOLib
     use model_mod, only: rdt ! The model time-step
     use continuity_mod,  only: continuity
     use momentum_mod,    only: momentum_u, momentum_v
     use boundary_conditions_mod, only: bc_ssh, bc_solid_u
     !> The current time step
     integer,         intent(inout) :: istp
     type(r2d_field), intent(inout) :: un, vn, sshn_t, sshn_u, sshn_v
     type(r2d_field), intent(inout) :: ua, va, ssha_t, ssha_u, ssha_v
     type(r2d_field), intent(inout) :: hu, hv, ht

     call invoke(                                               &
                 continuity(ssha_t, sshn_t, sshn_u, sshn_v,     &
                            hu, hv, un, vn, rdt),               &
                 momentum_u(ua, un, vn, hu, hv, ht,             &
                            ssha_u, sshn_t, sshn_u, sshn_v),    &
                 momentum_v(va, un, vn, hu, hv, ht,             &
                            ssha_v, sshn_t, sshn_u, sshn_v),    &
                 bc_ssh(istp, ssha_t),                          &
                 bc_solid_u(ua),                                &
                 ...
                )
   end subroutine step

Note that in this example the grid was constructed for a
model with 'external' boundary conditions. These boundary conditions
are applied through several user-supplied kernels, two of which
(``bc_ssh`` and ``bc_solid_u``) are include in the above code
fragment.

.. _gocean1.0-kernels:

Kernel
-------

The general requirements for the structure of a Kernel are explained
in the :ref:`kernel-layer` section. This section explains the meta-data
and subroutine arguments that are specific to the GOcean 1.0 API.

Metadata
++++++++

The meta-data for a GOcean 1.0 API kernel has four components:

 1) 'meta_args',
 2) 'iterates_over',
 3) 'index_offset' and
 4) 'procedure':

These are illustrated in the code below:

::

  type, extends(kernel_type) :: my_kernel_type
     type(arg), dimension(...) :: meta_args = (/ ... /)
     integer :: iterates_over = ...
     integer :: index_offset = ...
  contains
    procedure, nopass :: code => my_kernel_code
  end type my_kernel_type

These four meta-data elements are discussed in order in the following
sections.

meta_args
#########

The ``meta_args`` array specifies information about data that the
kernel code expects to be passed to it via its argument list. There is
one entry in the ``meta_args`` array for each **scalar**, **field**,
or **grid-property** passed into the Kernel. Their ordering in the
``meta_args`` array must be the same as that in the kernel code
argument list. The entry must be of type ``arg`` which itself contains
metadata about the associated argument. The size of the meta_args
array must correspond to the total number of **scalars**, **fields**
and **grid properties** passed into the Kernel.

For example, if there are a total of two **field** entities being passed
to the Kernel then the meta_args array will be of size 2 and there
will be two entries of type ``arg``:

::

  type(arg) :: meta_args(2) = (/                                  &
       arg( ... ),                                                &
       arg( ... )                                                 &
       /)

Argument-metadata (metadata contained within the brackets of an
``arg`` entry), describes either a **scalar**, a **field** or a **grid
property**.

The first argument-metadata entry describes how the kernel will access
the corresponding argument. As an example, the following ``meta_args``
metadata describes four entries, the first one is written to by the
kernel while the remaining three are only read.

::

  type(arg) :: meta_args(4) = (/                            &
       arg(WRITE, ... ),                                    &
       arg(READ, ... ),                                     &
       arg(READ, ... ),                                     &
       arg(READ, ...)                                       &
       /)

The second entry to argument-metadata (information contained within
the brackets of an ``arg`` type) describes the type of data
represented by the argument. This type falls into three categories;
field data, scalar data and grid properties. For field data the
meta-data entry consists of the type of grid-point that field values
are defined on. Since the GOcean API supports fields on an Arakawa C
grid, the possible grid-point types are ``CU``, ``CV``, ``CF`` and
``CT``. GOcean Kernels can also take scalar quantities as
arguments. Since these do not live on grid-points they are specified
as either ``R_SCALAR`` or ``I_SCALAR`` depending on whether the
corresponding Fortran variable is a real or integer quantity.
Finally, grid-property entries are used to specify any properties of
the grid required by the kernel (*e.g.* the area of cells at U points or
whether T points are wet or dry).

For example:

::

  type(arg) :: meta_args(4) = (/                            &
       arg(WRITE, CT, ... ),                                &
       arg(READ,  CU, ... ),                                &
       arg(READ,  R_SCALAR, ... ),                          &
       arg(READ,  GRID_AREA_U)                              &
       /)

Here, the first argument is a field on T points, the second is a field
on U points, the fourth is a real scalar and the fifth is a property
of the grid (cell area at U points).

The full list of supported grid properties in the GOcean 1.0 API is:

.. _gocean1.0-grid-props:

=============   =============================  ====================
Name            Description                    Type
=============   =============================  ====================
grid_area_t     Cell area at T point           Real array, rank=2
grid_area_u     Cell area at U point           Real array, rank=2
grid_area_v     Cell area at V point           Real array, rank=2
grid_mask_t     T-point mask (1=wet, 0=dry)    Integer array, rank=2
grid_dx_t       Grid spacing in x at T points  Real array, rank=2
grid_dx_u       Grid spacing in x at U points  Real array, rank=2
grid_dx_v       Grid spacing in x at V points  Real array, rank=2
grid_dy_t       Grid spacing in y at T points  Real array, rank=2
grid_dy_u       Grid spacing in y at U points  Real array, rank=2
grid_dy_v       Grid spacing in y at V points  Real array, rank=2
grid_lat_u      Latitude of U points (gphiu)   Real array, rank=2
grid_lat_v      Latitude of V points (gphiv)   Real array, rank=2
grid_dx_const   Grid spacing in x if constant  Real, scalar
grid_dy_const   Grid spacing in y if constant  Real, scalar
=============   =============================  ====================

These are stored in a dictionary named ``GRID_PROPERTY_DICT`` at the
top of the ``gocean1p0.py`` file. All of the rank-two arrays have the
first rank as longitude (*x*) and the second as latitude (*y*).

For scalar and field arguments the argument meta-data contains a third
argument which must be 'POINTWISE'. This is not currently used in this
version of the GOcean API. For grid-property arguments there is no
third meta-data argument. Therefore, the full argument meta-data for
our previous example will be:

::

  type(arg) :: meta_args(4) = (/                            &
       arg(WRITE, CT,       POINTWISE),                     &
       arg(READ,  CU,       POINTWISE),                     &
       arg(READ,  R_SCALAR, POINTWISE),                     &
       arg(READ,  GRID_AREA_U)                              &
       /)

Iterates Over
#############

The second element of kernel meta-data is ``ITERATES_OVER``. This
specifies that the Kernel has been written with the assumption that it
is iterating over grid points of the specified type. The supported
values are: ``INTERNAL_PTS``, ``EXTERNAL_PTS`` and ``ALL_PTS``. These
may be understood by considering the following diagram of an example
model configuration:

.. image:: grids_SW_stagger.png

``INTERNAL_PTS`` are then those points that are within the Model
domain (fuscia box), ``EXTERNAL_PTS`` are those outside the domain and
``ALL_PTS`` encompasses all grid points in the model. The chosen value
is specified in the kernel-meta data like so:

::

  integer :: iterates_over = INTERNAL_PTS

Index Offset
############

The third element of kernel meta-data, ``INDEX_OFFSET``, specifies the
index-offset that the kernel uses. This is the same quantity as
supplied to the grid constructor (see the :ref:`gocean1.0-grid`
Section for a description).

The GOcean 1.0 API supports two different offset schemes;
``OFFSET_NE``, ``OFFSET_SW``. The scheme used by a kernel is specified
in the meta-data as, e.g.:

::

  integer :: index_offset = OFFSET_NE

Currently all kernels used in an application must use the same offset
scheme which must also be the same as passed to the grid constructor.


Procedure
#########

The fourth and final type of meta-data is ``procedure`` meta-data. This
specifies the name of the Kernel Fortran subroutine that this meta-data
describes.

For example:

::

  procedure :: my_kernel_code

Subroutine
++++++++++

Rules
#####

Kernel arguments follow a set of rules which have been specified for
the GOcean 1.0 API. These rules are encoded in the ``gen_code()``
method of the ``GOKern`` class in the ``gocean1p0.py`` file. The
rules, along with PSyclone's naming conventions, are:

1) Every kernel has the indices of the current grid point as the first two arguments, ``i`` and ``j``. These are integers and have intent ``in``.

2) For each field/scalar/grid property in the order specified by the meta_args metadata:

    1) For a field; the field array itself. A field array is a real array of kind ``wp`` and rank two. The first rank is longitude (*x*) and the second latitude (*y*).
    2) For a scalar; the variable itself. A real scalar is of kind ``wp``.
    3) For a grid property; the array or variable (see the earlier table) containing the specified property.

.. note::
   Grid properties are not passed from the Algorithm
   Layer. PSyclone generates the necessary lookups in the PSy Layer
   and includes the resulting references in the arguments passed to
   the kernel.

As an example, consider the ``bc_solid_u`` kernel that is used in the
``gocean2d`` program shown earlier. The meta-data for this kernel is:

::

   type, extends(kernel_type) :: bc_solid_u
     type(arg), dimension(2) :: meta_args =  &
          (/ arg(WRITE, CU, POINTWISE),      &
             arg(READ,      GRID_MASK_T)     &
           /)

     !> This is a boundary-conditions kernel and therefore
     !! acts on all points of the domain rather than just
     !! those that are internal
     integer :: ITERATES_OVER = ALL_PTS

     integer :: index_offset = OFFSET_NE

  contains
    procedure, nopass :: code => bc_solid_u_code
  end type bc_solid_u

The interface to the subroutine containing the implementation of this
kernel is:

::
   
  subroutine bc_solid_u_code(ji, jj, ua, tmask)
    integer,                  intent(in)    :: ji, jj
    integer,  dimension(:,:), intent(in)    :: tmask
    real(wp), dimension(:,:), intent(inout) :: ua

As described above, the first two arguments to this subroutine specify
the grid-point at which the computation is to be performed. The third
argument is the field that this kernel updates and the fourth argument
is the T-point mask. The latter is a property of the grid and is
provided to the kernel call from the PSy Layer.

Comparing this interface definition with the use of the kernel in the
Invoke call:
::

   call invoke ( ...,            &
                 bc_solid_u(ua), &
		 ... )

we see that in the Algorithm Layer the user need only provide the
field(s) (and possibly scalars) that a kernel operates on. The index
of the grid point and any grid properties are provided in the
(generated) PSy Layer where the kernel subroutine proper is called.

Built-ins
---------

The GOcean 1.0 API does not support any built-in operations.

Conventions
-----------

There is a convention in the GOcean 1.0 API kernel code that if the
name of the operation being performed is ``<name>`` then a kernel file
is ``<name>_mod.[fF90]``, the name of the module inside the kernel
file is ``<name>_mod``, the name of the kernel metadata in the module
is ``<name>_type`` and the name of the kernel subroutine in the module
is ``<name>_code``. PSyclone does not require this convention to be
followed in the GOcean 1.0 API.

The contents of the metadata is also usually declared private but this
does not affect PSyclone.

Finally, the ``procedure`` metadata (located within the kernel
metadata) usually has ``nopass`` specified but again this is ignored
by PSyclone.

Transformations
---------------

.. highlight:: python

In this section we describe the transformations that are specific to
the GOcean 1.0 API. For an overview of transformations in general see
:ref:`transformations`.

.. autoclass:: transformations.GOceanLoopFuseTrans
   :members:
   :noindex:

.. autoclass:: transformations.GOceanOMPParallelLoopTrans
   :members:
   :noindex:

.. autoclass:: transformations.GOceanOMPLoopTrans
   :members:
   :noindex:

.. autoclass:: transformations.GOConstLoopBoundsTrans
   :members:
   :noindex:

