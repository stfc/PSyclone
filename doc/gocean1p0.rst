.. _gocean1.0-api:

GOcean1.0 API
=============

This section describes the GOcean 1.0 application programming interface
(API). This section explains what a user needs to write in order to make
use of the GOcean 1.0 API in PSyclone.

As with all PSyclone API's, the GOcean 1.0 API specifies how a user
must write the Algorithm Layer and the Kernel Layer to allow
PSyclone to generate the PSy Layer. These Algorithm and Kernel API's
are discussed separately in the following sections.

.. _gocean1.0-api-algorithm:

Algorithm
---------

The Algorithm is the top-level specification of the natural science
implemented in the software. Essentially it consists of mesh setup,
field declarations, initialisation of fields and (a series of) Kernel
calls. Infrastructure to support some of these tasks is provided in
the GOcean 1.0 library (GOLib v.1.0).

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

The ``grid_type`` constructor takes three arguments:

 1. The type of grid (only ARAKAWA_C is currently supported)
 2. The boundary conditions on the domain
 3. The 'index offset' - the convention used for indexing into offset fields.

The index offset is required because a model (kernel) developer has
choice in how they actually implement the staggering of variables on a
grid. This comes down to a choice of which grid points in the vicinity
of a given T point have the same array (*i*, *j*) array indices. In
the diagram below, the image on the left corresponds to choosing those
points to the South and West of a T point to have the same (*i*, *j*)
index. That on the right corresponds to choosing those points to the
North and East of the T point (this is the offset scheme used in the
NEMO ocean model):

.. _gocean1.0-offset-image:

.. image:: grid_offset_choices.pdf

The GOcean 1.0 API supports these two different offset schemes, which
we term ``OFFSET_SW`` and ``OFFSET_NE``.

The constructor does not specify the extent of the model grid. This is
because this information is normally obtained by reading a file (a
namelist file, a netcdf file etc.) which is specific to an
application.  Once this information has been obtained a second
routine, ``grid_init``, is provided with which to 'load' a grid object
with state:

::

  subroutine grid_init(grid, m, n, dxarg, dyarg, tmask)
    !> The grid object to configure
    type(grid_type), intent(inout) :: grid
    !> Dimensions of the model grid
    integer,         intent(in)    :: m, n
    !> The (constant) grid spacing in x and y
    real(wp),        intent(in)    :: dxarg, dyarg
    !> Optional T-point mask specifying wet (1) and dry (0) points
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

.. _gocean1.0-invokes:

Invokes
+++++++

The Kernels to call are specified through the use of Invokes, e.g.:

::

  call invoke(                                               &
              bc_flather_u(ua, hu, sshn_u),                  &
              bc_flather_v(va, hv, sshn_v),                  &
              copy(un, ua),                                  &
              copy(vn, va),                                  &
              copy(sshn_t, ssha_t),                          &
              next_sshu(sshn_u, sshn_t),                     &
              next_sshv(sshn_v, sshn_t)                      &
             )

The location and number of these ``call invoke(...)`` statements
within the source code is entirely up to the user. The only
requirement is that PSyclone must be run on every source file that
contains one or more Invokes. Note that the kernel names specified in
an Invoke are the names of the corresponding kernel types defined in
the kernel meta-data (see :ref:`gocean1.0-kernels`). These are not the same as the names
of the Fortran subroutines which contain the actual kernel code.

The kernel arguments are typically field objects, as described in
:ref:`gocean1.0-fields`, but they may also be scalar quantities.

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

For example, if there are a total of 2 **field** entities being passed
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

whether the data that is
being passed is for a real scalar (``R_SCALAR``), integer scalar
(``I_SCALAR``), field (``GH_FIELD``) or an operator
(``GH_OPERATOR``). This information is mandatory.

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
the grid required by the kernel (e.g. the area of cells at U points or
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
on U points, the fourth is a real scalar and the fifth is a quantity
of the grid (cell area at U points).

The full list of supported grid-properties in the GOcean 1.0 API is:

=============   =============================  ==================
Name            Description                    Type
=============   =============================  ==================
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
top of the ``gocean1p0.py`` file.

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

.. image:: grids_SW_stagger.pdf

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
supplied to the grid constructor (see :ref:`gocean1.0-grid` for a
description).

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
the GPcean 1.0 API. These rules are encoded in the ``gen_code()``
method of the ``GOKern`` class in the ``gocean1p0.py`` file. The
rules, along with PSyclone's naming conventions, are:

1) Every kernel has the indices of the current grid point as the first two arguments ``i`` and ``j``. These are integers and have intent ``in``.

2) For each field/scalar/grid property in the order specified by the meta_args metadata:

    1) For a field; the field array itself. A field array is a real array of kind ``wp`` and rank two.
    2) For a scalar; the variable itself. A real scalar is of kind ``wp``.
    3) For a grid property; the array or variable (see the earlier table) containing the specified property.


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

.. note:: To be written.
