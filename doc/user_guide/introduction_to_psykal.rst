.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2018-2025, Science and Technology Facilities Council.
.. All rights reserved.
..
.. Redistribution and use in source and binary forms, with or without
.. modification, are permitted provided that the following conditions are met:
..
.. * Redistributions of source code must retain the above copyright notice, this
..   list of conditions and the following disclaimer.
..
.. * Redistributions in binary form must reproduce the above copyright notice,
..   this list of conditions and the following disclaimer in the documentation
..   and/or other materials provided with the distribution.
..
.. * Neither the name of the copyright holder nor the names of its
..   contributors may be used to endorse or promote products derived from
..   this software without specific prior written permission.
..
.. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
.. "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
.. LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
.. FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
.. COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
.. INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
.. BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
.. LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
.. CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.. LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
.. ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
.. POSSIBILITY OF SUCH DAMAGE.
.. -----------------------------------------------------------------------------
.. Written by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
.. Modified I. Kavcic, Met Office
.. Modified B. P. Kinoshita, NIWA, New Zealand. 

.. _introduction_to_psykal:

Introduction to PSyKAl
======================

PSyKAl is a kernel-based software architecture proposed in the `GungHo project
<https://www.metoffice.gov.uk/research/foundation/dynamics/next-generation>`_
to design Fortran-embedded domain-specific languages that provide a clear
separations of concerns between the science code and the optimisation/parallelisation
details of an application.
The model distinguishes between three layers: the Algorithm layer, the
Kernel layer and the Parallelisation System (PSy) layer; which together give
the model its name.

The Algorithm layer is responsible for providing a high-level description of the
algorithm that the scientist wants to run. This layer operates on full fields
and includes calls to kernels and built-ins.

The Kernel layer contains the actual implementation of the kernels as functors.
Each functor implements a method that operates on an individual section of the fields.
Depending on the DSL, these can be a single element, a vertical column, or a set of
vertical columns. The kernels also specify some metadata that allow the data
dependencies between kernels to be determined. Built-ins are similar to kernels but
are provided by the PSyclone infrastructure itself.

The PSy layer acts as a bridge between the algorithm and kernel layers. It
contains the code that is responsible for performing concurrent execution
of kernels, including the way in which the iteration space is traversed,
while respecting kernel dependencies. This layer can be tuned for
specific platforms such as multi-node, multi-core and GPGPUs architectures
without affecting the user-supplied Algorithm and Kernel layers.

Rather than requiring that the PSy layer be written manually, PSyclone uses
the provided Algorithm and Kernel implementations to generate an initial PSyIR
for the PSy-layer, optionally with distributed-memory parallelism.
This then can be programmatically optimised by applying PSyclone transformations
(e.g. kernel fusing, colouring, inlining, ...) to better fit the target architecture.

The rest of this section describes how to use the ``psyclone`` command to process PSyKAl
DSLs and how to implement each layer, providing examples for each of them.

.. _psykal_usage:

Usage
-----

To use PSyclone to process a PSyKAl algorithm file, the ``-api API_NAME`` parameter
must be provided. In addition, distributed memory can be switched on or off by 
using the ``-dm``/``--dist_mem`` or ``-nodm``/``--no_dist_mem`` flags. For PSyKAl
DSLs, the optional transformation script provided by the ``-s SCRIPT``
parameter will be applied to the PSyIR of the PSy-layer (but the scripts can also
contain instructions to transform the code of the kernels used within it).

For example, the following command will process an LFRic PSyKAl algorithm file,
generate a PSy-layer containing distributed-memory parallelism and then transform
it using the ``additional_optimisations.py`` script:

.. code-block:: bash

   psyclone -api lfric -dm -s additional_optimisations.py algorithm.f90 \
      -oalg algorithm_output.f90 -opsy psy_layer_output.f90

To date, there are two PSyKAl DSL implementations: the
:ref:`LFRic PSyKAl API <lfric-api>`, a mixed
finite-element DSL used to implement the next-generation UK Met Office
atmospheric model dynamical core; and the
:ref:`GOcean PSyKAl API <gocean-api>`, a finite difference ocean model
benchmark. For more details on these see the corresponding sections of
this User Guide.

.. highlight:: fortran

.. _algorithm-layer:

Algorithm layer
---------------

As mentioned in the Introduction, the Algorithm layer provides a high-level
description of the algorithm that the scientist would like to run, in terms
of invocations to Kernel and Built-in operations. It operates on full fields
and therefore it is not allowed to call the individual kernel executors nor
include any parallelisation calls or directives. Instead, the algorithm layer
uses the ``invoke`` subroutine, which takes as arguments one or more kernel
functor constructors (as a Fortran type constructor) and, optionally, a name
argument. The kernel functors, in turn, take as argument the full
fields they operate on and any other quantities specified in their metadata.

For example::

    ...
    call invoke(kernel1(arg1,arg2), kernel2(arg1, 3), name="Example_Invoke")
    ...

A complete application can consist of many algorithm files, each of them
containing as many ``invoke()`` calls as required. PSyclone is applied to
each individual algorithm layer file and must therefore be run multiple
times if multiple algorithm files exist in a project.

The algorithm developer is also able to reference more than one
Kernel/Built-in within an invoke call (as indicated in the previous example).
In fact this feature is encouraged for performance reasons.
**As a general guideline the developer should aim to use as few invokes
as possible, each with as many Kernel functors in them
as is possible**. The reason for this is that it allows for greater
freedom for optimisations in the PSy-layer as these
are limited to the contents of individual invokes - PSyclone
currently does not attempt to optimise the PSy layer over multiple
invoke calls.

As well as generating the PSy-layer code, PSyclone modifies the
Algorithm layer code, replacing ``invoke`` calls with calls to the
generated PSy-layer subroutine(s) (plus the associated ``use`` statements)
so that the algorithm code is compilable and linkable. For example,
the invoke above is translated into something like the following::

  ...
  use psy, only : invoke_example_invoke
  ...
  call invoke_example_invoke(arg1, arg2, 3)
  ...

The ``name`` argument in the invoke call is optional. If supplied it
must be a string literal. Labels are not case-sensitive and must be
valid Fortran names (e.g. ``name="compute(1)"`` is invalid).
The label is used to name the corresponding PSy-layer routine generated
by PSyclone in order to make debugging and profiling outputs more readable.
So, for the above example, the generated PSy-layer subroutine
will be named "invoke_example_invoke", otherwise a numeric index is given
(e.g. "invoke_0").
Each invoke label must be unique within an Algorithm source file.

Limitations
+++++++++++

There are limitations in the Fortran expressions that can be used inside
the invoke call kernel arguments. The current list of known restrictions
on the form of kernel arguments within an invoke is:

 * No arithmetic expressions (e.g. ``kernel_type(a+b)`` or ``kernel_type(-a)``)
 * No named (optional) arguments (e.g. ``kernel_type(fn(my_arg=a))``)

If you encounter any other limitations (or have a burning desire to use one
of the above forms) then please contact the PSyclone developers.


.. _kernel-layer:

Kernel layer
------------

In the PSyKAl model, the Kernel code operates on an individual element of
a field (such as a column of cells). The reason for doing this is that it
gives the PSy layer flexibility in choosing the iteration order and exploiting
the spatial domain parallelisation. The Kernel layer is not allowed to include
any calls or directives related to parallelisation and works on
raw Fortran arrays (to allow the compiler to optimise the code).
Since a Kernel is called over the spatial domain (by the PSy layer) it
must take at least one field or operator as an argument.

Kernels are implemented as Fortran Functors. Functors are objects that can be
treated as if they are functions. As such they have two main interfaces for
calling and providing arguments to them: the object constructor (used by the
Algorithm layer) and a method that executes the code of the functor (used by
the PSy-layer).

PSyKal applications accept one or more modules providing kernels, each of which
can contain one or more kernel functors. Each kernel functor provides a set of
meta-data attributes and a method with its implementation.

In the example below the module ``w3_solver_kernel_mod`` contains one kernel
named ``w3_solver_kernel_type`` and its individual element execution method in
subroutine ``w3_solver_code``.

The metadata is API-specific and describes the kernel iteration space and
dependencies, so that PSyclone can generate correct PSy-layer code (including
any necessary halo-exchanges if DM is enabled). The
metadata is provided by the kernel developer, who must guarantee its
correctness. The example below shows meta-data for the ``LFRic`` API::

  module w3_solver_kernel_mod

  ...

    type, public, extends(kernel_type) :: w3_solver_kernel_type
      private
      type(arg_type) :: meta_args(4) = (/                 &
           arg_type(GH_FIELD,   GH_REAL, GH_WRITE, W3),   &
           arg_type(GH_FIELD,   GH_REAL, GH_READ,  W3),   &
           arg_type(GH_FIELD*3, GH_REAL, GH_READ,  Wchi), &
           arg_type(GH_SCALAR,  GH_REAL, GH_READ)         &
           /)
      type(func_type) :: meta_funcs(2) = (/               &
           func_type(W3,   GH_BASIS),                     &
           func_type(Wchi, GH_DIFF_BASIS)                 &
           /)
      integer :: gh_shape = GH_QUADRATURE_XYoZ
      integer :: operates_on = CELL_COLUMN
    contains
      procedure, nopass :: solver_w3_code
    end type
  
  contains
  
    subroutine solver_w3_code(nlayers,                                         &
                              x, rhs,                                          &
                              chi_1, chi_2, chi_3, ascalar,                    &
                              ndf_w3, undf_w3, map_w3, w3_basis,               &
                              ndf_wchi, undf_wchi, map_wchi, wchi_diff_basis,  &
                              nqp_h, nqp_v, wqp_h, wqp_v)
      ...
    end subroutine solver_w3_code
  
  end module w3_solver_kernel_mod


Note that the executor method can also be declared as a module procedure
interface to provide alternative implementations (e.g. different precisions)
of the kernel code. These are selected as appropriate by the Fortran compiler,
depending on the precision of the fields being passed to them::

    type, extends(kernel_type) :: kernel1
      ... 
      type(...) :: meta_args(...) = (/ ... /) 
      ... 
      integer :: operates_on = ... 
      ... 
    end type kernel1

    interface ...
      module procedure ... 
    end interface   


.. _psykal-built-ins:

Built-ins
---------

Built-ins are operations which can be specified within an invoke call in
the algorithm layer but do not require an associated kernel implementation
because they are provided by the infrastructure.

These are useful for commonly-used operations, as they reduce the amount of
code that the PSyKAl project has to maintain. In addition, they offer
potential performance advantages as their implementation can completely
change for different architectures and the PSy layer is free to implement
these operations in whatever way it chooses.

The list of supported Built-ins is API-specific and therefore these are
described under the documentation of each API. In general, PSyclone will
need to know the types of the arguments being passed to any Built-ins.
Each API provides a Fortran file that contains the metadata for all
Built-in operations supported for that API.

.. note:: When a particular Built-in is used, the name of this
          Built-in should not be used for anything else within the
          same scope. For example, it is not valid to make use of a
          Built-in called ``setval_c`` and for its parent subroutine
          to also be called ``setval_c``. In this case PSyclone will
          raise an exception.

Example
+++++++

In the following algorithm-layer example from the LFRic API, the invoke call
includes a call to two Built-ins (``setval_c`` and ``X_divideby_Y``) and a
user-supplied kernel that operates on cell columns
(``matrix_vector_kernel_mm_type``).
The ``setval_c`` Built-in sets all values in the field ``Ax`` to ``1.0`` and
the ``X_divideby_Y`` Built-in divides values in the field ``rhs`` by their
equivalent (per degree of freedom) values in the field ``lumped_weight``
(see :ref:`supported LFRic API Built-ins <lfric-built-ins>`).
Notice that, unlike the kernel call, no ``use`` association is required for
the Built-ins since they are provided as part of the environment.
::

  module solver_mod
    ...
    use matrix_vector_mm_kernel_mod, only: matrix_vector_kernel_mm_type
    ...

    subroutine jacobi_solver_algorithm(lhs, rhs, mm, mesh, n_iter)

      integer(kind=i_def), intent(in)    :: n_iter
      type(field_type),    intent(inout) :: lhs
      type(field_type),    intent(in)    :: rhs
      type(operator_type), intent(in)    :: mm
      type(mesh_type),     intent(in)    :: mesh
      type(field_type)                   :: Ax, lumped_weight

      ...

      ! Compute mass lump
      call invoke( name = "Jacobi_mass_lump",                           &
                   setval_c(Ax, 1.0_r_def),                             &
                   matrix_vector_kernel_mm_type(lumped_weight, Ax, mm), &
                   X_divideby_Y(lhs, rhs, lumped_weight) )

    end subroutine jacobi_solver_algorithm
    ...

  end module solver_mod


.. _PSy-layer:

PSy layer
---------

In the PSyKAl model, the PSy layer is the bridge between the Algorithm
full-field operations and the Kernel/Built-Ins individual element
operations. As such, it is responsible for:

1. calling any Kernel and expanding any Built-In so that they iterate over
   their specified iteration space;
2. map the Kernel and Built-In arguments supplied by an Algorithm ``invoke``
   call to the arguments required by a Built-in or Kernel method;
3. include any required distributed-memory operations such as halo swaps
   and reductions to guarantee the correctness of the code;
4. providing an entry point for the optimisation expert to provide
   additional optimisations for the target architecture.

The PSy layer can be written manually but this is error prone and
potentially complex to optimise. Therefore, the PSyclone code-generation
system automatically generates an initial version of the PSy layer by
parsing the associated Algorithm file and each of the kernels used in it.
This initial version of the PSy-layer can be further tuned with the PSyclone
code-transformation capabilities by providing a transformation script.
Each API comes with a set of specialised transformations designed for that API.

Also, in addition to all the functionality available for PSyIR Nodes, the
PSy-layer nodes have a `dag()` method (standing for directed acyclic graph),
which outputs the PSyIR nodes and their data dependencies. By default a file in
dot format is output with the name ``dag`` and a file in svg format is
output with the name ``dag.svg``. The file name can be changed using
the ``file_name`` optional argument and the output file format can be
changed using the ``file_format`` optional argument. The file_format
value is simply passed on to graphviz so the graphviz documentation
should be consulted for valid formats if svg is not required.
::

   >>> schedule.dag(file_name="lovely", file_format="png")

.. note:: The dag method can be called from any node and will
          output the dag for that node and all of its children.

If we were to look at the LFRic eg6 example we would see the
following image:

.. image:: dag.png
    :width: 256
    :align: center

In the image, all PSyIR nodes with children are split into a start
vertex and an end vertex (for example the InvokeSchedule node has
both `schedule_start` and `schedule_end` vertices).
Blue arrows indicate that there is a parent to child relationship (from
a start node) or a child to parent relationship (to an end node).
Green arrows indicate that a Node depends on another Node later in the
schedule (which we call a forward dependence). Therefore the OMP parallel
loop must complete before the globalsum is performed.
Red arrows indicate that a Node depends on
another Node that is earlier in the schedule (which we call a backward
dependence). However the direction of the red arrows are reversed to
improve the flow of the dag layout. In this example the forward and
backward dependence is the same, however this is not always the
case. The two built-ins do not depend on each other, so they have no
associated green or red arrows.

The dependence graph output gives an indication of whether nodes can
be moved within the InvokeSchedule. In this case it is valid to run the
built-ins in either order. The underlying dependence analysis used to
create this graph is used to determine whether a transformation of a
Schedule is valid from the perspective of data dependencies.
