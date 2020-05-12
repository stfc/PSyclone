.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2020, Science and Technology Facilities Council
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
.. Written by R. W. Ford and A. R. Porter, STFC Daresbury Lab
.. Modified by I. Kavcic, Met Office

.. _built-ins:

Built-ins
=========

Built-ins (named by analogy with the native functionality provided by
Python) are operations which can be specified within an invoke call in
the algorithm layer but do not require an associated kernel to be
implemented as they are provided directly by the infrastructure.

One use of Built-ins is for commonly used operations. In
this case Built-ins simplify the use of the system as users
do not need to write kernel routines. Built-ins also
offer a potential performance advantage as they provide a
specification of what is required without an implementation. Therefore
the PSy layer is free to implement these operations in whatever way it
chooses.

.. note:: In general, PSyclone will need to know the types of the arguments
          being passed to any Built-ins. The parser obtains this information
          from an API-specific file that contains the metadata for all
          Built-in operations supported for that API.

Example
-------

.. highlight:: fortran

In the following example, the invoke call includes a call to two Built-ins
(``setval_c`` and ``X_divideby_Y``) and a user-supplied kernel
(``matrix_vector_kernel_mm_type``).
The ``setval_c`` Built-in sets all values in the field ``Ax`` to ``1.0`` and
the ``X_divideby_Y`` Built-in divides values in the field ``rhs`` by their
equivalent (per degree of freedom) values in the field ``lumped_weight``
(see :ref:`supported Dynamo0.3 API Built-ins <dynamo0.3-built-ins>`). Notice
that, unlike the kernel call, no ``use`` association is required for the
Built-ins since they are provided as part of the environment (*c.f.* Fortran
intrinsics such as ``sin()``).
::

  module solver_mod
    ...
    use matrix_vector_mm_mod, only: matrix_vector_kernel_mm_type
    ...

    subroutine jacobi_solver_algorithm(lhs, rhs, mm, mesh, n_iter)

      integer(kind=i_def), intent(in)    :: n_iter
      type(field_type),    intent(inout) :: lhs
      type(field_type),    intent(in)    :: rhs
      type(operator_type), intent(in)    :: mm
      type(mesh_type),     intent(in)    :: mesh
      type(field_type)                   :: Ax, lumped_weight, res

      real(kind=r_def), parameter :: MU = 0.9_r_def
      ...

      ! Compute mass lump
      call invoke( name = "Jacobi_mass_lump",                           &
                   setval_c(Ax, 1.0_r_def),                             &
                   matrix_vector_kernel_mm_type(lumped_weight, Ax, mm), &
                   X_divideby_Y(lhs, rhs, lumped_weight) )

    end subroutine jacobi_solver_algorithm
    ...

  end module solver_mod

Below is an example of a kernel that is consistent with the
``matrix_vector_kernel_mm_type kernel`` specified in the example above.
::

  module matrix_vector_mm_mod
    type, public, extends(kernel_type) :: matrix_vector_kernel_mm_type
      private
      type(arg_type) :: meta_args(3) = (/                                  &
           arg_type(GH_FIELD,    GH_INC,  ANY_SPACE_1),                    &
           arg_type(GH_FIELD,    GH_READ, ANY_SPACE_1),                    &
           arg_type(GH_OPERATOR, GH_READ, ANY_SPACE_1, ANY_SPACE_1)        &
           /)
      integer :: iterates_over = CELLS
    contains
      procedure, nopass :: matrix_vector_mm_code
    end type
  contains
    subroutine matrix_vector_mm_code(cell,        &
                                     nlayers,     &
                                     lhs, x,      &
                                     ncell_3d,    &
                                     mass_matrix, &
                                     ndf, undf, map)
    end subroutine matrix_vector_mm_code
  end module matrix_vector_mm_mod

We now translate the algorithm layer code and generate the PSy layer
code. The algorithm code is assumed to be in a file called
``solver_mod.x90`` (see Example 3 in :ref:`LFRic examples <examples_lfric>`
section). In this case we use the top level Python interface. See the
:ref:`api-label` section for different ways to translate/generate code.

.. code-block:: bash

   > psyclone -nodm -oalg solver_mod.f90 -opsy solver_mod_psy.f90 \
   > solver_mod.x90

The resultant generated algorithm code is given below.

Ignoring the difference in case (which is due to the output format of
the code parser) the differences between the original algorithm code
and the translated algorithm code are:

* The generic calls to ``invoke`` have been replaced by specific
  ``CALL invoke_<xx>``. The kernel calls within the original ``invoke``
  are removed, as are duplicate arguments and any literals, leaving
  the five fields and one operator as arguments;

* A ``use`` statement is added for the each of the new ``CALL invoke_<xx>``
  which will call the generated PSy layer code.

The existence of calls to Built-ins has made no difference at this point::

    SUBROUTINE jacobi_solver_algorithm(lhs, rhs, mm, mesh, n_iter)
      USE solver_mod_psy, ONLY: invoke_jacobi_iterloop
      USE solver_mod_psy, ONLY: invoke_21
      USE solver_mod_psy, ONLY: invoke_jacobi_mass_lump

      IMPLICIT NONE

      INTEGER(KIND = i_def), INTENT(IN) :: n_iter
      TYPE(field_type), INTENT(INOUT) :: lhs
      TYPE(field_type), INTENT(IN) :: rhs
      TYPE(operator_type), INTENT(IN) :: mm
      TYPE(mesh_type), INTENT(IN) :: mesh
      TYPE(field_type) :: Ax, lumped_weight, res

      REAL(KIND = r_def), PARAMETER :: MU = 0.9_r_def

      INTEGER(KIND = i_def) :: iter
      INTEGER(KIND = i_def) :: rhs_fs
      TYPE(function_space_type) :: fs

      ...
      CALL invoke_jacobi_mass_lump(ax, lumped_weight, mm, lhs, rhs)
      ...

    END SUBROUTINE jacobi_solver_algorithm

A vanilla (with no distributed and shared-memory optimisations) version
of the generated PSy layer is given below. As expected, the kernel code is
called from the PSy layer. However, in the case of the Built-ins, the code
for these has been written directly into the PSy layer:

* ``setval_c`` translates to the loop setting
  ``ax_proxy%data(df) = 1.0_r_def``;

* ``X_divideby_Y`` translates to the loop setting
  ``lhs_proxy%data(df) = rhs_proxy%data(df) / lumped_weight_proxy%data(df)``.

This example illustrates that Built-ins may be implemented in whatever way
PSyclone sees fit with no change to the algorithm and kernel layers.
::

  MODULE solver_mod_psy
    ...

    SUBROUTINE invoke_jacobi_mass_lump(ax, lumped_weight, mm, lhs, rhs)
      USE matrix_vector_mm_mod, ONLY: matrix_vector_mm_code
      TYPE(field_type), intent(in) :: ax, lumped_weight, lhs, rhs
      TYPE(operator_type), intent(in) :: mm
      ...
      !
      ! Initialise field and/or operator proxies
      !
      ax_proxy = ax%get_proxy()
      lumped_weight_proxy = lumped_weight%get_proxy()
      mm_proxy = mm%get_proxy()
      lhs_proxy = lhs%get_proxy()
      rhs_proxy = rhs%get_proxy()
      !
      ! Initialise number of layers
      !
      nlayers = ax_proxy%vspace%get_nlayers()
      !
      ! Look-up dofmaps for each function space
      !
      map_aspc1_lumped_weight => lumped_weight_proxy%vspace%get_whole_dofmap()
      !
      ! Initialise number of DoFs for aspc1_ax
      !
      ndf_aspc1_ax = ax_proxy%vspace%get_ndf()
      undf_aspc1_ax = ax_proxy%vspace%get_undf()
      !
      ! Initialise number of DoFs for aspc1_lumped_weight
      !
      ndf_aspc1_lumped_weight = lumped_weight_proxy%vspace%get_ndf()
      undf_aspc1_lumped_weight = lumped_weight_proxy%vspace%get_undf()
      !
      ! Initialise number of DoFs for aspc1_lhs
      !
      ndf_aspc1_lhs = lhs_proxy%vspace%get_ndf()
      undf_aspc1_lhs = lhs_proxy%vspace%get_undf()
      !
      ! Call our kernels
      !
      DO df=1,undf_aspc1_ax
        ax_proxy%data(df) = 1.0_r_def
      END DO
      DO cell=1,lumped_weight_proxy%vspace%get_ncell()
        !
        CALL matrix_vector_mm_code(cell, nlayers,            &
                                   lumped_weight_proxy%data, &
                                   ax_proxy%data,            &
                                   mm_proxy%ncell_3d,        &
                                   mm_proxy%local_stencil,   &
                                   ndf_aspc1_lumped_weight,  &
                                   undf_aspc1_lumped_weight, &
                                   map_aspc1_lumped_weight(:,cell))
      END DO
      DO df=1,undf_aspc1_lhs
        lhs_proxy%data(df) = rhs_proxy%data(df) / lumped_weight_proxy%data(df)
      END DO
      !
    END SUBROUTINE invoke_jacobi_mass_lump
    ...
  END MODULE solver_mod_psy

This example is distributed with PSyclone and can be found in
``<PSYCLONEHOME>/examples/lfric/eg3``.

Supported Built-in operations
-----------------------------

The list of supported Built-ins is API-specific and
therefore is described under the documentation of each API.

Adding new Built-in operations
------------------------------

 1. Identify the PSyclone source file for the API to be extended. *e.g.* for
    Dynamo0.3 (LFRic) API it is ``src/psyclone/dynamo0p3_builtins.py``.
 2. Edit this source file to create the class for this new call. It must
    inherit from the API-specific parent class for Built-in operations
    (``DynBuiltInKern`` for Dynamo0.3).
 3. Implement ``__str__`` and ``gen_code()`` methods for this new class.
 4. Add the name of the new Built-in operation and its corresponding class
    to the ``BUILTIN_MAP`` dictionary in that source file.
 5. Add metadata describing this call to the appropriate file specified in
    the ``BUILTIN_DEFINITIONS_FILE`` in that source file. For Dynamo0.3
    this is ``src/psyclone/parse/dynamo0p3_builtins_mod.f90``.
 6. Add relevant tests to the PSyclone test file for the API to be extended.
    *e.g.* for Dynamo0.3 it is
    ``src/psyclone/tests/dynamo0p3_builtins_test.py``. The tests rely on
    ``single_invoke`` Fortran examples in the relevant
    ``src/psyclone/tests/test_files/`` subfolder.
 7. Add an appropriate Fortran ``single_invoke`` example for the new
    Built-in in the relevant ``src/psyclone/tests/test_files/`` subfolder.
    *e.g.* for Dynamo0.3 it is ``src/psyclone/tests/test_files/dynamo0p3/``.
    Names of examples follow the template
    ``<category.number>.<subcategory.number>_<built-in_name>.f90``.
    *e.g.* for Dynamo0.3 API ``<category.number>`` is 15 and
    ``<built-in_name>`` follows the :ref:`Dynamo0.3 API Built-in naming
    scheme <dynamo0.3-built-ins-names>`.
 8. Document the new Built-in in the documentation of the
    relevant API (*e.g.* ``doc/dynamo0p3.rst`` for Dynamo0.3 API).


If the API being extended does not currently support any Built-ins
then the ``BUILTIN_MAP`` and ``BUILTIN_DEFINITIONS_FILE`` module
variables must be added to the source file for the API.  A Fortran
module file must be created in the PSyclone src directory (with the
name specified in ``BUILTIN_DEFINITIONS_FILE``) containing metadata
describing the Built-in operations. Finally,
``parse.get_builtin_defs()`` must be extended to import
``BUILTIN_MAP`` and ``BUILTIN_DEFINITIONS_FILE`` for this API.
