.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2019, Science and Technology Facilities Council
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

In the following example, the invoke call includes a call to a Built-in
(``setval_c``) and a user-supplied kernel
(``matrix_vector_kernel_mm_type``). The
Built-in sets all values in the field ``Ax`` to
``0.0``. Notice that, unlike the kernel call, no use association is
required for the Built-in since it is provided as part of the environment
(*c.f.* Fortran intrinsics such as ``sin()``).
::

  subroutine jacobi_solver_algorithm(lhs, rhs, mm, mesh, n_iter)
    use matrix_vector_mm_mod, only: matrix_vector_kernel_mm_type
    integer,             intent(in)    :: n_iter
    type(field_type),    intent(inout) :: lhs, rhs
    type(operator_type), intent(inout) :: mm
    type(mesh_type),     intent(in)    :: mesh
    type(field_type)                   :: Ax, lumped_weight, res

    real(kind=r_def), parameter :: MU = 0.9_r_def
    ...

    do iter = 1,n_iter
      call invoke( setval_c(Ax, 0.0) )
      call invoke( matrix_vector_kernel_mm_type(Ax, lhs, mm) )
      ...
    end do

  end subroutine jacobi_solver_algorithm

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
code. The algorithm code is assumed to be in a file call
`solver_mod.x90`. In this case we use the top level python
interface. See the :ref:`api-label` section for different ways to
translate/generate code.
::

	>>> from psyclone.generator import generate
	>>> alg, psy = generate("solver_mod.x90")
	>>> print(alg)
	>>> print(psy)

The resultant generated algorithm code is given below.

Ignoring the difference in case (which is due to the output format of
the code parser) the differences between the original algorithm code
and the translated algorithm code are:

* The generic calls to ``invoke`` have been replaced by specific
  ``CALL invoke_xx``. The calls within the invoke are removed, as are
  duplicate arguments and any literals leaving the three fields being
  passed in;
* A use statement is added for the each of the new ``CALL invoke_xx``
  which will call the generated PSy layer code.

The existence of a call to a Built-in has made no difference at this point:
::

    SUBROUTINE jacobi_solver_algorithm(lhs, rhs, mm, mesh, n_iter)
      USE solver_mod_psy, ONLY: invoke_5_matrix_vector_kernel_mm_type
      USE solver_mod_psy, ONLY: invoke_4
      INTEGER, intent(in) :: n_iter
      TYPE(field_type), intent(inout) :: lhs, rhs
      TYPE(operator_type), intent(inout) :: mm
      TYPE(mesh_type), intent(in) :: mesh
      TYPE(field_type) ax, lumped_weight, res

      REAL(KIND=r_def), parameter :: mu = 0.9_r_def

      INTEGER iter
      INTEGER rhs_fs
      TYPE(function_space_type) fs
      ...
      DO iter = 1,n_iter
        CALL invoke_4(ax)
        CALL invoke_5_matrix_vector_kernel_mm_type(ax, lhs, mm)
        ...
      END DO
    END SUBROUTINE jacobi_solver_algorithm

A vanilla (not optimised) version of the generated PSy layer is given
below. As expected the kernel code is called from the PSy
layer. However, in the case of the `setval_c` Built-in, the
code for this has been written directly into the PSy layer (the loop
setting `ax_proxy%data(df) = 0.0`). This example illustrates that
Built-ins may be implemented in whatever way the generator
sees fit with no change to the algorithm and kernel layers.
::

  MODULE solver_mod_psy
    ...
    SUBROUTINE invoke_4(ax)
      USE mesh_mod, ONLY: mesh_type
      TYPE(field_type), intent(inout) :: ax
      INTEGER df
      INTEGER undf_any_space_1
      TYPE(field_proxy_type) ax_proxy
      !
      ! Initialise field proxies
      !
      ax_proxy = ax%get_proxy()
      !
      ! Initialise sizes and allocate any basis arrays for any_space_1
      !
      undf_any_space_1 = ax_proxy%vspace%get_undf()
      !
      ...
      ! Call our kernels
      !
      DO df=1,undf_any_space_1
        ax_proxy%data(df) = 0.0
      END DO
      !
      ...
      !
    END SUBROUTINE invoke_4
    SUBROUTINE invoke_5_matrix_vector_kernel_mm_type(ax, lhs, mm)
      USE matrix_vector_mm_mod, ONLY: matrix_vector_mm_code
      ...
      TYPE(field_type), intent(inout) :: ax, lhs
      TYPE(operator_type), intent(inout) :: mm
      ...
      !
      ! Initialise field proxies
      !
      ax_proxy = ax%get_proxy()
      lhs_proxy = lhs%get_proxy()
      mm_proxy = mm%get_proxy()
      !
      ! Initialise number of layers
      !
      nlayers = ax_proxy%vspace%get_nlayers()
      !
      ! Initialise sizes and allocate any basis arrays for any_space_1
      !
      ndf_any_space_1 = ax_proxy%vspace%get_ndf()
      undf_any_space_1 = ax_proxy%vspace%get_undf()
      !
      ...
      DO cell=1,mesh%get_last_halo_cell(1)
        !
        map_any_space_1 => ax_proxy%vspace%get_cell_dofmap(cell)
        !
        CALL matrix_vector_mm_code(cell, nlayers, ax_proxy%data,            &
                                   lhs_proxy%data, mm_proxy%ncell_3d,       &
                                   mm_proxy%local_stencil, ndf_any_space_1, &
                                   undf_any_space_1, map_any_space_1)
        ...
        !
      END DO
      !
      ...
      !
    END SUBROUTINE invoke_5_matrix_vector_kernel_mm_type
    ...
  END MODULE solver_mod_psy

This example is distributed with PSyclone and can be found in
``<PSYCLONEHOME>/examples/dynamo/eg3``.

Supported Built-in operations
-----------------------------

The list of supported Built-ins is API-specific and
therefore is described under the documentation of each API.

Adding new additional Built-in operations
-----------------------------------------

 1. Identify the PSyclone source file for the API to be extended. *e.g.* for
    dynamo0.3 it is ``src/psyclone/dynamo0p3_builtins.py``.
 2. Edit this source file to create the class for this new call. It must
    inherit from the API-specific parent class for Built-in operations
    (``DynBuiltInKern`` for dynamo0.3).
 3. Implement ``__str__`` and ``gen_code()`` methods for this new class.
 4. Add the name of the new Built-in operation and its corresponding class
    to the ``BUILTIN_MAP`` dictionary in that source file.
 5. Add metadata describing this call to the appropriate file specified in
    the ``BUILTIN_DEFINITIONS_FILE`` in that source file. For dynamo0.3
    this is ``dynamo0p3_builtins_mod.f90``.
 6. Add relevant tests to the PSyclone test file for the API to be extended.
    *e.g.* for dynamo0.3 it is ``src/psyclone/tests/dynamo0p3_builtins_test.py``.
    The tests rely on ``single_invoke`` Fortran examples in the relevant
    ``src/psyclone/tests/test_files/`` subfolder.
 7. Add an appropriate Fortran ``single_invoke`` example for the new
    Built-in in the relevant ``src/psyclone/tests/test_files/`` subfolder. *e.g.*
    for dynamo0.3 it is ``src/psyclone/tests/test_files/dynamo0p3/``.
    Names of examples follow the template
    ``<category.number>.<subcategory.number>_<single_invoke_name>.f90``.
    *e.g.* for dynamo0.3 ``<category.number>`` is 15.
 8. Document the new Built-in in the documentation of the
    relevant API (*e.g.* ``doc/dynamo0p3.rst``).


If the API being extended does not currently support any Built-ins
then the ``BUILTIN_MAP`` and ``BUILTIN_DEFINITIONS_FILE`` module
variables must be added to the source file for the API.  A Fortran
module file must be created in the PSyclone src directory (with the
name specified in ``BUILTIN_DEFINITIONS_FILE``) containing metadata
describing the Built-in operations. Finally,
``parse.get_builtin_defs()`` must be extended to import
``BUILTIN_MAP`` and ``BUILTIN_DEFINITIONS_FILE`` for this API.
