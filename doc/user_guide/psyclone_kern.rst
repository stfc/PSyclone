.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2025, Science and Technology Facilities Council
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
.. Modified by I. Kavcic, L. Turner and J. Dendy, Met Office

PSyclone Kernel Tools
=====================

In addition to the ``psyclone`` command, the PSyclone package also
provides tools related to generating code purely from kernel
metadata. Currently there are two such tools:

 1. :ref:`stub-generation`
 2. :ref:`alg-generation`

The kernel-stub generator takes a file containing kernel metadata as
input and outputs the (Fortran) kernel subroutine arguments and
declarations. The word "stub" is used to indicate that it is only the
subroutine arguments and their declarations that are generated; the
subroutine has no content.

The algorithm generator also takes a file containing a kernel
implementation but this time generates an appropriate algorithm layer
subroutine. This algorithm layer plus the associated kernel metadata
may then be processed with PSyclone in the usual way to generate code
which executes the supplied kernel.

This functionality is provided to the user via the ``psyclone-kern``
command, described in more detail below.

The psyclone-kern Command
---------------------------

Before using the ``psyclone-kern`` tool, PSyclone must be installed. If you
have not already done so, please follow the instructions for setting
up PSyclone in Section :ref:`Getting Going <getting-going>`.

PSyclone will be installed in a particular location on your machine,
which will be referred to as the ``<PSYCLONEINSTALL>`` directory. The
``psyclone-kern`` script comes with the PSyclone
installation. A quick check ``> which psyclone-kern`` should return
the location of the ``<PSYCLONEINSTALL>/bin`` directory.

The ``psyclone-kern`` command has the following arguments:

.. code-block:: bash

    > psyclone-kern -h
    usage: psyclone-kern [-h] [-gen {alg,stub}] [-o OUT_FILE] [-api API]
                         [-I INCLUDE] [-l {off,all,output}]
                         [--config CONFIG] [-v]
                         filename

    Run the PSyclone kernel generator on a particular file

    positional arguments:
      filename              file containing Kernel metadata

    optional arguments:
      -h, --help            show this help message and exit
      -gen {alg,stub)       what to generate for the supplied kernel
                            (alg=algorithm layer, stub=kernel-stub subroutine).
                            Defaults to stub.
      -o OUT_FILE           filename for created code.
      -api API              choose a particular API from ['lfric',
                            'gocean'].
      -I INCLUDE, --include INCLUDE
                            path to Fortran INCLUDE or module files
      -l {off,all,output}, --limit {off,all,output}
                            limit the Fortran line length to 132
                            characters (default 'off'). Use 'all' to
                            apply limit to both input and output
                            Fortran. Use 'output' to apply line-length
                            limit to output Fortran only.
      --config CONFIG, -c CONFIG
                            config file with PSyclone specific options.
      -v, --version         display version information (\ |release|\ )

The ``-o`` option allows the user to specify that the output should be
written to a particular file. If this is not specified then the Python
``print`` statement is used to write to stdout.  Typically this
results in the output being printed to the terminal.

The ``-l``, or ``--limit`` option utilises the PSyclone support for
wrapping of lines within the 132 character limit in the generated Fortran code.

.. _stub-generation:

Kernel-stub Generator
---------------------

Quick Start
+++++++++++

1) Use an existing Kernel file or create a Kernel file containing a
   Kernel module with the required metadata and an empty Kernel
   subroutine with no arguments.
2) Run the following command ::

    > psyclone-kern -api lfric -gen stub <PATH>/my_file.f90
3) To have the generated code written to file rather than stdout use the
   `-o` flag ::

    > psyclone-kern -api lfric -gen stub -o my_stub_file.f90 ./my_kernel_mod.f90

(Since stub generation is the default, the ``-gen stub`` may be omitted
if desired.)

Introduction
++++++++++++

PSyclone provides a kernel stub generator for the LFRic API.
The kernel stub generator takes a kernel file as input and outputs the
kernel subroutine arguments and declarations. The word "stub" is used
to indicate that it is only the subroutine arguments and their
declarations that are generated; the subroutine has no content.

The primary reason the stub generator is useful is that it generates
the correct Kernel subroutine arguments and declarations for the
LFRic API as specified by the Kernel metadata. As the number of
arguments to Kernel subroutines can become large and the arguments
have to follow a particular order, it can become burdensome, and
potentially error prone, for the user to have to work out the
appropriate argument list if written by hand.

The stub generator can be used when creating a new Kernel. A Kernel
can first be written to specify the required metadata and then the
generator can be used to create the appropriate (empty) Kernel
subroutine. The user can then fill in the content of the subroutine.

The stub generator can also be used to check whether the arguments for
an existing Kernel are correct i.e. whether the Kernel subroutine and
Kernel metadata are consistent. One example would be where a Kernel is
updated resulting in a change to the metadata and subroutine
arguments.

The LFRic API requires Kernels to conform to a set of rules which
determine the required arguments and types for a particular
Kernel. These rules are required as the generated PSy layer needs to
know exactly how to call a Kernel. These rules are outlined in Section
:ref:`Rules <lfric-stub-generation-rules>`.

Therefore PSyclone has been coded with the LFRic API rules which
are then applied when reading the Kernel metadata to produce the
required Kernel call and its arguments in the generated PSy
layer. These same rules are used by the Kernel stub generator to
produce Kernel subroutine stubs, thereby guaranteeing that Kernel
calls from the PSy layer and the associated Kernel subroutines are
consistent.

.. _stub-generation-kernels:

Kernels
+++++++

Any LFRic kernel can be used as input to the stub generator.
Example Kernels can be found in the ``examples/lfric`` repository or,
for more simple cases, in the ``tests/test_files/lfric`` directory.
These directories are located in the ``<PSYCLONEHOME>/src/psyclone``
directory where ``<PSYCLONEHOME>`` refers to the location where you
download or clone PSyclone (:ref:`Getting Going <getting-going>`).

In the ``tests/test_files/lfric`` directory the majority of examples
start with ``testkern``. Amongst the exceptions are: ``testkern_simple_mod.f90``,
``ru_kernel_mod.f90`` and ``matrix_vector_kernel_mod.F90``. The following
test kernels can be used to generate kernel stub code (running stub
generation from the ``<PSYCLONEHOME>/src/psyclone`` directory)::

    tests/test_files/lfric/testkern_chi_read_mod.F90
    tests/test_files/lfric/testkern_coord_w0_mod.F90
    tests/test_files/lfric/testkern_operator_mod.f90
    tests/test_files/lfric/testkern_operator_nofield_mod.f90
    tests/test_files/lfric/ru_kernel_mod.f90
    tests/test_files/lfric/testkern_simple_mod.f90

.. _stub-generation-example:

Example
+++++++

A simple, single field example of a kernel that can be used as input for the
stub generator is found in ``tests/test_files/lfric/testkern_simple_mod.f90`` and
is shown below:

.. _simple_metadata:

 .. code-block:: fortran

  module simple_mod

    use argument_mod
    use fs_continuity_mod
    use kernel_mod
    use constants_mod

    implicit none

    type, extends(kernel_type) :: simple_type
      type(arg_type), dimension(1) :: meta_args = &
           (/ arg_type(gh_field, gh_real, gh_inc, w1) /)
      integer :: operates_on = cell_column
    contains
      procedure, nopass :: code => simple_code
    end type simple_type

  contains

    subroutine simple_code()
    end subroutine

  end module simple_mod

.. note:: The module name ``simple_mod`` and the type name ``simple_type``
          share the same root ``simple`` and have the extensions ``_mod``
          and ``_type`` respectively. This is a convention in LFRic API
          and is required by the kernel stub generator as it needs to
          determine the name of the type containing the metadata and infers
          this by reading the module name. If this rule is not followed the
          kernel stub generator will return with an error message
          (see Section :ref:`Errors <stub-generation-errors>`).

.. note:: Whilst strictly the kernel stub generator only requires the Kernel
          metadata to generate the appropriate stub code, the parser that
          the generator relies on currently requires a dummy kernel subroutine
          to exist.

If we run the kernel stub generator on the ``testkern_simple_mod.f90`` example::

  > psyclone-kern -api lfric -gen stub tests/test_files/lfric/testkern_simple_mod.f90

we get the following kernel stub output:

 .. code-block:: fortran

  MODULE simple_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE simple_code(nlayers, field_1_w1, ndf_w1, undf_w1, map_w1)
      USE constants_mod, ONLY: r_def, i_def
      IMPLICIT NONE
      INTEGER(KIND=i_def), intent(in) :: nlayers
      INTEGER(KIND=i_def), intent(in) :: ndf_w1
      INTEGER(KIND=i_def), intent(in), dimension(ndf_w1) :: map_w1
      INTEGER(KIND=i_def), intent(in) :: undf_w1
      REAL(KIND=r_def), intent(inout), dimension(undf_w1) :: field_1_w1
    END SUBROUTINE simple_code
  END MODULE simple_mod

The subroutine content can then be copied into the required module,
used as the basis for a new module, or checked with an existing
subroutine for correctness.

.. note:: The output does not currently conform to Met Office coding
          standards so must be modified accordingly.

.. note:: The code will not compile without a) providing the
          ``constants_mod``, ``argument_mod`` and ``kernel_mod`` modules
          in the compiler include path and b) adding in code that writes
          to any arguments declared as intent ``out`` or ``inout``. For a
          quick check, the ``USE`` declaration and ``KIND`` declarations
          can be removed and the ``field_1_w1`` array can be initialised
          with some value in the subroutine. At this point the Kernel
          should compile successfully.

.. note:: Whilst there is only one field declared in the metadata there
          are 5 arguments to the Kernel. The first argument ``nlayers``
          specifies the number of layers in a column for a field. The
          second argument is the array associated with the field. The
          field array is dimensioned as the *number of unique degrees
          of freedom* (hereafter ``undf``) which is also passed into
          the kernel (the fourth argument). The naming convention is to
          call each field a ``field``, followed by its position in the
          (algorithm) argument list (which is reflected in the metadata
          ordering). The third argument is the number of degrees of freedom
          for the particular column and is used to dimension the final
          argument which is the *degrees of freedom map* (dofmap) which
          indicates the location of the required values in the field array.
          The naming convention for the ``dofmap``, ``undf`` and ``ndf`` is
          to append the name with the space that it is associated with.

We now take a look at a more complicated example. The metadata in this
example is the same as an actual LFRic kernel, however the
subroutine content and various comments have been removed. The metadata
specifies that there are four fields passed by the algorithm layer, the
fourth of which is a vector field of size three. All three of the spaces
require a basis function and the ``W0`` and ``W2`` function spaces
additionally require a differential basis function. The content of the
Kernel, excluding the subroutine body, is given below:

 .. code-block:: fortran

  module ru_kernel_mod

  use argument_mod
  use fs_continuity_mod
  use kernel_mod
  use constants_mod

  implicit none

  private

  type, public, extends(kernel_type) :: ru_kernel_type
    private
    type(arg_type) :: meta_args(6) = (/                                  &
         arg_type(GH_FIELD,   GH_REAL,    GH_INC,  W2),                  &
         arg_type(GH_FIELD,   GH_REAL,    GH_READ, W3),                  &
         arg_type(GH_SCALAR,  GH_INTEGER, GH_READ),                      &
         arg_type(GH_SCALAR,  GH_REAL,    GH_READ),                      &
         arg_type(GH_FIELD,   GH_REAL,    GH_READ, W0),                  &
         arg_type(GH_FIELD*3, GH_REAL,    GH_READ, W0)                   &
         /)
    type(func_type) :: meta_funcs(3) = (/                                &
         func_type(W2, GH_BASIS, GH_DIFF_BASIS),                         &
         func_type(W3, GH_BASIS),                                        &
         func_type(W0, GH_BASIS, GH_DIFF_BASIS)                          &
         /)
    integer :: operates_on = CELL_COLUMN
    integer :: gh_shape = gh_quadrature_XYoZ
  contains
    procedure, nopass :: ru_code
  end type

  public ru_code

  contains

    subroutine ru_code()
    end subroutine ru_code

  end module ru_kernel_mod

If we run the kernel stub generator on this example::

  > psyclone-kern -api lfric -gen stub tests/test_files/lfric/ru_kernel_mod.f90

we obtain the following output:

 .. code-block:: fortran

  MODULE ru_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE ru_code(nlayers, field_1_w2, field_2_w3, iscalar_3, rscalar_4, &
                       field_5_w0, field_6_w0_v1, field_6_w0_v2, field_6_w0_v3, &
                       ndf_w2, undf_w2, map_w2, basis_w2_qr_xyoz, &
                       diff_basis_w2_qr_xyoz, ndf_w3, undf_w3, map_w3, &
                       basis_w3_qr_xyoz, ndf_w0, undf_w0, map_w0, &
                       basis_w0_qr_xyoz, diff_basis_w0_qr_xyoz, &
                       np_xy_qr_xyoz, np_z_qr_xyoz, weights_xy_qr_xyoz, weights_z_qr_xyoz)
      USE constants_mod, ONLY: r_def, i_def
      IMPLICIT NONE
      INTEGER(KIND=i_def), intent(in) :: nlayers
      INTEGER(KIND=i_def), intent(in) :: ndf_w0
      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0
      INTEGER(KIND=i_def), intent(in) :: ndf_w2
      INTEGER(KIND=i_def), intent(in), dimension(ndf_w2) :: map_w2
      INTEGER(KIND=i_def), intent(in) :: ndf_w3
      INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3
      INTEGER(KIND=i_def), intent(in) :: undf_w2, undf_w3, undf_w0
      REAL(KIND=r_def), intent(in) :: rscalar_4
      INTEGER(KIND=i_def), intent(in) :: iscalar_3
      REAL(KIND=r_def), intent(inout), dimension(undf_w2) :: field_1_w2
      REAL(KIND=r_def), intent(in), dimension(undf_w3) :: field_2_w3
      REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_5_w0
      REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_6_w0_v1
      REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_6_w0_v2
      REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_6_w0_v3
      INTEGER(KIND=i_def), intent(in) :: np_xy_qr_xyoz, np_z_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2,np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w2_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2,np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w2_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w3_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(1,ndf_w0,np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w0_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(3,ndf_w0,np_xy_qr_xyoz,np_z_qr_xyoz) :: diff_basis_w0_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(np_xy_qr_xyoz) :: weights_xy_qr_xyoz
      REAL(KIND=r_def), intent(in), dimension(np_z_qr_xyoz) :: weights_z_qr_xyoz
    END SUBROUTINE ru_code
  END MODULE ru_mod

The above example demonstrates that the argument list can get quite
complex. Rather than going through an explanation of each argument you
are referred to Section :ref:`Rules <lfric-stub-generation-rules>` for
more details on the rules for argument types and argument ordering.
Regarding naming conventions for arguments you can see that the arrays
associated with the fields are labelled as 1-6 depending on their
position in the metadata. For a vector field, each vector results in a
different array. These are distinguished by appending ``_vx`` where ``x`` is
the number of the vector.

The introduction of stencil operations on field arguments further complicates
the argument list of a kernel. An example of the use of the stub generator
for a kernel that performs stencil operations is provided in
``examples/lfric/eg5``::

  > psyclone-kern -api lfric -gen stub ../../examples/lfric/eg5/conservative_flux_kernel_mod.F90

.. _stub-generation-errors:

Errors
++++++

The stub generator has been written to provide useful errors if
mistakes are found. If you run the generator and it does not produce a
useful error - and in particular if it produces a stack trace - please
contact the PSyclone developers.

The following tests do not produce stub kernel code either because
they are invalid or because they contain functionality that is not
supported in the stub generator::

    tests/test_files/lfric/testkern_any_space_1_mod.f90
    tests/test_files/lfric/testkern_any_space_4_mod.f90
    tests/test_files/lfric/testkern_any_discontinuous_space_op_2_mod.f90
    tests/test_files/lfric/testkern_dofs_mod.f90
    tests/test_files/lfric/testkern_invalid_fortran_mod.f90
    tests/test_files/lfric/testkern_short_name_mod.f90
    tests/test_files/lfric/testkern_no_datatype_mod.f90
    tests/test_files/lfric/testkern_wrong_file_name.F90

``testkern_invalid_fortran_mod.f90``, ``testkern_no_datatype_mod.f90``,
``testkern_short_name_mod.f90`` and ``testkern_wrong_file_name.F90`` are designed to be
invalid for PSyclone stub generation testing purposes and should produce
appropriate errors. Two examples are below::

    > psyclone-kern -api lfric -gen stub tests/test_files/lfric/testkern_invalid_fortran_mod.f90
    Error: 'Parse Error: Code appears to be invalid Fortran'

    > psyclone-kern -api lfric -gen stub tests/test_files/lfric/testkern_no_datatype_mod.f90
    Error: 'Parse Error: Kernel type testkern_type does not exist'

``testkern_dofs_mod.f90`` is an example with an unsupported feature, as the
``operates_on`` metadata specifies ``dof``. Currently only kernels with
``operates_on=CELL_COLUMN`` are supported by the stub generator.

Generic function space metadata ``any_space`` and ``any_discontinuous_space``
(see Section :ref:`Supported Function Spaces <lfric-function-space>`
for function-space identifiers) are currently only supported for
:ref:`LFRic fields <lfric-field>` in the stub generator. Basis
and differential basis functions on these generic function spaces, required
for :ref:`quadrature <lfric-quadrature>` and
:ref:`evaluators <lfric-gh-shape>`, are not supported. Hence,
``testkern_any_space_1_mod.f90``, ``testkern_any_space_4_mod.f90`` and
``testkern_any_discontinuous_space_op_2_mod.f90`` should fail with
appropriate warnings because of that. For example::

    > psyclone-kern -api lfric -gen stub tests/test_files/lfric/testkern_any_space_1_mod.f90
    Error: "Generation Error: Unsupported space for basis function, expecting
    one of ['w3', 'wtheta', 'w2v', 'w2vtrace', 'w2broken', 'w0', 'w1', 'w2',
    'w2trace', 'w2h', 'w2htrace', 'any_w2', 'wchi'] but found 'any_space_1'"

As noted above, if the LFRic API naming convention for module and type
names is not followed, the stub generator will return with an error
message. For example::

    > psyclone-kern -api lfric -gen stub tests/test_files/lfric/testkern_wrong_file_name.F90
    Error: "Parse Error: Error, module name 'testkern_wrong_file_name' does not have
    '_mod' as an extension. This convention is assumed."


.. _alg-generation:

Algorithm Generator
-------------------

Quick Start
+++++++++++

1) Use an existing Kernel file containing a full LFRic kernel implementation.
2) Run the following command ::

    > psyclone-kern -api lfric -gen alg <PATH>/my_kern_file_mod.f90

3) The generated Algorithm code will be output to stdout by default. To have
   it written to a file use the `-o` flag.

Introduction
++++++++++++

The ability to generate a valid LFRic Algorithm layer that calls a given kernel
is useful for a number of reasons:

1) Starting point for creating a test for a kernel;
2) Benchmarking an individual kernel;
3) Constructing a test harness for the adjoint of a kernel produced by
   :ref:`PSyAD <psyad_introduction>`.

Currently algorithm generation is only supported for the LFRic API but it
could be extended to the GOcean API if desired.

Mapping of Function Spaces
^^^^^^^^^^^^^^^^^^^^^^^^^^

Every field or operator argument to an LFRic kernel must have its
function space(s) specified in the metadata of the kernel. This information
is used by the algorithm generation to ensure that each kernel argument
is correctly constructed. However, the metadata permits the use of certain
'generic' function-space specifiers (see :ref:`Supported Function Spaces
<lfric-function-space>`). If an argument is specified as being on one of
these spaces then the algorithm generator chooses an appropriate, specific
function space for that argument. e.g. an argument that is specified as
being on ``ANY_SPACE_<n>`` will be constructed on ``W0`` while one on
``ANY_DISCONTINUOUS_SPACE_<n>`` will be constructed on ``W3``.

Example
+++++++

If we take the same kernel used in the stub-generation
:ref:`example <stub-generation-example>` then running ::

  > psyclone-kern -api lfric -gen alg tests/test_files/lfric/testkern_simple_mod.f90

gives the following algorithm layer code:

 .. code-block:: fortran

  module test_alg_mod
    implicit none
    public

  contains
    subroutine test_alg(mesh, chi, panel_id)
      use field_mod, only : field_type
      use function_space_mod, only : function_space_type
      use fs_continuity_mod, only : w1
      use function_space_collection_mod, only : function_space_collection
      use mesh_mod, only : mesh_type
      use simple_mod, only : simple_type
      use constants_mod, only : i_def, r_def
      integer(kind=i_def), parameter :: element_order_h = 1_i_def
      integer(kind=i_def), parameter :: element_order_v = 1_i_def
      type(mesh_type), pointer, intent(in) :: mesh
      type(field_type), dimension(3), intent(in), optional :: chi
      type(field_type), intent(in), optional :: panel_id
      TYPE(function_space_type), POINTER :: vector_space_w1_ptr
      type(field_type) :: field_1

      vector_space_w1_ptr => function_space_collection % get_fs(mesh, element_order_h, element_order_v, w1)
      call field_1 % initialise(vector_space=vector_space_w1_ptr, name='field_1')
      call invoke(setval_c(field_1, 1.0_r_def), simple_type(field_1))

    end subroutine test_alg

  end module test_alg_mod

Note that the generated code implements an Algorithm subroutine that
is intended to be called from within an LFRic application that has
already setup data structures for the mesh (and, optionally, the `chi`
coordinate field and panel ID mapping).  Since the :ref:`metadata
<simple_metadata>` for the `simple_type` kernel specifies that the
field argument is on `W1`, the generated code must ensure that the
appropriate function space is set up and used to initialise the field.
Once that's done, the interesting part is the `invoke` call:

 .. code-block:: fortran

      call invoke(setval_c(field_1, 1.0_r_def), &
                  simple_type(field_1))

(where a line-break has been added for clarity). In this example the `invoke`
is for two kernels: the first is a :ref:`Built-in <psykal-built-ins>` that gives
`field_1` the value `1.0` everywhere and the second is the 'simple' kernel
itself which is passed the now initialised `field_1`.

This Algorithm code can now be processed by PSyclone in the normal way in
order to generate a transformed version plus an associated PSy-layer routine.
See :ref:`lfric_alg_gen_example` for a full example of doing this.

Limitations
+++++++++++

 * Algorithm generation is only currently supported for the LFRic API.
 * All fields are currently set to unity. Obviously the generated algorithm
   code may be edited to change this.
 * The generator does not currently recognise 'special' fields that hold
   geometry information (such as Chi or the face IDs) and these too will
   all be initialised to unity. This is the subject of Issue #1708 (although
   note that the generated code already permits the caller to supply Chi and/or
   face IDs).
 * Kernels with operator arguments are not yet supported.
 * Kernels with stencil accesses are not yet supported.

