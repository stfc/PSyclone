.. _stub-generation:

Stub Generation
===============

Quick Start
-----------

1) Use an existing Kernel file or create a Kernel file containing a
   Kernel module with the required metadata and an empty Kernel
   subroutine with no arguments.
2) Run the following command from the PSyclone src directory ::

    > python ./genkernelstub.py my_file.f90

Introduction
------------

PSyclone provides a kernel stub generator for the dynamo0.3 API. The
kernel stub generator takes a kernel file as input and outputs the
kernel subroutine arguments and declarations. The word "stub" is used
to indicate that it is only the subroutine arguments and their
declarations that are generated; the subroutine has no content.

The primary reason the stub generator is useful is that it generates
the correct Kernel subroutine arguments and declarations for the
dynamo0.3 API as specified by the Kernel metadata. As the number of
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

The dynamo0.3 API requires Kernels to conform to a set of rules which
determine the required arguments and types for a particular
Kernel. These rules are required as the generated PSy layer needs to
know exactly how to call a Kernel. These rules are outlined in Section
:ref:`Rules <stub-generation-rules>`.

Therefore PSyclone has been coded with the dynamo0.3 API rules which
are then applied when reading the Kernel metadata to produce the
require Kernel call and its arguments in the generated PSy
layer. These same rules are used by the Kernel stub generator to
produce Kernel subroutine stubs, thereby guaranteeing that Kernel
calls from the PSy layer and the associated Kernel subroutines are
consistent.

.. _stub-generation-use:

Use
---

Before using the stub generator, PSyclone must be installed. If you
have not already done so, please follow the instructions for setting
up PSyclone in Section :ref:`Getting Going <getting-going>`.

PSyclone will be installed in a particular location on your
machine. For the remainder of this section the location where PSyclone
is installed (including the PSyclone directory itself) will be referred
to as ``<PSYCLONEHOME>``.

The easiest way to use the stub generator is to use the supplied
script called ``genkernelstub.py``, which is located in the ``src``
directory:
::

    > cd <PSYCLONEHOME>/src
    > python ./genkernelstub.py 
    usage: genkernelstub.py [-h] [-o OUTFILE] [-api API] filename
    genkernelstub.py: error: too few arguments

You can get information about the ``genkernelstub.py`` arguments using
``-h`` or ``--help``:
::

  >  python genkernelstub.py -h
  usage: genkernelstub.py [-h] [-o OUTFILE] [-api API] filename

  Create Kernel stub code from Kernel metadata

  positional arguments:
    filename              Kernel metadata

  optional arguments:
    -h, --help            show this help message and exit
    -o OUTFILE, --outfile OUTFILE
                          filename of output
    -api API              choose a particular api from ['dynamo0.3'], default
                          dynamo0.3

As is indicated when using the ``-h`` option, the ``-api`` option only
accepts ``dynamo0.3`` at the moment and is redundant as this option is
also the default. However the number of supported API's is expected to
expand in the future.

The ``-o``, or ``--outfile`` option allows the user specify that the
output should be written to a particular file. If ``-o`` is not
specified then the python ``print`` statement is used. Typically the
print statement results in the output being printed to the terminal.

.. _stub-generation-kernels:

Kernels
-------

Any dynamo0.3 kernel can be used as input to the stub
generator. Example Kernels can be found in the dynamo repository or,
for more simple cases, in the ``tests/test_files/dynamo0p3``
directory. In the latter directory the majority start with
``testkern``. The exceptions are: ``simple.f90``, ``ru_kernel_mod.f90``
and ``matrix_vector_mm_mod.F90``. The following test kernels can be used
to generate kernel stub code:
::

    tests/test_files/dynamo0p3/testkern_chi_2.F90
    tests/test_files/dynamo0p3/testkern_chi.F90
    tests/test_files/dynamo0p3/testkern_operator_mod.f90
    tests/test_files/dynamo0p3/testkern_operator_nofield_mod.f90
    tests/test_files/dynamo0p3/testkern_orientation.F90
    tests/test_files/dynamo0p3/testkern_operator_orient_mod.f90
    tests/test_files/dynamo0p3/testkern_qr.F90
    tests/test_files/dynamo0p3/ru_kernel_mod.f90
    tests/test_files/dynamo0p3/simple.f90

.. _stub-generation-example:

Example
-------

A simple single field example of a kernel that can be used as input for the
stub generator is found in ``tests/test_files/dynamo0p3/simple.f90`` and
is shown below:
::

    module simple_mod
    type, extends(kernel_type) :: simple_type
        type(arg_type), dimension(1) :: meta_args =  &
            (/ arg_type(gh_field,gh_write,w1) /)
        integer, parameter :: iterates_over = cells
      contains
        procedure() :: code => simple_code
    end type simple_type
    contains
    subroutine simple_code()
    end subroutine
    end module simple_mod

.. note::
  The module name ``simple_mod`` and the type name ``simple_type`` share the same root ``simple`` and have the extensions ``_mod`` and ``_type`` respectively. This is a convention in dynamo0.3 and is required by the kernel stub generator as it needs to determine the name of the type containing the metadata and infers this by reading the module name. If this rule is not followed the kernel stub generator will return with an error message (see Section :ref:`Errors <stub-generation-errors>`).

.. note::
  Whilst strictly the kernel stub generator only requires the Kernel metadata to generate the appropriate stub code, the parser that the generator relies on currently requires a dummy kernel subroutine to exist.

If we run the kernel stub generator on the ``simple.f90`` example:
::

  > python genkernelstub.py tests/test_files/dynamo0p3/simple.f90

we get the following kernel stub output:
::

  MODULE simple_code_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE simple_code(nlayers, field_1_w1, ndf_w1, undf_w1, map_w1)
      USE constants_mod, ONLY: r_def
      IMPLICIT NONE
      INTEGER, intent(in) :: nlayers
      INTEGER, intent(in) :: undf_w1
      REAL(KIND=r_def), intent(out), dimension(undf_w1) :: field_1_w1
      INTEGER, intent(in) :: ndf_w1
      INTEGER, intent(in), dimension(ndf_w1) :: map_w1
    END SUBROUTINE simple_code
  END MODULE simple_code_mod

The subroutine content can then be copied into the required module,
used as the basis for a new module, or checked with an existing
subroutine for correctness.

.. note::
  The output does not currently conform to Met Office coding standards so must be modified accordingly.

.. note::
  The code will not compile without a) providing the constants_mod module in the compiler include path and b) adding in code that writes to any arguments declared as intent ``out`` or ``inout``. For a quick check, the ``USE`` declaration and ``KIND`` declarations can be removed and the ``field_1_w1`` array can be initialised with some value in the subroutine. At this point the Kernel should compile successfully.

.. note::
  Whilst there is only one field declared in the metadata there are 5 arguments to the Kernel. The first argument ``nlayers`` specifies the number of layers in a column for a field. The second argument is the array associated with the field. The field array is dimensioned as the number of unique degrees of freedom (undf) which is also passed into the kernel (the fourth argument). The naming convention is to call each field a field, followed by it's position in the (algorithm) argument list (which is reflected in the metadata ordering). The third argument is the number of degrees of freedom for the particular column and is used to dimension the final argument which is the degrees of freedom map (dofmap) which indicates the location of the required values in the field array. The naming convention for the ``dofmap``, ``undf`` and ``ndf`` is to append the name with the space that it is associated with.

We now take a look at a more complicated example. The metadata in this
example is the same as an actual dynamo kernel, however the subroutine
content and various comments have been removed. The metadata specifies
that there are four fields passed by the algorithm layer, the fourth
of which is a vector field of size three. All three of the spaces
require a basis function and the w0 and w2 function spaces
additionally require a differential basis function. The content of the
Kernel is given below.
::

  module ru_kernel_mod
  type, public, extends(kernel_type) :: ru_kernel_type
    private
    type(arg_type) :: meta_args(4) = (/                                  &
         arg_type(GH_FIELD,   GH_INC,  W2),                              &
         arg_type(GH_FIELD,   GH_READ, W3),                              &
         arg_type(GH_FIELD,   GH_READ, W0),                              &
         arg_type(GH_FIELD*3, GH_READ, W0)                               &
         /)
    type(func_type) :: meta_funcs(3) = (/                                &
         func_type(W2, GH_BASIS, GH_DIFF_BASIS),                         &
         func_type(W3, GH_BASIS),                                        &
         func_type(W0, GH_BASIS, GH_DIFF_BASIS)                          &
         /)
    integer :: iterates_over = CELLS
  contains
    procedure, nopass ::ru_code
  end type
  contains
  subroutine ru_code()
  end subroutine ru_code
  end module ru_kernel_mod

If we run the kernel stub generator on this example:
::

  > python genkernelstub.py tests/test_files/dynamo0p3/ru_kernel_mod.f90

we obtain the following output:
::

  MODULE ru_code_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE ru_code_code(nlayers, field_1_w2, field_2_w3, field_3_w0, field_4_w0_v1, field_4_w0_v2, field_4_w0_v3, ndf_w2, undf_w2, map_w2, basis_w2, diff_basis_w2, boundary_dofs_w2, ndf_w3, undf_w3, map_w3, basis_w3, ndf_w0, undf_w0, map_w0, basis_w0, diff_basis_w0, nqp_h, nqp_v, wh, wv)
      USE constants_mod, ONLY: r_def
      IMPLICIT NONE
      INTEGER, intent(in) :: nlayers
      INTEGER, intent(in) :: undf_w2
      INTEGER, intent(in) :: undf_w3
      INTEGER, intent(in) :: undf_w0
      REAL(KIND=r_def), intent(inout), dimension(undf_w2) :: field_1_w2
      REAL(KIND=r_def), intent(in), dimension(undf_w3) :: field_2_w3
      REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_3_w0
      REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_4_w0_v1
      REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_4_w0_v2
      REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_4_w0_v3
      INTEGER, intent(in) :: ndf_w2
      INTEGER, intent(in), dimension(ndf_w2) :: map_w2
      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2,nqp_h,nqp_v) :: basis_w2
      REAL(KIND=r_def), intent(in), dimension(1,ndf_w2,nqp_h,nqp_v) :: diff_basis_w2
      INTEGER, intent(in), dimension(ndf_w2,2) :: boundary_dofs_w2
      INTEGER, intent(in) :: ndf_w3
      INTEGER, intent(in), dimension(ndf_w3) :: map_w3
      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,nqp_h,nqp_v) :: basis_w3
      INTEGER, intent(in) :: ndf_w0
      INTEGER, intent(in), dimension(ndf_w0) :: map_w0
      REAL(KIND=r_def), intent(in), dimension(1,ndf_w0,nqp_h,nqp_v) :: basis_w0
      REAL(KIND=r_def), intent(in), dimension(3,ndf_w0,nqp_h,nqp_v) :: diff_basis_w0
      INTEGER, intent(in) :: nqp_h, nqp_v
      REAL(KIND=r_def), intent(in), dimension(nqp_h) :: wh
      REAL(KIND=r_def), intent(in), dimension(nqp_v) :: wv
    END SUBROUTINE ru_code_code
  END MODULE ru_code_mod

The above example demonstrates that the argument list can get quite
complex. Rather than going through an explanation of each argument you
are referred to Section :ref:`Rules <stub-generation-rules>` for more details
on the rules for argument types and argument ordering. Regarding
naming conventions for arguments you can see that the arrays
associated with the fields are labelled as 1-4 depending on their
position in the metadata. For a vector field, each vector results in a
different array. These are distinguished by appending ``_vx`` where ``x`` is
the number of the vector.

The introduction of stencil operations on field arguments futher complicates
the argument list of a kernel. An example of the use of the stub generator
for a kernel that performs stencil operations is provided in
``examples/dynamo0p3/eg5``.
::

  > python genkernelstub.py ../examples/dynamo/eg5/conservative_flux_kernel_mod.F90

.. _stub-generation-errors:

Errors
------

The stub generator has been written to provide useful errors if
mistakes are found. If you run the generator and it does not produce a
useful error - and in particular if it produces a stack trace - please
contact the PSyclone developers.

The following tests do not produce stub kernel code either because
they are invalid or because they contain functionality that is not
supported in the stub generator.
::

    tests/test_files/dynamo0p3/matrix_vector_mm_mod.f90
    tests/test_files/dynamo0p3/testkern_any_space_1_mod.f90
    tests/test_files/dynamo0p3/testkern_any_space_2_mod.f90
    tests/test_files/dynamo0p3/testkern.F90
    tests/test_files/dynamo0p3/testkern_invalid_fortran.F90
    tests/test_files/dynamo0p3/testkern_no_datatype.F90
    tests/test_files/dynamo0p3/testkern_short_name.F90

``testkern_invalid_fortran.F90``, ``testkern_no_datatype.F90``,
``testkern_short_name.F90``, ``testkern.F90`` and
``matrix_vector_mm_mod.f90`` are designed to be invalid for PSyclone
testing purposes and should produce appropriate errors. For example:
::

    > python genkernelstub.py tests/test_files/dynamo0p3/testkern_invalid_fortran.F90 
    Error: 'Parse Error: Code appears to be invalid Fortran'

``any_space`` is not currently supported in the stub generator so
``testkern_any_space_1_mod.f90`` and ``testkern_any_space_2_mod.f90``
should fail with appropriate warnings because of that. For example:
::

    > python genkernelstub.py tests/test_files/dynamo0p3/testkern_any_space_1_mod.f90
    Error: "Generation Error: Unknown space, expecting one of ['w0', 'w1', 'w2', 'w3',
    'wtheta', 'w2h', 'w2v'] but found 'any_space_1'"
