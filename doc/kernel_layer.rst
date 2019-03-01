.. _kernel-layer:

Kernel layer
============

In the PSyKAl separation of concerns, Kernel code (code which is
created to run within the Kernel layer), works over a subset of a
field (such as a column). The reason for doing this is that it gives
the PSy layer the responsibility of calling the Kernel over the
spatial domain which is where parallelism is typically exploited in
finite element and finite difference codes. The PSy layer is therefore
able to call the kernel layer in a flexible way (blocked and/or in
parallel for example). Kernel code in the kernel layer is not allowed
to include any parallelisation calls or directives and works on
raw fortran arrays (to allow the compiler to optimise the code).

Since a Kernel is called over the spatial domain (by the PSy layer) it
must take at least one field or operator as an argument.

API
---

Kernels in the kernel layer are implemented as subroutines within
fortran modules. One or more kernel modules are allowed, each of which
can contain one or more kernel subroutines. In the example below there
is one module ``integrate_one_module`` which contains one kernel
subroutine ``integrate_one_code``. The kernel subroutines contain the
code that operates over a subset of the field (such as a column).

Metadata describing the kernel subroutines is required by the PSyclone
system to generate appropriate PSy layer code. The metadata is written
by the kernel developer and is kept with the kernel code in the same
module using a sub-type of the ``kernel_type`` type. In the example
below the ``w3_solver_kernel_type`` type specifies the appropriate
metadata information describing the kernel code for the
``dynamo0.3`` api::

  module w3_solver_kernel_mod

    use kernel_mod,      only : kernel_type
    use constants_mod,   only : r_def
    use argument_mod,    only : arg_type, func_type,                 &
                                GH_FIELD, GH_READ, GH_WRITE, W0, W3, &
                                GH_BASIS, GH_DIFF_BASIS, CELLS 
    implicit none

    type, public, extends(kernel_type) :: w3_solver_kernel_type
      private
      type(arg_type) :: meta_args(4) = (/                &
           arg_type(GH_FIELD,   GH_WRITE, W3),           &
           arg_type(GH_FIELD,   GH_READ,  W3),           &
           arg_type(GH_FIELD*3, GH_READ,  W0),           &
           arg_type(GH_REAL,    GH_READ)                 &
           /)
      type(func_type) :: meta_funcs(2) = (/              &
           func_type(W3, GH_BASIS),                      &
           func_type(W0, GH_DIFF_BASIS)                  &
           /)
      integer :: gh_shape = gh_quadrature_XYoZ
      integer :: iterates_over = CELLS
    contains
      procedure, nopass :: solver_w3_code
    end type
  
  contains
  
    subroutine solver_w3_code(nlayers,                                 &
                              x, rhs,                                  &
                              chi_1, chi_2, chi_3, ascalar,            &
                              ndf_w3, undf_w3, map_w3, w3_basis,       &
                              ndf_w0, undf_w0, map_w0, w0_diff_basis,  &
                              nqp_h, nqp_v, wqp_h, wqp_v)
      ...
    end subroutine solver_w3_code
  
  end module w3_solver_kernel_mod

Metadata
--------

Kernel metadata is not required if the PSy layer is going to be
written manually - its sole purpose is to let PSyclone know how to
generate the PSy layer. The content of Kernel metadata differs
depending on the particular API and this information can be found in
the API-specific sections of this document.

In all APIs the kernel metadata is implemented as an extension of the
`kernel_type` type. The reason for using a type to specify metadata is
that it allows the metadata to be kept with the code and for it to be
compilable. In addition, currently all APIs will contain information
about the arguments in an array called ``meta_args``, a specification
of what the kernel code iterates over in a variable called
``iterates_over`` and a reference to the kernel code as a type bound
procedure.

::

    type, extends(kernel_type) :: integrate_one_kernel
      ...
      type(...) :: meta_args(...) = (/ ... /)
      ...
      integer :: ITERATES_OVER = ...
      ...
      contains
      ...
      procedure ...
      ...
    end type integrate_one_kernel
