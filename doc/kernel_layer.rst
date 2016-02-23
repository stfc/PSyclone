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
below the ``integrate_one_kernel`` type specifies the appropriate
metadata information describing the kernel code for the
``gunghoproto`` api.

::

  module integrate_one_module
    use kernel_mod
    implicit none
    
    private
    public integrate_one_kernel
    public integrate_one_code
    
    type, extends(kernel_type) :: integrate_one_kernel
      type(arg) :: meta_args(2) = (/&
           arg(READ, (CG(1)*CG(1))**3, FE), &
           arg(SUM, R, FE)/)
      integer :: ITERATES_OVER = CELLS
      contains
      procedure, nopass :: code => integrate_one_code
    end type integrate_one_kernel
  
  contains
  
    subroutine integrate_one_code(layers, p1dofm, X, R)
      integer, intent(in) :: layers
      integer, intent(in) :: p1dofm(6)
      real(dp), intent(in) :: X(3,*)
      real(dp), intent(inout) :: R
    end subroutine integrate_one_code
  
  end module integrate_one_module

Metadata
--------

Kernel metadata is not required if the PSy layer is going to be
written manually, its sole purpose is to let PSyclone know how to
generate the PSy layer. The content of Kernel metadata differs
depending on the particular API and this information can be found in
the API-specific sections of this document.

In all API's the kernel metadata is implemented as an extension of the
`kernel_type` type. The reason for using a type to specify metadata is
that it allows the metadata to be kept with the code and for it to be
compilable. In addition, currently all API's will contain information
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
