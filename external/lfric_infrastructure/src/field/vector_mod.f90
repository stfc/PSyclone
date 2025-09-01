










!-------------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-------------------------------------------------------------------------------

!>@brief Abstract vector type for the new solver API and
!! extended vector types for particular solvers.

module vector_mod
  use constants_mod, only : i_def, r_def

  implicit none
  private

  !> @brief abstract vector type to define the interfaces for the solver API
  type, public, abstract :: abstract_vector_type
     private
   contains
     !> set the vector to a scalar value
     !> @param[in] scalar  real the scalar value
     procedure (set_scalar_interface),    deferred :: set_scalar
     !> Compute y = y + alpha * x
     !> @param[in] alpha  real
     !> @param[inout] x  vector
     procedure (axpy_interface),          deferred :: axpy
     !> Compute y = alpha*y + x
     !> @param[in] alpha  real
     !> @param[inout] x  vector
     procedure (aypx_interface),          deferred :: aypx
     !> Compute z = a*x + b*y
     !> @param[in] a  real
     !> @param[in] x  vector
     !> @param[in] b  real
     !> @param[in] y  vector
     procedure (axpby_interface),          deferred :: axpby
     !> Compute norm of the field vector, returns a real scalar
     !! n = sqrt( sum_i( v_i*v_i ))
     procedure (norm_interface),          deferred :: norm
     !> Compute the dot product of two vectors returns a real scalar
     !> @param[in] x vector to dot self with
     procedure (field_norm_interface),    deferred :: field_norm
     !> Compute the dot product of two fields in the vector returns a real scalar
     !> @param[in] x vector to dot self with
     !> @param[in] n index of field to norm
     procedure (dot_interface),           deferred :: dot
     !> multiply a vector by a scalar
     !> @param [in] scalar real
     procedure (scale_interface),         deferred :: scale
     !> Create a vector of this type
     !> param[in] vec, the vector to be created
     procedure (duplicate_interface),     deferred :: duplicate
     !> copy a vector of this type
     !> param[in] source, the vector to be copied
     procedure (copy_interface),          deferred :: copy
     !> Return the size of the vector
     procedure (vector_size_interface),   deferred :: vector_size

  end type abstract_vector_type

  abstract interface
     !> set the vector to a scalar value
     !> @param[in] scalar  real the scalar value
     subroutine set_scalar_interface(self, scalar)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(inout) :: self
       real(kind=r_def),            intent(in)    :: scalar
     end subroutine set_scalar_interface
  end interface

  abstract interface
     !> Compute y = y + alpha * x
     !> @param[in] alpha  real
     !> @param[inout] x  vector
     subroutine axpy_interface(self, alpha, x)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(inout) :: self
       real(kind=r_def),            intent(in)    :: alpha
       class(abstract_vector_type), intent(inout) :: x
     end subroutine axpy_interface
  end interface

  abstract interface
     !> Compute y = alpha*y + x
     !> @param[in] alpha  real
     !> @param[inout] x  vector
     subroutine aypx_interface(self, alpha, x)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(inout) :: self
       real(kind=r_def),            intent(in)    :: alpha
       class(abstract_vector_type), intent(inout) :: x
     end subroutine aypx_interface
  end interface

  abstract interface
     !> Compute z = a*x + b*y
     !> @param[in] a  real
     !> @param[in] x  vector
     !> @param[in] a  real
     !> @param[in] x  vector
     subroutine axpby_interface(self, a, x, b, y)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(inout) :: self
       real(kind=r_def),            intent(in)    :: a, b
       class(abstract_vector_type), intent(in)    :: x, y
     end subroutine axpby_interface
  end interface

  abstract interface
     !> Compute norm of the field vector, returns a real scalar
     !! n = sqrt( sum_i( v_i*v_i ))
     function norm_interface(self) result(normal)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(in) :: self
       real(kind=r_def) :: normal
     end function  norm_interface
  end interface

  abstract interface
     !> Compute norm of the field vector, returns a real scalar
     !! n = sqrt( sum_i( v_i*v_i ))
     function field_norm_interface(self, n) result(normal)
       import :: abstract_vector_type
       import :: r_def
       import :: i_def
       class(abstract_vector_type), intent(in) :: self
       integer(kind=i_def),         intent(in) :: n
       real(kind=r_def)                        :: normal
     end function  field_norm_interface
  end interface

  abstract interface
     !> Compute the dot product of two vectors returns a real scalar
     !> @param[in] x vector to dot self with
     function dot_interface(self, x) result(dot_prod)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(in) :: self
       class(abstract_vector_type), intent(in) :: x
       real(kind=r_def)                        :: dot_prod
     end function dot_interface
  end interface

  abstract interface
     !> multiply a vector by a scalar
     !> @param [in] scalar real
     subroutine scale_interface(self, scalar)
       import :: abstract_vector_type
       import :: r_def
       class(abstract_vector_type), intent(inout) :: self
       real(kind=r_def),            intent(in)    :: scalar
     end subroutine scale_interface
  end interface

  abstract interface
     !> Create a vector of this type
     !> param[in] vec, the vector to be created
     subroutine duplicate_interface(self, vec)
       import :: abstract_vector_type
       class(abstract_vector_type), intent(in)                 :: self
       class(abstract_vector_type), allocatable, intent(inout) :: vec
     end subroutine duplicate_interface
  end interface

  abstract interface
     !> copy a vector of this type
     !> param[in] source, the vector to be copied
     subroutine copy_interface(self, source)
       import :: abstract_vector_type
       class(abstract_vector_type), intent(out) :: self
       class(abstract_vector_type), intent(in)  :: source
     end subroutine copy_interface
  end interface

  abstract interface
     !> Return the size of the vectoy
     !> param[in] source, the vector to be copied
     function vector_size_interface(self) result(n_fields)
       import :: abstract_vector_type
       import :: i_def
       class(abstract_vector_type), intent(in) :: self
       integer(i_def)                          :: n_fields
     end function vector_size_interface
  end interface
end module vector_mod
