!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
module matrix_invert_mod

  use constants_mod, only : i_def, r_single, r_double

  implicit none

  private

  ! Public subroutines
  public :: matrix_invert_lu
  public :: matrix_invert_plu
  public :: matrix_invert_3x3
  public :: determinant_3x3

  !------------------------------------------------------------------------------
  ! Generic interface for real32 and real64 types
  !------------------------------------------------------------------------------

   interface matrix_invert_lu
      module procedure  &
         matrix_invert_lu_r_single, &
         matrix_invert_lu_r_double
   end interface

   interface matrix_invert_plu
      module procedure  &
         matrix_invert_plu_r_single, &
         matrix_invert_plu_r_double
   end interface

   interface matrix_invert_3x3
      module procedure  &
         matrix_invert_3x3_r_single, &
         matrix_invert_3x3_r_double
   end interface

   interface determinant_3x3
      module procedure  &
         determinant_3x3_r_single, &
         determinant_3x3_r_double
   end interface

  !------------------------------------------------------------------------------
  ! Contained functions/subroutines
  !------------------------------------------------------------------------------
  contains

   !> Subroutine Computes the inverse of a n x n
   !! matrix, a and returns the inverse in b.
   !! uses Dolittle algorithm for LU factorisation on a small, dense, matrix.
   !! @param[in] a Holds the matrix
   !! @param[out] b Holds the computed inverse
   !! @param[in] n Rank/dim size of the matrix/array

   subroutine matrix_invert_lu_r_single(a,b,n)
     !--------------------------------------------------------------------------
     ! Compute the inverse (b) of n x n matrix a
     ! using LU factorization by Dolittle algorithm
     !--------------------------------------------------------------------------

     implicit none

     integer(kind=i_def), intent(in)  :: n
     real(kind=r_single),    intent(in)  :: a(n,n)
     real(kind=r_single),    intent(out) :: b(n,n)

     integer(kind=i_def) :: i, j, m

     real(kind=r_single)    :: c(n,n), l(n,n), u(n,n)
     real(kind=r_single)    :: x(n), y(n), z(n)
     real(kind=r_single)    :: coeff

     ! Take copy of a and initialise l, u
     do i=1,n
        do j=1,n
           c(i,j) = a(i,j)
           l(i,j) = 0.0_r_single
           u(i,j) = 0.0_r_single
        end do
        z(i) = 0.0_r_single
     end do

     do m=1,n-1
        do i=m+1,n
           coeff = - c(i,m)/c(m,m)
           l(i,m) = -coeff
           ! Zero the column below the diagonal using row operations
           do j=m+1,n
              c(i,j) = c(i,j) + coeff*c(m,j)
           end do
        end do
     end do

     do i=1,n
        ! Set diagonal of l = 1
        l(i,i) = 1.0_r_single
        do j=1,i
           ! u to be what is in c
           u(j,i) = c(j,i)
        end do
     end do

     do m=1,n
        ! Solve Ly = z = I by forward substitution
        ! Since L(i,i) = 1 can remove 1/L(i,i) factor from y(i) computations
        z(m) = 1.0_r_single
        y(1) = z(1)
        do i=2,n
           y(i) = z(i)
           do j=1,i-1
              y(i) = y(i) - y(j)*l(i,j)
           end do
        end do
        ! Solve Ux = y by backward substitution
        x(n) = y(n)/u(n,n)
        do i=n-1,1,-1
           x(i) = y(i)
           do j=n,i+1,-1
              x(i) = x(i) - x(j)*u(i,j)
           end do
           x(i) = x(i)/u(i,i)
        end do
        ! reset z(m) to zero for next pass
        z(m) = 0.0_r_single
        ! Fill x into column of b
        do i=1,n
           b(i,m) = x(i)
        end do
     end do

   end subroutine matrix_invert_lu_r_single

   subroutine matrix_invert_lu_r_double(a,b,n)
     !--------------------------------------------------------------------------
     ! Compute the inverse (b) of n x n matrix a
     ! using LU factorization by Dolittle algorithm
     !--------------------------------------------------------------------------

     implicit none

     integer(kind=i_def), intent(in)  :: n
     real(kind=r_double),    intent(in)  :: a(n,n)
     real(kind=r_double),    intent(out) :: b(n,n)

     integer(kind=i_def) :: i, j, m

     real(kind=r_double)    :: c(n,n), l(n,n), u(n,n)
     real(kind=r_double)    :: x(n), y(n), z(n)
     real(kind=r_double)    :: coeff

     ! Take copy of a and initialise l, u
     do i=1,n
        do j=1,n
           c(i,j) = a(i,j)
           l(i,j) = 0.0_r_double
           u(i,j) = 0.0_r_double
        end do
        z(i) = 0.0_r_double
     end do

     do m=1,n-1
        do i=m+1,n
           coeff = - c(i,m)/c(m,m)
           l(i,m) = -coeff
           ! Zero the column below the diagonal using row operations
           do j=m+1,n
              c(i,j) = c(i,j) + coeff*c(m,j)
           end do
        end do
     end do

     do i=1,n
        ! Set diagonal of l = 1
        l(i,i) = 1.0_r_double
        do j=1,i
           ! u to be what is in c
           u(j,i) = c(j,i)
        end do
     end do

     do m=1,n
        ! Solve Ly = z = I by forward substitution
        ! Since L(i,i) = 1 can remove 1/L(i,i) factor from y(i) computations
        z(m) = 1.0_r_double
        y(1) = z(1)
        do i=2,n
           y(i) = z(i)
           do j=1,i-1
              y(i) = y(i) - y(j)*l(i,j)
           end do
        end do
        ! Solve Ux = y by backward substitution
        x(n) = y(n)/u(n,n)
        do i=n-1,1,-1
           x(i) = y(i)
           do j=n,i+1,-1
              x(i) = x(i) - x(j)*u(i,j)
           end do
           x(i) = x(i)/u(i,i)
        end do
        ! reset z(m) to zero for next pass
        z(m) = 0.0_r_double
        ! Fill x into column of b
        do i=1,n
           b(i,m) = x(i)
        end do
     end do

   end subroutine matrix_invert_lu_r_double

   subroutine matrix_invert_plu_r_single(a,b,n)
     !--------------------------------------------------------------------------
     ! Compute the inverse (b) of n x n matrix a using PLU factorization by
     ! modified Dolittle algorithm. Assumes A is non-singular.
     !--------------------------------------------------------------------------

     implicit none

     integer(kind=i_def), intent(in)  :: n
     real(kind=r_single), intent(in)  :: a(n,n)
     real(kind=r_single), intent(out) :: b(n,n)

     integer(kind=i_def) :: i, j, m
     integer(kind=i_def) :: col_argmax(n)

     real(kind=r_single) :: c(n,n), l(n,n), u(n,n), p(n,n)
     real(kind=r_single) :: x(n), y(n), z(n), row(n)
     real(kind=r_single) :: coeff

     ! Take copy of a and initialise l, u and p
     do i = 1, n
        do j = 1, n
           c(i,j) = a(i,j)
           l(i,j) = 0.0_r_single
           u(i,j) = 0.0_r_single
           if (i == j) then
              p(i,j) = 1.0_r_single
           else
              p(i,j) = 0.0_r_single
           end if
        end do
        z(i) = 0.0_r_single
     end do

     do m = 1, n-1
        ! Permute row if required: we place the largest absolute value in
        ! c(m:n,m) on the diagonal
        col_argmax(:) = maxloc(abs(c(m:n, m)), 1) + m - 1
        if (col_argmax(1) /= m) then
           row(:) = c(m,:)
           c(m,:) = c(col_argmax(1),:)
           c(col_argmax(1),:) = row(:)

           row(:) = p(m,:)
           p(m,:) = p(col_argmax(1),:)
           p(col_argmax(1),:) = row(:)

           row(:) = l(m,:)
           l(m,:) = l(col_argmax(1),:)
           l(col_argmax(1),:) = row(:)
        end if
        do i = m+1, n
           ! We have performed Gaussian elimination on the first m-1 columns.
           ! The top left sub-matrix is upper triangular with a non-zero
           ! diagonal, and the bottom left sub-matrix is zero.
           ! det(a) /= 0 implies the bottom right sub-matrix is non-singular,
           ! so c(m:n,m) is non-zero and its maximum value is at c(m,m).
           ! Division is safe:
           coeff = -c(i,m) / c(m,m)
           l(i,m) = -coeff
           ! Zero the column below the diagonal with row operations
           do j = m+1, n
              c(i,j) = c(i,j) + coeff*c(m,j)
           end do
        end do
     end do

     do i = 1, n
        ! Set diagonal of l = 1
        l(i,i) = 1.0_r_single
        do j = 1, i
           ! u to be what is in c
           u(j,i) = c(j,i)
        end do
     end do

     do m=1,n
        ! Solve Ly = z = I by forward substitution
        ! Since L(i,i) = 1 can remove 1/L(i,i) factor from y(i) computations
        z(:) = p(:,m)
        y(1) = z(1)
        do i = 2, n
           y(i) = z(i)
           do j = 1, i-1
              y(i) = y(i) - y(j)*l(i,j)
           end do
        end do
        ! Solve Ux = y by backward substitution. u(n,n) = c(n,n) /= 0
        x(n) = y(n)/u(n,n)
        do i = n-1, 1, -1
           x(i) = y(i)
           do j = n, i+1, -1
              x(i) = x(i) - x(j)*u(i,j)
           end do
           x(i) = x(i)/u(i,i)
        end do
        ! Fill x into column of b
        do i = 1, n
           b(i,m) = x(i)
        end do
     end do

   end subroutine matrix_invert_plu_r_single

   subroutine matrix_invert_plu_r_double(a,b,n)
     !--------------------------------------------------------------------------
     ! Compute the inverse (b) of n x n matrix a using PLU factorization by
     ! modified Dolittle algorithm. Assumes A is non-singular.
     !--------------------------------------------------------------------------

     implicit none

     integer(kind=i_def), intent(in)  :: n
     real(kind=r_double), intent(in)  :: a(n,n)
     real(kind=r_double), intent(out) :: b(n,n)

     integer(kind=i_def) :: i, j, m
     integer(kind=i_def) :: col_argmax(n)

     real(kind=r_double) :: c(n,n), l(n,n), u(n,n), p(n,n)
     real(kind=r_double) :: x(n), y(n), z(n), row(n)
     real(kind=r_double) :: coeff

     ! Take copy of a and initialise l, u and p
     do i = 1, n
        do j = 1, n
           c(i,j) = a(i,j)
           l(i,j) = 0.0_r_single
           u(i,j) = 0.0_r_single
           if (i == j) then
              p(i,j) = 1.0_r_single
           else
              p(i,j) = 0.0_r_single
           end if
        end do
        z(i) = 0.0_r_single
     end do

     do m = 1, n-1
        ! Permute row if required: we place the largest absolute value in
        ! c(m:n,m) on the diagonal
        col_argmax(:) = maxloc(abs(c(m:n, m)), 1) + m - 1
        if (col_argmax(1) /= m) then
           row(:) = c(m,:)
           c(m,:) = c(col_argmax(1),:)
           c(col_argmax(1),:) = row(:)

           row(:) = p(m,:)
           p(m,:) = p(col_argmax(1),:)
           p(col_argmax(1),:) = row(:)

           row(:) = l(m,:)
           l(m,:) = l(col_argmax(1),:)
           l(col_argmax(1),:) = row(:)
        end if
        do i = m+1, n
           ! We have performed Gaussian elimination on the first m-1 columns.
           ! The top left sub-matrix is upper triangular with a non-zero
           ! diagonal, and the bottom left sub-matrix is zero.
           ! det(a) /= 0 implies the bottom right sub-matrix is non-singular,
           ! so c(m:n,m) is non-zero and its maximum value is at c(m,m).
           ! Division is safe:
           coeff = -c(i,m) / c(m,m)
           l(i,m) = -coeff
           ! Zero the column below the diagonal with row operations
           do j = m+1, n
              c(i,j) = c(i,j) + coeff*c(m,j)
           end do
        end do
     end do

     do i = 1, n
        ! Set diagonal of l = 1
        l(i,i) = 1.0_r_single
        do j = 1, i
           ! u to be what is in c
           u(j,i) = c(j,i)
        end do
     end do

     do m=1,n
        ! Solve Ly = z = I by forward substitution
        ! Since L(i,i) = 1 can remove 1/L(i,i) factor from y(i) computations
        z(:) = p(:,m)
        y(1) = z(1)
        do i = 2, n
           y(i) = z(i)
           do j = 1, i-1
              y(i) = y(i) - y(j)*l(i,j)
           end do
        end do
        ! Solve Ux = y by backward substitution. u(n,n) = c(n,n) /= 0
        x(n) = y(n)/u(n,n)
        do i = n-1, 1, -1
           x(i) = y(i)
           do j = n, i+1, -1
              x(i) = x(i) - x(j)*u(i,j)
           end do
           x(i) = x(i)/u(i,i)
        end do
        ! Fill x into column of b
        do i = 1, n
           b(i,m) = x(i)
        end do
     end do

   end subroutine matrix_invert_plu_r_double

   !---------------------------------------------------------------------
   !> @details Computes the inverse of 3x3 matrix using the exact formula
   !> @param[in] a Holds the values of the matrix to be inverted
   !> @result a_inv Holds the values of the computed inverse

   function matrix_invert_3x3_r_single(A) result ( A_inv )

     use constants_mod, only: r_single

     implicit none
     real(kind=r_single), intent(in) :: A(3,3)
     real(kind=r_single)             :: A_inv(3,3)
     real(kind=r_single)             :: det_inv

     ! Form inverse determinant
     det_inv = (A(1,1)*(A(2,2)*A(3,3) - A(2,3)*A(3,2)) &
              - A(1,2)*(A(2,1)*A(3,3) - A(2,3)*A(3,1)) &
              + A(1,3)*(A(2,1)*A(3,2) - A(3,1)*A(2,2)))
     det_inv = 1.0_r_single/det_inv

     ! Form inverse
     A_inv(1,1) =  (A(2,2)*A(3,3) - A(2,3)*A(3,2))*det_inv
     A_inv(1,2) = -(A(1,2)*A(3,3) - A(1,3)*A(3,2))*det_inv
     A_inv(1,3) =  (A(1,2)*A(2,3) - A(1,3)*A(2,2))*det_inv
     A_inv(2,1) = -(A(2,1)*A(3,3) - A(2,3)*A(3,1))*det_inv
     A_inv(2,2) =  (A(1,1)*A(3,3) - A(1,3)*A(3,1))*det_inv
     A_inv(2,3) = -(A(1,1)*A(2,3) - A(1,3)*A(2,1))*det_inv
     A_inv(3,1) =  (A(2,1)*A(3,2) - A(2,2)*A(3,1))*det_inv
     A_inv(3,2) = -(A(1,1)*A(3,2) - A(1,2)*A(3,1))*det_inv
     A_inv(3,3) =  (A(1,1)*A(2,2) - A(1,2)*A(2,1))*det_inv

     return
   end function matrix_invert_3x3_r_single

   function matrix_invert_3x3_r_double(A) result ( A_inv )

     use constants_mod, only: r_double

     implicit none
     real(kind=r_double), intent(in) :: A(3,3)
     real(kind=r_double)             :: A_inv(3,3)
     real(kind=r_double)             :: det_inv

     ! Form inverse determinant
     det_inv = (A(1,1)*(A(2,2)*A(3,3) - A(2,3)*A(3,2)) &
              - A(1,2)*(A(2,1)*A(3,3) - A(2,3)*A(3,1)) &
              + A(1,3)*(A(2,1)*A(3,2) - A(3,1)*A(2,2)))
     det_inv = 1.0_r_double/det_inv

     ! Form inverse
     A_inv(1,1) =  (A(2,2)*A(3,3) - A(2,3)*A(3,2))*det_inv
     A_inv(1,2) = -(A(1,2)*A(3,3) - A(1,3)*A(3,2))*det_inv
     A_inv(1,3) =  (A(1,2)*A(2,3) - A(1,3)*A(2,2))*det_inv
     A_inv(2,1) = -(A(2,1)*A(3,3) - A(2,3)*A(3,1))*det_inv
     A_inv(2,2) =  (A(1,1)*A(3,3) - A(1,3)*A(3,1))*det_inv
     A_inv(2,3) = -(A(1,1)*A(2,3) - A(1,3)*A(2,1))*det_inv
     A_inv(3,1) =  (A(2,1)*A(3,2) - A(2,2)*A(3,1))*det_inv
     A_inv(3,2) = -(A(1,1)*A(3,2) - A(1,2)*A(3,1))*det_inv
     A_inv(3,3) =  (A(1,1)*A(2,2) - A(1,2)*A(2,1))*det_inv

     return
   end function matrix_invert_3x3_r_double

   !---------------------------------------------------------------------
   !> @details Computes the determinant of 3x3 matrix using the exact formula
   !> @param[in] j Holds the values of the matrix to be inverted
   !> @result determinant_3x3 Holds the values of the computed determinant

   pure real(kind=r_single) function determinant_3x3_r_single(j)

     implicit none

     real(kind=r_single), intent(in) :: j(3,3)

     determinant_3x3_r_single = &
                       j(1,1)*(j(2,2)*j(3,3) - j(2,3)*j(3,2)) &
                     - j(1,2)*(j(2,1)*j(3,3) - j(2,3)*j(3,1)) &
                     + j(1,3)*(j(2,1)*j(3,2) - j(2,2)*j(3,1))

   end function determinant_3x3_r_single

   pure real(kind=r_double) function determinant_3x3_r_double(j)

     implicit none

     real(kind=r_double), intent(in) :: j(3,3)

     determinant_3x3_r_double = &
                       j(1,1)*(j(2,2)*j(3,3) - j(2,3)*j(3,2)) &
                     - j(1,2)*(j(2,1)*j(3,3) - j(2,3)*j(3,1)) &
                     + j(1,3)*(j(2,1)*j(3,2) - j(2,2)*j(3,1))

   end function determinant_3x3_r_double

end module matrix_invert_mod
