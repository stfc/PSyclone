!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!> @brief Provides access to the members of the psy class.

!> @details Accessor functions for the psy class are defined in this module.

!> @param invoke_RHS_V3              Invoke the RHS for a v3 field
!> @param invoke_v3_solver_kernel    Invoke the solver for a v3 field kernel

module psy

  use field_mod, only : field_type, field_proxy_type
  use lfric
  use omp_lib

  implicit none

contains

  subroutine invoke_rhs_v3( right_hand_side )

    use v3_rhs_kernel_mod, only : rhs_v3_code

    implicit none

    type( field_type ),  intent( in ) :: right_hand_side

    type( field_proxy_type)           :: right_hand_side_proxy
    integer :: cell
    integer, pointer :: map(:)
    integer :: ndf
    real(kind=dp), pointer  :: basis(:,:,:,:)

    right_hand_side_proxy = right_hand_side%get_proxy()
    ! Unpack data

    ndf = right_hand_side_proxy%vspace%get_ndf()
    basis => right_hand_side_proxy%vspace%get_basis()

!$omp parallel do default(shared), private(cell, map)
    do cell = 1, right_hand_side_proxy%ncell
!       write(*,'("V3K, thread=",I1,":cell=",I2)') omp_get_thread_num(),cell
       map => right_hand_side_proxy%vspace%get_cell_dofmap( cell )
       call rhs_v3_code( right_hand_side_proxy%nlayers, &
                         ndf, &
                         map, &
                         basis, &
                         right_hand_side_proxy%data, &
                         right_hand_side_proxy%gaussian_quadrature )
    end do
!$omp end parallel do
  end subroutine invoke_rhs_v3

  subroutine invoke_v3_solver_kernel( pdfield, rhs )

    use v3_solver_kernel_mod, only : solver_v3_code

    type( field_type ), intent( in ) :: pdfield
    type( field_type ), intent( in ) :: rhs

    integer                 :: cell
    integer, pointer        :: map(:)
    integer                 :: ndf
    real(kind=dp), pointer  :: basis(:,:,:,:)

    type( field_proxy_type )        :: pd_proxy
    type( field_proxy_type )        :: rhs_proxy 

    pd_proxy  = pdfield%get_proxy()
    rhs_proxy = rhs%get_proxy()

    ndf    = pd_proxy%vspace%get_ndf( )
    basis => pd_proxy%vspace%get_basis()

!$omp parallel do default(shared), private(cell, map)    
    do cell = 1, pd_proxy%ncell
       map=>pd_proxy%vspace%get_cell_dofmap(cell)
       call solver_v3_code( pd_proxy%nlayers, &
                            ndf, &
                            map, &
                            basis, &
                            pd_proxy%data, &
                            rhs_proxy%data, &
                            pd_proxy%gaussian_quadrature )
    end do
!$omp end parallel do
  end subroutine invoke_v3_solver_kernel

  subroutine invoke_rhs_v2( right_hand_side )

    use v2_kernel_mod, only : rhs_v2_code

    implicit none

    type( field_type ), intent( inout ) :: right_hand_side

    type( field_proxy_type)             :: rhs_proxy
    integer :: cell
    integer, pointer :: map(:)
    integer :: ndf
    real(kind=dp), pointer  :: basis(:,:,:,:)

    integer                 :: ncolour, colour
    integer, pointer        :: ncp_colour(:)
    integer, pointer        :: cmap(:,:)

    rhs_proxy = right_hand_side%get_proxy()
    ! Unpack data
    ndf = rhs_proxy%vspace%get_ndf()
    basis => rhs_proxy%vspace%get_basis()
    call rhs_proxy%vspace%get_colours(ncolour,ncp_colour,cmap)

    do colour = 1, ncolour
!    do cell = 1, rhs_proxy%ncell
!$omp parallel do default(shared), private(cell,map)
       do cell = 1, ncp_colour(colour)
!          write(*,*) 'v2:',colour,omp_get_thread_num(),cell
!          map=> rhs_proxy%vspace%get_cell_dofmap(cell)
          map => rhs_proxy%vspace%get_cell_dofmap(cmap(colour,cell))
          call rhs_v2_code( rhs_proxy%nlayers, &
               ndf, &
               map, &
               basis, &
               rhs_proxy%data, &
               rhs_proxy%gaussian_quadrature )
       end do
!$omp end parallel do
    end do
  end subroutine invoke_rhs_v2

  subroutine invoke_rhs_v1( rhs )

    use v1_kernel_mod, only : rhs_v1_code

    implicit none

    type( field_type ), intent( inout ) :: rhs

    type( field_proxy_type) :: rhs_p
    integer :: cell
    integer, pointer :: map(:)
    integer :: ndf
    real(kind=dp), pointer  :: basis(:,:,:,:)
    integer                 :: ncolour, colour
    integer, pointer        :: ncp_colour(:)
    integer, pointer        :: cmap(:,:)


    rhs_p = rhs%get_proxy()
    ! Unpack data
    ndf = rhs_p%vspace%get_ndf()
    basis=>rhs_p%vspace%get_basis()
    call rhs_p%vspace%get_colours(ncolour,ncp_colour,cmap)

    do colour = 1, ncolour
!    do cell = 1, rhs_proxy%ncell
!$omp parallel do default(shared), private(cell,map)
       do cell = 1, ncp_colour(colour)
          !          write(*,*) 'v2:',colour,omp_get_thread_num(),cell
          !          map=> rhs_p%vspace%get_cell_dofmap(cell)
          map => rhs_p%vspace%get_cell_dofmap(cmap(colour,cell))
          call rhs_v1_code( rhs_p%nlayers, &
               ndf, &
               map, &
               basis, &
               rhs_p%data, &
               rhs_p%gaussian_quadrature )
       end do
 !$omp end parallel do
    end do
  end subroutine invoke_rhs_v1

  subroutine invoke_matrix_vector(x,Ax)
    use matrix_vector_mod, only : matrix_vector_code
    type(field_type), intent(inout) :: x
    type(field_type), intent(inout) :: Ax

    integer                 :: cell
    integer, pointer        :: map(:)
    integer                 :: ndf
    real(kind=dp), pointer  :: basis(:,:,:,:)

    type( field_proxy_type )        :: x_p
    type( field_proxy_type )        :: Ax_p

    integer                 :: ncolour, colour
    integer, pointer        :: ncp_colour(:)
    integer, pointer        :: cmap(:,:)

    x_p = x%get_proxy()
    Ax_p = Ax%get_proxy()

    ndf     = x_p%vspace%get_ndf( )
    basis  => x_p%vspace%get_basis()

    call x_p%vspace%get_colours(ncolour,ncp_colour,cmap)

    do colour = 1, ncolour
!$omp parallel do default(shared), private(cell,map)
       do cell = 1, ncp_colour(colour)
!          write(*,*) 'v2:',colour,omp_get_thread_num(),cell
          map => x_p%vspace%get_cell_dofmap(cmap(colour,cell))
          
          !    do cell = 1, x_p%ncell
!          map=>x_p%vspace%get_cell_dofmap(cell)
          call matrix_vector_code( x_p%nlayers, &
               ndf, &
               map, &
               basis, &
               x_p%data, &
               Ax_p%data, &
               x_p%gaussian_quadrature )
       end do
!$omp end parallel do       
    end do
  end subroutine invoke_matrix_vector

  real(kind=dp) function inner_prod(x,y)
    use log_mod, only : log_event, LOG_LEVEL_ERROR
    implicit none
    type( field_type ), intent( inout ) :: x,y
    type( field_proxy_type)             ::  x_p,y_p

    integer                          :: i

    x_p = x%get_proxy()
    y_p = y%get_proxy()


    !sanity check
    if(x_p%undf .ne. y_p%undf ) then
       ! they are not on the same function space
       call log_event("Psy:inner_prod:x and y live on different w-spaces",LOG_LEVEL_ERROR)
       !abort
       stop
    endif

    inner_prod = 0.0_dp
!$omp parallel do default(shared), private(i), reduction(+:inner_prod)
    do i = 1,x_p%undf
       inner_prod = inner_prod + ( x_p%data(i) * y_p%data(i) )
    end do
!$omp end parallel do
  end function inner_prod
    
end module psy
