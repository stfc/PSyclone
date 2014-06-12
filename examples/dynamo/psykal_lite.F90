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
  use lfric
implicit none

contains
  subroutine invoke_rhs_v3(rhs)
    use v3_kernel_mod,        only : rhs_v3_code
    type(field_type), intent(inout) :: rhs
    integer :: cell
    integer, pointer :: map(:)
    integer :: nlayers
    integer :: ndf
    real(kind=dp), pointer  :: v3_basis(:,:,:,:,:)

! Unpack data
    nlayers=rhs%get_nlayers()
    ndf = rhs%vspace%get_ndf()

    call rhs%vspace%get_basis(v3_basis)
    do cell = 1, rhs%get_ncell()
       call rhs%vspace%get_cell_dofmap(cell,map)
       call rhs_v3_code(nlayers,ndf,map,v3_basis,rhs%data,rhs%gaussian_quadrature)
    end do

  end subroutine invoke_rhs_v3

  subroutine invoke_v3_solver_kernel(pdfield,rhs)
    use v3_solver_kernel_mod, only : solver_v3_code
    type(field_type), intent(inout) :: pdfield
    type(field_type), intent(in)    :: rhs       
    integer :: cell
    integer, pointer :: map(:)
    integer :: nlayers
    integer :: ndf
    real(kind=dp), pointer  :: v3_basis(:,:,:,:,:)    
        
    nlayers=pdfield%get_nlayers()
    ndf = pdfield%vspace%get_ndf()
    call pdfield%vspace%get_basis(v3_basis)
    
    do cell = 1, pdfield%get_ncell()
       call pdfield%vspace%get_cell_dofmap(cell,map)
       call solver_v3_code(nlayers,ndf,map,v3_basis,pdfield%data,rhs%data,pdfield%gaussian_quadrature)
    end do

  end subroutine invoke_v3_solver_kernel

end module psy
