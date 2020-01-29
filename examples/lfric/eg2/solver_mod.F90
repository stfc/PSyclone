
!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown, 
! Met Office and NERC 2014. 
! However, it has been created with the help of the GungHo Consortium, 
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
!
!> @brief Solver algortithm - Krylov subspace iterative solver
!!
!! @details Only BiCGstab so far but can contain other solvers so not typing at the momoent
!! Simply contains a single subroutine
module solver_mod
  use constants_mod,   only : dp, max_iter
  use log_mod,         only : log_event, LOG_LEVEL_INFO, LOG_LEVEL_ERROR, &
                              LOG_LEVEL_DEBUG
  use field_mod,       only : field_type, field_proxy_type                 
                                   
  use psy,             only : inner_prod, invoke_matrix_vector

  implicit none
  private

  public :: solver_algorithm

contains
!> @brief BiCGStab solver with no preconditioning. 
!! @details solves A.x = b where the operation A.x is encoded in a kernel
!! @param rhs_field_p The input b
!! @param lhs_field_p The answser, x
  subroutine solver_algorithm(lhs, rhs)
    implicit none
    type(field_type), intent(inout)    :: lhs
    type(field_type), intent(inout)    :: rhs

    character(len=80)               :: cmessage
    ! The temporary fields
    type(field_type)                :: Ax, res, cr, p, v, s, t

    ! the scalars 
    ! the greeks - standard BiCGStab
    real(kind=dp)                    :: rho,alpha,omega,beta, norm
    real(kind=dp)                   :: ts,tt
    ! others
    real(kind=dp)                    :: err,sc_err, tol, init_err
    integer                          :: iter

   tol = 1.0e-8
    ! compute the residual this is a global sum to the PSy ---
   sc_err = inner_prod(rhs,rhs)
   sc_err = sqrt(sc_err)
   write(cmessage,'("solver_algorithm: starting ... ||b|| = ",E15.8)') sc_err
   call log_event(trim(cmessage), LOG_LEVEL_INFO)
   call lhs%set_field_scalar(0.0_dp)

    
    ! call the copy constructor on the field_type    
   Ax = field_type(rhs)
   call Ax%set_field_scalar(0.0_dp)

    ! call into the psy layer with the new_proxy method
   call invoke_matrix_vector(Ax,lhs )
   err = inner_prod(Ax,Ax)

   res = field_type(rhs)

   call res%minus_field_data(rhs, Ax)

   err = inner_prod(res,res)
   err = sqrt(err)/sc_err
   init_err=err
   
   alpha  = 1.0
   omega  = 1.0
   norm   = 1.0
   
   cr = field_type(rhs)
   call cr%copy_field_data(res)
   
   p = field_type(rhs)
   call p%set_field_scalar(0.0_dp)

   v = field_type(rhs)
   call v%set_field_scalar(0.0_dp)

   t = field_type(rhs)
   s = field_type(rhs)

   do iter = 1, max_iter

      rho = inner_prod(cr,res)
      beta = (rho/norm) * (alpha/omega)
      call t%axpy( (-beta*omega), v, res )
      
!      ! this is where the preconitioner would go
      call s%copy_field_data(t)
      call p%axpy( beta, p, s)
      call v%set_field_scalar(0.0_dp)
      call invoke_matrix_vector(v,p)
      
      norm = inner_prod(cr,v)
      alpha = rho/norm
      call s%axpy(-alpha, v, res)

      !precon cs, s - again no preconditioner
      ! either use a cs or zero t first as its an inc field!
      call t%set_field_scalar(0.0_dp)
      call invoke_matrix_vector(t,s)

      tt = inner_prod(t,t)
      ts = inner_prod(t,s)

      omega = ts/tt

!      lhs = lhs + omega * s + alpha * p
      call lhs%axpy(omega,s,lhs)
      call lhs%axpy(alpha,p,lhs)
      call res%axpy(-omega,t,s)
      norm = rho

      ! check for convergence
      err = inner_prod(res,res)
      err = sqrt(err)/sc_err

      write(cmessage,'("solver_algorithm[",I2,"]: res = ", E15.8)')        &
           iter, err
      call log_event(trim(cmessage), LOG_LEVEL_DEBUG)

      if (err < tol) then 
         write(cmessage,'("solver_algorithm:converged in ", I2," iters, init=",E12.4," final=",E15.8)') iter,init_err,err
         call log_event(trim(cmessage),LOG_LEVEL_INFO)
         exit
      end if 
   end do

   if(iter.ge.max_iter) then
      write(cmessage,'("solver_algortihm: NOT converged in", I3," iters, Res=",E15.8)') &
           iter, err
      call log_event(trim(cmessage),LOG_LEVEL_ERROR)
      call log_event(" ... time to flee, bye.",LOG_LEVEL_ERROR)
      stop
   end if

  end subroutine solver_algorithm


end module solver_mod
