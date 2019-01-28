!-------------------------------------------------------------------------------
! (c) The copyright relating to this work is owned jointly by the Crown,
! Met Office and NERC 2015.
! However, it has been created with the help of the GungHo Consortium,
! whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
!-------------------------------------------------------------------------------
! Author R, Ford STFC Daresbury Lab

PROGRAM module_inline_same_kernel

use kind_params_mod
  use grid_mod
  use field_mod
  use time_smooth_mod, only: time_smooth
  implicit none

  type(r2d_field) :: a, b, c, d
  
  call invoke( time_smooth(a,b,c), time_smooth(b,c,d) )

END PROGRAM module_inline_same_kernel
