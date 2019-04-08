module data_mod
  use kind_params_mod, only: go_wp
  implicit none
  real(go_wp), parameter :: gravity = -9.8
  
contains

  function my_function(val)
    real(go_wp), intent(in) :: val
    real(go_wp) :: my_function

    my_function = 2.0*val
    
  end function my_function
  
end module data_mod
