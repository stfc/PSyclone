! TODO: #647 - Once this is implemented, this function can be removed.
! Dummy main program that just calls the create driver.
  program main
    use main_code_mod, only: main_code
    call main_code()
  end program main