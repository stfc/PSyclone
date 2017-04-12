program enforce_operator_bc_kernel_example

  ! this boundary condition kernel has been created as a temporary measure
  ! as boundary layer information is not currently described
  ! in the API. Therefore, for the moment, users can add this kernel
  ! when they want to enforce boundary conditions to operators

  use enforce_operator_bc_kernel_mod, only : enforce_operator_bc_kernel_type

  call invoke(enforce_operator_bc_kernel_type(op_a))

end program enforce_operator_bc_kernel_example
