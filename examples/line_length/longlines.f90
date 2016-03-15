program long_lines
  use testkern_qr, only : testkern_qr_type
  real(r_def) :: rdt
  integer(i_def) :: iflag
  call invoke(testkern_qr_type(f1, f2, f3, rdt, f4, iflag, qr))
end program long_lines
