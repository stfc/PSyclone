program long_lines
  use testkern_qr, only : testkern_qr_type
  call invoke(testkern_qr_type(f1, f2, f3, f4, qr))
end program long_lines
