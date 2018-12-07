subroutine data_ref()
  INTEGER :: dummy
  do ji = 1, n
     prof%npind(ji) = a(ji)
  end do
END subroutine data_ref
