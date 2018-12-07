subroutine array_section()
  INTEGER :: dummy

  a(:,:) = b(:,:) * c(:,:)

  do ji = 1, n
     a(ji,:) = b(ji,:) * c(ji,:)
  end do

END subroutine array_section


**** nemo_merge
