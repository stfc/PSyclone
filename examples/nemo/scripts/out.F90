module stringop
  implicit none
  public

  contains
  subroutine cmpblank(str)
    use profile_psy_data_mod, only : profile_PSyDataType
    character(len=*), intent(inout) :: str
    integer :: lcc
    integer :: ipb
    type(profile_PSyDataType), save, target :: profile_psy_data

    CALL profile_psy_data % PreStart("stringop", "cmpblank-r0", 0, 0)
    lcc = LEN_TRIM(str)
    ipb = 1
    do while (.true.)
      if (ipb >= lcc) then
        ! PSyclone CodeBlock (unsupported code) reason:
        !  - Unsupported statement: Exit_Stmt
        EXIT
      end if
      if (str(ipb:ipb + 1) == '  ') then
        str(ipb + 1:) = str(ipb + 2:lcc)
        lcc = lcc - 1
      else
        ipb = ipb + 1
      end if
    end do
    CALL profile_psy_data % PostEnd

  end subroutine cmpblank

end module stringop
