program test_compare

    use compare_variables_mod, only: compare_init, compare, compare_summary

    implicit none

    real, dimension(10) :: a_single, a_single_correct
    double precision, dimension(15) :: a_dbl, a_dbl_correct
    integer, dimension(15) :: a_int, a_int_correct

    a_dbl_correct = 1.0d0
    ! This should result in counts of:
    ! 1 2 3 4 5  (identical, <1e-9, <1e-6, <1e-3, >=1e-3)
    a_dbl( 1: 1) = 1.0d0
    a_dbl( 2: 3) = 1.0+1d-10
    a_dbl( 4: 6) = 1.0+1d-7
    a_dbl( 7:10) = 1.0+1d-4
    a_dbl(11:15) = 1.0+1d-1

    ! Test single precision. Note that single precision cannot store an
    ! error of 1e-9, so we only test up to 1e-6:
    ! resulting in counts of: 1 0 2 3 4
    a_single_correct = 1.0
    a_single( 1: 1) = 1.0
    a_single( 2: 3) = 1.0+1d-7
    a_single( 4: 6) = 1.0+1d-4
    a_single( 7:10) = 1.0+1d-1

    ! Test integer, max. value is 2147483648, so use 2*10^9
    ! to create errors with 1e-9 etc
    a_int_correct = 2000000000
    a_int( 1: 1) = 2000000000
    a_int( 2: 3) = 2000000001
    a_int( 4: 6) = 2000001000
    a_int( 7:10) = 2001000000
    a_int(11:15) = 2100000000

    call compare_init(3)
    call compare("a_dbl", a_dbl, a_dbl_correct)
    call compare("a_single", a_single, a_single_correct)
    call compare("a_int", a_int, a_int_correct)
    call compare_summary()

end program test_compare
