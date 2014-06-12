MODULE intel_timer_mod
   USE iso_c_binding
   IMPLICIT none

   PUBLIC

   ! Need 64-bit integers when using the Intel counter for timing
   INTEGER, PARAMETER :: int64 = SELECTED_INT_KIND(14)

#if defined RDTSC_TIMER
   INTERFACE

      FUNCTION getticks()  bind(C,name="getticks")
        USE iso_c_binding
        IMPLICIT none
        INTEGER (C_INT64_T) :: getticks
      END FUNCTION getticks

   END INTERFACE
#else
CONTAINS

  ! Dummy implementation
   FUNCTION getticks()
     INTEGER (C_INT64_T) :: getticks
     getticks = 1
   END FUNCTION getticks
#endif

END MODULE intel_timer_mod
