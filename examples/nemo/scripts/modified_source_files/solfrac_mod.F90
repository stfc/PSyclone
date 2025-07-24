MODULE solfrac_mod
   !!======================================================================
   !!                    ***  MODULE  solfrac  ***
   !!     POSH representation of solar absorption (Gntermann, 2009)
   !!=====================================================================
   !! History :        !  11-10  (J. While)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   solfrac  : function to calculate the solar fraction
   !!----------------------------------------------------------------------
   
   USE par_kind
   IMPLICIT NONE
     
   
   PUBLIC solfrac
                                                   
CONTAINS

   REAL(dp) FUNCTION solfrac(ptop,pbottom)
       !!----------------------------------------------------------------------
      !! *** ROUTINE solfrac ***
      !!
      !! ** Purpose :   Calculate the solar fraction absorbed between two 
      !!                layers
      !!
      !! ** Reference : POSH a model of diurnal warming, Gentemann et al, 
      !!                 JGR, 2009 
      !!----------------------------------------------------------------------
      
      ! Parameters 
      REAL(wp), PARAMETER, DIMENSION(9) :: &
      &                                     pp_wgt = (/0.2370, 0.36,  0.1790, &
      &                                                0.087,  0.08,  0.025,  &
      &                                                0.025,  0.007, 0.0004/)
      REAL(wp), PARAMETER, DIMENSION(9) :: &
      &                                    pp_len = (/34.84,   2.266,   0.0315,  &
      &                                               0.0055,  8.32e-4, 1.26e-4, &
      &                                                3.13e-4, 7.82e-4, 1.44e-5/)

      ! Dummy variabes
      REAL(wp), INTENT(IN) :: ptop, pbottom   ! Top and bottom of layer
      
      ! local variables
      INTEGER :: jt
      
      ! Calculate the solar fraction absorbed between the two layers
      solfrac = 0._wp
      DO jt = 1, 9
           solfrac = solfrac + pp_wgt(jt) * ( exp ( -ptop / pp_len(jt) ) &
            &                                 - exp ( -pbottom / pp_len(jt) ) )
      END DO
      
   END FUNCTION solfrac
   
END MODULE solfrac_mod
