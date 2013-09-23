!$Author: joelevin $
!$Author:$
!$Date:$
!$Revision:$
!$HeadURL:$

program tstresevapredu

!   +++  PURPOSE +++
!   Test code for resevapredu function

!   +++ FUNCTIONS CALLED+++
    real resevapredu
     
!   ++++ LOCAL VARIABLES +++
    real coef_a1, coef_b1
    real coef_a2, coef_b2
    real mass_1, mass_2
    real redu_1, redu_2
    real redu_cmp_12, redu_cmp_21
    
!   +++ END SPECIFICATIONS +++

    ! residue like alfalfa
    coef_a1 = -1.2
    coef_b1 = 0.6
    mass_1 = 0.4

    ! residue like cotton
    coef_a2 = -0.6
    coef_b2 = 0.7
    mass_2 = 0.6

    redu_1 = resevapredu( 1.0, mass_1, coef_a1, coef_b1 )
    redu_2 = resevapredu( 1.0, mass_2, coef_a2, coef_b2 )

    redu_cmp_12 = resevapredu( redu_1, mass_2, coef_a2, coef_b2 )
    redu_cmp_21 = resevapredu( redu_2, mass_1, coef_a1, coef_b1 )

    write(*,*) 'resevapredu function results'
    write(*,*) 'redu_1  redu_2  redu_cmp_12 redu_cmp_21'
    write(*,*) redu_1, redu_2, redu_cmp_12, redu_cmp_21

end program tstresevapredu
