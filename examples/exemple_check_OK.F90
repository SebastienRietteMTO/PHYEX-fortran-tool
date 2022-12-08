SUBROUTINE CHECK(ARG1, ARG2, &
                 !La ligne suivante est commentée, Z1 n'est donc pas un argument
                 !Z1, &
                 ARG3)
   
!L'instruction est ici décommentée, l'outil ne doit pas générer de warning
!ou d'erreur
IMPLICIT NONE   

REAL, INTENT(IN) :: ARG1, ARG3

!L'argument suivant est bien déclaré avec INTENT
!
REAL, INTENT(OUT) :: ARG2

!Déclaration des arguments locaux

!L'argument suivant est local et ne doit pas avoir d'INTENT
REAL :: Z1

END SUBROUTINE CHECK
