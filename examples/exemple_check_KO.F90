SUBROUTINE CHECK(ARG1, ARG2, &
                 !La ligne suivante est commentée, Z1 n'est donc pas un argument
                 !Z1, &
                 ARG3)
   
!L'instruction est ici commentée, l'outil doit générer un warning ou une
!erreur si la vérification est demandée
!IMPLICIT NONE   

REAL, INTENT(IN) :: ARG1, ARG3

!L'argument suivant est déclaré sans INTENT, l'outil doit générer un
!warning ou une erreur si la vérification est demandé
REAL :: ARG2

!Déclaration des arguments locaux

!L'argument suivant est local et ne doit pas avoir d'INTENT
REAL :: Z1

END SUBROUTINE CHECK
