SUBROUTINE EXPAND5
IMPLICIT NONE

INTEGER :: JI
REAL, DIMENSION(5) :: ZZ

!L'indice va de 1 à 3 selon la directive mais de 2 à 4 selon l'instruction.
!L'outil doit refuser de faire la transformation.

!$mnh_expand_array(JI=1:3)
ZZ(2:4)=1.
!$mnh_end_expand_array(JI=1:3)

END SUBROUTINE EXPAND5
