SUBROUTINE EXPAND3
IMPLICIT NONE

INTEGER :: JI
REAL, DIMENSION(5) :: ZZ
REAL :: Z

!Z est scalaire alors qu'il est dans le bloc, l'outil doit
!refuser de faire la transformation.

!Par simplicité, on peut également refuser de faire la transformation
!s'il n'y avait que ZZ=1. dans le bloc. En effet dans ce cas il faudrait
!remonter à la déclaration pour savoir si ZZ est scalaire ou tableau.
!On peut imposer que tous les tableaux soit écrits avec des (:).


!$mnh_expand_array(JI=1:5)
ZZ(:)=1.
Z=0.
!$mnh_end_expand_array(JI=1:5)

END SUBROUTINE EXPAND3
