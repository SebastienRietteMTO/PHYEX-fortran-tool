SUBROUTINE EXPAND2
IMPLICIT NONE

INTEGER :: JI
REAL, DIMENSION(5) :: ZZ

!Les directives ci-dessous ne sont pas valides car la variable
!utilisée dans la directive de début (JI) n'est pas la même
!que dans la directive de fin (JJ).
!De plus, elle n'est également pas valide car les bornes de l'indices
!dans la directive de début (1 et 5) ne sont pas les mêmes que dans
!la directive de fin (2 et 6).

!On peut imposer à ce que l'écriture soit strictement identique y compris
!la présence ou non d'espaces de façon à simplifier le contrôle.

!$mnh_expand_array(JI=1:5)
ZZ(:)=1.
!$mnh_end_expand_array(JJ=2:6)

END SUBROUTINE EXPAND2
