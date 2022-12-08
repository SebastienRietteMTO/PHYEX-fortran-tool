SUBROUTINE ALLOC()
IMPLICIT NONE

!Pas des tableaux
REAL :: Z
INTEGER, PARAMETER :: ISIZE=5

!Z1 est un tableau
REAL, DIMENSION(ISIZE) :: Z1

!Z2 est également un tableau
REAL :: Z2(5)

!Z3 est un tableau Z4 ne l'est pas
REAL :: Z3(5) , Z4

!Multipligne 1
REAL(KIND=8), DIMENSION(5) :: Z5, & ! Z23, Z24
                              Z6, Z7

!Multipligne 2
REAL :: Z8, & ! Z25, Z26
        Z9(5,3), Z10(5)
END SUBROUTINE ALLOC
