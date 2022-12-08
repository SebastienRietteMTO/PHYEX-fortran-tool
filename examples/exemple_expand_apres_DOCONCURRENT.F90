SUBROUTINE EXPAND
IMPLICIT NONE

INTEGER :: JI, JJ
INTEGER :: IOPT
INTEGER, DIMENSION(5) :: ICASE
REAL, DIMENSION(5) :: ZZ
REAL, DIMENSION(5, 6) :: ZZZ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemple le plus simple

DO CONCURRENT (JI=1:5)
ZZ(JI)=1.
ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Le même avec les bornes définies dans les expressions

DO CONCURRENT (JI=1:5)
ZZ(JI)=1.
ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Une varainte avec deux dimensions

DO CONCURRENT (JJ=1:6,JI=1:5)
ZZZ(JI,JJ)=1.
ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemples avec des IF
IOPT=1

DO CONCURRENT (JI=1:5)
IF(IOPT==1) THEN
  ZZ(JI)=1.
ELSEIF(IOPT==2) THEN
  ZZ(JI)=2.
ELSE IF(IOPT==3) THEN
  ZZ(JI)=3.
ELSE
  ZZ(JI)=4.
ENDIF
ENDDO

DO CONCURRENT (JI=1:5)
ZZ(JI)=1.
IF (IOPT==1) ZZ(JI)=2.
ENDDO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemple avec des WHERE

DO JI=1, 5
  ICASE(JI)=JI
ENDDO

DO CONCURRENT (JI=1:5)
IF(ICASE(JI)==1) THEN
  ZZ(JI)=1.
ELSEIF(ICASE(JI)==2) THEN
  ZZ(JI)=2.
ELSEIF(ICASE(JI)==3) THEN
  ZZ(JI)=3.
ELSE
  ZZ(JI)=4.
ENDIF
ENDDO

DO CONCURRENT (JI=1:5)
ZZ(JI)=1.
IF (ICASE(JI)==1) ZZ(JI)=2.
ENDDO

END SUBROUTINE EXPAND
