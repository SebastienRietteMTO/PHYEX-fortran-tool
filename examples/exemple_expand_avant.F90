SUBROUTINE EXPAND
IMPLICIT NONE

INTEGER :: JI, JJ
INTEGER :: IOPT
INTEGER, DIMENSION(5) :: ICASE
REAL, DIMENSION(5) :: ZZ
REAL, DIMENSION(5, 6) :: ZZZ

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemple le plus simple

!$mnh_expand_array(JI=1:5)
ZZ(:)=1.
!$mnh_end_expand_array(JI=1:5)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Le même avec les bornes définies dans les expressions

!$mnh_expand_array(JI=1:5)
ZZ(1:5)=1.
!$mnh_end_expand_array(JI=1:5)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Une varainte avec deux dimensions

!$mnh_expand_array(JI=1:5,JJ=1:6)
ZZZ(:,:)=1.
!$mnh_end_expand_array(JI=1:5,JJ=1:6)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemples avec des IF
IOPT=1

!$mnh_expand_array(JI=1:5)
IF(IOPT==1) THEN
  ZZ(:)=1.
ELSEIF(IOPT==2) THEN
  ZZ(:)=2.
ELSE IF(IOPT==3) THEN
  ZZ(:)=3.
ELSE
  ZZ(:)=4.
ENDIF
!$mnh_end_expand_array(JI=1:5)

!$mnh_expand_array(JI=1:5)
ZZ(:)=1.
IF (IOPT==1) ZZ(:)=2.
!$mnh_end_expand_array(JI=1:5)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Exemple avec des WHERE

DO JI=1, 5
  ICASE(JI)=JI
ENDDO

!$mnh_expand_where(JI=1:5)
WHERE(ICASE(:)==1)
  ZZ(:)=1.
ELSEWHERE(ICASE(:)==2)
  ZZ(:)=2.
ELSE WHERE(ICASE(:)==3)
  ZZ(:)=3.
ELSEWHERE
  ZZ(:)=4.
ENDWHERE
!$mnh_end_expand_where(JI=1:5)

!$mnh_expand_where(JI=1:5)
ZZ(:)=1.
WHERE (ICASE(:)==1) ZZ(:)=2.
!$mnh_end_expand_where(JI=1:5)

END SUBROUTINE EXPAND
