SUBROUTINE EXPAND4
IMPLICIT NONE

INTEGER :: JI
REAL, DIMENSION(5,4) :: ZZ

!ZZ est déclaré avec deux dimensions alors que la directive mnh_expand n'en défini
!qu'une seule. L'outil doit refuser de faire la transformation.

!$mnh_expand_array(JI=1:5)
ZZ(:,:)=1.
!$mnh_end_expand_array(JI=1:5)

END SUBROUTINE EXPAND4
