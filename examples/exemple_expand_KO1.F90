SUBROUTINE EXPAND1
IMPLICIT NONE

INTEGER :: JI
REAL, DIMENSION(5) :: ZZ

!Les directives ci-dessous ne sont pas valides car la directive
!de début (expand_where) ne porte pas le même nom que
!la directive de fin (expand_array).

!$mnh_expand_where(JI=1:5)
ZZ(:)=1.
!$mnh_end_expand_array(JI=1:5)

END SUBROUTINE EXPAND1
