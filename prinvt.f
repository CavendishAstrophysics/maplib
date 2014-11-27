C
C
C
C
       SUBROUTINE PRINVT(A)
C      --------------------
C
C  Routine to invert 3x3 matrix.  First construct adjoint matrix.
       REAL*8 A(3,3),B(3,3),DTERM
       INTEGER I,J
       B(1,1) = A(2,2)*A(3,3) - A(3,2)*A(2,3)
       B(1,2) = A(3,2)*A(1,3) - A(1,2)*A(3,3)
       B(1,3) = A(1,2)*A(2,3) - A(2,2)*A(1,3)
       B(2,1) = A(2,3)*A(3,1) - A(3,3)*A(2,1)
       B(2,2) = A(3,3)*A(1,1) - A(1,3)*A(3,1)
       B(2,3) = A(1,3)*A(2,1) - A(2,3)*A(1,1)
       B(3,1) = A(2,1)*A(3,2) - A(3,1)*A(2,2)
       B(3,2) = A(3,1)*A(1,2) - A(1,1)*A(3,2)
       B(3,3) = A(1,1)*A(2,2) - A(2,1)*A(1,2)
C
C  Normalise using determinant
       DTERM = A(1,1)*B(1,1) + A(2,1)*B(1,2) + A(3,1)*B(1,3)
       DO 1 I=1,3
       DO 1 J=1,3
    1  A(I,J) = B(I,J) / DTERM
       RETURN
       END
