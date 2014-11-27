


*+INTROW

       SUBROUTINE INTROW (DATA, M, N)
C      ------------------------------
C
C  Interpolates map row to mid-points.
C
C  Given:
C      DATA      real*4()    input data
C      M         integer     data group length
C      N         integer     no.of data groups
C
C  Array DATA contains N groups of data, of group length M, where N is
C  an odd number.  The data in even-numbered groups are interpolated
C  from the data in odd-numbered groups, using a standard eight-point
C  sinT/T convolution, with repeated end-points.  Using a group length
C  of 1, this routine can be used to interpolate intermediate points
C  within a map row.  Using a group length equal to the map row length,
C  it can be used to interpolate intermediate rows within a complete
C  map array.
C
C  Note that if any of the data points used in the convolution contain
C  undefined values, the interpolated value will also be undefined.
C
C  (DJT, 17 November 86)
C
*-
       REAL*4   DATA(1)
       INTEGER  M, N
       INTEGER  I, IX, IX1, IX2, J, K, MM, NM
C
C  Coefficients of symmetric 8-point convolution function
       REAL*4  COEFF(4)
       DATA  COEFF /0.609375, -0.140625, 0.046875, -0.015625/
C
       include '/mrao/include/maplib_redtape.inc'
C
       IX=M
       MM=M+M
       NM=N*M-M
       DO I=2,N-1,2
         IX1=IX-M
         IX2=IX+M
         DO J=1,M
           DATA(IX+J)=0.0
         ENDDO
         DO K=1,4
           DO J=1,M
             IF (DATA(IX+J).EQ.BLANK .OR.
     :           DATA(IX1+J).EQ.BLANK .OR.
     :           DATA(IX2+J).EQ.BLANK) THEN
               DATA(IX+J)=BLANK
             ELSE
               DATA(IX+J)=DATA(IX+J)+COEFF(K)*(DATA(IX1+J)+DATA(IX2+J))
             ENDIF
           ENDDO
           IX1=MAX0(IX1-MM,0)
           IX2=MIN0(IX2+MM,NM)
         ENDDO
         IX=IX+MM
       ENDDO
C
       END
