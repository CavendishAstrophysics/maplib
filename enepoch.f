


*+ENEPOCH

       SUBROUTINE ENEPOCH ( EN_EPOCH, STATUS )
C      ---------------------------------------
C
C  Returns the epoch of the map from the redtape
C
C  Returned:
C      EN_EPOCH    real*8      map epoch
C      STATUS      integer     status value
C
C  P.Alexander  20/01/93
*-
       REAL*8  EN_EPOCH
       INTEGER STATUS
C
       include '/mrao/include/maplib_redtape.inc'
C
       IF (STATUS .NE. 0) RETURN
C
       EN_EPOCH = EPOCH
C
       END
