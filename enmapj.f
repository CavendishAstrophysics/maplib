


*+ENMAPJ

       SUBROUTINE ENMAPJ (EN_IPROJ, EN_USAMP, EN_SKEW,
     :                              EN_EPOCH, EN_PRANG, STATUS)
C      ----------------------------------------------------------
C
C  Returns map projection parameters from redtape.
C
C  Returned:
C      EN_IPROJ    integer     projection code (1,2,3 for equatorial,
C                                      sky coordinates or tangent plane)
C      EN_USAMP    real*8      U-coordinate sampling at map centre
C                                                     (arcsec/gridpoint)
C      EN_SKEW     real*8      skew angle of U,V plane (radians)
C      EN_EPOCH    real*8      epoch of projection (years)
C      EN_PRANG    real*8      precession angle between epoch and 1950.0
C      STATUS      integer     status value
C
C  The coordinates of the map projection are returned from the
C  'astronomical' redtape (section 4).
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 10 November 87)
C
*-
       REAL*8    EN_USAMP, EN_SKEW, EN_EPOCH, EN_PRANG
       INTEGER   EN_IPROJ, STATUS
C
       include '/mrao/include/maplib_redtape.inc'
C
       IF (STATUS .NE. 0) RETURN
C
       EN_IPROJ = IPROJ
       EN_USAMP = USAMP
       EN_SKEW  = SKEW
       EN_EPOCH = EPOCH
       EN_PRANG = PRANG
C
       END
