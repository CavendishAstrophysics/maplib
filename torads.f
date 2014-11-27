




*+TORADS

       SUBROUTINE TORADS (KEY, IDH, IMIN, SECS, DRADS)
C      -----------------------------------------------
C
C  Converts degrees/hours, minutes, seconds to radians.
C
C  Given:
C      KEY       char*3      'DMS' or 'HMS'
C      IDH       integer     degrees or hours
C      IMIN      integer     (arc)minutes
C      SECS      real*4      (arc)seconds
C
C  Returned:
C      DRADS     real*8      radians
C
C  Converts degrees, arcminutes, arcseconds or hours, minutes, seconds to
C  radians, returned as a double precision variable.  No range checking of
C  the input data is performed.  Negative values should be input as, e.g.:
C           -2,10,35.5  or  0,-10,35.5  or  0,0,-35.5
C
C  (DJT, 14 October 86)
C
*-
       CHARACTER*3  KEY
       INTEGER  IDH, IMIN, ISIGN
       REAL*4  SECS
       REAL*8  DRADS, DHS
       LOGICAL  CHR_CHSAME
C
       include '/mrao/include/constants.inc'
C
       ISIGN=1
       DHS=DBLE(SECS)/3600.D0
       IF (IDH.LT.0) ISIGN=-1
       IF (IMIN.LT.0) DHS=-DHS
C
       IF (CHR_CHSAME(KEY,'DMS')) THEN
         DRADS=(ISIGN*IDH+IMIN/60.D0+DHS)*CONST_D2R
         DRADS=ISIGN*DRADS
       ELSEIF (CHR_CHSAME(KEY,'HMS')) THEN
         DRADS=(ISIGN*IDH+IMIN/60.D0+DHS)*CONST_H2R
         DRADS=ISIGN*DRADS
       ELSE
         DRADS=0.D0
       ENDIF
C
       END
