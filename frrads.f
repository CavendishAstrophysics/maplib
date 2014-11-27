




*+FRRADS

       SUBROUTINE FRRADS (KEY, DRADS, IDH, IMIN, SECS)
C      -----------------------------------------------
C
C  Converts radians to degrees/hours, minutes, seconds.
C
C  Given:
C      KEY       char*3      'DMS' or 'HMS'
C      DRADS     real*8      radians
C
C  Returned:
C      IDH       integer     degrees or hours
C      IMIN      integer     (arc)minutes
C      SECS      real        (arc)seconds
C
C  Converts a double precision value from radians to degrees, arcminutes,
C  arcseconds or hours, minutes, seconds,  depending on the value of the
C  input key,  returning the result as two integers and one real variable.
C  No range checking of the input value is performed.  Negative input
C  values are returned as, e.g.:
C           -2,10,35.5  or  0,-10,35.5  or  0,0,-35.5
C
C  (DJT, 14 October 86)
C
*-
       CHARACTER*3  KEY
       REAL*8  DRADS, DHS
       INTEGER  IDH, IMIN, ISIGN
       REAL*4  SECS
       LOGICAL  CHR_CHSAME
C
       include '/mrao/include/constants.inc'
C
       ISIGN=1
       IF (DRADS.LT.0.D0) ISIGN=-1
       IF (CHR_CHSAME(KEY,'DMS')) THEN
         DHS=ISIGN*DRADS/CONST_D2R
       ELSEIF (CHR_CHSAME(KEY,'HMS')) THEN
         DHS=ISIGN*DRADS/CONST_H2R
       ENDIF
C
       IDH=DHS
       IMIN=(DHS-IDH)*60.D0
       SECS=((DHS-IDH)*60.D0-IMIN)*60.D0
C
       IF (ISIGN.EQ.-1) THEN
         IDH=-IDH
         IF (IDH.EQ.0) THEN
           IMIN=-IMIN
           IF (IMIN.EQ.0) SECS=-SECS
         ENDIF
       ENDIF
C
       END
