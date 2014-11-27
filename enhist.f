
*+ENHIST

       SUBROUTINE ENHIST (IDATE, PROGNAM, STATUS)
C      ------------------------------------------
C
C  Returns most recent map processing history.
C
C  Returned:
C      IDATE     integer(3)  date last processed (day, month, year)
C      PROGNAM   char*(*)    program name
C      STATUS    integer     status value
C
C  Returns date and program name of most recent processing.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 17 November 87)
C  (DJT, 5 January 00)
C
*-
       CHARACTER  PROGNAM*(*)
       INTEGER    IDATE(3), STATUS
C
       include '/mrao/include/maplib_redtape.inc'
C
       IF (STATUS.NE.0) RETURN
C
       IF (NHIST.EQ.0) THEN
         IDATE(1)=0
         IDATE(2)=0
         IDATE(3)=0
         PROGNAM=' '
       ELSE
         READ(HISTRY(NHIST),'(I2,X,I2,X,I2)') IDATE
         IF(IDATE(3).GE.50)IDATE(3)=IDATE(3)+1900
         IF(IDATE(3).LT.50)IDATE(3)=IDATE(3)+2000
         PROGNAM=HISTRY(NHIST)(9:)
       ENDIF
C
       END
