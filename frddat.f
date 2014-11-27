



*$(7)  Coordinate conversion routines


*+FRDDAT

       SUBROUTINE FRDDAT (DATE, IDATE)
C      -------------------------------
C
C  Converts decimal date in years to calendar date.
C
C  Given:
C      DATE      real*8      decimal date in years
C
C  Returned:
C      IDATE     integer(3)   day, month, year
C
C  Routine to convert a date from the double precision variable DATE
C  to the integer array IDATE containing the calendar day, month and
C  year.
C
C  (DJT, 13 October 86)
C
*-
       REAL*8  DATE
       INTEGER  IDATE(3)
       INTEGER  IDAYS(12), IDAY, LEAP
C
       DATA  IDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
C
       IDATE(1)=0
       IDATE(2)=0
       IDATE(3)=IDINT(DATE)
C
       IF (DINT(DATE).NE.DATE) THEN
         LEAP=0
         IF (MOD(IDATE(3),4).EQ.0 .AND. MOD(IDATE(3),100).NE.0) THEN
           LEAP=1
         ENDIF
         IDAY=(DATE-IDATE(3))*(365+LEAP)+1
         DO WHILE (IDAY.GT.0)
           IDATE(1)=IDAY
           IDATE(2)=IDATE(2)+1
           IDAY=IDAY-IDAYS(IDATE(2))
           IF (IDATE(2).EQ.2) IDAY=IDAY-LEAP
         ENDDO
       ENDIF
C
       END
