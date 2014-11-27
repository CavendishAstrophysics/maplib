*+TODDAT

       SUBROUTINE TODDAT (IDATE, DATE)
C      -------------------------------
C
C  Converts calendar date to decimal date in years.
C
C  Given:
C      IDATE     integer(3)  day, month, year
C
C  Returned:
C      DATE      real*8      decimal date in years
C
C  Converts the calendar day, month, year from the integer array IDATE
C  to a decimal date in years, e.g.  22 4 1976  to  1976.309.  DATE will
C  represent noon on the day in question.  Note that the century may be
C  omitted from the year in IDATE(3).
C
C  (DJT, 13 October 86)
C  (DJT, 5 January 00)
C
*-
       INTEGER  IDATE(3), IDAYS(12)
       REAL*8  DATE, DYEARS, DDAYS
C
       DATA  IDAYS/0,31,59,90,120,151,181,212,243,273,304,334/
C
       DDAYS=IDATE(1)
       DYEARS=IDATE(3)
       IF (IDATE(2).GT.0) DDAYS=DDAYS+IDAYS(IDATE(2))
       IF (IDATE(3).LT.50) THEN
          DYEARS=DYEARS+2000
       ELSEIF (IDATE(3).LT.100) THEN
          DYEARS=DYEARS+1900
       ENDIF
C
C  Normalise to noon on the day in question
C
       IF (DDAYS.GT.0.D0) DDAYS=DDAYS-0.5D0
C
C  Test for leap year
C
       IF ((MOD(IDATE(3),4).EQ.0 .AND. MOD(IDATE(3),100).NE.0) .OR.
     :      MOD(IDATE(3),1000).EQ.0) THEN
         IF (IDATE(2).GT.2) DDAYS=DDAYS+1.D0
         DATE=DYEARS+DDAYS/366.D0
       ELSE
         DATE=DYEARS+DDAYS/365.D0
       ENDIF
C
       END
