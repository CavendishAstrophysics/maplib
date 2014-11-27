


*+PRECRD2

       SUBROUTINE PRECRD2 (PRTYPE, DATE1, RA1, DEC1, DATE2, RA2, DEC2)
C      ---------------------------------------------------------------
C
C  Does precession between any two arbitary dates.
C
C  Given:
C      PRTYPE     integer     Precession type.
C      DATE1      real*8      First date expressed as a decimal year.
C      RA1,DEC1   real*8      Position of source at DATE1.
C      DATE2      real*8      Date to precess to expressed as dec. year.
C
C  Returned:
C      RA2,DEC2   real*8      Position RA1,DEC1 at DATE2.
C
C  Uses the specified precession type to precess between any two
C  arbitary dates. The following precession types are defined:
C      PRTYPE=0, Precession using fast precession routine PRECES
C      PRTYPE=1, Precession using full precession routine PRECRD
C
C  (NPR, 1 December 87)
C
*-
       INTEGER        PRTYPE
       REAL*8         DATE1, RA1, DEC1, DATE2, RA2, DEC2
C
       REAL*8         RA_1950, DEC_1950
C
       IF (PRTYPE .EQ. 0) THEN
         CALL PRECES( RA1, DEC1, RA2, DEC2, (DATE2-DATE1) )
       ELSE IF (PRTYPE .EQ. 1) THEN
         IF (DATE2 .EQ. DATE1) THEN
           RA2  = RA1
           DEC2 = DEC1
         ELSE
           IF (DATE1 .NE. 1950.0D+0) THEN
             CALL PRECRD( DATE1,RA1,DEC1, 1950.0D+0,RA_1950,DEC_1950 )
           ELSE
             RA_1950  = RA1
             DEC_1950 = DEC1
           END IF

           IF (DATE2 .NE. 1950.0D+0) THEN
             CALL PRECRD( 1950.0D+0,RA_1950,DEC_1950, DATE2,RA2,DEC2 )
           ELSE
             RA2  = RA_1950
             DEC2 = DEC_1950
           END IF
         END IF
       END IF

       RETURN
       END
