

*$(6)  Map projection handling routines


*+DPPROJ

       SUBROUTINE DPPROJ (BUFFER, STATUS)
C      ----------------------------------
C
C  Dumps the current map projection to a save buffer.
C
C  Parameters:
C      BUFFER    integer(*)  save buffer
C      STATUS    integer     status value
C
C  Saves the current map projection description in the array BUFFER,
C  which should be at least 30 words (120 bytes) in length.
C
C  The details of the map projection are held in the include file
C  (LIBRARY)MAPLIB-PROJ:INCL.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 22 October 87)
C
*-
C
       include '/mrao/include/maplib_proj.inc'
C
       INTEGER*4  BUFFER(LENPRJ)
       INTEGER    STATUS, I
C
       IF (STATUS.EQ.0) THEN
         DO I=1,LENPRJ
           BUFFER(I)=IPRALL(I)
         ENDDO
       ENDIF
C
       END
