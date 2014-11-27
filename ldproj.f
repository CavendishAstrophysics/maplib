




*+LDPROJ

       SUBROUTINE LDPROJ (BUFFER, STATUS)
C      ----------------------------------
C
C  Loads the current map projection from a save buffer.
C
C  Parameters:
C      BUFFER    integer(*)  save buffer
C      STATUS    integer     status value
C
C  Restores the map projection from the array BUFFER, containing parameters
C  previously saved by routine DPPROJ.  The map projection parameters are
C  defined within the include file (LIBRARY)MAPLIB-PROJ:INCL.
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
           IPRALL(I)=BUFFER(I)
         ENDDO
       ENDIF
C
       END
