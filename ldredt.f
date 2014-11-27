

*+LDREDT

       SUBROUTINE LDREDT (BUFFER, STATUS)
C      ----------------------------------
C
C  Loads the current redtape from a save buffer.
C
C  Parameters:
C      BUFFER    integer(*)  save buffer
C      STATUS    integer     status value
C
C  Restores the redtape common blocks from the array BUFFER, containing
C  previously saved redtape.  BUFFER should be at least one page (2048
C  bytes) in length.  Note that this routine does not load any extra
C  redtape pages;  these can be copied using routine LDXRDT.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 3 October 86)
C
*-
       INTEGER*4  BUFFER(512)
       INTEGER  STATUS, I
C
       include '/mrao/include/maplib_redtape.inc'
C
       IF (STATUS.EQ.0) THEN
         DO I=1,512
           IRTALL(I)=BUFFER(I)
         ENDDO
       ENDIF
C
       END
