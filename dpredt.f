


*+DPREDT

       SUBROUTINE DPREDT (BUFFER, STATUS)
C      ----------------------------------
C
C  Dumps the current redtape to a save buffer.
C
C  Parameters:
C      BUFFER    integer(*)  save buffer
C      STATUS    integer     status value
C
C  Saves the current redtape common blocks in the array BUFFER, which
C  should be at least one page (2048 bytes) in length.  Note that this
C  routine does not dump any extra redtape pages; these can be copied
C  with routine DPXRDT.
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
           BUFFER(I)=IRTALL(I)
         ENDDO
       ENDIF
C
       END
