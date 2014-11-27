


*$(3b)  Extra redtape handling routines

*+DPXRDT

       SUBROUTINE DPXRDT (BUFFER, PAGES, STATUS)
C      -----------------------------------------
C
C  Dumps the current extra redtape to a save buffer.
C
C  Given:
C      BUFFER    integer(*)  save buffer
C      PAGES     integer     no.of extra redtape pages to save
C      STATUS    integer     status value
C
C  Saves the current extra redtape pages in the array BUFFER.  The
C  size of the current extra redtape can be found via routine ENXRDT.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 4 December 89)
C
*-
       INTEGER*4  BUFFER(*)
       INTEGER    PAGES, STATUS
       INTEGER    I, N
C
       include '/mrao/include/maplib_extrart.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
       IF (PAGES.LE.MAX_EXTRA) THEN
         N=PAGES*512
         DO I=1,N
           BUFFER(I)=EXTRART(I)
         ENDDO
       ELSE
         STATUS=NO_EXREDT
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS, 'in routine DPXRDT')
C
       END
