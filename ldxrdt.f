


*+LDXRDT

       SUBROUTINE LDXRDT (BUFFER, PAGES, STATUS)
C      -----------------------------------------
C
C  Loads the extra redtape from a save buffer.
C
C  Parameters:
C      BUFFER    integer(*)  save buffer
C      PAGES     integer     no.of redtape pages to load
C      STATUS    integer     status value
C
C  Restores the extra redtape pages from the array BUFFER, containing
C  previously saved redtape.
C
C  The STATUS value should be zero on entry.
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
           EXTRART(I)=BUFFER(I)
         ENDDO
       ELSE
         STATUS=ILL_EXREDT
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS, 'in routine LDXRDT')
C
       END
