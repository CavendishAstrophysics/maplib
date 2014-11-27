


*+ENXRDT

       SUBROUTINE ENXRDT (PAGES, STYLE, STATUS)
C      ----------------------------------------
C
C  Returns number of extra redtape pages.
C
C  Parameters:
C      PAGES     integer     no.of extra redtape pages
C      STYLE     integer     style of extra redtape records
C      STATUS    integer     status value
C
C  Returns the number of pages of extra redtape held with the current
C  map.  These redtape pages are positioned after the usual Nord redtape,
C  and before the map data.  Extra redtape pages are allocated by the
C  STXRDT routine.  Redtape records may be written and read using routines
C  STXREC and ENXREC.
C
C  The STATUS value should be zero on entry.  A value of ILL_REDTAPE
C  will be returned if the current redtape is invalid.
C
C  (DJT, 29 November 89)
C
*-
       INTEGER    PAGES, STYLE, STATUS
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_extrart.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
       CALL CHREDT(STATUS)
C
       IF (STATUS.EQ.0) THEN
         PAGES=IPEXRT
         STYLE=ISTYRT
         IF (ISTYRT.LT.0 .OR. ISTYRT.GT.1 .OR.
     :       IPEXRT.LT.0 .OR. IPEXRT.GT.MAX_EXTRA) THEN
            STATUS=ILL_EXREDT
         ENDIF
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine ENXRDT')
C
       END
