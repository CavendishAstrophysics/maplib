


*+CHREDT

       SUBROUTINE CHREDT (STATUS)
C      --------------------------
C
C  Checks current redtape for validity.
C
C  Returned:
C      STATUS    integer     status value
C
C  Checks the map redtape currently present in the run-time common blocks
C  for validity.  Checks are made on the map keyword and on the internal
C  data type.
C
C  The STATUS value should be zero on entry.  Possible error values are:
C
C      - invalid map redtape (ILL_REDTAPE)
C      - invalid internal data type (ILL_DATYPE)
C
C  (DJT, 24 November 87)
C
*-
       INTEGER  STATUS
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
C  Check the first page of redtape
C
       IF (RTHDR.NE.'MAP' .AND. RTHDR.NE.'APERTURE') THEN
         STATUS=ILL_REDTAPE
       ELSEIF ((RTHDR.EQ.'MAP' .AND. ISWD.GT.3) .OR.
     :         (RTHDR.EQ.'APERTURE' .AND. ISWD.NE.4)) THEN
         STATUS=ILL_DATYPE
       ENDIF
C
       END
