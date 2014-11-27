


*+ENXREC

       SUBROUTINE ENXREC (DESCRIPTOR, RECORD, STATUS)
C      ----------------------------------------------
C
C  Returns a record of extra redtape.
C
C  Parameters:
C      DESCRIPTOR  char*8      redtape descriptor
C      RECORD      integer(*)  redtape record (128 4-byte words)
C      STATUS      integer     status value
C
C  Scans the extra redtape for a record matching the given descriptor,
C  and returns the redtape record in the array provided.  Note that
C  extra redtape records are read from the map file, together with the
C  basic map redtape, by routine RDREDT.
C
C  The STATUS value should be zero on entry and is unchanged if a
C  redtape record is successfully returned.  Other possible values
C  are:
C          - illegal map redtape (ILL_REDTAPE)
C          - redtape record not found (NO_EXTREC)
C
C  (DJT, 30 November 89)
C
*-
       CHARACTER  DESCRIPTOR*8
       INTEGER    RECORD(*), STATUS
       INTEGER    NXREC, I, J, N
       LOGICAL    FOUND
C
       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/maplib_common.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_extrart.inc'
       include '/mrao/include/maplib_errors.inc'
C
       CHARACTER    CDESC*8
       INTEGER*4    RTDESC(2)
       EQUIVALENCE (CDESC, RTDESC)
C
       IF (STATUS.NE.0) RETURN
C
       CALL CHREDT(STATUS)
C
       IF (STATUS.EQ.0) THEN
         IF (ISTYRT.NE.1 .OR.
     :       IPEXRT.LE.0 .OR. IPEXRT.GT.MAX_EXTRA) THEN
           STATUS=ILL_EXREDT
         ELSE
C
C    Look for matching descriptor
C
           N=0
           FOUND=.FALSE.
           NXREC=(IPEXRT*PAGE)/ILEXRT
           DO I=1,NXREC
             IF (.NOT.FOUND) THEN
               RTDESC(1)=EXTRART(N+1)
               RTDESC(2)=EXTRART(N+2)
               IF (CHR_CHSAME(DESCRIPTOR,CDESC)) THEN
                 DO J=1,ILEXRT
                   RECORD(J)=EXTRART(N+J)
                 ENDDO
                 FOUND=.TRUE.
               ENDIF
             ENDIF
             N=N+ILEXRT
           ENDDO
C
           IF (.NOT.FOUND) STATUS=NO_EXTREC
C
         ENDIF
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine ENXREC')
C
       END
