


*+STXREC

       SUBROUTINE STXREC (DESCRIPTOR, RECORD, STATUS)
C      ----------------------------------------------
C
C  Sets up a record of extra redtape.
C
C  Parameters:
C      DESCRIPTOR  char*8      redtape descriptor
C      RECORD      integer(*)  redtape record (128 4-byte words)
C      STATUS      integer     status value
C
C  Writes the given record as an extra redtape record with the given
C  redtape descriptor.  The record is entered into the extra redtape
C  buffer, and will overwrite any existing record with the same
C  descriptor.  A date stamp is added to the record by this routine.
C  The extra redtape will be written to the map file by routine WRREDT.
C
C  The STATUS value should be zero on entry.  Possible error values are:
C
C     - illegal map redtape (ILL_REDTAPE)
C     - no room in extra redtape (FLL_EXREDT)
C
C  (DJT, 29 November 89)
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
C    Look for matching descriptor, or empty redtape record
C
           N=0
           FOUND=.FALSE.
           NXREC=(IPEXRT*PAGE)/ILEXRT
           DO I=1,NXREC
             IF (.NOT.FOUND) THEN
               RTDESC(1)=EXTRART(N+1)
               RTDESC(2)=EXTRART(N+2)
               IF (CDESC.EQ.' ' .OR.
     :              chr_CHSAME(DESCRIPTOR,CDESC) .OR.
     :             (RTDESC(1).EQ.0 .AND. RTDESC(2).EQ.0)) THEN
                 CDESC=DESCRIPTOR
                 EXTRART(N+1)=RTDESC(1)
                 EXTRART(N+2)=RTDESC(2)
                 CALL util_enqnow(EXTRART(N+3))
                 DO J=4,ILEXRT
                   EXTRART(N+J)=RECORD(J)
                 ENDDO
                 FOUND=.TRUE.
               ENDIF
             ENDIF
             N=N+ILEXRT
           ENDDO
C
           IF (.NOT.FOUND) STATUS=FLL_EXREDT
C
         ENDIF
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine STXREC')
C
       END
