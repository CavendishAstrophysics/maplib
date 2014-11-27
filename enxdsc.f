


*+ENXDSC

       SUBROUTINE ENXDSC (DESCRIPTORS, NDESC, STATUS)
C      ----------------------------------------------
C
C  Returns a list of extra redtape descriptors.
C
C  Parameters:
C      DESCRIPTORS  char*8(*)   redtape descriptor list
C      NDESC        integer(*)  number of redtape records present
C      STATUS       integer     status value
C
C  Scans the extra redtape area and returns a list of redtape descriptors.
C  The redtape records may be accessed by calls to routine ENXREC.  Note
C  that the extra redtape records are read from the map file, together
C  with the basic map redtape, by routine RDREDT.
C
C  The STATUS value should be zero on entry.  Possible error values are:
C
C        - illegal map redtape (ILL_REDTAPE)
C
C  (DJT, 30 November 89)
C
*-
       CHARACTER  DESCRIPTORS(*)*8
       INTEGER    NDESC, STATUS
       INTEGER    NXREC, I, N
C
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
C    Scan the extra redtape area
C
           N=0
           NDESC=0.
           NXREC=(IPEXRT*PAGE)/ILEXRT
           DO I=1,NXREC
             RTDESC(1)=EXTRART(N+1)
             RTDESC(2)=EXTRART(N+2)
             IF (CDESC.EQ.' ' .OR.
     :          (RTDESC(1).EQ.0 .AND. RTDESC(2).EQ.0) ) THEN
               CONTINUE
             ELSE
               NDESC=NDESC+1
               DESCRIPTORS(NDESC)=CDESC
             ENDIF
             N=N+ILEXRT
           ENDDO
C
         ENDIF
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine ENXDSC')
C
       END
