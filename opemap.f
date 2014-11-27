*+OPEMAP

       SUBROUTINE OPEMAP (IUNIT, FILE, ACCESS, IPRINT, STATUS)
C      -------------------------------------------------------
C
C  Opens a map file.
C
C  Given:
C      FILE      char*(*)    filename
C      ACCESS    char*(*)    access code, 'READ', 'WRITE', or 'UPDATE'
C      IPRINT    integer     report flag
C
C  Returned:
C      FILE      char*(*)    filename
C      IUNIT     integer     logical unit number
C      STATUS    integer     status value
C
C  Attempts to open a map file with access 'READ', 'WRITE', or 'UPDATE',
C  and returns the logical unit number IUNIT if successful.  If the file is
C  opened for 'READ' or 'UPDATE' a check is made that it is a valid map file.
C  A new file opened for 'WRITE' will be created using the size in pages
C  currently set up in  the redtape common blocks; an existing file will be
C  expanded to this size.  Note that the common blocks are not updated by
C  this routine.  Various levels of interaction are available:
C
C  IPRINT=1,  the full opened file name is printed on the output device.
C  IPRINT=2,  the routine prompts for re-try if the open fails.
C  IPRINT=3,  the routine prompts for confirmation before opening the file.
C
C  Note that if IPRINT>1, the filename FILE may be updated interactively
C  within the routine, and so should be provided as a variable, not a
C  constant, on entry.
C
C  The STATUS value should be zero on entry:  the returned status value
C  is zero if successful.  Other possible values are:
C
C      - invalid access code (ILL_ACCESS)
C      - invalid map redtape (ILL_REDTAPE)
C      - unexpected IO_SYSTEM filesystem error code
C
C  (DJT, 19 January 90)
C  (DJT, 11 March 92; Unix implementation)
C
*-
       CHARACTER*(*) FILE, ACCESS
       CHARACTER*64  STRING
       INTEGER    IUNIT, IPRINT, STATUS
       INTEGER    BSIZE, NPAGES, NWD
       INTEGER    IPR, LS
       LOGICAL    YES
C
       include '/mrao/include/maplib_common.inc'
       include '/mrao/include/maplib_errors.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       CHARACTER  HEADER*12
       EQUIVALENCE (I4BUF,HEADER)
C
       IF (STATUS.NE.0) RETURN
C
C  Apply default directory path and file type
C
       CALL IO_MAKFIL(' ',FILE,'map',STRING,LS)
C
C  Open map for READ only, check redtape header
C
       IF (CHR_CHSAME(ACCESS,'READ')) THEN
         BSIZE=PAGE*LWD
         CALL IO_OPERAN(IUNIT,STRING,'READ',BSIZE,IPRINT,STATUS)
         IF (STATUS.EQ.0) THEN
           NWD=PAGE
           CALL IO_RDFILE(IUNIT,1,I4BUF,NWD,STATUS)
           IF (STATUS.EQ.0) THEN
             IF (HEADER.NE.'MAP' .AND. HEADER.NE.'APERTURE') THEN
               STATUS=ILL_REDTAPE
             ENDIF
           ENDIF
         ENDIF
C
C  Open map for UPDATE, check redtape header
C
       ELSEIF (CHR_CHSAME(ACCESS,'UPDATE')) THEN
         BSIZE=PAGE*LWD
         CALL IO_OPERAN(IUNIT,STRING,'WRITE',BSIZE,IPRINT,STATUS)
         IF (STATUS.EQ.0) THEN
           NWD=PAGE
           CALL IO_RDFILE(IUNIT,1,I4BUF,NWD,STATUS)
           IF (STATUS.EQ.0) THEN
             IF (HEADER.NE.'MAP' .AND. HEADER.NE.'APERTURE') THEN
               STATUS=ILL_REDTAPE
             ENDIF
           ENDIF
         ENDIF
C
C  Open map for WRITE.  Create a new file if necessary.
C
       ELSEIF (CHR_CHSAME(ACCESS,'WRITE')) THEN
         IPR=MIN0(1,IPRINT)
         NPAGES=NPTOT
         BSIZE=PAGE*LWD
         IF (ISWD.LT.1) NPAGES=0
         INQUIRE (FILE=STRING(1:LS), EXIST=YES, IOSTAT=STATUS)
         IF (.NOT.YES) THEN
           STATUS=0
           CALL IO_CREFIL(STRING,NPAGES,.TRUE.,IPRINT,STATUS)
           CALL IO_OPERAN(IUNIT,STRING,'WRITE',BSIZE,IPR,STATUS)
         ELSE
           CALL IO_EXPFIL(STRING,NPAGES,.TRUE.,IPRINT,STATUS)
           CALL IO_OPERAN(IUNIT,STRING,'WRITE',BSIZE,IPR,STATUS)
         ENDIF
C
       ELSE
         STATUS=ILL_ACCESS
       ENDIF
C
       IF (IPRINT.GT.1) FILE=STRING
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine OPEMAP')
C
       END
