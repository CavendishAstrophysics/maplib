C
C
C
C
C
C+
       SUBROUTINE EXM_TAPE (FILE, VOLUME, STATUS)
C      ------------------------------------------
C
C  Prints file statistics for files on magnetic tape archive.
C
C  Prints details of files stored on magnetic tape by reading the
C  volume listing files produced by the TAPEREAD utility.  The routine
C  finds the tape listing for tape volume VOLUME, and prints entries
C  for files matching FILE.
C
C  The STATUS value should be zero on entry.
C
C-
       CHARACTER*(*) FILE, VOLUME
       INTEGER       ILIST, STATUS
C
       INTEGER       NFILES
       PARAMETER    (NFILES=64)
       CHARACTER*64  FILES(NFILES)
C
       CHARACTER*80  LINE, HEADER
       CHARACTER*64  TFILE, NFILE
       CHARACTER*6   CFILE, CPAGE
       INTEGER*4     NP
       INTEGER       I1, I2, IT, IV, IV1, IV2, IFILE
       INTEGER       L, LF, LP, LT, LV
       INTEGER       MF, NF, NT, NV, NPAGE
       INTEGER       KEY(2), NKEY
       LOGICAL       VMATCH
C
$INCLUDE (LIBRARY)SINTRAN-ERRORS:INCL
$INCLUDE (LIBRARY)IOLIB-FUNCTIONS:INCL
$INCLUDE (LIBRARY)CHRLIB-FUNCTIONS:INCL
C
       IF (STATUS.NE.0) RETURN
C
       NF=0
       NP=0
       NV=0
       LF=LENB(FILE)
       CALL ENQOUT(ILIST)
C
C  Find the appropriate tape listing file
C
       IV1=INDEX(VOLUME,')')+1
       IV2=INDEX(VOLUME,':VOL')-1
       LV=MIN0(IV1+4,IV2)
       TFILE=VOLUME(1:LV)//':VOL'
C
C  Scan all listing files matching TFILE
C
       NT=0
       DO WHILE (STATUS.EQ.0)
         CALL NXTFIL(TFILE,NFILE,IT,NT,STATUS)
         IF (STATUS.EQ.0) THEN
           FILES(NT)=NFILE
         ENDIF
       ENDDO
C
C  Sort the volume files before processing
C
      IF (STATUS.EQ.NO_FILE) THEN
        NKEY=1
        KEY(1)=1
        KEY(2)=LEN(NFILE)
        CALL QSORTC(FILES,NT,KEY,NKEY)
        STATUS=0
      ENDIF
C
      DO IT=1,NT
        NFILE=FILES(IT)
C
C    Select RYLE volume files matching 'RxxxxM:VOL'
C
CC       IF (INDEX(NFILE,'RYLE)').GT.0) THEN
CC         IF (INDEX(NFILE,'M:VOL').EQ.0) GOTO 3
CC       ENDIF
C
         CALL OPEFIL(IFILE,NFILE,'READ',0,STATUS)
         IF (STATUS.EQ.0) THEN
C
C    Identify volume and owner
C
           HEADER=' '
    1      READ(IFILE,'(A)',END=2)LINE
           IV=INDEX(LINE,'Volume')
           IF (IV.GT.0) THEN
             VMATCH=.FALSE.
             IF (IV1.GT.IV2 .OR.
     :           INDEX(LINE(IV+7:),VOLUME(IV1:IV2)).GT.0) THEN
c    :           MATCH(VOLUME(IV1:IV2),LINE(IV+7:IV+12))) THEN
               HEADER=LINE(1:40)
               READ(IFILE,'(A)',END=2)LINE
               READ(IFILE,'(A)',END=2)LINE
               HEADER(41:)=LINE(41:)
               VMATCH=.TRUE.
               NV=NV+1
               MF=0
             ENDIF
           ELSEIF (VMATCH) THEN
C
C    Print all entries matching FILE
C
             I1=INDEX(LINE,'(')
             IF (I1.GT.0) THEN
               I2=INDEX(LINE(I1:),' ')
               IF (LF.EQ.0 .OR.
     :             FMATCH(FILE(1:LF),LINE(I1:I1+I2-1))) THEN
C
C      Print header before first matching entry
C
                 IF (MF.EQ.0) THEN
                   IF (NF.GT.0) WRITE(ILIST,*)
                   WRITE(ILIST,*)HEADER(1:LENB(HEADER(1:40)))
                   HEADER(1:40)=' '
                   WRITE(ILIST,*)HEADER(1:LENB(HEADER))
                 ENDIF
C
C      Print matching entry and accumulate page count
C
                 L=LENB(LINE)
                 WRITE(ILIST,*)LINE(1:L)
                 CALL CHCTOI(LINE(72:L),NPAGE,STATUS)
                 IF (STATUS.NE.0) STATUS=0
                 NP=NP+NPAGE
                 MF=MF+1
                 NF=NF+1
               ENDIF
             ENDIF
             IF (ATTN(STATUS)) GOTO 4
           ENDIF
           GOTO 1
C
    2      CLOSE(IFILE)
         ENDIF
    3    CONTINUE
       ENDDO
C
C  Print out totals
C
    4  IF (NF.GT.0) THEN
         CALL CHITOC(NF,CFILE,LF)
         WRITE(CPAGE,'(I6)')NP
         CALL CHLJUS(CPAGE,LP)
         WRITE(ILIST,'(/X,5A)')
     :    'Total of ',CFILE(1:LF),' files,  ',CPAGE(1:LP),' pages'
       ELSEIF (NV.GT.0) THEN
         WRITE(ILIST,*)'*** no file found matching ',FILE(1:LF)
       ELSE
         WRITE(ILIST,*)'*** tape volume ',VOLUME(1:IV2),' not found'
       ENDIF
C
       END
