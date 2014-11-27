C+
       SUBROUTINE EXM_DISC (FILE, OPTION, INTER, STATUS)
C      -------------------------------------------------
C
C  Executes EXAMINE-MAPS functions for map files on disc, according
C  to the specified OPTION.  All disc files matching the pattern FILE
C  will be examined in turn.  The logical flag INTER denotes interactive
C  working if set.  The STATUS value should be zero on entry.
C
C-
       CHARACTER*(*) FILE, OPTION
       INTEGER       STATUS
C
       INTEGER    NFILES
       PARAMETER (NFILES=256)
       CHARACTER  FILES(NFILES)*64, NFILE*64
C
       INTEGER    NOPT
       PARAMETER  (NOPT=8)
       CHARACTER  OPTIONS(NOPT)*48, OPT*12
       CHARACTER  STRING*80, FNAME*64, DIR*32, SOURCE*16
       CHARACTER  CDATE*16, SAVED*16, UNAME*10, CFILE*6, CPAGE*6
       INTEGER*4  BUFFER(16), IDATE, ICR, IRD, IWR, NP
       INTEGER    ITIME(7), ILIST, I, I1, I2, IN, J
       INTEGER    LD, LF, LP, LS, LU, MF, MSAVE, NF, NBYTE, NR
       INTEGER    KEY(2), NKEY, LSTATUS
       LOGICAL    INTER
C
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       DATA OPTIONS/
     * ' TITLE ......... print map title from redtape',
     * ' ASTRO ......... print astronomical redtape',
     * ' COMPUTING ..... print computing redtape',
     * ' MAPPING ....... print the map making redtape',
     * ' REDTAPE ....... print complete map redtape',
C    * ' SAVE .......... mark map for saving on tape',
     * ' DELETE ........ delete map file from disc',
     * ' NEXT .......... find next map file on disc',
     * ' QUIT .......... exit from EXAMINE-MAPS'/
C
       IF (STATUS.NE.0) RETURN
C
       CALL io_enqout(ILIST)
C
C  Set up the selection criterion for the 'CLEAR' option, current date
C  minus one month
C
       IF (OPTION.EQ.'CLEAR') THEN
         call util_enqtim(ITIME)
         call util_enqdat(ITIME(4))
         ITIME(5)=ITIME(5)-1
         IF (ITIME(5).EQ.0) THEN
           ITIME(6)=ITIME(6)-1
           ITIME(5)=12
         ENDIF
         call util_datint(ITIME,IDATE)
         WRITE(ILIST,'(X,A)')
     :     'Delete all saved maps not used within the past month'
       ENDIF
C
C  Examine all files matching FILE
C
    1  MF=0
       NF=0
       NR=0
C
       DO WHILE (STATUS.EQ.0)
         CALL NXTMAP(FILE,NFILE,SOURCE,NF,STATUS)
C
C    If last file, sort the files and process the list
C
         IF (STATUS.NE.0) THEN
           LSTATUS=STATUS
           STATUS=0
C
           NKEY=1
           KEY(1)=I2+1
           KEY(2)=64
c          CALL util_qsortc(FILES,NR,KEY,NKEY)
C
           DO I=1,NR
C
             FNAME=FILES(I)
             LF=chr_lenb(FNAME)
             I1=chr_ilstc(FNAME,'/')-1
             DIR=FNAME(1:I1)
             IN=I1+2
C
C    Read object entry and map redtape
C
             CALL EXM_READ(FNAME,BUFFER,MSAVE,UNAME,STATUS)
             CALL chr_chrjus(UNAME,LU)
             IF (STATUS.NE.0) THEN
               CALL io_wrerr(STATUS,'reading '//FNAME(1:LF))
               STATUS=0
               GOTO 4
             ENDIF
C
             DO J=9,11
               CALL LTIME(BUFFER(J),ITIME)
               ITIME(5)=ITIME(5)+1
               IF (ITIME(6).LT.50) ITIME(6)=ITIME(6)+2000
               IF (ITIME(6).LT.100) ITIME(6)=ITIME(6)+1900
               CALL util_datint(ITIME,BUFFER(J))
             ENDDO
C
C      Apply selection criterion for the CLEAR option
C
             IF (OPTION.EQ.'CLEAR') THEN
               IRD=IDATE-BUFFER(9)
               IWR=IDATE-BUFFER(10)
               ICR=IDATE-BUFFER(11)
               IF (MSAVE.NE.1 .OR.
     :             (IRD.LT.0 .AND. BUFFER(9).NE.0) .OR.
     :             (IWR.LT.0 .AND. BUFFER(10).NE.0)) GOTO 4
             ENDIF
C
C      Set up output text and print
C
             IF (STATUS.EQ.0) THEN
               NBYTE=BUFFER(8)
               MF=MF+1
C
               IF (MSAVE.EQ.1) THEN
                 SAVED='saved on tape'
               ELSEIF (MSAVE.EQ.-1) THEN
                 SAVED='save requested'
               ELSE
                 SAVED='not saved'
               ENDIF
               CALL util_extdat(BUFFER(11),1,CDATE,LD)
C
               IF (I.EQ.1) THEN
                 IF (MF.GT.1) WRITE(ILIST,*)
                 WRITE(ILIST,'(X,A,A)') 'Directory ',DIR
                 WRITE(ILIST,*)
               ENDIF
               IF (OPTION.EQ.'LIST') THEN
                 IF (I.EQ.1) THEN
                   WRITE(ILIST,'(29X,A,6X,A,8X,A)')
     :                                    'bytes','created','owner'
                 ENDIF
                 WRITE(ILIST,'(2X,A24,I8,4X,A,3X,A,4X,A))')
     :                      FNAME(IN:),NBYTE,CDATE(1:9),UNAME,SAVED
               ELSEIF (.NOT.INTER) THEN
                 WRITE(ILIST,'(X,A32,I8,2A)')
     :                   FNAME(IN:),NBYTE,' bytes, ',CDATE(1:9)
               ELSE
                 WRITE(STRING,'(5A)')
     :               FNAME(IN:LF),', ',CDATE(1:9),', ',SAVED
                 LS=chr_lenb(STRING)
                 STRING(LS+1:)=' : '
                 LS=LS+2
               ENDIF
C
C      Interactive mode, prompt for option
C
    3          IF (INTER) THEN
                 OPT='NEXT'
                 CALL io_getopt(STRING(1:LS),' ',OPTIONS,NOPT,OPT,
     :                                                        STATUS)
               ELSE
                 OPT=OPTION
               ENDIF
C
C      Perform required function
C
               IF (OPT.EQ.'TITLE') THEN
                 CALL EXM_PRINT('TITLE',STATUS)
                 IF (INTER) GOTO 3
               ELSEIF (OPT.EQ.'ASTRO') THEN
                 CALL EXM_PRINT('ASTRO',STATUS)
                 IF (INTER) GOTO 3
               ELSEIF (OPT.EQ.'COMPUTING') THEN
                 CALL EXM_PRINT('COMP',STATUS)
                 IF (INTER) GOTO 3
               ELSEIF (OPT.EQ.'MAPPING') THEN
                 CALL EXM_PRINT('MAPPING',STATUS)
                 IF (INTER) GOTO 3
               ELSEIF (OPT.EQ.'REDTAPE') THEN
                 CALL EXM_PRINT('ALL',STATUS)
                 IF (INTER) GOTO 3
               ELSEIF (OPT.EQ.'SAVE') THEN
c                CALL EXM_SAVE(FNAME,MSAVE,INTER,STATUS)
               ELSEIF (OPT.EQ.'DELETE' .OR. OPT.EQ.'CLEAR') THEN
                 CALL EXM_DELETE(FNAME,MSAVE,INTER,STATUS)
               ELSEIF (OPT.EQ.'QUIT') THEN
                 STATUS=USR_BREAK
               ENDIF
C
               IF (STATUS.EQ.USR_BREAK) GOTO 5
               IF (io_attn(STATUS)) GOTO 5
C
             ENDIF
    4        CONTINUE
           ENDDO
C
           IF (STATUS.EQ.0) STATUS=LSTATUS
           NR=0
C
         ENDIF
C
         NR=NR+1
         LF=chr_lenb(NFILE)
         FILES(NR)=NFILE(1:LF)
C
       ENDDO
C
    5  IF (STATUS.GT.0) THEN
         IF (MF.GT.0) THEN
           IF (OPTION.EQ.'LIST') THEN
             CALL chr_chitoc(MF,CFILE,LF)
             WRITE(CPAGE,'(I6)')NP
             CALL chr_chljus(CPAGE,LP)
             WRITE(ILIST,'(/X,5A)')
     :        'Total of ',CFILE(1:LF),' files'
           ENDIF
         ELSEIF (OPTION.EQ.'DELETE' .OR. OPTION.EQ.'CLEAR') THEN
           WRITE(ILIST,*)'*** no files deleted'
         ELSE
           LF=chr_lenb(FILE)
           WRITE(ILIST,*)'*** no file found matching ',FILE(1:LF)
         ENDIF
       ENDIF
C
       IF (OPTION.EQ.'INTERACTIVE') THEN
         STATUS=0
         STRING=' '
         WRITE(ILIST,*)
         CALL io_getc('map filename:',' ',STRING,STATUS)
         CALL io_makfil(DIR,STRING,'map',FILE,LF)
         IF (STRING.NE.' ') THEN
           WRITE(ILIST,*)
           GOTO 1
         ENDIF
       ENDIF
C
       END
