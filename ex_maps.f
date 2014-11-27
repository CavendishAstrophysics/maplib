C+
       SUBROUTINE EX_MAPS (PARAMETERS, STATUS)
C      ---------------------------------------
C
C  Executes the EXAMINE-MAPS utility program.
C
C  Given:
C      PARAMETERS  char*(*)    input parameter string
C
C  Returned:
C      STATUS      integer     status value
C
C  Program to examine map files on disc and magnetic tape archive.
C  This routine decodes the input parameter string, prompting for more
C  input if necessary, and calls the appropriate EXAMINE-MAPS routine.
C
C  The STATUS value should be zero on entry.
C
C-
       CHARACTER PARAMETERS*(*)
       INTEGER  STATUS, NOPT
C
       PARAMETER  (NOPT=9)
       CHARACTER  OPTIONS(NOPT)*48, OPT*12
       CHARACTER  DEF_DIR*16
       CHARACTER  STRING*64, FILE*64, LFILE*64
       INTEGER    IOUT, IOLD, TERMI, TERMO
       INTEGER    LF
       LOGICAL    FLAG, INTER
C
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       DATA  OPTIONS/
     * ' LIST .......... list map filenames (default)',
     * ' TITLE ......... print map title from redtape',
     * ' ASTRO ......... print astronomical redtape',
     * ' COMPUTING ..... print computing redtape',
     * ' MAPPING ....... print the map making redtape',
     * ' REDTAPE ....... print complete map redtape',
c    * ' SAVE .......... mark map for saving on tape',
     * ' DELETE ........ delete map file from disc',
c    * ' CLEAR ......... clear saved map files from disc',
c    * ' TAPE .......... find map file in tape archive',
     * ' INTERACTIVE ... enter interactive mode',
     * ' QUIT .......... exit from EXAMINE-MAPS'/
C
       IF (STATUS.NE.0) RETURN
C
       CALL IO_ENQOUT(IOUT)
       IOLD=IOUT
C
C  Prime the command line with parameter string
C
       CALL io_setcli(PARAMETERS)
C
C  Get default map user and execution mode
C
       CALL ENMDIR(DEF_DIR,STATUS)
C
C  Get map file name pattern
C
       CALL io_getc('map filename: ',' ',LFILE,STATUS)
       CALL io_makfil(DEF_DIR,LFILE,' ',FILE,LF)
C
C  Get option, default is 'LIST'
C
       OPT='LIST'
       CALL io_getopt('option: ',' ',OPTIONS,NOPT,OPT,STATUS)
       IF (OPT.EQ.'QUIT') STATUS=USR_BREAK
C
C  Get tape volume listing file, default searches (T151) map volumes
C
c      IF (OPT.EQ.'TAPE') THEN
c        STRING='MAP'
CC       IF (match(USER,'RYLE')) STRING='R'
c        IF (INDEX(LFILE,'(').EQ.0) USER=DEF_USER
c        CALL io_getc('tape volume: ',' ',STRING,STATUS)
c        CALL io_makfil(USER,STRING,'VOL',VOLUME,LV)
c        CALL chr_chucas(VOLUME(1:LV))
c        CALL io_brkfil(LFILE,USER,STRING,TYPE)
c        I1=INDEX(USER,':')+1
c        CALL io_makfil(USER(I1:),STRING,TYPE,FILE,LF)
c      ENDIF
C
       INTER=OPT.EQ.'INTERACTIVE'
C
C  Offer interactive mode for DELETE options
C
       IF (OPT.EQ.'DELETE' .OR. OPT.EQ.'CLEAR') THEN
         CALL io_setcli(' ')
         IF
     :   (io_yesno('manual check for delete option?','yes',STATUS)) THEN
           INTER=.TRUE.
         ENDIF
C
       ELSEIF (OPT.NE.'INTERACTIVE') THEN
C
C  Get output file, write header text if not to terminal
C
         CALL IO_OPEOUT(IOUT,STATUS)
         CALL IO_ENQTIO(TERMI,TERMO)
         IF (STATUS.EQ.0) THEN
           IF (IOUT.NE.TERMO) THEN
             LF=chr_lenb(FILE)
             STRING='EXAMINE-MAPS '//FILE(1:LF)
             CALL IO_LOGOUT(STRING(1:LF+13),1)
           ENDIF
         ENDIF
       ENDIF
C
       CALL IO_ENQILF(FLAG)
       IF (FLAG .OR.INTER) WRITE(*,*)
       IF (INTER) CALL IO_SETCLI(' ')
C
C  Call the appropriate subroutine
C
c      IF (OPT.EQ.'TAPE') THEN
c        CALL EXM_TAPE(FILE,VOLUME,STATUS)
c      ELSE
         CALL EXM_DISC(FILE,OPT,INTER,STATUS)
c      ENDIF
C
       CALL IO_CLOSE(IOUT,STATUS)
       CALL IO_SETOUT(IOLD)
       CALL IO_WROUT(' ')
C
       STATUS=0
C
       END
