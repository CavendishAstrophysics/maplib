*+NXTMAP

       SUBROUTINE NXTMAP (NAME, FILE, SOURCE, COUNT, STATUS)
C      -----------------------------------------------------
C
C  Finds the next file matching a given filename.
C
C  Given:
C      NAME      char*(*)    input filename pattern
C      COUNT     integer     current count of matching files
C
C  Returned:
C      FILE      char*(*)    next matching filename
C      SOURCE    char*(*)    source name
C      COUNT     integer     current count of matching files
C      STATUS    integer     status value
C
C  Returns the next filename from a list of disc files matching NAME.
C  The found filename is returned in FILE and the number of matching
C  files in COUNT.  The STATUS value should be zero on entry, and is
C  normally unchanged.  If the list is exhausted, the returned status
C  will be non-zero (NO_FILE), and COUNT will contain the number of
C  files in the list.  The list will be re-initialised if the routine
C  is entered with a new value for the input file name, or with COUNT
C  equal to zero.  The routine works by executing the ls command to a
C  scratch file, and reading the output.
C
C
C  (DJT, 26 March 90)
C  (PA, 10 October; UNIX implementation; uses ls to a file)
C  (DJT, 21 March 94; revisions)
C
*-
       include '/mrao/include/iolib_constants.inc'

       CHARACTER*(*) NAME, FILE, SOURCE
       CHARACTER  STRING*(IOLEN_FILE), SCRFIL*(IOLEN_FILE),
     *            ONAME*(IOLEN_FILE)
       INTEGER    COUNT, STATUS
       INTEGER    IFILE, LN, LS
C
       include '/mrao/include/iolib_errors.inc'
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       integer     maxt, len_ls, it
       parameter  (maxt = 20)
       character   lsource*(iolen_file), type*4, dir*(iolen_file)
       character*4 type_list(maxt)
C
       logical     test
C
       data  type_list / 'map ','cln ',
     *                   'imap','icln',
     *                   'qmap','qcln',
     *                   'umap','ucln',
     *                   'beam','bset',
     *                   'mi  ','perc',
     *                   'chi ','resi',
     *                   'ccmp','aper',
     *                   'cos ','sin ',
     *                   'amp ','phi '
     *                 /

       SAVE  ONAME, IFILE
       DATA  ONAME /'*'/
       DATA  IFILE /0/
C
       FILE=' '
C
       IF (STATUS.EQ.0) THEN
C
         IF (COUNT.EQ.0 .OR. NAME.NE.ONAME) THEN
C
C    Establish new list of matching files
C
           IF (IFILE.GT.0) CLOSE(IFILE,STATUS='DELETE')
           CALL IO_NXTLUN(IFILE,STATUS)
           CALL IO_NXTSCR(SCRFIL,STATUS)
           IF (STATUS.EQ.0) THEN
             LN=CHR_LENB(NAME)
             STRING='ls '//NAME(1:LN)
             IF (NAME(1:LN).EQ.'*') THEN
               STRING='ls '//NAME(1:LN)//' >&! '//SCRFIL
             ELSE
               STRING='ls '//NAME(1:LN)//'* >&! '//SCRFIL
             ENDIF
             LS = CHR_LENB(STRING)
             call io_system( string(1:ls), status )
             OPEN (IFILE, FILE=SCRFIL,
     :                    ACCESS='SEQUENTIAL',
     :                    STATUS='OLD',
     :                    IOSTAT=STATUS )
             ONAME=NAME
             STRING=' '
           ENDIF
           COUNT=0
         ENDIF
C
C    Read next name from list
C
         IF (STATUS.EQ.0) THEN
 1         READ (IFILE, '(A)', IOSTAT=STATUS) STRING
           IF (STRING.EQ.' ' .OR. STRING.EQ.'No match.') STATUS=NO_FILE
           if (status.eq.0) then
             ln=chr_lenb(string)
             file=string(1:ln)
             call io_brkfil(file,dir,lsource,type)
             len_ls = chr_lenb(lsource)
             source = lsource(1:len_ls)//'-'//type
             test = .false.
             do it=1,maxt
               test = test .or. chr_chsame(type,type_list(it))
             end do
             if (.not.test) goto 1
           end if
           IF (STATUS.EQ.0) THEN
             ln=chr_lenb(string)
             call io_namfil( string(1:ln),file,0,status )
             count = count + 1
           ELSEIF (STATUS.NE.0) THEN
             STATUS=NO_FILE
             CLOSE(IFILE,STATUS='DELETE')
             ONAME='*'
             IFILE=0
           ENDIF
         ENDIF
C
       ENDIF
C
       END
