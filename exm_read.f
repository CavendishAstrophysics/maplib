C+
       SUBROUTINE EXM_READ (FILE, BUFFER, MSAVE, UNAME, STATUS)
C      --------------------------------------------------------
C
C  Routine to read the object entry and redtape from a map file
C  on disc.  If the user has directory access to the file, the
C  object entry will be preserved during the process of reading
C  the redtape (i.e. opening the file is not recorded).
C
C  MSAVE reports whether the map file has been saved on tape.
C  (MSAVE>0 if saved, =0 if not saved, <0 if save requested.)
C
C  The STATUS value should be zero on entry.
C
C-
       CHARACTER  FILE*(*)
c      CHARACTER  STRING*80
       CHARACTER  UNAME*10
       INTEGER*4  BUFFER(13)
       INTEGER    MSAVE, STATUS
C
c      CHARACTER  LFILE*64
       INTEGER    IFILE, LF
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       IF (STATUS.NE.0) RETURN
C
C  Find the file indexes and read the object entry
C
       MSAVE=0
       IFILE=10
       LF=chr_lenb(FILE)
       CALL io_rdobjn(FILE,BUFFER,STATUS)
C
C    Open the map file and read the redtape
C
       CALL OPEMAP(IFILE,FILE,'READ',0,STATUS)
       CALL RDREDT(IFILE,0,STATUS)
       CLOSE(IFILE)
       IF (STATUS.EQ.0) THEN
C
C      Return map owner name
C
         UNAME=' '
         IF (IRT0(36).NE.0) UNAME=RTOWNR
C
C      Check whether map file has been saved on tape.
C
         IF (STATUS.EQ.0) THEN
           MSAVE=RTSFLG
           IF (MSAVE.EQ.1) THEN
c            CALL EXM_LFILE(FILE,LFILE,STATUS)
c            OPEN(IFILE, FILE=LFILE, ACCESS='READ', STATUS='OLD',
c    :                                              IOSTAT=STATUS)
c            IF (STATUS.EQ.0) THEN
c              DO WHILE (STATUS.EQ.0)
c                READ(IFILE,'(A)',END=1)STRING
c                IF (INDEX(STRING,FILE(1:LF)).GT.0) THEN
c                  STATUS=-1
c                  MSAVE=-1
c                ENDIF
c              ENDDO
c   1          CLOSE(IFILE)
c            ENDIF
             STATUS=0
           ENDIF
         ENDIF
       ENDIF
C
       END
