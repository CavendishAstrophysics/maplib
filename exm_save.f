C
C
C
C
C
C+
       SUBROUTINE EXM_SAVE (FILE, MSAVE, INTER, STATUS)
C      ------------------------------------------------
C
C  Routine to mark a map file on disc for saving on tape.
C
C  MSAVE records whether the map file has already been saved.
C  (MSAVE>0 if saved, =0 if not saved, <0 if save requested.)
C
C  The STATUS value should be zero on entry.
C
C-
       CHARACTER  FILE*(*)
       INTEGER    MSAVE, STATUS
       LOGICAL    INTER
C
       CHARACTER  STRING*64, LFILE*64
       INTEGER    LF, IFILE, WFILE
       INTEGER    I1, I2, ILIST
C
       INTEGER LWD, PAGE
C1     PARAMETER (LWD=2,PAGE=2048/LWD)
C5     PARAMETER (LWD=4,PAGE=2048/LWD)
C
include '/mrao/include/maplib_redtape.inc'
include '/mrao/include/iolib_functions.inc'
include '/mrao/include/chrlib_functions.inc'
C
       IF (STATUS.NE.0) RETURN
C
       CALL io_enqout(ILIST)
C
C  Check current save status
C
       IFILE=10
       IF (MSAVE.EQ.-1) THEN
         WRITE(ILIST,*)'this map is already on the save list'
       ELSEIF (MSAVE.EQ.1) THEN
         STRING='this map has already been saved, '//
     :           'do you want to save it again? '
         IF (INTER) THEN
           IF (io_yesno(STRING,' ',STATUS)) MSAVE=0
         ELSE
           WRITE(ILIST,*)STRING(1:31)
         ENDIF
       ELSE
         MSAVE=0
       ENDIF
C
C    Find the appropriate save list file
C
       IF (MSAVE.EQ.0) THEN
         CALL EXM_LFILE(FILE,LFILE,STATUS)
C
C    Add the filename to the list file
C
         OPEN (IFILE, FILE=LFILE, ACCESS='WA', STATUS='UNKNOWN',
     :                                            IOSTAT=STATUS)
         IF (STATUS.EQ.0) THEN
           WRITE(IFILE,*)FILE
           CLOSE(IFILE)
           LF=chr_lenb(LFILE)
           WRITE(ILIST,*)'... map file name added to ',LFILE(1:LF)
C
C    Mark the map redtape saved
C
           OPEN (IFILE, FILE=FILE, ACCESS='WX', STATUS='OLD',
     :                                            IOSTAT=STATUS)
           IF (STATUS.EQ.0) THEN
             MSAVE=-1
             RTSFLG=1
             STATUS=WFILE(IFILE,0,IRTALL,0,PAGE)
             CLOSE(IFILE)
           ENDIF
         ENDIF
       ENDIF
C
       END
