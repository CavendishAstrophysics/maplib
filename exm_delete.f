C+
       SUBROUTINE EXM_DELETE (FILE, MSAVE, INTER, STATUS)
C      --------------------------------------------------
C
C  Routine to delete a map file from disc.
C
C  MSAVE records whether the map file has already been saved.
C  (MSAVE>0 if not saved, =0 if saved, <0 if save requested.)
C
C  The STATUS value should be zero on entry.
C
C-
       CHARACTER  FILE*(*)
       CHARACTER  STRING*80
       INTEGER    MSAVE, STATUS
       INTEGER    ILIST, LF
       LOGICAL    INTER
C
       include '/mrao/include/iolib_functions.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       IF (STATUS.NE.0) RETURN
C
       CALL io_enqout(ILIST)
C
       IF (MSAVE.NE.1) THEN
         STRING='this map has not yet been saved, '//
     :           'do you want to delete it? '
         IF (INTER) THEN
           IF (io_yesno(STRING,' ',STATUS)) MSAVE=1
c          IF (IABS(MSAVE).NE.1) THEN
c            IF (io_yesno('do you want to save it on tape? ',' ',
c    :                                                    STATUS)) THEN
c              CALL EXM_SAVE(FILE,MSAVE,INTER,STATUS)
c            ENDIF
c          ENDIF
         ELSE
           WRITE(ILIST,*)STRING(1:31)
         ENDIF
       ENDIF
C
C    Delete the file from disc
C
       IF (MSAVE.EQ.1) THEN
         LF=chr_lenb(FILE)
         STRING='rm '//FILE(1:LF)//' >&! /dev/null'
         call io_system(string(1:chr_lenb(string)),status)
         IF (STATUS.EQ.0) THEN
           WRITE(ILIST,*)'... map deleted'
         ELSE
           STRING='deleting '//FILE(1:LF)
           CALL io_wrerr(STATUS,STRING)
         ENDIF
       ENDIF
C
       END
