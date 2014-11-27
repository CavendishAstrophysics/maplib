C+
       SUBROUTINE EXM_LFILE (FILE, LFILE, STATUS)
C      ------------------------------------------
C
C  Routine to return the save list file name.
C
C  Finds the name of the appropriate save list file for the current map.
C  Maps made by MRAO telescopes will be saved by the local archive system,
C  i.e. the save list for CLFST maps is on user T151 or 38MHZ, the save list
C  for RYLE maps is on user RYLE.  Other imported maps will appear in a save
C  list in the default file space of the map owner.
C
C  The STATUS value should be zero on entry.
C
C-
       CHARACTER  FILE*(*), LFILE*(*)
       INTEGER    STATUS
C
       INTEGER    I1, I2
C
$INCLUDE (LIBRARY)MAPLIB-REDTAPE:INCL
C
       IF (STATUS.NE.0) RETURN
C
       IF (RTELES.EQ.1) THEN
         LFILE='(T151)SAVE-MAPS:LIST'
       ELSEIF (RTELES.EQ.2) THEN
         LFILE='(38MHZ)SAVE-MAPS:LIST'
       ELSEIF (RTELES.EQ.4 .OR. RTELES.EQ.5) THEN
         LFILE='(RYLE)SAVE-MAPS:LIST'
       ELSEIF (FREQ.GT.150.D0 .AND. FREQ.LT.152.D0) THEN
         LFILE='(T151)SAVE-MAPS:LIST'
       ELSEIF (FREQ.GT.36.D0 .AND. FREQ.LT.40.D0) THEN
         LFILE='(38MHZ)SAVE-MAPS:LIST'
       ELSE
         I2=INDEX(FILE,')')
         I1=INDEX(FILE(1:I2),':')+1
         IF (INDEX(FILE(I1:I2),'RYLE').GT.0) THEN
           LFILE='(RYLE)SAVE-MAPS:LIST'
         ELSE
           LFILE='('//FILE(I1:I2)//'SAVE-MAPS:LIST'
         ENDIF
       ENDIF
C
       END
