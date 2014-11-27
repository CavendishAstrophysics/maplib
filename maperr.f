*+MAPERR

       SUBROUTINE MAPERR (STATUS, TEXT)
C      --------------------------------
C
C  Writes an error message to the error device.
C
C  Given:
C      STATUS    integer     status value
C      TEXT      char*(*)    message text
C
C  Writes an error message to the error device, appropriate to the given
C  status value, and including the message text if this is not blank.
C  The message is also written to the error log file if this has been
C  set up by a previous call to IOLIB routine SETLOG.
C
C  This routine sets up the error message file for the MAPLIB library
C  and calls the IOLIB routine WRERR.
C
*-
       CHARACTER  TEXT*(*)
       INTEGER    ISTAT, STATUS
C
       CHARACTER   ERRFIL*32
       PARAMETER ( ERRFIL = '/mrao/include/maplib_errors.inc')
C
       include '/mrao/include/iolib_errors.inc'
C
       ISTAT=IOLIB_OK
       CALL IO_SETMSG(ERRFIL,ISTAT)
       IF (ISTAT.NE.IOLIB_OK) CALL IO_WRERR(ISTAT,ERRFIL)
C
       CALL IO_WRERR(STATUS,TEXT)
C
       END
