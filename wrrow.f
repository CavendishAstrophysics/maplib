*+WRROW

       SUBROUTINE WRROW (IUNIT, IV, DATA, STATUS)
C      ------------------------------------------
C
C  Writes a row of data to a map file.
C
C  Given:
C      IUNIT     integer     logical unit number
C      IV        integer     V (row) coordinate
C      DATA      real*4()    map data
C
C  Returned:
C      STATUS    integer     status value
C
C  Routine to write the map row data stored in array DATA, in external
C  units, to the file opened on unit IUNIT.  Parameter values currently
C  set up in the redtape common blocks identify the internal data type,
C  the size of the map and also the conversion from external to internal
C  units.  Note that for aperture data the array should be set up to
C  provide complex data values.
C
C  NB: Routine WRROW should only be used for updating rows of an existing
C  map file.  Use the routine WRMAP to create a new map file.
C
C  The STATUS value should be zero on entry:  possible error conditions
C  on exit are:
C
C      - invalid map redtape (ILL_REDTAPE)
C      - unexpected IO_SYSTEM I/O error code
C
C  (DJT, 9 July 90)
C  (DJT, 1 April 92; Unix implementation)
C
*-
       INTEGER    IUNIT, IV, STATUS
       INTEGER    IBLK, IBLK1, NW, NWD, NX
       INTEGER    IX1, IXX, IX, IY, IZ
       INTEGER    I2, I4
       REAL*4     DATA(*)
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_common.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
C  Check map redtape
C
       CALL CHREDT(STATUS)
C
C  Check the given row number
C
       IF (STATUS.EQ.0) THEN
         IF (IV.GT.IVMAP1 .OR. IV.LT.IVMAP2) THEN
           STATUS=UV_OUTMAP
         ENDIF
       ENDIF
C
       IF (STATUS.NE.0) GOTO 2
C
C
       IX1=1
       NW=IWMAX
       NX=IXMAX*IWMAX
       IF (ISWD.EQ.4) NX=2*NX
C
C  Read/write row using the available buffer
C
       IZ=0
       IBLK1=0
       NWD=MPBLK/LWD
       IY=IVMAP1-IV+1
C
C  Find block and offset for start of row IY
C
       IXX=(IY-1)*IXMAX*IWMAX+IX1
       IF (ISWD.EQ.1) IXX=IXX*2
       IF (ISWD.EQ.2) IXX=IXX*4
       IF (ISWD.EQ.3) IXX=IXX*4
       IF (ISWD.EQ.4) IXX=IXX*8
       IBLK=MPBLK1+(IXX-1)/MPBLK
       IXX=MOD(IXX,MPBLK)
       I4=IXX/4
       I2=IXX/2
C
       DO 1 IX=1,NX
C
         IF (IBLK.NE.IBLK1) THEN
           CALL IO_RDFILE(IUNIT,IBLK,R4BUF,NWD,STATUS)
           IF (STATUS.NE.0) GOTO 2
           IBLK1=IBLK
         ENDIF
C
         IZ=IZ+1
C
C    Internal data type integer*2
C
         IF (ISWD.EQ.1) THEN
           IF (DATA(IZ).EQ.BLANK) THEN
             I2BUF(I2)=IBLNK2
           ELSE
             I2BUF(I2)=NINT(DATA(IZ)/SCALEF+ZEROL)
           ENDIF
           I2=I2+NW
           I4=I2/2
C
C    Internal data type integer*4
C
         ELSEIF (ISWD.EQ.2) THEN
           IF (DATA(IZ).EQ.BLANK) THEN
             I4BUF(I4)=IBLNK4
           ELSE
             I4BUF(I4)=NINT(DATA(IZ)/SCALEF+ZEROL)
           ENDIF
           I4=I4+NW
           I2=I4*2
C
C    Internal data type real*4, complex*8
C
         ELSEIF (ISWD.GE.3) THEN
           IF (DATA(IZ).EQ.BLANK) THEN
             R4BUF(I4)=BLANK
           ELSE
             R4BUF(I4)=DATA(IZ)/SCALEF+ZEROL
           ENDIF
           I4=I4+NW
           I2=I4*2
         ELSE
C
           STATUS=ILL_DATYPE
           GOTO 2
         ENDIF
C
         IF (I2.GT.NWD*2 .OR. IX.EQ.NX) THEN
           CALL IO_WRFILE(IUNIT,IBLK,R4BUF,NWD,STATUS)
           IF (STATUS.NE.0) GOTO 2
           IBLK=IBLK+1
           I2=I2-NWD*2
           I4=I4-NWD
         ENDIF
C
    1  CONTINUE
C
    2  IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine WRROW')
C
       END
