*+WRMAP

       SUBROUTINE WRMAP (IUNIT, DATA, STATUS)
C      --------------------------------------
C
C  Writes map data to a map file.
C
C  Given:
C      IUNIT     integer     logical unit number
C      DATA      real*4()    map data
C
C  Returned:
C      STATUS    integer     status value
C
C  Routine to write the map stored by rows in array DATA, in external
C  units, to the file opened on unit IUNIT.  Parameter values currently
C  set up in the redtape common blocks identify the internal data type,
C  the size of the map and also the conversion from external to internal
C  units.  Note that for aperture data the array should be set up to
C  provide complex data values.
C
C  If the internal data code ISWD>0, the complete map is written to the
C  file.  If ISWD<0, only non-blank data points, together with their
C  map coordinates, are written out.
C
C  The STATUS value should be zero on entry:  possible error conditions
C  on exit are:
C
C      - invalid map redtape (ILL_REDTAPE)
C      - unexpected IO_SYSTEM I/O error code
C
C  (DJT, 5 December 89)
C  (DJT, 23 March 92; Unix implementation)
C
*-
       REAL*4     DATA(*)
       INTEGER    IUNIT, STATUS
       INTEGER    I2, I4, IBLK
       INTEGER    NBLK, NWD, MWD
       INTEGER    IW, IX, IY, IZ, NPAGES
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
       IF (STATUS.NE.0) GOTO 3
C
C
C  If internal data type ISWD>0, write the complete map
C
       IF (ISWD.GT.0) THEN
C
C    Write rows in multiples, using the available buffer
C
         I2=0
         I4=0
         IZ=0
         IBLK=MPBLK1
         NBLK=(MAXBUF*4)/MPBLK
C
         DO 1 IY=1,IYMAX
         DO 1 IX=1,IXMAX
         DO 1 IW=1,IWMAX
C
           IZ=IZ+1
C
C      Internal data type integer*2
C
           IF (ISWD.EQ.1) THEN
             IF (DATA(IZ).EQ.BLANK) THEN
               I2BUF(I2+1)=IBLNK2
             ELSE
c              I2BUF(I2+1) ININT(DATA(IZ)/SCALEF+ZEROL)
               I2BUF(I2+1)=NINT(DATA(IZ)/SCALEF+ZEROL)
             ENDIF
             I2=I2+1
             I4=I2/2
C
C      Internal data type integer*4
C
           ELSEIF (ISWD.EQ.2) THEN
             IF (DATA(IZ).EQ.BLANK) THEN
               I4BUF(I4+1)=IBLNK4
             ELSE
c              I4BUF(I4+1)=ININT(DATA(IZ)/SCALEF+ZEROL)
              I4BUF(I4+1)=NINT(DATA(IZ)/SCALEF+ZEROL)
             ENDIF
             I2=I2+2
             I4=I4+1
C
C      Internal data real*4 or complex*8
C
           ELSEIF (ISWD.GE.3) THEN
             IF (DATA(IZ).EQ.BLANK) THEN
               R4BUF(I4+1)=BLANK
             ELSE
               R4BUF(I4+1)=DATA(IZ)/SCALEF+ZEROL
             ENDIF
             I2=I2+2
             I4=I4+1
           ENDIF
C
           IF (I4.EQ.MAXBUF) THEN
             NWD=(MAXBUF*4)/LWD
             CALL IO_WRFILE(IUNIT,IBLK,R4BUF,NWD,STATUS)
             IF (STATUS.NE.0) GOTO 3
             IBLK=IBLK+NBLK
             I4=0
             I2=0
           ENDIF
C
    1    CONTINUE
C
C    Write incomplete buffer
C
         IF (I2.GT.0) THEN
           MWD=MPBLK/LWD
           NWD=((I4-1)/MWD+1)*MWD
           CALL IO_WRFILE(IUNIT,IBLK,R4BUF,NWD,STATUS)
           IF (STATUS.NE.0) GOTO 3
         ENDIF
C
C
C  If internal data type ISWD<0, write non-blank data points only
C
       ELSEIF (ISWD.LT.0 .AND. IWMAX.EQ.1) THEN
C
         I2=0
         I4=0
         IZ=0
         IPMAP=0
         IBLK=MPBLK1
         NBLK=(MAXBUF*4)/MPBLK
C
         DO 2 IY=1,IYMAX
         DO 2 IX=1,IXMAX
C
           IZ=IZ+1
           IF (DATA(IZ).NE.BLANK) THEN
             I2BUF(I2+1)=IX
             I2BUF(I2+2)=IY
             IF (ISWD.EQ.-1) THEN
c              I2BUF(I2+3) ININT(DATA(IZ)/SCALEF+ZEROL)
               I2BUF(I2+3)=NINT(DATA(IZ)/SCALEF+ZEROL)
               I2=I2+3
               I4=(I2+1)/2
               IF (I4.EQ.MPBLK/4 .AND. MOD(I2,2).EQ.1) I2=I2+1
             ELSEIF (ISWD.EQ.-2) THEN
c              I4BUF(I4+2)=ININT(DATA(IZ)/SCALEF+ZEROL)
               I4BUF(I4+2)=NINT(DATA(IZ)/SCALEF+ZEROL)
               I2=I2+4
               I4=I4+2
             ELSEIF (ISWD.EQ.-3) THEN
               R4BUF(I4+2)=DATA(IZ)/SCALEF+ZEROL
               I2=I2+4
               I4=I4+2
             ENDIF
             IPMAP=IPMAP+1
           ENDIF
C
           IF (I4.EQ.MAXBUF) THEN
             NWD=(MAXBUF*4)/LWD
             CALL IO_WRFILE(IUNIT,IBLK,R4BUF,NWD,STATUS)
             IF (STATUS.NE.0) GOTO 3
             IBLK=IBLK+NBLK
             I4=0
             I2=0
           ENDIF
C
    2    CONTINUE
C
C    Write incomplete buffer
C
         IF (I4.GT.0) THEN
           MWD=MPBLK/LWD
           NWD=((I4-1)/MWD+1)*MWD
           CALL IO_WRFILE(IUNIT,IBLK,R4BUF,NWD,STATUS)
           IF (STATUS.NE.0) GOTO 3
         ENDIF
C
C    Update map page count in redtape
C
         NPAGES=(IBLK*MPBLK+I4*4-1)/2048+1
         NPTOT=NPAGES
         NPMAP=NPAGES
         NWD=PAGE
         CALL IO_WRFILE(IUNIT,1,IRTALL,NWD,STATUS)
C
       ELSE
         STATUS=ILL_DATYPE
       ENDIF
C
C
    3  IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine WRMAP')
C
       END
