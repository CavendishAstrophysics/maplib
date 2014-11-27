*+WRMAPC

       SUBROUTINE WRMAPC (IUNIT, IXY, DATA, NDATA, STATUS)
C      ---------------------------------------------------
C
C  Writes map component data to an indexed map file.
C
C  Given:
C      IUNIT     integer        logical unit number
C      IXY       integer*2(2,*) array containing map coordinates
C      DATA      real*4(*)      map data
C      NDATA     integer        number of map components
C
C  Returned:
C      STATUS    integer      status value
C
C  Routine to write map component data to the indexed map data file
C  opened on unit IUNIT.  The map data file must have redtape set up
C  for indexed data storage (internal data type <0) by routine NWREDT
C  or STREDT.
C
C  The STATUS value should be zero on entry:  possible error conditions
C  on exit are:
C
C      - invalid map redtape (ILL_REDTAPE)
C      - illegal internal data type (ILL_DATYPE)
C      - unexpected IO_SYSTEM I/O error code
C
C  (DJT, 13 December 89)
C  (DJT, 6 April 92; Unix implementation)
C
*-
       INTEGER    IUNIT, NDATA, STATUS
       REAL*4     DATA(NDATA)
       INTEGER*2  IXY(2,NDATA)
       INTEGER    NBLK, NWD, MWD
       INTEGER    I2, I4, IBLK, IZ
       INTEGER*4  NPAGES
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
       IF (STATUS.NE.0) GOTO 2
C
C  Internal data type must be <0
C
       IF (ISWD.LT.0 .AND. IWMAX.EQ.1) THEN
C
         I2=0
         I4=0
         IBLK=MPBLK1
         NBLK=(MAXBUF*4)/MPBLK
C
         DO 1 IZ=1,NDATA
C
           I2BUF(I2+1)=IXY(1,IZ)
           I2BUF(I2+2)=IXY(2,IZ)
           IF (ISWD.EQ.-1) THEN
             I2BUF(I2+3)=NINT(DATA(IZ)/SCALEF+ZEROL)
             I2=I2+3
             I4=(I2+1)/2
             IF (I4.EQ.MPBLK/4 .AND. MOD(I2,2).EQ.1) I2=I2+1
           ELSEIF (ISWD.EQ.-2) THEN
             I4BUF(I4+2)=NINT(DATA(IZ)/SCALEF+ZEROL)
             I2=I2+4
             I4=I4+2
           ELSEIF (ISWD.EQ.-3) THEN
             R4BUF(I4+2)=DATA(IZ)/SCALEF+ZEROL
             I2=I2+4
             I4=I4+2
           ENDIF
C
           IF (I4.EQ.MAXBUF) THEN
             NWD=(MAXBUF*4)/LWD
             CALL IO_WRFILE(IUNIT,IBLK,R4BUF,NWD,STATUS)
             IF (STATUS.NE.0) GOTO 2
             IBLK=IBLK+NBLK
             I4=0
             I2=0
           ENDIF
C
    1    CONTINUE
C
C    Write incomplete buffer
C
         IF (I4.GT.0) THEN
           MWD=MPBLK/LWD
           NWD=((I4-1)/MWD+1)*MWD
           CALL IO_WRFILE(IUNIT,IBLK,R4BUF,NWD,STATUS)
           IF (STATUS.NE.0) GOTO 2
         ENDIF
C
C    Update map page count in redtape
C
         NPAGES=((IBLK-1)*MPBLK+I4*4-1)/2048+1
         NPTOT=NPAGES
         NPMAP=NPAGES
         IPMAP=NDATA
         NWD=PAGE
         CALL IO_WRFILE(IUNIT,1,IRTALL,NWD,STATUS)
C
       ELSE
         STATUS=ILL_DATYPE
       ENDIF
C
C
    2  IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine WRMAPC')
C
       END
