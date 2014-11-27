


*+RDMAPC

       SUBROUTINE RDMAPC (IUNIT, IXY, DATA, MDATA, NDATA, STATUS)
C      ----------------------------------------------------------
C
C  Reads map component data from an indexed map file.
C
C  Given:
C      IUNIT     integer        logical unit number
C      MDATA     integer        maximum size of component arrays
C
C  Returned:
C      IXY       integer*2(2,*) array containing map coordinates
C      DATA      real*4(*)      array containing map data values
C      NDATA     integer        number of map components read
C      STATUS    integer        status value
C
C  Reads the map component values from the indexed map data file opened
C  on unit IUNIT, returning map coordinates and data values for each
C  component.  The map data file must have been written in indexed form
C  (internal data type<0), by means of routine NWREDT or STREDT, and
C  routine WRMAP.
C
C  The STATUS value should be zero on entry:  the returned status value
C  is zero if successful.  Other possible values are:
C
C      - invalid map redtape (ILL_REDTAPE)
C      - illegal internal data type (ILL_DATYPE)
C      - internal work array too small (ARR_TOOSMALL)
C      - unexpected IO_SYSTEM I/O error code
C
C  (DJT, 13 December 89)
C  (DJT, 11 March 92; Unix implementation)
C
*-
       REAL*4     DATA(*)
       INTEGER*2  IXY(2,*)
       INTEGER    IUNIT, MDATA, NDATA, STATUS
       INTEGER    I2, I4, IBLK, IZ, MBLK, NBLK, NWD
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
C  Internal data type ISWD must be <0
C
       IF (ISWD.LT.0 .AND. IWMAX.EQ.1) THEN
         IF (MDATA.LT.IPMAP) THEN
           STATUS=ARR_TOOSMALL
         ELSE
C
           IZ=0
           NBLK=(MAXBUF*4)/MPBLK
           MBLK=(NPTOT*2048)/MPBLK
           DO 1 IBLK=MPBLK1,MBLK,NBLK
             NWD=MIN0(NBLK,MBLK-IBLK+1)*(MPBLK/LWD)
             CALL IO_RDFILE(IUNIT,IBLK,R4BUF,NWD,STATUS)
             IF (STATUS.NE.0) GOTO 2
C
             I2=0
             I4=0
             DO WHILE (I4.LT.MAXBUF .AND. IZ.LT.IPMAP)
C
               IZ=IZ+1
               IXY(1,IZ)=I2BUF(I2+1)
               IXY(2,IZ)=I2BUF(I2+2)
               IF (ISWD.EQ.-1) THEN
                 DATA(IZ)=(I2BUF(I2+3)-ZEROL)*SCALEF
                 I2=I2+3
                 I4=(I2+1)/2
                 IF (I4.EQ.MPBLK/4 .AND. MOD(I2,2).EQ.1) I2=I2+1
               ELSEIF (ISWD.EQ.-2) THEN
                 DATA(IZ)=(I4BUF(I4+2)-ZEROL)*SCALEF
                 I2=I2+4
                 I4=I4+2
               ELSEIF (ISWD.EQ.-3) THEN
                 DATA(IZ)=(R4BUF(I4+2)-ZEROL)*SCALEF
                 I2=I2+4
                 I4=I4+2
               ENDIF
             ENDDO
C
    1      CONTINUE
C
           NDATA=IZ
C
         ENDIF
C
       ELSE
         STATUS=ILL_DATYPE
       ENDIF
C
C
    2  IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine RDMAPC')
C
       END
