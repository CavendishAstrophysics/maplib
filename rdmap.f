*+RDMAP

       SUBROUTINE RDMAP (IUNIT, DATA, STATUS)
C      --------------------------------------
C
C  Reads map data from a map file.
C
C  Given:
C      IUNIT     integer     logical unit number
C
C  Returned:
C      DATA      real*4()    map data
C      STATUS    integer     status value
C
C  Reads the data from the map opened on unit IUNIT, and stores it by
C  rows in the array DATA, in external units.  The array may be declared
C  2-d or 3-d as appropriate in the calling routine, depending on the
C  multiplicity of the map (data for each component of a multiple map is
C  stored consecutively for each U-V point).  Parameter values currently
C  set up in the redtape common blocks identify the internal data type,
C  the size of the map and also the conversion from internal to external
C  units.  Note that for aperture data the output array should be set
C  up to accept complex data values.
C
C  The STATUS value should be zero on entry:  the returned status value
C  is zero if successful.  Other possible values are:
C
C      - invalid map redtape (ILL_REDTAPE)
C      - unexpected IO_SYSTEM I/O error code
C
C  (DJT, 6 December 89)
C  (DJT, 1 April 92; Unix implementation)
C
*-
       INTEGER    IUNIT, IW, STATUS
       INTEGER    IBLK, MBLK, NBLK, NWD
       INTEGER    I2, I4, IP, IW1, IX, IY, IZ
       INTEGER    NW, NX, NZ
       REAL*4     DATA(*)
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_common.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
C  Only single valued maps supported in Unix implementation
C
       IW=1
C
C  Check map redtape and component number
C
       CALL CHREDT(STATUS)
       IF (STATUS.EQ.0 .AND. IW.GT.IWMAX) STATUS=ILL_COMPNO
       IF (STATUS.NE.0) GOTO 4
C
       IW1=IW
       NW=IWMAX
       NX=IXMAX
       IF (IW.LE.0) THEN
         NX=IXMAX*IWMAX
         NW=1
         IW1=1
       ENDIF
       IF (ISWD.EQ.4) NX = 2*NX
C
C
C  If internal data type ISWD>0, read the complete map
C
       IF (ISWD.GT.0) THEN
C
C    Read data using the available buffer
C
         IZ=0
         I2=IW1
         I4=IW1
         NZ=NX*IYMAX
         NBLK=(MAXBUF*4)/MPBLK
         MBLK=(NPTOT*2048)/MPBLK
         DO 1 IBLK=MPBLK1,MBLK,NBLK
           NWD=MIN0(NBLK,MBLK-IBLK+1)*(MPBLK/LWD)
           CALL IO_RDFILE(IUNIT,IBLK,R4BUF,NWD,STATUS)
           IF (STATUS.NE.0) GOTO 4
C
           DO WHILE (I2.LE.NWD*2 .AND. IZ.LT.NZ)
C
             IZ=IZ+1
C
C      Internal data type integer*2
C
             IF (ISWD.EQ.1) THEN
               IF (I2BUF(I2).EQ.IBLNK2) THEN
                 DATA(IZ)=BLANK
               ELSE
                 DATA(IZ)=(I2BUF(I2)-ZEROL)*SCALEF
               ENDIF
               I2=I2+NW
               I4=I2/2
C
C      Internal data type integer*4
C
             ELSEIF (ISWD.EQ.2) THEN
               IF (I4BUF(I4).EQ.IBLNK4) THEN
                 DATA(IZ)=BLANK
               ELSE
                 DATA(IZ)=(I4BUF(I4)-ZEROL)*SCALEF
               ENDIF
               I4=I4+NW
               I2=I4*2
C
C      Internal data type real*4, complex*8
C
             ELSEIF (ISWD.GE.3) THEN
               IF (R4BUF(I4).EQ.BLANK) THEN
                 DATA(IZ)=BLANK
               ELSE
                 DATA(IZ)=(R4BUF(I4)-ZEROL)*SCALEF
               ENDIF
               I4=I4+NW
               I2=I4*2
             ENDIF
           ENDDO
C
           I2=I2-NWD*2
           I4=I4-NWD
C
    1    CONTINUE
C
C
C  If internal data type ISWD<0, read indexed non-blank data points
C
       ELSEIF (ISWD.LT.0 .AND. IW1.EQ.1) THEN
C
C    Initialise map to blanks
C
         NZ=IXMAX*IYMAX
         DO 2 IZ=1,NZ
           DATA(IZ)=BLANK
    2    CONTINUE
C
         IP=0
         NBLK=(MAXBUF*4)/MPBLK
         MBLK=(NPTOT*2048)/MPBLK
         DO 3 IBLK=MPBLK1,MBLK,NBLK
           NWD=MIN0(NBLK,MBLK-IBLK+1)*(MPBLK/LWD)
           CALL IO_RDFILE(IUNIT,IBLK,R4BUF,NWD,STATUS)
           IF (STATUS.NE.0) GOTO 4
C
           I2=0
           I4=0
           DO WHILE (I4.LT.MAXBUF .AND. IP.LT.IPMAP)
C
             IP=IP+1
             IX=I2BUF(I2+1)
             IY=I2BUF(I2+2)
             IZ=(IY-1)*IXMAX+IX
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
    3    CONTINUE
C
       ELSE
         STATUS=ILL_DATYPE
       ENDIF
C
C
    4  IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine RDMAP')
C
       END


