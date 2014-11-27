*+RDAREA

       SUBROUTINE RDAREA (IUNIT, IUV, DATA, STATUS)
C      --------------------------------------------
C
C  Reads a map area from a map file.
C
C  Given:
C      IUNIT     integer     logical unit number
C      IUV       integer(4)  U,V coordinate window
C
C  Returned:
C      DATA      real*4()    map data
C      STATUS    integer     status value
C
C  Reads the map data within a specified U,V window from the map opened
C  on unit IUNIT, and stores it by rows in the array DATA, in external
C  units.  Parameter values currently set up in the redtape common blocks
C  identify the internal data type, map size and also the conversion from
C  internal to external units.  Note that current redtape values are not
C  changed.
C
C  The STATUS value should be zero on entry:  the returned status value
C  is zero if successful.  Other possible values are :
C
C      - invalid map redtape (ILL_REDTAPE)
C      - invalid U,V window (ILL_UVWIND)
C      - U,V window outside map (UV_OUTMAP)
C      - unexpected IO_SYSTEM I/O error code
C
C  (DJT, 24 November 87)
C  (DJT, 1 April 92; Unix implementation)
C
*-
       INTEGER    IUNIT, IW, IUV(4), STATUS
       INTEGER    IX, IX1, IXX, IY, IY1, IY2, IZ
       INTEGER    IBLK, IBLK1, NW, NWD, NX
       INTEGER    I2, I4
       REAL*4     DATA(*)
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_common.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
C  Single valued maps only supported in Unix implementation
C
       IW=1
C
C  Check map redtape
C
       CALL CHREDT(STATUS)
C
C  Check the given U,V window
C
       CALL CHCKUV(IUV,STATUS)
C
C  Check component number
C
       IF (STATUS.EQ.0) THEN
         IF (IW.GT.IWMAX) STATUS=ILL_COMPNO
       ENDIF
C
       IF (STATUS.NE.0) GOTO 2
C
C
       NW=IWMAX
       NX=IUV(2)-IUV(1)+1
       IX1=(IUV(1)-IUMAP1)*IWMAX+IW
       IF (IW.LE.0) THEN
         NW=1
         NX=(IUV(2)-IUV(1)+1)*IWMAX
         IX1=(IUV(1)-IUMAP1)*IWMAX+1
       ENDIF
       IF (ISWD.EQ.4) NX=2*NX
C
C  Read rows using the available buffer
C
       IZ=0
       IBLK1=0
       NWD=MPBLK/LWD
       IY1=IVMAP1-IUV(3)+1
       IY2=IVMAP1-IUV(4)+1
C
       DO 1 IY=IY1,IY2
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
           IF (I2BUF(I2).EQ.IBLNK2) THEN
             DATA(IZ)=BLANK
           ELSE
             DATA(IZ)=(I2BUF(I2)-ZEROL)*SCALEF
           ENDIF
           I2=I2+NW
           I4=I2/2
C
C    Internal data type integer*4
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
C    Internal data type real*4, complex*8
C
         ELSEIF (ISWD.GE.3) THEN
           IF (R4BUF(I4).EQ.BLANK) THEN
             DATA(IZ)=BLANK
           ELSE
             DATA(IZ)=(R4BUF(I4)-ZEROL)*SCALEF
           ENDIF
           I4=I4+NW
           I2=I4*2
         ELSE
C
           STATUS=ILL_DATYPE
           GOTO 2
         ENDIF
C
         IF (I2.GT.NWD*2) THEN
           IBLK=IBLK+1
           I2=I2-NWD*2
           I4=I4-NWD
         ENDIF
C
    1  CONTINUE
C
    2  IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine RDAREA')
C
       END
