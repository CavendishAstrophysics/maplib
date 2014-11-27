


*+SCNMAP

       SUBROUTINE SCNMAP (DATA, IUV, ZMNX, IZMNX, STATUS)
C      --------------------------------------------------
C
C  Scans a map area for data maximum and minimum.
C
C  Given:
C      DATA      real*4()    map data
C      IUV       integer(4)  U,V coordinate window
C
C  Returned:
C      ZMNX      real*4(4)   maximum, minimum data values
C      IZMNX     integer(4)  U,V coordinates of max, min points
C      STATUS    integer     status value
C
C  Scans the map data held by rows in array DATA, within the U,V window
C  specified by the array IUV (IU1,IU2,IV1,IV2) and finds the maximum and
C  minimum data values and their U,V coordinates.  The mean and standard
C  deviation of the data within the specified window are also returned:
C
C      ZMNX(1)  maximum data value
C      ZMNX(2)  minimum data value
C      ZMNX(3)  mean data value
C      ZMNX(4)  standard deviation
C      IZMNX(1,2)  U,V coordinates of data maximum
C      IZMNX(3,4)  U,V coordinates of data minimum
C
C  The redtape common blocks are assumed to be set up appropriately for
C  the input data array, and are not updated.
C
C  For aperture data, the map array should be set up to provide complex
C  data values:  the routine returns the max, min, etc amplitude values.
C
C  The STATUS values should be zero on entry, and is normally unchanged
C  on exit.  Possible error conditions are:
C
C      - invalid u,v window (ILL_UVWIND)
C      - u,v window outside map (UV_OUTMAP)
C
C  (DJT, 20 January 87)
C
*-
       REAL*4     DATA(1), ZMNX(4), RDATA
       INTEGER    IUV(4), IZMNX(4), STATUS
       INTEGER    IU1, IU2, IV1, IV2, IU, IV
       INTEGER*4  I, IX, N, NW
       COMPLEX*8  ZDATA
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
C  Initialise output arrays
C
       DO I=1,4
         IZMNX(I)=0
         ZMNX(I)=0.0
       ENDDO
       ZMNX(1)=-1.E30
       ZMNX(2)=-ZMNX(1)
C
C  Check U,V range
C
       CALL CHCKUV(IUV,STATUS)
       IF (STATUS.EQ.0) THEN
C
         IU1=IUV(1)
         IU2=IUV(2)
         IV1=IUV(3)
         IV2=IUV(4)
C
         NW=1
         IF (ISWD.EQ.4) NW=2
C
C  Scan the U,V window, accumulating max, min, and data sums
C
         N=0
         DO IV=IV1,IV2,-1
           IX=((IVMAP1-IV)*IXMAX+(IU1-IUMAP1))*NW-NW+1
           DO IU=IU1,IU2
             IX=IX+NW
             RDATA=DATA(IX)
             IF (RDATA.NE.BLANK) THEN
               IF (ISWD.EQ.4) THEN
                 ZDATA=CMPLX(DATA(IX),DATA(IX+1))
                 RDATA=CABS(ZDATA)
               ENDIF
               IF (RDATA.GT.ZMNX(1)) THEN
                 ZMNX(1)=RDATA
                 IZMNX(1)=IU
                 IZMNX(2)=IV
               ELSEIF (RDATA.LT.ZMNX(2)) THEN
                 ZMNX(2)=RDATA
                 IZMNX(3)=IU
                 IZMNX(4)=IV
               ENDIF
               ZMNX(3)=ZMNX(3)+RDATA
               ZMNX(4)=ZMNX(4)+RDATA*RDATA
               N=N+1
             ENDIF
           ENDDO
         ENDDO
C
C  Compute mean and standard deviation
C
         IF (N.GT.0) THEN
           ZMNX(3)=ZMNX(3)/FLOAT(N)
           ZMNX(4)=SQRT(ZMNX(4)/FLOAT(N)-ZMNX(3)*ZMNX(3))
         ENDIF
C
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine SCNMAP')
C
       END
