


*+PIPSB

       SUBROUTINE PIPSB (IUV, MAXIN, RA_RANGE, DEC_RANGE,
     :                               PIPINR, PIPIND, IPRINT, STATUS)
C      -------------------------------------------------------------
C
C  Calculates pip intervals in RA and Dec.
C
C  Given:
C      IUV       integer(4)     U,V range
C      MAXIN     integer        maximum intervals per side
C      RA_RANGE  real*8         range in RA (radians)
C      DEC_RANGE real*8         range in Dec (radians)
C      IPRINT    integer        report flag
C
C  Returned:
C      PIPINR    real*4         RA pip interval (secs)
C      PIPIND    real*4         Dec pip interval (arcsecs)
C      STATUS    integer        status value
C
C  This routine calculates useful RA and Dec intervals for display of
C  pips around the boundary of the given map area, given the range in
C  RA and Dec along the boundaries of the area.  The pip intervals
C  are chosen so that the distance between pips on all boundary sides
C  will be roughly the same, and there will be a maximum of MAXIN pip
C  intervals on any one side.
C
C  The STATUS value should be zero on entry, and is unchanged.
C
C  (EMW, October 87)
C  (DJT, April 88)
C
C-
       INTEGER    IUV(4), MAXIN, IPRINT, STATUS
       REAL*8     RA_RANGE, DEC_RANGE
       REAL*4     PIPINR, PIPIND
C
       include '/mrao/include/constants.inc'
C
       CHARACTER  UNITR*4, UNITD*4
       REAL*4     FACR, FACD, SIZE, XSIZE, YSIZE
       REAL*8     RA, DEC
       INTEGER    I
C
       INTEGER    NTAB
       PARAMETER (NTAB=25)
       REAL*4     PIPTAB(NTAB)
       DATA       PIPTAB /
     :     0.1, 0.25, 0.5, 1.0, 2.0, 3.0, 5.0, 6.0, 10.0, 15.0,
     :     30.0, 60.0, 120.0, 180.0, 300.0, 360.0, 600.0, 900.0,
     :     1800.0, 3600.0, 7200.0, 10800.0, 14400.0, 21600.0, 36000.0 /
C
       IF (STATUS.NE.0) RETURN
C
       XSIZE = IUV(2)-IUV(1)
       YSIZE = IUV(3)-IUV(4)
       SIZE = MAX(XSIZE,YSIZE)
       XSIZE = XSIZE/SIZE
       YSIZE = YSIZE/SIZE
C
       RA = RA_RANGE/CONST_ST2R/XSIZE/MAXIN
       DEC = DEC_RANGE/CONST_SA2R/YSIZE/MAXIN
C
       PIPINR = PIPTAB(NTAB)
       PIPIND = PIPTAB(NTAB)
C
       DO I=NTAB,1,-1
         IF (PIPTAB(I).GE.RA)PIPINR=PIPTAB(I)
         IF (PIPTAB(I).GE.DEC)PIPIND=PIPTAB(I)
       ENDDO
C
C  Report the pip intervals if required
C
       IF (IPRINT.GT.0) THEN
         FACR=1.0
         FACD=1.0
         UNITR='secs'
         UNITD='secs'
         IF (PIPINR.GT.500.) THEN
           FACR=60.
           UNITR='mins'
         ENDIF
         IF (PIPIND.GT.500.) THEN
           FACD=60.0
           UNITD='mins'
         ENDIF
         WRITE (*,1) PIPINR/FACR,UNITR,PIPIND/FACD,UNITD
    1    FORMAT (' Pip intervals    RA:',F7.2,X,A,4X,'Dec:',F7.2,
     :                                                      ' arc',A/)
       ENDIF
C
      END
