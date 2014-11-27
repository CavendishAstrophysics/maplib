

*$(8)  Pip routines for annotation and display

*+PIPSA

       SUBROUTINE PIPSA (IUV, RAC, DECC, RA_RANGE, DEC_RANGE, IPRINT,
     :                                                        STATUS)
C      --------------------------------------------------------------
C
C  Calculates RA, Dec at the corners of a map area.
C
C  Given:
C      IUV       integer(4)     U,V range
C      IPRINT    integer        report flag
C
C  Returned:
C      RAC       real*8(4)      RA (radians)
C      DECC      real*8(4)      declination (radians)
C      RA_RANGE  real*8         range in RA (radians)
C      DEC_RANGE real*8         range in Dec (radians)
C      STATUS    integer        status value
C
C  Calculates RA and Dec at reference date, at the four corners of the
C  map area defined by the U,V range IUV.  The coordinates are returned
C  in radians as elements of arrays RAC, DECC :
C
C        1 ------ 2
C         :      :
C         :      :
C        3 ------ 4
C
C  Estimates of the maximum range in RA and Declination along the
C  boundaries of the area are also returned.  This information may
C  be used later to set up RA,Dec pips when the area is displayed.
C
C  If IPRINT is non-zero, the coordinates of the four corners are
C  printed out.  The STATUS value should be zero on entry.  If the
C  map is found to extend to unreal points, the returned status
C  value will be UV_UNREAL.
C
C  (EMW, October 87)
C  (DJT, March 88)
C-

       INTEGER    IUV(4), IPRINT, STATUS
       REAL*8     RAC(4), DECC(4), RA_RANGE, DEC_RANGE
C
       include '/mrao/include/constants.inc'
       include '/mrao/include/maplib_errors.inc'
       include '/mrao/include/maplib_redtape.inc'
C
       INTEGER    NSTEP
       PARAMETER (NSTEP=20)
C
       CHARACTER  CHRA*16, CHDEC*16
       INTEGER    IU(5), IV(5), PRSEC, LR, LD, I, J
       REAL*8     DX0, DY0, U, V, U1, V1, USTEP, VSTEP
       REAL*8     DRA, RA, DEC, RA1, RACP, DECCP, YEARS
       REAL*8     RA_MIN, RA_MAX, DEC_MIN, DEC_MAX
       REAL*8     RA_SIDE, DEC_SIDE
C
       IF (STATUS.NE.0) RETURN
C
       IU(1)=IUV(1)
       IU(2)=IUV(2)
       IU(3)=IUV(1)
       IU(4)=IUV(2)
       IV(1)=IUV(3)
       IV(2)=IUV(3)
       IV(3)=IUV(4)
       IV(4)=IUV(4)
       IU(5)=IU(1)
       IV(5)=IV(1)
C
       RA_RANGE=0.D0
       DEC_RANGE=0.D0
       YEARS=EPOCH-REFDAT
       IF (REFDAT.EQ.EPOCH) YEARS=EPOCH-1950.D0
C
C  Allow for fractional map centre
C
       DX0=XMC+IUMAP1-1
       DY0=YMC-IVMAP1-1
C
C  Scan RA, Dec at reference date along each boundary side
C
       DO I=1,4
         U1=DFLOAT(IU(I))-DX0
         V1=DFLOAT(IV(I))+DY0
         USTEP=DFLOAT(IU(I+1)-IU(I))/NSTEP
         VSTEP=DFLOAT(IV(I+1)-IV(I))/NSTEP
C
         DO J=1,NSTEP
           U=U1+(J-1)*USTEP
           V=V1+(J-1)*VSTEP
           CALL UVTORD(U,V,RACP,DECCP,STATUS)
           IF (STATUS.NE.0) GOTO 10
           CALL PRECES(RACP,DECCP,RA,DEC,-YEARS)
C
C    Store RA, Dec for starting corner
C
           IF (J.EQ.1) THEN
             DRA=0.D0
             RA_MIN=RA
             RA_MAX=RA
             RAC(I)=RA
             DEC_MIN=DEC
             DEC_MAX=DEC
             DECC(I)=DEC
           ELSE
             IF (DRA*(RA1-RA).LT.0.D0) RA=RA-DSIGN(CONST_2PI,DRA)
             IF (RA.LT.RA_MIN) RA_MIN=RA
             IF (RA.GT.RA_MAX) RA_MAX=RA
             IF (DEC.LT.DEC_MIN) DEC_MIN=DEC
             IF (DEC.GT.DEC_MAX) DEC_MAX=DEC
             DRA=RA1-RA
           ENDIF
C
           RA1=RA
C
         ENDDO
C
         RA_SIDE=DABS(RA_MAX-RA_MIN)
         DEC_SIDE=DABS(DEC_MAX-DEC_MIN)
         IF (RA_SIDE.GT.RA_RANGE) RA_RANGE=RA_SIDE
         IF (DEC_SIDE.GT.DEC_RANGE) DEC_RANGE=DEC_SIDE
C
       ENDDO
C
C  Report corner coordinates if required
C
       IF (IPRINT.GT.0) THEN
         PRSEC=2
         WRITE(*,*)
         WRITE(*,'(2(5X,A),12X,A,F6.1,A/)')
     :                             'U','V','RA & Dec (',1950.0D+0,')'
         DO I=1,4
           CALL CHR_CHDTOS(RAC(I)/CONST_H2R,PRSEC,CHRA,LR)
           CALL CHR_CHDTOS(DECC(I)/CONST_D2R,PRSEC,CHDEC,LD)
           WRITE(*,'(2I6,6X,A,1X,A)') IU(I),IV(I),CHRA(1:LR),CHDEC(1:LD)
         ENDDO
         WRITE(*,*)
       ENDIF
C
   10  IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine PIPSA')
C
       END
