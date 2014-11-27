




*+PRECRD

       SUBROUTINE PRECRD (DATE1, RA, DEC, DATE2, ALPHA, DELTA)
C      -------------------------------------------------------
C
C  Performs precession with respect to a standard epoch.
C
C  Given:
C      DATE1     real*8      initial epoch (years)
C      RA        real*8      right ascension at DATE1 (radians)
C      DEC       real*8      declination at DATE1 (radians)
C      DATE2     real*8      final epoch (years)
C
C  Returned:
C      ALPHA     real*8      right ascension at DATE2 (radians)
C      DELTA     real*8      declination at DATE2 (radians)
C
C  Precession subroutine.  Position (RA,DEC) is precessed from date DATE1
C  to date DATE2. Except for the special case where DATE1=DATE2 (where
C  this routine just copies (RA,DEC) to (ALPHA,DELTA)), one of the two
C  dates must represent the standard epoch B1950.0.
C
C  If the map redtape is valid this routine can always be used to
C  precess from REFDAT to EPOCH and visa versa.
C
C  The method is described in Harris and Large, Proc.A.S.Aust., vol1,
C  no1, pp31-32, January 1967.
C
C  (this is the old 'full precession' PRECES routine from RALIB)
C
C  (DMO, DJT, 21 October 86)
C
*-
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8  DATE1, RA, DEC, DATE2, ALPHA, DELTA
      REAL*8 JDATE
      INTEGER IDATE(3)
      INTEGER*4 IDINT,NUT,K,N,KN
      INTEGER   MODE, NY, IM, ID, MJD, KAT, KBT, KB, I, J
      DIMENSION NUT(69),AN(69),BN(40),ANT(12),BNT(7),CP(9),CN(20),
     1 CA(10),ZP(9),ZN(20),ZA(10),XM(9),YM(9),CF(5),XX(3),AA(3)
C
C  Precession constants
       DATA NUT
     *  /600004,600005,310504,150100,301514,310505,142020,600515,
     *   204000,604515,602515,400514,150010,100510,205000,304004,
     *   405515,302004,310054,302514,350014,304514,140020,600505,
     *   240000,400504,540505,140010,320505,100050,340004,320004,
     *   320555,340504,300555,150000,340515,350505,100500,320504,
     *   320054,340014,320554,144010,304505,140050,300054,302505,
     *   340555,350515,300014,300554,340514,100040,104010,142000,
     *   140100,350504,140500,144000,142505,110004,120514,150004,
     *   122555,102555,140005,144505,160505/
       DATA AN/
     *  -172327.D0,2088.D0,45.D0,10.D0,-4.D0,-3.D0,-2.D0,-12729.D0,
     *   1261.D0,-497.D0,214.D0,124.D0,45.D0,-21.D0,16.D0,-15.D0,
     *  -15.D0,-10.D0,-5.D0,-5.D0,4.D0,3.D0,-3.D0,-2037.D0,
     *   675.D0,-342.D0,-261.D0,-149.D0,114.D0,60.D0,58.D0,-57.D0,
     *  -52.D0,-44.D0,-32.D0,28.D0,26.D0,-26.D0,25.D0,19.D0,
     *   14.D0,-13.D0,-9.D0,-7.D0,7.D0,6.D0,-6.D0,-6.D0,
     *  -6.D0,6.D0,-5.D0,-5.D0,5.D0,-4.D0,-4.D0,4.D0,
     *   4.D0,-4.D0,3.D0,-3.D0,-3.D0,-2.D0,-2.D0,2.D0,
     *  -2.D0,-2.D0,-2.D0,2.D0,-2.0D0/
       DATA BN/
     *   92100.D0,-904.D0,-24.D0,2.D0,2.D0,5522.D0,216.D0,-93.D0,
     *  -66.D0,8.D0,7.D0,5.D0,3.D0,3.D0,-2.D0,-2.D0,
     *   884.D0,183.D0,113.D0,-50.D0,-31.D0,30.D0,22.D0,23.D0,
     *   14.D0,-11.D0,11.D0,-10.D0,-7.D0,7.D0,5.D0,-3.D0,
     *   3.D0,3.D0,3.D0,-2.D0,3.D0,3.D0,-3.D0,2.0D0/
       DATA ANT/
     *  -173.7D0,0.2D0,-1.3D0,-3.1D0,1.2D0,-0.5D0,0.1D0,-0.1D0,
     *   0.1D0,-0.2D0,0.1D0,-0.4D0/
       DATA BNT/
     *   9.1D0,0.4D0,-2.9D0,-0.6D0,0.3D0,-0.5D0,-0.1D0/
       DATA CP/
     *   2304.250D0,1.396D0,0.302D0,0.018D0,0.791D0,
     *   2004.682D0,-0.853D0,-0.426D0,-0.042D0/
       DATA CN/
     * 296.104608D0,13.0649924465D0,0.6890D-11,2.95D-19,358.475833D0,
     * 0.9856002669D0,-0.0112D-11,-0.68D-19,11.250889D0,13.2293504490D0,
     * -0.2407D-11,-0.07D-19,350.737486D0,12.1907491914D0,-0.1076D-11,
     * 0.39D-19,259.183275D0,-0.0529539222D0,0.1557D-11,0.46D-19/
       DATA CA/
     *   23.452294D0,-3.5626D-7,-1.23D-15,1.03D-20,279.696678D0,
     *   0.9856473354D0,0.2267D-12,0.01675104D0,-0.11444D-8,-0.94D-16/
C
       IF(DATE1.EQ.DATE2)THEN
         ALPHA=RA
         DELTA=DEC
         RETURN
       ELSEIF(DATE1.EQ.1950.D0)THEN
         DDATE=DATE2
         MODE=1
       ELSEIF(DATE2.EQ.1950.D0)THEN
         DDATE=DATE1
         MODE=-1
       ELSE
         WRITE(*,*)'*** PRECRD invalid arguments'
       ENDIF
C
      PI=3.141592653589793D0
      TWOPI=2.D0*PI
      RAD=PI/180.D0
      HRAD=PI/12.D0
      SRAD=RAD/3600.D0
      AKK=20.49581D0
      AK=AKK*SRAD
      CON=0.686511715D0
      DK=0.19671D0*SRAD
      DO 1 I=1,9
    1 ZP(I)=CP(I)*SRAD
      DO 2 I=1,20
    2 ZN(I)=CN(I)*RAD
      DO 3 I=1,7
    3 ZA(I)=CA(I)*RAD
C
      IF(IABS(MODE).LE.1)THEN
C  Calculate Julian Date
        CALL FRDDAT(DDATE,IDATE)
        NY=IDATE(3)
        IM=IDATE(2)
        ID=IDATE(1)
        CALL TODDAT(IDATE,RDATE)
        DAY=DDATE-RDATE
        MJD=ID-32+IDINT((DFLOAT(IM)*3057.D0)/100.D0)+NY/4
        IF(IM.LT.3)MJD=MJD+2+(NY-1)/4-NY/4
        JDATE=DFLOAT(MJD)+DFLOAT(NY)*365.D0+1721044.5D0+DAY
      ELSE
        JDATE=DDATE
      ENDIF
C  Date parameters set up
      T0=0.5D0
      TP=(JDATE-2433282.423D0)/36524.21988D0
      D=JDATE-2415020.D0
      TN=D/36525.D0
C
C ****************  PRECESSION SECTION  ********************
      ZETA0=((ZP(4)*TP+ZP(3))*TP+(ZP(2)*T0)+ZP(1))*TP
      THETA=((ZP(9)*TP+ZP(8))*TP+(ZP(7)*T0)+ZP(6))*TP
      Z=ZETA0+(ZP(5)*TP*TP)
      S0=DSIN(ZETA0)
      C0=DCOS(ZETA0)
      ST=DSIN(THETA)
      CT=DCOS(THETA)
      SZ=DSIN(Z)
      CZ=DCOS(Z)
C  Precession matrix formation
      XM(1)= (C0*CT*CZ)-(S0*SZ)
      XM(4)=-(S0*CT*CZ)-(C0*SZ)
      XM(7)=-(ST*CZ)
      XM(2)= (C0*CT*SZ)+(S0*CZ)
      XM(5)=-(S0*CT*SZ)+(C0*CZ)
      XM(8)=-(ST*SZ)
      XM(3)=  C0*ST
      XM(6)=-(S0*ST)
      XM(9)= CT
      IF(MODE.LE.0)CALL PRINVT(XM)
C
C ******************  NUTATION SECTION  ********************
      N=1
      DO 4 I=1,5
      CF(I)=((ZN(N+3)*D+ZN(N+2))*D+ZN(N+1))*D+ZN(N)
    4 N=N+4
      SUMA=0.D0
      SUMB=0.D0
      KAT=0
      KBT=0
      KB=0
C  Nutation codeword evaluation
      DO 10 I=1,69
      K=NUT(I)
      SUM=0.D0
      DO 5 J=1,5
      KN=K/10
      N=K-(KN*10)
      K=KN
      IF(N.GT.0)SUM=SUM+(N-3)*CF(6-J)
    5 CONTINUE
      SA=0.D0
      SB=0.D0
      GOTO (7,6,7,6,7,6),K
    6 KAT=KAT+1
      SA=ANT(KAT)*TN
    7 SUMA=SUMA+((SA+AN(I))*DSIN(DMOD(SUM,TWOPI)))
      GOTO (10,10,9,9,8,8),K
    8 KBT=KBT+1
      SB=BNT(KBT)*TN
    9 KB=KB+1
      SUMB=SUMB+((SB+BN(KB))*DCOS(DMOD(SUM,TWOPI)))
   10 CONTINUE
      SUMA=SUMA*0.0001D0*SRAD
      SUMB=SUMB*0.0001D0*SRAD
      EPS=((ZA(4)*D+ZA(3))*D+ZA(2))*D+ZA(1)
      SE=DSIN(EPS)
      CE=DCOS(EPS)
C  Nutation matrix formation
      YM(1)=1.D0
      YM(5)=1.D0
      YM(9)=1.D0
      YM(2)=0.D0
      YM(4)=-YM(2)
      YM(3)=DSIN(SUMA)*SE
      YM(7)=-YM(3)
      YM(6)=DSIN(SUMB)
      YM(8)=-YM(6)
      IF(MODE.LE.0)CALL PRINVT(YM)
C
C  Convert to rectangular coordinates
      AA(1)=DCOS(DEC)*DCOS(RA)
      AA(2)=DCOS(DEC)*DSIN(RA)
      AA(3)=DSIN(DEC)
      IF(MODE.LE.0)GOTO 13
C
C  Forward precession matrix multiplication
      DO 11 I=1,3
   11 XX(I)=XM(I)*AA(1) + XM(I+3)*AA(2) + XM(I+6)*AA(3)
C  Forward nutation matrix multiplication
      DO 12 I=1,3
   12 AA(I)=YM(I)*XX(1) + YM(I+3)*XX(2) + YM(I+6)*XX(3)
C
C ******************  ABERRATION SECTION *******************
C  Annual aberration
   13 X=AA(1)
      Y=AA(2)
      Z=AA(3)
      RXYSQ=DSQRT(X*X+Y*Y)
      SD=Z
      CD=RXYSQ
      SR=Y/RXYSQ
      CR=X/RXYSQ
      AL=(ZA(7)*D+ZA(6))*D+ZA(5)
      AE=(ZA(10)*D+ZA(9))*D+ZA(8)
      ALB=(1.25D0*DSIN(2.D0*CF(2))*AE+2.D0*DSIN(CF(2)))*AE+AL
      SL=DSIN(ALB)
      CL=DCOS(ALB)
      DD=AK*((CL*CE*SR*SD)-(SL*CR*SD)-(CL*SE*CD))
      DR=-AK*((SL*SR)+(CL*CE*CR))
      DX=-SR*DR - CR*SD*DD
      DY= CR*DR - SR*SD*DD
      DZ= CD*DD
      IF(MODE.LE.0)GO TO 14
C  Forward aberration
      AA(1)=X+DX
      AA(2)=Y+DY
      AA(3)=Z+DZ
      GO TO 17
C  Reverse aberration
   14 AA(1)=X-DX
      AA(2)=Y-DY
      AA(3)=Z-DZ
C
C  Reverse nutation matrix multiplication
      DO 15 I=1,3
   15 XX(I)=YM(I)*AA(1) + YM(I+3)*AA(2) + YM(I+6)*AA(3)
C  Reverse precession matrix multiplication
      DO 16 I=1,3
   16 AA(I)=XM(I)*XX(1) + XM(I+3)*XX(2) + XM(I+6)*XX(3)
C
C  Convert to spherical coordinates
   17 ALPHA=DATAN2(AA(2),AA(1))
      DELTA=DATAN2(AA(3),DSQRT(AA(1)*AA(1)+AA(2)*AA(2)))
      IF(ALPHA.LT.0.D0)ALPHA=ALPHA+2.D0*PI
C
      RETURN
      END
