




*+LBTORD

       SUBROUTINE LBTORD (LRAD, BRAD, RA, DEC)
C      ---------------------------------------
C
C  Converts galactic coordinates L,B to RA and Dec.
C
C  Given:
C      LRAD      real*8      galactic longitude (radians)
C      BRAD      real*8      galactic latitude (radians)
C
C  Returned:
C      RA        real*8      right ascension (radians)
C      DEC       real*8      declination (radians)
C
C  (DAG)
C
*-
       REAL*8     RA,DEC,LRAD,BRAD
       REAL*8     X,Y,SINA
       REAL*8     PI,TWOPI
       REAL*8     CON27,CON33,CON192
C
       PARAMETER (PI=3.1415926535898D0)
       PARAMETER (TWOPI=2.D0*PI)
C
       PARAMETER (CON27=27.40D0*PI/180.D0)
       PARAMETER (CON33=33.00D0*PI/180.D0)
       PARAMETER (CON192=192.25D0*PI/180.D0)
C
       SINA=DSIN(BRAD)*DSIN(CON27)+
     :      DCOS(BRAD)*DCOS(CON27)*DSIN(LRAD-CON33)
       X=DCOS(BRAD)*DCOS(LRAD-CON33)
       Y=DSIN(BRAD)*DCOS(CON27)-
     :   DCOS(BRAD)*DSIN(CON27)*DSIN(LRAD-CON33)
       RA=DATAN2(X,Y)
       RA=RA+CON192
       DEC=DASIN(SINA)
       RA=MOD(RA+TWOPI,TWOPI)
C
       END
