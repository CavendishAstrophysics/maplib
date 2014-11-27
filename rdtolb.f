



*+RDTOLB

       SUBROUTINE RDTOLB (RA, DEC, LRAD, BRAD)
C      ---------------------------------------
C
C  Converts RA and Dec to galactic coordinates L,B.
C
C  Given:
C      RA        real*8      right ascension (radians)
C      DEC       real*8      declination (radians)
C
C  Returned:
C      LRAD      real*8      galactic longitude
C      BRAD      real*8      galactic latitude
C
C  (DAG)
C
*-
       REAL*8     RA,DEC,LRAD,BRAD
       REAL*8     X,Y,SINB
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
       SINB=DCOS(DEC)*DCOS(CON27)*DCOS(RA-CON192)+
     :      DSIN(DEC)*DSIN(CON27)
       Y=DSIN(DEC)-SINB*DSIN(CON27)
       X=DCOS(DEC)*DSIN(RA-CON192)*DCOS(CON27)
       LRAD=DATAN2(Y,X)
       LRAD=LRAD+CON33
       BRAD=DASIN(SINB)
       LRAD=MOD(LRAD+TWOPI,TWOPI)
C
       END
