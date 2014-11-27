


C+PIPSC

       SUBROUTINE PIPSC (IUV, SIDE, FLAG, PIPIN, PDIST, PIPVAL, NPIP,
     :                                                          STATUS)
C      ----------------------------------------------------------------
C
C  Calculates pips information for one side of a map area.
C
C  Given:
C      IUV       integer(4)    U,V range
C      SIDE      integer       boundary side index (1-4)
C      FLAG      integer       find RA/Dec pips (1=RA, 2=Dec)
C      PIPIN     real*4        RA/Dec pip interval (sec/arcsec)
C
C  Returned:
C      PDIST     real*4(*)     distance of pips along side (0.0->1.0)
C      PIPVAL    real*8(*)     pip values (sec/arcsec)
C      NPIP      integer       number of pips
C      STATUS    integer       status value
C
C  The SIDE index numbers and pip distances are defined as follows:
C
C                side 1
C          ------------------
C         :--PDIST->         :
C         :                  :
C   side 4:                  :side 2
C         :                  :
C         :                  :
C         :                  :
C          ------------------
C                side 3
C
C  SIDE = 1,  RA pips,   PDIST measured from:    top LH corner
C         2,  Dec pips,  PDIST - - - - - - -     top RH corner
C         3,  RA pips,   PDIST - - - - - - -  bottom LH corner
C         4,  Dec pips,  PDIST - - - - - - -     top LH corner
C
C  The STATUS value should be zero on entry.  If the map is found
C  to extend to unreal points, the returned status value will be
C  UV_UNREAL.
C
C  (EMW, October 87)
C  (DJT, April 88)
C-
       INTEGER  IUV(4), SIDE, FLAG, NPIP, STATUS
       REAL*4   PDIST(*), PIPIN
       REAL*8   PIPVAL(*)
C
       include '/mrao/include/constants.inc'
       include '/mrao/include/maplib_redtape.inc'
C
       REAL*8   U_RANGE, V_RANGE
       REAL*8   DX0, DY0, U, V, YEARS
       REAL*8   RA, DEC, RAP, DECP, PIP, DSECS, SECS1, SECS2
       INTEGER  IU, IV, IPIP, IPIP1, IPIP2
C
       IF (STATUS.NE.0) RETURN
C
       YEARS=EPOCH-REFDAT
       IF (REFDAT.EQ.EPOCH) YEARS=EPOCH-1950.D0
C
C  Allow for fractional map centre
C
       DX0=XMC+IUMAP1-1
       DY0=YMC-IVMAP1-1
C
C  Set up starting corner coordinates
C
       IU=IUV(1)
       IV=IUV(3)
       IF (SIDE.EQ.2) IU=IUV(2)
       IF (SIDE.EQ.3) IV=IUV(4)
       U_RANGE=IUV(2)-IUV(1)
       V_RANGE=IUV(3)-IUV(4)
C
       U=IU-DX0
       V=IV+DY0
       CALL UVTORD(U,V,RAP,DECP,STATUS)
       IF (STATUS.NE.0) GOTO 9999
       CALL PRECES(RAP,DECP,RA,DEC,-YEARS)
       IF (FLAG.EQ.1) SECS1=RA/CONST_ST2R
       IF (FLAG.EQ.2) SECS1=DEC/CONST_SA2R
       IPIP1=SECS1/PIPIN
C
C  Scan boundary side for pips
C
       NPIP=0
       DSECS=0.D0
       IU = IU+MOD(SIDE,2)
       IV = IV-MOD(SIDE+1,2)
       DO WHILE (IU.LE.IUV(2) .AND. IV.GE.IUV(4))
           U=IU-DX0
           V=IV+DY0
           CALL UVTORD(U,V,RAP,DECP,STATUS)
           IF (STATUS.NE.0) GOTO 9999
           CALL PRECES(RAP,DECP,RA,DEC,-YEARS)
           IF (FLAG.EQ.1) SECS2=RA/CONST_ST2R
           IF (FLAG.EQ.2) SECS2=DEC/CONST_SA2R

C          Correct for discontinuity at 0 hrs RA
           IF (FLAG.EQ.1) THEN
             IF (DSECS*(SECS1-SECS2).LT.0.D0) THEN
               SECS1=SECS1+DSIGN(86400.D0,DSECS)
             ENDIF
             DSECS=SECS1-SECS2
           ENDIF
C
           IPIP2=SECS2/PIPIN
           IF (IPIP1.NE.IPIP2) THEN
C
C  Compute fractional distance of pip from the starting corner
C
             NPIP=NPIP+1
             IF (DABS(SECS1).GT.DABS(SECS2)) THEN
               IPIP=SECS1/PIPIN
             ELSE
               IPIP=SECS2/PIPIN
             ENDIF
             PIP=IPIP*PIPIN
             IF (MOD(SIDE,2).EQ.1) THEN
               PDIST(NPIP)=((IU-IUV(1))-(PIP-SECS2)/
     *                      (SECS1-SECS2))/U_RANGE
             ELSE
               PDIST(NPIP)=((IUV(3)-IV)-(PIP-SECS2)/
     *                      (SECS1-SECS2))/V_RANGE
             ENDIF
             IF (FLAG.EQ.1) THEN
               PIPVAL(NPIP)=DMOD(PIP,86400.0D+0)
             ELSE
               PIPVAL(NPIP)=PIP
             END IF
           END IF
C
           SECS1=SECS2
           IPIP1=IPIP2
           IU = IU+MOD(SIDE,2)
           IV = IV-MOD(SIDE+1,2)
       END DO
C
 9999  IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine PIPSC')
C
       END
