*+RDREDT

       SUBROUTINE RDREDT (IUNIT, IPRINT, STATUS)
C      -----------------------------------------
C
C  Reads redtape from a map file.
C
C  Given:
C      IUNIT     integer     logical unit number
C      IPRINT    integer     print control
C
C  Returned:
C      STATUS    integer     status value
C
C  Reads redtape for the map or aperture currently opened on unit IUNIT
C  into the current redtape common blocks.  Calls routine STPROJ to set
C  up the map projection parameters for use by the coordinate conversion
C  routines RDTOUV and UVTORD.  Any extra redtape pages are also read in
C  to the extra redtape buffer.
C
C  If IPRINT=1, the redtape title will be printed on the output device.
C
C  The STATUS value should be zero on entry:  the returned status value
C  is zero if successful.  Other possible values are:
C
C      - invalid map redtape (ILL_REDTAPE)
C      - unexpected IO_SYSTEM I/O error code
C
C  (DJT, 10 January 90)
C  (DJT, 1 April 92; Unix implementation)
C
*-
       INTEGER  IUNIT, IW, IPRINT, STATUS
       INTEGER  I, IBLK, IPREC, L, NWD
       INTEGER  IOUT
C
       include '/mrao/include/maplib_common.inc'
       include '/mrao/include/maplib_errors.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_extrart.inc'
       include '/mrao/include/chrlib_functions.inc'
C
       CHARACTER  CS2W12*4
       EQUIVALENCE (CS2W12,IRT2(12))
C
       IF (STATUS.NE.0) RETURN
C
       CALL IO_ENQOUT(IOUT)
C
C  Single valued maps only supported in Unix implementation
C
       IW=1
C
C  Read and check first page of redtape
C
       IBLK=0
    1  CONTINUE
       NWD=PAGE
       CALL IO_RDFILE(IUNIT,1,IRTALL,NWD,STATUS)
C
       CALL CHREDT(STATUS)
C
       IF (STATUS.EQ.0) THEN
         IF (IW.LT.1 .OR. IW.GT.IWMAX) THEN
           STATUS=ILL_COMPNO
         ELSEIF (IBLK.EQ.0 .AND. IW.GT.1) THEN
           IBLK=IBLK+(IW-1)*IPCOMP
           GOTO 1
         ENDIF
       ENDIF
C
       IF (STATUS.EQ.0) THEN
C
C  Print redtape
C
         IF (IPRINT.GT.0) THEN
           DO I=1,20
             L=CHR_LENB(RTITLE(I))
             IF (L.GT.0) WRITE(IOUT,*) '  ',RTITLE(I)(1:L)
           ENDDO
           WRITE(IOUT,*)
         ENDIF
C
C  Fix redtape for archaic maps
C
         IF (IPCOMP.EQ.0) IPCOMP=1
         IF (MAPTYP.EQ.0) MAPTYP=1
         IF (IWMAP.EQ.0) IWMAP=IW
         IF (IBLNK2.EQ.0) IBLNK2=-32768
         IF (BLANK.EQ.0.0) BLANK=-1.E30
         IF (CS2W12.EQ.'BSET') BLANK=-1.E30
         IF (EPOCH.EQ.0.D0) EPOCH=OBSDAT
         IF (OBSDAT.EQ.0.D0) OBSDAT=EPOCH
         IF (RAOBS.EQ.0.D0 .AND. DECOBS.EQ.0.D0)
     :      CALL PRECRD(REFDAT,RAMAP,DECMAP,OBSDAT,RAOBS,DECOBS)
         IF (NAME.EQ.' ') NAME='FLUX DENSITY'
C
C  Set up map projection parameters
C
         IPREC=1
         CALL STPROJ(IPROJ,IPREC,USAMP,SKEW,RAMAP,DECMAP,REFDAT,EPOCH,
     :                                                          STATUS)
C
C  Read extra redtape pages, if any, into the extra redtape buffer
C
         IF (IPEXRT.GT.0) THEN
           IF (IPEXRT.LE.MAX_EXTRA) THEN
             NWD=IPEXRT*PAGE
             IBLK=IWMAX*IPCOMP + 1
             CALL IO_RDFILE(IUNIT,IBLK,EXTRART,NWD,STATUS)
           ELSE
             STATUS=ILL_EXREDT
           ENDIF
         ENDIF
C
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine RDREDT')
C
       END
