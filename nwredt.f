


*+NWREDT

       SUBROUTINE NWREDT (NX, NY, ICODE, ITELES, STATUS)
C      -------------------------------------------------
C
C  Initialises new map redtape.
C
C  Given:
C      NX        integer     row length
C      NY        integer     number of rows
C      ICODE     integer     data type (1,2,3,4 for I*2,I*4,R*4,C*8)
C      ITELES    integer     telescope identifier
C      STATUS    integer     status value
C
C  Initialises the common blocks for MRAO-ND style map redtape,
C  for a map of size NX by NY pixels, and internal data type ICODE.
C  Note that this routine is designed to provide an initial template for
C  the redtape of a new map, and will overwrite the current contents of
C  the redtape common blocks.  Astronomical redtape parameters will be
C  set to zero.  The scale factor and zero level will be set to standard
C  values of 1.0 and 0.0.
C
C  If ICODE is negative (-1,-2,-3) the map data will be stored in
C  indexed format, i.e. map data values and coordinates are stored
C  only for those data points whose value is non-blank.
C
C  If ICODE=4, the redtape will be initialised for the corresponding
C  complex aperture data.
C
C  The STATUS value should be zero on entry.
C
C  (DJT, 1 April 92; Unix implementation)
C
*-
       INTEGER  NW, NX, NY, ICODE, ITELES, STATUS
       INTEGER  ITIME(7), LENGTH, MODE, TERMNO, I, LX, LY
       CHARACTER  USER*16, DATE*9, TIME*8, SIZEX*4, SIZEY*4
       INTEGER*4  NBYTES, NPAGES
C
       INTEGER  LSECT(8)
       DATA LSECT/50,200,20,50,40,20,80,52/
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
C  Single valued maps only supported in Unix implementation
C
       NW=1
C
C  Check input parameters
C
       IF (NW.NE.1) THEN
         STATUS=ILL_REDTAPE
       ELSEIF (ICODE.LT.-3 .OR. ICODE.GT.4) THEN
         STATUS=ILL_DATYPE
       ENDIF
C
       IF (STATUS.EQ.0) THEN
C
C    Find current user name
C
         CALL IO_ENQEXE(USER,MODE,TERMNO)
         IF (USER.EQ.'BATCH-USER' .OR.
     :       USER.EQ.'SYSTEM' .OR. USER.EQ.'RT') THEN
           IF (ITELES.EQ.1) USER='T151'
           IF (ITELES.EQ.2) USER='38MHZ'
           IF (ITELES.EQ.4) USER='5KM'
         ENDIF
C
C    Zero all redtape initially
C
         DO I=1,512
           IRTALL(I)=0
         ENDDO
C
C    Redtape section 0 : redtape format
C
         MAPTYP=1
         RTHDR='MAP'
         IF (ICODE.EQ.4) RTHDR='APERTURE'
         RTOWNR=USER
         RTUSER=USER
         RTELES=ITELES
         RTSVOL=' '
         ICMAX=1
         ICMAP=1
         IWMAX=NW
         IWMAP=1
         ISTYND=1
         IPCOMP=1
         ISTYRT=0
         IPEXRT=0
         ILEXRT=0
C      DO I=1,8
C        IRT0(2*I+15)=LSECT(I)
C      ENDDO
C
C    Redtape section 1 : map title
C
         DO I=1,20
           RTITLE(I)=' '
         ENDDO
         RTITLE(1) ='Source title : '
         RTITLE(2) ='Instrument   : '
         RTITLE(3) ='Frequency    : '
         RTITLE(4) ='Polarisation : '
         RTITLE(5) ='Spacings     : '
         RTITLE(6) ='Map size     : '
         RTITLE(7) ='Projection   : '
         RTITLE(8) ='Observation date  : '
         RTITLE(9) ='1950.0 obs centre : '
         RTITLE(10)='1950.0 map centre : '
         RTITLE(11)='Map created'
C
         CALL CHR_CHITOC(NX,SIZEX,LX)
         CALL CHR_CHITOC(NY,SIZEY,LY)
         RTITLE(6)(16:)=SIZEX(1:LX)//' x '//SIZEY(1:LY)
C
         CALL UTIL_ENQTIM(ITIME(2))
         CALL UTIL_ENQDAT(ITIME(5))
         CALL CHR_CHTIME(ITIME(2),TIME,LENGTH)
         CALL CHR_CHDATE(ITIME(5),0,DATE,LENGTH)
         RTITLE(11)(15:)=TIME(1:5)//'  '//DATE
C
C    Redtape section 2 : computing redtape
C
         ISWD=ICODE
         IUMAP1=-NX/2
         IUMAP2=IUMAP1+NX-1
         IF (ISWD.EQ.4) IUMAP2=0
         IVMAP1=NY/2
         IVMAP2=IVMAP1-NY+1
         IXMAX=IUMAP2-IUMAP1+1
         IYMAX=IVMAP1-IVMAP2+1
         MPBLK=2048
         MPBLK1=(2048*(IWMAX*IPCOMP+IPEXRT)-1)/MPBLK+2
         IF (IABS(ISWD).EQ.1) IBLNK2=-32768
         IF (IABS(ISWD).EQ.2) IBLNK4=-2147483647 - 1
         BLANK=-1.E30
         BSETID=' '
C
C    Redtape section 3 : astronomical redtape
C
         NAME=' '
         POLN=' '
         EXUNIT=' '
         ZEROL=0.0
         SCALEF=1.0
         IPOLN=0
         IPROJ=0
C
C    Redtape section 6 : astrometric redtape
C
         XMC=1-IUMAP1
         YMC=IVMAP1+1
C
C    Redtape section 7 : observation date list
C
C    Redtape section 8 : processing history
C
         DO I=1,15
           HISTRY(I)=' '
         ENDDO
C
C    Redtape section 9 : logical sample file details
C
         NUMLSF=0
         MAXLSF=10
C
C    Redtape section 0 : file size in pages
C
         IF (ISWD.LT.1) THEN
           NPAGES=0
         ELSE
           NBYTES=IXMAX*IYMAX*IWMAX
           IF (ISWD.EQ.1) NBYTES=2*NBYTES
           IF (ISWD.EQ.2) NBYTES=4*NBYTES
           IF (ISWD.EQ.3) NBYTES=4*NBYTES
           IF (ISWD.EQ.4) NBYTES=8*NBYTES
           NBYTES=(MPBLK1-1)*MPBLK+NBYTES
           NPAGES=(NBYTES-1)/2048+1
         ENDIF
         NPTOT=NPAGES
         NPMAP=NPAGES
         IPMAP=NPAGES
C
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,' in routine NWREDT')
C
       END
