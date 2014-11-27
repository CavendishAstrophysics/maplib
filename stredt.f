
*+STREDT

       SUBROUTINE STREDT (IUV, ICODE, STATUS)
C      --------------------------------------
C
C  Sets up 'computing' redtape.
C
C  Parameters:
C      IUV       integer(4)  new U,V range
C      ICODE     integer     data type (1,2,3,4 for I*2,I*4,R*4,C*8)
C      STATUS    integer     status value
C
C  Sets up sections 0 and 2 of the map redtape (the computing redtape)
C  given a new U,V range in the array IUV (IU1,IU2,IV1,IV2), and an
C  internal data type specified by ICODE.  If ICODE is given equal to
C  zero, the current internal data type is used.  The exact position of
C  the map centre point in redtape section 4 is also updated.
C
C  This routine is provided to update an existing redtape, and astronomical
C  parameters and other redtape sections are not changed.
C
C  The STATUS value should be zero on entry.
C
C  (DJT, 4 December 89)
C  (DJT, 11 March 92; Unix implementation)
C
*-
       INTEGER    IUV(4), ICODE, STATUS
       INTEGER    NX, NY, MODE, TERMNO
       INTEGER*4  NBYTES, NPAGES
       CHARACTER  USER*16, STRING*12, SIZEX*4, SIZEY*4
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
C  Check input parameters
C
       IF (ICODE.LT.-3 .OR. ICODE.GT.4) THEN
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
           USER=RTUSER
         ENDIF
C
C    Redtape section 0 : map saved flag and current user
C
         RTSFLG=0
         RTHDR='MAP'
         IF (ICODE.EQ.4) RTHDR='APERTURE'
         RTOWNR=USER
         RTUSER=USER
         RTSVOL=' '
C
C    Redtape section 1 : title
C
         NX=IUV(2)-IUV(1)+1
         NY=IUV(3)-IUV(4)+1
         WRITE(SIZEX,'(I4)')NX
         WRITE(SIZEY,'(I4)')NY
c        STRING=SIZEX(-1:)//' x '//SIZEY(-1:)
         STRING=SIZEX(:)//' x '//SIZEY(:)
         CALL ADREDT('Map size',STRING,STATUS)
C
C    Redtape section 4 : exact map centre coordinates
C
         XMC=XMC+(IUMAP1-IUV(1))
         YMC=YMC+(IUV(3)-IVMAP1)
C
C    Redtape section 2 : computing redtape
C
         IF (IABS(ICODE).GE.1 .AND. ICODE.LE.4) ISWD=ICODE
         IUMAP1=IUV(1)
         IUMAP2=IUV(2)
         IVMAP1=IUV(3)
         IVMAP2=IUV(4)
         IXMAX=IUMAP2-IUMAP1+1
         IYMAX=IVMAP1-IVMAP2+1
c        IF (ISWD.LT.1) THEN
c          MPBLK=2048
c        ELSE
c          MPBLK=2*IWMAX*IXMAX
c          IF (IABS(ISWD).GT.1) MPBLK=2*MPBLK
c          IF (IABS(ISWD).GT.3) MPBLK=2*MPBLK
c          IF (MOD(MPBLK,4).EQ.2) MPBLK=MPBLK+2
c        ENDIF
         MPBLK=2048
         MPBLK1=(2048*(IWMAX*IPCOMP+IPEXRT)-1)/MPBLK+2
         IF (IABS(ISWD).EQ.1) IBLNK2=-32768
         IF (IABS(ISWD).GT.1) IBLNK4=-2147483647 - 1
         BLANK=-1.E30
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
       IF (STATUS.NE.0) CALL MAPERR(STATUS, 'in routine STREDT')
C
       END
