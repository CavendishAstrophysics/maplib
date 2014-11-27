


*+PRREDT

       SUBROUTINE PRREDT (IUNIT, ISECT, STATUS)
C      ----------------------------------------
C
C  Prints map redtape by section.
C
C  Parameters:
C      IUNIT     integer     logical unit number
C      ISECT     integer     redtape section number
C      STATUS    integer     status value
C
C  Prints parameters from the current redtape common blocks on the output
C  unit IUNIT.  The redtape may be printed selectively by section:
C
C    ISECT < 0  print all sections.
C    ISECT >=0  print redtape section ISECT only.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 3 October 86)
C
*-
       INTEGER  IUNIT, ISECT, STATUS
       INTEGER  I, L, CHR_LENB
C
       include '/mrao/include/maplib_redtape.inc'
C
       IF (STATUS.NE.0) RETURN
C
       IF (ISECT.LE.0) THEN
         WRITE(IUNIT,1) (IRT0(I),I=1,33),RTOWNR,RTUSER,RTSVOL
       ENDIF
C
       IF (ISECT.LT.0 .OR. ISECT.EQ.1) THEN
         WRITE(IUNIT,2)
         DO I=1,20
           L=CHR_LENB(RTITLE(I))
           IF (L.GT.0) WRITE(IUNIT,*)'  ',RTITLE(I)(1:L)
         ENDDO
       ENDIF
C
       IF (ISECT.LT.0 .OR. ISECT.EQ.2) THEN
         WRITE(IUNIT,3) (IRT2(I),I=1,10)
         IF (IABS(ISWD).EQ.1) WRITE(IUNIT,9) IBLNK2,BLANK
         IF (IABS(ISWD).GT.1) WRITE(IUNIT,10) IBLNK4,BLANK
       ENDIF
C
       IF (ISECT.LT.0 .OR. ISECT.EQ.3) THEN
         WRITE(IUNIT,4) (IRT3(I),I=1,12),FREQ,ZEROL,SCALEF,
     :     ZMAX,IUZMAX,IVZMAX,ZMIN,IUZMIN,IVZMIN,(IRT3(I),I=22,28)
       ENDIF
C
       IF (ISECT.LT.0 .OR. ISECT.EQ.6) THEN
         WRITE(IUNIT,5) RAMAP,DECMAP,REFDAT,XMC,YMC,SKEW,USAMP,VSAMP,
     :     EPOCH,PRANG,RAOBS,DECOBS,RAPNT,DECPNT,OBSDAT
       ENDIF
C
       IF (ISECT.LT.0 .OR. ISECT.EQ.7) THEN
         WRITE(IUNIT,6) (IRT7(I),I=1,20)
       ENDIF
C
       IF (ISECT.LT.0 .OR. ISECT.EQ.8) THEN
         WRITE(IUNIT,7) NHIST,(HISTRY(I),I=1,NHIST)
       ENDIF
C
       IF (ISECT.LT.0 .OR. ISECT.EQ.7) THEN
         WRITE(IUNIT,8) (IRT7(I),I=1,2)
       ENDIF
C
C
    1  FORMAT('0Redtape section 0'/X,17('-')/X,3A4,3(/X,10I8)/X,A,A,A)
    2  FORMAT('0Redtape section 1'/X,17('-'))
    3  FORMAT('0Redtape section 2'/X,17('-')/(X,10I8))
    4  FORMAT('0Redtape section 3'/X,17('-')/X,12A4/X,3E16.4/
     :     X,2(E16.4,2I8)/X,8I8)
    5  FORMAT('0Redtape section 4'/X,17('-')/(X,5F16.5))
    6  FORMAT('0Redtape section 5'/X,17('-')/(X,10I8))
    7  FORMAT('0Redtape section 6'/X,17('-')/X,I8/(X,4A20))
    8  FORMAT('0Redtape section 7'/X,17('-')/X,2I8/(X,10A4))
    9  FORMAT(X,I8,E16.4)
   10  FORMAT(X,I16,E16.4)
C
       END
