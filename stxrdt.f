


*+STXRDT

       SUBROUTINE STXRDT (PAGES, STYLE, STATUS)
C      ----------------------------------------
C
C  Sets up extra redtape pages.
C
C  Parameters:
C      PAGES     integer     no.of extra redtape pages
C      STYLE     integer     style of extra redtape pages
C      STATUS    integer     status value
C
C  Assigns pages for extra redtape in a new output map.  These redtape
C  pages are positioned after the usual Nord redtape, and before the
C  map data.  This routine should be called for a new map after calls
C  to NWREDT or STREDT are made to set up the basic Nord style redtape.
C
C  The only acceptable STYLE of extra redtape records at present is STYLE=1.
C  This corresponds to 128 word records introduced by an 8-character redtape
C  descriptor, followed by a 4-byte date stamp, leaving space for up to 124
C  4-byte words of redtape information.  Redtape records may be written and
C  read using routines STXREC, ENXREC.
C
C  The STATUS value should be zero on entry.  A value of ILL_REDTAPE may
C  be returned if the current redtape is invalid.
C
C  (DJT, 29 November 89)
C  (DJT, 24 March 92; Unix implementation)
C  (PA,  16 January 94; Fixed bug which incorrectly set page size = 0)
C
*-
       INTEGER    PAGES, STYLE, STATUS
       INTEGER*4  NPAGES
       INTEGER    I
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_extrart.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
       CALL CHREDT(STATUS)
C
       IF (STATUS.EQ.0) THEN
C
C    Redtape section 0 : extra redtape information
C
         IF (IPEXRT.GE.1 .AND. IPEXRT.LE.MAX_EXTRA) THEN
           NPAGES = NPTOT - IPEXRT
         ELSE
           NPAGES = NPTOT
         ENDIF
         IF (STYLE.EQ.1 .AND.
     :       PAGES.GE.0 .AND. PAGES.LE.MAX_EXTRA) THEN
           ISTYRT=STYLE
           IPEXRT=PAGES
           ILEXRT=128
         ELSE
           STATUS=ILL_EXREDT
         ENDIF
C
         IF (STATUS.EQ.0) THEN
C
C    Adjust redtape section 2 : computing redtape
C
           MPBLK1=(2048*(IWMAX*IPCOMP+IPEXRT)-1)/MPBLK+2
C
C    Adjust redtape section 0 : file size in pages
C
           NPAGES=NPAGES+PAGES
           NPTOT=NPAGES
           NPMAP=NPAGES
           IPMAP=NPAGES
C
C    Initialise the extra redtape buffer array
C
           DO I=1,EXTRART_LEN
             EXTRART(I)=0
           ENDDO
C
         ENDIF
C
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine STXDRT')
C
       END


