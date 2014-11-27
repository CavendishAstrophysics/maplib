


*+ENMLSF

       SUBROUTINE ENMLSF (LSFNUM, SFNAME, LSFKEY, STATUS)
C      --------------------------------------------------
C
C  Returns sample file name and lsf key from the redtape.
C
C  Given:
C      LSFNUM     integer     Number of the lsf in the redtape.
C
C  Returned:
C      SFNAME     char*(*)    Name of the sample file (does not have
C                             directory:user or :type extensions.)
C      LSFKEY     integer     Key of the logical sample file.
C      STATUS     integer     Status value-must be 0 on entry.
C
C  Returns the sample file name and lsf key corresponding to the given
C  logical sample file number, from the current map redtape.
C
C  A status of ILL_REDTAPE is returned if the specified LSFNUM is less
C  than zero or larger than the number of logical sample files in the
C  map.
C
C  (NPR, 26 November 86)
C
*-
       INTEGER        LSFNUM, LSFKEY, STATUS
       CHARACTER*(*)  SFNAME

       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'

       INTEGER*4      LSFDESC(5), MLSFKEY, I
       CHARACTER*16   MSFNAME
       EQUIVALENCE  ( LSFDESC(1), MSFNAME )
       EQUIVALENCE  ( LSFDESC(5), MLSFKEY )

       IF (STATUS .NE. 0) RETURN

       IF (LSFNUM.LE.0 .OR. LSFNUM.GT.NUMLSF .OR. NUMLSF.GT.MAXLSF) THEN
          STATUS = ILL_REDTAPE
       ELSE

         DO I = 1, 5
            LSFDESC(I) = LSFARR(I,LSFNUM)
         END DO

         SFNAME = MSFNAME
         LSFKEY = MLSFKEY

       END IF

       END
