*+STMLSF

       SUBROUTINE STMLSF (LSFNUM, SFNAME, LSFKEY, STATUS)
C      --------------------------------------------------
C
C  Sets a sample file name and lsf key in the redtape.
C
C  Given:
C      LSFNUM     integer     Number of the lsf in the redtape.
C      SFNAME     char*(*)    Name of the sample file
C      LSFKEY     integer     Key of the logical sample file.
C
C  Returned:
C      STATUS     integer     Status value-must be 0 on entry.
C
C  Sets up LSF information in the map redtape.
C
C  If either the LSFKEY is zero or the SFNAME is blank then the
C  specified LSF is deleted and all other LSF's are moved up one and
C  the NUMLSF counter is decremented by one.
C
C  Otherwise the name part of the full sample file name is isolated and
C  stored along with the specified LSF key.
C
C  A status of ILL_REDTAPE is returned if the specified LSFNUM is less
C  than zero or larger than the number of logical sample files in the
C  map or if you attempt to delete the last LSF.
C
C  (NPR, 26 November 86)
C
*-
       INTEGER        LSFNUM, LSFKEY, STATUS
       CHARACTER*(*)  SFNAME

       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'

       INTEGER*4      LSFDESC(5), MLSFKEY, I, J
       INTEGER        I1, I2
       CHARACTER      MSFNAME*16
       EQUIVALENCE  ( LSFDESC(1), MSFNAME )
       EQUIVALENCE  ( LSFDESC(5), MLSFKEY )

       IF (STATUS .NE. 0) RETURN

       IF (LSFNUM.LE.0 .OR. LSFNUM.GT.NUMLSF .OR. NUMLSF.GT.MAXLSF) THEN
          STATUS = ILL_REDTAPE

       ELSE IF (LSFKEY.NE.0 .AND. SFNAME.NE. ' ') THEN
C         Update the specified LSF
          I2 = CHR_ILSTC( SFNAME, '/') - 1
          I1 = CHR_ILSTC( SFNAME(1:I2), '/') + 1
          MSFNAME = SFNAME(I1:I2)
          MLSFKEY = LSFKEY
          DO I = 1, 5
             LSFARR(I,LSFNUM) = LSFDESC(I)
          END DO
       ELSE IF (NUMLSF .GT. 1) THEN
C         Delete the specified LSF
          DO I = LSFNUM+1, NUMLSF
            DO J = 1, 5
              LSFARR(J,I-1) = LSFARR(J,I)
            END DO
          END DO
          NUMLSF = NUMLSF-1
       ELSE
C         Cannot delete the LSF since it is the last one left
          STATUS = ILL_REDTAPE
       END IF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine STMLSF')
C
       END
