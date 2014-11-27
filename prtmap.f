


*+PRTMAP

       SUBROUTINE PRTMAP (IUNIT, DATA, IUV, FORM, STATUS)
C      --------------------------------------------------
C
C  Prints out map data.
C
C  Given:
C      IUNIT     integer     logical unit number
C      DATA      real*4()    map data
C      IUV       integer(4)  U,V coordinate window
C      FORM      char*(*)    output format
C      STATUS    integer     status value
C
C  Prints out the map data held by rows in array DATA, on the output
C  unit IUNIT.  The data within the coordinate window specified by the
C  array IUV (IU1,IU2,IV1,IV2) are printed out in external units, using
C  the format provided in character string FORM.  The redtape common
C  blocks are assumed to be set up appropriately for the data array,
C  and are not updated.
C
C  For aperture data, the array should be set up in the calling program
C  to provide complex data values.
C
C  The STATUS value should be zero on entry, and is normally unchanged
C  on exit.  Possible error conditions are:
C
C      - invalid u,v window (ILL_UVWIND)
C      - u,v window outside map (UV_OUTMAP)
C
C  (DJT, 21 January 87)
C
*-
       REAL*4     DATA(1)
       CHARACTER  FORM*(*)
       INTEGER    IUNIT, IUV(4), STATUS
       INTEGER    IU1, IU2, IV, IV1, IV2
       INTEGER*4  IX, IX1, IX2, NW
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'
C
       IF (STATUS.NE.0) RETURN
C
       CALL CHCKUV(IUV,STATUS)
       IF (STATUS.EQ.0) THEN
C
         IU1=IUV(1)
         IU2=IUV(2)
         IV1=IUV(3)
         IV2=IUV(4)
C
         NW=1
         IF (ISWD.EQ.4) NW=2
C
C  Print out the data within the U,V window, in external units
C
         DO IV=IV1,IV2,-1
           IX1=((IVMAP1-IV)*IXMAX+(IU1-IUMAP1))*NW+1
           IX2=IX1+(IU2-IU1+1)*NW-1
           WRITE (IUNIT,'(A,2I4,4X,A,I4)') '0U1,U2 =',IU1,IU2,'V =',IV
           WRITE (IUNIT,FORM) (DATA(IX),IX=IX1,IX2)
         ENDDO
C
       ENDIF
C
       IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine PRTMAP')
C
       END
