


*+LMXMAP

       SUBROUTINE LMXMAP (DATA, IUV, GATE, NMX, VALMX, IUVMX, STATUS)
C      --------------------------------------------------------------
C
C Scans a map area for local maxima.
C
C Given:
C     DATA      real*4(*)    map data
C     IUV       integer(4)   u,v coordinate window
C     GATE      real*4       gate fr search
C
C Returned:
C     NMX       integer      no. of local maxima found
C     VALMX     real*4(*)    array of values of local maxima
C     IUVMX     integer(2,*) u,v coordinates of local maxima
C     STATUS    integer      status value
C
C Searches for local maxima.
C Finds any pixel such that the map value at that point is:
C     - greater than or equal to GATE
C     - greater than or equal to the value at each of the
C       8 neighbouring pixels.
C
C The pixels at the edge of the selected area are never
C included in the returned list of maxima. They are used only
C in searching for maxima.
C
C The routine is applicable to map data only, NOT apertures.
C
C (EMW, 8 March 88)
C
*-
      INTEGER  IUV(4),NMX,IUVMX(2,*),STATUS,IOFF(8)
      REAL*4   DATA(*),GATE,VALMX(*)
      INTEGER  IU, IV, IU1, IU2, IV1, IV2
      INTEGER  I, IX
C
       include '/mrao/include/maplib_redtape.inc'
       include '/mrao/include/maplib_errors.inc'

      IF (STATUS.NE.0) RETURN
C
C Check not an aperture
C
      IF (ISWD.EQ.4) STATUS=ILL_DATYPE
C
C Check u,v range
C
      CALL CHCKUV(IUV,STATUS)
      IF (STATUS.EQ.0) THEN
C
        IU1=IUV(1)
        IU2=IUV(2)
        IV1=IUV(3)
        IV2=IUV(4)
C
C Set up IOFF: offsets in index IX used in checking
C neighbouring 8 pixels
C
        IOFF(1)=-IXMAX-1
        IOFF(2)=-IXMAX
        IOFF(3)=-IXMAX+1
        IOFF(4)=-1
        IOFF(5)=+1
        IOFF(6)=IXMAX-1
        IOFF(7)=IXMAX
        IOFF(8)=IXMAX+1
C
C Work through the map area, omitting edge rows and columns,
C except for checking
C
        NMX=0
C
        DO IV=IV1-1,IV2+1,-1
          IX=(IVMAP1-IV)*IXMAX+(IU1+1-IUMAP1)
          DO IU=IU1+1,IU2-1
            IX=IX+1
            IF (DATA(IX).LT.GATE) GOTO 10
            DO I=1,8
              IF (DATA(IX).LT.DATA(IX+IOFF(I))) GOTO 10
            ENDDO
            NMX=NMX+1
            VALMX(NMX)=DATA(IX)
            IUVMX(1,NMX)=IU
            IUVMX(2,NMX)=IV
10          CONTINUE
          ENDDO
        ENDDO
C
      ENDIF
C
      IF (STATUS.NE.0) CALL MAPERR(STATUS,'in routine LMXMAP')
C
      END
