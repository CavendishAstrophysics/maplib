



*+REPROJ

       SUBROUTINE REPROJ ( MAP1,
     *                     MAP2, UV2,
     *                     IPROJ2, IPREC, USAMP2, SKEW2,
     *                     RAMAP2, DECMAP2, REFDAT2, EPOCH2,
     *                     DEGRID,
     *                     S )
C
C     General routine to reproject a map.

C     Given:
C         Input map - redtape must be in common blocks.
              REAL        MAP1(*)
C         U, V dimensions of output map.
              INTEGER     UV2(4)
C         Precession type to use if the map epochs differ.
              INTEGER     IPREC
C         Projection parameters of output map.
              INTEGER     IPROJ2
              REAL*8      USAMP2, SKEW2, RAMAP2,DECMAP2, REFDAT2, EPOCH2
C         Degridding type to use in routine RUVVAL
              INTEGER     DEGRID

C     Returned:
C         Reprojected map.
              REAL        MAP2(*)
C         S - must be zero on entry.
              INTEGER     S

C     This routine is fairly efficient for all kinds of reprojections
C     and so can be used for:
C
C     1.  Integral resampling (treated as a special case in the routine)
C     2.  Extracting a small portion of a map (special case of 1)
C     3.  Expanding a map beyond its present boundaries (again, a
C         special case of 1)
C     4.  Other, more normal cases of reprojection.
C
C     Notes:
C     1.  Speed.
C     The major rate determining step is if slow precession is used.
C     This can take over an hour of CPU time for a 512x512 map so avoid
C     it if at all possible. With fast precession, a complete
C     reprojection with interpolation by convolution for a 512x512 map
C     should take less than 10 minutes of CPU time. Naturally linear
C     interpolations are faster still and integral rescalings are the
C     fastest of all.
C
C     2.  Apertures.
C     These are handled with some provisos. They are treated as complex
C     (ie two component) maps in all respects. This leads to some
C     anomalies. Firstly, it doesn't make sense to change the map
C     centre or epoch, only the skew angle, uv window and sampling.
C     Secondly the sampling is treated as a map sampling - if you double
C     it you sample half as often,  not as the true sampling where if
C     you doubled it you would sample twice as often. Thus you have to
C     be careful when writing the sampling to the redtape in the
C     aperture after the reprojection (ie changing the uv window of an
C     aperture will alter the sampling).
C
C
C     NPR     26 January 1987.
C
*-
C     ******************************************************************
C
C     Function declarations
C
      integer         iuvmap

C     ******************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
             include        '/mrao/include/maplib_errors.inc'
             include        '/mrao/include/maplib_redtape.inc'
             include        '/mrao/include/maplib_proj.inc'

C     ****************************************************************
C
C     Local variables.
C         Map projection save buffers.
              INTEGER         PROJ1( LENPRJ ), PROJ2( LENPRJ )
C         Current pixel in input and output maps and the pixel size.
              INTEGER         PIXEL1, PIXEL2, PIXEL_SIZE
C         Flag indicating map is complex.
              LOGICAL         APERTURE
C         Integral and corresponding real map co-ordinates
              INTEGER         IU, IV
              REAL*8          U, V, RA, DEC
C         Flag set if reprojection is a pure integral resampling.
              LOGICAL         RESAMP
C         Rescaling factor
              INTEGER         SAMP_FACT

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Setup and save the input map projection.
      CALL STPROJ( IPROJ, 1, USAMP, SKEW,
     *             RAMAP, DECMAP, REFDAT, EPOCH, S )
      CALL DPPROJ( PROJ1, S )

C     Setup and save the output map projection.
      CALL STPROJ( IPROJ2, 1, USAMP2, SKEW2,
     *             RAMAP2, DECMAP2, REFDAT2, EPOCH2, S )
      CALL DPPROJ( PROJ2, S )

C     Check to see if the reprojection is just a re-sampling.
      SAMP_FACT = NINT( USAMP2/USAMP )
      RESAMP = ((USAMP2  .EQ. USAMP*DBLE(SAMP_FACT)) .AND.
     *          (IPROJ2  .EQ. IPROJ )                .AND.
     *          (SKEW2   .EQ. SKEW  )                .AND.
     *          (RAMAP2  .EQ. RAMAP )                .AND.
     *          (DECMAP2 .EQ. DECMAP)                .AND.
     *          (REFDAT2 .EQ. REFDAT)                .AND.
     *          (EPOCH2  .EQ. EPOCH )                       )

C     Work through output map point by point, finding the corresponding
C     position and map value on the input map.
      PIXEL2 = 1
      PIXEL_SIZE = 1
      APERTURE   = (ISWD .EQ. 4)
      IF (APERTURE) PIXEL_SIZE = 2

      DO 200 IV = UV2(3), UV2(4), -1
          DO 100 IU = UV2(1), UV2(2)
              IF (RESAMP) THEN
                  PIXEL1 = IUVMAP( INT(IU*SAMP_FACT), INT(IV*SAMP_FACT))
                  IF (PIXEL1.NE.0) THEN
                      IF (APERTURE) THEN
                          PIXEL1 = PIXEL1*2-1
                          MAP2( PIXEL2 )   = MAP1( PIXEL1 )
                          MAP2( PIXEL2+1 ) = MAP1( PIXEL1+1 )
                      ELSE
                          MAP2( PIXEL2 )   = MAP1( PIXEL1 )
                      END IF
                  ELSE
                      MAP2( PIXEL2 ) = BLANK
                      IF (APERTURE) MAP2(PIXEL2+1) = BLANK
                  END IF
              ELSE
                  U = DBLE( IU )
                  V = DBLE( IV )

C                 Find RA and Dec of the current pixel of the output map
                  CALL LDPROJ( PROJ2, S )
                  CALL UVTORD( U, V, RA, DEC, S )

C                 Precess RA and Dec to epoch of projection of input map
                  CALL PRECRD2( IPREC, EPOCH2,RA,DEC, EPOCH,RA,DEC )

C                 Convert (precessed) RA, Dec to u,v on input map
                  CALL LDPROJ( PROJ1, S )
                  CALL RDTOUV( RA, DEC, U, V, S )

C                 Interpolate input map to find map value at this point
                  CALL RUVVAL( MAP1, U, V, DEGRID, MAP2(PIXEL2), S )

C                 Check for an out of map error.
                  IF (S .EQ. UV_UNREAL .OR. S .EQ. UV_OUTMAP) THEN
                      S = 0
                      MAP2( PIXEL2 ) = BLANK
                      IF (APERTURE) MAP2( PIXEL2+1 ) = BLANK
                  ELSE IF (S .NE. 0) THEN
                      GOTO 9999
                  END IF
              END IF

              PIXEL2 = PIXEL2 + PIXEL_SIZE
  100     CONTINUE
  200 CONTINUE

      RETURN

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 CONTINUE
          CALL MAPERR( S, ' in routine REPROJ' )
          RETURN
      END
