



*+REPRUV

       SUBROUTINE REPRUV ( UV, UV2,
     *                     IPROJ2, IPREC, USAMP2, SKEW2,
     *                     RAMAP2, DECMAP2, REFDAT2, EPOCH2, S )
C
C     Routine to find uv range UV2 for reprojected map to cover 
C     a given range UV on input map - for use in do_reproject

C     Given:
C         Input map redtape assumed resident in common blocks.
C         U, V dimensions of input map.
              INTEGER     UV(4)
C         Precession type to use if the map epochs differ.
              INTEGER     IPREC
C         Projection parameters of output map.
              INTEGER     IPROJ2
              REAL*8      USAMP2, SKEW2, RAMAP2,DECMAP2, REFDAT2, EPOCH2


C     Returned:
C         UV2 required uv range of reprojected map.
              INTEGER     UV2(4)
C         S - must be zero on entry.
              INTEGER     S


C     SEGH    17 July 1998
C
*-
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
C         Integral and corresponding real map co-ordinates
              INTEGER         IU, IV
              REAL*8          U, V, RA, DEC
C         Increments in u,v, index, for moving along edges of input map        
              INTEGER         USTEP, VSTEP, UVSTEP(4), J
C         Maximum and minimum corres. u,v values found for output map.
              REAL*8          UMIN, UMAX, VMAX, VMIN
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

      IF (RESAMP) THEN

        UMIN = DBLE(UV(1))/DBLE(SAMP_FACT)
        UMAX = DBLE(UV(2))/DBLE(SAMP_FACT)
        VMAX = DBLE(UV(3))/DBLE(SAMP_FACT)
        VMIN = DBLE(UV(4))/DBLE(SAMP_FACT)

      ELSE

C       Work through input region edges, finding the corresponding uv
C       position on the output map. saving max or min value as approp.      

        UMAX = -1.0E+10
        UMIN = +1.0E+10
        VMAX = -1.0E+10
        VMIN = +1.0E+10
        UVSTEP(1) = 1
        UVSTEP(2) = UV(4) - UV(3)
        UVSTEP(3) = UV(2) - UV(1)
        UVSTEP(4) = -1

        DO 300 J = 1, 2
        USTEP = UVSTEP(2*J-1)
        VSTEP = UVSTEP(2*J)


          DO 200 IV = UV(3), UV(4), VSTEP
            DO 100 IU = UV(1), UV(2), USTEP

                  U = DBLE( IU )
                  V = DBLE( IV )

C                 Find RA and Dec of the current pixel of the input map
                  CALL LDPROJ( PROJ1, S )
                  CALL UVTORD( U, V, RA, DEC, S )

C                 Precess RA and Dec to epoch of projection of output map
                  CALL PRECRD2( IPREC, EPOCH,RA,DEC, EPOCH2,RA,DEC )

C                 Convert (precessed) RA, Dec to u,v on output map
                  CALL LDPROJ( PROJ2, S )
                  CALL RDTOUV( RA, DEC, U, V, S )
                 
C                 Save current maxima and minima in U and V
                  UMAX = MAX( U, UMAX)
                  UMIN = MIN( U, UMIN)
                  VMAX = MAX( V, VMAX)
                  VMIN = MIN( V, VMIN)

                  IF (S .NE. 0) GOTO 9999

  100       CONTINUE
  200     CONTINUE
 300    CONTINUE

      ENDIF

C     Round maxima up and minima down to next whole pixel
      UV2(1) = UMIN - 1
      UV2(2) = UMAX + 1
      UV2(3) = VMAX + 1
      UV2(4) = VMIN - 1

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
