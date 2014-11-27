

C+pbcorr
C
      subroutine pbcorr ( ra, dec,
     *                    ra_aerial, dec_aerial,
     *                    tscope_type,
     *                    pb_value, s                      )
C
C     Calculates the primary beam correction for a telescope.
C
C     Given:
C         RA (or hour angle) and dec of the direction that the
C         correction is to be calculated for (angles in radians)
              real*8      ra, dec
C         RA (or hour angle) and dec of the direction that the
C         aerial was pointing at the time
              real*8      ra_aerial, dec_aerial
C         Telescope type
              integer     tscope_type
C
C     Return values:
C         Primary beam correction factor - a value between 0 and 1
              real        pb_value
C         Status value
              integer     s
C
C     Returns the value of the primary beam for the given telescope.
C     For most telescopes the primary beams are tabulated in the file
C     (library)maplib-telescope:incl, which also contains the telescope
C     type definitions.
C
C     The special problems of the 38 MHz at pointings other than the
C     North Pole require a different approach. In this case:
C     1a. If ra_aerial is positive or zero, it is interpreted as a MAP
C         centre, with the map being made in the standard way. The
C         value returned is the primary beam correction for a source on
C         this map.
C     1b. If ra_aerial is negative, then it and the ra are interpreted
C         as hour angles.
C     2.  The beams for all cases are tabulated as two-dimensional maps
C         held in standard files in the (maps-3:38mhz) directory.
C
C     Warning: fails for dec 70 38 MHz maps made at the North-Pole!
C
C     Written by Nick Rees,  August 1986 - November 1988.
C
C-
             include  '/mrao/include/constants.inc'
             include  '/mrao/include/maplib_minirt.inc'
             include  '/mrao/include/maplib_telescope.inc'

C     Number of 2-D modelled beams allowable.
          integer     num_models
          parameter ( num_models = 4 )

C     Micellaneous variables.
          integer     lun, i, j

C     Variables for calculating tabulated beams
          real        separation, fract

C     Variables for calculating 2-D modelled beams (38 MHz).
          logical     setup
          character   pb2d_name(num_models)*64
          integer     pb2d_rt( minirt_len, num_models )
          integer     pb2d_proj( 32, num_models )
          real        pb2d_pb( 128*64, num_models )
          real        rtdump(512), prdump(32)
          real*8      uv(2), ra_map

          common /prim_beam/  setup, pb2d_rt, pb2d_proj, pb2d_pb
          data    setup       / .false. /
          data    pb2d_name   / '/mrao/data/38MHz_pb_000070.full',
     *                          '/mrao/data/38MHz_pb_000070.map',
     *                          '/mrao/data/38MHz_pb_040070.map',
     *                          '/mrao/data/38MHz_pb_200070.map'  /

C     ****** Main Code **************************************

      if (s.ne.0) return

      if (tscope_type.le.0.or.tscope_type.gt.max_tscope) then
C         Undefined telescope.
          pb_value = 1.0
      else if (tscope_type.eq.T38      .and.
     *         dec_aerial.lt.(const_piby2-0.01) ) then

C         Dec 72 pointing of 38MHz - use 2-D modelled beams.
          call dpproj( prdump, s )

          if (.not. setup) then
              call dpredt( rtdump, s )

C             Read in the data for the primary beam models.
              do 100, i = 1, num_models
                  call opemap(lun, pb2d_name(i),'READ',0,s)
                  call rdredt(lun, 1, 0, s )
                  call dpproj( pb2d_proj(1,i), s )
                  call enminirt( pb2d_rt(1,i), s )
                  call rdmap( lun, 1, pb2d_pb(1,i), s )
                  close( lun )
  100         continue
              call ldredt( rtdump, s )
              setup = (s.eq.0)
          end if
          if (s.ne.0) goto 9999

C         Ascertain which model to use.
          ra_map = ra
          if (ra_aerial.ge.0) then
              i = 1
              ra_map = ra-ra_aerial
          else if (mod(ra_aerial,const_2pi).gt.-const_piby4) then
              i = 2
          else if (mod(ra_aerial,const_2pi).gt.-const_piby2) then
              i = 3
          else
              i = 4
          end if

          call ldproj( pb2d_proj(1,i), s )
          call rdtouv( ra_map, dec, uv(1), uv(2), s )
          call ruvval2( pb2d_rt(1,i), pb2d_pb(1,i), uv, 1, pb_value, s )
          if (pb_value .lt. 0.0 .or. s.ne.0 ) pb_value = 0.0
          s = 0
          call ldproj( prdump, s )
      else if (pb_indx(tscope_type).gt.0) then
C         Symmetrical beams tabulated in include file.
          separation = dsin(dec)*dsin(dec_aerial)+
     *                 dcos(dec)*dcos(dec_aerial)*dcos(ra_aerial-ra)
          if ( separation .gt. 1.0 ) then
              separation = 0
          else if ( separation .lt. -1.0 ) then
              separation = const_pi
          else
              separation = abs(acos( separation ))
          end if

C         Convert separation to a table index.
          separation = separation/(const_sa2r*pb_scale(tscope_type))

          if (separation.ge.max_pbtab) then
              pb_value = 0.0
          else
C             Linearly interpolate between points.
              i = int( separation )
              j = pb_indx( tscope_type )
              fract    = separation - real(i)
              pb_value = pb(i,j)+fract*(pb(i+1,j)-pb(i,j))
          end if
      else
C         Telescope type valid, but primary beam unknown.
          pb_value = 1.0
      end if

 9999 if (s.ne.0) call maperr( s, 'in subroutine pbcorr.' )
      return
      end
