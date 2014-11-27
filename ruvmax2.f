

*+RUVMAX2

       SUBROUTINE RUVMAX2( REDTAPE, DATA, IUV, RUV, MAX, S )
C      -----------------------------------------------------
C
C     Returns the real valued u and v of a local map maximum.
C
C     Given:
C         Abbreviated map redtape - see (library)maplib-minirt:incl
              INTEGER     REDTAPE(*)
C         Map data
              REAL        DATA(*)
C         U-V coordinates of local pixel maximum
              INTEGER     IUV(2)

C     Returned:
C         Position of map maximum.
              REAL*8      RUV(2)
C         Value of map at this position
              REAL        MAX
C         Status - must be zero on entry.
              INTEGER     S
C
C     NPR     9 September 1988.
C
*-
C     ******************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
             include        '/mrao/include/maplib_subgrid.inc'

C     ****************************************************************
C
C     Local variables.
C         Loop counter
              integer         i
C         Value of the map maximum from the previous iteration.
              real*8          u, v
C         Value & position of the map maximum for the next iteration.
              real            next_max
              real*8          next_u, next_v
C         Value & position in the map of the current point
              real            curr_val
              real*8          curr_u, curr_v, curr_uv(2)
              equivalence   ( curr_u, curr_uv(1))
              equivalence   ( curr_v, curr_uv(2))
C         Search size increment
              real*8          inc

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


      u = iuv(1)
      v = iuv(2)
      call iuvval2( redtape, data, iuv, max, s )
      next_u   = u
      next_v   = v
      next_max = max
      inc      = 0.5D+0

      do 300, i = 1, conv_osp2
          do 200, curr_u = u-inc, u+inc, inc
              do 100, curr_v = v-inc, v+inc, inc
                  if (curr_u.ne.u .or. curr_v.ne.v) then
                      call ruvval2(redtape,data,curr_uv,2, curr_val, s)
                      if (curr_val .gt. next_max) then
                          next_max = curr_val
                          next_u   = curr_u
                          next_v   = curr_v
                      end if
                  end if
  100         continue
  200     continue

          inc = 0.5*inc
          max = next_max
          u   = next_u
          v   = next_v
          if (s.ne.0) goto 9999
  300 continue

      ruv(1) = u
      ruv(2) = v
      if (s.ne.0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call maperr( s, 'in routine RUVMAX2' )
          return
      end
