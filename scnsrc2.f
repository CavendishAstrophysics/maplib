


C+SCNSRC2

       SUBROUTINE SCNSRC2( REDTAPE, MAP, UV, STATS, POSNS, S )
C      -------------------------------------------------------

C     Returns statistics for a source region

C     Given
C         Abbreviated form of map redtape
              integer     redtape(*)
C         Map data and uv window
              real        map(*)
              integer     uv(4)

C     Returned
C         Map statistics
              real        stats(6)
              integer     posns(4)
C         Status - must be zero on entry
              integer     s

C     Exactly the same as the maplib routine scnmap except there
C     are an additional two statistics returned - stats(5) contains
C     the zero level and stats(6) the map noise.
C
C     These are evaluated using one of two methods:
C     1.  If there are greater than 1000 points a standard iterative
C         technique is used where points are clipped if they are greater
C         than n times the noise in the previous iteration. The value
C         of n used is 4, and termination occurs when the noise from
C         succesive iterates differs by less than 10%.
C     2.  If there are less than 1000 points then the zero is determined
C         via a mode calculation. The points are sorted, and the zero
C         is the average of the maximum number of points which occur
C         in a flux range equal to the noise calculated from two
C         iterations of method 1. The noise is then recalculated using
C         only points less than this zero.
C
C     NPR  June 1988 - December 1988
C-
             include    '/mrao/include/maplib_minirt.inc'

      integer     max_pts
      parameter ( max_pts = 1000 )

      real*8      sum, sumsq
      real        gate, noise, old_noise, mean
      integer     n, v, row_offset, map_ptr
      integer     low, high, up_bnd, range, max_range
      real        points(max_pts)

      if (s.ne.0) return

C     Copy redtape
      do 100, n = 1, minirt_len
          minirt(n) = redtape(n)
  100 continue

      call scnmap2( redtape, map, uv, stats, posns, s )
      if (s.ne.0) goto 9999

      mean  = stats(3)
      noise = stats(4)
      n     = (uv(2)-uv(1)+1)*(uv(3)-uv(4)+1)

      if (noise*4.0.le.stats(1).or.n.le.max_pts) then
C         Evaluate noise estimate iteratively
  200     continue
              old_noise = noise
              gate  = 4.0*noise
              sum   = 0.0
              sumsq = 0.0
              n=0

              do 400, v = uv(3), uv(4), -1
                  row_offset = (v1_mrt-v)*nx_mrt-u1_mrt+1
                  do 300, map_ptr = row_offset+uv(1), row_offset+uv(2)
                      if (abs(map(map_ptr)-mean).le.gate) then
                          n=n+1
                          sum   = sum+map(map_ptr)
                          sumsq = sumsq+map(map_ptr)*map(map_ptr)

C                         Fill array for mode calculation later.
                          if (n.le.max_pts) points(n) = map(map_ptr)
                      end if
  300             continue
  400         continue

              mean  = sum/n
              noise = sqrt(sumsq/n-mean*mean)
          if ((n.gt.max_pts) .and.
     *        (old_noise-noise).gt.0.1*old_noise) goto 200

          if (n.le.max_pts) then
C             Small number of points - recalculate zero using a mode
              call util_qsortr( points, n )
              low  = 1
              max_range = -1
              do high = 1, n
                  do while (points(low).lt.(points(high)-noise))
                      low = low+1
                  end do

                  range = high-low
                  if (range.gt.max_range) then
                      max_range = range
                      up_bnd = high
                  end if
              end do

              sum = 0.0
              do n = up_bnd-max_range, up_bnd
                  sum = sum+points(n)
              end do
              mean = sum/(max_range+1)

C             Recalculate noise using only points less than the zero.
              n   = 0
              sum = 0.0
              do 800, v = uv(3), uv(4), -1
                  row_offset = (v1_mrt-v)*nx_mrt-u1_mrt+1
                  do 700, map_ptr = row_offset+uv(1), row_offset+uv(2)
                      if (map(map_ptr).le.mean) then
                          n=n+1
                          sum = sum+(map(map_ptr)-mean)**2
                      end if
  700             continue
  800         continue
              noise = sqrt(sum/n)
          end if
      end if

      stats(5) = mean
      stats(6) = noise

      if (s.ne.0) goto 9999
      return

 9999 call maperr( s, 'in routine SCNSRC2' )
      return
      end
