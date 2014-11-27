



*+ENMINIRT

       SUBROUTINE ENMINIRT( REDTAPE, STATUS )
C      --------------------------------------
C
C     Returns the abbreviated map redtape definition.
C
C     Returned:
C         Abbreviated map redtape - see (library)maplib-minirt:incl
              INTEGER     REDTAPE(*)
C         Status value - unchanged, must be zero on entry.
              INTEGER     STATUS

C     NPR - 9 September 1988
C
C-

             include '/mrao/include/maplib_minirt.inc'
             include '/mrao/include/maplib_redtape.inc'

      integer     i

      if (status.ne.0) return

      u1_mrt = iumap1
      u2_mrt = iumap2
      v1_mrt = ivmap1
      v2_mrt = ivmap2
      nx_mrt = ixmax
      ny_mrt = iymax
      dtp_mrt= iabs(iswd)
      blk_mrt= blank

      do 10, i = 1, minirt_len
          redtape(i) = minirt(i)
   10 continue

      return
      end
