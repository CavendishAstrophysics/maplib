


*+ENAPER

       SUBROUTINE ENAPER( UWL2GP, VWL2GP, APSKEW, STATUS )
C      ----------------------------------------------------

C     Returns details of aperture scaling parameters from the redtape.
C
C     Given:
C         None.
C
C     Returned:
C         Conversion from wavelengths to gridpoints in u and v.
              real        uwl2gp, vwl2gp
C         Aperture skew angle.
              real*8      apskew
C         Status value - must be zero on entry - otherwise unchanged
              integer     status
C
C     If the map dimensions are neither:
C
C         1. A power of 2 in both U and V, or
C         2. A power of 2 plus 1 (ie 257) in U, and a power of 2 in V
C            (the redtape header must also be 'APERTURE' in this case)
C
C     then a status of ILL_REDTAPE is returned.
C
C     A tangent plane projection is treated as its sky projection
C     equivalent.
C
C     NPR  10 August 1988.
C
C-

             include    '/mrao/include/constants.inc'
             include    '/mrao/include/maplib_errors.inc'
             include    '/mrao/include/maplib_redtape.inc'

      integer     xpwr2, ypwr2

      if (status.ne.0) return

C     Find the nearest power of 2 to the map dimensions
      xpwr2 = nint(2.0**real(nint(log(real(ixmax))/log(2.0))))
      ypwr2 = nint(2.0**real(nint(log(real(iymax))/log(2.0))))

      if (xpwr2.eq.ixmax .and. ypwr2.eq.iymax) then
C         Assume a standard sky projection first.
          uwl2gp = ixmax * usamp * const_sa2r
          vwl2gp = iymax * usamp * const_sa2r
      else if (xpwr2.eq.(ixmax-1).and.ypwr2.eq.iymax.and.
     *         rthdr.eq.'APERTURE'                         ) then
          uwl2gp = (ixmax-1) * 2.0 * usamp * const_sa2r
          vwl2gp = iymax * usamp * const_sa2r
      else
          status = ILL_REDTAPE
      end if

      if (status.eq.0) then
          apskew = skew

C         Check for sky or tangent plane projection
          if ( iproj .ne. 1 ) vwl2gp = vwl2gp * dsin(decobs)
      end if

      return
      end
