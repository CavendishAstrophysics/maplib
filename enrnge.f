C
C
*+ENRNGE

       subroutine enrnge(range,status)
C      -------------------------------
C
C Enquire data range on map
C
C Returned
C  RANGE          -      R4(2)     -       min/max from redtape
C  STATUS         -      I4        -       Error return
C
C STATUS should be zero on entry and is not changed by this routine.
C The range on the current map is returned from the information in the
C redtape.
C
C [PA, 4 April 1991]
*-

       include '/mrao/include/maplib_redtape.inc'

       integer    status
       real*4     range(2)

       if (status.ne.0) return

       range(1) = (zmin-zerol)*scalef
       range(2) = (zmax-zerol)*scalef
       end
