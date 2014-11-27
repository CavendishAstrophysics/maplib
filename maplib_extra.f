*+ENSAMP

       subroutine ensamp(en_usamp,en_vsamp,status)
C      -------------------------------------------
C
C Enquire parameters for the map sampling
C
C Returned
C  EN_USAMP       -      R8        -       sampling in U
C  EN_VSAMP       -      R8        -       sampling in V
C  STATUS         -      I4        -       Error return
C
C STATUS should be zero on entry and is not changed by this routine.
C The map sampling in both U and V is returned.
C
C [PA, 4 April 1991]
*-

       include '/mrao/include/maplib_redtape.inc'

       integer    status
       real*8     en_usamp, en_vsamp

       if (status.ne.0) return

       en_usamp = usamp
       en_vsamp = vsamp
       end
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
