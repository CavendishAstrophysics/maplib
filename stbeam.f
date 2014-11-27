


*+STBEAM

       subroutine stbeam(st_flxnrm,st_beam,st_posa,status)
C      ---------------------------------------------------
C
C Set parameters for the CLEAN beam shape in the redtape
C
C Input
C  ST_FLXNRM      -      R4        -       flux normalization FLXNRM
C  ST_BEAM        -      R4(2)     -       beam widths U and V arcsec
C                                          HPFBWU, HPFBWV
C  ST_POSA        -      R4        -       beam position-angle, degrees
C                                          BEAMPA
C Returned
C  STATUS         -      I4        -       Error return
C
C STATUS should be zero on entry and is not changed by this routine.
C The beam sizes are returned as an array for compatability with
C GETBW.
C
C [PA, 1 September 1988]
*-

       include '/mrao/include/maplib_redtape.inc'

       integer    status
       real*4     st_flxnrm, st_beam(2), st_posa

       if (status.ne.0) return

       flxnrm = st_flxnrm
       hpfbwu = st_beam(1)
       hpfbwv = st_beam(2)
       beampa = st_posa

       end
