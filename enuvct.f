
*+ENUVCT

       subroutine enuvct(en_xmc,en_ymc,en_iumap1,en_ivmap1,
     *                                 en_iumap2,en_ivmap2, status)
C      ------------------------------------------------------------
C
C Enquire information on the UV centre and range of map
C
C Returned
C   EN_XMC, YMC       --  real*8   --         decimal map centre
C   EN_IUMAP1 etc.    --  integer  --         range on map
C
C STATUS should be zero on entry and is not changed by this routine.
C
C [PA, 26 February 1993]
*-

       include '/mrao/include/maplib_redtape.inc'
       integer      status
       integer      en_iumap1, en_iumap2, en_ivmap1, en_ivmap2
       real*8       en_xmc, en_ymc

       if (status.ne.0) return

       en_xmc = xmc
       en_ymc = ymc
       en_iumap1 = iumap1
       en_iumap2 = iumap2
       en_ivmap1 = ivmap1
       en_ivmap2 = ivmap2
       end
