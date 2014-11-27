C
C
*+ enpoint

       subroutine enpoint(en_rapnt,en_decpnt,en_rteles,status)
C      -------------------------------------------------------
C
C Enquire pointing information for telescope
C
C Returned:
C   RA and DEC of pointing centre
       real*8       en_rapnt, en_decpnt
C   telescope code
       integer      en_rteles
C   error return code
       integer      status
C
C-
       include '/mrao/include/maplib_redtape.inc'

       if (status.ne.0) return
       en_rapnt = rapnt
       en_decpnt = decpnt
       en_rteles = rteles
       if (status.ne.0) call io_wrerr(status,'in ENPOINT')
       end
