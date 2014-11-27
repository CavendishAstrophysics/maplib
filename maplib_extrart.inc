C     *****************************************************************
C
C     Include file for MAPLIB extra redtape buffer.
C
C     Defines a buffer array to hold the extra redtape pages, if any.
C     The extra redtape pages are read in to the buffer by the RDREDT
C     routine, and written out to the map file by WRREDT.
C
C     *****************************************************************

C     Maximum number of extra redtape pages
          integer         max_extra
          parameter     ( max_extra = 1 )

C     Buffer length in 4-byte words
          integer         extrart_len
          parameter     ( extrart_len = max_extra*512 )

C     Record declaration
          integer*4       extrart ( extrart_len )

C     Common block declaration
          common /extra_rt/ extrart
