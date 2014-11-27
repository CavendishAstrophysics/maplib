C  maplib_common.inc
C  -----------------
C
C  Common include file for MAPLIB library
C
C
C  Word length in bytes
       INTEGER    LWD
       PARAMETER (LWD=4)
C
C  Page size in words
       INTEGER    PAGE
       PARAMETER (PAGE=2048/LWD)
C
C  Buffer size in words
       INTEGER    MAXBUF, M2BUF
       PARAMETER (MAXBUF=4096, M2BUF=2*MAXBUF)
C
C  Common block containing data buffer for map I/O
       INTEGER*2  I2BUF(M2BUF)
       INTEGER*4  I4BUF(MAXBUF)
       REAL*4     R4BUF(MAXBUF)
C
       EQUIVALENCE (I2BUF, I4BUF, R4BUF)
       COMMON /MAPBUF/ R4BUF
C
