


*+STSCAL

       SUBROUTINE STSCAL (ZMNX, IZMNX, STATUS)
C      ---------------------------------------
C
C  Sets up data scaling parameters in map redtape.
C
C  Given:
C      ZMNX      real*4(2)   maximum, minimum data values
C      IZMNX     integer(4)  U,V coordinates of max, min points
C      STATUS    integer     status value
C
C  Sets internal maximum and minimum data values in section 3 of the
C  redtape common blocks, given the data maximum and minimum in external
C  units, and the U,V coordinates of the maximum and minimum points.
C  The zero level and scaling factor are also set up appropriately
C  for the currently selected internal data type.
C
C  The STATUS value should be zero on entry, and is not changed.
C
C  (DJT, 19 May 87)
C
*-
       REAL*4  ZMNX(2)
       INTEGER  IZMNX(4), STATUS
C
       include '/mrao/include/maplib_redtape.inc'
C
       IF (STATUS.NE.0) RETURN
C
C  Internal data type integer*2
C
       IF (IABS(ISWD).EQ.1) THEN
         ZEROL=0.0
         SCALEF=AMAX1(ABS(ZMNX(1)),ABS(ZMNX(2)))/32760.0
         ZMAX=ZMNX(1)/SCALEF+ZEROL
         ZMIN=ZMNX(2)/SCALEF+ZEROL
C
C  Internal data type integer*4
C
       ELSEIF (IABS(ISWD).EQ.2) THEN
         ZEROL=0.0
         SCALEF=AMAX1(ABS(ZMNX(1)),ABS(ZMNX(2)))/2147483640.0
         ZMAX=ZMNX(1)/SCALEF+ZEROL
         ZMIN=ZMNX(2)/SCALEF+ZEROL
C
C  Internal data type real*4 or complex*8
C
       ELSEIF (IABS(ISWD).GE.3) THEN
         ZEROL=0.0
         SCALEF=1.0
         ZMAX=ZMNX(1)
         ZMIN=ZMNX(2)
       ENDIF
C
       IUZMAX=IZMNX(1)
       IVZMAX=IZMNX(2)
       IUZMIN=IZMNX(3)
       IVZMIN=IZMNX(4)
C
       END
