C  maplib_redtape.inc
C  ------------------
C
C  INCLUDE file for use with MAPLIB redtape routines
C
C  This file defines the format of MRAO-ND/SUN map redtape.  Items of redtape
C  are equivalenced to elements of the integer array IRTALL, which is
C  one page (2048 bytes) in length, and is available to the routines of
C  the MAPLIB library, and to applications programs, via common /REDTAPE/.
C
C  .... modified by NPR, 13 July 1987
C  .... modified by DJT, 28 August 1987
C  .... modified by NPR, 16 November 1987
C  .... modified by GGP,  9 May 1988
C  .... modified by PA,  31 July 1988
C  last modified by DJT, 29 November 1989
C  .... modified by PJW  20 August 1991
C  .... modified by PA,   8 September 1991; UNIX/SUNOS implementation
C

       INTEGER*4  IRTALL(512)
       COMMON /REDTAPE/ IRTALL

C
C  Redtape section 0 : redtape format
C
       INTEGER*4  IRT0(50)
       EQUIVALENCE (IRT0(1),IRTALL(1))
C
C    1     RTHDR   (C*12)  'MAP' or 'APERTURE' keyword
C    4     RTSFLG  (I*4)   flag for map saved on tape [1 if saved]
C    5     NPTOT   (I*4)   total no.of pages on file
C    6     ICMAX   (I*4)   no.of maps on file
C    7     NPMAP   (I*4)   no.of pages of each map (if equal)
C    8     ICMAP   (I*4)   map number
C    9     IPMAP   (I*4)   total no.of pages in this map
C                          total no.of indexed values (ISWD<0)
C    10    IWMAX   (I*4)   no.of components for this map
C    11    ISTYND  (I*4)   style of MRAO redtape
C    12    IPCOMP  (I*4)   no.of pages of redtape for each component
C    13    ISTYRT  (I*4)   style of other redtape
C    14    IPEXRT  (I*4)   no.of pages of other redtape
C    15    ILEXRT  (I*4)   record length of other redtape
C    16    IWMAP   (I*4)   component number

C    33    RTCSM1  (I*4)   checksum for redtape page
C    34    RTCSM2  (I*4)   check sum for data
C    35    RTCSXR  (I*4)   exclusive OR of RTCMS1 and RTCSM2
C    36    RTOWNR  (C*16)  user name for map owner
C    40    RTUSER  (C*16)  user name for last user
C    44    RTSVOL  (C*8)   save tape volume name
C    46    RTSDAT  (I*4)   time and date saved on tape
C    47    RTELES  (I*4)   telescope identifier code
*                                                0 - undefined
*                            (1-20)    MRAO      1 - CLFST 151 MHz
*                                                2 - CLFST 38 MHz
*                                                3 - VLBI 81.5 MHz
*                                                4 - RYLE 5 GHz
*                                                5 - RYLE 15 GHz
*                                                6 - 6C 151MHz
*                                                7 - old 38MHz
*                                                8 - one mile
*                                                9 - old 5km
*                            (21-99)   radio    21 - VLA A config
*                                               22 - VLA B config
*                                               23 - VLA C config
*                                               24 - VLA D config
*                                               30 - WSRT
*                                               40 - MERLIN
*                                               50 - EVN
*                                               60 - VLBI
*                                               70 - VLBA
*                                               80 - JCMT
*                            (101-199) optical
*                            (200-299) X-ray

C    48    MAPTYP  (I*4)   map type (1=map, 2=beam)
C
       INTEGER*4  NPTOT,ICMAX,NPMAP,ICMAP,IPMAP,IWMAX,IWMAP
       INTEGER*4  ISTYND,IPCOMP,ISTYRT,IPEXRT,ILEXRT
       INTEGER*4  RTSFLG,RTCSM1,RTCSM2,RTCSXR,RTSDAT,RTELES, MAPTYP
       CHARACTER  RTHDR*12,RTOWNR*16,RTUSER*16,RTSVOL*8
C
       EQUIVALENCE  (IRT0(1),RTHDR)
       EQUIVALENCE  (IRT0(4),RTSFLG)
       EQUIVALENCE  (IRT0(5),NPTOT)
       EQUIVALENCE  (IRT0(6),ICMAX)
       EQUIVALENCE  (IRT0(7),NPMAP)
       EQUIVALENCE  (IRT0(8),ICMAP)
       EQUIVALENCE  (IRT0(9),IPMAP)
       EQUIVALENCE  (IRT0(10),IWMAX)
       EQUIVALENCE  (IRT0(11),ISTYND)
       EQUIVALENCE  (IRT0(12),IPCOMP)
       EQUIVALENCE  (IRT0(13),ISTYRT)
       EQUIVALENCE  (IRT0(14),IPEXRT)
       EQUIVALENCE  (IRT0(15),ILEXRT)
       EQUIVALENCE  (IRT0(16),IWMAP)
       EQUIVALENCE  (IRT0(33),RTCSM1)
       EQUIVALENCE  (IRT0(34),RTCSM2)
       EQUIVALENCE  (IRT0(35),RTCSXR)
       EQUIVALENCE  (IRT0(36),RTOWNR)
       EQUIVALENCE  (IRT0(40),RTUSER)
       EQUIVALENCE  (IRT0(44),RTSVOL)
       EQUIVALENCE  (IRT0(46),RTSDAT)
       EQUIVALENCE  (IRT0(47),RTELES)
       EQUIVALENCE  (IRT0(48),MAPTYP)

C
C  Redtape section 1 : map title
C
       INTEGER*4  IRT1(200)
       EQUIVALENCE (IRT1(1),IRTALL(51))
C
C    1-200 RTITLE  (C*40)  map title, 20 rows of 40 characters
C
       CHARACTER  RTITLE(20)*40
       EQUIVALENCE  (IRT1(1),RTITLE)

C
C  Redtape section 2 : computing redtape
C
       INTEGER*4  IRT2(20)
       EQUIVALENCE (IRT2(1),IRTALL(251))
C
C    1     IXMAX   (I*4)   map row length in pixels
C    2     IYMAX   (I*4)   no.of rows in map
C    4     UVMAPW  (4*I*4) UV window of complete map.
C    4     IUMAP1  (I*4)   lower limit in u-coordinate [(-Xmc+1) rounded]
C    5     IUMAP2  (I*4)   upper limit in u-coordinate [(-Xmc+IXmax) rounded]
C    6     IVMAP1  (I*4)   upper limit in v-coordinate [(Ymc-1) rounded]
C    7     IVMAP2  (I*4)   lower limit in v-coordinate [(Ymc-IYmax) rounded]
C    8     ISWD    (I*4)   code for internal data type 1,2,3,4 : I2,I4,R4,C8
C                          ISWD<0 for indexed map files (x,y,data stored
C                          non-blank values only)
C    9     MPBLK   (I*4)   block size (bytes)
C    10    MPBLK1  (I*4)   block number of start of data
C    11    IBLNK2  (I*2)   undefined pixel value for I*2 data type
C          IBLNK4  (I*4)   undefined pixel value for I*4 data type
C    12    BLANK   (R*4)   undefined pixel value for R*4 data type
C
C  Words 13 to 20 are set only within a SURVEY style beam set file
C
C    13    BSETID  (C*4)   Beam set identification - 'BSET'
C    14    BSETFW  (I*4)   Flag word:
C                          (B0) BSFREG Regular beam set
C                          (B1) BSFBCA Bandwidth correction applied
C                          (B2) BSFPBA Primary beam correction applied
C                          (B3) BSFITA Integration time corrn applied
C    15    U0SET   (I*4)   U position of grid origin (bottom LH corner)
C    16    V0SET   (I*4)   V position of grid origin
C    17    DUSET   (I*4)   U grid increment
C    18    DVSET   (I*4)   V grid increment
C    19    NUSET   (I*4)   Number of beams along U axis
C    20    NVSET   (I*4)   Number of beams along V axis
C
       INTEGER*4  IXMAX,IYMAX,UVMAPW(4),IUMAP1,IUMAP2,IVMAP1,IVMAP2
       INTEGER*4  ISWD,MPBLK,MPBLK1,IBLNK4
       INTEGER*2  IBLNK2
       REAL*4     BLANK
       CHARACTER  BSETID*4
       INTEGER*4  BSETFW, U0SET, V0SET, DUSET, DVSET, NUSET, NVSET
C
       EQUIVALENCE  (IRT2(1),IXMAX)
       EQUIVALENCE  (IRT2(2),IYMAX)
       EQUIVALENCE  (IRT2(4),UVMAPW(1))
       EQUIVALENCE  (IRT2(4),IUMAP1)
       EQUIVALENCE  (IRT2(5),IUMAP2)
       EQUIVALENCE  (IRT2(6),IVMAP1)
       EQUIVALENCE  (IRT2(7),IVMAP2)
       EQUIVALENCE  (IRT2(8),ISWD)
       EQUIVALENCE  (IRT2(9),MPBLK)
       EQUIVALENCE  (IRT2(10),MPBLK1)
       EQUIVALENCE  (IRT2(11),IBLNK2,IBLNK4)
       EQUIVALENCE  (IRT2(12),BLANK)
       EQUIVALENCE  (IRT2(13),BSETID)
       EQUIVALENCE  (IRT2(14),BSETFW)
       EQUIVALENCE  (IRT2(15),U0SET)
       EQUIVALENCE  (IRT2(16),V0SET)
       EQUIVALENCE  (IRT2(17),DUSET)
       EQUIVALENCE  (IRT2(18),DVSET)
       EQUIVALENCE  (IRT2(19),NUSET)
       EQUIVALENCE  (IRT2(20),NVSET)

C
C  Redtape section 3 : astronomical redtape
C
       INTEGER*4  IRT3(30)
       EQUIVALENCE (IRT3(1),IRTALL(271))
C
C    1-4   NAME    (C*16)  name of measured quantity
C    5-8   POLN    (C*16)  polarisation
C    9-12  EXUNIT  (C*16)  name of external unit
C    13    FREQ    (R*4)   frequency in MHz
C    14    ZEROL   (R*4)   zero level in internal units
C    15    SCALEF  (R*4)   scale factor, internal -> external units
*                         [value/ext-unit = (value/int-unit - zero)*scalef]
C    16    ZMAX    (R*4)   data maximum in internal units
C    17    IUZMAX  (I*4)   u-coordinate of data maximum
C    18    IVZMAX  (I*4)   v-coordinate of data maximum
C    19    ZMIN    (R*4)   data minumum in internal units
C    20    IUZMIN  (I*4)   u-coordinate of data minimum
C    21    IVZMIN  (I*4)   v-coordinate of data minimum
C    22    IPOLN   (I*4)   polarisation code
*               -1        0    1 2 3 4   5   6
*            undefined  beam   I Q U V  I-Q I+Q
*                         10   11   12   13
*                         mI   Chi  %P   Alpha
C    23    IPROJ   (I*4)   projection code (1=Equ., 2=Sky, 3=Tangent)
C     24 - 27 from 5-km spacing data
C    28    NSPAC   (I*4)   number of spacings used
C
       CHARACTER  NAME*16,POLN*16,EXUNIT*16
       INTEGER*4  IUZMAX,IUZMIN,IVZMAX,IVZMIN,IPOLN,IPROJ,NSPAC
       REAL*4     FREQ,ZEROL,SCALEF,ZMAX,ZMIN
C
       EQUIVALENCE  (IRT3(1),NAME)
       EQUIVALENCE  (IRT3(5),POLN)
       EQUIVALENCE  (IRT3(9),EXUNIT)
       EQUIVALENCE  (IRT3(13),FREQ)
       EQUIVALENCE  (IRT3(14),ZEROL)
       EQUIVALENCE  (IRT3(15),SCALEF)
       EQUIVALENCE  (IRT3(16),ZMAX)
       EQUIVALENCE  (IRT3(17),IUZMAX)
       EQUIVALENCE  (IRT3(18),IVZMAX)
       EQUIVALENCE  (IRT3(19),ZMIN)
       EQUIVALENCE  (IRT3(20),IUZMIN)
       EQUIVALENCE  (IRT3(21),IVZMIN)
       EQUIVALENCE  (IRT3(22),IPOLN)
       EQUIVALENCE  (IRT3(23),IPROJ)
       EQUIVALENCE  (IRT3(28),NSPAC)

C
C  Redtape section 4: Map making definition and control.
C
       INTEGER*4  IRT4(12)
       EQUIVALENCE (IRT4(1),IRTALL(301))
C
C     1    CONVTP  (I*4)   Convolution function type:
C                          1 = prolate spheroidal
C                          2 = Gaussian*sinc
C                          3 = L2 optimal gridding function.
C     2    CONVOS  (I*4)   Convolution function oversampling
C     3    CONVHW  (I*4)   Convolution function halfwidth
C     4-5  CONVPA  (R*4)   Other conv. function parameters (2 elements)
C     6    CORRTP  (I*4)   Window correction function type
C                          0 = No window correction.
C                          1 = Sze Tan's DFT-FFT optimised correction.
C                          2 = Old mapper's FFT of conv fn correction.
C     7    WGHTTP  (I*4)   Visibility weighting function type:
C                          0 = No weighting.
C                          1 = Super smooth weighting.
C                          2 = radial weighting.
C                          3 = gaussian weighting.
C                          4 = radial*gaussian weighting.
C                          10-14 = same as 0-4 except a visibility
C                                  cutoff applied at a given radius.
C     8-9  WGHTPA  (R*4)   Weighting function parameters (2 elements)
C    10    GRADTP  (I*4)   Aperture grading function type
C                          0 = no grading.
C                          1 = radial grading.
C                          2 = gaussian grading.
C                          3 = radial*gaussian grading.
C    11-12 GRADPA  (R*4)   Grading function parameters (2 elements)
C
       INTEGER*4  CONVTP, CONVOS, CONVHW, CORRTP, WGHTTP, GRADTP
       REAL*4     CONVPA(2), WGHTPA(2), GRADPA(2)
C
       EQUIVALENCE  (IRT4( 1),CONVTP)
       EQUIVALENCE  (IRT4( 2),CONVOS)
       EQUIVALENCE  (IRT4( 3),CONVHW)
       EQUIVALENCE  (IRT4( 4),CONVPA)
       EQUIVALENCE  (IRT4( 6),CORRTP)
       EQUIVALENCE  (IRT4( 7),WGHTTP)
       EQUIVALENCE  (IRT4( 8),WGHTPA)
       EQUIVALENCE  (IRT4(10),GRADTP)
       EQUIVALENCE  (IRT4(11),GRADPA)

C
C  Redtape section 5: Derived Quantities.
C
       INTEGER*4  IRT5(8)
       EQUIVALENCE (IRT5(1),IRTALL(313))
C
C     1    FLXNRM  (R*4)   Flux normalisation factor
C     2    HPFBWU  (R*4)   Half-power full beamwidth in U (arcsec)
C     3    HPFBWV  (R*4)   Half-power full beamwidth in V (arcsec)
C     4    BEAMPA  (R*4)   Beam position angle (degrees) N thr' E
C     5    effective_bandwidth (R*4)  of data being used
C     6    effective_integration (R*4)
C
       REAL*4     FLXNRM,HPFBWU,HPFBWV,BEAMPA
       real*4     effective_bandwidth, effective_integration
C
       EQUIVALENCE  (IRT5( 1),FLXNRM)
       EQUIVALENCE  (IRT5( 2),HPFBWU)
       EQUIVALENCE  (IRT5( 3),HPFBWV)
       EQUIVALENCE  (IRT5( 4),BEAMPA)
       EQUIVALENCE  (IRT5( 5),effective_bandwidth)
       EQUIVALENCE  (IRT5( 6),effective_integration)
C
C  Redtape section 6 : astrometric redtape
C
       INTEGER*4  IRT6(40)
       EQUIVALENCE (IRT6(1),IRTALL(321))
C
C    1     RAMAP   (R*8)   RA of map grid (0,0) at REFDAT (radians)
C    3     DECMAP  (R*8)   Dec of map grid (0,0) at REFDAT (radians)
C    5     REFDAT  (R*8)   reference date (years) -
C                          MUST be either 1950.0, 2000.0 or EPOCH.
C                          The latter is ONLY used if the map centre is
C                          at the North (or South) pole at EPOCH. If
C                          this is the case, the only valid value of
C                          REFDAT is EPOCH.
C    7     XMC     (R*8)   exact x-coordinate of map centre
C    9     YMC     (R*8)   exact y-coordinate of map centre
C    11    SKEW    (R*8)   projection skew angle (radians) -
C                          Defined in terms of RAMAP precessed, if
C                          necessary, to EPOCH. The position angle at
C                          the grid (0,0) of the line of RA equal to the
C                          precessed value of RAMAP is equal to SKEW for
C                          tangent-plane projections and and to -SKEW
C                          for other projections. Note that if the grid
C                          (0,0) is at either pole then REFDAT=EPOCH and
C                          the grid is still defined because no
C                          precession is done.
C    13    USAMP   (R*8)   sampling in u (arcsecs) for a map with SKEW=0
C    15    VSAMP   (R*8)   sampling in v (arcsecs) for a map with SKEW=0
C    17    EPOCH   (R*8)   epoch of projection (years).
C    19    PRANG   (R*8)   precession angle between EPOCH and 1950.0
C    21    RAOBS   (R*8)   RAMAP  precessed to OBSDAT.
C    23    DECOBS  (R*8)   DECMAP precessed to OBSDAT.
C    25    RAPNT   (R*8)   RA of telescope pointing direction at OBSDAT.
C    27    DECPNT  (R*8)   Dec of telescope pointing direction at OBSDAT
C    29    OBSDAT  (R*8)   Date of observation (decimal years)
C    31    RAPC    (R*8)   RA  of path compensators for observation.
C    33    DECPC   (R*8)   Dec of path compensators for observation.
C    35    RAMPC   (R*8)   RA of map-making phase centre at REFDAT.
C    37    DECMPC  (R*8)   Dec of map-making phase centre at REFDAT.
C
C    N.B. - RAPNT and RAPC are negative if the position is an hour angle
C         - OBSDAT, RAPNT, DECPNT, RAPC and DECPC are only necessarily
C           applicable to the first sample file if more than one is used
C
       REAL*8  RAMAP,DECMAP,REFDAT,XMC,YMC,SKEW,USAMP,VSAMP
       REAL*8  EPOCH,PRANG,RAOBS,DECOBS,RAPNT,DECPNT,OBSDAT
       REAL*8  RAPC, DECPC, RAMPC, DECMPC
C
       EQUIVALENCE  (IRT6(1),RAMAP)
       EQUIVALENCE  (IRT6(3),DECMAP)
       EQUIVALENCE  (IRT6(5),REFDAT)
       EQUIVALENCE  (IRT6(7),XMC)
       EQUIVALENCE  (IRT6(9),YMC)
       EQUIVALENCE  (IRT6(11),SKEW)
       EQUIVALENCE  (IRT6(13),USAMP)
       EQUIVALENCE  (IRT6(15),VSAMP)
       EQUIVALENCE  (IRT6(17),EPOCH)
       EQUIVALENCE  (IRT6(19),PRANG)
       EQUIVALENCE  (IRT6(21),RAOBS)
       EQUIVALENCE  (IRT6(23),DECOBS)
       EQUIVALENCE  (IRT6(25),RAPNT)
       EQUIVALENCE  (IRT6(27),DECPNT)
       EQUIVALENCE  (IRT6(29),OBSDAT)
       EQUIVALENCE  (IRT6(31),RAPC)
       EQUIVALENCE  (IRT6(33),DECPC)
       EQUIVALENCE  (IRT6(35),RAMPC)
       EQUIVALENCE  (IRT6(37),DECMPC)

C
C  Redtape section 7 : date list of observations
C
       INTEGER*4  IRT7(20)
       EQUIVALENCE (IRT7(1),IRTALL(361))
C
C    1     NODAYS  (R*4)   no.of days
C    2-20  IODATS  (I*4)   list of dates [as YYMMDD]
C
       INTEGER*4  NODAYS,IODATS(19)
C
       EQUIVALENCE  (IRT7(1),NODAYS)
       EQUIVALENCE  (IRT7(2),IODATS)

C
C  Redtape section 8 : processing history
C
       INTEGER*4  IRT8(80)
       EQUIVALENCE (IRT8(1),IRTALL(381))
C
C    1     NHIST           no.of entries
C    2-80  HISTRY  (C*20)  date and program name for each entry
C
       CHARACTER  HISTRY(15)*20
       INTEGER*4  NHIST
C
       EQUIVALENCE  (IRT8(1),NHIST)
       EQUIVALENCE  (IRT8(2),HISTRY)

C
C  Redtape section 9 : Logical sample file sources.
C
       INTEGER*4  IRT9(52)
       EQUIVALENCE (IRT9(1),IRTALL(461))
C
C     1    NUMLSF  (I*4)   Number of logical sample files used.
C     2    MAXLSF  (I*4)   The maximum number of LSF's allowed.
C     3    LSFARR  (I*4)   5*10 element array containing all the sample
C                          file names (16 characters each) in the first
C                          four elements of every row and the logical
C                          sample file keys as the last element. These
C                          can be accessed by the routines ENMLSF and
C                          STMLSF.
C
       INTEGER*4  NUMLSF, MAXLSF, LSFARR(5,10)
C
       EQUIVALENCE  (IRT9(1),NUMLSF)
       EQUIVALENCE  (IRT9(2),MAXLSF)
       EQUIVALENCE  (IRT9(3),LSFARR)

