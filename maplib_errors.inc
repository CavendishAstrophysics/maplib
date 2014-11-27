C  maplib_errors.inc
C
C  INCLUDE file containing error codes produced by the MAPLIB library
C
C  Local MAPLIB error codes
C
       INTEGER  MAPLIB_OK
       INTEGER  ILL_ACCESS,  ILL_COMPNO, ILL_REDTAPE, ILL_UVWIND
       INTEGER  ILL_PRECD,   ILL_PROJN,  ILL_DATYPE
       INTEGER  UV_OUTMAP,   UV_UNREAL,  ARR_TOOSMALL
       INTEGER  ILL_CONVFN,  ILL_MAPFN,  ILL_CORRFN
       INTEGER  ILL_GRADING, ILL_WEIGHT, ILL_POLCODE
       INTEGER  ILL_EXREDT,  FLL_EXREDT, NO_EXREDT, NO_EXTREC

       PARAMETER (MAPLIB_OK=0)         ! status OK
       PARAMETER (ILL_ACCESS=-21)      ! invalid access code for map
       PARAMETER (ILL_COMPNO=-22)      ! invalid map component number
       PARAMETER (ILL_REDTAPE=-23)     ! invalid map redtape
       PARAMETER (ILL_UVWIND=-24)      ! invalid u,v window
       PARAMETER (UV_OUTMAP=-25)       ! u,v window outside map
       PARAMETER (UV_UNREAL=-26)       ! u,v corresponds to unreal point
       PARAMETER (ILL_PRECD=-27)       ! invalid precession code
       PARAMETER (ILL_PROJN=-28)       ! invalid projection parameters
       PARAMETER (ILL_DATYPE=-29)      ! invalid internal data type
       PARAMETER (ARR_TOOSMALL=-30)    ! internal work array too small
       PARAMETER (ILL_MAPFN=-31)       ! illegal tabulation function
       PARAMETER (ILL_CONVFN=-31)      ! illegal convolution function
       PARAMETER (ILL_CORRFN=-32)      ! illegal map correction function
       PARAMETER (ILL_GRADING=-33)     ! illegal map radial grading type
       PARAMETER (ILL_WEIGHT=-34)      ! illegal map vis. weighting type
       PARAMETER (ILL_POLCODE=-35)     ! illegal polarization code
       PARAMETER (ILL_EXREDT=-36)      ! invalid extra redtape
       PARAMETER (FLL_EXREDT=-37)      ! no room in extra redtape
       PARAMETER (NO_EXREDT=-38)       ! extra redtape not found
       PARAMETER (NO_EXTREC=-39)       ! redtape record not found
C
