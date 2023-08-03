!MNH_LIC Copyright 2002-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ######spl
    SUBROUTINE CONDENSATION(D, CST, ICEP, NEBN, TURBN, &
                           &HFRAC_ICE, HCONDENS, HLAMBDA3,                                                  &
                           &PPABS, PZZ, PRHODREF, PT, PRV_IN, PRV_OUT, PRC_IN, PRC_OUT, PRI_IN, PRI_OUT,    &
                           &PRR, PRS, PRG, PSIGS, LMFCONV, PMFCONV, PCLDFR, PSIGRC, OUSERI,                 &
                           &OSIGMAS, OCND2,                                                                 &
                           &PICLDFR, PWCLDFR, PSSIO, PSSIU, PIFR, PSIGQSAT,                                 &
                           &PLV, PLS, PCPH,                                                                 &
                           &PHLC_HRC, PHLC_HCF, PHLI_HRI, PHLI_HCF,                                         &
                           &PICE_CLD_WGT)
!   ################################################################################
!
!!
!!    PURPOSE
!!    -------
!!**  Routine to diagnose cloud fraction, liquid and ice condensate mixing ratios
!!    and s'rl'/sigs^2
!!
!!
!!**  METHOD
!!    ------
!!    Based on the large-scale fields of temperature, water vapor, and possibly
!!    liquid and solid condensate, the conserved quantities r_t and h_l are constructed
!!    and then fractional cloudiness, liquid and solid condensate is diagnosed.
!!
!!    The total variance is parameterized as the sum of  stratiform/turbulent variance
!!    and a convective variance.
!!    The turbulent variance is parameterized as a function of first-order moments, and
!!    the convective variance is modelled as a function of the convective mass flux
!!    (units kg/s m^2) as provided by the  mass flux convection scheme.
!!
!!    Nota: if the host model does not use prognostic values for liquid and solid condensate
!!    or does not provide a convective mass flux, put all these values to zero.
!!
!!
!!    EXTERNAL
!!    --------
!!      INI_CST
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST       : contains physical constants
!!
!!    REFERENCE
!!    ---------
!!      Chaboureau J.P. and P. Bechtold (J. Atmos. Sci. 2002)
!!
!!    AUTHOR
!!    ------
!!      P. BECHTOLD       * Laboratoire d'Aerologie *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original: 31.1.2002
!!     modified : 21.3.2002
!!     S.Malardel : 05.2006 : Correction sur le calcul de la fonction de
!!                                         Bougeault F2
!!     W. de Rooy: 06-06-2010: Modification in the statistical cloud scheme
!!                             more specifically adding a variance term
!!                             following ideas of Lenderink & Siebesma 2002
!!                             and adding a height dependence
!!     S. Riette, 18 May 2010 : PSIGQSAT is added
!!     S. Riette, 11 Oct 2011 : MIN function in PDF for continuity
!!                              modification of minimum value for Rc+Ri to create cloud and minimum value for sigma
!!                              Use of guess point as a starting point instead of liquid point
!!                              Better computation of ZCPH and dRsat/dT
!!                              Set ZCOND to zero if PCLDFR==0
!!                              Safety limitation to .99*Pressure for saturation vapour pressure
!!      2012-02 Y. Seity,  add possibility to run with reversed vertical levels
!!      2014-11 K.I Ivarsson add possibility to run with OCND2 option
!!      2016   S.Riette Change INQ1
!!      2016-11 S. Riette: use HFRAC_ICE, output adjusted state
!!      2018-02 K.I Ivarsson: Some modificatons of OCND2 option, mainly for optimation - new outputs
!!      2019-06 W.C. de Rooy: Mods for new set up statistical cloud scheme
!!      2019-07 K.I.Ivarsson: Switch for height dependent VQSIGSAT: LHGT_QS
!!      2020-12 U. Andrae : Introduce SPP for HARMONIE-AROME
!!     R. El Khatib 24-Aug-2021 Optimizations
!!      2021-01: SPP computations moved in aro_adjust (AROME/HARMONIE)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
USE MODD_DIMPHYEX,       ONLY: DIMPHYEX_t
USE MODD_CST,            ONLY: CST_t
USE MODD_RAIN_ICE_PARAM_n, ONLY: RAIN_ICE_PARAM_t
USE MODD_NEB_n,          ONLY: NEB_t
USE MODD_TURB_n,     ONLY: TURB_t
USE MODE_TIWMX,          ONLY : ESATW, ESATI
USE MODE_ICECLOUD,       ONLY : ICECLOUD
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
TYPE(DIMPHYEX_t),             INTENT(IN)    :: D
TYPE(CST_t),                  INTENT(IN)    :: CST
TYPE(RAIN_ICE_PARAM_t),       INTENT(IN)    :: ICEP
TYPE(NEB_t),                  INTENT(IN)    :: NEBN
TYPE(TURB_t),                 INTENT(IN)    :: TURBN
CHARACTER(LEN=1),             INTENT(IN)    :: HFRAC_ICE
CHARACTER(LEN=4),             INTENT(IN)    :: HCONDENS
CHARACTER(LEN=*),             INTENT(IN)    :: HLAMBDA3 ! formulation for lambda3 coeff
REAL,  INTENT(IN)    :: PPABS(D%NIJT,D%NKT)  ! pressure (Pa)
REAL,  INTENT(IN)    :: PZZ(D%NIJT,D%NKT)    ! height of model levels (m)
REAL,  INTENT(IN)    :: PRHODREF(D%NIJT,D%NKT)
REAL,  INTENT(INOUT) :: PT(D%NIJT,D%NKT)     ! grid scale T  (K)
REAL,  INTENT(IN)    :: PRV_IN(D%NIJT,D%NKT) ! grid scale water vapor mixing ratio (kg/kg) in input
REAL,  INTENT(OUT)   :: PRV_OUT(D%NIJT,D%NKT)! grid scale water vapor mixing ratio (kg/kg) in output
REAL,  INTENT(IN)    :: PRC_IN(D%NIJT,D%NKT) ! grid scale r_c mixing ratio (kg/kg) in input
REAL,  INTENT(OUT)   :: PRC_OUT(D%NIJT,D%NKT)! grid scale r_c mixing ratio (kg/kg) in output
REAL,  INTENT(IN)    :: PRI_IN(D%NIJT,D%NKT) ! grid scale r_i (kg/kg) in input
REAL,  INTENT(OUT)   :: PRI_OUT(D%NIJT,D%NKT)! grid scale r_i (kg/kg) in output
REAL,  INTENT(IN)    :: PRR(D%NIJT,D%NKT)    ! grid scale mixing ration of rain (kg/kg)
REAL,  INTENT(IN)    :: PRS(D%NIJT,D%NKT)    ! grid scale mixing ration of snow (kg/kg)
REAL,  INTENT(IN)    :: PRG(D%NIJT,D%NKT)    ! grid scale mixing ration of graupel (kg/kg)
REAL,  INTENT(IN)    :: PSIGS(D%NIJT,D%NKT)  ! Sigma_s from turbulence scheme
LOGICAL,                                                       INTENT(IN)    ::  LMFCONV ! =SIZE(PMFCONV)!=0
REAL,               INTENT(IN)    :: PMFCONV(MERGE(D%NIJT,0,LMFCONV),&
                MERGE(D%NKT,0,LMFCONV))! convective mass flux (kg /s m^2)
REAL,  INTENT(OUT)   :: PCLDFR(D%NIJT,D%NKT) ! cloud fraction
REAL,  INTENT(OUT)   :: PSIGRC(D%NIJT,D%NKT) ! s r_c / sig_s^2

LOGICAL, INTENT(IN)                         :: OUSERI ! logical switch to compute both
                                                      ! liquid and solid condensate (OUSERI=.TRUE.)
                                                      ! or only solid condensate (OUSERI=.FALSE.)
LOGICAL, INTENT(IN)                         :: OSIGMAS! use present global Sigma_s values
                                                      ! or that from turbulence scheme
LOGICAL, INTENT(IN)                         :: OCND2  ! logical switch to sparate liquid and ice
                                                      ! more rigid (DEFALT value : .FALSE.)
REAL,  INTENT(OUT)   :: PICLDFR(D%NIJT,D%NKT)  ! ice cloud fraction
REAL,  INTENT(OUT)   :: PWCLDFR(D%NIJT,D%NKT)  ! water or mixed-phase cloud fraction
REAL,  INTENT(OUT)   :: PSSIO(D%NIJT,D%NKT)    ! Super-saturation with respect to ice in the  
                                                              ! supersaturated fraction
REAL,  INTENT(OUT)   :: PSSIU(D%NIJT,D%NKT)    ! Sub-saturation with respect to ice in the  
                                                              ! subsaturated fraction
REAL,  INTENT(OUT)   :: PIFR(D%NIJT,D%NKT)     ! Ratio cloud ice moist part
REAL,        INTENT(IN)    :: PSIGQSAT ! use an extra "qsat" variance contribution (OSIGMAS case)
                                                              ! multiplied by PSIGQSAT

REAL,  OPTIONAL, INTENT(IN)    :: PLV(D%NIJT,D%NKT)    ! Latent heat L_v
REAL,  OPTIONAL, INTENT(IN)    :: PLS(D%NIJT,D%NKT)    ! Latent heat L_s
REAL,  OPTIONAL, INTENT(IN)    :: PCPH(D%NIJT,D%NKT)   ! Specific heat C_ph
REAL,  OPTIONAL, INTENT(OUT)   :: PHLC_HRC(D%NIJT,D%NKT)
REAL,  OPTIONAL, INTENT(OUT)   :: PHLC_HCF(D%NIJT,D%NKT) ! cloud fraction
REAL,  OPTIONAL, INTENT(OUT)   :: PHLI_HRI(D%NIJT,D%NKT)
REAL,  OPTIONAL, INTENT(OUT)   :: PHLI_HCF(D%NIJT,D%NKT)
REAL,        OPTIONAL, INTENT(IN)    :: PICE_CLD_WGT
!
!
!*       0.2   Declarations of local variables :
!
INTEGER :: JIJ, JK, JKP, JKM                    ! loop index
INTEGER :: IKTB, IKTE, IKB, IKE, IKL, IIJB, IIJE
REAL  :: ZTLK(D%NIJT,D%NKT), ZRT(D%NIJT,D%NKT)     ! work arrays for T_l and total water mixing ratio
REAL  :: ZL(D%NIJT,D%NKT)            ! length scale
INTEGER   :: ITPL            ! top levels of troposphere
REAL      :: ZTMIN           ! minimum Temp. related to ITPL
!
REAL  :: ZLV(D%NIJT,D%NKT), ZLS(D%NIJT,D%NKT), ZCPD(D%NIJT,D%NKT)
REAL :: ZGCOND, ZAUTC, ZAUTI, ZGAUV, ZGAUC, ZGAUI, ZGAUTC, ZGAUTI, ZCRIAUTI   ! Used for Gaussian PDF integration
REAL :: ZLVS                                      ! thermodynamics
REAL  :: ZPV, ZPIV, ZQSL, ZQSI ! thermodynamics
REAL :: ZLL, DZZ, ZZZ                           ! used for length scales
REAL :: ZAH, ZDRW, ZDTL, ZSIG_CONV                     ! related to computation of Sig_s
REAL  :: ZA, ZB, ZSBAR, ZSIGMA, ZQ1 ! related to computation of Sig_s
REAL  :: ZCOND
REAL  :: ZFRAC           ! Ice fraction
INTEGER  :: INQ1
REAL :: ZINC
! related to OCND2 noise check :
REAL :: ZRSP,  ZRSW, ZRFRAC, ZRSDIF, ZRCOLD
! related to OCND2  ice cloud calulation :
REAL  :: ESATW_T
REAL :: ZDUM1,ZDUM2,ZDUM3,ZDUM4,ZPRIFACT,ZLWINC
REAL  :: ZDZ, ZARDUM, ZARDUM2, ZCLDINI
! end OCND2

! LHGT_QS:
REAL :: ZDZFACT,ZDZREF
! LHGT_QS END

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
INTEGER :: IERR
!
!
!*       0.3  Definition of constants :
!
!-------------------------------------------------------------------------------
!
REAL,PARAMETER :: ZL0     = 600.        ! tropospheric length scale
REAL,PARAMETER :: ZCSIGMA = 0.2         ! constant in sigma_s parameterization
REAL,PARAMETER :: ZCSIG_CONV = 0.30E-2  ! scaling factor for ZSIG_CONV as function of mass flux
!

REAL, PARAMETER :: ZSRC_1D(-22:11) =(/                         &
       0.           ,  0.           ,  2.0094444E-04,   0.316670E-03,    &
       4.9965648E-04,  0.785956E-03 ,  1.2341294E-03,   0.193327E-02,    &
       3.0190963E-03,  0.470144E-02 ,  7.2950651E-03,   0.112759E-01,    &
       1.7350994E-02,  0.265640E-01 ,  4.0427860E-02,   0.610997E-01,    &
       9.1578111E-02,  0.135888E+00 ,  0.1991484    ,   0.230756E+00,    &
       0.2850565    ,  0.375050E+00 ,  0.5000000    ,   0.691489E+00,    &
       0.8413813    ,  0.933222E+00 ,  0.9772662    ,   0.993797E+00,    &
       0.9986521    ,  0.999768E+00 ,  0.9999684    ,   0.999997E+00,    &
       1.0000000    ,  1.000000     /)

JIJ=D%NIJB
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) THEN
  CALL DR_HOOK('CONDENSATION',0,ZHOOK_HANDLE)
END IF
!
IKTB=D%NKTB
IKTE=D%NKTE
IKB=D%NKB
IKE=D%NKE
IKL=D%NKL
IIJB=D%NIJB
IIJE=D%NIJE
!
DO JK=1,D%NKT
  PCLDFR(JIJ,JK) = 0. 

  END DO
! Initialize values
DO JK=1,D%NKT
  PSIGRC(JIJ,JK) = 0. 

  END DO
! Initialize values
DO JK=1,D%NKT
  PRV_OUT(JIJ,JK)= 0. 

  END DO
! Initialize values
DO JK=1,D%NKT
  PRC_OUT(JIJ,JK)= 0. 

  END DO
! Initialize values
DO JK=1,D%NKT
  PRI_OUT(JIJ,JK)= 0. 

  END DO
! Initialize values
ZPRIFACT = 1.    ! Initialize value
ZARDUM2 = 0.  ! Initialize values
ZCLDINI = -1. ! Dummy Initialized cloud input to icecloud routine
PIFR = 10. ! ratio of cloud ice water mixing ratio wet to dry
           ! part of a gridbox
ZDZREF = ICEP%XFRMIN(25) ! Thickness for unchanged vqsigsat (only used for LHGT_QS)
!
IF(OCND2)THEN
  ZPRIFACT = 0.
END IF
!
!
!-------------------------------------------------------------------------------
! store total water mixing ratio
DO JK=IKTB,IKTE
  ZRT(JIJ,JK)  = PRV_IN(JIJ,JK) + PRC_IN(JIJ,JK) + PRI_IN(JIJ,JK)*ZPRIFACT
  
    END DO
!-------------------------------------------------------------------------------
! Preliminary calculations
! latent heat of vaporisation/sublimation
IF(PRESENT(PLV) .AND. PRESENT(PLS)) THEN
  DO JK=1,D%NKT
  ZLV(JIJ,JK)=PLV(JIJ,JK)
  

  END DO
DO JK=1,D%NKT
  ZLS(JIJ,JK)=PLS(JIJ,JK)


  END DO
ELSE
  DO JK=IKTB,IKTE
    ! latent heat of vaporisation/sublimation
      ZLV(JIJ,JK) = CST%XLVTT + ( CST%XCPV - CST%XCL ) * ( PT(JIJ,JK) - CST%XTT )
      ZLS(JIJ,JK) = CST%XLSTT + ( CST%XCPV - CST%XCI ) * ( PT(JIJ,JK) - CST%XTT )
      
      ENDDO
ENDIF
IF(PRESENT(PCPH)) THEN
  DO JK=1,D%NKT
  ZCPD(JIJ,JK)=PCPH(JIJ,JK)


  END DO
ELSE
  DO JK=IKTB,IKTE
    ZCPD(JIJ,JK) = CST%XCPD + CST%XCPV*PRV_IN(JIJ,JK) + CST%XCL*PRC_IN(JIJ,JK) + CST%XCI*PRI_IN(JIJ,JK) + &
                                  CST%XCL*PRR(JIJ,JK) +  &
                                  CST%XCI*(PRS(JIJ,JK) + PRG(JIJ,JK) )
      
      ENDDO
ENDIF
! Preliminary calculations needed for computing the "turbulent part" of Sigma_s
IF ( .NOT. OSIGMAS ) THEN
  DO JK=IKTB,IKTE
    ! store temperature at saturation
      ZTLK(JIJ,JK) = PT(JIJ,JK) - ZLV(JIJ,JK)*PRC_IN(JIJ,JK)/ZCPD(JIJ,JK) &
                                    - ZLS(JIJ,JK)*PRI_IN(JIJ,JK)/ZCPD(JIJ,JK)*ZPRIFACT
      
      END DO
  ! Determine tropopause/inversion  height from minimum temperature
  ITPL  = IKB+IKL
  

  ZTMIN = 400.
  

  DO JK = IKTB+1,IKTE-1
    IF ( PT(JIJ,JK) < ZTMIN ) THEN
        ZTMIN = PT(JIJ,JK)
        ITPL = JK
      ENDIF
      
      END DO
  ! Set the mixing length scale
  ZL(JIJ,IKB) = 20.
  

  DO JK = IKB+IKL,IKE,IKL
    ! free troposphere
      ZL(JIJ,JK) = ZL0
      ZZZ =  PZZ(JIJ,JK) -  PZZ(JIJ,IKB)
      JKP = ITPL
      ! approximate length for boundary-layer
      IF ( ZL0 > ZZZ ) THEN
        ZL(JIJ,JK) = ZZZ
      END IF
      ! gradual decrease of length-scale near and above tropopause
      IF ( ZZZ > 0.9*(PZZ(JIJ,JKP)-PZZ(JIJ,IKB)) ) THEN
        ZL(JIJ,JK) = .6 * ZL(JIJ,JK-IKL)
      END IF
      
      END DO
END IF
!-------------------------------------------------------------------------------
!
DO JK=IKTB,IKTE
  JKP=MAX(MIN(JK+IKL,IKTE),IKTB)
  JKM=MAX(MIN(JK-IKL,IKTE),IKTB)
  IF (OCND2) THEN
     ZDZ = PZZ(JIJ,JKP) - PZZ(JIJ,JKP-IKL)
          
       CALL ICECLOUD(D,PPABS(JIJ,JK),PZZ(JIJ,JK),ZDZ, &
          & PT(JIJ,JK),PRV_IN(JIJ,JK),1.,-1., &
          & ZCLDINI,PIFR(IIJB,JK),PICLDFR(JIJ,JK), &
          & PSSIO(JIJ,JK),PSSIU(JIJ,JK),ZARDUM2,ZARDUM)
     ! latent heats
     ! saturated water vapor mixing ratio over liquid water and ice
     ESATW_T=ESATW(PT(JIJ,JK))
       ZPV  = MIN(ESATW_T, .99*PPABS(JIJ,JK))
       ZPIV = MIN(ESATI(PT(JIJ,JK)), .99*PPABS(JIJ,JK))
       
       ELSE
     ! latent heats
     ! saturated water vapor mixing ratio over liquid water and ice
    ZPV  = MIN(EXP( CST%XALPW - CST%XBETAW / PT(JIJ,JK) - CST%XGAMW * LOG( PT(JIJ,JK) ) ), .99*PPABS(JIJ,JK))
      ZPIV = MIN(EXP( CST%XALPI - CST%XBETAI / PT(JIJ,JK) - CST%XGAMI * LOG( PT(JIJ,JK) ) ), .99*PPABS(JIJ,JK))
      
      ENDIF
  !Ice fraction
  ZFRAC = 0.
  

  IF (OUSERI .AND. .NOT.OCND2) THEN
    IF (PRC_IN(JIJ,JK)+PRI_IN(JIJ,JK) > 1.E-20) THEN
        ZFRAC = PRI_IN(JIJ,JK) / (PRC_IN(JIJ,JK)+PRI_IN(JIJ,JK))
      ENDIF
        
      IF ( .TRUE. ) THEN
        IERR=0
      END IF
SELECT CASE(HFRAC_ICE)
  CASE ('T') !using Temperature
    ZFRAC = MAX( 0., MIN(1., (( NEBN%XTMAXMIX - PT(JIJ,JK) ) / ( NEBN%XTMAXMIX - NEBN%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    ZFRAC = MAX( 0., MIN(1., (( CST%XTT - PT(JIJ,JK) ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    ZFRAC = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    ZFRAC = MAX( 0., MIN(1., ZFRAC ) )
  CASE DEFAULT
    IF ( .TRUE. ) THEN
      IERR=1
    END IF
END SELECT


!error code IERR cannot be checked here to not break vectorization
      
      ENDIF
  ZQSL   = CST%XRD / CST%XRV * ZPV / ( PPABS(JIJ,JK) - ZPV )
    ZQSI   = CST%XRD / CST%XRV * ZPIV / ( PPABS(JIJ,JK) - ZPIV )

    ! interpolate between liquid and solid as function of temperature
    ZQSL = (1. - ZFRAC) * ZQSL + ZFRAC * ZQSI
    ZLVS = (1. - ZFRAC) * ZLV(JIJ,JK) + &
           & ZFRAC      * ZLS(JIJ,JK)

    ! coefficients a and b
    ZAH  = ZLVS * ZQSL / ( CST%XRV * PT(JIJ,JK)**2 ) * (CST%XRV * ZQSL / CST%XRD + 1.)
    ZA   = 1. / ( 1. + ZLVS/ZCPD(JIJ,JK) * ZAH )
    ZB   = ZAH * ZA
    ZSBAR = ZA * ( ZRT(JIJ,JK) - ZQSL + &
                 & ZAH * ZLVS * (PRC_IN(JIJ,JK)+PRI_IN(JIJ,JK)*ZPRIFACT) / ZCPD(JIJ,JK))
    
    ! switch to take either present computed value of SIGMAS
  ! or that of Meso-NH turbulence scheme
  IF ( OSIGMAS ) THEN
    IF (PSIGQSAT/=0.) THEN
        ZDZFACT = 1.
        IF(NEBN%LHGT_QS .AND. JK+1 <= IKTE)THEN
           ZDZFACT= MAX(ICEP%XFRMIN(23),MIN(ICEP%XFRMIN(24),(PZZ(JIJ,JK) - PZZ(JIJ,JK+1))/ZDZREF))
        ELSEIF(NEBN%LHGT_QS)THEN
           ZDZFACT= MAX(ICEP%XFRMIN(23),MIN(ICEP%XFRMIN(24),((PZZ(JIJ,JK-1) - PZZ(JIJ,JK)))*0.8/ZDZREF))
        ENDIF
        IF (NEBN%LSTATNW) THEN
          ZSIGMA = SQRT((PSIGS(JIJ,JK))**2 + (PSIGQSAT*ZDZFACT*ZQSL*ZA)**2)
        ELSE
          ZSIGMA = SQRT((2*PSIGS(JIJ,JK))**2 + (PSIGQSAT*ZQSL*ZA)**2)
        ENDIF
      ELSE
        IF (NEBN%LSTATNW) THEN
          ZSIGMA = PSIGS(JIJ,JK)
        ELSE
          ZSIGMA = 2*PSIGS(JIJ,JK)
        ENDIF
      END IF
      
      ELSE
    ! parameterize Sigma_s with first_order closure
      DZZ    =  PZZ(JIJ,JKP) - PZZ(JIJ,JKM)
      ZDRW   =  ZRT(JIJ,JKP) - ZRT(JIJ,JKM)
      ZDTL   =  ZTLK(JIJ,JKP) - ZTLK(JIJ,JKM) + CST%XG/ZCPD(JIJ,JK) * DZZ
      ZLL = ZL(JIJ,JK)
      ! standard deviation due to convection
      ZSIG_CONV =0.
      IF(LMFCONV) THEN
        ZSIG_CONV = ZCSIG_CONV * PMFCONV(JIJ,JK) / ZA
      END IF
      ! zsigma should be of order 4.e-4 in lowest 5 km of atmosphere
      ZSIGMA =  SQRT( MAX( 1.E-25, ZCSIGMA * ZCSIGMA * ZLL*ZLL/(DZZ*DZZ)*(&
           ZA*ZA*ZDRW*ZDRW - 2.*ZA*ZB*ZDRW*ZDTL + ZB*ZB*ZDTL*ZDTL) + &
           ZSIG_CONV * ZSIG_CONV ) )
      
      END IF
  ZSIGMA= MAX( 1.E-10, ZSIGMA )

    ! normalized saturation deficit
    ZQ1   = ZSBAR/ZSIGMA
    
    IF(HCONDENS == 'GAUS') THEN
    ! Gaussian Probability Density Function around ZQ1
      ! Computation of ZG and ZGAM(=erf(ZG))
      ZGCOND = -ZQ1/SQRT(2.)

      !Approximation of erf function for Gaussian distribution
      ZGAUV = 1 - SIGN(1., ZGCOND) * SQRT(1-EXP(-4*ZGCOND**2/CST%XPI))

      !Computation Cloud Fraction
      PCLDFR(JIJ,JK) = MAX( 0., MIN(1.,0.5*ZGAUV))

      !Computation of condensate
      ZCOND = (EXP(-ZGCOND**2)-ZGCOND*SQRT(CST%XPI)*ZGAUV)*ZSIGMA/SQRT(2.*CST%XPI)
      ZCOND = MAX(ZCOND, 0.)

      PSIGRC(JIJ,JK) = PCLDFR(JIJ,JK)
        
      !Computation warm/cold Cloud Fraction and content in high water content part
    IF(PRESENT(PHLC_HCF) .AND. PRESENT(PHLC_HRC))THEN
      IF(1-ZFRAC > 1.E-20)THEN
          ZAUTC = (ZSBAR - ICEP%XCRIAUTC/(PRHODREF(JIJ,JK)*(1-ZFRAC)))/ZSIGMA
          ZGAUTC = -ZAUTC/SQRT(2.)
          !Approximation of erf function for Gaussian distribution
          ZGAUC = 1 - SIGN(1., ZGAUTC) * SQRT(1-EXP(-4*ZGAUTC**2/CST%XPI))
          PHLC_HCF(JIJ,JK) = MAX( 0., MIN(1.,0.5*ZGAUC))
          PHLC_HRC(JIJ,JK) = (1-ZFRAC)*(EXP(-ZGAUTC**2)-ZGAUTC*SQRT(CST%XPI)*ZGAUC)*ZSIGMA/SQRT(2.*CST%XPI)
          PHLC_HRC(JIJ,JK) = PHLC_HRC(JIJ,JK) + ICEP%XCRIAUTC/PRHODREF(JIJ,JK) * PHLC_HCF(JIJ,JK)
          PHLC_HRC(JIJ,JK) = MAX(PHLC_HRC(JIJ,JK), 0.)
        ELSE
          PHLC_HCF(JIJ,JK)=0.
          PHLC_HRC(JIJ,JK)=0.
        ENDIF
          
        ENDIF

    IF(PRESENT(PHLI_HCF) .AND. PRESENT(PHLI_HRI))THEN
      IF(ZFRAC > 1.E-20)THEN
          ZCRIAUTI=MIN(ICEP%XCRIAUTI,10**(ICEP%XACRIAUTI*(PT(JIJ,JK)-CST%XTT)+ICEP%XBCRIAUTI))
          ZAUTI = (ZSBAR - ZCRIAUTI/ZFRAC)/ZSIGMA
          ZGAUTI = -ZAUTI/SQRT(2.)
          !Approximation of erf function for Gaussian distribution
          ZGAUI = 1 - SIGN(1., ZGAUTI) * SQRT(1-EXP(-4*ZGAUTI**2/CST%XPI))
          PHLI_HCF(JIJ,JK) = MAX( 0., MIN(1.,0.5*ZGAUI))
          PHLI_HRI(JIJ,JK) = ZFRAC*(EXP(-ZGAUTI**2)-ZGAUTI*SQRT(CST%XPI)*ZGAUI)*ZSIGMA/SQRT(2.*CST%XPI)
          PHLI_HRI(JIJ,JK) = PHLI_HRI(JIJ,JK) + ZCRIAUTI*PHLI_HCF(JIJ,JK)
          PHLI_HRI(JIJ,JK) = MAX(PHLI_HRI(JIJ,JK), 0.)
        ELSE
          PHLI_HCF(JIJ,JK)=0.
          PHLI_HRI(JIJ,JK)=0.
        ENDIF
          
        ENDIF

  ELSEIF(HCONDENS == 'CB02')THEN
    !Total condensate
      IF (ZQ1 > 0. .AND. ZQ1 <= 2) THEN
        ZCOND = MIN(EXP(-1.)+.66*ZQ1+.086*ZQ1**2, 2.) ! We use the MIN function for continuity
      ELSE IF (ZQ1 > 2.) THEN
        ZCOND = ZQ1
      ELSE
        ZCOND = EXP( 1.2*ZQ1-1. )
      ENDIF
      ZCOND = ZCOND * ZSIGMA

      !Cloud fraction
      IF (ZCOND < 1.E-12) THEN
        PCLDFR(JIJ,JK) = 0.
      ELSE
        PCLDFR(JIJ,JK) = MAX( 0., MIN(1.,0.5+0.36*ATAN(1.55*ZQ1)) )
      ENDIF
      IF (PCLDFR(JIJ,JK)==0.) THEN
        ZCOND=0.
      ENDIF

      INQ1 = MIN( MAX(-22,FLOOR(MIN(100., MAX(-100., 2*ZQ1))) ), 10)  !inner min/max prevents sigfpe when 2*zq1 does not fit into an int
      ZINC = 2.*ZQ1 - INQ1

      PSIGRC(JIJ,JK) =  MIN(1.,(1.-ZINC)*ZSRC_1D(INQ1)+ZINC*ZSRC_1D(INQ1+1))
        
      IF(PRESENT(PHLC_HCF) .AND. PRESENT(PHLC_HRC))THEN
      PHLC_HCF(JIJ,JK)=0.
      

  PHLC_HRC(JIJ,JK)=0.
    

  ENDIF
    IF(PRESENT(PHLI_HCF) .AND. PRESENT(PHLI_HRI))THEN
      PHLI_HCF(JIJ,JK)=0.
      

  PHLI_HRI(JIJ,JK)=0.
    

  ENDIF
  END IF !HCONDENS

  IF(.NOT. OCND2) THEN
    PRC_OUT(JIJ,JK) = (1.-ZFRAC) * ZCOND ! liquid condensate
      PRI_OUT(JIJ,JK) = ZFRAC * ZCOND   ! solid condensate
      PT(JIJ,JK) = PT(JIJ,JK) + ((PRC_OUT(JIJ,JK)-PRC_IN(JIJ,JK))*ZLV(JIJ,JK) + &
                                    &(PRI_OUT(JIJ,JK)-PRI_IN(JIJ,JK))*ZLS(JIJ,JK)   ) &
                                  & /ZCPD(JIJ,JK)
      PRV_OUT(JIJ,JK) = ZRT(JIJ,JK) - PRC_OUT(JIJ,JK) - PRI_OUT(JIJ,JK)*ZPRIFACT
      
      ELSE
    PRC_OUT(JIJ,JK) = (1.-ZFRAC) * ZCOND ! liquid condensate
      ZLWINC = PRC_OUT(JIJ,JK) - PRC_IN(JIJ,JK)
      !
!     This check is mainly for noise reduction :
!     -------------------------
      IF(ABS(ZLWINC)>1.0E-12  .AND.  ESATW(PT(JIJ,JK)) < PPABS(JIJ,JK)*0.5 )THEN
         ZRCOLD = PRC_OUT(JIJ,JK)
         ZRFRAC = PRV_IN(JIJ,JK) - ZLWINC
         IF( PRV_IN(JIJ,JK) < ZRSW )THEN ! sub - saturation over water:
            ! Avoid drying of cloudwater leading to supersaturation with
            ! respect to water
            ZRSDIF= MIN(0.,ZRSP-ZRFRAC)
         ELSE  ! super - saturation over water:
            ! Avoid deposition of water leading to sub-saturation with
            ! respect to water
            !            ZRSDIF= MAX(0.,ZRSP-ZRFRAC)
            ZRSDIF= 0. ! t7
         ENDIF
         PRC_OUT(JIJ,JK) = ZCOND  - ZRSDIF
      ELSE
        ZRCOLD = PRC_IN(JIJ,JK)
      ENDIF
 !    end check

 !    compute separate ice cloud:
      PWCLDFR(JIJ,JK) = PCLDFR(JIJ,JK)
      ZDUM1 = MIN(1.0,20.* PRC_OUT(JIJ,JK)*SQRT(ZDZ)/ZQSL) ! cloud liquid water factor
      ZDUM3 = MAX(0.,PICLDFR(JIJ,JK)-PWCLDFR(JIJ,JK)) ! pure ice cloud part
      IF (JK==IKTB) THEN
        ZDUM4 = PRI_IN(JIJ,JK)
      ELSE
        ZDUM4 = PRI_IN(JIJ,JK) + PRS(JIJ,JK)*0.5 + PRG(JIJ,JK)*0.25
      ENDIF

      ZDUM4 = MAX(0.,MIN(1.,PICE_CLD_WGT*ZDUM4*SQRT(ZDZ)/ZQSI)) ! clould ice+solid 
                                                         ! precip. water factor 

      ZDUM2 = (0.8*PCLDFR(JIJ,JK)+0.2)*MIN(1.,ZDUM1 + ZDUM4*PCLDFR(JIJ,JK))
      ! water cloud, use 'statistical' cloud, but reduce it in case of low liquid content

      PCLDFR(JIJ,JK) = MIN(1., ZDUM2 + (0.5*ZDUM3+0.5)*ZDUM4) ! Rad cloud
           ! Reduce ice cloud part in case of low ice water content
      PRI_OUT(JIJ,JK) = PRI_IN(JIJ,JK)
      PT(JIJ,JK) = PT(JIJ,JK) + ((PRC_OUT(JIJ,JK)-ZRCOLD)*ZLV(JIJ,JK) + &
                                    &(PRI_OUT(JIJ,JK)-PRI_IN(JIJ,JK))*ZLS(JIJ,JK)   ) &
                                  & /ZCPD(JIJ,JK)
      PRV_OUT(JIJ,JK) = ZRT(JIJ,JK) - PRC_OUT(JIJ,JK) - PRI_OUT(JIJ,JK)*ZPRIFACT
      
      END IF ! End OCND2
  IF(HLAMBDA3=='CB')THEN
    ! s r_c/ sig_s^2
      !    PSIGRC(JIJ,JK) = PCLDFR(JIJ,JK)  ! use simple Gaussian relation
      !
      !    multiply PSRCS by the lambda3 coefficient
      !
      !      PSIGRC(JIJ,JK) = 2.*PCLDFR(JIJ,JK) * MIN( 3. , MAX(1.,1.-ZQ1(JIJ)) )
      ! in the 3D case lambda_3 = 1.

      PSIGRC(JIJ,JK) = PSIGRC(JIJ,JK)* MIN( 3. , MAX(1.,1.-ZQ1) )
      
      END IF
END DO
!
IF (LHOOK) THEN
  CALL DR_HOOK('CONDENSATION',1,ZHOOK_HANDLE)
END IF
!
CONTAINS
!MNH_LIC Copyright 2006-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
      !
END SUBROUTINE CONDENSATION
