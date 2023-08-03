!MNH_LIC Copyright 2004-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ######spl
     MODULE MODE_COMPUTE_UPDRAFT
!    ###########################
!
IMPLICIT NONE
CONTAINS
      SUBROUTINE COMPUTE_UPDRAFT(D,CST,NEBN,PARAMMF,TURBN,CSTURB, &
                                 KSV,                             &
                                 OENTR_DETR,                      &
                                 ONOMIXLG,KSV_LGBEG,KSV_LGEND,    &
                                 PZZ,PDZZ,                        &
                                 PSFTH,PSFRV,                     &
                                 PPABSM,PRHODREF,PUM,PVM, PTKEM,  &
                                 PTHM,PRVM,PTHLM,PRTM,            &
                                 PSVM,PTHL_UP,PRT_UP,             &
                                 PRV_UP,PRC_UP,PRI_UP,PTHV_UP,    &
                                 PW_UP,PU_UP, PV_UP, PSV_UP,      &
                                 PFRAC_UP,PFRAC_ICE_UP,PRSAT_UP,  &
                                 PEMF,PDETR,PENTR,                &
                                 PBUO_INTEG,KKLCL,KKETL,KKCTL,    &
                                 PDEPTH, PDX, PDY     )

!     #################################################################
!!
!!****  *COMPUTE_UPDRAFT* - calculates caracteristics of the updraft 
!!                         
!!
!!    PURPOSE
!!    -------
!!****  The purpose of this routine is to build the updraft model 
!!
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      !!     REFERENCE
!!     ---------
!!       Book 1 of Meso-NH documentation (chapter Turbulence)
!!       Soares et al. 2004 QJ
!!
!!     AUTHOR
!!     ------
!!     J.Pergaud
!!     V.Masson : Optimization 07/2010
!!     S. Riette : 07/2010 : modification for reproducibility  
!!     S. Riette may 2011: ice added, interface modified
!!     S. Riette Jan 2012: support for both order of vertical levels
!!     V.Masson, C.Lac : 02/2011 : SV_UP initialized by a non-zero value
!!     S. Riette Apr 2013: improvement of continuity at the condensation level
!!     R.Honnert Oct 2016 : Add ZSURF and Update with AROME
!!     Q.Rodier  01/2019 : support RM17 mixing length
!!     R.Honnert 01/2019 : add LGZ (reduction of the mass-flux surface closure with the resolution)
!!     S. Riette 06/2022: compute_entr_detr is inlined
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_DIMPHYEX,        ONLY: DIMPHYEX_t
USE MODD_CST,             ONLY: CST_t
USE MODD_NEB_n,           ONLY: NEB_t
USE MODD_PARAM_MFSHALL_n, ONLY: PARAM_MFSHALL_t
USE MODD_TURB_n,          ONLY: TURB_t
USE MODD_CTURB,           ONLY: CSTURB_t
!
USE MODI_SHUMAN_MF, ONLY: MZM_MF, MZF_MF, GZ_M_W_MF

USE MODE_COMPUTE_BL89_ML, ONLY: COMPUTE_BL89_ML
USE MODE_MSG, ONLY: PRINT_MSG, NVERB_FATAL
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK

IMPLICIT NONE

!*                    1.1  Declaration of Arguments
!
!
!
TYPE(DIMPHYEX_t),       INTENT(IN)   :: D
TYPE(CST_t),            INTENT(IN)   :: CST
TYPE(NEB_t),            INTENT(IN)   :: NEBN
TYPE(PARAM_MFSHALL_t),  INTENT(IN)   :: PARAMMF
TYPE(TURB_t),           INTENT(IN)   :: TURBN
TYPE(CSTURB_t),         INTENT(IN)   :: CSTURB
INTEGER,                INTENT(IN)   :: KSV
LOGICAL,                INTENT(IN) :: OENTR_DETR! flag to recompute entrainment, detrainment and mass flux
LOGICAL,                INTENT(IN)   :: ONOMIXLG  ! False if mixing of lagrangian tracer
INTEGER,                INTENT(IN)   :: KSV_LGBEG ! first index of lag. tracer
INTEGER,                INTENT(IN)   :: KSV_LGEND ! last  index of lag. tracer
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)   :: PZZ       !  Height at the flux point
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)   :: PDZZ      !  Metrics coefficient
 
REAL, DIMENSION(D%NIJT),   INTENT(IN)   ::  PSFTH,PSFRV
! normal surface fluxes of theta,rv,(u,v) parallel to the orography
!
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PPABSM     ! Pressure at t-dt
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PRHODREF   ! dry density of the
                                                  ! reference state
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PUM        ! u mean wind
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PVM        ! v mean wind
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN) ::  PTKEM      ! TKE at t-dt
!
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN)   ::  PTHM           ! liquid pot. temp. at t-dt
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN)   ::  PRVM           ! vapor mixing ratio at t-dt
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(IN)   ::  PTHLM,PRTM     ! cons. var. at t-dt

REAL, DIMENSION(D%NIJT,D%NKT,KSV), INTENT(IN)   ::  PSVM           ! scalar var. at t-dt

REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(OUT)  ::  PTHL_UP,PRT_UP   ! updraft properties
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(OUT)  ::  PU_UP, PV_UP     ! updraft wind components
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(INOUT)::  PRV_UP,PRC_UP, & ! updraft rv, rc
                                         PRI_UP,PTHV_UP,& ! updraft ri, THv
                                         PW_UP,PFRAC_UP,& ! updraft w, fraction
                                         PFRAC_ICE_UP,&   ! liquid/solid fraction in updraft
                                         PRSAT_UP         ! Rsat

REAL, DIMENSION(D%NIJT,D%NKT,KSV), INTENT(OUT)  ::  PSV_UP           ! updraft scalar var. 
                                         
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(INOUT)::  PEMF,PDETR,PENTR ! Mass_flux,
                                                          ! detrainment,entrainment
REAL, DIMENSION(D%NIJT,D%NKT),   INTENT(INOUT) :: PBUO_INTEG       ! Integrated Buoyancy 
INTEGER, DIMENSION(D%NIJT),  INTENT(INOUT) :: KKLCL,KKETL,KKCTL! LCL, ETL, CTL
REAL, DIMENSION(D%NIJT),     INTENT(OUT)   :: PDEPTH           ! Deepness of cloud
REAL,                   INTENT(IN)    :: PDX, PDY
!                       1.2  Declaration of local variables
!
!
! Mean environment variables at t-dt at flux point
REAL, DIMENSION(D%NIJT,D%NKT) ::    &
                        ZTHM_F,ZRVM_F                 ! Theta,rv of
                                                      ! updraft environnement
REAL, DIMENSION(D%NIJT,D%NKT) ::    &
                        ZRTM_F, ZTHLM_F, ZTKEM_F,&    ! rt, thetal,TKE,pressure,
                        ZUM_F,ZVM_F,ZRHO_F,      &    ! density,momentum
                        ZPRES_F,ZTHVM_F,ZTHVM,   &    ! interpolated at the flux point
                        ZG_O_THVREF,             &    ! g*ThetaV ref
                        ZW_UP2,                  &    ! w**2  of the updraft
                        ZBUO_INTEG_DRY, ZBUO_INTEG_CLD,&! Integrated Buoyancy
                        ZENTR_CLD,ZDETR_CLD           ! wet entrainment and detrainment

REAL, DIMENSION(D%NIJT,D%NKT,KSV) :: &
                        ZSVM_F ! scalar variables 

                        
REAL, DIMENSION(D%NIJT,D%NKT) ::  &
                        ZTH_UP,                  &    ! updraft THETA 
                        ZRC_MIX, ZRI_MIX              ! guess of Rc and Ri for KF mixture

REAL, DIMENSION(D%NIJT,D%NKT) ::  ZCOEF  ! diminution coefficient for too high clouds 
                        
REAL, DIMENSION(D%NIJT)            ::  ZWTHVSURF  ! Surface w'thetav'

REAL  :: ZRDORV       ! RD/RV
REAL  :: ZRVORD       ! RV/RD


REAL, DIMENSION(D%NIJT) :: ZMIX1,ZMIX2,ZMIX3_CLD,ZMIX2_CLD

REAL, DIMENSION(D%NIJT) :: ZLUP         ! Upward Mixing length from the ground

INTEGER  :: JK,JIJ,JSV          ! loop counters

LOGICAL, DIMENSION(D%NIJT) ::  GTEST,GTESTLCL,GTESTETL
                               ! Test if the ascent continue, if LCL or ETL is reached
LOGICAL                          ::  GLMIX 
                               ! To choose upward or downward mixing length
LOGICAL, DIMENSION(D%NIJT)              :: GWORK1
LOGICAL, DIMENSION(D%NIJT,D%NKT) :: GWORK2

INTEGER  :: ITEST

REAL, DIMENSION(D%NIJT) :: ZRC_UP, ZRI_UP, ZRV_UP,&
                                 ZRSATW, ZRSATI,&
                                 ZPART_DRY

REAL  :: ZDEPTH_MAX1, ZDEPTH_MAX2 ! control auto-extinction process

REAL  :: ZTMAX,ZRMAX  ! control value

REAL, DIMENSION(D%NIJT) :: ZSURF
REAL, DIMENSION(D%NIJT,D%NKT) :: ZSHEAR,ZDUDZ,ZDVDZ ! vertical wind shear
!
REAL, DIMENSION(D%NIJT,D%NKT) :: ZWK
REAL, DIMENSION(D%NIJT,16) :: ZBUF
!
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
!
!                       1.3  Declaration of additional local variables for compute_entr_detr
!
! Variables for cloudy part
REAL, DIMENSION(D%NIJT) :: ZKIC, ZKIC_F2  ! fraction of env. mass in the muxtures
REAL, DIMENSION(D%NIJT) :: ZEPSI,ZDELTA   ! factor entrainment detrainment
REAL                   :: ZEPSI_CLOUD    ! factor entrainment detrainment
REAL                   :: ZCOEFFMF_CLOUD ! factor for compputing entr. detr.
REAL, DIMENSION(D%NIJT) :: ZMIXTHL,ZMIXRT ! Thetal and rt in the mixtures
REAL, DIMENSION(D%NIJT) :: ZTHMIX         ! Theta and Thetav  of mixtures
REAL, DIMENSION(D%NIJT) :: ZRVMIX,ZRCMIX,ZRIMIX ! mixing ratios in mixtures
REAL, DIMENSION(D%NIJT) :: ZTHVMIX, ZTHVMIX_F2 ! Theta and Thetav  of mixtures
REAL, DIMENSION(D%NIJT) :: ZTHV_UP_F2     ! thv_up at flux point kk+kkl
REAL, DIMENSION(D%NIJT) :: ZRSATW_ED, ZRSATI_ED ! working arrays (mixing ratio at saturation)
REAL, DIMENSION(D%NIJT) :: ZTHV           ! theta V of environment at the bottom of cloudy part  
REAL                   :: ZKIC_INIT      !Initial value of ZKIC
REAL                   :: ZCOTHVU              ! Variation of Thvup between bottom and top of cloudy part

! Variables for dry part
REAL                   :: ZFOESW, ZFOESI       ! saturating vapor pressure
REAL                   :: ZDRSATODP            ! d.Rsat/dP
REAL                   :: ZT                   ! Temperature
REAL                   :: ZWK0D                ! Work array

! Variables for dry and cloudy parts
REAL, DIMENSION(D%NIJT) :: ZCOEFF_MINUS_HALF,&  ! Variation of Thv between mass points kk-kkl and kk
                                  ZCOEFF_PLUS_HALF     ! Variation of Thv between mass points kk and kk+kkl
REAL, DIMENSION(D%NIJT) :: ZPRE                 ! pressure at the bottom of the cloudy part
REAL, DIMENSION(D%NIJT) :: ZG_O_THVREF_ED
REAL, DIMENSION(D%NIJT) :: ZFRAC_ICE            ! fraction of ice
REAL, DIMENSION(D%NIJT) :: ZDZ_STOP,&           ! Exact Height of the LCL above flux level KK
                          ZTHV_MINUS_HALF,&    ! Thv at flux point(kk)  
                          ZTHV_PLUS_HALF       ! Thv at flux point(kk+kkl)
REAL                   :: ZDZ                  ! Delta Z used in computations
INTEGER :: JKLIM
INTEGER :: IIJB,IIJE ! physical horizontal domain indices
INTEGER :: IKT,IKB,IKE,IKL
INTEGER, PARAMETER :: IEXN=1, IRVSAT=2, ICPH=3, IRLTEMP=4, ICPH2=5, IT=6, ILVOCPEXN=7, ILSOCPEXN=8, &
                    & IDRSATODT=9, IDRSATODTW=10, IDRSATODTI=11, IFOESW=12, IFOESI=13, &
                    & ILOGT=14, I99PP=15, I1PRT=16
INTEGER :: II
INTEGER :: JITER
REAL :: ZVAR1
REAL :: ZVAR2
REAL :: ZTPOW2
REAL :: ZDELT
!
IF (LHOOK) CALL DR_HOOK('COMPUTE_UPDRAFT',0,ZHOOK_HANDLE)
!
IIJE=D%NIJE
IIJB=D%NIJB
IKT=D%NKT
IKB=D%NKB
IKE=D%NKE
IKL=D%NKL
!
! Thresholds for the  perturbation of
! theta_l and r_t at the first level of the updraft
ZTMAX=2.0
ZRMAX=1.E-3
!------------------------------------------------------------------------

!                     INITIALISATION

! Initialisation of the constants   
ZRDORV   = CST%XRD / CST%XRV   !=0.622
ZRVORD   = (CST%XRV / CST%XRD) 

ZDEPTH_MAX1=3000. ! clouds with depth inferior to this value are keeped untouched
ZDEPTH_MAX2=4000. ! clouds with depth superior to this value are suppressed

!                 Local variables, internal domain

IF (OENTR_DETR) THEN
  ! Initialisation of intersesting Level :LCL,ETL,CTL
  KKLCL(:)=IKE
  KKETL(:)=IKE
  KKCTL(:)=IKE

  !
  ! Initialisation
  !* udraft governing variables
  PEMF(:,:)=0.
  PDETR(:,:)=0.
  PENTR(:,:)=0.

  ! Initialisation
  !* updraft core variables
  PRV_UP(:,:)=0.
  PRC_UP(:,:)=0.
  PRI_UP(:,:)=0.
  PW_UP(:,:)=0.
  ZTH_UP(:,:)=0.
  PFRAC_UP(:,:)=0.
  PTHV_UP(:,:)=0.

  PBUO_INTEG=0.

  PFRAC_ICE_UP(:,:)=0.
  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  PRSAT_UP(IIJB:IIJE,1:IKT)=PRVM(IIJB:IIJE,1:IKT) ! should be initialised correctly but is (normaly) not used
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

  !cloud/dry air mixture cloud content
  ZRC_MIX = 0.
  ZRI_MIX = 0.

END IF

! Initialisation of environment variables at t-dt
! variables at flux level
CALL MZM_MF(D, PTHLM(:,:), ZTHLM_F(:,:))
CALL MZM_MF(D, PRTM(:,:), ZRTM_F (:,:))
CALL MZM_MF(D, PUM(:,:), ZUM_F  (:,:))
CALL MZM_MF(D, PVM(:,:), ZVM_F  (:,:))
CALL MZM_MF(D, PTKEM(:,:), ZTKEM_F(:,:))

DO JSV=1,KSV
  IF (ONOMIXLG .AND. JSV >= KSV_LGBEG .AND. JSV<= KSV_LGEND) CYCLE
  CALL MZM_MF(D, PSVM(:,:,JSV), ZSVM_F(:,:,JSV))
END DO
!                     
!          Initialisation of updraft characteristics 
!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
PTHL_UP(IIJB:IIJE,1:IKT)=ZTHLM_F(IIJB:IIJE,1:IKT)
PRT_UP(IIJB:IIJE,1:IKT)=ZRTM_F(IIJB:IIJE,1:IKT)
PU_UP(IIJB:IIJE,1:IKT)=ZUM_F(IIJB:IIJE,1:IKT)
PV_UP(IIJB:IIJE,1:IKT)=ZVM_F(IIJB:IIJE,1:IKT)
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
!$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT,JSV=1:KSV)
PSV_UP(IIJB:IIJE,1:IKT,:)=ZSVM_F(IIJB:IIJE,1:IKT,:)
!$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT,JSV=1:KSV)

! Computation or initialisation of updraft characteristics at the KKB level
! thetal_up,rt_up,thetaV_up, w2,Buoyancy term and mass flux (PEMF)
!$mnh_expand_array(JIJ=IIJB:IIJE)
PTHL_UP(IIJB:IIJE,IKB)= ZTHLM_F(IIJB:IIJE,IKB)+ &
                            & MAX(0.,MIN(ZTMAX,(PSFTH(IIJB:IIJE)/SQRT(ZTKEM_F(IIJB:IIJE,IKB)))* PARAMMF%XALP_PERT))
PRT_UP(IIJB:IIJE,IKB) = ZRTM_F(IIJB:IIJE,IKB)+ &
                            & MAX(0.,MIN(ZRMAX,(PSFRV(IIJB:IIJE)/SQRT(ZTKEM_F(IIJB:IIJE,IKB)))* PARAMMF%XALP_PERT)) 
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

IF (OENTR_DETR) THEN
  CALL MZM_MF(D, PTHM (:,:), ZTHM_F (:,:))
  CALL MZM_MF(D, PPABSM(:,:), ZPRES_F(:,:))
  CALL MZM_MF(D, PRHODREF(:,:), ZRHO_F (:,:))
  CALL MZM_MF(D, PRVM(:,:), ZRVM_F (:,:))

  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ! thetav at mass and flux levels
  ZTHVM_F(IIJB:IIJE,1:IKT)=ZTHM_F(IIJB:IIJE,1:IKT)* &
                                    &((1.+ZRVORD*ZRVM_F(IIJB:IIJE,1:IKT))/(1.+ZRTM_F(IIJB:IIJE,1:IKT)))
  ZTHVM(IIJB:IIJE,1:IKT)=PTHM(IIJB:IIJE,1:IKT)* &
                                    &((1.+ZRVORD*PRVM(IIJB:IIJE,1:IKT))/(1.+PRTM(IIJB:IIJE,1:IKT)))

  PTHV_UP(IIJB:IIJE,1:IKT)=ZTHVM_F(IIJB:IIJE,1:IKT)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

  ZW_UP2(:,:)=0.
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZW_UP2(IIJB:IIJE,IKB) = MAX(0.0001,(2./3.)*ZTKEM_F(IIJB:IIJE,IKB))

  ! Computation of non conservative variable for the KKB level of the updraft
  ! (all or nothing ajustement)
  PRC_UP(:,IKB)=0.
  PRI_UP(:,IKB)=0.
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  IIJB=MERGE(D%NIJB, 1,  .TRUE. )
IIJE=MERGE(D%NIJE, D%NIJT,  .TRUE. )
!Number of iterations
JITER=2
!
!Computation of PBUF(IIJB:IIJE, ICPH2) depending on dummy arguments received
ZBUF(IIJB:IIJE, ICPH2)=0
!
!Computation of an approximate state thanks to PRL and PRI guess
ZBUF(IIJB:IIJE, IEXN)=(ZPRES_F(IIJB:IIJE,IKB)/CST%XP00) ** CST%RDSCPD

DO JIJ=IIJB,IIJE
  ZBUF(JIJ, I99PP)=0.99*ZPRES_F(JIJ,IKB)
  PRV_UP(JIJ,IKB)=PRT_UP(JIJ,IKB)-PRC_UP(JIJ,IKB)-PRI_UP(JIJ,IKB)
  ZBUF(JIJ, ICPH)=CST%XCPD+ CST%XCPV * PRV_UP(JIJ,IKB)+ CST%XCL * PRC_UP(JIJ,IKB) + CST%XCI * PRI_UP(JIJ,IKB) + ZBUF(JIJ, ICPH2)
  ZVAR2=ZBUF(JIJ, ICPH)*ZBUF(JIJ, IEXN)
  ZDELT=(PTHL_UP(JIJ,IKB)*ZBUF(JIJ, IEXN))-CST%XTT
  ZBUF(JIJ, ILVOCPEXN) = (CST%XLVTT + (CST%XCPV-CST%XCL) * ZDELT) /ZVAR2
  ZBUF(JIJ, ILSOCPEXN) = (CST%XLSTT + (CST%XCPV-CST%XCI) * ZDELT) /ZVAR2 
  ZTH_UP(JIJ,IKB)=PTHL_UP(JIJ,IKB)+ZBUF(JIJ, ILVOCPEXN)*PRC_UP(JIJ,IKB)+ZBUF(JIJ, ILSOCPEXN)*PRI_UP(JIJ,IKB)
  ZBUF(JIJ, I1PRT)=1+PRT_UP(JIJ,IKB)
ENDDO
!
!
!       2 Iteration
!         ---------

DO II=1,JITER
  IF (.FALSE.) THEN
    ZBUF(IIJB:IIJE, IT)=ZTH_UP(IIJB:IIJE,IKB)
  ELSE
    ZBUF(IIJB:IIJE, IT)=ZTH_UP(IIJB:IIJE,IKB)*ZBUF(IIJB:IIJE, IEXN)
  END IF
  !Computation of liquid/ice fractions
  PFRAC_ICE_UP(IIJB:IIJE,IKB) = 0.
  DO JIJ=IIJB, IIJE
    IF(PRC_UP(JIJ,IKB)+PRI_UP(JIJ,IKB) > 1.E-20) THEN
      PFRAC_ICE_UP(JIJ,IKB) = PRI_UP(JIJ,IKB) / (PRC_UP(JIJ,IKB)+PRI_UP(JIJ,IKB))
    ENDIF
  ENDDO
  DO JIJ=IIJB,IIJE
  SELECT CASE(NEBN%CFRAC_ICE_SHALLOW_MF)
  CASE ('T') !using Temperature
    PFRAC_ICE_UP(JIJ,IKB) = MAX( 0., MIN(1., (( NEBN%XTMAXMIX - ZBUF(JIJ,IT) ) / ( NEBN%XTMAXMIX - NEBN%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    PFRAC_ICE_UP(JIJ,IKB) = MAX( 0., MIN(1., (( CST%XTT - ZBUF(JIJ,IT) ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    PFRAC_ICE_UP(JIJ,IKB) = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    PFRAC_ICE_UP(JIJ,IKB) = MAX( 0., MIN(1., PFRAC_ICE_UP(JIJ,IKB) ) )
  CASE DEFAULT
    END SELECT


END DO

!Computation of Rvsat and dRsat/dT
  !In this version QSAT, QSATI, DQSAT and DQASATI functions are not used
  !due to performance issue

  ! Log does not vectorize on all compilers:
  ZBUF(IIJB:IIJE, ILOGT)=LOG(ZBUF(IIJB:IIJE, IT))

  DO JIJ=IIJB, IIJE
    ZBUF(JIJ, IFOESW) = MIN(EXP( CST%XALPW - CST%XBETAW/ZBUF(JIJ, IT) - CST%XGAMW*ZBUF(JIJ, ILOGT)  ), ZBUF(JIJ, I99PP))
    ZBUF(JIJ, IFOESI) = MIN(EXP( CST%XALPI - CST%XBETAI/ZBUF(JIJ, IT) - CST%XGAMI*ZBUF(JIJ, ILOGT)  ), ZBUF(JIJ, I99PP))
    ZRSATW(JIJ) = CST%XRD/CST%XRV*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,IKB) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,IKB))
    ZRSATI(JIJ) = CST%XRD/CST%XRV*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,IKB) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,IKB))
    ZTPOW2=ZBUF(JIJ, IT)**2
    ZBUF(JIJ, IDRSATODTW) = ZRSATW(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,IKB) ) &
                     * (CST%XBETAW/ZTPOW2 - CST%XGAMW/ZBUF(JIJ, IT))*ZBUF(JIJ, I1PRT)
    ZBUF(JIJ, IDRSATODTI) = ZRSATI(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,IKB) ) &
                     * (CST%XBETAI/ZTPOW2 - CST%XGAMI/ZBUF(JIJ, IT))*ZBUF(JIJ, I1PRT)
    !PRSATW(JIJ) =  QSAT(PBUF(JIJ, IT),PP(JIJ)) !qsatw
    !PRSATI(JIJ) = QSATI(PBUF(JIJ, IT),PP(JIJ)) !qsati
    !PBUF(JIJ, IDRSATODTW) =  DQSAT(PBUF(JIJ, IT),PP(JIJ),PRSATW(JIJ))*PBUF(JIJ, I1PRT)
    !PBUF(JIJ, IDRSATODTI) = DQSATI(PBUF(JIJ, IT),PP(JIJ),PRSATI(JIJ))*PBUF(JIJ, I1PRT)
    ZRSATW(JIJ) = ZRSATW(JIJ)*ZBUF(JIJ, I1PRT)
    ZRSATI(JIJ) = ZRSATI(JIJ)*ZBUF(JIJ, I1PRT)
    ZBUF(JIJ, IRVSAT) = ZRSATW(JIJ)*(1-PFRAC_ICE_UP(JIJ,IKB)) + ZRSATI(JIJ)*PFRAC_ICE_UP(JIJ,IKB)
    ZBUF(JIJ, IDRSATODT) = (ZBUF(JIJ, IDRSATODTW)*(1-PFRAC_ICE_UP(JIJ,IKB))+ &
              & ZBUF(JIJ, IDRSATODTI)*PFRAC_ICE_UP(JIJ,IKB))

    !Computation of new PRL, PRI and PRV
    !Correction term applied to (PRV(JIJ)-PBUF(JIJ, IRVSAT)) is computed assuming that
    !PBUF(JIJ, ILVOCPEXN), PBUF(JIJ, ILSOCPEXN) and PBUF(JIJ, ICPH) don't vary to much with T. It takes into account
    !the variation (estimated linear) of Qsat with T
    ZBUF(JIJ, IRLTEMP)=(PRV_UP(JIJ,IKB)-ZBUF(JIJ, IRVSAT))/ &
                  &(1 + ZBUF(JIJ, IDRSATODT)*ZBUF(JIJ, IEXN)* &
                  &     (ZBUF(JIJ, ILVOCPEXN)*(1-PFRAC_ICE_UP(JIJ,IKB))+ZBUF(JIJ, ILSOCPEXN)*PFRAC_ICE_UP(JIJ,IKB)))
    ZBUF(JIJ, IRLTEMP)=MIN(MAX(-PRC_UP(JIJ,IKB)-PRI_UP(JIJ,IKB), ZBUF(JIJ, IRLTEMP)),PRV_UP(JIJ,IKB))
    PRV_UP(JIJ,IKB)=PRV_UP(JIJ,IKB)-ZBUF(JIJ, IRLTEMP)
    PRC_UP(JIJ,IKB)=PRC_UP(JIJ,IKB)+PRI_UP(JIJ,IKB)+ZBUF(JIJ, IRLTEMP)
    PRI_UP(JIJ,IKB)=PFRAC_ICE_UP(JIJ,IKB)     * (PRC_UP(JIJ,IKB))
    PRC_UP(JIJ,IKB)=(1-PFRAC_ICE_UP(JIJ,IKB)) * (PRT_UP(JIJ,IKB) - PRV_UP(JIJ,IKB))

    !Computation of Cph (as defined in Meso-NH doc, equation 2.2, to be used with mixing ratios)
    ZBUF(JIJ, ICPH)=CST%XCPD+ CST%XCPV * PRV_UP(JIJ,IKB)+ CST%XCL * PRC_UP(JIJ,IKB) + CST%XCI * PRI_UP(JIJ,IKB) + ZBUF(JIJ, ICPH2)

    !Computation of L/Cph/EXN, then new PTH
    ZVAR2=ZBUF(JIJ, ICPH)*ZBUF(JIJ, IEXN)
    ZBUF(JIJ, ILVOCPEXN) = (CST%XLVTT + (CST%XCPV-CST%XCL) * (ZBUF(JIJ, IT)-CST%XTT)) /ZVAR2
    ZBUF(JIJ, ILSOCPEXN) = (CST%XLSTT + (CST%XCPV-CST%XCI) * (ZBUF(JIJ, IT)-CST%XTT)) /ZVAR2
    ZTH_UP(JIJ,IKB)=PTHL_UP(JIJ,IKB)+ZBUF(JIJ, ILVOCPEXN)*PRC_UP(JIJ,IKB)+ZBUF(JIJ, ILSOCPEXN)*PRI_UP(JIJ,IKB)

    !Computation of estimated mixing ration at saturation
    !To compute the adjustement a first order development was used
    ZVAR1=ZTH_UP(JIJ,IKB)*ZBUF(JIJ, IEXN)-ZBUF(JIJ, IT)
    ZRSATW(JIJ)=ZRSATW(JIJ) + ZBUF(JIJ, IDRSATODTW)*ZVAR1
    ZRSATI(JIJ)=ZRSATI(JIJ) + ZBUF(JIJ, IDRSATODTI)*ZVAR1
  ENDDO
ENDDO


!$mnh_expand_array(JIJ=IIJB:IIJE)
  ! compute updraft thevav and buoyancy term at KKB level
  PTHV_UP(IIJB:IIJE,IKB) = ZTH_UP(IIJB:IIJE,IKB)*&
                               & ((1+ZRVORD*PRV_UP(IIJB:IIJE,IKB))/(1+PRT_UP(IIJB:IIJE,IKB)))
  ! compute mean rsat in updraft
  PRSAT_UP(IIJB:IIJE,IKB) = ZRSATW(IIJB:IIJE)*(1-PFRAC_ICE_UP(IIJB:IIJE,IKB)) + &
                              & ZRSATI(IIJB:IIJE)*PFRAC_ICE_UP(IIJB:IIJE,IKB)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  ! Closure assumption for mass flux at KKB level
  !

  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ZG_O_THVREF(IIJB:IIJE,1:IKT)=CST%XG/ZTHVM_F(IIJB:IIJE,1:IKT)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

  ! compute L_up
  GLMIX=.TRUE.
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZTKEM_F(IIJB:IIJE,IKB)=0.
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  !
  IF(TURBN%CTURBLEN=='RM17') THEN
    CALL GZ_M_W_MF(D, PUM, PDZZ, ZWK)
    CALL MZF_MF(D, ZWK, ZDUDZ)
    CALL GZ_M_W_MF(D, PVM, PDZZ, ZWK)
    CALL MZF_MF(D, ZWK, ZDVDZ)
    !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
    ZSHEAR(IIJB:IIJE,1:IKT) = SQRT(ZDUDZ(IIJB:IIJE,1:IKT)**2 + ZDVDZ(IIJB:IIJE,1:IKT)**2)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  ELSE
    ZSHEAR = 0. !no shear in bl89 mixing length
  END IF
  !
  CALL COMPUTE_BL89_ML(D, CST, CSTURB, PDZZ,ZTKEM_F(:,IKB),&
                      &ZG_O_THVREF(:,IKB),ZTHVM,IKB,GLMIX,.TRUE.,ZSHEAR,ZLUP)
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  ZLUP(IIJB:IIJE)=MAX(ZLUP(IIJB:IIJE),1.E-10)

  ! Compute Buoyancy flux at the ground
  ZWTHVSURF(IIJB:IIJE) = (ZTHVM_F(IIJB:IIJE,IKB)/ZTHM_F(IIJB:IIJE,IKB))*PSFTH(IIJB:IIJE)+     &
                (0.61*ZTHM_F(IIJB:IIJE,IKB))*PSFRV(IIJB:IIJE)
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)

  ! Mass flux at KKB level (updraft triggered if PSFTH>0.)
  IF (PARAMMF%LGZ) THEN
    IF(PDX==0. .OR. PDY==0.) THEN                                                                                                   
      CALL PRINT_MSG(NVERB_FATAL, 'GEN', 'COMPUTE_UPDRAFT', 'PDX or PDY is NULL with option LGZ!')                                  
    ENDIF
    !$mnh_expand_array(JIJ=IIJB:IIJE)
    ZSURF(IIJB:IIJE)=TANH(PARAMMF%XGZ*SQRT(PDX*PDY)/ZLUP(IIJB:IIJE))
    !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  ELSE
    ZSURF(IIJB:IIJE)=1.
  END IF
  !$mnh_expand_where(JIJ=IIJB:IIJE)
  WHERE (ZWTHVSURF(IIJB:IIJE)>0.)
    PEMF(IIJB:IIJE,IKB) = PARAMMF%XCMF * ZSURF(IIJB:IIJE) * ZRHO_F(IIJB:IIJE,IKB) *  &
            ((ZG_O_THVREF(IIJB:IIJE,IKB))*ZWTHVSURF(IIJB:IIJE)*ZLUP(IIJB:IIJE))**(1./3.)
    PFRAC_UP(IIJB:IIJE,IKB)=MIN(PEMF(IIJB:IIJE,IKB)/(SQRT(ZW_UP2(IIJB:IIJE,IKB))*ZRHO_F(IIJB:IIJE,IKB)), &
                                   &PARAMMF%XFRAC_UP_MAX)
    ZW_UP2(IIJB:IIJE,IKB)=(PEMF(IIJB:IIJE,IKB)/(PFRAC_UP(IIJB:IIJE,IKB)*ZRHO_F(IIJB:IIJE,IKB)))**2
    GTEST(IIJB:IIJE)=.TRUE.
  ELSEWHERE
    PEMF(IIJB:IIJE,IKB) =0.
    GTEST(IIJB:IIJE)=.FALSE.
  ENDWHERE
  !$mnh_end_expand_where(JIJ=IIJB:IIJE)
ELSE
  !$mnh_expand_array(JIJ=IIJB:IIJE)
  GTEST(IIJB:IIJE)=PEMF(IIJB:IIJE,IKB+IKL)>0.
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
END IF

!--------------------------------------------------------------------------

!                        3. Vertical ascending loop
!                           -----------------------
!
! If GTEST = T the updraft starts from the KKB level and stops when GTEST becomes F
!
!
GTESTLCL(:)=.FALSE.
GTESTETL(:)=.FALSE.

!       Loop on vertical level

DO JK=IKB,IKE-IKL,IKL

  ! IF the updraft top is reached for all column, stop the loop on levels
  ITEST=0
  DO JIJ=IIJB,IIJE
    IF(GTEST(JIJ)) ITEST = ITEST + 1
  END DO
  IF (ITEST==0) CYCLE

  !       Computation of entrainment and detrainment with KF90
  !       parameterization in clouds and LR01 in subcloud layer


  ! to find the LCL (check if JK is LCL or not)
  !$mnh_expand_where(JIJ=IIJB:IIJE)
  WHERE ((PRC_UP(IIJB:IIJE,JK)+PRI_UP(IIJB:IIJE,JK)>0.).AND.(.NOT.(GTESTLCL(IIJB:IIJE))))
      KKLCL(IIJB:IIJE) = JK           
      GTESTLCL(IIJB:IIJE)=.TRUE.
  ENDWHERE
  !$mnh_end_expand_where(JIJ=IIJB:IIJE)

  ! COMPUTE PENTR and PDETR at mass level JK
  IF (OENTR_DETR) THEN
    IF(JK/=IKB) THEN
      !$mnh_expand_array(JIJ=IIJB:IIJE)
      ZRC_MIX(IIJB:IIJE,JK) = ZRC_MIX(IIJB:IIJE,JK-IKL) ! guess of Rc of mixture
      ZRI_MIX(IIJB:IIJE,JK) = ZRI_MIX(IIJB:IIJE,JK-IKL) ! guess of Ri of mixture
      !$mnh_end_expand_array(JIJ=IIJB:IIJE)
    ENDIF
    ZCOEFFMF_CLOUD=PARAMMF%XENTR_MF * CST%XG / PARAMMF%XCRAD_MF
!$mnh_expand_array(JIJ=IIJB:IIJE)
ZG_O_THVREF_ED(IIJB:IIJE)=CST%XG/ZTHVM(IIJB:IIJE,JK)

ZFRAC_ICE(IIJB:IIJE)=PFRAC_ICE_UP(IIJB:IIJE,JK) ! to not modify fraction of ice

ZPRE(IIJB:IIJE)=ZPRES_F(IIJB:IIJE,JK)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

!                1.4 Estimation of PPART_DRY
DO JIJ=IIJB,IIJE
  IF(GTEST(JIJ) .AND. GTESTLCL(JIJ)) THEN
    !No dry part when condensation level is reached
    ZPART_DRY(JIJ)=0.
    ZDZ_STOP(JIJ)=0.
    ZPRE(JIJ)=ZPRES_F(JIJ,JK)
  ELSE IF (GTEST(JIJ) .AND. .NOT. GTESTLCL(JIJ)) THEN
    !Temperature at flux level KK
    ZT=ZTH_UP(JIJ,JK)*(ZPRES_F(JIJ,JK)/CST%XP00) ** (CST%XRD/CST%XCPD)
    !Saturating vapor pressure at flux level KK
    ZFOESW = MIN(EXP( CST%XALPW - CST%XBETAW/ZT - CST%XGAMW*LOG(ZT)  ), 0.99*ZPRES_F(JIJ,JK))
    ZFOESI = MIN(EXP( CST%XALPI - CST%XBETAI/ZT - CST%XGAMI*LOG(ZT)  ), 0.99*ZPRES_F(JIJ,JK))
    !Computation of d.Rsat / dP (partial derivations with respect to P and T
    !and use of T=Theta*(P/P0)**(R/Cp) to transform dT into dP with theta_up
    !constant at the vertical)
    ZDRSATODP=(CST%XBETAW/ZT-CST%XGAMW)*(1-ZFRAC_ICE(JIJ))+(CST%XBETAI/ZT-CST%XGAMI)*ZFRAC_ICE(JIJ)
    ZDRSATODP=((CST%XRD/CST%XCPD)*ZDRSATODP-1.)*PRSAT_UP(JIJ,JK)/ &
                &(ZPRES_F(JIJ,JK)-(ZFOESW*(1-ZFRAC_ICE(JIJ)) + ZFOESI*ZFRAC_ICE(JIJ)))
    !Use of d.Rsat / dP and pressure at flux level KK to find pressure (ZPRE)
    !where Rsat is equal to PRT_UP
    ZPRE(JIJ)=ZPRES_F(JIJ,JK)+(PRT_UP(JIJ,JK)-PRSAT_UP(JIJ,JK))/ZDRSATODP
    !Fraction of dry part (computed with pressure and used with heights, no
    !impact found when using log function here and for pressure on flux levels
    !computation)
    ZPART_DRY(JIJ)=MAX(0., MIN(1., (ZPRES_F(JIJ,JK)-ZPRE(JIJ))/(ZPRES_F(JIJ,JK)-ZPRES_F(JIJ,JK+IKL))))
    !Height above flux level KK of the cloudy part
    ZDZ_STOP(JIJ) = (PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*ZPART_DRY(JIJ)
  ELSE
    ZPART_DRY(JIJ)=0. ! value does not matter, here
  END IF
END DO

!               1.5 Gradient and flux values of thetav
!$mnh_expand_array(JIJ=IIJB:IIJE)
IF(JK/=IKB)THEN
  ZCOEFF_MINUS_HALF(IIJB:IIJE)=((ZTHVM(IIJB:IIJE,JK)-ZTHVM(IIJB:IIJE,JK-IKL))/PDZZ(IIJB:IIJE,JK))
  ZTHV_MINUS_HALF(IIJB:IIJE) = ZTHVM(IIJB:IIJE,JK) - &
                               & ZCOEFF_MINUS_HALF(IIJB:IIJE)*0.5*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))
ELSE
  ZCOEFF_MINUS_HALF(IIJB:IIJE)=0.
  ZTHV_MINUS_HALF(IIJB:IIJE) = ZTHVM(IIJB:IIJE,JK)
ENDIF
ZCOEFF_PLUS_HALF(IIJB:IIJE)  = ((ZTHVM(IIJB:IIJE,JK+IKL)-ZTHVM(IIJB:IIJE,JK))/PDZZ(IIJB:IIJE,JK+IKL))
ZTHV_PLUS_HALF(IIJB:IIJE)  = ZTHVM(IIJB:IIJE,JK) + &
                             & ZCOEFF_PLUS_HALF(IIJB:IIJE)*0.5*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

!               2  Dry part computation:
!                  Integral buoyancy and computation of PENTR and PDETR for dry part
!               --------------------------------------------------------------------

DO JIJ=IIJB,IIJE
  IF (GTEST(JIJ) .AND. ZPART_DRY(JIJ)>0.) THEN
    !Buoyancy computation in two parts to use change of gradient of theta v of environment
    !Between flux level KK and min(mass level, bottom of cloudy part)
    ZDZ=MIN(ZDZ_STOP(JIJ),(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*0.5)
    ZBUO_INTEG_DRY(JIJ,JK) = ZG_O_THVREF_ED(JIJ)*ZDZ*&
                (0.5 * (  - ZCOEFF_MINUS_HALF(JIJ))*ZDZ  &
                  - ZTHV_MINUS_HALF(JIJ) + PTHV_UP(JIJ,JK) )

    !Between mass flux KK and bottom of cloudy part (if above mass flux)
    ZDZ=MAX(0., ZDZ_STOP(JIJ)-(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*0.5)
    ZBUO_INTEG_DRY(JIJ,JK) = ZBUO_INTEG_DRY(JIJ,JK) + ZG_O_THVREF_ED(JIJ)*ZDZ*&
                (0.5 * (  - ZCOEFF_PLUS_HALF(JIJ))*ZDZ &
                  - ZTHVM(JIJ,JK) + PTHV_UP(JIJ,JK) )

    !Entr//Detr. computation
    IF (ZBUO_INTEG_DRY(JIJ,JK)>=0.) THEN
      PENTR(JIJ,JK) = 0.5/(PARAMMF%XABUO-PARAMMF%XBENTR*PARAMMF%XENTR_DRY)*&
                 LOG(1.+ (2.*(PARAMMF%XABUO-PARAMMF%XBENTR*PARAMMF%XENTR_DRY)/ZW_UP2(JIJ,JK))* &
                 ZBUO_INTEG_DRY(JIJ,JK))
      PDETR(JIJ,JK) = 0.
    ELSE
      PENTR(JIJ,JK) = 0.
      PDETR(JIJ,JK) = 0.5/(PARAMMF%XABUO)*&
                 LOG(1.+ (2.*(PARAMMF%XABUO)/ZW_UP2(JIJ,JK))* &
                 (-ZBUO_INTEG_DRY(JIJ,JK)))
    ENDIF
    PENTR(JIJ,JK) = PARAMMF%XENTR_DRY*PENTR(JIJ,JK)/(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))    
    PDETR(JIJ,JK) = PARAMMF%XDETR_DRY*PDETR(JIJ,JK)/(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))
    !Minimum value of detrainment
    ZWK0D=ZLUP(JIJ)-0.5*(PZZ(JIJ,JK)+PZZ(JIJ,JK+IKL))
    ZWK0D=SIGN(MAX(1., ABS(ZWK0D)), ZWK0D) ! ZWK0D must not be zero
    PDETR(JIJ,JK) = MAX(ZPART_DRY(JIJ)*PARAMMF%XDETR_LUP/ZWK0D, PDETR(JIJ,JK))
  ELSE
    !No dry part, condensation reached (OTESTLCL)
    ZBUO_INTEG_DRY(JIJ,JK) = 0.
    PENTR(JIJ,JK)=0.
    PDETR(JIJ,JK)=0.
  ENDIF
ENDDO

!               3  Wet part computation
!               -----------------------

!               3.1 Integral buoyancy for cloudy part

! Compute theta_v of updraft at flux level KK+KKL                   
!MIX variables are used to avoid declaring new variables
!but we are dealing with updraft and not mixture
!$mnh_expand_array(JIJ=IIJB:IIJE)
ZRCMIX(IIJB:IIJE)=PRC_UP(IIJB:IIJE,JK)
ZRIMIX(IIJB:IIJE)=PRI_UP(IIJB:IIJE,JK)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)
IIJB=MERGE(D%NIJB, 1,  .TRUE. )
IIJE=MERGE(D%NIJE, D%NIJT,  .TRUE. )
!Number of iterations
JITER=2
!
!Computation of PBUF(IIJB:IIJE, ICPH2) depending on dummy arguments received
ZBUF(IIJB:IIJE, ICPH2)=0
!
!Computation of an approximate state thanks to PRL and PRI guess
ZBUF(IIJB:IIJE, IEXN)=(ZPRES_F(IIJB:IIJE,JK+IKL)/CST%XP00) ** CST%RDSCPD

DO JIJ=IIJB,IIJE
  ZBUF(JIJ, I99PP)=0.99*ZPRES_F(JIJ,JK+IKL)
  ZRVMIX(JIJ)=PRT_UP(JIJ,JK)-ZRCMIX(JIJ)-ZRIMIX(JIJ)
  ZBUF(JIJ, ICPH)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ)+ CST%XCL * ZRCMIX(JIJ) + CST%XCI * ZRIMIX(JIJ) + ZBUF(JIJ, ICPH2)
  ZVAR2=ZBUF(JIJ, ICPH)*ZBUF(JIJ, IEXN)
  ZDELT=(PTHL_UP(JIJ,JK)*ZBUF(JIJ, IEXN))-CST%XTT
  ZBUF(JIJ, ILVOCPEXN) = (CST%XLVTT + (CST%XCPV-CST%XCL) * ZDELT) /ZVAR2
  ZBUF(JIJ, ILSOCPEXN) = (CST%XLSTT + (CST%XCPV-CST%XCI) * ZDELT) /ZVAR2 
  ZTHMIX(JIJ)=PTHL_UP(JIJ,JK)+ZBUF(JIJ, ILVOCPEXN)*ZRCMIX(JIJ)+ZBUF(JIJ, ILSOCPEXN)*ZRIMIX(JIJ)
  ZBUF(JIJ, I1PRT)=1+PRT_UP(JIJ,JK)
ENDDO
!
!
!       2 Iteration
!         ---------

DO II=1,JITER
  IF (.FALSE.) THEN
    ZBUF(IIJB:IIJE, IT)=ZTHMIX(IIJB:IIJE)
  ELSE
    ZBUF(IIJB:IIJE, IT)=ZTHMIX(IIJB:IIJE)*ZBUF(IIJB:IIJE, IEXN)
  END IF
  !Computation of liquid/ice fractions
  ZFRAC_ICE(IIJB:IIJE) = 0.
  DO JIJ=IIJB, IIJE
    IF(ZRCMIX(JIJ)+ZRIMIX(JIJ) > 1.E-20) THEN
      ZFRAC_ICE(JIJ) = ZRIMIX(JIJ) / (ZRCMIX(JIJ)+ZRIMIX(JIJ))
    ENDIF
  ENDDO
  DO JIJ=IIJB,IIJE
  SELECT CASE(NEBN%CFRAC_ICE_SHALLOW_MF)
  CASE ('T') !using Temperature
    ZFRAC_ICE(JIJ) = MAX( 0., MIN(1., (( NEBN%XTMAXMIX - ZBUF(JIJ,IT) ) / ( NEBN%XTMAXMIX - NEBN%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    ZFRAC_ICE(JIJ) = MAX( 0., MIN(1., (( CST%XTT - ZBUF(JIJ,IT) ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    ZFRAC_ICE(JIJ) = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    ZFRAC_ICE(JIJ) = MAX( 0., MIN(1., ZFRAC_ICE(JIJ) ) )
  CASE DEFAULT
    END SELECT


END DO

!Computation of Rvsat and dRsat/dT
  !In this version QSAT, QSATI, DQSAT and DQASATI functions are not used
  !due to performance issue

  ! Log does not vectorize on all compilers:
  ZBUF(IIJB:IIJE, ILOGT)=LOG(ZBUF(IIJB:IIJE, IT))

  DO JIJ=IIJB, IIJE
    ZBUF(JIJ, IFOESW) = MIN(EXP( CST%XALPW - CST%XBETAW/ZBUF(JIJ, IT) - CST%XGAMW*ZBUF(JIJ, ILOGT)  ), ZBUF(JIJ, I99PP))
    ZBUF(JIJ, IFOESI) = MIN(EXP( CST%XALPI - CST%XBETAI/ZBUF(JIJ, IT) - CST%XGAMI*ZBUF(JIJ, ILOGT)  ), ZBUF(JIJ, I99PP))
    ZRSATW_ED(JIJ) = CST%XRD/CST%XRV*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,JK+IKL))
    ZRSATI_ED(JIJ) = CST%XRD/CST%XRV*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,JK+IKL))
    ZTPOW2=ZBUF(JIJ, IT)**2
    ZBUF(JIJ, IDRSATODTW) = ZRSATW_ED(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,JK+IKL) ) &
                     * (CST%XBETAW/ZTPOW2 - CST%XGAMW/ZBUF(JIJ, IT))*ZBUF(JIJ, I1PRT)
    ZBUF(JIJ, IDRSATODTI) = ZRSATI_ED(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,JK+IKL) ) &
                     * (CST%XBETAI/ZTPOW2 - CST%XGAMI/ZBUF(JIJ, IT))*ZBUF(JIJ, I1PRT)
    !PRSATW(JIJ) =  QSAT(PBUF(JIJ, IT),PP(JIJ)) !qsatw
    !PRSATI(JIJ) = QSATI(PBUF(JIJ, IT),PP(JIJ)) !qsati
    !PBUF(JIJ, IDRSATODTW) =  DQSAT(PBUF(JIJ, IT),PP(JIJ),PRSATW(JIJ))*PBUF(JIJ, I1PRT)
    !PBUF(JIJ, IDRSATODTI) = DQSATI(PBUF(JIJ, IT),PP(JIJ),PRSATI(JIJ))*PBUF(JIJ, I1PRT)
    ZRSATW_ED(JIJ) = ZRSATW_ED(JIJ)*ZBUF(JIJ, I1PRT)
    ZRSATI_ED(JIJ) = ZRSATI_ED(JIJ)*ZBUF(JIJ, I1PRT)
    ZBUF(JIJ, IRVSAT) = ZRSATW_ED(JIJ)*(1-ZFRAC_ICE(JIJ)) + ZRSATI_ED(JIJ)*ZFRAC_ICE(JIJ)
    ZBUF(JIJ, IDRSATODT) = (ZBUF(JIJ, IDRSATODTW)*(1-ZFRAC_ICE(JIJ))+ &
              & ZBUF(JIJ, IDRSATODTI)*ZFRAC_ICE(JIJ))

    !Computation of new PRL, PRI and PRV
    !Correction term applied to (PRV(JIJ)-PBUF(JIJ, IRVSAT)) is computed assuming that
    !PBUF(JIJ, ILVOCPEXN), PBUF(JIJ, ILSOCPEXN) and PBUF(JIJ, ICPH) don't vary to much with T. It takes into account
    !the variation (estimated linear) of Qsat with T
    ZBUF(JIJ, IRLTEMP)=(ZRVMIX(JIJ)-ZBUF(JIJ, IRVSAT))/ &
                  &(1 + ZBUF(JIJ, IDRSATODT)*ZBUF(JIJ, IEXN)* &
                  &     (ZBUF(JIJ, ILVOCPEXN)*(1-ZFRAC_ICE(JIJ))+ZBUF(JIJ, ILSOCPEXN)*ZFRAC_ICE(JIJ)))
    ZBUF(JIJ, IRLTEMP)=MIN(MAX(-ZRCMIX(JIJ)-ZRIMIX(JIJ), ZBUF(JIJ, IRLTEMP)),ZRVMIX(JIJ))
    ZRVMIX(JIJ)=ZRVMIX(JIJ)-ZBUF(JIJ, IRLTEMP)
    ZRCMIX(JIJ)=ZRCMIX(JIJ)+ZRIMIX(JIJ)+ZBUF(JIJ, IRLTEMP)
    ZRIMIX(JIJ)=ZFRAC_ICE(JIJ)     * (ZRCMIX(JIJ))
    ZRCMIX(JIJ)=(1-ZFRAC_ICE(JIJ)) * (PRT_UP(JIJ,JK) - ZRVMIX(JIJ))

    !Computation of Cph (as defined in Meso-NH doc, equation 2.2, to be used with mixing ratios)
    ZBUF(JIJ, ICPH)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ)+ CST%XCL * ZRCMIX(JIJ) + CST%XCI * ZRIMIX(JIJ) + ZBUF(JIJ, ICPH2)

    !Computation of L/Cph/EXN, then new PTH
    ZVAR2=ZBUF(JIJ, ICPH)*ZBUF(JIJ, IEXN)
    ZBUF(JIJ, ILVOCPEXN) = (CST%XLVTT + (CST%XCPV-CST%XCL) * (ZBUF(JIJ, IT)-CST%XTT)) /ZVAR2
    ZBUF(JIJ, ILSOCPEXN) = (CST%XLSTT + (CST%XCPV-CST%XCI) * (ZBUF(JIJ, IT)-CST%XTT)) /ZVAR2
    ZTHMIX(JIJ)=PTHL_UP(JIJ,JK)+ZBUF(JIJ, ILVOCPEXN)*ZRCMIX(JIJ)+ZBUF(JIJ, ILSOCPEXN)*ZRIMIX(JIJ)

    !Computation of estimated mixing ration at saturation
    !To compute the adjustement a first order development was used
    ZVAR1=ZTHMIX(JIJ)*ZBUF(JIJ, IEXN)-ZBUF(JIJ, IT)
    ZRSATW_ED(JIJ)=ZRSATW_ED(JIJ) + ZBUF(JIJ, IDRSATODTW)*ZVAR1
    ZRSATI_ED(JIJ)=ZRSATI_ED(JIJ) + ZBUF(JIJ, IDRSATODTI)*ZVAR1
  ENDDO
ENDDO


!$mnh_expand_array(JIJ=IIJB:IIJE)
ZTHV_UP_F2(IIJB:IIJE) = ZTHMIX(IIJB:IIJE)*(1.+ZRVORD*ZRVMIX(IIJB:IIJE))/(1.+PRT_UP(IIJB:IIJE,JK))
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

! Integral buoyancy for cloudy part
DO JIJ=IIJB,IIJE
  IF(GTEST(JIJ) .AND. ZPART_DRY(JIJ)<1.) THEN
    !Gradient of Theta V updraft over the cloudy part, assuming that thetaV updraft don't change
    !between flux level KK and bottom of cloudy part
    ZCOTHVU=(ZTHV_UP_F2(JIJ)-PTHV_UP(JIJ,JK))/((PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*(1-ZPART_DRY(JIJ)))

    !Computation in two parts to use change of gradient of theta v of environment
    !Between bottom of cloudy part (if under mass level) and mass level KK
    ZDZ=MAX(0., 0.5*(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))-ZDZ_STOP(JIJ))
    ZBUO_INTEG_CLD(JIJ,JK) = ZG_O_THVREF_ED(JIJ)*ZDZ*&
            (0.5*( ZCOTHVU - ZCOEFF_MINUS_HALF(JIJ))*ZDZ &
              - (ZTHVM(JIJ,JK)-ZDZ*ZCOEFF_MINUS_HALF(JIJ)) + PTHV_UP(JIJ,JK) )

    !Between max(mass level, bottom of cloudy part) and flux level KK+KKL
    ZDZ=(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))-MAX(ZDZ_STOP(JIJ),0.5*(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK)))
    ZBUO_INTEG_CLD(JIJ,JK) = ZBUO_INTEG_CLD(JIJ,JK)+ZG_O_THVREF_ED(JIJ)*ZDZ*&
                      (0.5*( ZCOTHVU - ZCOEFF_PLUS_HALF(JIJ))*ZDZ&
              - (ZTHVM(JIJ,JK)+(0.5*((PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK)))-ZDZ)*ZCOEFF_PLUS_HALF(JIJ)) +&
              PTHV_UP(JIJ,JK) )

  ELSE
    !No cloudy part
    ZBUO_INTEG_CLD(JIJ,JK)=0.
  END IF
END DO

!               3.2 Critical mixed fraction for KK+KKL flux level (ZKIC_F2) and
!                   for bottom of cloudy part (ZKIC), then a mean for the cloudy part
!                   (put also in ZKIC)
!
!                   computation by estimating unknown  
!                   T^mix r_c^mix and r_i^mix from enthalpy^mix and r_w^mix
!                   We determine the zero crossing of the linear curve
!                   evaluating the derivative using ZMIXF=0.1
                
ZKIC_INIT=0.1  ! starting value for critical mixed fraction for CLoudy Part

!  Compute thetaV of environment at the bottom of cloudy part
!    and cons then non cons. var. of mixture at the bottom of cloudy part

!   JKLIM computed to avoid KKL(KK-KKL) being < KKL*KKB
JKLIM=IKL*MAX(IKL*(JK-IKL),IKL*IKB)
DO JIJ=IIJB,IIJE
  IF(GTEST(JIJ) .AND. ZPART_DRY(JIJ)>0.5) THEN
    ZDZ=ZDZ_STOP(JIJ)-0.5*(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))
    ZTHV(JIJ)= ZTHVM(JIJ,JK)+ZCOEFF_PLUS_HALF(JIJ)*ZDZ
    ZMIXTHL(JIJ) = ZKIC_INIT * &
               (PTHLM(JIJ,JK)+ZDZ*(PTHLM(JIJ,JK+IKL)-PTHLM(JIJ,JK))/PDZZ(JIJ,JK+IKL)) + &
               (1. - ZKIC_INIT)*PTHL_UP(JIJ,JK)
    ZMIXRT(JIJ)  = ZKIC_INIT * &
               (PRTM(JIJ,JK)+ZDZ*(PRTM(JIJ,JK+IKL)-PRTM(JIJ,JK))/PDZZ(JIJ,JK+IKL)) +   &
               (1. - ZKIC_INIT)*PRT_UP(JIJ,JK)
  ELSEIF(GTEST(JIJ)) THEN
    ZDZ=0.5*(PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))-ZDZ_STOP(JIJ)
    ZTHV(JIJ)= ZTHVM(JIJ,JK)-ZCOEFF_MINUS_HALF(JIJ)*ZDZ
    ZMIXTHL(JIJ) = ZKIC_INIT * &
               (PTHLM(JIJ,JK)-ZDZ*(PTHLM(JIJ,JK)-PTHLM(JIJ,JKLIM))/PDZZ(JIJ,JK)) + &
               (1. - ZKIC_INIT)*PTHL_UP(JIJ,JK)
    ZMIXRT(JIJ)  = ZKIC_INIT * &
               (PRTM(JIJ,JK)-ZDZ*(PRTM(JIJ,JK)-PRTM(JIJ,JKLIM))/PDZZ(JIJ,JK)) + &
               (1. - ZKIC_INIT)*PRT_UP(JIJ,JK)
  ELSE
    ZMIXTHL(JIJ) = 300.
    ZMIXRT(JIJ) = 0.1
  ENDIF
ENDDO
IIJB=MERGE(D%NIJB, 1,  .TRUE. )
IIJE=MERGE(D%NIJE, D%NIJT,  .TRUE. )
!Number of iterations
JITER=2
!
!Computation of PBUF(IIJB:IIJE, ICPH2) depending on dummy arguments received
ZBUF(IIJB:IIJE, ICPH2)=0
!
!Computation of an approximate state thanks to PRL and PRI guess
ZBUF(IIJB:IIJE, IEXN)=(ZPRE(IIJB:IIJE)/CST%XP00) ** CST%RDSCPD

DO JIJ=IIJB,IIJE
  ZBUF(JIJ, I99PP)=0.99*ZPRE(JIJ)
  ZRVMIX(JIJ)=ZMIXRT(JIJ)-ZRC_MIX(JIJ,JK)-ZRI_MIX(JIJ,JK)
  ZBUF(JIJ, ICPH)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ)+ CST%XCL * ZRC_MIX(JIJ,JK) + CST%XCI * ZRI_MIX(JIJ,JK) + ZBUF(JIJ, ICPH2)
  ZVAR2=ZBUF(JIJ, ICPH)*ZBUF(JIJ, IEXN)
  ZDELT=(ZMIXTHL(JIJ)*ZBUF(JIJ, IEXN))-CST%XTT
  ZBUF(JIJ, ILVOCPEXN) = (CST%XLVTT + (CST%XCPV-CST%XCL) * ZDELT) /ZVAR2
  ZBUF(JIJ, ILSOCPEXN) = (CST%XLSTT + (CST%XCPV-CST%XCI) * ZDELT) /ZVAR2 
  ZTHMIX(JIJ)=ZMIXTHL(JIJ)+ZBUF(JIJ, ILVOCPEXN)*ZRC_MIX(JIJ,JK)+ZBUF(JIJ, ILSOCPEXN)*ZRI_MIX(JIJ,JK)
  ZBUF(JIJ, I1PRT)=1+ZMIXRT(JIJ)
ENDDO
!
!
!       2 Iteration
!         ---------

DO II=1,JITER
  IF (.FALSE.) THEN
    ZBUF(IIJB:IIJE, IT)=ZTHMIX(IIJB:IIJE)
  ELSE
    ZBUF(IIJB:IIJE, IT)=ZTHMIX(IIJB:IIJE)*ZBUF(IIJB:IIJE, IEXN)
  END IF
  !Computation of liquid/ice fractions
  ZFRAC_ICE(IIJB:IIJE) = 0.
  DO JIJ=IIJB, IIJE
    IF(ZRC_MIX(JIJ,JK)+ZRI_MIX(JIJ,JK) > 1.E-20) THEN
      ZFRAC_ICE(JIJ) = ZRI_MIX(JIJ,JK) / (ZRC_MIX(JIJ,JK)+ZRI_MIX(JIJ,JK))
    ENDIF
  ENDDO
  DO JIJ=IIJB,IIJE
  SELECT CASE(NEBN%CFRAC_ICE_SHALLOW_MF)
  CASE ('T') !using Temperature
    ZFRAC_ICE(JIJ) = MAX( 0., MIN(1., (( NEBN%XTMAXMIX - ZBUF(JIJ,IT) ) / ( NEBN%XTMAXMIX - NEBN%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    ZFRAC_ICE(JIJ) = MAX( 0., MIN(1., (( CST%XTT - ZBUF(JIJ,IT) ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    ZFRAC_ICE(JIJ) = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    ZFRAC_ICE(JIJ) = MAX( 0., MIN(1., ZFRAC_ICE(JIJ) ) )
  CASE DEFAULT
    END SELECT


END DO

!Computation of Rvsat and dRsat/dT
  !In this version QSAT, QSATI, DQSAT and DQASATI functions are not used
  !due to performance issue

  ! Log does not vectorize on all compilers:
  ZBUF(IIJB:IIJE, ILOGT)=LOG(ZBUF(IIJB:IIJE, IT))

  DO JIJ=IIJB, IIJE
    ZBUF(JIJ, IFOESW) = MIN(EXP( CST%XALPW - CST%XBETAW/ZBUF(JIJ, IT) - CST%XGAMW*ZBUF(JIJ, ILOGT)  ), ZBUF(JIJ, I99PP))
    ZBUF(JIJ, IFOESI) = MIN(EXP( CST%XALPI - CST%XBETAI/ZBUF(JIJ, IT) - CST%XGAMI*ZBUF(JIJ, ILOGT)  ), ZBUF(JIJ, I99PP))
    ZRSATW_ED(JIJ) = CST%XRD/CST%XRV*ZBUF(JIJ, IFOESW)/ZPRE(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESW)/ZPRE(JIJ))
    ZRSATI_ED(JIJ) = CST%XRD/CST%XRV*ZBUF(JIJ, IFOESI)/ZPRE(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESI)/ZPRE(JIJ))
    ZTPOW2=ZBUF(JIJ, IT)**2
    ZBUF(JIJ, IDRSATODTW) = ZRSATW_ED(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESW)/ZPRE(JIJ) ) &
                     * (CST%XBETAW/ZTPOW2 - CST%XGAMW/ZBUF(JIJ, IT))*ZBUF(JIJ, I1PRT)
    ZBUF(JIJ, IDRSATODTI) = ZRSATI_ED(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESI)/ZPRE(JIJ) ) &
                     * (CST%XBETAI/ZTPOW2 - CST%XGAMI/ZBUF(JIJ, IT))*ZBUF(JIJ, I1PRT)
    !PRSATW(JIJ) =  QSAT(PBUF(JIJ, IT),PP(JIJ)) !qsatw
    !PRSATI(JIJ) = QSATI(PBUF(JIJ, IT),PP(JIJ)) !qsati
    !PBUF(JIJ, IDRSATODTW) =  DQSAT(PBUF(JIJ, IT),PP(JIJ),PRSATW(JIJ))*PBUF(JIJ, I1PRT)
    !PBUF(JIJ, IDRSATODTI) = DQSATI(PBUF(JIJ, IT),PP(JIJ),PRSATI(JIJ))*PBUF(JIJ, I1PRT)
    ZRSATW_ED(JIJ) = ZRSATW_ED(JIJ)*ZBUF(JIJ, I1PRT)
    ZRSATI_ED(JIJ) = ZRSATI_ED(JIJ)*ZBUF(JIJ, I1PRT)
    ZBUF(JIJ, IRVSAT) = ZRSATW_ED(JIJ)*(1-ZFRAC_ICE(JIJ)) + ZRSATI_ED(JIJ)*ZFRAC_ICE(JIJ)
    ZBUF(JIJ, IDRSATODT) = (ZBUF(JIJ, IDRSATODTW)*(1-ZFRAC_ICE(JIJ))+ &
              & ZBUF(JIJ, IDRSATODTI)*ZFRAC_ICE(JIJ))

    !Computation of new PRL, PRI and PRV
    !Correction term applied to (PRV(JIJ)-PBUF(JIJ, IRVSAT)) is computed assuming that
    !PBUF(JIJ, ILVOCPEXN), PBUF(JIJ, ILSOCPEXN) and PBUF(JIJ, ICPH) don't vary to much with T. It takes into account
    !the variation (estimated linear) of Qsat with T
    ZBUF(JIJ, IRLTEMP)=(ZRVMIX(JIJ)-ZBUF(JIJ, IRVSAT))/ &
                  &(1 + ZBUF(JIJ, IDRSATODT)*ZBUF(JIJ, IEXN)* &
                  &     (ZBUF(JIJ, ILVOCPEXN)*(1-ZFRAC_ICE(JIJ))+ZBUF(JIJ, ILSOCPEXN)*ZFRAC_ICE(JIJ)))
    ZBUF(JIJ, IRLTEMP)=MIN(MAX(-ZRC_MIX(JIJ,JK)-ZRI_MIX(JIJ,JK), ZBUF(JIJ, IRLTEMP)),ZRVMIX(JIJ))
    ZRVMIX(JIJ)=ZRVMIX(JIJ)-ZBUF(JIJ, IRLTEMP)
    ZRC_MIX(JIJ,JK)=ZRC_MIX(JIJ,JK)+ZRI_MIX(JIJ,JK)+ZBUF(JIJ, IRLTEMP)
    ZRI_MIX(JIJ,JK)=ZFRAC_ICE(JIJ)     * (ZRC_MIX(JIJ,JK))
    ZRC_MIX(JIJ,JK)=(1-ZFRAC_ICE(JIJ)) * (ZMIXRT(JIJ) - ZRVMIX(JIJ))

    !Computation of Cph (as defined in Meso-NH doc, equation 2.2, to be used with mixing ratios)
    ZBUF(JIJ, ICPH)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ)+ CST%XCL * ZRC_MIX(JIJ,JK) + CST%XCI * ZRI_MIX(JIJ,JK) + ZBUF(JIJ, ICPH2)

    !Computation of L/Cph/EXN, then new PTH
    ZVAR2=ZBUF(JIJ, ICPH)*ZBUF(JIJ, IEXN)
    ZBUF(JIJ, ILVOCPEXN) = (CST%XLVTT + (CST%XCPV-CST%XCL) * (ZBUF(JIJ, IT)-CST%XTT)) /ZVAR2
    ZBUF(JIJ, ILSOCPEXN) = (CST%XLSTT + (CST%XCPV-CST%XCI) * (ZBUF(JIJ, IT)-CST%XTT)) /ZVAR2
    ZTHMIX(JIJ)=ZMIXTHL(JIJ)+ZBUF(JIJ, ILVOCPEXN)*ZRC_MIX(JIJ,JK)+ZBUF(JIJ, ILSOCPEXN)*ZRI_MIX(JIJ,JK)

    !Computation of estimated mixing ration at saturation
    !To compute the adjustement a first order development was used
    ZVAR1=ZTHMIX(JIJ)*ZBUF(JIJ, IEXN)-ZBUF(JIJ, IT)
    ZRSATW_ED(JIJ)=ZRSATW_ED(JIJ) + ZBUF(JIJ, IDRSATODTW)*ZVAR1
    ZRSATI_ED(JIJ)=ZRSATI_ED(JIJ) + ZBUF(JIJ, IDRSATODTI)*ZVAR1
  ENDDO
ENDDO


!$mnh_expand_array(JIJ=IIJB:IIJE)
ZTHVMIX(IIJB:IIJE) = ZTHMIX(IIJB:IIJE)*(1.+ZRVORD*ZRVMIX(IIJB:IIJE))/(1.+ZMIXRT(IIJB:IIJE))

!  Compute cons then non cons. var. of mixture at the flux level KK+KKL  with initial ZKIC
ZMIXTHL(IIJB:IIJE) = ZKIC_INIT * 0.5*(PTHLM(IIJB:IIJE,JK)+PTHLM(IIJB:IIJE,JK+IKL))+&
                       & (1. - ZKIC_INIT)*PTHL_UP(IIJB:IIJE,JK)
ZMIXRT(IIJB:IIJE)  = ZKIC_INIT * 0.5*(PRTM(IIJB:IIJE,JK)+PRTM(IIJB:IIJE,JK+IKL))+&
                       & (1. - ZKIC_INIT)*PRT_UP(IIJB:IIJE,JK)
!$mnh_end_expand_array(JIJ=IIJB:IIJE)
IIJB=MERGE(D%NIJB, 1,  .TRUE. )
IIJE=MERGE(D%NIJE, D%NIJT,  .TRUE. )
!Number of iterations
JITER=2
!
!Computation of PBUF(IIJB:IIJE, ICPH2) depending on dummy arguments received
ZBUF(IIJB:IIJE, ICPH2)=0
!
!Computation of an approximate state thanks to PRL and PRI guess
ZBUF(IIJB:IIJE, IEXN)=(ZPRES_F(IIJB:IIJE,JK+IKL)/CST%XP00) ** CST%RDSCPD

DO JIJ=IIJB,IIJE
  ZBUF(JIJ, I99PP)=0.99*ZPRES_F(JIJ,JK+IKL)
  ZRVMIX(JIJ)=ZMIXRT(JIJ)-ZRC_MIX(JIJ,JK)-ZRI_MIX(JIJ,JK)
  ZBUF(JIJ, ICPH)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ)+ CST%XCL * ZRC_MIX(JIJ,JK) + CST%XCI * ZRI_MIX(JIJ,JK) + ZBUF(JIJ, ICPH2)
  ZVAR2=ZBUF(JIJ, ICPH)*ZBUF(JIJ, IEXN)
  ZDELT=(ZMIXTHL(JIJ)*ZBUF(JIJ, IEXN))-CST%XTT
  ZBUF(JIJ, ILVOCPEXN) = (CST%XLVTT + (CST%XCPV-CST%XCL) * ZDELT) /ZVAR2
  ZBUF(JIJ, ILSOCPEXN) = (CST%XLSTT + (CST%XCPV-CST%XCI) * ZDELT) /ZVAR2 
  ZTHMIX(JIJ)=ZMIXTHL(JIJ)+ZBUF(JIJ, ILVOCPEXN)*ZRC_MIX(JIJ,JK)+ZBUF(JIJ, ILSOCPEXN)*ZRI_MIX(JIJ,JK)
  ZBUF(JIJ, I1PRT)=1+ZMIXRT(JIJ)
ENDDO
!
!
!       2 Iteration
!         ---------

DO II=1,JITER
  IF (.FALSE.) THEN
    ZBUF(IIJB:IIJE, IT)=ZTHMIX(IIJB:IIJE)
  ELSE
    ZBUF(IIJB:IIJE, IT)=ZTHMIX(IIJB:IIJE)*ZBUF(IIJB:IIJE, IEXN)
  END IF
  !Computation of liquid/ice fractions
  ZFRAC_ICE(IIJB:IIJE) = 0.
  DO JIJ=IIJB, IIJE
    IF(ZRC_MIX(JIJ,JK)+ZRI_MIX(JIJ,JK) > 1.E-20) THEN
      ZFRAC_ICE(JIJ) = ZRI_MIX(JIJ,JK) / (ZRC_MIX(JIJ,JK)+ZRI_MIX(JIJ,JK))
    ENDIF
  ENDDO
  DO JIJ=IIJB,IIJE
  SELECT CASE(NEBN%CFRAC_ICE_SHALLOW_MF)
  CASE ('T') !using Temperature
    ZFRAC_ICE(JIJ) = MAX( 0., MIN(1., (( NEBN%XTMAXMIX - ZBUF(JIJ,IT) ) / ( NEBN%XTMAXMIX - NEBN%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    ZFRAC_ICE(JIJ) = MAX( 0., MIN(1., (( CST%XTT - ZBUF(JIJ,IT) ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    ZFRAC_ICE(JIJ) = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    ZFRAC_ICE(JIJ) = MAX( 0., MIN(1., ZFRAC_ICE(JIJ) ) )
  CASE DEFAULT
    END SELECT


END DO

!Computation of Rvsat and dRsat/dT
  !In this version QSAT, QSATI, DQSAT and DQASATI functions are not used
  !due to performance issue

  ! Log does not vectorize on all compilers:
  ZBUF(IIJB:IIJE, ILOGT)=LOG(ZBUF(IIJB:IIJE, IT))

  DO JIJ=IIJB, IIJE
    ZBUF(JIJ, IFOESW) = MIN(EXP( CST%XALPW - CST%XBETAW/ZBUF(JIJ, IT) - CST%XGAMW*ZBUF(JIJ, ILOGT)  ), ZBUF(JIJ, I99PP))
    ZBUF(JIJ, IFOESI) = MIN(EXP( CST%XALPI - CST%XBETAI/ZBUF(JIJ, IT) - CST%XGAMI*ZBUF(JIJ, ILOGT)  ), ZBUF(JIJ, I99PP))
    ZRSATW_ED(JIJ) = CST%XRD/CST%XRV*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,JK+IKL))
    ZRSATI_ED(JIJ) = CST%XRD/CST%XRV*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,JK+IKL))
    ZTPOW2=ZBUF(JIJ, IT)**2
    ZBUF(JIJ, IDRSATODTW) = ZRSATW_ED(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,JK+IKL) ) &
                     * (CST%XBETAW/ZTPOW2 - CST%XGAMW/ZBUF(JIJ, IT))*ZBUF(JIJ, I1PRT)
    ZBUF(JIJ, IDRSATODTI) = ZRSATI_ED(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,JK+IKL) ) &
                     * (CST%XBETAI/ZTPOW2 - CST%XGAMI/ZBUF(JIJ, IT))*ZBUF(JIJ, I1PRT)
    !PRSATW(JIJ) =  QSAT(PBUF(JIJ, IT),PP(JIJ)) !qsatw
    !PRSATI(JIJ) = QSATI(PBUF(JIJ, IT),PP(JIJ)) !qsati
    !PBUF(JIJ, IDRSATODTW) =  DQSAT(PBUF(JIJ, IT),PP(JIJ),PRSATW(JIJ))*PBUF(JIJ, I1PRT)
    !PBUF(JIJ, IDRSATODTI) = DQSATI(PBUF(JIJ, IT),PP(JIJ),PRSATI(JIJ))*PBUF(JIJ, I1PRT)
    ZRSATW_ED(JIJ) = ZRSATW_ED(JIJ)*ZBUF(JIJ, I1PRT)
    ZRSATI_ED(JIJ) = ZRSATI_ED(JIJ)*ZBUF(JIJ, I1PRT)
    ZBUF(JIJ, IRVSAT) = ZRSATW_ED(JIJ)*(1-ZFRAC_ICE(JIJ)) + ZRSATI_ED(JIJ)*ZFRAC_ICE(JIJ)
    ZBUF(JIJ, IDRSATODT) = (ZBUF(JIJ, IDRSATODTW)*(1-ZFRAC_ICE(JIJ))+ &
              & ZBUF(JIJ, IDRSATODTI)*ZFRAC_ICE(JIJ))

    !Computation of new PRL, PRI and PRV
    !Correction term applied to (PRV(JIJ)-PBUF(JIJ, IRVSAT)) is computed assuming that
    !PBUF(JIJ, ILVOCPEXN), PBUF(JIJ, ILSOCPEXN) and PBUF(JIJ, ICPH) don't vary to much with T. It takes into account
    !the variation (estimated linear) of Qsat with T
    ZBUF(JIJ, IRLTEMP)=(ZRVMIX(JIJ)-ZBUF(JIJ, IRVSAT))/ &
                  &(1 + ZBUF(JIJ, IDRSATODT)*ZBUF(JIJ, IEXN)* &
                  &     (ZBUF(JIJ, ILVOCPEXN)*(1-ZFRAC_ICE(JIJ))+ZBUF(JIJ, ILSOCPEXN)*ZFRAC_ICE(JIJ)))
    ZBUF(JIJ, IRLTEMP)=MIN(MAX(-ZRC_MIX(JIJ,JK)-ZRI_MIX(JIJ,JK), ZBUF(JIJ, IRLTEMP)),ZRVMIX(JIJ))
    ZRVMIX(JIJ)=ZRVMIX(JIJ)-ZBUF(JIJ, IRLTEMP)
    ZRC_MIX(JIJ,JK)=ZRC_MIX(JIJ,JK)+ZRI_MIX(JIJ,JK)+ZBUF(JIJ, IRLTEMP)
    ZRI_MIX(JIJ,JK)=ZFRAC_ICE(JIJ)     * (ZRC_MIX(JIJ,JK))
    ZRC_MIX(JIJ,JK)=(1-ZFRAC_ICE(JIJ)) * (ZMIXRT(JIJ) - ZRVMIX(JIJ))

    !Computation of Cph (as defined in Meso-NH doc, equation 2.2, to be used with mixing ratios)
    ZBUF(JIJ, ICPH)=CST%XCPD+ CST%XCPV * ZRVMIX(JIJ)+ CST%XCL * ZRC_MIX(JIJ,JK) + CST%XCI * ZRI_MIX(JIJ,JK) + ZBUF(JIJ, ICPH2)

    !Computation of L/Cph/EXN, then new PTH
    ZVAR2=ZBUF(JIJ, ICPH)*ZBUF(JIJ, IEXN)
    ZBUF(JIJ, ILVOCPEXN) = (CST%XLVTT + (CST%XCPV-CST%XCL) * (ZBUF(JIJ, IT)-CST%XTT)) /ZVAR2
    ZBUF(JIJ, ILSOCPEXN) = (CST%XLSTT + (CST%XCPV-CST%XCI) * (ZBUF(JIJ, IT)-CST%XTT)) /ZVAR2
    ZTHMIX(JIJ)=ZMIXTHL(JIJ)+ZBUF(JIJ, ILVOCPEXN)*ZRC_MIX(JIJ,JK)+ZBUF(JIJ, ILSOCPEXN)*ZRI_MIX(JIJ,JK)

    !Computation of estimated mixing ration at saturation
    !To compute the adjustement a first order development was used
    ZVAR1=ZTHMIX(JIJ)*ZBUF(JIJ, IEXN)-ZBUF(JIJ, IT)
    ZRSATW_ED(JIJ)=ZRSATW_ED(JIJ) + ZBUF(JIJ, IDRSATODTW)*ZVAR1
    ZRSATI_ED(JIJ)=ZRSATI_ED(JIJ) + ZBUF(JIJ, IDRSATODTI)*ZVAR1
  ENDDO
ENDDO


!$mnh_expand_array(JIJ=IIJB:IIJE)
ZTHVMIX_F2(IIJB:IIJE) = ZTHMIX(IIJB:IIJE)*(1.+ZRVORD*ZRVMIX(IIJB:IIJE))/(1.+ZMIXRT(IIJB:IIJE))
!$mnh_end_expand_array(JIJ=IIJB:IIJE)

!Computation of mean ZKIC over the cloudy part
DO JIJ=IIJB,IIJE
  IF (GTEST(JIJ)) THEN
    ! Compute ZKIC at the bottom of cloudy part
    ! Thetav_up at bottom is equal to Thetav_up at flux level KK
    IF (ABS(PTHV_UP(JIJ,JK)-ZTHVMIX(JIJ))<1.E-10) THEN
      ZKIC(JIJ)=1.
    ELSE
      ZKIC(JIJ) = MAX(0.,PTHV_UP(JIJ,JK)-ZTHV(JIJ))*ZKIC_INIT /  &  
                 (PTHV_UP(JIJ,JK)-ZTHVMIX(JIJ))
    END IF
    ! Compute ZKIC_F2 at flux level KK+KKL
    IF (ABS(ZTHV_UP_F2(JIJ)-ZTHVMIX_F2(JIJ))<1.E-10) THEN
      ZKIC_F2(JIJ)=1.
    ELSE
      ZKIC_F2(JIJ) = MAX(0.,ZTHV_UP_F2(JIJ)-ZTHV_PLUS_HALF(JIJ))*ZKIC_INIT /  &  
                 (ZTHV_UP_F2(JIJ)-ZTHVMIX_F2(JIJ))
    END IF
    !Mean ZKIC over the cloudy part
    ZKIC(JIJ)=MAX(MIN(0.5*(ZKIC(JIJ)+ZKIC_F2(JIJ)),1.),0.)
  END IF
END DO

!               3.3 Integration of PDF
!                   According to Kain and Fritsch (1990), we replace delta Mt
!                   in eq. (7) and (8) using eq. (5). Here we compute the ratio
!                   of integrals without computing delta Me

!Constant PDF
!For this PDF, eq. (5) is delta Me=0.5*delta Mt
DO JIJ=IIJB,IIJE
  IF(GTEST(JIJ)) THEN
    ZEPSI(JIJ) = ZKIC(JIJ)**2. !integration multiplied by 2
    ZDELTA(JIJ) = (1.-ZKIC(JIJ))**2. !idem
  ENDIF
ENDDO

!Triangular PDF
!Calculus must be verified before activating this part, but in this state,
!results on ARM case are almost identical
!For this PDF, eq. (5) is also delta Me=0.5*delta Mt
!WHERE(OTEST(IIJB:IIJE))
!  !Integration multiplied by 2
!  WHERE(ZKIC<0.5)
!    ZEPSI(IIJB:IIJE)=8.*ZKIC(IIJB:IIJE)**3/3.
!    ZDELTA(IIJB:IIJE)=1.-4.*ZKIC(IIJB:IIJE)**2+8.*ZKIC(IIJB:IIJE)**3/3.
!  ELSEWHERE
!    ZEPSI(IIJB:IIJE)=5./3.-4*ZKIC(IIJB:IIJE)**2+8.*ZKIC(IIJB:IIJE)**3/3.
!    ZDELTA(IIJB:IIJE)=8.*(1.-ZKIC(IIJB:IIJE))**3/3.
!  ENDWHERE
!ENDWHERE

!               3.4 Computation of PENTR and PDETR
DO JIJ=IIJB,IIJE
  IF(GTEST(JIJ)) THEN
    ZEPSI_CLOUD=MIN(ZDELTA(JIJ), ZEPSI(JIJ))
    ZENTR_CLD(JIJ,JK) = (1.-ZPART_DRY(JIJ))*ZCOEFFMF_CLOUD*PRHODREF(JIJ,JK)*ZEPSI_CLOUD
    ZDETR_CLD(JIJ,JK) = (1.-ZPART_DRY(JIJ))*ZCOEFFMF_CLOUD*PRHODREF(JIJ,JK)*ZDELTA(JIJ)
    PENTR(JIJ,JK) = PENTR(JIJ,JK)+ZENTR_CLD(JIJ,JK)
    PDETR(JIJ,JK) = PDETR(JIJ,JK)+ZDETR_CLD(JIJ,JK)
  ELSE
    ZENTR_CLD(JIJ,JK) = 0.
    ZDETR_CLD(JIJ,JK) = 0.
  ENDIF
ENDDO


!$mnh_expand_where(JIJ=IIJB:IIJE)
    PBUO_INTEG(IIJB:IIJE,JK)=ZBUO_INTEG_DRY(IIJB:IIJE,JK)+ZBUO_INTEG_CLD(IIJB:IIJE,JK)

    IF (JK==IKB) THEN
       PDETR(IIJB:IIJE,JK)=0.
       ZDETR_CLD(IIJB:IIJE,JK)=0.
    ENDIF   
 
    !       Computation of updraft characteristics at level JK+KKL
    WHERE(GTEST(IIJB:IIJE))
      ZMIX1(IIJB:IIJE)=0.5*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))*&
                          &(PENTR(IIJB:IIJE,JK)-PDETR(IIJB:IIJE,JK))
      PEMF(IIJB:IIJE,JK+IKL)=PEMF(IIJB:IIJE,JK)*EXP(2*ZMIX1(IIJB:IIJE))
    ENDWHERE
    !$mnh_end_expand_where(JIJ=IIJB:IIJE)
  ELSE !OENTR_DETR
    !$mnh_expand_array(JIJ=IIJB:IIJE)
    GTEST(IIJB:IIJE) = (PEMF(IIJB:IIJE,JK+IKL)>0.)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  END IF !OENTR_DETR
  
  ! stop the updraft if MF becomes negative
  !$mnh_expand_where(JIJ=IIJB:IIJE)
  WHERE (GTEST(IIJB:IIJE).AND.(PEMF(IIJB:IIJE,JK+IKL)<=0.))
    PEMF(IIJB:IIJE,JK+IKL)=0.
    KKCTL(IIJB:IIJE) = JK+IKL
    GTEST(IIJB:IIJE)=.FALSE.
    PFRAC_ICE_UP(IIJB:IIJE,JK+IKL)=PFRAC_ICE_UP(IIJB:IIJE,JK)
    PRSAT_UP(IIJB:IIJE,JK+IKL)=PRSAT_UP(IIJB:IIJE,JK)
  ENDWHERE
  !$mnh_end_expand_where(JIJ=IIJB:IIJE)

  ! If the updraft did not stop, compute cons updraft characteritics at jk+KKL
  DO JIJ=IIJB,IIJE
    IF(GTEST(JIJ)) THEN
      ZMIX2(JIJ) = (PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*PENTR(JIJ,JK) !&
      ZMIX3_CLD(JIJ) = (PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*(1.-ZPART_DRY(JIJ))*ZDETR_CLD(JIJ,JK) !&                   
      ZMIX2_CLD(JIJ) = (PZZ(JIJ,JK+IKL)-PZZ(JIJ,JK))*(1.-ZPART_DRY(JIJ))*ZENTR_CLD(JIJ,JK)
      PTHL_UP(JIJ,JK+IKL)=(PTHL_UP(JIJ,JK)*(1.-0.5*ZMIX2(JIJ)) + PTHLM(JIJ,JK)*ZMIX2(JIJ)) &
                            /(1.+0.5*ZMIX2(JIJ))   
      PRT_UP(JIJ,JK+IKL) =(PRT_UP (JIJ,JK)*(1.-0.5*ZMIX2(JIJ)) + PRTM(JIJ,JK)*ZMIX2(JIJ))  &
                            /(1.+0.5*ZMIX2(JIJ))
    ENDIF
  ENDDO
  
  IF(PARAMMF%LMIXUV) THEN
    IF(JK/=IKB) THEN
      !$mnh_expand_where(JIJ=IIJB:IIJE)
      WHERE(GTEST(IIJB:IIJE))
        PU_UP(IIJB:IIJE,JK+IKL) = (PU_UP(IIJB:IIJE,JK)*(1-0.5*ZMIX2(IIJB:IIJE)) + &
                                        &PUM(IIJB:IIJE,JK)*ZMIX2(IIJB:IIJE)+ &
                          0.5*PARAMMF%XPRES_UV*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))*&
                          ((PUM(IIJB:IIJE,JK+IKL)-PUM(IIJB:IIJE,JK))/PDZZ(IIJB:IIJE,JK+IKL)+&
                           (PUM(IIJB:IIJE,JK)-PUM(IIJB:IIJE,JK-IKL))/PDZZ(IIJB:IIJE,JK))        )   &
                          /(1+0.5*ZMIX2(IIJB:IIJE))
        PV_UP(IIJB:IIJE,JK+IKL) = (PV_UP(IIJB:IIJE,JK)*(1-0.5*ZMIX2(IIJB:IIJE)) + &
                                        &PVM(IIJB:IIJE,JK)*ZMIX2(IIJB:IIJE)+ &
                          0.5*PARAMMF%XPRES_UV*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))*&
                          ((PVM(IIJB:IIJE,JK+IKL)-PVM(IIJB:IIJE,JK))/PDZZ(IIJB:IIJE,JK+IKL)+&
                           (PVM(IIJB:IIJE,JK)-PVM(IIJB:IIJE,JK-IKL))/PDZZ(IIJB:IIJE,JK))    )   &
                          /(1+0.5*ZMIX2(IIJB:IIJE))
      ENDWHERE
      !$mnh_end_expand_where(JIJ=IIJB:IIJE)
    ELSE
      !$mnh_expand_where(JIJ=IIJB:IIJE)
      WHERE(GTEST(IIJB:IIJE))
        PU_UP(IIJB:IIJE,JK+IKL) = (PU_UP(IIJB:IIJE,JK)*(1-0.5*ZMIX2(IIJB:IIJE)) + &
                                        &PUM(IIJB:IIJE,JK)*ZMIX2(IIJB:IIJE)+ &
                          0.5*PARAMMF%XPRES_UV*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))*&
                          ((PUM(IIJB:IIJE,JK+IKL)-PUM(IIJB:IIJE,JK))/PDZZ(IIJB:IIJE,JK+IKL))        )   &
                          /(1+0.5*ZMIX2(IIJB:IIJE))
        PV_UP(IIJB:IIJE,JK+IKL) = (PV_UP(IIJB:IIJE,JK)*(1-0.5*ZMIX2(IIJB:IIJE)) + &
                                        &PVM(IIJB:IIJE,JK)*ZMIX2(IIJB:IIJE)+ &
                          0.5*PARAMMF%XPRES_UV*(PZZ(IIJB:IIJE,JK+IKL)-PZZ(IIJB:IIJE,JK))*&
                          ((PVM(IIJB:IIJE,JK+IKL)-PVM(IIJB:IIJE,JK))/PDZZ(IIJB:IIJE,JK+IKL))    )   &
                          /(1+0.5*ZMIX2(IIJB:IIJE))
      ENDWHERE
      !$mnh_end_expand_where(JIJ=IIJB:IIJE)
    ENDIF
  ENDIF !PARAMMF%LMIXUV
  DO JSV=1,KSV 
    IF (ONOMIXLG .AND. JSV >= KSV_LGBEG .AND. JSV<= KSV_LGEND) CYCLE
    !$mnh_expand_where(JIJ=IIJB:IIJE)
    WHERE(GTEST(IIJB:IIJE)) 
      PSV_UP(IIJB:IIJE,JK+IKL,JSV) = (PSV_UP(IIJB:IIJE,JK,JSV)*(1-0.5*ZMIX2(IIJB:IIJE)) + &
                   PSVM(IIJB:IIJE,JK,JSV)*ZMIX2(IIJB:IIJE))  /(1+0.5*ZMIX2(IIJB:IIJE))
    ENDWHERE
    !$mnh_end_expand_where(JIJ=IIJB:IIJE)
  END DO  
  
  IF (OENTR_DETR) THEN

    ! Compute non cons. var. at level JK+KKL
    !$mnh_expand_array(JIJ=IIJB:IIJE)
    ZRC_UP(IIJB:IIJE)=PRC_UP(IIJB:IIJE,JK) ! guess = level just below
    ZRI_UP(IIJB:IIJE)=PRI_UP(IIJB:IIJE,JK) ! guess = level just below
    !$mnh_end_expand_array(JIJ=IIJB:IIJE)
    IIJB=MERGE(D%NIJB, 1,  .TRUE. )
IIJE=MERGE(D%NIJE, D%NIJT,  .TRUE. )
!Number of iterations
JITER=2
!
!Computation of PBUF(IIJB:IIJE, ICPH2) depending on dummy arguments received
ZBUF(IIJB:IIJE, ICPH2)=0
!
!Computation of an approximate state thanks to PRL and PRI guess
ZBUF(IIJB:IIJE, IEXN)=(ZPRES_F(IIJB:IIJE,JK+IKL)/CST%XP00) ** CST%RDSCPD

DO JIJ=IIJB,IIJE
  ZBUF(JIJ, I99PP)=0.99*ZPRES_F(JIJ,JK+IKL)
  ZRV_UP(JIJ)=PRT_UP(JIJ,JK+IKL)-ZRC_UP(JIJ)-ZRI_UP(JIJ)
  ZBUF(JIJ, ICPH)=CST%XCPD+ CST%XCPV * ZRV_UP(JIJ)+ CST%XCL * ZRC_UP(JIJ) + CST%XCI * ZRI_UP(JIJ) + ZBUF(JIJ, ICPH2)
  ZVAR2=ZBUF(JIJ, ICPH)*ZBUF(JIJ, IEXN)
  ZDELT=(PTHL_UP(JIJ,JK+IKL)*ZBUF(JIJ, IEXN))-CST%XTT
  ZBUF(JIJ, ILVOCPEXN) = (CST%XLVTT + (CST%XCPV-CST%XCL) * ZDELT) /ZVAR2
  ZBUF(JIJ, ILSOCPEXN) = (CST%XLSTT + (CST%XCPV-CST%XCI) * ZDELT) /ZVAR2 
  ZTH_UP(JIJ,JK+IKL)=PTHL_UP(JIJ,JK+IKL)+ZBUF(JIJ, ILVOCPEXN)*ZRC_UP(JIJ)+ZBUF(JIJ, ILSOCPEXN)*ZRI_UP(JIJ)
  ZBUF(JIJ, I1PRT)=1+PRT_UP(JIJ,JK+IKL)
ENDDO
!
!
!       2 Iteration
!         ---------

DO II=1,JITER
  IF (.FALSE.) THEN
    ZBUF(IIJB:IIJE, IT)=ZTH_UP(IIJB:IIJE,JK+IKL)
  ELSE
    ZBUF(IIJB:IIJE, IT)=ZTH_UP(IIJB:IIJE,JK+IKL)*ZBUF(IIJB:IIJE, IEXN)
  END IF
  !Computation of liquid/ice fractions
  PFRAC_ICE_UP(IIJB:IIJE,JK+IKL) = 0.
  DO JIJ=IIJB, IIJE
    IF(ZRC_UP(JIJ)+ZRI_UP(JIJ) > 1.E-20) THEN
      PFRAC_ICE_UP(JIJ,JK+IKL) = ZRI_UP(JIJ) / (ZRC_UP(JIJ)+ZRI_UP(JIJ))
    ENDIF
  ENDDO
  DO JIJ=IIJB,IIJE
  SELECT CASE(NEBN%CFRAC_ICE_SHALLOW_MF)
  CASE ('T') !using Temperature
    PFRAC_ICE_UP(JIJ,JK+IKL) = MAX( 0., MIN(1., (( NEBN%XTMAXMIX - ZBUF(JIJ,IT) ) / ( NEBN%XTMAXMIX - NEBN%XTMINMIX )) ) ) ! freezing interval
  CASE ('O') !using Temperature with old formulae
    PFRAC_ICE_UP(JIJ,JK+IKL) = MAX( 0., MIN(1., (( CST%XTT - ZBUF(JIJ,IT) ) / 40.) ) ) ! freezing interval
  CASE ('N') !No ice
    PFRAC_ICE_UP(JIJ,JK+IKL) = 0.
  CASE ('S') !Same as previous
    ! (almost) nothing to do
    PFRAC_ICE_UP(JIJ,JK+IKL) = MAX( 0., MIN(1., PFRAC_ICE_UP(JIJ,JK+IKL) ) )
  CASE DEFAULT
    END SELECT


END DO

!Computation of Rvsat and dRsat/dT
  !In this version QSAT, QSATI, DQSAT and DQASATI functions are not used
  !due to performance issue

  ! Log does not vectorize on all compilers:
  ZBUF(IIJB:IIJE, ILOGT)=LOG(ZBUF(IIJB:IIJE, IT))

  DO JIJ=IIJB, IIJE
    ZBUF(JIJ, IFOESW) = MIN(EXP( CST%XALPW - CST%XBETAW/ZBUF(JIJ, IT) - CST%XGAMW*ZBUF(JIJ, ILOGT)  ), ZBUF(JIJ, I99PP))
    ZBUF(JIJ, IFOESI) = MIN(EXP( CST%XALPI - CST%XBETAI/ZBUF(JIJ, IT) - CST%XGAMI*ZBUF(JIJ, ILOGT)  ), ZBUF(JIJ, I99PP))
    ZRSATW(JIJ) = CST%XRD/CST%XRV*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,JK+IKL))
    ZRSATI(JIJ) = CST%XRD/CST%XRV*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,JK+IKL) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,JK+IKL))
    ZTPOW2=ZBUF(JIJ, IT)**2
    ZBUF(JIJ, IDRSATODTW) = ZRSATW(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESW)/ZPRES_F(JIJ,JK+IKL) ) &
                     * (CST%XBETAW/ZTPOW2 - CST%XGAMW/ZBUF(JIJ, IT))*ZBUF(JIJ, I1PRT)
    ZBUF(JIJ, IDRSATODTI) = ZRSATI(JIJ) / (1.+(CST%XRD/CST%XRV-1.)*ZBUF(JIJ, IFOESI)/ZPRES_F(JIJ,JK+IKL) ) &
                     * (CST%XBETAI/ZTPOW2 - CST%XGAMI/ZBUF(JIJ, IT))*ZBUF(JIJ, I1PRT)
    !PRSATW(JIJ) =  QSAT(PBUF(JIJ, IT),PP(JIJ)) !qsatw
    !PRSATI(JIJ) = QSATI(PBUF(JIJ, IT),PP(JIJ)) !qsati
    !PBUF(JIJ, IDRSATODTW) =  DQSAT(PBUF(JIJ, IT),PP(JIJ),PRSATW(JIJ))*PBUF(JIJ, I1PRT)
    !PBUF(JIJ, IDRSATODTI) = DQSATI(PBUF(JIJ, IT),PP(JIJ),PRSATI(JIJ))*PBUF(JIJ, I1PRT)
    ZRSATW(JIJ) = ZRSATW(JIJ)*ZBUF(JIJ, I1PRT)
    ZRSATI(JIJ) = ZRSATI(JIJ)*ZBUF(JIJ, I1PRT)
    ZBUF(JIJ, IRVSAT) = ZRSATW(JIJ)*(1-PFRAC_ICE_UP(JIJ,JK+IKL)) + ZRSATI(JIJ)*PFRAC_ICE_UP(JIJ,JK+IKL)
    ZBUF(JIJ, IDRSATODT) = (ZBUF(JIJ, IDRSATODTW)*(1-PFRAC_ICE_UP(JIJ,JK+IKL))+ &
              & ZBUF(JIJ, IDRSATODTI)*PFRAC_ICE_UP(JIJ,JK+IKL))

    !Computation of new PRL, PRI and PRV
    !Correction term applied to (PRV(JIJ)-PBUF(JIJ, IRVSAT)) is computed assuming that
    !PBUF(JIJ, ILVOCPEXN), PBUF(JIJ, ILSOCPEXN) and PBUF(JIJ, ICPH) don't vary to much with T. It takes into account
    !the variation (estimated linear) of Qsat with T
    ZBUF(JIJ, IRLTEMP)=(ZRV_UP(JIJ)-ZBUF(JIJ, IRVSAT))/ &
                  &(1 + ZBUF(JIJ, IDRSATODT)*ZBUF(JIJ, IEXN)* &
                  &     (ZBUF(JIJ, ILVOCPEXN)*(1-PFRAC_ICE_UP(JIJ,JK+IKL))+ZBUF(JIJ, ILSOCPEXN)*PFRAC_ICE_UP(JIJ,JK+IKL)))
    ZBUF(JIJ, IRLTEMP)=MIN(MAX(-ZRC_UP(JIJ)-ZRI_UP(JIJ), ZBUF(JIJ, IRLTEMP)),ZRV_UP(JIJ))
    ZRV_UP(JIJ)=ZRV_UP(JIJ)-ZBUF(JIJ, IRLTEMP)
    ZRC_UP(JIJ)=ZRC_UP(JIJ)+ZRI_UP(JIJ)+ZBUF(JIJ, IRLTEMP)
    ZRI_UP(JIJ)=PFRAC_ICE_UP(JIJ,JK+IKL)     * (ZRC_UP(JIJ))
    ZRC_UP(JIJ)=(1-PFRAC_ICE_UP(JIJ,JK+IKL)) * (PRT_UP(JIJ,JK+IKL) - ZRV_UP(JIJ))

    !Computation of Cph (as defined in Meso-NH doc, equation 2.2, to be used with mixing ratios)
    ZBUF(JIJ, ICPH)=CST%XCPD+ CST%XCPV * ZRV_UP(JIJ)+ CST%XCL * ZRC_UP(JIJ) + CST%XCI * ZRI_UP(JIJ) + ZBUF(JIJ, ICPH2)

    !Computation of L/Cph/EXN, then new PTH
    ZVAR2=ZBUF(JIJ, ICPH)*ZBUF(JIJ, IEXN)
    ZBUF(JIJ, ILVOCPEXN) = (CST%XLVTT + (CST%XCPV-CST%XCL) * (ZBUF(JIJ, IT)-CST%XTT)) /ZVAR2
    ZBUF(JIJ, ILSOCPEXN) = (CST%XLSTT + (CST%XCPV-CST%XCI) * (ZBUF(JIJ, IT)-CST%XTT)) /ZVAR2
    ZTH_UP(JIJ,JK+IKL)=PTHL_UP(JIJ,JK+IKL)+ZBUF(JIJ, ILVOCPEXN)*ZRC_UP(JIJ)+ZBUF(JIJ, ILSOCPEXN)*ZRI_UP(JIJ)

    !Computation of estimated mixing ration at saturation
    !To compute the adjustement a first order development was used
    ZVAR1=ZTH_UP(JIJ,JK+IKL)*ZBUF(JIJ, IEXN)-ZBUF(JIJ, IT)
    ZRSATW(JIJ)=ZRSATW(JIJ) + ZBUF(JIJ, IDRSATODTW)*ZVAR1
    ZRSATI(JIJ)=ZRSATI(JIJ) + ZBUF(JIJ, IDRSATODTI)*ZVAR1
  ENDDO
ENDDO


!$mnh_expand_where(JIJ=IIJB:IIJE)
    WHERE(GTEST(IIJB:IIJE))
      PRC_UP(IIJB:IIJE,JK+IKL)=ZRC_UP(IIJB:IIJE)
      PRV_UP(IIJB:IIJE,JK+IKL)=ZRV_UP(IIJB:IIJE)
      PRI_UP(IIJB:IIJE,JK+IKL)=ZRI_UP(IIJB:IIJE)
      PRSAT_UP(IIJB:IIJE,JK+IKL) = ZRSATW(IIJB:IIJE)*(1-PFRAC_ICE_UP(IIJB:IIJE,JK+IKL)) + &
                                     & ZRSATI(IIJB:IIJE)*PFRAC_ICE_UP(IIJB:IIJE,JK+IKL)
    ENDWHERE

    ! Compute the updraft theta_v, buoyancy and w**2 for level JK+KKL
  DO JIJ=IIJB,IIJE
    IF(GTEST(JIJ)) THEN
      PTHV_UP(JIJ,JK+IKL) = ZTH_UP(JIJ,JK+IKL)* &
                                    & ((1+ZRVORD*PRV_UP(JIJ,JK+IKL))/(1+PRT_UP(JIJ,JK+IKL)))
      IF (ZBUO_INTEG_DRY(JIJ,JK)>0.) THEN
        ZW_UP2(JIJ,JK+IKL)  = ZW_UP2(JIJ,JK) + 2.*(PARAMMF%XABUO-PARAMMF%XBENTR*PARAMMF%XENTR_DRY)* &
                                                                &ZBUO_INTEG_DRY(JIJ,JK)
      ELSE
        ZW_UP2(JIJ,JK+IKL)  = ZW_UP2(JIJ,JK) + 2.*PARAMMF%XABUO* ZBUO_INTEG_DRY(JIJ,JK)
      END IF
      ZW_UP2(JIJ,JK+IKL)  = ZW_UP2(JIJ,JK+IKL)*(1.-(PARAMMF%XBDETR*ZMIX3_CLD(JIJ)+ &
                                                                       &PARAMMF%XBENTR*ZMIX2_CLD(JIJ)))&
              /(1.+(PARAMMF%XBDETR*ZMIX3_CLD(JIJ)+PARAMMF%XBENTR*ZMIX2_CLD(JIJ))) &
              +2.*(PARAMMF%XABUO)*ZBUO_INTEG_CLD(JIJ,JK)/ &
              &(1.+(PARAMMF%XBDETR*ZMIX3_CLD(JIJ)+PARAMMF%XBENTR*ZMIX2_CLD(JIJ)))
    END IF
  END DO

    ! Test if the updraft has reach the ETL
    WHERE (GTEST(IIJB:IIJE).AND.(PBUO_INTEG(IIJB:IIJE,JK)<=0.))
      KKETL(IIJB:IIJE) = JK+IKL
      GTESTETL(IIJB:IIJE)=.TRUE.
    ELSEWHERE
      GTESTETL(IIJB:IIJE)=.FALSE.
    ENDWHERE

    ! Test is we have reached the top of the updraft
    WHERE (GTEST(IIJB:IIJE).AND.((ZW_UP2(IIJB:IIJE,JK+IKL)<=0.).OR.(PEMF(IIJB:IIJE,JK+IKL)<=0.)))
        ZW_UP2(IIJB:IIJE,JK+IKL)=0.
        PEMF(IIJB:IIJE,JK+IKL)=0.
        GTEST(IIJB:IIJE)=.FALSE.
        PTHL_UP(IIJB:IIJE,JK+IKL)=ZTHLM_F(IIJB:IIJE,JK+IKL)
        PRT_UP(IIJB:IIJE,JK+IKL)=ZRTM_F(IIJB:IIJE,JK+IKL)
        PRC_UP(IIJB:IIJE,JK+IKL)=0.
        PRI_UP(IIJB:IIJE,JK+IKL)=0.
        PRV_UP(IIJB:IIJE,JK+IKL)=0.
        PTHV_UP(IIJB:IIJE,JK+IKL)=ZTHVM_F(IIJB:IIJE,JK+IKL)
        PFRAC_UP(IIJB:IIJE,JK+IKL)=0.
        KKCTL(IIJB:IIJE)=JK+IKL
    ENDWHERE
 
    ! compute frac_up at JK+KKL
    WHERE (GTEST(IIJB:IIJE))
      PFRAC_UP(IIJB:IIJE,JK+IKL)=PEMF(IIJB:IIJE,JK+IKL)/&
                                      &(SQRT(ZW_UP2(IIJB:IIJE,JK+IKL))*ZRHO_F(IIJB:IIJE,JK+IKL))
    ENDWHERE

    ! Updraft fraction must be smaller than XFRAC_UP_MAX
    WHERE (GTEST(IIJB:IIJE))
      PFRAC_UP(IIJB:IIJE,JK+IKL)=MIN(PARAMMF%XFRAC_UP_MAX,PFRAC_UP(IIJB:IIJE,JK+IKL))
    ENDWHERE

    ! When cloudy and non-buoyant, updraft fraction must decrease
    WHERE ((GTEST(IIJB:IIJE).AND.GTESTETL(IIJB:IIJE)).AND.GTESTLCL(IIJB:IIJE))
      PFRAC_UP(IIJB:IIJE,JK+IKL)=MIN(PFRAC_UP(IIJB:IIJE,JK+IKL),PFRAC_UP(IIJB:IIJE,JK))
    ENDWHERE

    ! Mass flux is updated with the new updraft fraction
    IF (OENTR_DETR) PEMF(IIJB:IIJE,JK+IKL)=PFRAC_UP(IIJB:IIJE,JK+IKL)*SQRT(ZW_UP2(IIJB:IIJE,JK+IKL))* &
                                              &ZRHO_F(IIJB:IIJE,JK+IKL)
    !$mnh_end_expand_where(JIJ=IIJB:IIJE)

  END IF !OENTR_DETR
ENDDO

IF(OENTR_DETR) THEN

  !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
  PW_UP(IIJB:IIJE,1:IKT)=SQRT(ZW_UP2(IIJB:IIJE,1:IKT))
  !$mnh_end_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)

  !$mnh_expand_array(JIJ=IIJB:IIJE)
  PEMF(IIJB:IIJE,IKB) =0.
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)

  ! Limits the shallow convection scheme when cloud heigth is higher than 3000m.
  ! To do this, mass flux is multiplied by a coefficient decreasing linearly
  ! from 1 (for clouds of ZDEPTH_MAX1 m of depth) to 0 (for clouds of ZDEPTH_MAX2 m of depth).
  ! This way, all MF fluxes are diminished by this amount.
  ! Diagnosed cloud fraction is also multiplied by the same coefficient.
  !
  DO JIJ=IIJB,IIJE
     PDEPTH(JIJ) = MAX(0., PZZ(JIJ,KKCTL(JIJ)) -  PZZ(JIJ,KKLCL(JIJ)) )
  END DO

  !$mnh_expand_array(JIJ=IIJB:IIJE)
  GWORK1(IIJB:IIJE)= (GTESTLCL(IIJB:IIJE) .AND. (PDEPTH(IIJB:IIJE) > ZDEPTH_MAX1) )
  !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  DO JK=1,IKT
    !$mnh_expand_array(JIJ=IIJB:IIJE)
    GWORK2(IIJB:IIJE,JK) = GWORK1(IIJB:IIJE)
    ZCOEF(IIJB:IIJE,JK) = (1.-(PDEPTH(IIJB:IIJE)-ZDEPTH_MAX1)/(ZDEPTH_MAX2-ZDEPTH_MAX1))
    ZCOEF(IIJB:IIJE,JK)=MIN(MAX(ZCOEF(IIJB:IIJE,JK),0.),1.)
    !$mnh_end_expand_array(JIJ=IIJB:IIJE)
  ENDDO
  !$mnh_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)
  WHERE (GWORK2(IIJB:IIJE,1:IKT)) 
    PEMF(IIJB:IIJE,1:IKT)     = PEMF(IIJB:IIJE,1:IKT)     * ZCOEF(IIJB:IIJE,1:IKT)
    PFRAC_UP(IIJB:IIJE,1:IKT) = PFRAC_UP(IIJB:IIJE,1:IKT) * ZCOEF(IIJB:IIJE,1:IKT)
  ENDWHERE
  !$mnh_end_expand_where(JIJ=IIJB:IIJE,JK=1:IKT)
ENDIF

IF (LHOOK) CALL DR_HOOK('COMPUTE_UPDRAFT',1,ZHOOK_HANDLE)
CONTAINS
          !MNH_LIC Copyright 2006-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
      !MNH_LIC Copyright 2006-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
      END SUBROUTINE COMPUTE_UPDRAFT
END MODULE MODE_COMPUTE_UPDRAFT

