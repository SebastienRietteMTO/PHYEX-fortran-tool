!MNH_LIC Copyright 1994-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
MODULE MODE_ICE4_FAST_RS
IMPLICIT NONE
CONTAINS
SUBROUTINE ICE4_FAST_RS(CST, PARAMI, ICEP, ICED, KPROMA, KSIZE, LDSOFT, LDCOMPUTE, &
                       &PRHODREF, PLVFACT, PLSFACT, PPRES, &
                       &PDV, PKA, PCJ, &
                       &PLBDAR, PLBDAS, &
                       &PT,  PRVT, PRCT, PRRT, PRST, &
                       &PRIAGGS, &
                       &PRCRIMSS, PRCRIMSG, PRSRIMCG, &
                       &PRRACCSS, PRRACCSG, PRSACCRG, PRSMLTG, &
                       &PRCMLTSR, &
                       &PRS_TEND)
!!
!!**  PURPOSE
!!    -------
!!      Computes the fast rs processes
!!
!!    AUTHOR
!!    ------
!!      S. Riette from the splitting of rain_ice source code (nov. 2014)
!!
!!    MODIFICATIONS
!!    -------------
!!
!  P. Wautelet 26/04/2019: replace non-standard FLOAT function by REAL function
!  P. Wautelet 29/05/2019: remove PACK/UNPACK intrinsics (to get more performance and better OpenACC support)
!!     R. El Khatib 24-Aug-2021 Optimizations
!  J. Wurtz       03/2022: New snow characteristics with LSNOW_T
!
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CST,            ONLY: CST_t
USE MODD_PARAM_ICE_n,      ONLY: PARAM_ICE_t
USE MODD_RAIN_ICE_DESCR_n, ONLY: RAIN_ICE_DESCR_t
USE MODD_RAIN_ICE_PARAM_n, ONLY: RAIN_ICE_PARAM_t
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
TYPE(CST_t),              INTENT(IN)    :: CST
TYPE(PARAM_ICE_t),        INTENT(IN)    :: PARAMI
TYPE(RAIN_ICE_PARAM_t),   INTENT(IN)    :: ICEP
TYPE(RAIN_ICE_DESCR_t),   INTENT(IN)    :: ICED
INTEGER,                      INTENT(IN)    :: KPROMA, KSIZE
LOGICAL,                      INTENT(IN)    :: LDSOFT
LOGICAL, DIMENSION(KPROMA),   INTENT(IN)    :: LDCOMPUTE
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PRHODREF ! Reference density
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PLVFACT
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PLSFACT
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PPRES    ! absolute pressure at t
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PDV      ! Diffusivity of water vapor in the air
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PKA      ! Thermal conductivity of the air
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PCJ      ! Function to compute the ventilation coefficient
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PLBDAR   ! Slope parameter of the raindrop  distribution
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PLBDAS   ! Slope parameter of the aggregate distribution
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PT       ! Temperature
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PRVT     ! Water vapor m.r. at t
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PRCT     ! Cloud water m.r. at t
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PRRT     ! Rain water m.r. at t
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PRST     ! Snow/aggregate m.r. at t
REAL, DIMENSION(KPROMA),      INTENT(IN)    :: PRIAGGS  ! r_i aggregation on r_s
REAL, DIMENSION(KPROMA),      INTENT(OUT)   :: PRCRIMSS ! Cloud droplet riming of the aggregates
REAL, DIMENSION(KPROMA),      INTENT(OUT)   :: PRCRIMSG ! Cloud droplet riming of the aggregates
REAL, DIMENSION(KPROMA),      INTENT(OUT)   :: PRSRIMCG ! Cloud droplet riming of the aggregates
REAL, DIMENSION(KPROMA),      INTENT(OUT)   :: PRRACCSS ! Rain accretion onto the aggregates
REAL, DIMENSION(KPROMA),      INTENT(OUT)   :: PRRACCSG ! Rain accretion onto the aggregates
REAL, DIMENSION(KPROMA),      INTENT(OUT)   :: PRSACCRG ! Rain accretion onto the aggregates
REAL, DIMENSION(KPROMA),      INTENT(INOUT) :: PRSMLTG  ! Conversion-Melting of the aggregates
REAL, DIMENSION(KPROMA),      INTENT(INOUT) :: PRCMLTSR ! Cloud droplet collection onto aggregates by positive temperature
REAL, DIMENSION(KPROMA, 8),   INTENT(INOUT) :: PRS_TEND ! Individual tendencies
!
!*       0.2  declaration of local variables
!
INTEGER, PARAMETER :: IRCRIMS=1, IRCRIMSS=2, IRSRIMCG=3, IRRACCS=4, IRRACCSS=5, IRSACCRG=6, &
                    & IFREEZ1=7, IFREEZ2=8
LOGICAL, DIMENSION(KPROMA) :: GRIM, GACC
INTEGER :: IGRIM, IGACC
INTEGER, DIMENSION(KPROMA) :: IBUF1, IBUF2, IBUF3
REAL, DIMENSION(KPROMA) :: ZBUF1, ZBUF2, ZBUF3
REAL, DIMENSION(KPROMA) :: ZZW, ZZW1, ZZW2, ZZW3, ZFREEZ_RATE
INTEGER :: JL
REAL :: ZZW0D
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ICE4_FAST_RS', 0, ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
!
!*       5.0    maximum freezing rate
!
DO JL=1, KSIZE
  IF(PRST(JL)>ICED%XRTMIN(5) .AND. LDCOMPUTE(JL)) THEN
    IF(.NOT. LDSOFT) THEN
      PRS_TEND(JL, IFREEZ1)=PRVT(JL)*PPRES(JL)/(CST%XEPSILO+PRVT(JL)) ! Vapor pressure
      IF(PARAMI%LEVLIMIT) THEN
        PRS_TEND(JL, IFREEZ1)=MIN(PRS_TEND(JL, IFREEZ1), EXP(CST%XALPI-CST%XBETAI/PT(JL)-CST%XGAMI*ALOG(PT(JL)))) ! min(ev, es_i(T))
      ENDIF
      PRS_TEND(JL, IFREEZ1)=PKA(JL)*(CST%XTT-PT(JL)) +                              &
                           &(PDV(JL)*(CST%XLVTT+(CST%XCPV-CST%XCL)*(PT(JL)-CST%XTT)) &
                           &*(CST%XESTT-PRS_TEND(JL, IFREEZ1))/(CST%XRV*PT(JL))           )
PRS_TEND(JL, IFREEZ1)=PRS_TEND(JL, IFREEZ1)* (ICEP%X0DEPS*       PLBDAS(JL)**ICEP%XEX0DEPS +     &
                           &                        ICEP%X1DEPS*PCJ(JL)*PLBDAS(JL)**ICEP%XEX1DEPS )/ &
                           &(PRHODREF(JL)*(CST%XLMTT-CST%XCL*(CST%XTT-PT(JL))))
PRS_TEND(JL, IFREEZ2)=(PRHODREF(JL)*(CST%XLMTT+(CST%XCI-CST%XCL)*(CST%XTT-PT(JL)))   ) / &
                           &(PRHODREF(JL)*(CST%XLMTT-CST%XCL*(CST%XTT-PT(JL))))
    ENDIF
    !We must agregate, at least, the cold species
    !And we are only interested by the freezing rate of liquid species
    ZFREEZ_RATE(JL)=MAX(0., MAX(0., PRS_TEND(JL, IFREEZ1) + &
                                    &PRS_TEND(JL, IFREEZ2) * PRIAGGS(JL)) - &
                            PRIAGGS(JL))
  ELSE
    PRS_TEND(JL, IFREEZ1)=0.
    PRS_TEND(JL, IFREEZ2)=0.
    ZFREEZ_RATE(JL)=0.
  ENDIF
ENDDO
!
!*       5.1    cloud droplet riming of the aggregates
!
DO JL=1, KSIZE
  IF (PRCT(JL)>ICED%XRTMIN(2) .AND. PRST(JL)>ICED%XRTMIN(5) .AND. LDCOMPUTE(JL)) THEN
ZZW(JL) = PLBDAS(JL)
GRIM(JL) = .TRUE.
  ELSE
    GRIM(JL) = .FALSE.
    PRS_TEND(JL, IRCRIMS)=0.
    PRS_TEND(JL, IRCRIMSS)=0.
    PRS_TEND(JL, IRSRIMCG)=0.
  ENDIF
ENDDO
!
! Collection of cloud droplets by snow: this rate is used for riming (T<0) and for conversion/melting (T>0)
IF(.NOT. LDSOFT) THEN
  CALL INTERP_MICRO_1D(KPROMA, KSIZE, ZZW, ICEP%NGAMINC, ICEP%XRIMINTP1, ICEP%XRIMINTP2, &
                           PARAMI%LPACK_INTERP, GRIM(:), IBUF1, IBUF2, ZBUF1, ZBUF2, &
                           IGRIM, &
                           ICEP%XGAMINC_RIM1(:), ZZW1(:), ICEP%XGAMINC_RIM2(:), ZZW2(:), ICEP%XGAMINC_RIM4(:), ZZW3(:))
  IF(IGRIM>0) THEN
    !
    !        5.1.4  riming of the small sized aggregates
    !
    !$mnh_expand_where(JL=1:KSIZE)
    WHERE (GRIM(1:KSIZE))
PRS_TEND(1:KSIZE, IRCRIMSS) = ICEP%XCRIMSS * ZZW1(1:KSIZE) * PRCT(1:KSIZE) & ! RCRIMSS
                                      * PLBDAS(1:KSIZE)**ICEP%XEXCRIMSS &
                                      * PRHODREF(1:KSIZE)**(-ICED%XCEXVT)
END WHERE
    !$mnh_end_expand_where(JL=1:KSIZE)
    !
    !        5.1.6  riming-conversion of the large sized aggregates into graupeln
    !
    !
    !$mnh_expand_where(JL=1:KSIZE)
    WHERE(GRIM(1:KSIZE))
PRS_TEND(1:KSIZE, IRCRIMS)=ICEP%XCRIMSG * PRCT(1:KSIZE)               & ! RCRIMS
                                   * PLBDAS(1:KSIZE)**ICEP%XEXCRIMSG  &
                                   * PRHODREF(1:KSIZE)**(-ICED%XCEXVT)
END WHERE
    !$mnh_end_expand_where(JL=1:KSIZE)

    IF(PARAMI%CSNOWRIMING=='M90 ')THEN
      !Murakami 1990
      !$mnh_expand_where(JL=1:KSIZE)
      WHERE(GRIM(1:KSIZE))
        ZZW(1:KSIZE) = PRS_TEND(1:KSIZE, IRCRIMS) - PRS_TEND(1:KSIZE, IRCRIMSS) ! RCRIMSG
PRS_TEND(1:KSIZE, IRSRIMCG)=ICEP%XSRIMCG * PLBDAS(1:KSIZE)**ICEP%XEXSRIMCG*(1.0-ZZW2(1:KSIZE))
PRS_TEND(1:KSIZE, IRSRIMCG)=ZZW(1:KSIZE)*PRS_TEND(1:KSIZE, IRSRIMCG)/ &
                       MAX(1.E-20, &
                           ICEP%XSRIMCG3*ICEP%XSRIMCG2*PLBDAS(1:KSIZE)**ICEP%XEXSRIMCG2*(1.-ZZW3(1:KSIZE)) - &
                           ICEP%XSRIMCG3*PRS_TEND(1:KSIZE, IRSRIMCG))
END WHERE
      !$mnh_end_expand_where(JL=1:KSIZE)
    ELSE
      PRS_TEND(:, IRSRIMCG)=0.
    END IF
  ENDIF
ENDIF
!
DO JL=1, KSIZE
  ! More restrictive RIM mask to be used for riming by negative temperature only
  IF(GRIM(JL) .AND. PT(JL)<CST%XTT) THEN
    PRCRIMSS(JL)=MIN(ZFREEZ_RATE(JL), PRS_TEND(JL, IRCRIMSS))
    ZFREEZ_RATE(JL)=MAX(0., ZFREEZ_RATE(JL)-PRCRIMSS(JL))
    ZZW0D = MIN(1., ZFREEZ_RATE(JL) / MAX(1.E-20, PRS_TEND(JL, IRCRIMS) - PRCRIMSS(JL))) ! proportion we are able to freeze
    PRCRIMSG(JL) = ZZW0D * MAX(0., PRS_TEND(JL, IRCRIMS) - PRCRIMSS(JL)) ! RCRIMSG
    ZFREEZ_RATE(JL)=MAX(0., ZFREEZ_RATE(JL)-PRCRIMSG(JL))
    PRSRIMCG(JL) = ZZW0D * PRS_TEND(JL, IRSRIMCG)

    PRSRIMCG(JL) = PRSRIMCG(JL) * MAX(0., -SIGN(1., -PRCRIMSG(JL)))
    PRCRIMSG(JL)=MAX(0., PRCRIMSG(JL))
  ELSE
    PRCRIMSS(JL)=0.
    PRCRIMSG(JL)=0.
    PRSRIMCG(JL)=0.
  ENDIF
ENDDO
!
!*       5.2    rain accretion onto the aggregates
!
DO JL = 1, KSIZE
  IF (PRRT(JL)>ICED%XRTMIN(3) .AND. PRST(JL)>ICED%XRTMIN(5) .AND. LDCOMPUTE(JL)) THEN
    GACC(JL) = .TRUE.
  ELSE
    GACC(JL) = .FALSE.
    PRS_TEND(JL, IRRACCS)=0.
    PRS_TEND(JL, IRRACCSS)=0.
    PRS_TEND(JL, IRSACCRG)=0.
  END IF
ENDDO
IF(.NOT. LDSOFT) THEN
  PRS_TEND(:, IRRACCS)=0.
  PRS_TEND(:, IRRACCSS)=0.
  PRS_TEND(:, IRSACCRG)=0.
  CALL INTERP_MICRO_2D(KPROMA, KSIZE, PLBDAS, PLBDAR, ICEP%NACCLBDAS, ICEP%NACCLBDAR, &
                      &ICEP%XACCINTP1S, ICEP%XACCINTP2S, ICEP%XACCINTP1R, ICEP%XACCINTP2R,&
                      &PARAMI%LPACK_INTERP, GACC(:), IBUF1(:), IBUF2(:), IBUF3(:), ZBUF1(:), ZBUF2(:), ZBUF3(:), &
                      &IGACC, &
                      &ICEP%XKER_RACCSS(:,:), ZZW1(:), ICEP%XKER_RACCS(:,:), ZZW2(:), ICEP%XKER_SACCRG(:,:), ZZW3(:))
  IF(IGACC>0)THEN
    !        5.2.4  raindrop accretion on the small sized aggregates
    !
    !$mnh_expand_where(JL=1:KSIZE)
    WHERE(GACC(1:KSIZE))
ZZW(1:KSIZE) =                                                        & !! coef of RRACCS
            ICEP%XFRACCSS*( PLBDAS(1:KSIZE)**ICED%XCXS )*( PRHODREF(1:KSIZE)**(-ICED%XCEXVT-1.) ) &
       *( ICEP%XLBRACCS1/((PLBDAS(1:KSIZE)**2)               ) +                  &
          ICEP%XLBRACCS2/( PLBDAS(1:KSIZE)    * PLBDAR(1:KSIZE)    ) +                  &
          ICEP%XLBRACCS3/(               (PLBDAR(1:KSIZE)**2)) )/PLBDAR(1:KSIZE)**4
PRS_TEND(1:KSIZE, IRRACCSS) =ZZW1(1:KSIZE)*ZZW(1:KSIZE)
    END WHERE
    !$mnh_end_expand_where(JL=1:KSIZE)
    !
    !$mnh_expand_where(JL=1:KSIZE)
    WHERE(GACC(1:KSIZE))
      PRS_TEND(1:KSIZE, IRRACCS) = ZZW2(1:KSIZE)*ZZW(1:KSIZE)
    END WHERE
    !$mnh_end_expand_where(JL=1:KSIZE)
    !
    !        5.2.6  raindrop accretion-conversion of the large sized aggregates
    !               into graupeln
    !
    !$mnh_expand_where(JL=1:KSIZE)
    WHERE(GACC(1:KSIZE))
PRS_TEND(1:KSIZE, IRSACCRG) = ICEP%XFSACCRG*ZZW3(1:KSIZE)*                    & ! RSACCRG
          ( PLBDAS(1:KSIZE)**(ICED%XCXS-ICED%XBS) )*( PRHODREF(1:KSIZE)**(-ICED%XCEXVT-1.) ) &
         *( ICEP%XLBSACCR1/((PLBDAR(1:KSIZE)**2)               ) +           &
            ICEP%XLBSACCR2/( PLBDAR(1:KSIZE)    * PLBDAS(1:KSIZE)    ) +           &
            ICEP%XLBSACCR3/(               (PLBDAS(1:KSIZE)**2)) )/PLBDAR(1:KSIZE)
END WHERE
    !$mnh_end_expand_where(JL=1:KSIZE)
  ENDIF
ENDIF
!
DO JL=1, KSIZE
  ! More restrictive ACC mask to be used for accretion by negative temperature only
  IF(GACC(JL) .AND. PT(JL)<CST%XTT) THEN
    PRRACCSS(JL)=MIN(ZFREEZ_RATE(JL), PRS_TEND(JL, IRRACCSS))
    ZFREEZ_RATE(JL)=MAX(0., ZFREEZ_RATE(JL)-PRRACCSS(JL))
    ZZW(JL) = MIN(1., ZFREEZ_RATE(JL) / MAX(1.E-20, PRS_TEND(JL, IRRACCS)-PRRACCSS(JL))) ! proportion we are able to freeze
    PRRACCSG(JL)=ZZW(JL) * MAX(0., PRS_TEND(JL, IRRACCS)-PRRACCSS(JL))
    ZFREEZ_RATE(JL) = MAX(0., ZFREEZ_RATE(JL)-PRRACCSG(JL))
    PRSACCRG(JL)=ZZW(JL) * PRS_TEND(JL, IRSACCRG)

    PRSACCRG(JL) = PRSACCRG(JL) * MAX(0., -SIGN(1., -PRRACCSG(JL)))
    PRRACCSG(JL)=MAX(0., PRRACCSG(JL))
  ELSE
    PRRACCSS(JL)=0.
    PRRACCSG(JL)=0.
    PRSACCRG(JL)=0.
  ENDIF
ENDDO
!
!
!*       5.3    Conversion-Melting of the aggregates
!
DO JL=1, KSIZE
  IF(PRST(JL)>ICED%XRTMIN(5) .AND. PT(JL)>CST%XTT .AND. LDCOMPUTE(JL)) THEN
    IF(.NOT. LDSOFT) THEN
      PRSMLTG(JL)=PRVT(JL)*PPRES(JL)/(CST%XEPSILO+PRVT(JL)) ! Vapor pressure
      IF(PARAMI%LEVLIMIT) THEN
        PRSMLTG(JL)=MIN(PRSMLTG(JL), EXP(CST%XALPW-CST%XBETAW/PT(JL)-CST%XGAMW*ALOG(PT(JL)))) ! min(ev, es_w(T))
      ENDIF
      PRSMLTG(JL)= PKA(JL)*(CST%XTT-PT(JL)) +                                 &
                  &(PDV(JL)*(CST%XLVTT + ( CST%XCPV - CST%XCL ) * ( PT(JL) - CST%XTT )) &
                  & *(CST%XESTT-PRSMLTG(JL))/(CST%XRV*PT(JL))             )
      !
      ! compute RSMLT
      !
PRSMLTG(JL)  = ICEP%XFSCVMG*MAX(0., (-PRSMLTG(JL) * &
                 (ICEP%X0DEPS*       PLBDAS(JL)**ICEP%XEX0DEPS +     &
                 ICEP%X1DEPS*PCJ(JL)*PLBDAS(JL)**ICEP%XEX1DEPS)    &
                 -(PRS_TEND(JL, IRCRIMS) + PRS_TEND(JL, IRRACCS)) *       &
                 (PRHODREF(JL)*CST%XCL*(CST%XTT-PT(JL))) &
                 ) / (PRHODREF(JL)*CST%XLMTT))
!
      ! note that RSCVMG = RSMLT*XFSCVMG but no heat is exchanged (at the rate RSMLT)
      ! because the graupeln produced by this process are still icy!!!
      !
      ! When T < XTT, rc is collected by snow (riming) to produce snow and graupel
      ! When T > XTT, if riming was still enabled, rc would produce snow and graupel with snow becomming graupel (conversion/melting) and graupel becomming rain (melting)
      ! To insure consistency when crossing T=XTT, rc collected with T>XTT must be transformed in rain.
      ! rc cannot produce iced species with a positive temperature but is still collected with a good efficiency by snow
      PRCMLTSR(JL) = PRS_TEND(JL, IRCRIMS) ! both species are liquid, no heat is exchanged
    ENDIF
  ELSE
    PRSMLTG(JL)=0.
    PRCMLTSR(JL)=0.
  ENDIF
ENDDO

IF (LHOOK) CALL DR_HOOK('ICE4_FAST_RS', 1, ZHOOK_HANDLE)
!
CONTAINS
!
INCLUDE "interp_micro.func.h"
!These routines are intented to be included in the contains part of other subroutines.
!To allow the transformation for GPU, no local array must be declared.
!If a temporary local array is needed, it must be added as a buffer in the interface (IBUF?, ZBUF?)

SUBROUTINE INTERP_MICRO_1D(KPROMA, KSIZE, PIN, KNUM, P1, P2, &
                           LDPACK, LDMASK, KBUF1, KBUF2, PBUF1, PBUF2, &
                           KLEN, &
                           PLT1, POUT1, PLT2, POUT2, PLT3, POUT3)

IMPLICIT NONE

INTEGER,                    INTENT(IN)  :: KPROMA       !Array size
INTEGER,                    INTENT(IN)  :: KSIZE        !Last usefull array index
REAL,    DIMENSION(KPROMA), INTENT(IN)  :: PIN          !Input array
INTEGER,                    INTENT(IN)  :: KNUM         !Number of points in the look-up table
REAL,                       INTENT(IN)  :: P1           !Scaling factor
REAL,                       INTENT(IN)  :: P2           !Scaling factor
LOGICAL,                    INTENT(IN)  :: LDPACK       !.TRUE. to perform packing
LOGICAL, DIMENSION(KPROMA), INTENT(IN)  :: LDMASK       !Computation mask
INTEGER, DIMENSION(KPROMA), INTENT(OUT) :: KBUF1, KBUF2 !Buffer arrays
REAL,    DIMENSION(KPROMA), INTENT(OUT) :: PBUF1, PBUF2 !Buffer arrays
INTEGER,                    INTENT(OUT) :: KLEN         !Number of active points
REAL,    DIMENSION(KNUM),   INTENT(IN)            :: PLT1  !Look-up table
REAL,    DIMENSION(KPROMA), INTENT(OUT)           :: POUT1 !Interpolated values
REAL,    DIMENSION(KNUM),   INTENT(IN) , OPTIONAL :: PLT2
REAL,    DIMENSION(KPROMA), INTENT(OUT), OPTIONAL :: POUT2
REAL,    DIMENSION(KNUM),   INTENT(IN) , OPTIONAL :: PLT3
REAL,    DIMENSION(KPROMA), INTENT(OUT), OPTIONAL :: POUT3

INTEGER :: JL
INTEGER :: IINDEX
REAL :: ZINDEX

IF (LDPACK) THEN

  !Pack input array
  KLEN=0
  DO JL=1, KSIZE
    IF (LDMASK(JL)) THEN
      KLEN=KLEN+1
      PBUF1(KLEN)=PIN(JL)
      KBUF1(KLEN)=JL
    ENDIF
  ENDDO

  IF (KLEN>0) THEN
    !Index computation
    !$mnh_expand_array(JL=1:KLEN)
    PBUF1(1:KLEN) = MAX(1.00001, MIN(REAL(KNUM)-0.00001, P1 * LOG(PBUF1(1:KLEN)) + P2))
    KBUF2(1:KLEN) = INT(PBUF1(1:KLEN))
    PBUF1(1:KLEN) = PBUF1(1:KLEN) - REAL(KBUF2(1:KLEN))
    !$mnh_end_expand_array(JL=1:KLEN)

    !Interpolation and unpack
    !$mnh_expand_array(JL=1:KLEN)
    PBUF2(1:KLEN) = PLT1(KBUF2(1:KLEN)+1) *  PBUF1(1:KLEN)       &
                  &-PLT1(KBUF2(1:KLEN)  ) * (PBUF1(1:KLEN) - 1.0)
    !$mnh_end_expand_array(JL=1:KLEN)
    POUT1(:)=0.
    DO JL=1, KLEN
      POUT1(KBUF1(JL))=PBUF2(JL)
    ENDDO

    !Interpolation and unpack 2
    IF(PRESENT(PLT2)) THEN
      !$mnh_expand_array(JL=1:KLEN)
      PBUF2(1:KLEN) = PLT2(KBUF2(1:KLEN)+1) *  PBUF1(1:KLEN)       &
                    &-PLT2(KBUF2(1:KLEN)  ) * (PBUF1(1:KLEN) - 1.0)
      !$mnh_end_expand_array(JL=1:KLEN)
      POUT2(:)=0.
      DO JL=1, KLEN
        POUT2(KBUF1(JL))=PBUF2(JL)
      ENDDO
    ENDIF

    !Interpolation and unpack 3
    IF(PRESENT(PLT3)) THEN
      !$mnh_expand_array(JL=1:KLEN)
      PBUF2(1:KLEN) = PLT3(KBUF2(1:KLEN)+1) *  PBUF1(1:KLEN)       &
                    &-PLT3(KBUF2(1:KLEN)  ) * (PBUF1(1:KLEN) - 1.0)
      !$mnh_end_expand_array(JL=1:KLEN)
      POUT3(:)=0.
      DO JL=1, KLEN
        POUT3(KBUF1(JL))=PBUF2(JL)
      ENDDO
    ENDIF

  ENDIF

ELSE

  KLEN=0
  DO JL=1, KSIZE
    IF (LDMASK(JL)) THEN
      KLEN=KLEN+1

      !Index computation
      ZINDEX = MAX(1.00001, MIN(REAL(KNUM)-0.00001, P1 * LOG(PIN(JL)) + P2))
      IINDEX = INT(ZINDEX)
      ZINDEX = ZINDEX - REAL(IINDEX)

      !Interpolations
      POUT1(JL) = PLT1(IINDEX+1) *  ZINDEX       &
                &-PLT1(IINDEX  ) * (ZINDEX - 1.0)

      IF(PRESENT(PLT2)) THEN
        POUT2(JL) = PLT2(IINDEX+1) *  ZINDEX       &
                  &-PLT2(IINDEX  ) * (ZINDEX - 1.0)
      ENDIF

      IF(PRESENT(PLT3)) THEN
        POUT3(JL) = PLT3(IINDEX+1) *  ZINDEX       &
                  &-PLT3(IINDEX  ) * (ZINDEX - 1.0)
      ENDIF

    ELSE
      POUT1(JL) = 0.
      IF(PRESENT(PLT2)) POUT2(JL) = 0.
      IF(PRESENT(PLT3)) POUT3(JL) = 0.
    ENDIF
  ENDDO

ENDIF
END SUBROUTINE INTERP_MICRO_1D

SUBROUTINE INTERP_MICRO_2D(KPROMA, KSIZE, PIN1, PIN2, KNUM1, KNUM2, P11, P12, P21, P22,&
                           LDPACK, LDMASK, KBUF1, KBUF2, KBUF3, PBUF1, PBUF2, PBUF3, &
                           KLEN, &
                           PLT1, POUT1, PLT2, POUT2, PLT3, POUT3)

IMPLICIT NONE

INTEGER,                    INTENT(IN)  :: KPROMA       !Array size
INTEGER,                    INTENT(IN)  :: KSIZE        !Last usefull array index
REAL,    DIMENSION(KPROMA), INTENT(IN)  :: PIN1                !Input array
REAL,    DIMENSION(KPROMA), INTENT(IN)  :: PIN2                !Input array
INTEGER,                    INTENT(IN)  :: KNUM1               !First dimension of the look-up table
INTEGER,                    INTENT(IN)  :: KNUM2               !Second dimension of the look-up table
REAL,                       INTENT(IN)  :: P11                 !Scaling factor
REAL,                       INTENT(IN)  :: P12                 !Scaling factor
REAL,                       INTENT(IN)  :: P21                 !Scaling factor
REAL,                       INTENT(IN)  :: P22                 !Scaling factor
LOGICAL,                    INTENT(IN)  :: LDPACK              !.TRUE. to perform packing
LOGICAL, DIMENSION(KPROMA), INTENT(IN)  :: LDMASK              !Computation mask
INTEGER, DIMENSION(KPROMA), INTENT(OUT) :: KBUF1, KBUF2, KBUF3 !Buffer arrays
REAL,    DIMENSION(KPROMA), INTENT(OUT) :: PBUF1, PBUF2, PBUF3 !Buffer arrays
INTEGER,                    INTENT(OUT) :: KLEN                !Number of active points
REAL,    DIMENSION(KNUM1, KNUM2),   INTENT(IN)            :: PLT1  !Look-up table
REAL,    DIMENSION(KPROMA),         INTENT(OUT)           :: POUT1 !Interpolated values from the first look-up table
REAL,    DIMENSION(KNUM1, KNUM2),   INTENT(IN) , OPTIONAL :: PLT2  !Other look-up table
REAL,    DIMENSION(KPROMA),         INTENT(OUT), OPTIONAL :: POUT2 !Interpolated values from the second look-up table
REAL,    DIMENSION(KNUM2, KNUM1),   INTENT(IN) , OPTIONAL :: PLT3  !Another look-up table **CAUTION, TABLE IS REVERSED**
REAL,    DIMENSION(KPROMA),         INTENT(OUT), OPTIONAL :: POUT3 !Interpolated values from the third look-up table

INTEGER :: JL
INTEGER :: IINDEX1, IINDEX2
REAL :: ZINDEX1, ZINDEX2

IF (LDPACK) THEN

  !Pack input array
  KLEN=0
  DO JL=1, KSIZE
    IF (LDMASK(JL)) THEN
      KLEN=KLEN+1
      PBUF1(KLEN)=PIN1(JL)
      PBUF2(KLEN)=PIN2(JL)
      KBUF3(KLEN)=JL
    ENDIF
  ENDDO

  IF (KLEN>0) THEN
    !Index computation
    !$mnh_expand_array(JL=1:KLEN)
    PBUF1(1:KLEN) = MAX(1.00001, MIN(REAL(KNUM1)-0.00001, P11 * LOG(PBUF1(1:KLEN)) + P12))
    KBUF1(1:KLEN) = INT(PBUF1(1:KLEN))
    PBUF1(1:KLEN) = PBUF1(1:KLEN) - REAL(KBUF1(1:KLEN))

    PBUF2(1:KLEN) = MAX(1.00001, MIN(REAL(KNUM2)-0.00001, P21 * LOG(PBUF2(1:KLEN)) + P22))
    KBUF2(1:KLEN) = INT(PBUF2(1:KLEN))
    PBUF2(1:KLEN) = PBUF2(1:KLEN) - REAL(KBUF2(1:KLEN))
    !$mnh_end_expand_array(JL=1:KLEN)

    !Interpolation and unpack 1
    DO JL=1, KLEN
      PBUF3(JL) = ( PLT1(KBUF1(JL)+1,KBUF2(JL)+1)* PBUF2(JL)         &
                   -PLT1(KBUF1(JL)+1,KBUF2(JL)  )*(PBUF2(JL) - 1.0)) *  PBUF1(JL) &
                 -( PLT1(KBUF1(JL)  ,KBUF2(JL)+1)* PBUF2(JL)         &
                   -PLT1(KBUF1(JL)  ,KBUF2(JL)  )*(PBUF2(JL) - 1.0)) * (PBUF1(JL) - 1.0)
    ENDDO
    POUT1(:)=0.
    DO JL=1, KLEN
      POUT1(KBUF3(JL))=PBUF3(JL)
    ENDDO

    !Interpolation and unpack 2
    IF(PRESENT(PLT2)) THEN
      DO JL=1, KLEN
        PBUF3(JL) = ( PLT2(KBUF1(JL)+1,KBUF2(JL)+1)* PBUF2(JL)         &
                     -PLT2(KBUF1(JL)+1,KBUF2(JL)  )*(PBUF2(JL) - 1.0)) *  PBUF1(JL) &
                   -( PLT2(KBUF1(JL)  ,KBUF2(JL)+1)* PBUF2(JL)         &
                     -PLT2(KBUF1(JL)  ,KBUF2(JL)  )*(PBUF2(JL) - 1.0)) * (PBUF1(JL) - 1.0)
      ENDDO
      POUT2(:)=0.
      DO JL=1, KLEN
        POUT2(KBUF3(JL))=PBUF3(JL)
      ENDDO
    ENDIF

    !Interpolation and unpack 3
    IF(PRESENT(PLT3)) THEN
      DO JL=1, KLEN
        PBUF3(JL) = ( PLT3(KBUF2(JL)+1,KBUF1(JL)+1)* PBUF1(JL)         &
                     -PLT3(KBUF2(JL)+1,KBUF1(JL)  )*(PBUF1(JL) - 1.0)) *  PBUF2(JL) &
                   -( PLT3(KBUF2(JL)  ,KBUF1(JL)+1)* PBUF1(JL)         &
                     -PLT3(KBUF2(JL)  ,KBUF1(JL)  )*(PBUF1(JL) - 1.0)) * (PBUF2(JL) - 1.0)
      ENDDO
      POUT3(:)=0.
      DO JL=1, KLEN
        POUT3(KBUF3(JL))=PBUF3(JL)
      ENDDO
    ENDIF
  ENDIF

ELSE

  KLEN=0
  DO JL=1, KSIZE
    IF (LDMASK(JL)) THEN
      KLEN=KLEN+1

      !Indexes computation
      ZINDEX1 = MAX(1.00001, MIN(REAL(KNUM1)-0.00001, P11 * LOG(PIN1(JL)) + P12))
      IINDEX1 = INT(ZINDEX1)
      ZINDEX1 = ZINDEX1 - REAL(IINDEX1)
  
      ZINDEX2 = MAX(1.00001, MIN(REAL(KNUM1)-0.00001, P21 * LOG(PIN2(JL)) + P22))
      IINDEX2 = INT(ZINDEX2)
      ZINDEX2 = ZINDEX2 - REAL(IINDEX2)
  
      !Interpolations
      POUT1(JL) = ( PLT1(IINDEX1+1,IINDEX2+1)* ZINDEX2         &
                   -PLT1(IINDEX1+1,IINDEX2  )*(ZINDEX2 - 1.0)) *  ZINDEX1 &
                 -( PLT1(IINDEX1  ,IINDEX2+1)* ZINDEX2         &
                   -PLT1(IINDEX1  ,IINDEX2  )*(ZINDEX2 - 1.0)) * (ZINDEX1 - 1.0)

      IF(PRESENT(PLT2)) THEN
        POUT2(JL) = ( PLT2(IINDEX1+1,IINDEX2+1)* ZINDEX2         &
                     -PLT2(IINDEX1+1,IINDEX2  )*(ZINDEX2 - 1.0)) *  ZINDEX1 &
                   -( PLT2(IINDEX1  ,IINDEX2+1)* ZINDEX2         &
                     -PLT2(IINDEX1  ,IINDEX2  )*(ZINDEX2 - 1.0)) * (ZINDEX1 - 1.0)
      ENDIF

      IF(PRESENT(PLT3)) THEN
        POUT3(JL) = ( PLT3(IINDEX2+1,IINDEX1+1)* ZINDEX1         &
                     -PLT3(IINDEX2+1,IINDEX1  )*(ZINDEX1 - 1.0)) *  ZINDEX2 &
                   -( PLT3(IINDEX2  ,IINDEX1+1)* ZINDEX1         &
                     -PLT3(IINDEX2  ,IINDEX1  )*(ZINDEX1 - 1.0)) * (ZINDEX2 - 1.0)
      ENDIF

    ELSE
      POUT1(JL)=0.
      IF(PRESENT(PLT2)) POUT2(JL)=0.
      IF(PRESENT(PLT3)) POUT3(JL)=0.
    ENDIF
  ENDDO

ENDIF
END SUBROUTINE INTERP_MICRO_2D
!
END SUBROUTINE ICE4_FAST_RS
END MODULE MODE_ICE4_FAST_RS
