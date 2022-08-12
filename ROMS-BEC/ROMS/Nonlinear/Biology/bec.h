      SUBROUTINE biology (ng,tile)

      USE mod_param
#ifdef DIAGNOSTICS_BIO
      USE mod_diags
#endif
      USE mod_forces
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
#ifdef LIGAND3D
      USE mod_ligand3d
#endif
#ifdef DICE
      USE mod_dice
#endif
#ifdef DUST
      USE mod_dust
#endif

!
!     Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!     Local variable declarations.
!
#include "tile.h"
!
!     Set header file name.
!
#ifdef DISTRIBUTE
      IF (Lbiofile(iNLM)) THEN
#else
         IF (Lbiofile(iNLM).and.(tile.eq.0)) THEN
#endif
            Lbiofile(iNLM)=.FALSE.
            BIONAME(iNLM)=__FILE__
         END IF
!
#ifdef PROFILE
         CALL wclock_on (ng, iNLM, 15)
#endif


         CALL biology_tile (ng, tile,                                      &
     &        LBi, UBi, LBj, UBj, N(ng), NT(ng),             &
     &        IminS, ImaxS, JminS, JmaxS,                    &
     &        nstp(ng), nnew(ng),                            &
#ifdef MASKING
     &        GRID(ng) % rmask,                              &
# if defined WET_DRY && defined DIAGNOSTICS_BIO
     &        GRID(ng) % rmask_io,                           &
# endif
#endif
     &        GRID(ng) % Hz,                                 &
     &        GRID(ng) % z_r,                                &
     &        GRID(ng) % z_w,                                &
     &        FORCES(ng) % srflx,                            &
     &        FORCES(ng) % stflx,                            &
#if defined(BULK_FLUXES)
     &        FORCES(ng) % Uwind,                            &
     &        FORCES(ng) % Vwind,                            &
     &        FORCES(ng) % Pair,                             &
#else
     &        FORCES(ng) % sustr,                            &
     &        FORCES(ng) % svstr,                            &
#endif
#if defined LIGAND3D
     &        L3d(ng) % lig3d,                               &
#endif
#if defined DICE
     &        D_ICE(ng) % ice,                               &
#endif
#if defined DUST
     &        D_DUST(ng) % dust,                               &
#endif
#ifdef DIAGNOSTICS_BIO
     &        DIAGS(ng) % DiaBio2d,                          &
     &        DIAGS(ng) % DiaBio3d,                          &
#endif
#ifdef CISO
     &        GRID(ng) % pm,                                 &
     &        GRID(ng) % pn,                                 &
#endif
     &        OCEAN(ng) % t)

#ifdef PROFILE
         CALL wclock_off (ng, iNLM, 15)
#endif
         RETURN
         END SUBROUTINE biology
!
!-----------------------------------------------------------------------
      SUBROUTINE biology_tile (ng, tile,                                &
     &                         LBi, UBi, LBj, UBj, UBk, UBt,            &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         nstp, nnew,                              &
#ifdef MASKING
     &                         rmask,                                   &
# if defined WET_DRY && defined DIAGNOSTICS_BIO
     &                         rmask_io,                                &
# endif
#endif
     &                         Hz, z_r, z_w,                            &
     &                         srflx,                                   &
     &                         stflx,                                   &
#if defined(BULK_FLUXES)
     &                         Uwind, Vwind, Pair,                      &
#else
     &                         sustr, svstr,                            &
#endif
#if defined LIGAND3D
     &                         lig3d,                                   &
#endif
#if defined DICE
     &                         ice,                                     &
#endif
#if defined DUST
     &                         dust,                                    &
#endif
#ifdef DIAGNOSTICS_BIO
     &                         DiaBio2d, DiaBio3d,                      &
#endif
#ifdef CISO
     &                         pm, pn,                                  &
#endif
     &                         t)
!-----------------------------------------------------------------------!
      USE mod_biology
      USE mod_kinds
      USE mod_param
      USE mod_scalars
      USE kinds_mod
      USE POP_KindsMod
      USE ecosys_mod
#ifdef CISO
      USE ecosys_ciso_mod
#endif
      USE constants, ONLY : ppt_to_salt
      USE popmini_mod
      use prognostic, only : tracer_d, init_prognostic
      USE co2calc
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, UBk, UBt
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew

#ifdef ASSUMED_SHAPE
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#  if defined WET_DRY && defined DIAGNOSTICS_BIO
      real(r8), intent(in) :: rmask_io(LBi:,LBj:)
#  endif
# endif
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: srflx(LBi:,LBj:)
      real(r8), intent(in) :: stflx(LBi:,LBj:,:)
# if defined(BULK_FLUXES)
      real(r8), intent(in) :: Uwind(LBi:,LBj:)
      real(r8), intent(in) :: Vwind(LBi:,LBj:)
      real(r8), intent(in) :: Pair (LBi:,LBj:)
# else
      real(r8), intent(in) :: sustr(LBi:,LBj:)
      real(r8), intent(in) :: svstr(LBi:,LBj:)
# endif
# ifdef DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaBio3d(LBi:,LBj:,:,:)
# endif
# ifdef LIGAND3D
      real(r8), intent(in) :: lig3d(LBi:, LBj:, :)
# endif
# ifdef CISO
      real(r8), intent(inout) :: pm(LBi:,LBj:)
      real(r8), intent(inout) :: pn(LBi:,LBj:)
# endif
# ifdef DICE
      real(r8), intent(in) :: ice(LBi:, LBj:)
# endif
# ifdef DUST
      real(r8), intent(in) :: dust(LBi:, LBj:)
# endif
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)
#else
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#  if defined WET_DRY && defined DIAGNOSTICS_BIO
      real(r8), intent(in) :: rmask_io(LBi:UBi,LBj:UBj)
#  endif
# endif
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:UBk)
      real(r8), intent(in) :: srflx(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: stflx(LBi:UBi,LBj:UBj,UBt)
# ifdef DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:UBi,LBj:UBj,NDbio2d)
      real(r8), intent(inout) :: DiaBio3d(LBi:UBi,LBj:UBj,UBk,NDbio3d)
# endif
# ifdef LIGAND3D
      real(r8), intent(in) :: lig3d(LBi:UBi,LBj:UBj,N(ng))
# endif
# ifdef DICE
      real(r8), intent(in) :: ice(LBi:UBi,LBj:UBj)
# endif
# ifdef CISO
      real(r8), intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pn(LBi:UBi,LBj:UBj)
# endif
# ifdef DUST
      real(r8), intent(in) :: dust(LBi:UBi,LBj:UBj)
# endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,UBk,3,UBt)
#endif

!
!  Local variable declarations.
!

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,UBk,UBt) :: TRACER_nstp,TRACER_nnew
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,UBt) :: TRACER_SOURCE, cff

!c      integer :: nx_block, ny_block
      integer :: inv_k, ecosys_ind_begin, ecosys_ind_end
#ifdef CISO
      integer :: ecosys_ciso_ind_begin, ecosys_ciso_ind_end
#endif
      integer :: i, j, k, itim, itrc, ibio, ivar
      logical (log_kind), save :: is_init = .true.
#ifdef CISO
      logical (log_kind), save :: ciso_on = .true.
#else
      logical (log_kind), save :: ciso_on = .false.
#endif

      integer, parameter :: bid = 1

#ifdef BEC
      real (r8), dimension(IminS:ImaxS,JminS:JmaxS,max_blocks_clinic) :: &
         SHF_QSW_RAW,  &! penetrative solar heat flux, from coupler (degC*cm/s)
         SHF_QSW,      &! SHF_QSW used by physics, may have diurnal cylce imposed (degC*cm/s)
         U10_SQR,      &! 10m wind speed squared (cm/s)**2
         IFRAC,        &! sea ice fraction (non-dimensional)
         PRESS,        &! sea level atmospheric pressure (dyne/cm**2)
         SST,          &! sea surface temperature (C)
         SSS            ! sea surface salinity (psu)

      real (r8), dimension(IminS:ImaxS,JminS:JmaxS,UBt,max_blocks_clinic) :: &
         SURF_VALS_OLD, SURF_VALS_CUR, STF_MODULE

!c      logical (log_kind) :: &
!c         lexport_shared_vars ! flag to save shared_vars or not

#endif

#include "set_bounds.h"

#ifdef DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
! If appropriate, initialize diagnostic arrays.
!-----------------------------------------------------------------------
!
      IF (((iic(ng).gt.ntsDIA(ng)).and.                                 &
     &     (MOD(iic(ng),nDIA(ng)).eq.1)).or.                            &
     &    ((iic(ng).ge.ntsDIA(ng)).and.(nDIA(ng).eq.1)).or.             &
     &    ((nrrec(ng).gt.0).and.(iic(ng).eq.ntstart(ng)))) THEN
        DO ivar=1,NDbio2d
	  DiaBio2d(Istr:Iend,Jstr:Jend,ivar)=0.0_r8
        END DO
        DO ivar=1,NDbio3d
	  DiaBio3d(Istr:Iend,Jstr:Jend,1:UBk,ivar)=0.0_r8
        END DO
      END IF

#endif
!
!-----------------------------------------------------------------------
!
!c      nx_block = Iend - Istr + 1
!c      ny_block = Jend - Jstr + 1

#ifdef CISO
      ecosys_ind_begin = idbio(1)
      ecosys_ind_end   = idbio(27)
      ecosys_ciso_ind_begin = idbio(28)
      ecosys_ciso_ind_end   = idbio(NBT)
#else
      ecosys_ind_begin = idbio(1)
      ecosys_ind_end   = idbio(NBT)
#endif

      if (is_init)then
#ifdef CISO
         CALL popmini_init(Istr, Iend, Jstr, Jend, UBk, pm(Istr:Iend,Jstr:Jend), pn(Istr:Iend,Jstr:Jend))
#else
         CALL popmini_init(Istr, Iend, Jstr, Jend, UBk)
#endif
         CALL init_prognostic
         CALL ecosys_init(tracer_d)
#ifdef CISO
         CALL ecosys_ciso_init(tracer_d)
#endif
!cj         CALL ecosys_roms_init(nx_block, ny_block, Ubk)
         CALL co2calc_roms_init
         is_init = .false.
      end if

      CALL popmini_set_boundary(Hz(Istr:Iend,Jstr:Jend,1:UBk), &
           z_r(Istr:Iend,Jstr:Jend,1:UBk),z_w(Istr:Iend,Jstr:Jend,1:UBk))
#ifdef DUST
      CALL ecosys_roms_set_boundary(rho0, Cp, rmask(Istr:Iend,Jstr:Jend),dust(Istr:Iend,Jstr:Jend))
#else
      CALL ecosys_roms_set_boundary(rho0, Cp, rmask(Istr:Iend,Jstr:Jend))
#endif
#ifdef CISO
      CALL ecosys_roms_ciso_set_boundary(rmask(Istr:Iend,Jstr:Jend))
#endif

      DO itrc = 1 ,UBt
         TRACER_nstp(Istr:Iend,Jstr:Jend,1:UBk,itrc)       &
                 = t(Istr:Iend,Jstr:Jend,UBk:1:-1,nstp,itrc)
         TRACER_nnew(Istr:Iend,Jstr:Jend,1:UBk,itrc)       &
                 = t(Istr:Iend,Jstr:Jend,UBk:1:-1,nnew,itrc)/Hz(Istr:Iend,Jstr:Jend,UBk:1:-1)

         SURF_VALS_OLD(Istr:Iend,Jstr:Jend,itrc,bid) &
                 = t(Istr:Iend,Jstr:Jend,UBk,nstp,itrc)
         SURF_VALS_CUR(Istr:Iend,Jstr:Jend,itrc,bid) &
                 = t(Istr:Iend,Jstr:Jend,UBk,nnew,itrc)/Hz(Istr:Iend,Jstr:Jend,UBk)
      END DO

      TRACER_nstp(:,:,:,isalt) = TRACER_nstp(:,:,:,isalt)*ppt_to_salt           !conv [ppt] -> [g/g]
      TRACER_nnew(:,:,:,isalt) = TRACER_nnew(:,:,:,isalt)*ppt_to_salt           !conv [ppt] -> [g/g]

      SHF_QSW_RAW(Istr:Iend,Jstr:Jend,bid) = srflx(Istr:Iend,Jstr:Jend) * 100.0 !conv [degC m/sec] -> [degC cm/sec]
      SHF_QSW    (Istr:Iend,Jstr:Jend,bid) = srflx(Istr:Iend,Jstr:Jend) * 100.0 !conv [degC m/sec] -> [degC cm/sec]

      SST        (Istr:Iend,Jstr:Jend,bid) = t(Istr:Iend,Jstr:Jend,UBk,nstp,itemp)
      SSS        (Istr:Iend,Jstr:Jend,bid) = t(Istr:Iend,Jstr:Jend,UBk,nstp,isalt)

      IFRAC      (Istr:Iend,Jstr:Jend,bid) = 0.0_r8

      U10_SQR    (Istr:Iend,Jstr:Jend,bid) &                                    !conv [m/sec] -> [cm/sec]^2
                 = Uwind(Istr:Iend,Jstr:Jend)*Uwind(Istr:Iend,Jstr:Jend)*1.d4 &
                 + Vwind(Istr:Iend,Jstr:Jend)*Vwind(Istr:Iend,Jstr:Jend)*1.d4

      PRESS      (Istr:Iend,Jstr:Jend,bid) = Pair(Istr:Iend,Jstr:Jend)*1.d3    !conv [mb] -> [dyne/cm**2]

#ifdef DICE
      IFRAC(Istr:Iend,Jstr:Jend,bid) = ice(Istr:Iend,Jstr:Jend)
#endif

      CALL ecosys_set_sflux(                             &
           SHF_QSW_RAW  (Istr:Iend, Jstr:Jend, bid:bid), &
           SHF_QSW      (Istr:Iend, Jstr:Jend, bid:bid), &
           U10_SQR      (Istr:Iend, Jstr:Jend, bid:bid), &
           IFRAC        (Istr:Iend, Jstr:Jend, bid:bid), &
           PRESS        (Istr:Iend, Jstr:Jend, bid:bid), &
           SST          (Istr:Iend, Jstr:Jend, bid:bid), &
           SSS          (Istr:Iend, Jstr:Jend, bid:bid), &
           SURF_VALS_OLD(Istr:Iend, Jstr:Jend, ecosys_ind_begin:ecosys_ind_end, bid:bid), &
           SURF_VALS_CUR(Istr:Iend, Jstr:Jend, ecosys_ind_begin:ecosys_ind_end, bid:bid), &
           STF_MODULE   (Istr:Iend, Jstr:Jend, ecosys_ind_begin:ecosys_ind_end, bid:bid), &
           ciso_on )

#ifdef CISO
      CALL ecosys_ciso_set_sflux(                        &
           SST          (Istr:Iend, Jstr:Jend, bid:bid), &
           SURF_VALS_OLD(Istr:Iend, Jstr:Jend, ecosys_ciso_ind_begin:ecosys_ciso_ind_end, bid:bid), &
           SURF_VALS_CUR(Istr:Iend, Jstr:Jend, ecosys_ciso_ind_begin:ecosys_ciso_ind_end, bid:bid), &
           STF_MODULE   (Istr:Iend, Jstr:Jend, ecosys_ciso_ind_begin:ecosys_ciso_ind_end, bid:bid)  &
	   )
#endif

      STF_MODULE = STF_MODULE * 1.d-2  ! nmol/cm2/sec -> mmol/m2/sec
      DO itrc = 1 ,UBt ! mmol/m2/sec -> mmol/m3/sec
         STF_MODULE(Istr:Iend,Jstr:Jend,itrc,bid) =  &
         STF_MODULE(Istr:Iend,Jstr:Jend,itrc,bid) / Hz(Istr:Iend,Jstr:Jend,UBk)
      END DO

      DO k=1,UBk
         CALL ecosys_set_interior(k,                     &
         TRACER_nstp(Istr:Iend,Jstr:Jend,k,itemp),       &
         TRACER_nnew(Istr:Iend,Jstr:Jend,k,itemp),       &
         TRACER_nstp(Istr:Iend,Jstr:Jend,k,isalt),       &
         TRACER_nnew(Istr:Iend,Jstr:Jend,k,isalt),       &
         TRACER_nstp(Istr:Iend,Jstr:Jend,k,          &
                     ecosys_ind_begin:ecosys_ind_end),   &
         TRACER_nnew(Istr:Iend,Jstr:Jend,k,          &
                     ecosys_ind_begin:ecosys_ind_end),   &
         TRACER_SOURCE(Istr:Iend,Jstr:Jend,              &
                       ecosys_ind_begin:ecosys_ind_end), &
#ifdef LIGAND3D
         lig3d      (Istr:Iend,Jstr:Jend,k),             &
#endif
#ifdef DICE
         ice        (Istr:Iend,Jstr:Jend),               &
#endif
         ciso_on)

#ifdef CISO
	 CALL ecosys_ciso_set_interior(k,                          &
         TRACER_nstp(Istr:Iend,Jstr:Jend,k,itemp),                 &
         TRACER_nnew(Istr:Iend,Jstr:Jend,k,itemp),                 &
         TRACER_nstp(Istr:Iend,Jstr:Jend,1:UBk,                    &
                     ecosys_ciso_ind_begin:ecosys_ciso_ind_end),   &
         TRACER_nnew(Istr:Iend,Jstr:Jend,1:UBk,                    &
                     ecosys_ciso_ind_begin:ecosys_ciso_ind_end),   &
         TRACER_SOURCE(Istr:Iend,Jstr:Jend,                        &
                       ecosys_ciso_ind_begin:ecosys_ciso_ind_end) )
#endif

           if (k .eq. 1) then
            cff = (TRACER_SOURCE + STF_MODULE(:,:,:,bid)) * dt(ng)
         else
            cff = TRACER_SOURCE * dt(ng)
         end if

         DO itrc=1,NBT
            ibio=idbio(itrc)
            t(Istr:Iend,Jstr:Jend,UBk-k+1,nnew,ibio)   &
           =t(Istr:Iend,Jstr:Jend,UBk-k+1,nnew,ibio)   &
           +cff(Istr:Iend,Jstr:Jend,ibio)*Hz(Istr:Iend,Jstr:Jend,UBk-k+1)
         END DO
!
!-----------------------------------------------------------------------
!  Accumulated diagnostic terms.
!-----------------------------------------------------------------------
!
#ifdef DIAGNOSTICS_BIO
        DO ivar=1,NDbio3d
	   DiaBio3d(Istr:Iend,Jstr:Jend,UBk-k+1,ivar)  &
	      =DiaBio3d(Istr:Iend,Jstr:Jend,UBk-k+1,ivar)+ &
# ifdef    WET_DRY
	   rmask_io(Istr:Iend,Jstr:Jend)*               &
# endif
	   bec_diag_3d(1:nx_block,1:ny_block,k,ivar,bid)
        END DO
#endif

      END DO

#ifdef DIAGNOSTICS_BIO
      DO ivar=1,NDbio2d
         DiaBio2d(Istr:Iend,Jstr:Jend,ivar)  &
            =DiaBio2d(Istr:Iend,Jstr:Jend,ivar)+ &
#ifdef WET_DRY
         rmask_io(Istr:Iend,Jstr:Jend)*               &
#endif
         bec_diag_2d(1:nx_block,1:ny_block,ivar,bid)
      END DO
#endif

      RETURN
      END SUBROUTINE biology_tile
