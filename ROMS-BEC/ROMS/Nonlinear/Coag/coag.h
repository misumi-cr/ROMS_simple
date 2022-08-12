      SUBROUTINE coag (ng,tile)

      USE mod_param
#ifdef DIAGNOSTICS_COAG
      USE mod_diags
#endif
      USE mod_forces
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping

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

#ifdef PROFILE
         CALL wclock_on (ng, iNLM, 15)
#endif


         CALL coag_tile (ng, tile,                           &
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
#ifdef DIAGNOSTICS_COAG
     &        DIAGS(ng) % DiaBio2d,                          &
     &        DIAGS(ng) % DiaBio3d,                          &
#endif
     &        OCEAN(ng) % t)

#ifdef PROFILE
         CALL wclock_off (ng, iNLM, 15)
#endif
         RETURN
         END SUBROUTINE coag
!
!-----------------------------------------------------------------------
      SUBROUTINE coag_tile    (ng, tile,                                &
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
#ifdef DIAGNOSTICS_COAG
     &                         DiaBio2d, DiaBio3d,                      &
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
      USE constants, ONLY : ppt_to_salt
      USE popmini_mod
      use prognostic, only : tracer_d, init_prognostic
      USE co2calc
      USE mod_coag
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
# ifdef DIAGNOSTICS_COAG
      real(r8), intent(inout) :: DiaBio2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaBio3d(LBi:,LBj:,:,:)
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
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,UBk,3,UBt)
#endif

!
!  Local variable declarations.
!

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,UBk,UBt) :: TRACER_nstp,TRACER_nnew
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,UBt) :: TRACER_SOURCE, cff
      real(r8), dimension(UBk) :: ds


      real(r8) :: tstrS, tstr0, tstr1, tstr2, tend0, tend1, tend2
      real(r8) :: atol, rtol, rpar, djac1
      real(r8), dimension(param_n_sections*param_n_layers) :: dvdt0
      real(r8), dimension(param_n_sections*param_n_layers) :: vconS, vcon0
      real(r8), dimension(param_n_sections) :: vconC
      integer  :: idid, ipar
      integer, parameter :: lrw = 10000000, liw = 10000000
      real(r8), dimension(lrw) :: rwork
      integer, dimension(liw)  :: iwork

      integer, dimension(15) :: info

      external calccoagderiv0, calccoagderiv1, calccoagderiv2

      integer :: inv_k, coag_ind_begin, coag_ind_end
      integer :: i, j, k, itim, itrc, icoag, ivar
      integer :: mmm
      integer :: nstr, nend
      integer :: krow, kcol, mistr, miend, mjstr, mjend

      integer :: DT_h, NT_h, N_h, N_e


!      logical (log_kind), save :: is_init = .true.
!      logical (log_kind), save :: ciso_on = .false.
!
!      integer, parameter :: bid = 1
!
!#ifdef BEC
!      real (r8), dimension(IminS:ImaxS,JminS:JmaxS,max_blocks_clinic) :: &
!         SHF_QSW_RAW,  &! penetrative solar heat flux, from coupler (degC*cm/s)
!         SHF_QSW,      &! SHF_QSW used by physics, may have diurnal cylce imposed (degC*cm/s)
!         U10_SQR,      &! 10m wind speed squared (cm/s)**2
!         IFRAC,        &! sea ice fraction (non-dimensional)
!         PRESS,        &! sea level atmospheric pressure (dyne/cm**2)
!         SST,          &! sea surface temperature (C)
!         SSS            ! sea surface salinity (psu)
!
!      real (r8), dimension(IminS:ImaxS,JminS:JmaxS,UBt,max_blocks_clinic) :: &
!         SURF_VALS_OLD, SURF_VALS_CUR, STF_MODULE
!
!      logical (log_kind) :: &
!         lexport_shared_vars ! flag to save shared_vars or not
!
!#endif
!
#include "set_bounds.h"
!
!#ifdef DIAGNOSTICS_BIO
!!
!!-----------------------------------------------------------------------
!! If appropriate, initialize diagnostic arrays.
!!-----------------------------------------------------------------------
!!
!      IF (((iic(ng).gt.ntsDIA(ng)).and.                                 &
!     &     (MOD(iic(ng),nDIA(ng)).eq.1)).or.                            &
!     &    ((iic(ng).ge.ntsDIA(ng)).and.(nDIA(ng).eq.1)).or.             &
!     &    ((nrrec(ng).gt.0).and.(iic(ng).eq.ntstart(ng)))) THEN
!        DO ivar=1,NDbio2d
!	  DiaBio2d(Istr:Iend,Jstr:Jend,ivar)=0.0_r8
!        END DO
!        DO ivar=1,NDbio3d
!	  DiaBio3d(Istr:Iend,Jstr:Jend,1:UBk,ivar)=0.0_r8
!        END DO
!      END IF
!
!#endif
!!
!!-----------------------------------------------------------------------


!
! Check consistency of vertical levels
!

  if (UBk .ne. param_n_layers) then
    write(*,*) '### Inconsistent vertical layers between ROMS and coag. ###'
    stop
  end if

!
! Get concentration of particles
!

       coag_ind_begin = idcoag(1)
       coag_ind_end   = idcoag(NCT)

       DO itrc = 1, UBt
         TRACER_nstp(Istr:Iend,Jstr:Jend,1:UBk,itrc)       &
                 = t(Istr:Iend,Jstr:Jend,UBk:1:-1,nstp,itrc)
         TRACER_nnew(Istr:Iend,Jstr:Jend,1:UBk,itrc)       &
                 = t(Istr:Iend,Jstr:Jend,UBk:1:-1,nnew,itrc)/Hz(Istr:Iend,Jstr:Jend,UBk:1:-1)
       END DO
       ds(:) = Hz(Istr,Jstr,UBk:1:-1)

!
! Set betas depending on the ocean state variables
!


       do krow = 1, param_n_layers
         do kcol = 1, param_n_layers

           if (krow .eq. kcol .and. krow .le. param_n_grw) then
             call calcgrowth(b(krow,kcol)%growth)
           end if

           if (krow .eq. kcol) then
             call calcsinkingloss(b(krow,kcol)%settle,ds(krow))
             call calcdisagg(b(krow,kcol)%disagg,b(krow,kcol)%redist)
             call calcremin (b(krow,kcol)%remin)
             if (krow .ne. param_n_layers) then
               call calcsinkinggain(b(krow+1,kcol)%gain,ds(krow+1),ds(krow))
             end if
           end if

         end do
       end do

       do krow = 1, param_n_layers
         do kcol = 1, param_n_layers

           mistr = 1                + param_n_sections * (krow-1)
           miend = param_n_sections + param_n_sections * (krow-1)
           mjstr = 1                + param_n_sections * (kcol-1)
           mjend = param_n_sections + param_n_sections * (kcol-1)

           ba%settle(mistr:miend,mjstr:mjend) = b(krow,kcol)%settle
           ba%gain  (mistr:miend,mjstr:mjend) = b(krow,kcol)%gain
           ba%growth(mistr:miend,mjstr:mjend) = b(krow,kcol)%growth
           ba%disagg(mistr:miend,mjstr:mjend) = b(krow,kcol)%disagg
           ba%redist(mistr:miend,mjstr:mjend) = b(krow,kcol)%redist
           ba%remin (mistr:miend,mjstr:mjend) = b(krow,kcol)%remin * param_f_rem(krow)

         end do
       end do

!
! Solve ODEs with mode split.
! This code solves only a single column (Istr,Jstr) to reduce computational cost.
!

       vcon0 = 0.d0

       do k = 1, param_n_layers
         nstr = 1                + param_n_sections * (k-1)
         nend = param_n_sections + param_n_sections * (k-1)
         vconS(nstr:nend) = 0.5d0 * ( &
             TRACER_nnew(Istr,Jstr,k,coag_ind_begin:coag_ind_end)   &
           + TRACER_nstp(Istr,Jstr,k,coag_ind_begin:coag_ind_end) )

         vconC=vconS(nstr:nend)
         do j = 1, param_n_sections
           if ( vconC(j) / param_av_vol(j)/ param_dwidth(j) .le. 1.d-30 ) then
             vconC(j) = 1.d-30 * param_av_vol(j) * param_dwidth(j)
           end if
         end do
         vconS(nstr:nend)=vconC

       end do

       vcon0 = vconS

       tstrS = tdays(ng)
       tstr0 = tstrS
       dvdt0 = 0.d0

       tend0 = tstr0 + DT(ng) / param_day_to_sec ! convert sec to day

       atol = 0.0
       rtol = 1.d-2
       info = 0
       info(6)  = 1
       info(11) = 1
       iwork(1) = param_n_sections+1
       iwork(2) = param_n_sections
       call ddassl(calccoagderiv0,param_n_sections*param_n_layers,tstr0,vcon0,dvdt0,tend0,&
                   info,rtol,atol,idid,rwork,lrw,iwork,liw,rpar,ipar,djac1)

       vcon0 = vcon0 - vconS

       DO k = 1, UBk
         nstr = 1                + param_n_sections * (k-1)
         nend = param_n_sections + param_n_sections * (k-1)
         DO j = Jstr, Jend
           DO i = Istr, Iend
             cff(i,j,coag_ind_begin:coag_ind_end) = vcon0(nstr:nend)
!!             t(i,j,UBk-k+1,nnew,coag_ind_begin:coag_ind_end) = vcon1                   ! it works for TS_FIXED
!!             t(i,j,UBk-k+1,nnew,coag_ind_begin:coag_ind_end) = vcon1 * Hz(i,j,UBk-k+1) ! it overestimates, likely due to treatment of t(nnew) is different in TS_FIXED
           END DO
         END DO

!
! Time integration of particle concentration.
!

         DO itrc=1,NCT
            icoag=idcoag(itrc)
            t(Istr:Iend,Jstr:Jend,UBk-k+1,nnew,icoag)   &
           =t(Istr:Iend,Jstr:Jend,UBk-k+1,nnew,icoag)   &
           +cff(Istr:Iend,Jstr:Jend,icoag)*Hz(Istr:Iend,Jstr:Jend,UBk-k+1)
         END DO
       END DO

!!-----------------------------------------------------------------------
!!  Accumulated diagnostic terms.
!!-----------------------------------------------------------------------
!!
!#ifdef DIAGNOSTICS_BIO
!        DO ivar=1,NDbio3d
!	   DiaBio3d(Istr:Iend,Jstr:Jend,UBk-k+1,ivar)  &
!	      =DiaBio3d(Istr:Iend,Jstr:Jend,UBk-k+1,ivar)+ &
!# ifdef    WET_DRY
!	   rmask_io(Istr:Iend,Jstr:Jend)*               &
!# endif
!	   bec_diag_3d(1:nx_block,1:ny_block,k,ivar,bid)
!        END DO
!        call accumulate_tavg_field_3d(TRACER_SOURCE(Istr:Iend,Jstr:Jend,fe_ind),iJ_Fe,bid,k)
!#endif
!
!      END DO
!
!#ifdef DIAGNOSTICS_BIO
!      DO ivar=1,NDbio2d
!         DiaBio2d(Istr:Iend,Jstr:Jend,ivar)  &
!            =DiaBio2d(Istr:Iend,Jstr:Jend,ivar)+ &
!#ifdef WET_DRY
!         rmask_io(Istr:Iend,Jstr:Jend)*               &
!#endif
!         bec_diag_2d(1:nx_block,1:ny_block,ivar,bid)
!      END DO
!#endif
!
      RETURN
      END SUBROUTINE coag_tile
