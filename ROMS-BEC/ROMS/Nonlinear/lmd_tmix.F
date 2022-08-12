#include "cppdefs.h"
#define SASHA
      MODULE lmd_tmix_mod
#if defined NONLINEAR && defined LMD_TMIX && defined SOLVE3D
!
!svn $Id: lmd_bkpp.F 645 2013-01-22 23:21:54Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2013 The ROMS/TOMS Group       Scott M. Durski   !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine determines the depth of  bottom  oceanic boundary      !
!  layer,  hbbl,  as the deepest depth  where the bulk Richardson      !
!  number is equal to the critical value, Ric.                         !
!                                                                      !
!  Then,  it computes the vertical mixing coefficients  within the     !
!  boundary layer. They depend on surface forcing and the magnitude    !
!  and gradient of interior mixing below  the boundary layer.  The     !
!  ocean interior is allowed to force the boundary layer through a     !
!  dependence of the nondimensional vertical shape function G(sigma)   !
!  and its vertical derivative at  sigma=1  on the interior  mixing    !
!  coefficients, and it vertical derivative at d=hsbl. The boundary    !
!  layer mixing coefficients are computed by matching these values.    !
!                                                                      !
! Reference:                                                           !
!                                                                      !
!  Large, W.G., J.C. McWilliams, and S.C. Doney, 1994: A Review        !
!    and model with a nonlocal boundary layer parameterization,        !
!    Reviews of Geophysics, 32,363-403.                                !
!                                                                      !
!  This routine was adapted from Bill Large 1995 code.                 !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: lmd_tmix

      CONTAINS
!
!***********************************************************************
      SUBROUTINE lmd_tmix (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_forces
      USE mod_grid
      USE mod_mixing
      USE mod_ocean
      USE mod_stepping
      USE mod_tmix

!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
# include "tile.h"
!
      CALL lmd_tmix_tile (ng, tile,                                     &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    IminS, ImaxS, JminS, JmaxS,                   &
     &                    nstp(ng),                                     &
# ifdef MASKING
     &                    GRID(ng) % rmask,                             &
# endif
     &                    GRID(ng) % f,                                 &
     &                    GRID(ng) % h,                                 &
     &                    GRID(ng) % Hz,                                &
     &                    GRID(ng) % z_r,                               &
     &                    GRID(ng) % z_w,                               &
     &                    OCEAN(ng) % u,                                &
     &                    OCEAN(ng) % v,                                &
     &                    OCEAN(ng) % rho,                              &
     &                    D_TMIX(ng) % tmix,                            &
     &                    D_TMIX(ng) % tmix_topo,                       &
     &                    FORCES(ng) % btflx,                           &
     &                    FORCES(ng) % bustr,                           &
     &                    FORCES(ng) % bvstr,                           &
     &                    MIXING(ng) % alpha,                           &
# ifdef SALINITY
     &                    MIXING(ng) % beta,                            &
# endif
     &                    MIXING(ng) % bvf,                             &
     &                    MIXING(ng) % ksbl,                            &
     &                    MIXING(ng) % Akt,                             &
     &                    MIXING(ng) % Akv)
      RETURN
      END SUBROUTINE lmd_tmix
!
!***********************************************************************
      SUBROUTINE lmd_tmix_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          nstp,                                   &
# ifdef MASKING
     &                          rmask,                                  &
# endif
     &                          f, h, Hz, z_r, z_w,                     &
     &                          u, v, rho,                              &
     &                          tmix, tmix_topo,                        &
     &                          btflx,  bustr, bvstr,                   &
     &                          alpha,                                  &
# ifdef SALINITY
     &                          beta,                                   &
# endif
     &                          bvf,                                    &
     &                          ksbl, Akt, Akv)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
      USE bc_2d_mod, ONLY : bc_r2d_tile
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
# endif
# ifdef LMD_SHAPIRO
      USE shapiro_mod
# endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, Intent(in) :: nstp
!
# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: f(LBi:,LBj:)
      real(r8), intent(in) :: h(LBi:,LBj:)
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: u(LBi:,LBj:,:,:)
      real(r8), intent(in) :: v(LBi:,LBj:,:,:)
      real(r8), intent(in) :: rho(LBi:,LBj:,:)
      real(r8), intent(in) :: tmix(LBi:,LBj:)
      real(r8), intent(in) :: tmix_topo(LBi:,LBj:)
      real(r8), intent(in) :: btflx(LBi:,LBj:,:)
      real(r8), intent(in) :: bustr(LBi:,LBj:)
      real(r8), intent(in) :: bvstr(LBi:,LBj:)
      real(r8), intent(in) :: alpha(LBi:,LBj:)
#  ifdef SALINITY
      real(r8), intent(in) :: beta(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: bvf(LBi:,LBj:,0:)
      integer,  intent(in) :: ksbl(LBi:,LBj:)
      real(r8), intent(inout) :: Akt(LBi:,LBj:,0:,:)
      real(r8), intent(inout) :: Akv(LBi:,LBj:,0:)
# else
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: f(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: h(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:N(ng))
      real(r8), intent(in) :: u(LBi:UBi,LBj:UBj,N(ng),3)
      real(r8), intent(in) :: v(LBi:UBi,LBj:UBj,N(ng),3)
      real(r8), intent(in) :: rho(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: tmix(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: tmix_topo(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: btflx(LBi:UBi,LBj:UBj,NT(ng))
      real(r8), intent(in) :: bustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bvstr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: alpha(LBi:UBi,LBj:UBj)
#  ifdef SALINITY
      real(r8), intent(in) :: beta(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: bvf(LBi:UBi,LBj:UBj,0:N(ng))
      integer,  intent(in) :: ksbl(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: Akt(LBi:UBi,LBj:UBj,0:N(ng),NAT)
      real(r8), intent(inout) :: Akv(LBi:UBi,LBj:UBj,0:N(ng))
# endif
!
!  Local variable declarations.
!
      integer :: i, j, k, itrc

      real(r8), parameter :: gamma = 0.2E0_r8
      real(r8), parameter :: q = 0.6E0_r8
      real(r8), parameter :: zeta  = 1000.E0_r8 ! in meters
      real(r8), parameter :: Akt_max = 30.E-4_r8 ! in m2/sec
      real(r8), parameter :: dthres= 0.E0_r8 ! in meters
      real(r8)            :: Akt_tide, bvfmax

# include "set_bounds.h"

      Akt_tide = 0.0_r8

      DO k=1,N(ng)-1
        DO j=Jstr,Jend
          DO i=Istr,Iend
            bvfmax=max(1.e-7_r8,bvf(i,j,k))
            Akt_tide= &
     &        gamma*q*F_tide(z_w(i,j,k),abs(tmix_topo(i,j)),zeta) &
     &        / (rho(i,j,k)+1000.E0_r8) / bvfmax * tmix(i,j)
            Akt_tide=min(Akt_max,Akt_tide)
            if (abs(tmix_topo(i,j)) .ge. dthres) then
              Akt(i,j,k,itemp) = Akt(i,j,k,itemp) + Akt_tide
              Akt(i,j,k,isalt) = Akt(i,j,k,isalt) + Akt_tide
            end if
          END DO
        END DO
      END DO

      RETURN
      END SUBROUTINE lmd_tmix_tile
!
!***********************************************************************
      FUNCTION F_tide (z, H, zeta)
!***********************************************************************
!
      USE mod_kinds

      real(r8),intent(in)  :: z, H, zeta ! z: negative, H: positive
      real(r8)             :: F_tide, zrho

      if (abs(z).le.H) then
        zrho = z
      else
        zrho = -H
      end if

      F_tide = exp(-(H+zrho)/zeta)/zeta/(1.0_r8-exp(-H/zeta))

      RETURN
      END FUNCTION F_tide
#endif
      END MODULE lmd_tmix_mod
