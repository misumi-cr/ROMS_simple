#include "cppdefs.h"
      MODULE mod_tmix
#if defined LMD_TMIX
!
!svn $Id: mod_clima.F 645 2013-01-22 23:21:54Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2013 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Sea surface height fields.                                          !
!                                                                      !
!   ssh         Climatology for sea surface height (m).                !
!   sshG        Latest two-time snapshots of input "ssh" grided        !
!                 data used for interpolation.                         !
!   zeta_ads    Sensitivity functional for sea surface height.         !
!   zeta_adsF   Latest two-time snapshots of input "zeta_ads" grided   !
!                 data used fot interpolation.                         !
!                                                                      !
!  2D momentum fields.                                                 !
!                                                                      !
!   ubarclm     Vertically integrated U-momentum climatology (m/s).    !
!   ubarclmG    Latest two-time snapshots of input "ubarclm" grided    !
!                 data used for interpolation.                         !
!   ubar_ads    Sensitivity functional for vertically integrated       !
!                 U-momentum.                                          !
!   ubar_adsG   Latest two-time snapshots of input "ubar_ads" grided   !
!                 data used for interpolation.                         !
!   vbarclm     Vertically integrated V-momentum climatology (m/s).    !
!   vbarclmG    Latest two-time snapshots of input "vbarclm" grided    !
!                 data used for interpolation.                         !
!   vbar_ads    Sensitivity functional for vertically integrated       !
!                 V-momentum.                                          !
!   vbar_adsG   Latest two-time snapshots of input "vbar_ads" grided   !
!                 data used for interpolation.                         !
!                                                                      !
!  Tracer fields.                                                      !
!                                                                      !
!   tclm        Climatology for tracer type variables (usually,        !
!                 temperature: degC; salinity: PSU).                   !
!   tclmG       Latest two-time snapshots of input "tclm" grided       !
!                 data used for interpolation.                         !
!   t_ads       Sensitivity functional for tracer type variables.      !
!   t_adsG      Latest two-time snapshots of input "t_ads" grided      !
!                 data used for interpolation.                         !
!                                                                      !
!  3D momentum climatology.                                            !
!                                                                      !
!   uclm        3D U-momentum climatology (m/s).                       !
!   uclmG       Latest two-time snapshots of input "uclm" grided       !
!                 data used for interpolation.                         !
!   u_ads       Sensitivity functional for 3D U-momentum.              !
!   u_adsG      Latest two-time snapshots of input "u_ads" grided      !
!                 data used for interpolation.                         !
!   vclm        3D V-momentum climatology (m/s).                       !
!   vclmG       Latest two-time snapshots of input "vclm" grided       !
!                 data used for interpolation.                         !
!   v_ads       Sensitivity functional for 3D V-momentum.              !
!   v_adsG      Latest two-time snapshots of input "v_ads" grided      !
!                 data used for interpolation.                         !
!                                                                      !
!  Nudging variables.                                                  !
!                                                                      !
!   M2nudgcof   Time-scale (1/sec) coefficients for nudging towards    !
!                 2D momentum data.                                    !
!   M3nudgcof   Time-scale (1/sec) coefficients for nudging towards    !
!                 3D momentum data.                                    !
!   Tnudgcof    Time-scale (1/sec) coefficients for nudging towards    !
!                 tracer data.                                         !
!   Znudgcof    Time-scale (1/sec) coefficients for nudging towards    !
!                 sea surface height data.                             !
!                                                                      !
!=======================================================================
!
        USE mod_kinds

        implicit none

        TYPE T_TMIX
        real(r8), pointer :: tmix(:,:)
        real(r8), pointer :: tmixG(:,:,:)
        real(r8), pointer :: tmix_topo(:,:)
        real(r8), pointer :: tmix_topoG(:,:,:)
        END TYPE T_TMIX

        TYPE (T_TMIX), allocatable :: D_TMIX(:)

      CONTAINS

      SUBROUTINE allocate_tmix (ng, LBi, UBi, LBj, UBj)
!
!=======================================================================
!                                                                      !
!  This routine allocates all variables in the module for all nested   !
!  grids.                                                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
!  Local variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj
!
!-----------------------------------------------------------------------
!  Allocate module variables.
!-----------------------------------------------------------------------
!
      IF (ng.eq.1) allocate ( D_TMIX(Ngrids) )
!
      allocate ( D_TMIX(ng) % tmix (LBi:UBi,LBj:UBj  ) )
      allocate ( D_TMIX(ng) % tmixG(LBi:UBi,LBj:UBj,2) )
      allocate ( D_TMIX(ng) % tmix_topo (LBi:UBi,LBj:UBj  ) )
      allocate ( D_TMIX(ng) % tmix_topoG(LBi:UBi,LBj:UBj,2) )

      RETURN
      END SUBROUTINE allocate_tmix

      SUBROUTINE initialize_tmix (ng, tile)
!
!=======================================================================
!                                                                      !
!  This routine initialize all variables in the module using first     !
!  touch distribution policy. In shared-memory configuration, this     !
!  operation actually performs propagation of the  "shared arrays"     !
!  across the cluster, unless another policy is specified to           !
!  override the default.                                               !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, j
      integer :: itrc, k

      real(r8), parameter :: IniVal = 0.0_r8

# include "set_bounds.h"
!
!  Set array initialization range.
!
# ifdef DISTRIBUTE
      Imin=BOUNDS(ng)%LBi(tile)
      Imax=BOUNDS(ng)%UBi(tile)
      Jmin=BOUNDS(ng)%LBj(tile)
      Jmax=BOUNDS(ng)%UBj(tile)
# else
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        Imin=BOUNDS(ng)%LBi(tile)
      ELSE
        Imin=Istr
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
        Imax=BOUNDS(ng)%UBi(tile)
      ELSE
        Imax=Iend
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        Jmin=BOUNDS(ng)%LBj(tile)
      ELSE
        Jmin=Jstr
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        Jmax=BOUNDS(ng)%UBj(tile)
      ELSE
        Jmax=Jend
      END IF
# endif
!
!-----------------------------------------------------------------------
!  Initialize module variables.
!-----------------------------------------------------------------------
!
      DO j=Jmin,Jmax
        DO i=Imin,Imax
          D_TMIX(ng) % tmix (i,j  ) = IniVal
          D_TMIX(ng) % tmixG(i,j,1) = IniVal
          D_TMIX(ng) % tmixG(i,j,2) = IniVal
          D_TMIX(ng) % tmix_topo (i,j  ) = IniVal
          D_TMIX(ng) % tmix_topoG(i,j,1) = IniVal
          D_TMIX(ng) % tmix_topoG(i,j,2) = IniVal
        END DO
      END DO

      END SUBROUTINE initialize_tmix
#endif
      END MODULE mod_tmix
