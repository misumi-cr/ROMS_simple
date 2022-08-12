!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#include "cppdefs.h"
 module prognostic
#if defined BEC

!BOP
! !MODULE: prognostic
! !DESCRIPTION:
!  This module contains all the prognostic variables used in POP.
!
! !REVISION HISTORY:
!  SVN:$Id: prognostic.F90 22881 2010-05-11 04:23:39Z njn01 $

! !USES:
#ifdef BEC
   use kinds_mod
!c   use blocks
!c   use domain_size
!c   use domain
   use constants
   use popmini_mod
   use mod_param, only : mt
#else
   use kinds_mod
   use blocks
   use domain_size
   use domain
   use constants
#endif

   implicit none
   public
   save

! !PUBLIC DATA TYPES:

   type, public :: tracer_field
      character(char_len) :: short_name
      character(char_len) :: long_name
      character(char_len) :: units
      character(char_len) :: tend_units
      character(char_len) :: flux_units
      real(rtavg)         :: scale_factor
      logical :: lfull_depth_tavg
   end type

!cj! !PUBLIC DATA MEMBERS:
!cj
!cj   real (r8), dimension(nx_block,ny_block,km,nt,3,max_blocks_clinic), &
!cj      target :: &
!cj      TRACER     ! 3d tracer fields for all blocks at 3 time levels
!cj
   type (tracer_field), dimension(:), allocatable :: &
      tracer_d   ! descriptors for each tracer
!cj
!cj   real (r8), dimension(nx_block,ny_block,km,3,max_blocks_clinic), &
!cj      target :: &
!cj      UVEL,     &! 3d horizontal velocity for all blocks at 3 time lvls
!cj      VVEL,     &! 3d horizontal velocity for all blocks at 3 time lvls
!cj      RHO        ! 3d density fields,     for all blocks at 3 time lvls
!cj
!cj   real (r8), dimension(nx_block,ny_block,3,max_blocks_clinic), &
!cj      target :: &
!cj      PSURF,    &! surface pressure for all blocks at 3 time levels
!cj      GRADPX,   &! surface-pressure gradient for all blocks at
!cj      GRADPY,   &!   3 time levels
!cj      UBTROP,   &! barotropic velocities for all blocks at
!cj      VBTROP     !   3 time levels 
!cj
!cj   real (r8), dimension(nx_block,ny_block,max_blocks_clinic), &
!cj      target :: &
!cj      PGUESS     ! next guess for surface pressure
!cj
!cj   integer (int_kind) :: &! time indices for prognostic arrays
!cj      curtime,           &! current time level  (n) 
!cj      newtime,           &! next time level     (n+1)
!cj      oldtime,           &! previous time level (n-1)
!cj      mixtime             ! set to oldtime on leafrog steps
!cj                          ! and to curtime on matsuno steps
!cj
!cj!EOP
!cj!BOC
!cj!EOC
!cj!***********************************************************************
!cj
 contains

!***********************************************************************
!BOP
! !IROUTINE: init_prognostic
! !INTERFACE:

 subroutine init_prognostic

!cj! !DESCRIPTION:
!cj!  This subroutine allocates prognostic arrays and intializes all
!cj!  prognostic arrays to zero.
!cj!
!cj! !REVISION HISTORY:
!cj!  same as module
!cj
!cj!EOP
!cj!BOC
!cj!-----------------------------------------------------------------------
!cj!
!cj!     initialize prognostic arrays to zero - they will be filled
!cj!     later with real values from restart or initialization.
!cj!     initialize time indices.
!cj!
!cj!-----------------------------------------------------------------------
!cj
!cj      oldtime = 1
!cj      curtime = 2
!cj      newtime = 3
!cj
!cj      TRACER  = c0
!cj      UVEL    = c0
!cj      VVEL    = c0
!cj      RHO     = c0
!cj      PSURF   = c0
!cj      GRADPX  = c0
!cj      GRADPY  = c0
!cj      UBTROP  = c0
!cj      VBTROP  = c0
!cj      PGUESS  = c0

      allocate(tracer_d(mt))

      tracer_d(1)%short_name = 'TEMP'
      tracer_d(1)%long_name  = 'Potential temperature'
      tracer_d(1)%units      = 'degC'
      tracer_d(1)%tend_units = 'degC/s'
      tracer_d(1)%flux_units = 'degC cm/s'
      tracer_d(1)%scale_factor = 1.0_rtavg 

      tracer_d(2)%short_name = 'SALT'
      tracer_d(2)%long_name  = 'Salinity'
      tracer_d(2)%units      = 'msu (g/g)'
      tracer_d(2)%tend_units = 'gram/kilogram/s'
      tracer_d(2)%flux_units = 'gram/kilogram cm/s'
      tracer_d(2)%scale_factor = 1000.0_rtavg

!-----------------------------------------------------------------------
!EOC

 end subroutine init_prognostic

!***********************************************************************
#endif
 end module prognostic

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
