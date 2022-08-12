!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#include "cppdefs.h"
 module ecosys_mod
#if defined BEC

!BOP
! !MODULE: ecosys_mod
!
! !DESCRIPTION:
!
!  Multispecies ecosystem based on Doney et al. 1996, Moore et al., 2002
!  Based on POP Global NCAR Nitrogen Ecosystem Model
!  version 0.0 (June 15th, 1998) from S.C. Doney.
!  Based on Doney et al., 1996 model.
!  Climate and Global Dynamics, NCAR
!  (doney@whoi.edu)
!
!  Version 1.0
!  Multispecies, multiple limiting nutrient version of ecosystem
!  based on mixed layer model of Moore et al.(2002).  Implemented here with
!  fixed elemental ratios and including only the diatoms and small
!  phytoplankton, with a parameterization of calcification,
!  by Keith Lindsay and Keith Moore, Fall 2001 - Spring 2002.
!  Calcification parameterization based on Moore et al. 2002.
!
!  Version 2.0, January 2003
!    Adds diazotrophs as a phytoplankton group, (based on Moore et al., 2002a)
!    Allows for variable fe/C for all phytoplankton groups
!     Allows for variable si/C for the diatoms
!     Adds explicit tracers for DON, DOP, DOFe
!     variable remin length scale for detrital soft POM and bSi f(temperature)
!     Extensive modifications to iron scavenging parameterization
!     Addition of a sedimentary dissolved iron source,
!        (implemented in ballast code as excess remin in bottom cell)
!        coded by J.K. Moore, (jkmoore@uci.edu)
!
!   Version 2.01. March 2003
!     corrected O2 bug
!     corrected grazing parameter z_grz bug at depth
!     dust dissolution at depth releases iron,
!     increased length scale for dust diss., increased hard fraction dust
!     no deep ocean reduction in scavenging rates,
!     increase bSi OC/ballast ratio 0.3 -> 0.35,
!     corrected bug in diazotroph photoadaptation, and diat and sp adapatation
!
!   Version 2.02.
!     corrected bug in Fe_scavenge (units for dust), May 2003
!     changed C/N/P ratios to 117/16/1 (Anderson & Sarmiento, 1994)
!
!   Version 2.03., July 2003
!     Remin of DOM no longer temperature dependent,
!     new iron scavenging parameterization added,
!     some dissolution of hard fraction of ballast materials added
!
!   Version 2.1, September 2003
!     modfied iron scavenging and dust dissolution at depth
!
!   Version 2.11, March 2004
!     fixed bug in iron scavenging code, replace dust and POC flux_in w/ flux_out
!
!   Version 2.12, April 2004 - Final version for GBC paper revision,
!     (Questions/comments, Keith Moore - jkmoore@uci.edu
!
!   References
!   Doney, S.C., Glover, D.M., Najjar, R.G., 1996. A new coupled, one-dimensional
!   biological-physical model for the upper ocean: applications to the JGOFS
!   Bermuda Time-Series Study (BATS) site. Deep-Sea Res. II, 43: 591-624.
!
!   Moore, JK, Doney, SC, Kleypas, JA, Glover, DM, Fung, IY, 2002. An intermediate
!   complexity marine ecosystem model for the global domain. Deep-Sea Res. II, 49:
!   403-462.
!
!   Moore, JK, Doney, SC, Glover, DM, Fung, IY, 2002. Iron cycling and nutrient
!   limitation patterns in surface waters of the world ocean. Deep-Sea Res. II,
!   49: 463-507.

! !REVISION HISTORY:

!  SVN:$Id:  $

!-----------------------------------------------------------------------
!  variables/subroutines/function used from other modules
!  The following are used extensively in this ecosys, so are used at
!  the module level. The use statements for variables that are only needed
!  locally are located at the module subprogram level.
!-----------------------------------------------------------------------

! !USES:

#ifdef BEC
   use mod_kinds
   use POP_KindsMod
   use kinds_mod
   use constants
   use ecosys_parms
   use popmini_mod          !DCC new module
   use mod_iounits          !for debug use (iounits of stdout)
   use prognostic

   use passive_tracer_tools
   use ecosys_share
   use ecosys_constants, only : ecosys_tracer_cnt
   use co2calc
#else
   use POP_KindsMod
   use POP_ErrorMod
   use POP_CommMod
   use POP_GridHorzMod
   use POP_FieldMod
   use POP_HaloMod

   use kinds_mod
   use constants
   use communicate
   use broadcast
   use global_reductions
   use blocks
   use domain_size
   use domain
   use exit_mod
   use prognostic
   use grid
   use io
   use io_types
   use io_tools
   use tavg
   use timers
   use passive_tracer_tools
   use named_field_mod
   use forcing_tools
   use time_management
   use ecosys_constants, only : ecosys_tracer_cnt
   use ecosys_parms
   use registry
   use named_field_mod
   use co2calc
   use ecosys_share
   use ecosys_restore_mod, only : ecosys_restore_type
#ifdef CCSMCOUPLED
   use POP_MCT_vars_mod
   use shr_strdata_mod
#endif
#endif

! !INPUT PARAMETERS:
!-----------------------------------------------------------------------
!  include ecosystem parameters
!  all variables from this modules have a parm_ prefix
!-----------------------------------------------------------------------

   implicit none
   save
   private

!-----------------------------------------------------------------------
!  public/private declarations
!-----------------------------------------------------------------------

   public :: &
#ifdef BEC
      ecosys_tracer_cnt,            &
      ecosys_set_interior,          &
      ecosys_roms_set_boundary,     &  !DCC new subroutine
!c      ecosys_roms_init                 !DCC new subroutine
      ecosys_init,                  &
      ecosys_set_sflux
#else
!c      ecosys_init,                  &
      ecosys_tracer_ref_val,        &
      ecosys_set_sflux,             &
      ecosys_tavg_forcing,          &
      ecosys_set_interior,          &
      ecosys_write_restart,         &
      ecosys_qsw_distrb_const
#endif

!EOP
!BOC

!-----------------------------------------------------------------------
!  flags controlling which portion of code are executed
!  usefull for debugging
!-----------------------------------------------------------------------

  logical (log_kind) :: &
     lsource_sink, &
     lflux_gas_o2, &
     lflux_gas_co2,&
     locmip_k1_k2_bug_fix

  logical (log_kind), dimension(:,:,:), allocatable :: &
     LAND_MASK

!-----------------------------------------------------------------------
!  non-autotroph relative tracer indices
!  autotroph relative tracer indices are in autotroph derived type and are determined at run time
!-----------------------------------------------------------------------
! *** note that these values must be consistent with bec_mod.h in ROMS-BEC ***
! *** note that these values must be consistently set for ecosys_init for routines setting tracer_d_module.
!-----------------------------------------------------------------------
  integer (int_kind), parameter :: &
     po4_ind           = 1,  &
     no3_ind           = 2,  &
     sio3_ind          = 3,  &
     nh4_ind           = 4,  &
     o2_ind            = 5,  &
     dic_ind           = 6,  &
     dic_alt_co2_ind   = 7,  &
     alk_ind           = 8,  &
     doc_ind           = 9,  &
     don_ind           = 10, &
     dop_ind           = 11, &
     dopr_ind          = 12, &
     donr_ind          = 13, &
     zooC_ind          = 14, &
     spChl_ind         = 15, &
     spC_ind           = 16, &
     spCaCO3_ind       = 17, &
     diatChl_ind       = 18, &
     diatC_ind         = 19, &
     diatSi_ind        = 20, &
     diazChl_ind       = 21, &
     diazC_ind         = 22

#ifndef FE_TAG
  integer  (int_kind), parameter :: &
     fe_ind             = 23, &
     dofe_ind           = 24, &
     spFe_ind           = 25, &
     diatFe_ind         = 26, &
     diazFe_ind         = 27
#else
   integer  (int_kind), parameter :: &
      fe0_ind           = 23, &
      dofe0_ind         = 24, &
      spFe0_ind         = 25, &
      diatFe0_ind       = 26, &
      diazFe0_ind       = 27, &
      zooFe0_ind        = 28

   integer (int_kind), parameter :: &
      fe1_ind           = 29, &
      dofe1_ind         = 30, &
      spFe1_ind         = 31, &
      diatFe1_ind       = 32, &
      diazFe1_ind       = 33, &
      zooFe1_ind        = 34

   integer (int_kind), parameter :: &
      fe2_ind           = 35, &
      dofe2_ind         = 36, &
      spFe2_ind         = 37, &
      diatFe2_ind       = 38, &
      diazFe2_ind       = 39, &
      zooFe2_ind        = 40

   integer (int_kind), parameter :: &
      fe3_ind           = 41, &
      dofe3_ind         = 42, &
      spFe3_ind         = 43, &
      diatFe3_ind       = 44, &
      diazFe3_ind       = 45, &
      zooFe3_ind        = 46

   integer (int_kind), parameter :: &
      fe4_ind           = 47, &
      dofe4_ind         = 48, &
      spFe4_ind         = 49, &
      diatFe4_ind       = 50, &
      diazFe4_ind       = 51, &
      zooFe4_ind        = 52

   integer (int_kind), parameter :: &
      fe5_ind           = 53, &
      dofe5_ind         = 54, &
      spFe5_ind         = 55, &
      diatFe5_ind       = 56, &
      diazFe5_ind       = 57, &
      zooFe5_ind        = 58

   integer (int_kind), parameter :: &
      fe6_ind           = 59, &
      dofe6_ind         = 60, &
      spFe6_ind         = 61, &
      diatFe6_ind       = 62, &
      diazFe6_ind       = 63, &
      zooFe6_ind        = 64

   integer (int_kind), parameter :: &
      fe7_ind           = 65, &
      dofe7_ind         = 66, &
      spFe7_ind         = 67, &
      diatFe7_ind       = 68, &
      diazFe7_ind       = 69, &
      zooFe7_ind        = 70
#endif

#if defined BEC && defined DIAGNOSTICS_BIO

   integer (int_kind), parameter :: &
      iphotoC_sp       = 1, &
      iphotoC_diat     = 2, &
      iphotoC_diaz     = 3

   integer (int_kind), parameter :: &
      ilight_lim_sp    = 4, &
      ilight_lim_diat  = 5, &
      ilight_lim_diaz  = 6

   integer (int_kind), parameter :: &
      iVNtot_sp        = 7, &
      iVFe_sp          = 8, &
      iVPtot_sp        = 9

   integer (int_kind), parameter :: &
      iVNtot_diat      = 10, &
      iVFe_diat        = 11, &
      iVPtot_diat      = 12, &
      iVSiO3_diat      = 13

   integer (int_kind), parameter :: &
      iVNtot_diaz      = 14, &
      iVFe_diaz        = 15, &
      iVPtot_diaz      = 16

# if !defined FE_TAG
   integer (int_kind), parameter :: &
      iFe_brate        = 17, &
      iFe_scav         = 18, &
      iFe_disag        = 19, &
      iFe_pgen         = 20, &
      iFe_premin       = 21, &
      iFe_hbio         = 22
# else
   integer (int_kind), parameter :: &
      iFe0_brate       = 17, &
      iFe0_scav        = 18, &
      iFe0_disag       = 19, &
      iFe0_pgen        = 20, &
      iFe0_premin      = 21, &
      iFe0_hbio        = 22

   integer (int_kind), parameter :: &
      iFe1_brate       = 23, &
      iFe1_scav        = 24, &
      iFe1_disag       = 25, &
      iFe1_pgen        = 26, &
      iFe1_premin      = 27, &
      iFe1_hbio        = 28

   integer (int_kind), parameter :: &
      iFe2_brate       = 29, &
      iFe2_scav        = 30, &
      iFe2_disag       = 31, &
      iFe2_pgen        = 32, &
      iFe2_premin      = 33, &
      iFe2_hbio        = 34

   integer (int_kind), parameter :: &
      iFe3_brate       = 35, &
      iFe3_scav        = 36, &
      iFe3_disag       = 37, &
      iFe3_pgen        = 38, &
      iFe3_premin      = 39, &
      iFe3_hbio        = 40

   integer (int_kind), parameter :: &
      iFe4_brate       = 41, &
      iFe4_scav        = 42, &
      iFe4_disag       = 43, &
      iFe4_pgen        = 44, &
      iFe4_premin      = 45, &
      iFe4_hbio        = 46

   integer (int_kind), parameter :: &
      iFe5_brate       = 47, &
      iFe5_scav        = 48, &
      iFe5_disag       = 49, &
      iFe5_pgen        = 50, &
      iFe5_premin      = 51, &
      iFe5_hbio        = 52

   integer (int_kind), parameter :: &
      iFe6_brate       = 53, &
      iFe6_scav        = 54, &
      iFe6_disag       = 55, &
      iFe6_pgen        = 56, &
      iFe6_premin      = 57, &
      iFe6_hbio        = 58

   integer (int_kind), parameter :: &
      iFe7_brate       = 59, &
      iFe7_scav        = 60, &
      iFe7_disag       = 61, &
      iFe7_pgen        = 62, &
      iFe7_premin      = 63, &
      iFe7_hbio        = 64
# endif
# if defined CISO
   integer (int_kind), parameter :: &
      iDIC_d13C        = 22, &
      iDIC_d14C        = 23
# endif

   integer (int_kind), parameter :: &
      ipCO2           = 1, &
      ipH             = 2, &
      iCO2_flux       = 3

   integer, parameter :: bid = 1

#endif

!-----------------------------------------------------------------------
!  derived type & parameter for tracer index lookup
!-----------------------------------------------------------------------

   type(ind_name_pair), dimension(ecosys_tracer_cnt) :: &
      ind_name_table

!-----------------------------------------------------------------------
!  options for forcing of gas fluxes
!-----------------------------------------------------------------------

   integer (int_kind), parameter :: &
      gas_flux_forcing_iopt_drv   = 1,   &
      gas_flux_forcing_iopt_file  = 2,   &
      atm_co2_iopt_const          = 1,   &
      atm_co2_iopt_drv_prog       = 2,   &
      atm_co2_iopt_drv_diag       = 3

   integer (int_kind) :: &
      gas_flux_forcing_iopt,             &
      atm_co2_iopt,                      &
      atm_alt_co2_iopt

   real (r8)       :: &
      atm_co2_const,                     &  ! value of atmospheric co2 (ppm, dry-air, 1 atm)
      atm_alt_co2_const                     ! value of atmospheric alternative co2 (ppm, dry-air, 1 atm)

   character(char_len) :: &
      gas_flux_forcing_file    ! file containing gas flux forcing fields

!-----------------------------------------------------------------------

   type(tracer_read) :: &
      gas_flux_fice,       & ! ice fraction for gas fluxes
      gas_flux_ws,         & ! wind speed for gas fluxes
      gas_flux_ap,         & ! atmospheric pressure for gas fluxes
      fesedflux_input        ! namelist input for iron_flux

!-----------------------------------------------------------------------
!  module variables related to ph computations
!-----------------------------------------------------------------------

   real (r8), dimension(:,:,:), allocatable :: &
      dust_FLUX_IN      ! dust flux not stored in STF since dust is not prognostic

#ifdef BEC
   real (r8), dimension(:,:,:), allocatable, target :: &
      PH_PREV,        & ! computed ph from previous time step
      PH_PREV_ALT_CO2,& ! computed ph from previous time step, alternative CO2
      IRON_PATCH_FLUX   ! localized iron patch flux

   real (r8), dimension(:,:,:,:), allocatable, target :: &
      PH_PREV_3D,      & ! computed pH_3D from previous time step
      PH_PREV_ALT_CO2_3D ! computed pH_3D from previous time step, alternative CO2
#endif

!-----------------------------------------------------------------------
!  restoring climatologies for nutrients
!-----------------------------------------------------------------------

#ifdef BEC
   real (r8), dimension(:), allocatable :: &
      nutr_rest_time_inv ! inverse restoring time scale for nutrients (1/secs)
#else
   real (r8), dimension(km) :: &
      nutr_rest_time_inv ! inverse restoring time scale for nutrients (1/secs)
#endif

   real (r8), dimension(:,:,:,:), allocatable, target :: &
      PO4_CLIM, NO3_CLIM, SiO3_CLIM

   real (r8), dimension(:,:,:,:), allocatable, target :: &
      FESEDFLUX      !  sedimentary Fe inputs

   character(char_len) :: &
      nutr_rest_file               ! file containing nutrient fields

!maltrud variable restoring
   logical (log_kind) :: &
      lnutr_variable_restore       ! geographically varying nutrient restoring

   character(char_len) :: &
      nutr_variable_rest_file,   & ! file containing variable restoring info
      nutr_variable_rest_file_fmt  ! format of file containing variable restoring info

   real (r8), dimension(:,:,:), allocatable, target :: &
      NUTR_RESTORE_RTAU            ! inverse restoring timescale for variable
                                   ! interior restoring

   integer (int_kind), dimension(:,:,:), allocatable :: &
      NUTR_RESTORE_MAX_LEVEL       ! maximum level for applying variable
                                   ! interior restoring

   real (r8), dimension(:,:,:,:), allocatable :: &
      INTERP_WORK                  ! temp array for interpolate_forcing output

   type(forcing_monthly_every_ts) :: &
      dust_flux,                 & ! surface dust flux
      iron_flux,                 & ! iron component of surface dust flux
      fice_file,                 & ! ice fraction, if read from file
      xkw_file,                  & ! a * wind-speed ** 2, if read from file
      ap_file                      ! atmoshperic pressure, if read from file

   character(char_len) :: &
      ndep_data_type               ! type of ndep forcing

   type(forcing_monthly_every_ts) :: &
      nox_flux_monthly,          & ! surface NOx species flux, added to nitrate pool
      nhy_flux_monthly             ! surface NHy species flux, added to ammonium pool

   integer (int_kind) :: &
      ndep_shr_stream_year_first, & ! first year in stream to use
      ndep_shr_stream_year_last,  & ! last year in stream to use
      ndep_shr_stream_year_align    ! align ndep_shr_stream_year_first with this model year

   integer (int_kind), parameter :: &
      ndep_shr_stream_var_cnt = 2, & ! number of variables in ndep shr_stream
      ndep_shr_stream_no_ind  = 1, & ! index for NO forcing
      ndep_shr_stream_nh_ind  = 2    ! index for NH forcing

   character(char_len) :: &
      ndep_shr_stream_file          ! file containing domain and input data

   real (r8) :: &
      ndep_shr_stream_scale_factor  ! unit conversion factor

#ifdef CCSMCOUPLED
   type(shr_strdata_type) :: ndep_sdat ! input data stream for ndep
#endif

   type(forcing_monthly_every_ts) :: &
      din_riv_flux,              & ! river DIN species flux, added to nitrate pool
      dip_riv_flux,              & ! river DIP species flux, added to phosphate pool
      don_riv_flux,              & ! river DON flux, added to semi-lab don pool
      dop_riv_flux,              & ! river DOP flux, added to semi-lab dop pool
      dsi_riv_flux,              & ! river DSI flux, added to dsi pool
      dfe_riv_flux,              & ! river dfe flux, added to dfe pool
      dic_riv_flux,              & ! river dic flux, added to dic pool
      alk_riv_flux,              & ! river alk flux, added to alk pool
      doc_riv_flux                 ! river doc flux, added to semi-labile DOC

!-----------------------------------------------------------------------
!  tavg ids and buffer indices (into ECO_SFLUX_TAVG) for 2d fields related to surface fluxes
!  duplicates, which are used for placing fields into multiple tavg streams,
!  do not need separate buffer indices
!  fields that are recoverable from the STF field do not need separate buffer indices
!-----------------------------------------------------------------------

#ifndef BEC
   integer (int_kind) :: &
      tavg_ECOSYS_IFRAC,          buf_ind_ECOSYS_IFRAC,          &! ice fraction
      tavg_ECOSYS_IFRAC_2,                                       &! ice fraction duplicate
      tavg_ECOSYS_XKW,            buf_ind_ECOSYS_XKW,            &! xkw
      tavg_ECOSYS_XKW_2,                                         &! xkw duplicate
      tavg_ECOSYS_ATM_PRESS,      buf_ind_ECOSYS_ATM_PRESS,      &! atmospheric pressure
      tavg_PV_O2,                 buf_ind_PV_O2,                 &! o2 piston velocity
      tavg_SCHMIDT_O2,            buf_ind_SCHMIDT_O2,            &! O2 schmidt number
      tavg_O2SAT,                 buf_ind_O2SAT,                 &! O2 saturation
      tavg_O2_GAS_FLUX_2,                                        &! O2 flux duplicate
      tavg_CO2STAR,               buf_ind_CO2STAR,               &! co2star
      tavg_DCO2STAR,              buf_ind_DCO2STAR,              &! dco2star
      tavg_pCO2SURF,              buf_ind_pCO2SURF,              &! surface pco2
      tavg_DpCO2,                 buf_ind_DpCO2,                 &! delta pco2
      tavg_DpCO2_2,                                              &! delta pco2 duplicate
      tavg_PV_CO2,                buf_ind_PV_CO2,                &! co2 piston velocity
      tavg_SCHMIDT_CO2,           buf_ind_SCHMIDT_CO2,           &! co2 schmidt number
      tavg_DIC_GAS_FLUX,          buf_ind_DIC_GAS_FLUX,          &! dic flux
      tavg_DIC_GAS_FLUX_2,                                       &! dic flux duplicate
      tavg_PH,                    buf_ind_PH,                    &! surface pH
      tavg_ATM_CO2,               buf_ind_ATM_CO2,               &! atmospheric CO2
      tavg_CO2STAR_ALT_CO2,       buf_ind_CO2STAR_ALT_CO2,       &! co2star alternative CO2
      tavg_DCO2STAR_ALT_CO2,      buf_ind_DCO2STAR_ALT_CO2,      &! dco2star alternative CO2
      tavg_pCO2SURF_ALT_CO2,      buf_ind_pCO2SURF_ALT_CO2,      &! surface pco2 alternative CO2
      tavg_DpCO2_ALT_CO2,         buf_ind_DpCO2_ALT_CO2,         &! delta pco2 alternative CO2
      tavg_DIC_GAS_FLUX_ALT_CO2,  buf_ind_DIC_GAS_FLUX_ALT_CO2,  &! dic flux alternative CO2
      tavg_PH_ALT_CO2,            buf_ind_PH_ALT_CO2,            &! surface pH alternative CO2
      tavg_ATM_ALT_CO2,           buf_ind_ATM_ALT_CO2,           &! atmospheric alternative CO2
      tavg_IRON_FLUX,             buf_ind_IRON_FLUX,             &! iron flux
      tavg_DUST_FLUX,                                            &! dust flux
      tavg_NOx_FLUX,              buf_ind_NOx_FLUX,              &! nox flux
      tavg_NHy_FLUX,                                             &! nhy flux
      tavg_DIN_RIV_FLUX,          buf_ind_DIN_RIV_FLUX,          &! din river flux
      tavg_DIP_RIV_FLUX,                                         &! dip river flux
      tavg_DON_RIV_FLUX,                                         &! don river flux
      tavg_DONr_RIV_FLUX,                                        &! donr river flux
      tavg_DOP_RIV_FLUX,                                         &! dop river flux
      tavg_DOPr_RIV_FLUX,                                        &! dopr river flux
      tavg_DSI_RIV_FLUX,                                         &! dsi river flux
      tavg_DFE_RIV_FLUX,          buf_ind_DFE_RIV_FLUX,          &! dfe river flux
      tavg_DIC_RIV_FLUX,          buf_ind_DIC_RIV_FLUX,          &! dic river flux
      tavg_ALK_RIV_FLUX,          buf_ind_ALK_RIV_FLUX,          &! alk river flux
      tavg_DOC_RIV_FLUX                                           ! doc river flux

!-----------------------------------------------------------------------
!  define tavg id for nonstandard 2d fields
!-----------------------------------------------------------------------

   integer (int_kind) :: &
      tavg_O2_ZMIN,      &! tavg id for vertical minimum of O2
      tavg_O2_ZMIN_DEPTH  ! tavg id for depth of vertical minimum of O2

!-----------------------------------------------------------------------
!  define tavg id for nonstandard 3d fields
!-----------------------------------------------------------------------

   integer (int_kind) :: &
      tavg_O2_PRODUCTION,&! tavg id for o2 production
      tavg_O2_CONSUMPTION,&! tavg id for o2 consumption
      tavg_AOU,          &! tavg id for AOU
      tavg_PAR_avg,      &! tavg id for available radiation avg over mixed layer
      tavg_POC_FLUX_IN,  &! tavg id for poc flux into cell
      tavg_POC_PROD,     &! tavg id for poc production
      tavg_POC_REMIN,    &! tavg id for poc remineralization
      tavg_POC_ACCUM,    &! tavg id for poc accumulation
      tavg_CaCO3_FLUX_IN,&! tavg id for caco3 flux into cell
      tavg_CaCO3_PROD,   &! tavg id for caco3 production
      tavg_CaCO3_REMIN,  &! tavg id for caco3 remineralization
      tavg_SiO2_FLUX_IN, &! tavg id for sio2 flux into cell
      tavg_SiO2_PROD,    &! tavg id for sio2 production
      tavg_SiO2_REMIN,   &! tavg id for sio2 remineralization
      tavg_dust_FLUX_IN, &! tavg id for dust flux into cell
      tavg_dust_REMIN,   &! tavg id for dust remineralization
      tavg_P_iron_FLUX_IN, &! tavg id for p_iron flux into cell
      tavg_P_iron_PROD,    &! tavg id for p_iron production
      tavg_P_iron_REMIN,   &! tavg id for p_iron remineralization
      tavg_auto_graze_TOT   ! tavg id for total autotroph grazing


!-----------------------------------------------------------------------
!  define tavg id for MORE nonstandard 3d fields
!-----------------------------------------------------------------------

   integer (int_kind) :: &
      tavg_photoC_TOT,           &! tavg id for total C fixation
      tavg_photoC_TOT_zint,      &! tavg id for total C fixation vertical integral
      tavg_photoC_NO3_TOT,       &! tavg id for total C fixation from NO3
      tavg_photoC_NO3_TOT_zint    ! tavg id for total C fixation from NO3 vertical integral

!-----------------------------------------------------------------------
!  define tavg id for MORE nonstandard 3d fields
!-----------------------------------------------------------------------

   integer (int_kind) :: &
      tavg_DOC_prod,       &! tavg id for doc production
      tavg_DOC_remin,      &! tavg id for doc remineralization
      tavg_DON_prod,       &! tavg id for don production
      tavg_DON_remin,      &! tavg id for don remineralization
      tavg_DOFe_prod,      &! tavg id for dofe production
      tavg_DOFe_remin,     &! tavg id for dofe remineralization
      tavg_DOP_prod,       &! tavg id for dop production
      tavg_DOP_remin,      &! tavg id for dop remineralization
      tavg_Fe_scavenge,    &! tavg id for iron scavenging
      tavg_Fe_scavenge_rate,   &! tavg id for iron scavenging rate
      tavg_NITRIF,         &! tavg id for nitrification
      tavg_DENITRIF,       &! tavg id for denitrification
      tavg_DONr_remin,     &! tavg id for DONrefractory remin
      tavg_DOPr_remin       ! tavg id for DOPrefractory remin

   integer (int_kind), dimension(zooplankton_cnt) :: &
        tavg_zoo_loss,      & ! tavg id for zooplankton loss
        tavg_zoo_loss_poc,  & ! tavg id for zooplankton loss to poc
        tavg_zoo_loss_doc,  & ! tavg id for zooplankton loss to doc
        tavg_zoo_graze,     & ! tavg id for zooplankton grazing
        tavg_zoo_graze_poc, & ! tavg id for zooplankton grazing to poc
        tavg_zoo_graze_doc, & ! tavg id for zooplankton grazing to doc
        tavg_zoo_graze_zoo, & ! tavg id for zooplankton grazing to zoo
        tavg_x_graze_zoo     ! tavg id for zooplankton grazing assimilation

   integer (int_kind), dimension(autotroph_cnt) :: &
      tavg_N_lim,          &! tavg id for N limitation
      tavg_P_lim,          &! tavg id for P limitation
      tavg_Fe_lim,         &! tavg id for Fe limitation
      tavg_SiO3_lim,       &! tavg id for SiO3 limitation
      tavg_light_lim,      &! tavg id for light limitation
      tavg_photoC,         &! tavg id for C fixation
      tavg_photoC_zint,    &! tavg id for C fixation vertical integral
      tavg_photoC_NO3,     &! tavg id for C fixation from NO3
      tavg_photoC_NO3_zint,&! tavg id for C fixation from NO3 vertical integral
      tavg_photoFe,        &! tavg id for Fe uptake
      tavg_photoNO3,       &! tavg id for NO3 uptake
      tavg_photoNH4,       &! tavg id for NH4 uptake
      tavg_DOP_uptake,     &! tavg id for DOP uptake
      tavg_PO4_uptake,     &! tavg id for PO4 uptake
      tavg_auto_graze,     &! tavg id for autotroph grazing
      tavg_auto_graze_poc, &! tavg id for autotroph grazing to poc
      tavg_auto_graze_doc, &! tavg id for autotroph grazing to doc
      tavg_auto_graze_zoo, &! tavg id for autotroph grazing to zoo
      tavg_auto_loss,      &! tavg id for autotroph loss
      tavg_auto_loss_poc,  &! tavg id for autotroph loss to poc
      tavg_auto_loss_doc,  &! tavg id for autotroph loss to doc
      tavg_auto_agg,       &! tavg id for autotroph aggregate
      tavg_bSi_form,       &! tavg id for Si uptake
      tavg_CaCO3_form,     &! tavg id for CaCO3 formation
      tavg_CaCO3_form_zint,&! tavg id for CaCO3 formation vertical integral
      tavg_Nfix             ! tavg id for N fixation

   integer (int_kind) :: &
      tavg_tot_bSi_form,       &! tavg id for Si uptake
      tavg_tot_CaCO3_form,     &! tavg id for CaCO3 formation
      tavg_tot_CaCO3_form_zint,&! tavg id for CaCO3 formation vertical integral
      tavg_tot_Nfix             ! tavg id for N fixation

#endif

   integer (int_kind) :: &
      tavg_CO3,            &! tavg id for 3D carbonate ion
      tavg_HCO3,           &! tavg id for 3D bicarbonate ion
      tavg_H2CO3,          &! tavg id for 3D carbonic acid
      tavg_pH_3D,          &! tavg id for 3D pH
      tavg_CO3_ALT_CO2,    &! tavg id for 3D carbonate ion, alternative CO2
      tavg_HCO3_ALT_CO2,   &! tavg id for 3D bicarbonate ion, alternative CO2
      tavg_H2CO3_ALT_CO2,  &! tavg id for 3D carbonic acid, alternative CO2
      tavg_pH_3D_ALT_CO2,  &! tavg id for 3D pH, alternative CO2
      tavg_co3_sat_calc,   &! tavg id for co3 concentration at calcite saturation
      tavg_zsatcalc,       &! tavg id for calcite saturation depth
      tavg_co3_sat_arag,   &! tavg id for co3 concentration at aragonite saturation
      tavg_zsatarag         ! tavg id for aragonite saturation depth

#ifndef BEC
   integer (int_kind) :: &
      tavg_calcToSed,      &! tavg id for calcite flux sedimentary burial
      tavg_pocToSed,       &! tavg id for poc burial flux to sediments
      tavg_ponToSed,       &! tavg id for pon burial flux to sediments
      tavg_popToSed,       &! tavg id for pop burial flux to sediments
      tavg_bsiToSed,       &! tavg id for bsi burial flux to sediments
      tavg_dustToSed,      &! tavg id for dust burial flux to sediments
      tavg_pfeToSed,       &! tavg id for pFe burial flux to sediments
      tavg_SedDenitrif,    &! tavg id for sedimentary denitrification
      tavg_OtherRemin       ! tavg id for non-oxic, non-denitr sed remin

   integer (int_kind) :: &
      tavg_Jint_Ctot,      &! tavg id for vertical integral of conservative subterms of source sink term for Ctot
      tavg_Jint_100m_Ctot, &! tavg id for vertical integral of conservative subterms of source sink term for Ctot, 0-100m
      tavg_Jint_Ntot,      &! tavg id for vertical integral of conservative subterms of source sink term for Ntot
      tavg_Jint_100m_Ntot, &! tavg id for vertical integral of conservative subterms of source sink term for Ntot, 0-100m
      tavg_Jint_Ptot,      &! tavg id for vertical integral of conservative subterms of source sink term for Ptot
      tavg_Jint_100m_Ptot, &! tavg id for vertical integral of conservative subterms of source sink term for Ptot, 0-100m
      tavg_Jint_Sitot,     &! tavg id for vertical integral of conservative subterms of source sink term for Sitot
      tavg_Jint_100m_Sitot  ! tavg id for vertical integral of conservative subterms of source sink term for Sitot, 0-100m
#endif

!-----------------------------------------------------------------------
!  define array for holding flux-related quantities that need to be time-averaged
!  this is necessary since the forcing routines are called before tavg flags
!-----------------------------------------------------------------------

#ifdef BEC
   real (r8), dimension(:,:,:,:), allocatable :: &
      ECO_SFLUX_TAVG

!-----------------------------------------------------------------------
!  average surface tracer value related variables
!  used as reference value for virtual flux computations
!-----------------------------------------------------------------------

   logical (log_kind), dimension(ecosys_tracer_cnt) :: &
      vflux_flag                ! which tracers get virtual fluxes applied

   integer (int_kind) :: &
      comp_surf_avg_flag        ! time flag id for computing average
                                ! surface tracer values

   real (r8), dimension(ecosys_tracer_cnt) :: &
      surf_avg                  ! average surface tracer values

   logical (log_kind) :: &
      ecosys_qsw_distrb_const

!-----------------------------------------------------------------------
!  iron patch fertilization
!-----------------------------------------------------------------------

   logical (log_kind) :: &
      liron_patch               ! flag for iron patch fertilization

   character(char_len) :: &
      iron_patch_flux_filename  ! file containing name of iron patch file

   integer (int_kind) :: &
      iron_patch_month          !  integer month to add patch flux

!-----------------------------------------------------------------------
!  timers
!-----------------------------------------------------------------------

   integer (int_kind) :: &
      ecosys_shr_strdata_advance_timer,        &
      ecosys_comp_CO3terms_timer,              &
      ecosys_interior_timer,                   &
      ecosys_sflux_timer

!-----------------------------------------------------------------------
!  named field indices
!-----------------------------------------------------------------------

   integer (int_kind) :: &
      totChl_surf_nf_ind = 0,    & ! total chlorophyll in surface layer
      sflux_co2_nf_ind   = 0,    & ! air-sea co2 gas flux
      atm_co2_nf_ind     = 0       ! atmospheric co2
#endif

!-----------------------------------------------------------------------

#ifdef BEC
   real (r8), dimension(:,:,:), allocatable :: &
      PAR_out           ! photosynthetically available radiation (W/m^2)
#else
   real (r8), dimension(nx_block,ny_block,max_blocks_clinic) :: &
      PAR_out           ! photosynthetically available radiation (W/m^2)
#endif

!-----------------------------------------------------------------------

#ifdef FE_TAG
   real (r8), dimension(:,:,:), allocatable :: &
      wgt
#endif

#ifdef DIAGNOSTICS_BIO
# ifndef FE_TAG
   real (r8), dimension(:,:), allocatable :: &
      aFe_brate, aFe_scav, aFe_disag, aFe_pgen, aFe_premin, aFe_hbio
# else
   real (r8), dimension(:,:), allocatable :: &
      aFe0_brate, aFe0_scav, aFe0_disag, aFe0_pgen, aFe0_premin, aFe0_hbio
   real (r8), dimension(:,:), allocatable :: &
      aFe1_brate, aFe1_scav, aFe1_disag, aFe1_pgen, aFe1_premin, aFe1_hbio
   real (r8), dimension(:,:), allocatable :: &
      aFe2_brate, aFe2_scav, aFe2_disag, aFe2_pgen, aFe2_premin, aFe2_hbio
   real (r8), dimension(:,:), allocatable :: &
      aFe3_brate, aFe3_scav, aFe3_disag, aFe3_pgen, aFe3_premin, aFe3_hbio
   real (r8), dimension(:,:), allocatable :: &
      aFe4_brate, aFe4_scav, aFe4_disag, aFe4_pgen, aFe4_premin, aFe4_hbio
   real (r8), dimension(:,:), allocatable :: &
      aFe5_brate, aFe5_scav, aFe5_disag, aFe5_pgen, aFe5_premin, aFe5_hbio
   real (r8), dimension(:,:), allocatable :: &
      aFe6_brate, aFe6_scav, aFe6_disag, aFe6_pgen, aFe6_premin, aFe6_hbio
   real (r8), dimension(:,:), allocatable :: &
      aFe7_brate, aFe7_scav, aFe7_disag, aFe7_pgen, aFe7_premin, aFe7_hbio
# endif
#endif

   real (r8), parameter :: &
      phlo_surf_init = 7.0_r8, & ! low bound for surface ph for no prev soln
      phhi_surf_init = 9.0_r8, & ! high bound for surface ph for no prev soln
      phlo_3d_init = 6.0_r8,   & ! low bound for subsurface ph for no prev soln
      phhi_3d_init = 9.0_r8,   & ! high bound for subsurface ph for no prev soln
      del_ph = 0.20_r8           ! delta-ph for prev soln


!-----------------------------------------------------------------------
#ifndef BEC
   type(ecosys_restore_type) :: ecosys_restore
#endif

!EOC
!*****************************************************************************

contains

!*****************************************************************************
!BOP
! !IROUTINE: ecosys_init
! !INTERFACE:

#ifdef BEC
 subroutine ecosys_init(tracer_d_module)
#else
 subroutine ecosys_init(init_ts_file_fmt, read_restart_filename, &
                        tracer_d_module, TRACER_MODULE, &
                        lmarginal_seas, errorCode)
#endif

! !DESCRIPTION:
!  Initialize ecosys tracer module. This involves setting metadata, reading
!  the module namelist, setting initial conditions, setting up forcing,
!  and defining additional tavg variables.
!
! !REVISION HISTORY:
!  same as module

!c! !INPUT PARAMETERS:
!c
!c   character (*), intent(in) :: &
!c      init_ts_file_fmt,     & ! format (bin or nc) for input file
!c      read_restart_filename   ! file name for restart file
!c
!c   logical (kind=log_kind), intent(in) :: &
!c     lmarginal_seas               ! Is ecosystem active in marginal seas ?
!c

! !INPUT/OUTPUT PARAMETERS:

   type (tracer_field), dimension(:), intent(inout) :: &
      tracer_d_module   ! descriptors for each tracer
!c
!c   real (r8), dimension(:,:,:,:,:,:), &
!c      intent(inout) :: TRACER_MODULE
!c
!c! !OUTPUT PARAMETERS:
!c
!c   integer (POP_i4), intent(out) :: &
!c      errorCode
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------

   character(*), parameter :: subname = 'ecosys_mod:ecosys_init'

   character(char_len) :: &
      init_ecosys_option,        & ! option for initialization of bgc
      init_ecosys_init_file,     & ! filename for option 'file'
      init_ecosys_init_file_fmt, & ! file format for option 'file'
      comp_surf_avg_freq_opt,    & ! choice for freq of comp_surf_avg
      gas_flux_forcing_opt,      & ! option for forcing gas fluxes
      atm_co2_opt,               & ! option for atmospheric co2 concentration
      atm_alt_co2_opt              ! option for atmospheric alternative CO2

   type(tracer_read), dimension(ecosys_tracer_cnt) :: &
      tracer_init_ext              ! namelist variable for initializing tracers

   type(tracer_read) :: &
      dust_flux_input,           & ! namelist input for dust_flux
      iron_flux_input,           & ! namelist input for iron_flux
      nox_flux_monthly_input,    & ! namelist input for nox_flux_monthly
      nhy_flux_monthly_input,    & ! namelist input for nhy_flux_monthly
      din_riv_flux_input,        & ! namelist input for din_riv_flux
      dip_riv_flux_input,        & ! namelist input for dip_riv_flux
      don_riv_flux_input,        & ! namelist input for don_riv_flux
      dop_riv_flux_input,        & ! namelist input for dop_riv_flux
      dsi_riv_flux_input,        & ! namelist input for dsi_riv_flux
      dfe_riv_flux_input,        & ! namelist input for dfe_riv_flux
      dic_riv_flux_input,        & ! namelist input for dic_riv_flux
      alk_riv_flux_input,        & ! namelist input for alk_riv_flux
      doc_riv_flux_input           ! namelist input for doc_riv_flux

   logical (log_kind) :: &
      default,                   & ! arg to init_time_flag
      lnml_found                   ! Was ecosys_nml found ?

   integer (int_kind) :: &
      non_living_biomass_ecosys_tracer_cnt, & ! number of non-autotroph ecosystem tracers
      auto_ind,                  & ! autotroph functional group index
      n,                         & ! index for looping over tracers
      k,                         & ! index for looping over depth levels
      l,                         & ! index for looping over time levels
      ind,                       & ! tracer index for tracer name from namelist
      iblock,                    & ! index for looping over blocks
      nml_error                    ! namelist i/o error flag

   integer (int_kind) :: &
        zoo_ind                    ! zooplankton functional group index


   integer (int_kind) :: &
      freq_opt, freq,            & ! args for init_time_flag
      comp_surf_avg_freq_iopt,   & ! choice for freq of comp_surf_avg
      comp_surf_avg_freq           ! choice for freq of comp_surf_avg

   logical (log_kind) :: &
      use_nml_surf_vals            ! do namelist surf values override values from restart file

   logical (log_kind) :: &
      lecovars_full_depth_tavg     ! should ecosystem vars be written full depth

!-----------------------------------------------------------------------
!  values to be used when comp_surf_avg_freq_opt==never
!-----------------------------------------------------------------------

   real (r8) :: &
      surf_avg_dic_const, surf_avg_alk_const

   integer, parameter :: bid = 1

!c   namelist /ecosys_nml/ &
!c      init_ecosys_option, init_ecosys_init_file, tracer_init_ext, &
!c      init_ecosys_init_file_fmt, &
!c      dust_flux_input, iron_flux_input, fesedflux_input, &
!c      ndep_data_type, nox_flux_monthly_input, nhy_flux_monthly_input, &
!c      ndep_shr_stream_year_first, ndep_shr_stream_year_last, &
!c      ndep_shr_stream_year_align, ndep_shr_stream_file, &
!c      ndep_shr_stream_scale_factor, &
!c      din_riv_flux_input, dip_riv_flux_input, don_riv_flux_input, &
!c      dop_riv_flux_input, dsi_riv_flux_input, dfe_riv_flux_input, &
!c      dic_riv_flux_input, alk_riv_flux_input, doc_riv_flux_input, &
!c      gas_flux_forcing_opt, gas_flux_forcing_file, &
!c      gas_flux_fice, gas_flux_ws, gas_flux_ap, &
!c      nutr_rest_file, &
!c      comp_surf_avg_freq_opt, comp_surf_avg_freq,  &
!c      use_nml_surf_vals, surf_avg_dic_const, surf_avg_alk_const, &
!c      ecosys_qsw_distrb_const, &
!c      lsource_sink, lflux_gas_o2, lflux_gas_co2, locmip_k1_k2_bug_fix, &
!c      lnutr_variable_restore, nutr_variable_rest_file,  &
!c      nutr_variable_rest_file_fmt,atm_co2_opt,atm_co2_const, &
!c      atm_alt_co2_opt, atm_alt_co2_const, &
!c      liron_patch,iron_patch_flux_filename,iron_patch_month, &
!c      lecovars_full_depth_tavg
!c
!c   character (char_len) :: &
!c      ecosys_restart_filename  ! modified file name for restart file

!c#ifdef BEC
!c   real (r8), dimension (:,:), allocatable :: WORK
!c#else
   real (r8), dimension (nx_block,ny_block) :: WORK
!c#endif

!c#ifdef BEC
!c   nx_block = nx_block_in
!c   ny_block = ny_block_in
!c   km       = km_in
!c   allocate(WORK(nx_block, ny_block))
!c#endif

!c!-----------------------------------------------------------------------
!c!  initialize name table
!c!-----------------------------------------------------------------------
!c
!c   errorCode = POP_Success
!c
!c!-----------------------------------------------------------------------
!c!  initialize forcing_monthly_every_ts variables
!c!-----------------------------------------------------------------------
!c
!c   call init_forcing_monthly_every_ts(dust_flux)
!c   call init_forcing_monthly_every_ts(iron_flux)
!c   call init_forcing_monthly_every_ts(fice_file)
!c   call init_forcing_monthly_every_ts(xkw_file)
!c   call init_forcing_monthly_every_ts(ap_file)
!c   call init_forcing_monthly_every_ts(nox_flux_monthly)
!c   call init_forcing_monthly_every_ts(nhy_flux_monthly)
!c   call init_forcing_monthly_every_ts(din_riv_flux)
!c   call init_forcing_monthly_every_ts(dip_riv_flux)
!c   call init_forcing_monthly_every_ts(don_riv_flux)
!c   call init_forcing_monthly_every_ts(dop_riv_flux)
!c   call init_forcing_monthly_every_ts(dsi_riv_flux)
!c   call init_forcing_monthly_every_ts(dfe_riv_flux)
!c   call init_forcing_monthly_every_ts(dic_riv_flux)
!c   call init_forcing_monthly_every_ts(alk_riv_flux)
!c   call init_forcing_monthly_every_ts(doc_riv_flux)
!c
!c!-----------------------------------------------------------------------
!c!  initialize ecosystem parameters
!c!-----------------------------------------------------------------------

   call ecosys_parms_init

!-----------------------------------------------------------------------
!  initialize non-autotroph tracer_d values
!  accumulate non_living_biomass_ecosys_tracer_cnt
!-----------------------------------------------------------------------

   non_living_biomass_ecosys_tracer_cnt = 0

   tracer_d_module(po4_ind)%short_name='PO4'
   tracer_d_module(po4_ind)%long_name='Dissolved Inorganic Phosphate'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(no3_ind)%short_name='NO3'
   tracer_d_module(no3_ind)%long_name='Dissolved Inorganic Nitrate'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(sio3_ind)%short_name='SiO3'
   tracer_d_module(sio3_ind)%long_name='Dissolved Inorganic Silicate'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(nh4_ind)%short_name='NH4'
   tracer_d_module(nh4_ind)%long_name='Dissolved Ammonia'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(o2_ind)%short_name='O2'
   tracer_d_module(o2_ind)%long_name='Dissolved Oxygen'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dic_ind)%short_name='DIC'
   tracer_d_module(dic_ind)%long_name='Dissolved Inorganic Carbon'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dic_alt_co2_ind)%short_name='DIC_ALT_CO2'
   tracer_d_module(dic_alt_co2_ind)%long_name='Dissolved Inorganic Carbon, Alternative CO2'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(alk_ind)%short_name='ALK'
   tracer_d_module(alk_ind)%long_name='Alkalinity'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(doc_ind)%short_name='DOC'
   tracer_d_module(doc_ind)%long_name='Dissolved Organic Carbon'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(don_ind)%short_name='DON'
   tracer_d_module(don_ind)%long_name='Dissolved Organic Nitrogen'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dop_ind)%short_name='DOP'
   tracer_d_module(dop_ind)%long_name='Dissolved Organic Phosphorus'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dopr_ind)%short_name='DOPr'
   tracer_d_module(dopr_ind)%long_name='Refractory DOP'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(donr_ind)%short_name='DONr'
   tracer_d_module(donr_ind)%long_name='Refractory DON'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

#ifndef FE_TAG
   tracer_d_module(fe_ind)%short_name='Fe'
   tracer_d_module(fe_ind)%long_name='Dissolved Inorganic Iron'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dofe_ind)%short_name='DOFe'
   tracer_d_module(dofe_ind)%long_name='Dissolved Organic Iron'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1
#else
   tracer_d_module(fe0_ind)%short_name='Fe0'
   tracer_d_module(fe0_ind)%long_name='Dissolved Inorganic Iron 0'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(fe1_ind)%short_name='Fe1'
   tracer_d_module(fe1_ind)%long_name='Dissolved Inorganic Iron 1'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(fe2_ind)%short_name='Fe2'
   tracer_d_module(fe2_ind)%long_name='Dissolved Inorganic Iron 2'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(fe3_ind)%short_name='Fe3'
   tracer_d_module(fe3_ind)%long_name='Dissolved Inorganic Iron 3'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(fe4_ind)%short_name='Fe4'
   tracer_d_module(fe4_ind)%long_name='Dissolved Inorganic Iron 4'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(fe5_ind)%short_name='Fe5'
   tracer_d_module(fe5_ind)%long_name='Dissolved Inorganic Iron 5'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(fe6_ind)%short_name='Fe6'
   tracer_d_module(fe6_ind)%long_name='Dissolved Inorganic Iron 6'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(fe7_ind)%short_name='Fe7'
   tracer_d_module(fe7_ind)%long_name='Dissolved Inorganic Iron 7'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dofe0_ind)%short_name='DOFe0'
   tracer_d_module(dofe0_ind)%long_name='Dissolved Organic Iron 0'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dofe1_ind)%short_name='DOFe1'
   tracer_d_module(dofe1_ind)%long_name='Dissolved Organic Iron 1'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dofe2_ind)%short_name='DOFe2'
   tracer_d_module(dofe2_ind)%long_name='Dissolved Organic Iron 2'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dofe3_ind)%short_name='DOFe3'
   tracer_d_module(dofe3_ind)%long_name='Dissolved Organic Iron 3'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dofe4_ind)%short_name='DOFe4'
   tracer_d_module(dofe4_ind)%long_name='Dissolved Organic Iron 4'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dofe5_ind)%short_name='DOFe5'
   tracer_d_module(dofe5_ind)%long_name='Dissolved Organic Iron 5'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dofe6_ind)%short_name='DOFe6'
   tracer_d_module(dofe6_ind)%long_name='Dissolved Organic Iron 6'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1

   tracer_d_module(dofe7_ind)%short_name='DOFe7'
   tracer_d_module(dofe7_ind)%long_name='Dissolved Organic Iron 7'
   non_living_biomass_ecosys_tracer_cnt = non_living_biomass_ecosys_tracer_cnt + 1
#endif

   do n = 1, non_living_biomass_ecosys_tracer_cnt
      if (n == alk_ind) then
         tracer_d_module(n)%units      = 'meq/m^3'
         tracer_d_module(n)%tend_units = 'meq/m^3/s'
         tracer_d_module(n)%flux_units = 'meq/m^3 cm/s'
      else
         tracer_d_module(n)%units      = 'mmol/m^3'
         tracer_d_module(n)%tend_units = 'mmol/m^3/s'
         tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      endif
   end do

!-----------------------------------------------------------------------
!  confirm that ecosys_tracer_cnt is consistent with autotroph declarations
!-----------------------------------------------------------------------
   n = non_living_biomass_ecosys_tracer_cnt
   do zoo_ind = 1, zooplankton_cnt
      n = n + 1 ! C
   end do

   do auto_ind = 1, autotroph_cnt
      n = n + 3 ! Chl,C,Fe tracers
      if (autotrophs(auto_ind)%kSiO3 > c0) n = n + 1 ! Si tracer
      if (autotrophs(auto_ind)%imp_calcifier .or. &
          autotrophs(auto_ind)%exp_calcifier) n = n + 1 ! CaCO3 tracer
   end do

!c   if (ecosys_tracer_cnt /= n) then
!c      call document(subname, 'actual ecosys_tracer_cnt', ecosys_tracer_cnt)
!c      call document(subname, 'computed ecosys_tracer_cnt', n)
!c      call exit_POP(sigAbort, 'inconsistency between actual ecosys_tracer_cnt and computed ecosys_tracer_cnt')
!c   endif

!-----------------------------------------------------------------------
!  initialize zooplankton tracer_d values and tracer indices
!-----------------------------------------------------------------------
   n = non_living_biomass_ecosys_tracer_cnt + 1

   do zoo_ind = 1, zooplankton_cnt
      tracer_d_module(n)%short_name = trim(zooplankton(zoo_ind)%sname) // 'C'
      tracer_d_module(n)%long_name  = trim(zooplankton(zoo_ind)%lname) // ' Carbon'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      zooplankton(zoo_ind)%C_ind = zooC_ind
      n = n + 1

#ifdef FE_TAG
      tracer_d_module(n)%short_name = trim(zooplankton(zoo_ind)%sname) // 'Fe0'
      tracer_d_module(n)%long_name  = trim(zooplankton(zoo_ind)%lname) // ' Fe0'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      zooplankton(zoo_ind)%Fe0_ind = zooFe0_ind
      n = n + 1

      tracer_d_module(n)%short_name = trim(zooplankton(zoo_ind)%sname) // 'Fe1'
      tracer_d_module(n)%long_name  = trim(zooplankton(zoo_ind)%lname) // ' Fe1'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      zooplankton(zoo_ind)%Fe1_ind = zooFe1_ind
      n = n + 1

      tracer_d_module(n)%short_name = trim(zooplankton(zoo_ind)%sname) // 'Fe2'
      tracer_d_module(n)%long_name  = trim(zooplankton(zoo_ind)%lname) // ' Fe2'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      zooplankton(zoo_ind)%Fe2_ind = zooFe2_ind
      n = n + 1

      tracer_d_module(n)%short_name = trim(zooplankton(zoo_ind)%sname) // 'Fe3'
      tracer_d_module(n)%long_name  = trim(zooplankton(zoo_ind)%lname) // ' Fe3'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      zooplankton(zoo_ind)%Fe3_ind = zooFe3_ind
      n = n + 1

      tracer_d_module(n)%short_name = trim(zooplankton(zoo_ind)%sname) // 'Fe4'
      tracer_d_module(n)%long_name  = trim(zooplankton(zoo_ind)%lname) // ' Fe4'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      zooplankton(zoo_ind)%Fe4_ind = zooFe4_ind
      n = n + 1

      tracer_d_module(n)%short_name = trim(zooplankton(zoo_ind)%sname) // 'Fe5'
      tracer_d_module(n)%long_name  = trim(zooplankton(zoo_ind)%lname) // ' Fe5'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      zooplankton(zoo_ind)%Fe5_ind = zooFe5_ind
      n = n + 1

      tracer_d_module(n)%short_name = trim(zooplankton(zoo_ind)%sname) // 'Fe6'
      tracer_d_module(n)%long_name  = trim(zooplankton(zoo_ind)%lname) // ' Fe6'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      zooplankton(zoo_ind)%Fe6_ind = zooFe6_ind
      n = n + 1

      tracer_d_module(n)%short_name = trim(zooplankton(zoo_ind)%sname) // 'Fe7'
      tracer_d_module(n)%long_name  = trim(zooplankton(zoo_ind)%lname) // ' Fe7'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      zooplankton(zoo_ind)%Fe7_ind = zooFe7_ind
      n = n + 1
#endif
   end do

!c   if (my_task == master_task) THEN
!c      write (stdout,*) '----- zooplankton tracer indices -----'
!c      do zoo_ind = 1, zooplankton_cnt
!c         write (stdout,*) 'C_ind(', trim(zooplankton(zoo_ind)%sname), ') = ', zooplankton(zoo_ind)%C_ind
!c      end do
!c      write (stdout,*) '------------------------------------'
!c   endif

!-----------------------------------------------------------------------
!  initialize autotroph tracer_d values and tracer indices
!-----------------------------------------------------------------------

   do auto_ind = 1, autotroph_cnt
      tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Chl'
      tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Chlorophyll'
      tracer_d_module(n)%units      = 'mg/m^3'
      tracer_d_module(n)%tend_units = 'mg/m^3/s'
      tracer_d_module(n)%flux_units = 'mg/m^3 cm/s'
      select case(auto_ind)
      case(1)
         autotrophs(auto_ind)%Chl_ind = spChl_ind
         n = n + 1
      case(2)
         autotrophs(auto_ind)%Chl_ind = diatChl_ind
         n = n + 1
      case(3)
         autotrophs(auto_ind)%Chl_ind = diazChl_ind
         n = n + 1
      end select

      tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'C'
      tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Carbon'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      select case(auto_ind)
      case(1)
         autotrophs(auto_ind)%C_ind = spC_ind
         n = n + 1
      case(2)
         autotrophs(auto_ind)%C_ind = diatC_ind
         n = n + 1
      case(3)
         autotrophs(auto_ind)%C_ind = diazC_ind
         n = n + 1
      end select

#ifndef FE_TAG
      tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Fe'
      tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Iron'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      select case(auto_ind)
      case(1)
         autotrophs(auto_ind)%Fe_ind = spFe_ind
         n = n + 1
      case(2)
         autotrophs(auto_ind)%Fe_ind = diatFe_ind
         n = n + 1
      case(3)
         autotrophs(auto_ind)%Fe_ind = diazFe_ind
         n = n + 1
      end select
#else
      tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Fe0'
      tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Iron0'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      select case(auto_ind)
      case(1)
         autotrophs(auto_ind)%Fe0_ind = spFe0_ind
         n = n + 1
      case(2)
         autotrophs(auto_ind)%Fe0_ind = diatFe0_ind
         n = n + 1
      case(3)
         autotrophs(auto_ind)%Fe0_ind = diazFe0_ind
         n = n + 1
      end select

      tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Fe1'
      tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Iron1'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      select case(auto_ind)
      case(1)
         autotrophs(auto_ind)%Fe1_ind = spFe1_ind
         n = n + 1
      case(2)
         autotrophs(auto_ind)%Fe1_ind = diatFe1_ind
         n = n + 1
      case(3)
         autotrophs(auto_ind)%Fe1_ind = diazFe1_ind
         n = n + 1
      end select

      tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Fe2'
      tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Iron2'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      select case(auto_ind)
      case(1)
         autotrophs(auto_ind)%Fe2_ind = spFe2_ind
         n = n + 1
      case(2)
         autotrophs(auto_ind)%Fe2_ind = diatFe2_ind
         n = n + 1
      case(3)
         autotrophs(auto_ind)%Fe2_ind = diazFe2_ind
         n = n + 1
      end select

      tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Fe3'
      tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Iron3'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      select case(auto_ind)
      case(1)
         autotrophs(auto_ind)%Fe3_ind = spFe3_ind
         n = n + 1
      case(2)
         autotrophs(auto_ind)%Fe3_ind = diatFe3_ind
         n = n + 1
      case(3)
         autotrophs(auto_ind)%Fe3_ind = diazFe3_ind
         n = n + 1
      end select

      tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Fe4'
      tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Iron4'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      select case(auto_ind)
      case(1)
         autotrophs(auto_ind)%Fe4_ind = spFe4_ind
         n = n + 1
      case(2)
         autotrophs(auto_ind)%Fe4_ind = diatFe4_ind
         n = n + 1
      case(3)
         autotrophs(auto_ind)%Fe4_ind = diazFe4_ind
         n = n + 1
      end select

      tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Fe5'
      tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Iron5'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      select case(auto_ind)
      case(1)
         autotrophs(auto_ind)%Fe5_ind = spFe5_ind
         n = n + 1
      case(2)
         autotrophs(auto_ind)%Fe5_ind = diatFe5_ind
         n = n + 1
      case(3)
         autotrophs(auto_ind)%Fe5_ind = diazFe5_ind
         n = n + 1
      end select

      tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Fe6'
      tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Iron6'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      select case(auto_ind)
      case(1)
         autotrophs(auto_ind)%Fe6_ind = spFe6_ind
         n = n + 1
      case(2)
         autotrophs(auto_ind)%Fe6_ind = diatFe6_ind
         n = n + 1
      case(3)
         autotrophs(auto_ind)%Fe6_ind = diazFe6_ind
         n = n + 1
      end select

      tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Fe7'
      tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Iron7'
      tracer_d_module(n)%units      = 'mmol/m^3'
      tracer_d_module(n)%tend_units = 'mmol/m^3/s'
      tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
      select case(auto_ind)
      case(1)
         autotrophs(auto_ind)%Fe7_ind = spFe7_ind
         n = n + 1
      case(2)
         autotrophs(auto_ind)%Fe7_ind = diatFe7_ind
         n = n + 1
      case(3)
         autotrophs(auto_ind)%Fe7_ind = diazFe7_ind
         n = n + 1
      end select
#endif

      if (autotrophs(auto_ind)%kSiO3 > c0) then
         tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'Si'
         tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' Silicon'
         tracer_d_module(n)%units      = 'mmol/m^3'
         tracer_d_module(n)%tend_units = 'mmol/m^3/s'
         tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
         select case(auto_ind)
         case(2)
            autotrophs(auto_ind)%Si_ind = diatSi_ind
            n = n + 1
         end select
      else
         autotrophs(auto_ind)%Si_ind = 0
      endif

      if (autotrophs(auto_ind)%imp_calcifier .or. &
          autotrophs(auto_ind)%exp_calcifier) then
         tracer_d_module(n)%short_name = trim(autotrophs(auto_ind)%sname) // 'CaCO3'
         tracer_d_module(n)%long_name  = trim(autotrophs(auto_ind)%lname) // ' CaCO3'
         tracer_d_module(n)%units      = 'mmol/m^3'
         tracer_d_module(n)%tend_units = 'mmol/m^3/s'
         tracer_d_module(n)%flux_units = 'mmol/m^3 cm/s'
         select case(auto_ind)
         case(1)
            autotrophs(auto_ind)%CaCO3_ind = spCaCO3_ind
            n = n + 1
         end select
      else
         autotrophs(auto_ind)%CaCO3_ind = 0
      endif
   end do

!c   if (my_task == master_task) THEN
!c      write (stdout,*) '----- autotroph tracer indices -----'
!c      do auto_ind = 1, autotroph_cnt
!c         write (stdout,*) 'Chl_ind(', trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%Chl_ind
!c         write (stdout,*) 'C_ind(', trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%C_ind
!c         write (stdout,*) 'Fe_ind(', trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%Fe_ind
!c         write (stdout,*) 'Si_ind(', trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%Si_ind
!c         write (stdout,*) 'CaCO3_ind(', trim(autotrophs(auto_ind)%sname), ') = ', autotrophs(auto_ind)%CaCO3_ind
!c      end do
!c      write (stdout,*) '------------------------------------'
!c   endif

!-----------------------------------------------------------------------
!  initialize ind_name_table
!-----------------------------------------------------------------------

   do n = 1, ecosys_tracer_cnt
      ind_name_table(n) = ind_name_pair(n, tracer_d_module(n)%short_name)
   end do

!-----------------------------------------------------------------------
!  default namelist settings
!-----------------------------------------------------------------------

   init_ecosys_option = 'unknown'
   init_ecosys_init_file = 'unknown'
   init_ecosys_init_file_fmt = 'bin'

   gas_flux_forcing_opt  = 'drv'
   gas_flux_forcing_file = 'unknown'

   gas_flux_fice%filename     = 'unknown'
   gas_flux_fice%file_varname = 'FICE'
   gas_flux_fice%scale_factor = c1
   gas_flux_fice%default_val  = c0
   gas_flux_fice%file_fmt     = 'bin'

   gas_flux_ws%filename     = 'unknown'
   gas_flux_ws%file_varname = 'XKW'
   gas_flux_ws%scale_factor = c1
   gas_flux_ws%default_val  = c0
   gas_flux_ws%file_fmt     = 'bin'

   gas_flux_ap%filename     = 'unknown'
   gas_flux_ap%file_varname = 'P'
   gas_flux_ap%scale_factor = c1
   gas_flux_ap%default_val  = c0
   gas_flux_ap%file_fmt     = 'bin'

   nutr_rest_file = 'unknown'

!maltrud variable restoring
   lnutr_variable_restore      = .false.
   nutr_variable_rest_file     = 'unknown'
   nutr_variable_rest_file_fmt = 'bin'

   dust_flux_input%filename     = 'unknown'
   dust_flux_input%file_varname = 'dust_flux'
   dust_flux_input%scale_factor = c1
   dust_flux_input%default_val  = c0
   dust_flux_input%file_fmt     = 'bin'

   iron_flux_input%filename     = 'unknown'
   iron_flux_input%file_varname = 'iron_flux'
   iron_flux_input%scale_factor = c1
   iron_flux_input%default_val  = c0
   iron_flux_input%file_fmt     = 'bin'

   fesedflux_input%filename     = 'unknown'
   fesedflux_input%file_varname = 'FESEDFLUXIN'
   fesedflux_input%scale_factor = c1
   fesedflux_input%default_val  = c0
   fesedflux_input%file_fmt     = 'bin'

   ndep_data_type              = 'monthly-calendar'

   nox_flux_monthly_input%filename     = 'unknown'
   nox_flux_monthly_input%file_varname = 'nox_flux'
   nox_flux_monthly_input%scale_factor = c1
   nox_flux_monthly_input%default_val  = c0
   nox_flux_monthly_input%file_fmt     = 'bin'

   nhy_flux_monthly_input%filename     = 'unknown'
   nhy_flux_monthly_input%file_varname = 'nhy_flux'
   nhy_flux_monthly_input%scale_factor = c1
   nhy_flux_monthly_input%default_val  = c0
   nhy_flux_monthly_input%file_fmt     = 'bin'

   ndep_shr_stream_year_first = 1
   ndep_shr_stream_year_last  = 1
   ndep_shr_stream_year_align = 1
   ndep_shr_stream_file       = 'unknown'
   ndep_shr_stream_scale_factor = c1

   din_riv_flux_input%filename     = 'unknown'
   din_riv_flux_input%file_varname = 'din_riv_flux'
   din_riv_flux_input%scale_factor = c1
   din_riv_flux_input%default_val  = c0
   din_riv_flux_input%file_fmt     = 'nc'

   dip_riv_flux_input%filename     = 'unknown'
   dip_riv_flux_input%file_varname = 'dip_riv_flux'
   dip_riv_flux_input%scale_factor = c1
   dip_riv_flux_input%default_val  = c0
   dip_riv_flux_input%file_fmt     = 'nc'

   don_riv_flux_input%filename     = 'unknown'
   don_riv_flux_input%file_varname = 'don_riv_flux'
   don_riv_flux_input%scale_factor = c1
   don_riv_flux_input%default_val  = c0
   don_riv_flux_input%file_fmt     = 'nc'

   dop_riv_flux_input%filename     = 'unknown'
   dop_riv_flux_input%file_varname = 'dop_riv_flux'
   dop_riv_flux_input%scale_factor = c1
   dop_riv_flux_input%default_val  = c0
   dop_riv_flux_input%file_fmt     = 'nc'

   dsi_riv_flux_input%filename     = 'unknown'
   dsi_riv_flux_input%file_varname = 'dsi_riv_flux'
   dsi_riv_flux_input%scale_factor = c1
   dsi_riv_flux_input%default_val  = c0
   dsi_riv_flux_input%file_fmt     = 'nc'

   dfe_riv_flux_input%filename     = 'unknown'
   dfe_riv_flux_input%file_varname = 'dfe_riv_flux'
   dfe_riv_flux_input%scale_factor = c1
   dfe_riv_flux_input%default_val  = c0
   dfe_riv_flux_input%file_fmt     = 'nc'

   dic_riv_flux_input%filename     = 'unknown'
   dic_riv_flux_input%file_varname = 'dic_riv_flux'
   dic_riv_flux_input%scale_factor = c1
   dic_riv_flux_input%default_val  = c0
   dic_riv_flux_input%file_fmt     = 'nc'

   alk_riv_flux_input%filename     = 'unknown'
   alk_riv_flux_input%file_varname = 'alk_riv_flux'
   alk_riv_flux_input%scale_factor = c1
   alk_riv_flux_input%default_val  = c0
   alk_riv_flux_input%file_fmt     = 'nc'

   doc_riv_flux_input%filename     = 'unknown'
   doc_riv_flux_input%file_varname = 'doc_riv_flux'
   doc_riv_flux_input%scale_factor = c1
   doc_riv_flux_input%default_val  = c0
   doc_riv_flux_input%file_fmt     = 'nc'

   do n = 1,ecosys_tracer_cnt
      tracer_init_ext(n)%mod_varname  = 'unknown'
      tracer_init_ext(n)%filename     = 'unknown'
      tracer_init_ext(n)%file_varname = 'unknown'
      tracer_init_ext(n)%scale_factor = c1
      tracer_init_ext(n)%default_val  = c0
      tracer_init_ext(n)%file_fmt     = 'bin'
   end do

   lsource_sink          = .true.
   lflux_gas_o2          = .true.
   lflux_gas_co2         = .true.
   locmip_k1_k2_bug_fix  = .true.

   comp_surf_avg_freq_opt        = 'never'
   comp_surf_avg_freq            = 1
   use_nml_surf_vals             = .false.
   surf_avg_dic_const            = 1944.0_r8
   surf_avg_alk_const            = 2225.0_r8

   ecosys_qsw_distrb_const  = .true.

   liron_patch              = .false.
   iron_patch_flux_filename = 'unknown_iron_patch_filename'
   iron_patch_month         = 1

   atm_co2_opt   = 'const'
   atm_co2_const = 280.0_r8

   atm_alt_co2_opt   = 'const'
   atm_alt_co2_const = 280.0_r8

   lecovars_full_depth_tavg = .false.

!c   if (my_task == master_task) then
!c      open (nml_in, file=nml_filename, status='old',iostat=nml_error)
!c      if (nml_error /= 0) then
!c         nml_error = -1
!c      else
!c         nml_error =  1
!c      endif
!c      do while (nml_error > 0)
!c         read(nml_in, nml=ecosys_nml,iostat=nml_error)
!c      end do
!c      if (nml_error == 0) close(nml_in)
!c   endif
!c
!c   call broadcast_scalar(nml_error, master_task)
!c   if (nml_error /= 0) then
!c      call document(subname, 'ecosys_nml not found')
!c      call exit_POP(sigAbort, 'ERROR : stopping in '/&
!c                           &/ subname)
!c   endif
!c
!c   if (my_task == master_task) then
!c      write(stdout,blank_fmt)
!c      write(stdout,ndelim_fmt)
!c      write(stdout,blank_fmt)
!c      write(stdout,*) ' ecosys:'
!c      write(stdout,blank_fmt)
!c      write(stdout,*) ' ecosys_nml namelist settings:'
!c      write(stdout,blank_fmt)
!c      write(stdout,ecosys_nml)
!c      write(stdout,blank_fmt)
!c      write(stdout,delim_fmt)
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!  broadcast all namelist variables
!c!-----------------------------------------------------------------------
!c
!c   call broadcast_scalar(init_ecosys_option, master_task)
!c   call broadcast_scalar(init_ecosys_init_file, master_task)
!c   call broadcast_scalar(init_ecosys_init_file_fmt, master_task)
!c
!c   call broadcast_scalar(gas_flux_forcing_opt, master_task)
   if (trim(gas_flux_forcing_opt) == 'drv') then
      gas_flux_forcing_iopt = gas_flux_forcing_iopt_drv
   else if (trim(gas_flux_forcing_opt) == 'file') then
      gas_flux_forcing_iopt = gas_flux_forcing_iopt_file
   else
!c      call document(subname, 'gas_flux_forcing_opt', gas_flux_forcing_opt)
!c      call exit_POP(sigAbort, 'unknown gas_flux_forcing_opt')
   endif
!c
!c   call broadcast_scalar(gas_flux_forcing_file, master_task)
!c
!c   call broadcast_scalar(gas_flux_fice%filename, master_task)
!c   call broadcast_scalar(gas_flux_fice%file_varname, master_task)
!c   call broadcast_scalar(gas_flux_fice%scale_factor, master_task)
!c   call broadcast_scalar(gas_flux_fice%default_val, master_task)
!c   call broadcast_scalar(gas_flux_fice%file_fmt, master_task)
!c
!c   fice_file%input = gas_flux_fice
!c
!c   call broadcast_scalar(gas_flux_ws%filename, master_task)
!c   call broadcast_scalar(gas_flux_ws%file_varname, master_task)
!c   call broadcast_scalar(gas_flux_ws%scale_factor, master_task)
!c   call broadcast_scalar(gas_flux_ws%default_val, master_task)
!c   call broadcast_scalar(gas_flux_ws%file_fmt, master_task)
!c
!c   xkw_file%input = gas_flux_ws
!c
!c   call broadcast_scalar(gas_flux_ap%filename, master_task)
!c   call broadcast_scalar(gas_flux_ap%file_varname, master_task)
!c   call broadcast_scalar(gas_flux_ap%scale_factor, master_task)
!c   call broadcast_scalar(gas_flux_ap%default_val, master_task)
!c   call broadcast_scalar(gas_flux_ap%file_fmt, master_task)
!c
!c   ap_file%input = gas_flux_ap
!c
!c   call broadcast_scalar(nutr_rest_file, master_task)
!c
!c!maltrud variable restoring
!c   call broadcast_scalar(lnutr_variable_restore, master_task)
!c   call broadcast_scalar(nutr_variable_rest_file, master_task)
!c   call broadcast_scalar(nutr_variable_rest_file_fmt, master_task)
!c
!c   call broadcast_scalar(dust_flux_input%filename, master_task)
!c   call broadcast_scalar(dust_flux_input%file_varname, master_task)
!c   call broadcast_scalar(dust_flux_input%scale_factor, master_task)
!c   call broadcast_scalar(dust_flux_input%default_val, master_task)
!c   call broadcast_scalar(dust_flux_input%file_fmt, master_task)
!c
!c   dust_flux%input = dust_flux_input
!c
!c   call broadcast_scalar(iron_flux_input%filename, master_task)
!c   call broadcast_scalar(iron_flux_input%file_varname, master_task)
!c   call broadcast_scalar(iron_flux_input%scale_factor, master_task)
!c   call broadcast_scalar(iron_flux_input%default_val, master_task)
!c   call broadcast_scalar(iron_flux_input%file_fmt, master_task)
!c
!c   iron_flux%input = iron_flux_input
!c
!c   call broadcast_scalar(fesedflux_input%filename, master_task)
!c   call broadcast_scalar(fesedflux_input%file_varname, master_task)
!c   call broadcast_scalar(fesedflux_input%scale_factor, master_task)
!c   call broadcast_scalar(fesedflux_input%default_val, master_task)
!c   call broadcast_scalar(fesedflux_input%file_fmt, master_task)
!c
!c   call broadcast_scalar(ndep_data_type, master_task)
!c
!c   call broadcast_scalar(nox_flux_monthly_input%filename, master_task)
!c   call broadcast_scalar(nox_flux_monthly_input%file_varname, master_task)
!c   call broadcast_scalar(nox_flux_monthly_input%scale_factor, master_task)
!c   call broadcast_scalar(nox_flux_monthly_input%default_val, master_task)
!c   call broadcast_scalar(nox_flux_monthly_input%file_fmt, master_task)
!c
!c   nox_flux_monthly%input = nox_flux_monthly_input
!c
!c   call broadcast_scalar(nhy_flux_monthly_input%filename, master_task)
!c   call broadcast_scalar(nhy_flux_monthly_input%file_varname, master_task)
!c   call broadcast_scalar(nhy_flux_monthly_input%scale_factor, master_task)
!c   call broadcast_scalar(nhy_flux_monthly_input%default_val, master_task)
!c   call broadcast_scalar(nhy_flux_monthly_input%file_fmt, master_task)
!c
!c   nhy_flux_monthly%input = nhy_flux_monthly_input
!c
!c   call broadcast_scalar(ndep_shr_stream_year_first, master_task)
!c   call broadcast_scalar(ndep_shr_stream_year_last, master_task)
!c   call broadcast_scalar(ndep_shr_stream_year_align, master_task)
!c   call broadcast_scalar(ndep_shr_stream_file, master_task)
!c   call broadcast_scalar(ndep_shr_stream_scale_factor, master_task)
!c
!c   call broadcast_scalar(din_riv_flux_input%filename, master_task)
!c   call broadcast_scalar(din_riv_flux_input%file_varname, master_task)
!c   call broadcast_scalar(din_riv_flux_input%scale_factor, master_task)
!c   call broadcast_scalar(din_riv_flux_input%default_val, master_task)
!c   call broadcast_scalar(din_riv_flux_input%file_fmt, master_task)
!c
!c   din_riv_flux%input = din_riv_flux_input
!c
!c   call broadcast_scalar(dip_riv_flux_input%filename, master_task)
!c   call broadcast_scalar(dip_riv_flux_input%file_varname, master_task)
!c   call broadcast_scalar(dip_riv_flux_input%scale_factor, master_task)
!c   call broadcast_scalar(dip_riv_flux_input%default_val, master_task)
!c   call broadcast_scalar(dip_riv_flux_input%file_fmt, master_task)
!c
!c   dip_riv_flux%input = dip_riv_flux_input
!c
!c   call broadcast_scalar(don_riv_flux_input%filename, master_task)
!c   call broadcast_scalar(don_riv_flux_input%file_varname, master_task)
!c   call broadcast_scalar(don_riv_flux_input%scale_factor, master_task)
!c   call broadcast_scalar(don_riv_flux_input%default_val, master_task)
!c   call broadcast_scalar(don_riv_flux_input%file_fmt, master_task)
!c
!c   don_riv_flux%input = don_riv_flux_input
!c
!c   call broadcast_scalar(dop_riv_flux_input%filename, master_task)
!c   call broadcast_scalar(dop_riv_flux_input%file_varname, master_task)
!c   call broadcast_scalar(dop_riv_flux_input%scale_factor, master_task)
!c   call broadcast_scalar(dop_riv_flux_input%default_val, master_task)
!c   call broadcast_scalar(dop_riv_flux_input%file_fmt, master_task)
!c
!c   dop_riv_flux%input = dop_riv_flux_input
!c
!c   call broadcast_scalar(dsi_riv_flux_input%filename, master_task)
!c   call broadcast_scalar(dsi_riv_flux_input%file_varname, master_task)
!c   call broadcast_scalar(dsi_riv_flux_input%scale_factor, master_task)
!c   call broadcast_scalar(dsi_riv_flux_input%default_val, master_task)
!c   call broadcast_scalar(dsi_riv_flux_input%file_fmt, master_task)
!c
!c   dsi_riv_flux%input = dsi_riv_flux_input
!c
!c   call broadcast_scalar(dfe_riv_flux_input%filename, master_task)
!c   call broadcast_scalar(dfe_riv_flux_input%file_varname, master_task)
!c   call broadcast_scalar(dfe_riv_flux_input%scale_factor, master_task)
!c   call broadcast_scalar(dfe_riv_flux_input%default_val, master_task)
!c   call broadcast_scalar(dfe_riv_flux_input%file_fmt, master_task)
!c
!c   dfe_riv_flux%input = dfe_riv_flux_input
!c
!c   call broadcast_scalar(dic_riv_flux_input%filename, master_task)
!c   call broadcast_scalar(dic_riv_flux_input%file_varname, master_task)
!c   call broadcast_scalar(dic_riv_flux_input%scale_factor, master_task)
!c   call broadcast_scalar(dic_riv_flux_input%default_val, master_task)
!c   call broadcast_scalar(dic_riv_flux_input%file_fmt, master_task)
!c
!c   dic_riv_flux%input = dic_riv_flux_input
!c
!c   call broadcast_scalar(alk_riv_flux_input%filename, master_task)
!c   call broadcast_scalar(alk_riv_flux_input%file_varname, master_task)
!c   call broadcast_scalar(alk_riv_flux_input%scale_factor, master_task)
!c   call broadcast_scalar(alk_riv_flux_input%default_val, master_task)
!c   call broadcast_scalar(alk_riv_flux_input%file_fmt, master_task)
!c
!c   alk_riv_flux%input = alk_riv_flux_input
!c
!c   call broadcast_scalar(doc_riv_flux_input%filename, master_task)
!c   call broadcast_scalar(doc_riv_flux_input%file_varname, master_task)
!c   call broadcast_scalar(doc_riv_flux_input%scale_factor, master_task)
!c   call broadcast_scalar(doc_riv_flux_input%default_val, master_task)
!c   call broadcast_scalar(doc_riv_flux_input%file_fmt, master_task)
!c
!c   doc_riv_flux%input = doc_riv_flux_input
!c
!c   do n = 1,ecosys_tracer_cnt
!c      call broadcast_scalar(tracer_init_ext(n)%mod_varname, master_task)
!c      call broadcast_scalar(tracer_init_ext(n)%filename, master_task)
!c      call broadcast_scalar(tracer_init_ext(n)%file_varname, master_task)
!c      call broadcast_scalar(tracer_init_ext(n)%scale_factor, master_task)
!c      call broadcast_scalar(tracer_init_ext(n)%default_val, master_task)
!c      call broadcast_scalar(tracer_init_ext(n)%file_fmt, master_task)
!c   end do
!c
!c   call broadcast_scalar(comp_surf_avg_freq_opt, master_task)
!c   call broadcast_scalar(comp_surf_avg_freq, master_task)
!c   call broadcast_scalar(use_nml_surf_vals, master_task)
!c   call broadcast_scalar(surf_avg_dic_const, master_task)
!c   call broadcast_scalar(surf_avg_alk_const, master_task)
!c
!c   call broadcast_scalar(ecosys_qsw_distrb_const, master_task)
!c
!c   call broadcast_scalar(lsource_sink, master_task)
!c   call broadcast_scalar(lflux_gas_o2, master_task)
!c   call broadcast_scalar(lflux_gas_co2, master_task)
!c   call broadcast_scalar(locmip_k1_k2_bug_fix, master_task)
!c
!c   call broadcast_scalar(liron_patch, master_task)
!c   call broadcast_scalar(iron_patch_flux_filename, master_task)
!c   call broadcast_scalar(iron_patch_month, master_task)
!c
!c   call broadcast_scalar(atm_co2_opt, master_task)
!c   call broadcast_scalar(atm_co2_const, master_task)
!c
!c   call broadcast_scalar(atm_alt_co2_opt, master_task)
!c   call broadcast_scalar(atm_alt_co2_const, master_task)
!c
!c   call broadcast_scalar(lecovars_full_depth_tavg, master_task)
!c
!c
!c
!c!-----------------------------------------------------------------------
!c!  initialize modules with dependancies on above variables
!c!-----------------------------------------------------------------------
!c
!c   call ecosys_restore%Init(nml_filename, nml_in, &
!c        ind_name_table)
!c
!c!-----------------------------------------------------------------------
!c!  set variables immediately dependent on namelist variables
!c!-----------------------------------------------------------------------
!c
!c   select case (comp_surf_avg_freq_opt)
!c   case ('never')
!c      comp_surf_avg_freq_iopt = freq_opt_never
!c   case ('nyear')
!c      comp_surf_avg_freq_iopt = freq_opt_nyear
!c   case ('nmonth')
!c      comp_surf_avg_freq_iopt = freq_opt_nmonth
!c   case default
!c      call document(subname, 'comp_surf_avg_freq_opt', comp_surf_avg_freq_opt)
!c      call exit_POP(sigAbort, 'unknown comp_surf_avg_freq_opt')
!c   end select
!c
!c  call init_time_flag('ecosys_comp_surf_avg', comp_surf_avg_flag, &
!c     default=.false., freq_opt=comp_surf_avg_freq_iopt,  &
!c     freq=comp_surf_avg_freq, owner='ecosys_init')
!c
   select case (atm_co2_opt)
   case ('const')
      atm_co2_iopt = atm_co2_iopt_const
   case ('drv_prog')
      atm_co2_iopt = atm_co2_iopt_drv_prog
   case ('drv_diag')
      atm_co2_iopt = atm_co2_iopt_drv_diag
   case default
#ifdef BEC
      call popmini_abort('unknown atm_co2_opt')
#endif
!c      call document(subname, 'atm_co2_opt', atm_co2_opt)
!c      call exit_POP(sigAbort, 'unknown atm_co2_opt')
   end select

   select case (atm_alt_co2_opt)
   case ('const')
      atm_alt_co2_iopt = atm_co2_iopt_const
   case default
#ifdef BEC
      call popmini_abort('unknown atm_alt_co2_opt')
#endif
!c      call document(subname, 'atm_alt_co2_opt', atm_alt_co2_opt)
!c      call exit_POP(sigAbort, 'unknown atm_alt_co2_opt')
   end select
!c
!c!-----------------------------------------------------------------------
!c!  namelist consistency checking
!c!-----------------------------------------------------------------------
!c
!c   if (use_nml_surf_vals .and. comp_surf_avg_freq_iopt /= freq_opt_never) then
!c      call document(subname, 'use_nml_surf_vals', use_nml_surf_vals)
!c      call document(subname, 'comp_surf_avg_freq_opt', comp_surf_avg_freq_opt)
!c      call exit_POP(sigAbort, 'use_nml_surf_vals can only be .true. if ' /&
!c                           &/ ' comp_surf_avg_freq_opt is never')
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!  initialize virtual flux flag array
!c!-----------------------------------------------------------------------
!c
!c   vflux_flag = .false.
!c   vflux_flag(dic_ind) = .true.
!c   vflux_flag(alk_ind) = .true.
!c   vflux_flag(dic_alt_co2_ind) = .true.
!c
!c!-----------------------------------------------------------------------
!c!  allocate various ecosys allocatable module variables
!c!-----------------------------------------------------------------------
!c

   allocate( PH_PREV(nx_block,ny_block,max_blocks_clinic) )
   allocate( PH_PREV_ALT_CO2(nx_block,ny_block,max_blocks_clinic) )
   allocate( dust_FLUX_IN(nx_block,ny_block,max_blocks_clinic) )
   allocate( PH_PREV_3D(nx_block,ny_block,km,max_blocks_clinic) )
   allocate( PH_PREV_ALT_CO2_3D(nx_block,ny_block,km,max_blocks_clinic) )

#ifdef DIAGNOSTICS_BIO
# ifndef FE_TAG
   allocate( aFe_brate (nx_block,ny_block) )
   allocate( aFe_scav  (nx_block,ny_block) )
   allocate( aFe_disag (nx_block,ny_block) )
   allocate( aFe_pgen  (nx_block,ny_block) )
   allocate( aFe_premin(nx_block,ny_block) )
   allocate( aFe_hbio  (nx_block,ny_block) )
# else
   allocate( aFe0_brate (nx_block,ny_block) )
   allocate( aFe0_scav  (nx_block,ny_block) )
   allocate( aFe0_disag (nx_block,ny_block) )
   allocate( aFe0_pgen  (nx_block,ny_block) )
   allocate( aFe0_premin(nx_block,ny_block) )
   allocate( aFe0_hbio  (nx_block,ny_block) )

   allocate( aFe1_brate (nx_block,ny_block) )
   allocate( aFe1_scav  (nx_block,ny_block) )
   allocate( aFe1_disag (nx_block,ny_block) )
   allocate( aFe1_pgen  (nx_block,ny_block) )
   allocate( aFe1_premin(nx_block,ny_block) )
   allocate( aFe1_hbio  (nx_block,ny_block) )

   allocate( aFe2_brate (nx_block,ny_block) )
   allocate( aFe2_scav  (nx_block,ny_block) )
   allocate( aFe2_disag (nx_block,ny_block) )
   allocate( aFe2_pgen  (nx_block,ny_block) )
   allocate( aFe2_premin(nx_block,ny_block) )
   allocate( aFe2_hbio  (nx_block,ny_block) )

   allocate( aFe3_brate (nx_block,ny_block) )
   allocate( aFe3_scav  (nx_block,ny_block) )
   allocate( aFe3_disag (nx_block,ny_block) )
   allocate( aFe3_pgen  (nx_block,ny_block) )
   allocate( aFe3_premin(nx_block,ny_block) )
   allocate( aFe3_hbio  (nx_block,ny_block) )

   allocate( aFe4_brate (nx_block,ny_block) )
   allocate( aFe4_scav  (nx_block,ny_block) )
   allocate( aFe4_disag (nx_block,ny_block) )
   allocate( aFe4_pgen  (nx_block,ny_block) )
   allocate( aFe4_premin(nx_block,ny_block) )
   allocate( aFe4_hbio  (nx_block,ny_block) )

   allocate( aFe5_brate (nx_block,ny_block) )
   allocate( aFe5_scav  (nx_block,ny_block) )
   allocate( aFe5_disag (nx_block,ny_block) )
   allocate( aFe5_pgen  (nx_block,ny_block) )
   allocate( aFe5_premin(nx_block,ny_block) )
   allocate( aFe5_hbio  (nx_block,ny_block) )

   allocate( aFe6_brate (nx_block,ny_block) )
   allocate( aFe6_scav  (nx_block,ny_block) )
   allocate( aFe6_disag (nx_block,ny_block) )
   allocate( aFe6_pgen  (nx_block,ny_block) )
   allocate( aFe6_premin(nx_block,ny_block) )
   allocate( aFe6_hbio  (nx_block,ny_block) )

   allocate( aFe7_brate (nx_block,ny_block) )
   allocate( aFe7_scav  (nx_block,ny_block) )
   allocate( aFe7_disag (nx_block,ny_block) )
   allocate( aFe7_pgen  (nx_block,ny_block) )
   allocate( aFe7_premin(nx_block,ny_block) )
   allocate( aFe7_hbio  (nx_block,ny_block) )
# endif
#endif

!-----------------------------------------------------------------------
!  allocate and initialize LAND_MASK
!-----------------------------------------------------------------------

   allocate( LAND_MASK(nx_block,ny_block,nblocks_clinic) )

   allocate( PAR_out(nx_block,ny_block,nblocks_clinic) )

#ifdef FE_TAG
   allocate( wgt(nx_block,ny_block,8) )
#endif

!c   if (lmarginal_seas) then
!c      LAND_MASK = REGION_MASK /= 0
!c   else
!c      LAND_MASK = REGION_MASK > 0
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!  initialize tracers
!c!-----------------------------------------------------------------------
!c
!c   select case (init_ecosys_option)
!c
!c   case ('restart', 'ccsm_continue', 'ccsm_branch', 'ccsm_hybrid')
!c
!c      ecosys_restart_filename = char_blank
!c
!c      if (init_ecosys_init_file == 'same_as_TS') then
!c         if (read_restart_filename == 'undefined') then
!c            call document(subname, 'no restart file to read ecosys from')
!c            call exit_POP(sigAbort, 'stopping in ' /&
!c                                 &/ subname)
!c         endif
!c         ecosys_restart_filename = read_restart_filename
!c         init_ecosys_init_file_fmt = init_ts_file_fmt
!c
!c      else  ! do not read from TS restart file
!c
!c         ecosys_restart_filename = trim(init_ecosys_init_file)
!c
!c      endif
!c
!c      call rest_read_tracer_block(init_ecosys_init_file_fmt, &
!c                                  ecosys_restart_filename,   &
!c                                  tracer_d_module,           &
!c                                  TRACER_MODULE)
!c
!c      if (field_exists_in_file(init_ecosys_init_file_fmt, &
!c                               ecosys_restart_filename, &
!c                               'PH_SURF')) then
!c         call read_field(init_ecosys_init_file_fmt, &
!c                         ecosys_restart_filename,   &
!c                         'PH_SURF', PH_PREV)
!c      else
!c         call document(subname, 'PH_SURF does not exist in ' /&
!c                       &/ trim(ecosys_restart_filename) /&
!c                       &/ ', setting PH_PREV to 0')
!c         PH_PREV = c0
!c      endif
!c
!c      if (field_exists_in_file(init_ecosys_init_file_fmt, &
!c                               ecosys_restart_filename, &
!c                               'PH_SURF_ALT_CO2')) then
!c         call read_field(init_ecosys_init_file_fmt, &
!c                         ecosys_restart_filename,   &
!c                         'PH_SURF_ALT_CO2', PH_PREV_ALT_CO2)
!c      else
!c         call document(subname, 'PH_SURF_ALT_CO2 does not exist in ' /&
!c                       &/ trim(ecosys_restart_filename) /&
!c                       &/ ', setting PH_PREV_ALT_CO2 to 0')
!c         PH_PREV_ALT_CO2 = c0
!c      endif
!c
!c      if (field_exists_in_file(init_ecosys_init_file_fmt, &
!c                               ecosys_restart_filename, &
!c                               'PH_3D')) then
!c         call read_field(init_ecosys_init_file_fmt, &
!c                         ecosys_restart_filename,   &
!c                         'PH_3D', PH_PREV_3D)
!c      else
!c         call document(subname, 'PH_3D does not exist in ' /&
!c                       &/ trim(ecosys_restart_filename) /&
!c                       &/ ', setting PH_PREV_3D to 0')
!c         PH_PREV_3D  = c0
!c      endif
!c
!c      if (field_exists_in_file(init_ecosys_init_file_fmt, &
!c                               ecosys_restart_filename, &
!c                               'PH_3D_ALT_CO2')) then
!c         call read_field(init_ecosys_init_file_fmt, &
!c                         ecosys_restart_filename,   &
!c                         'PH_3D_ALT_CO2', PH_PREV_ALT_CO2_3D)
!c      else
!c         call document(subname, 'PH_3D_ALT_CO2 does not exist in ' /&
!c                       &/ trim(ecosys_restart_filename) /&
!c                       &/ ', setting PH_PREV_ALT_CO2_3D to 0')
!c         PH_PREV_ALT_CO2_3D = c0
!c      endif
!c
!c      if (use_nml_surf_vals) then
!c         surf_avg = c0
!c         surf_avg(dic_ind) = surf_avg_dic_const
!c         surf_avg(dic_alt_co2_ind) = surf_avg_dic_const
!c         surf_avg(alk_ind) = surf_avg_alk_const
!c      else
!c         call extract_surf_avg(init_ecosys_init_file_fmt,     &
!c                               ecosys_restart_filename,       &
!c                               ecosys_tracer_cnt, vflux_flag, &
!c                               ind_name_table,surf_avg)
!c      endif
!c
!c      call eval_time_flag(comp_surf_avg_flag) ! evaluates time_flag(comp_surf_avg_flag)%value via time_to_do
!c
!c      if (check_time_flag(comp_surf_avg_flag)) &
!c         call comp_surf_avg(TRACER_MODULE(:,:,1,:,oldtime,:), &
!c                            TRACER_MODULE(:,:,1,:,curtime,:), &
!c                            ecosys_tracer_cnt,vflux_flag,surf_avg)
!c
!c   case ('file', 'ccsm_startup')
!c      call document(subname, 'ecosystem vars being read from separate files')
!c
!c      call file_read_tracer_block(init_ecosys_init_file_fmt, &
!c                                  init_ecosys_init_file,     &
!c                                  tracer_d_module,           &
!c                                  ind_name_table,            &
!c                                  tracer_init_ext,           &
!c                                  TRACER_MODULE)
!c
!c      if (n_topo_smooth > 0) then
!c         do n = 1, ecosys_tracer_cnt
!c            do k=1,km
!c               call fill_points(k,TRACER_MODULE(:,:,k,n,oldtime,:), &
!c                                errorCode)
!c
!c               if (errorCode /= POP_Success) then
!c                  call POP_ErrorSet(errorCode, &
!c                     'ecosys_init: error in fill points for tracers(oldtime)')
!c                  return
!c               endif
!c
!c               call fill_points(k,TRACER_MODULE(:,:,k,n,curtime,:), &
!c                                errorCode)
!c
!c               if (errorCode /= POP_Success) then
!c                  call POP_ErrorSet(errorCode, &
!c                     'ecosys_init: error in fill points for tracers(newtime)')
!c                  return
!c               endif
!c
!c            enddo
!c         enddo
!c      endif
!c
      PH_PREV = c0
      PH_PREV_ALT_CO2 = c0
      PH_PREV_3D = c0
      PH_PREV_ALT_CO2_3D = c0
!c
!c      if (use_nml_surf_vals) then
!c         surf_avg = c0
!c         surf_avg(dic_ind) = surf_avg_dic_const
!c         surf_avg(dic_alt_co2_ind) = surf_avg_dic_const
!c         surf_avg(alk_ind) = surf_avg_alk_const
!c      else
!c         call comp_surf_avg(TRACER_MODULE(:,:,1,:,oldtime,:), &
!c                            TRACER_MODULE(:,:,1,:,curtime,:), &
!c                            ecosys_tracer_cnt,vflux_flag,surf_avg)
!c      endif
!c
!c   case default
!c      call document(subname, 'init_ecosys_option', init_ecosys_option)
!c      call exit_POP(sigAbort, 'unknown init_ecosys_option')
!c
!c   end select
!c
!c!-----------------------------------------------------------------------
!c!  register Chl field for short-wave absorption
!c!  apply land mask to tracers
!c!  set Chl field for short-wave absorption
!c!-----------------------------------------------------------------------
!c
!c   call named_field_register('model_chlorophyll', totChl_surf_nf_ind)
!c
!c   !$OMP PARALLEL DO PRIVATE(iblock,n,k,WORK)
!c   do iblock=1,nblocks_clinic
!c      do n = 1,ecosys_tracer_cnt
!c         do k = 1,km
!c            where (.not. LAND_MASK(:,:,iblock) .or. k > KMT(:,:,iblock))
!c               TRACER_MODULE(:,:,k,n,curtime,iblock) = c0
!c               TRACER_MODULE(:,:,k,n,oldtime,iblock) = c0
!c            end where
!c         end do
!c      end do
!c
!c      WORK = c0
!c      do auto_ind = 1, autotroph_cnt
!c         n = autotrophs(auto_ind)%Chl_ind
!c         WORK = WORK + max(c0,p5*(TRACER_MODULE(:,:,1,n,oldtime,iblock) + &
!c                                  TRACER_MODULE(:,:,1,n,curtime,iblock)))
!c      end do
!c      call named_field_set(totChl_surf_nf_ind, iblock, WORK)
!c   enddo
!c   !$OMP END PARALLEL DO
!c
!c!-----------------------------------------------------------------------
!c!  timer init
!c!-----------------------------------------------------------------------
!c
!c   call get_timer(ecosys_comp_CO3terms_timer, 'comp_CO3terms', &
!c                  nblocks_clinic, distrb_clinic%nprocs)
!c   call get_timer(ecosys_interior_timer, 'ECOSYS_INTERIOR', &
!c                  nblocks_clinic, distrb_clinic%nprocs)
!c   call get_timer(ecosys_sflux_timer, 'ECOSYS_SFLUX',1, &
!c                  distrb_clinic%nprocs)
!c   if (ndep_data_type == 'shr_stream') then
!c      call get_timer(ecosys_shr_strdata_advance_timer, &
!c                     'ecosys_shr_strdata_advance',1, distrb_clinic%nprocs)
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!  call other initialization subroutines
!c!-----------------------------------------------------------------------
!c
!c   call ecosys_init_tavg
   call ecosys_init_sflux
   call ecosys_init_interior_restore
!c
!c!-----------------------------------------------------------------------
!c!  set lfull_depth_tavg flag for short-lived ecosystem tracers
!c!-----------------------------------------------------------------------
!c   do zoo_ind = 1, zooplankton_cnt
!c      n = zooplankton(zoo_ind)%C_ind
!c      tracer_d_module(n)%lfull_depth_tavg = lecovars_full_depth_tavg
!c   end do
!c
!c   do auto_ind = 1, autotroph_cnt
!c      n = autotrophs(auto_ind)%Chl_ind
!c      tracer_d_module(n)%lfull_depth_tavg = lecovars_full_depth_tavg
!c
!c      n = autotrophs(auto_ind)%C_ind
!c      tracer_d_module(n)%lfull_depth_tavg = lecovars_full_depth_tavg
!c
!c      n = autotrophs(auto_ind)%Fe_ind
!c      tracer_d_module(n)%lfull_depth_tavg = lecovars_full_depth_tavg
!c
!c      n = autotrophs(auto_ind)%Si_ind
!c      if (n > 0) then
!c         tracer_d_module(n)%lfull_depth_tavg = lecovars_full_depth_tavg
!c      endif
!c
!c      n = autotrophs(auto_ind)%CaCO3_ind
!c      if (n > 0) then
!c         tracer_d_module(n)%lfull_depth_tavg = lecovars_full_depth_tavg
!c      endif
!c   end do
!c
!c!-----------------------------------------------------------------------

#ifdef DIAGNOSTICS_BIO
   PH_PREV_3d(:,:,:,bid) = c0
   tavg_CO3 = 1
   tavg_HCO3 = 1
   tavg_H2CO3 = 1
   tavg_pH_3D = 1
   tavg_zsatcalc = 1
   tavg_zsatarag = 1
#endif

!c!EOC
!c
 end subroutine ecosys_init
!c
!c!***********************************************************************
!c!BOP
!c! !IROUTINE: ecosys_init_tavg
!c! !INTERFACE:
!c
!c subroutine ecosys_init_tavg
!c
!c! !DESCRIPTION:
!c!  call define_tavg_field for nonstandard tavg fields
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c!
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   character(*), parameter :: &
!c      subname = 'ecosys_mod:ecosys_init_tavg'
!c
!c   integer (int_kind) :: &
!c      auto_ind,       & ! autotroph functional group index
!c      zoo_ind,        & ! zooplankton functional group index
!c      buf_len           ! how many surface flux fields are stored in ECO_SFLUX_TAVG
!c
!c   character(char_len) :: &
!c      sname             ! short-name of tavg variable
!c
!c!-----------------------------------------------------------------------
!c!  2D fields related to surface fluxes
!c!-----------------------------------------------------------------------
!c
!c   buf_len = 0
!c
!c   call define_tavg_field(tavg_ECOSYS_IFRAC,'ECOSYS_IFRAC',2,          &
!c                          long_name='Ice Fraction for ecosys fluxes',  &
!c                          units='fraction', grid_loc='2110',           &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_ECOSYS_IFRAC = buf_len
!c
!c   call define_tavg_field(tavg_ECOSYS_IFRAC_2,'ECOSYS_IFRAC_2',2,      &
!c                          long_name='Ice Fraction for ecosys fluxes',  &
!c                          units='fraction', grid_loc='2110',           &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_ECOSYS_XKW,'ECOSYS_XKW',2,              &
!c                          long_name='XKW for ecosys fluxes',           &
!c                          units='cm/s', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_ECOSYS_XKW = buf_len
!c
!c   call define_tavg_field(tavg_ECOSYS_XKW_2,'ECOSYS_XKW_2',2,          &
!c                          long_name='XKW for ecosys fluxes',           &
!c                          units='cm/s', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_ECOSYS_ATM_PRESS,'ECOSYS_ATM_PRESS',2,  &
!c                          long_name='Atmospheric Pressure for ecosys fluxes', &
!c                          units='atmospheres', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_ECOSYS_ATM_PRESS = buf_len
!c
!c   call define_tavg_field(tavg_PV_O2,'PV_O2',2,                        &
!c                          long_name='PV_O2',                           &
!c                          units='cm/s', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_PV_O2 = buf_len
!c
!c   call define_tavg_field(tavg_SCHMIDT_O2,'SCHMIDT_O2',2,              &
!c                          long_name='O2 Schmidt Number',               &
!c                          units='none', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_SCHMIDT_O2 = buf_len
!c
!c   call define_tavg_field(tavg_O2SAT,'O2SAT',2,                        &
!c                          long_name='O2 Saturation',                   &
!c                          units='mmol/m^3', grid_loc='2110',           &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_O2SAT = buf_len
!c
!c   call define_tavg_field(tavg_O2_GAS_FLUX_2,'STF_O2_2',2,             &
!c                          long_name='Dissolved Oxygen Surface Flux',   &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_CO2STAR,'CO2STAR',2,                    &
!c                          long_name='CO2 Star',                        &
!c                          units='mmol/m^3', grid_loc='2110',           &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_CO2STAR = buf_len
!c
!c   call define_tavg_field(tavg_DCO2STAR,'DCO2STAR',2,                  &
!c                          long_name='D CO2 Star',                      &
!c                          units='mmol/m^3', grid_loc='2110',           &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_DCO2STAR = buf_len
!c
!c   call define_tavg_field(tavg_pCO2SURF,'pCO2SURF',2,                  &
!c                          long_name='surface pCO2',                    &
!c                          units='ppmv', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_pCO2SURF = buf_len
!c
!c   call define_tavg_field(tavg_DpCO2,'DpCO2',2,                        &
!c                          long_name='D pCO2',                          &
!c                          units='ppmv', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_DpCO2 = buf_len
!c
!c   call define_tavg_field(tavg_DpCO2_2,'DpCO2_2',2,                    &
!c                          long_name='D pCO2',                          &
!c                          units='ppmv', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_PV_CO2,'PV_CO2',2,                      &
!c                          long_name='CO2 Piston Velocity',             &
!c                          units='cm/s', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_PV_CO2 = buf_len
!c
!c   call define_tavg_field(tavg_SCHMIDT_CO2,'SCHMIDT_CO2',2,            &
!c                          long_name='CO2 Schmidt Number',              &
!c                          units='none', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_SCHMIDT_CO2 = buf_len
!c
!c   call define_tavg_field(tavg_DIC_GAS_FLUX,'FG_CO2',2,                &
!c                          long_name='DIC Surface Gas Flux',            &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_DIC_GAS_FLUX = buf_len
!c
!c   call define_tavg_field(tavg_DIC_GAS_FLUX_2,'FG_CO2_2',2,            &
!c                          long_name='DIC Surface Gas Flux',            &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_PH,'PH',2,                              &
!c                          long_name='Surface pH',                      &
!c                          units='none', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_PH = buf_len
!c
!c   call define_tavg_field(tavg_ATM_CO2,'ATM_CO2',2,                    &
!c                          long_name='Atmospheric CO2',                 &
!c                          units='ppmv', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_ATM_CO2 = buf_len
!c
!c   call define_tavg_field(tavg_CO2STAR_ALT_CO2,'CO2STAR_ALT_CO2',2,    &
!c                          long_name='CO2 Star, Alternative CO2',       &
!c                          units='mmol/m^3', grid_loc='2110',           &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_CO2STAR_ALT_CO2 = buf_len
!c
!c   call define_tavg_field(tavg_DCO2STAR_ALT_CO2,'DCO2STAR_ALT_CO2',2,  &
!c                          long_name='D CO2 Star, Alternative CO2',     &
!c                          units='mmol/m^3', grid_loc='2110',           &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_DCO2STAR_ALT_CO2 = buf_len
!c
!c   call define_tavg_field(tavg_pCO2SURF_ALT_CO2,'pCO2SURF_ALT_CO2',2,  &
!c                          long_name='surface pCO2, Alternative CO2',   &
!c                          units='ppmv', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_pCO2SURF_ALT_CO2 = buf_len
!c
!c   call define_tavg_field(tavg_DpCO2_ALT_CO2,'DpCO2_ALT_CO2',2,        &
!c                          long_name='D pCO2, Alternative CO2',         &
!c                          units='ppmv', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_DpCO2_ALT_CO2 = buf_len
!c
!c   call define_tavg_field(tavg_DIC_GAS_FLUX_ALT_CO2,'FG_ALT_CO2',2,    &
!c                          long_name='DIC Surface Gas Flux, Alternative CO2', &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_DIC_GAS_FLUX_ALT_CO2 = buf_len
!c
!c   call define_tavg_field(tavg_PH_ALT_CO2,'PH_ALT_CO2',2,              &
!c                          long_name='Surface pH, Alternative CO2',     &
!c                          units='none', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_PH_ALT_CO2 = buf_len
!c
!c   call define_tavg_field(tavg_ATM_ALT_CO2,'ATM_ALT_CO2',2,            &
!c                          long_name='Atmospheric Alternative CO2',     &
!c                          units='ppmv', grid_loc='2110',               &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_ATM_ALT_CO2 = buf_len
!c
!c   call define_tavg_field(tavg_IRON_FLUX,'IRON_FLUX',2,                &
!c                          long_name='Atmospheric Iron Flux',           &
!c                          units='mmol/m^2/s', grid_loc='2110',         &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_IRON_FLUX = buf_len
!c
!c   call define_tavg_field(tavg_DUST_FLUX,'DUST_FLUX',2,                &
!c                          long_name='Dust Flux',                       &
!c                          units='g/cm^2/s', grid_loc='2110',           &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_NOx_FLUX,'NOx_FLUX',2,                  &
!c                          long_name='Flux of NOx from Atmosphere',     &
!c                          units='nmol/cm^2/s', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_NOx_FLUX = buf_len
!c
!c   call define_tavg_field(tavg_NHy_FLUX,'NHy_FLUX',2,                  &
!c                          long_name='Flux of NHy from Atmosphere',     &
!c                          units='nmol/cm^2/s', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_DIN_RIV_FLUX,'DIN_RIV_FLUX',2,          &
!c                          long_name='Flux of DIN from rivers',         &
!c                          units='nmol/cm^2/s', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_DIN_RIV_FLUX = buf_len
!c
!c   call define_tavg_field(tavg_DIP_RIV_FLUX,'DIP_RIV_FLUX',2,          &
!c                          long_name='Flux of DIP from rivers',         &
!c                          units='nmol/cm^2/s', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_DON_RIV_FLUX,'DON_RIV_FLUX',2,          &
!c                          long_name='Flux of DON from rivers',         &
!c                          units='nmol/cm^2/s', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_DONr_RIV_FLUX,'DONr_RIV_FLUX',2,        &
!c                          long_name='Flux of DONr from rivers',        &
!c                          units='nmol/cm^2/s', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_DOP_RIV_FLUX,'DOP_RIV_FLUX',2,          &
!c                          long_name='Flux of DOP from rivers',         &
!c                          units='nmol/cm^2/s', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_DOPr_RIV_FLUX,'DOPr_RIV_FLUX',2,        &
!c                          long_name='Flux of DOPr from rivers',        &
!c                          units='nmol/cm^2/s', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_DSI_RIV_FLUX,'DSI_RIV_FLUX',2,          &
!c                          long_name='Flux of DSI from rivers',         &
!c                          units='nmol/cm^2/s', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_DFE_RIV_FLUX,'DFE_RIV_FLUX',2,          &
!c                          long_name='Flux of DFE from rivers',         &
!c                          units='nmol/cm^2/s', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_DFE_RIV_FLUX = buf_len
!c
!c   call define_tavg_field(tavg_DIC_RIV_FLUX,'DIC_RIV_FLUX',2,          &
!c                          long_name='Flux of DIC from rivers',         &
!c                          units='nmol/cm^2/s', grid_loc='2110',        &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_DIC_RIV_FLUX = buf_len
!c
!c   call define_tavg_field(tavg_ALK_RIV_FLUX,'ALK_RIV_FLUX',2,          &
!c                          long_name='Flux of ALK from rivers',         &
!c                          units='alk/cm^2/s', grid_loc='2110',         &
!c                          coordinates='TLONG TLAT time')
!c   buf_len = buf_len+1
!c   buf_ind_ALK_RIV_FLUX = buf_len
!c
!c   call define_tavg_field(tavg_DOC_RIV_FLUX,'DOC_RIV_FLUX',2,          &
!c                          long_name='Flux of DOC from rivers',         &
!c                          units='alk/cm^2/s', grid_loc='2110',         &
!c                          coordinates='TLONG TLAT time')
!c
!c!-----------------------------------------------------------------------
!c
!c   allocate(ECO_SFLUX_TAVG(nx_block,ny_block,buf_len,max_blocks_clinic))
!c   ECO_SFLUX_TAVG = c0
!c
!c!-----------------------------------------------------------------------
!c!  nonstandard 2D fields
!c!-----------------------------------------------------------------------
!c
!c   call define_tavg_field(tavg_O2_ZMIN,'O2_ZMIN',2,                    &
!c                          long_name='Vertical Minimum of O2',          &
!c                          units='mmol/m^3', grid_loc='2110',           &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_O2_ZMIN_DEPTH,'O2_ZMIN_DEPTH',2,        &
!c                          long_name='Depth of Vertical Minimum of O2', &
!c                          units='cm', grid_loc='2110',                 &
!c                          coordinates='TLONG TLAT time')
!c
!c!-----------------------------------------------------------------------
!c!  nonstandard 3D fields
!c!-----------------------------------------------------------------------
!c
!c   call define_tavg_field(tavg_O2_PRODUCTION,'O2_PRODUCTION',3,        &
!c                          long_name='O2 Production',                   &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_O2_CONSUMPTION,'O2_CONSUMPTION',3,      &
!c                          long_name='O2 Consumption',                  &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_AOU,'AOU',3,                            &
!c                          long_name='Apparent O2 Utilization ',        &
!c                          units='mmol/m^3', grid_loc='3111',           &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_PAR_avg,'PAR_avg',3,                    &
!c                          long_name='PAR Average over Model Cell',     &
!c                          units='w/m^2', grid_loc='3114',              &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_POC_FLUX_IN,'POC_FLUX_IN',3,            &
!c                          long_name='POC Flux into Cell',              &
!c                          units='mmol/m^3 cm/s', grid_loc='3111',      &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_POC_PROD,'POC_PROD',3,                  &
!c                          long_name='POC Production',                  &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_POC_REMIN,'POC_REMIN',3,                &
!c                          long_name='POC Remineralization',            &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_POC_ACCUM,'POC_ACCUM',3,                &
!c                          long_name='POC Accumulation',                &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_CaCO3_FLUX_IN,'CaCO3_FLUX_IN',3,        &
!c                          long_name='CaCO3 flux into cell',            &
!c                          units='mmol/m^3 cm/s', grid_loc='3111',      &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_CaCO3_PROD,'CaCO3_PROD',3,              &
!c                          long_name='CaCO3 Production',                &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_CaCO3_REMIN,'CaCO3_REMIN',3,            &
!c                          long_name='CaCO3 Remineralization',          &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_SiO2_FLUX_IN,'SiO2_FLUX_IN',3,          &
!c                          long_name='SiO2 Flux into Cell',             &
!c                          units='mmol/m^3 cm/s', grid_loc='3111',      &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_SiO2_PROD,'SiO2_PROD',3,                &
!c                          long_name='SiO2 Production',                 &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_SiO2_REMIN,'SiO2_REMIN',3,              &
!c                          long_name='SiO2 Remineralization',           &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_dust_FLUX_IN,'dust_FLUX_IN',3,          &
!c                          long_name='Dust Flux into Cell',             &
!c                          units='ng/s/m^2', grid_loc='3111',           &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_dust_REMIN,'dust_REMIN',3,              &
!c                          long_name='Dust Remineralization',           &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_P_iron_FLUX_IN,'P_iron_FLUX_IN',3,      &
!c                          long_name='P_iron Flux into Cell',           &
!c                          units='mmol/m^3 cm/s', grid_loc='3111',      &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_P_iron_PROD,'P_iron_PROD',3,            &
!c                          long_name='P_iron Production',               &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_P_iron_REMIN,'P_iron_REMIN',3,          &
!c                          long_name='P_iron Remineralization',         &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_auto_graze_TOT,'graze_auto_TOT',3,      &
!c                          long_name='Total Autotroph Grazing',         &
!c                          units='mmol/m^3/s', grid_loc='3114',         &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   do zoo_ind = 1, zooplankton_cnt
!c      call define_tavg_field(tavg_zoo_loss(zoo_ind),                   &
!c           trim(zooplankton(zoo_ind)%sname) // '_loss', 3,             &
!c           long_name=trim(zooplankton(zoo_ind)%lname) // ' Loss',      &
!c           units='mmol/m^3/s', grid_loc='3114',                        &
!c           coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_zoo_loss_poc(zoo_ind),                  &
!c           trim(zooplankton(zoo_ind)%sname) // '_loss_poc', 3,            &
!c           long_name=trim(zooplankton(zoo_ind)%lname) // ' Loss to POC',  &
!c           units='mmol/m^3/s', grid_loc='3114',                           &
!c           coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_zoo_loss_doc(zoo_ind),                  &
!c           trim(zooplankton(zoo_ind)%sname) // '_loss_doc', 3,            &
!c           long_name=trim(zooplankton(zoo_ind)%lname) // ' Loss to DOC',  &
!c           units='mmol/m^3/s', grid_loc='3114',                           &
!c           coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_zoo_graze(zoo_ind),                       &
!c           'graze_' // trim(zooplankton(zoo_ind)%sname), 3,                 &
!c           long_name=trim(zooplankton(zoo_ind)%lname) // ' grazing loss',   &
!c           units='mmol/m^3/s', grid_loc='3114',                             &
!c           coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_zoo_graze_poc(zoo_ind),                        &
!c           'graze_' // trim(zooplankton(zoo_ind)%sname) // '_poc', 3,            &
!c           long_name=trim(zooplankton(zoo_ind)%lname) // ' grazing loss to POC', &
!c           units='mmol/m^3/s', grid_loc='3114',                                  &
!c           coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_zoo_graze_doc(zoo_ind),                        &
!c           'graze_' // trim(zooplankton(zoo_ind)%sname) // '_doc', 3,            &
!c           long_name=trim(zooplankton(zoo_ind)%lname) // ' grazing loss to DOC', &
!c           units='mmol/m^3/s', grid_loc='3114',                                  &
!c           coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_zoo_graze_zoo(zoo_ind),                        &
!c           'graze_' // trim(zooplankton(zoo_ind)%sname) // '_zoo', 3,            &
!c           long_name=trim(zooplankton(zoo_ind)%lname) // ' grazing loss to ZOO', &
!c           units='mmol/m^3/s', grid_loc='3114',                                  &
!c           coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_x_graze_zoo(zoo_ind),                    &
!c           'x_graze_' // trim(zooplankton(zoo_ind)%sname), 3,              &
!c           long_name=trim(zooplankton(zoo_ind)%lname) // ' grazing gain',  &
!c           units='mmol/m^3/s', grid_loc='3114',                            &
!c           coordinates='TLONG TLAT z_t_150m time')
!c   end do
!c
!c   call define_tavg_field(tavg_photoC_TOT,'photoC_TOT',3,              &
!c                          long_name='Total C Fixation',                &
!c                          units='mmol/m^3/s', grid_loc='3114',         &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_photoC_TOT_zint,'photoC_TOT_zint',2,    &
!c                          long_name='Total C Fixation Vertical Integral',&
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_photoC_NO3_TOT,'photoC_NO3_TOT',3,      &
!c                          long_name='Total C Fixation from NO3',            &
!c                          units='mmol/m^3/s', grid_loc='3114',         &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_photoC_NO3_TOT_zint,'photoC_NO3_TOT_zint',2,&
!c                          long_name='Total C Fixation from NO3 Vertical Integral',&
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c!-----------------------------------------------------------------------
!c!  MORE nonstandard 3D fields
!c!-----------------------------------------------------------------------
!c
!c   call define_tavg_field(tavg_DOC_prod,'DOC_prod',3,                  &
!c                          long_name='DOC Production',                  &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_DOC_remin,'DOC_remin',3,                &
!c                          long_name='DOC Remineralization',            &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_DON_prod,'DON_prod',3,                  &
!c                          long_name='DON Production',                  &
!c                          units='mmol/m^3/s', grid_loc='3114',          &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_DON_remin,'DON_remin',3,                &
!c                          long_name='DON Remineralization',            &
!c                          units='mmol/m^3/s', grid_loc='3114',         &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_DOFe_prod,'DOFe_prod',3,                &
!c                          long_name='DOFe Production',                 &
!c                          units='mmol/m^3/s', grid_loc='3114',         &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_DOFe_remin,'DOFe_remin',3,              &
!c                          long_name='DOFe Remineralization',           &
!c                          units='mmol/m^3/s', grid_loc='3114',         &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_DOP_prod,'DOP_prod',3,                  &
!c                          long_name='DOP Production',                  &
!c                          units='mmol/m^3/s', grid_loc='3114',         &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_DOP_remin,'DOP_remin',3,                &
!c                          long_name='DOP Remineralization',            &
!c                          units='mmol/m^3/s', grid_loc='3114',         &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_Fe_scavenge,'Fe_scavenge',3,            &
!c                          long_name='Iron Scavenging',                 &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_Fe_scavenge_rate,'Fe_scavenge_rate',3,  &
!c                          long_name='Iron Scavenging Rate',            &
!c                          units='1/y', grid_loc='3111',                &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c
!c!-----------------------------------------------------------------------
!c!  nonstandard 2D & 3D fields for each autotroph
!c!-----------------------------------------------------------------------
!c
!c   do auto_ind = 1, autotroph_cnt
!c      call define_tavg_field(tavg_photoC(auto_ind), &
!c                             'photoC_' // trim(autotrophs(auto_ind)%sname), 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' C Fixation', &
!c                             units='mmol/m^3/s', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_photoC_zint(auto_ind), &
!c                             'photoC_' // trim(autotrophs(auto_ind)%sname) // '_zint', 2, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' C Fixation Vertical Integral', &
!c                             units='mmol/m^3 cm/s', grid_loc='2110', &
!c                             coordinates='TLONG TLAT time')
!c
!c      call define_tavg_field(tavg_photoC_NO3(auto_ind), &
!c                             'photoC_NO3_' // trim(autotrophs(auto_ind)%sname), 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' C Fixation from NO3', &
!c                             units='mmol/m^3/s', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_photoC_NO3_zint(auto_ind), &
!c                             'photoC_NO3_' // trim(autotrophs(auto_ind)%sname) // '_zint',2, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' C Fixation from NO3 Vertical Integral', &
!c                             units='mmol/m^3 cm/s', grid_loc='2110', &
!c                             coordinates='TLONG TLAT time')
!c
!c      call define_tavg_field(tavg_photoFe(auto_ind), &
!c                             'photoFe_' // trim(autotrophs(auto_ind)%sname), 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' Fe Uptake', &
!c                             units='mmol/m^3/s', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_photoNO3(auto_ind), &
!c                             'photoNO3_' // trim(autotrophs(auto_ind)%sname), 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' NO3 Uptake', &
!c                             units='mmol/m^3/s', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_photoNH4(auto_ind), &
!c                             'photoNH4_' // trim(autotrophs(auto_ind)%sname), 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' NH4 Uptake', &
!c                             units='mmol/m^3/s', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_DOP_uptake(auto_ind), &
!c                             'DOP_' // trim(autotrophs(auto_ind)%sname) // '_uptake', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' DOP Uptake', &
!c                             units='mmol/m^3/s', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_PO4_uptake(auto_ind), &
!c                             'PO4_' // trim(autotrophs(auto_ind)%sname) // '_uptake', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' PO4 Uptake', &
!c                             units='mmol/m^3/s', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_N_lim(auto_ind), &
!c                             trim(autotrophs(auto_ind)%sname) // '_N_lim', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' N Limitation', &
!c                             units='none', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_P_lim(auto_ind), &
!c                             trim(autotrophs(auto_ind)%sname) // '_P_lim', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' P Limitation', &
!c                             units='none', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_Fe_lim(auto_ind), &
!c                             trim(autotrophs(auto_ind)%sname) // '_Fe_lim', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' Fe Limitation', &
!c                             units='none', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_SiO3_lim(auto_ind), &
!c                             trim(autotrophs(auto_ind)%sname) // '_SiO3_lim', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' SiO3 Limitation', &
!c                             units='none', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_light_lim(auto_ind), &
!c                             trim(autotrophs(auto_ind)%sname) // '_light_lim', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' Light Limitation', &
!c                             units='none', grid_loc='3114', &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_auto_graze(auto_ind), &
!c                             'graze_' // trim(autotrophs(auto_ind)%sname), 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' Grazing', &
!c                             units='mmol/m^3/s', grid_loc='3114',         &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_auto_graze_poc(auto_ind), &
!c                             'graze_' // trim(autotrophs(auto_ind)%sname) // '_poc', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' Grazing to POC', &
!c                             units='mmol/m^3/s', grid_loc='3114',         &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_auto_graze_doc(auto_ind), &
!c                             'graze_' // trim(autotrophs(auto_ind)%sname) // '_doc', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' Grazing to DOC', &
!c                             units='mmol/m^3/s', grid_loc='3114',         &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_auto_graze_zoo(auto_ind), &
!c                             'graze_' // trim(autotrophs(auto_ind)%sname) // '_zoo', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' Grazing to ZOO', &
!c                             units='mmol/m^3/s', grid_loc='3114',         &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_auto_loss(auto_ind), &
!c                             trim(autotrophs(auto_ind)%sname) // '_loss', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' Loss', &
!c                             units='mmol/m^3/s', grid_loc='3114',         &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_auto_loss_doc(auto_ind), &
!c                             trim(autotrophs(auto_ind)%sname) // '_loss_doc', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' Loss to DOC', &
!c                             units='mmol/m^3/s', grid_loc='3114',         &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_auto_loss_poc(auto_ind), &
!c                             trim(autotrophs(auto_ind)%sname) // '_loss_poc', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' Loss to POC', &
!c                             units='mmol/m^3/s', grid_loc='3114',         &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      call define_tavg_field(tavg_auto_agg(auto_ind), &
!c                             trim(autotrophs(auto_ind)%sname) // '_agg', 3, &
!c                             long_name=trim(autotrophs(auto_ind)%lname) // ' Aggregate', &
!c                             units='mmol/m^3/s', grid_loc='3114',         &
!c                             coordinates='TLONG TLAT z_t_150m time')
!c
!c      if (autotrophs(auto_ind)%Si_ind > 0) then
!c         sname = trim(autotrophs(auto_ind)%sname) // 'bSi_form'
!c         call define_tavg_field(tavg_bSi_form(auto_ind), sname, 3, &
!c                                long_name=trim(autotrophs(auto_ind)%lname) // ' Si Uptake', &
!c                                units='mmol/m^3/s', grid_loc='3114', &
!c                                coordinates='TLONG TLAT z_t_150m time')
!c      endif
!c
!c      if (autotrophs(auto_ind)%CaCO3_ind > 0) then
!c         sname = trim(autotrophs(auto_ind)%sname) // '_CaCO3_form'
!c         call define_tavg_field(tavg_CaCO3_form(auto_ind), sname, 3, &
!c                                long_name=trim(autotrophs(auto_ind)%lname) // ' CaCO3 Formation', &
!c                                units='mmol/m^3/s', grid_loc='3114', &
!c                                coordinates='TLONG TLAT z_t_150m time')
!c
!c         sname = trim(sname) // '_zint'
!c         call define_tavg_field(tavg_CaCO3_form_zint(auto_ind), sname, 2, &
!c                                long_name=trim(autotrophs(auto_ind)%lname) // ' CaCO3 Formation Vertical Integral', &
!c                                units='mmol/m^3 cm/s', grid_loc='2110', &
!c                                coordinates='TLONG TLAT time')
!c      endif
!c
!c      if (autotrophs(auto_ind)%Nfixer) then
!c         call define_tavg_field(tavg_Nfix(auto_ind), &
!c                                trim(autotrophs(auto_ind)%sname) // '_Nfix', 3, &
!c                                long_name=trim(autotrophs(auto_ind)%lname) // ' N Fixation', &
!c                                units='mmol/m^3/s', grid_loc='3114',   &
!c                                coordinates='TLONG TLAT z_t_150m time')
!c      endif
!c   end do
!c
!c   call define_tavg_field(tavg_tot_bSi_form, 'bSi_form', 3, &
!c                          long_name='Total Si Uptake', &
!c                          units='mmol/m^3/s', grid_loc='3114', &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_tot_CaCO3_form, 'CaCO3_form', 3, &
!c                          long_name='Total CaCO3 Formation', &
!c                          units='mmol/m^3/s', grid_loc='3114', &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_tot_CaCO3_form_zint, 'CaCO3_form_zint', 2, &
!c                          long_name='Total CaCO3 Formation Vertical Integral', &
!c                          units='mmol/m^3 cm/s', grid_loc='2110', &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_tot_Nfix, 'Nfix', 3, &
!c                          long_name='Total N Fixation', &
!c                          units='mmol/m^3/s', grid_loc='3114',   &
!c                          coordinates='TLONG TLAT z_t_150m time')
!c
!c   call define_tavg_field(tavg_NITRIF,'NITRIF',3,                      &
!c                          long_name='Nitrification',                   &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_DENITRIF,'DENITRIF',3,                  &
!c                          long_name='Denitrification',                 &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_DONr_REMIN,'DONr_REMIN',3,              &
!c                          long_name='DONr Remineralization',           &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_DOPr_REMIN,'DOPr_REMIN',3,              &
!c                          long_name='DOPr Remineralization',           &
!c                          units='mmol/m^3/s', grid_loc='3111',         &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_CO3,'CO3',3,                            &
!c                          long_name='Carbonate Ion Concentration',     &
!c                          units='mmol/m^3', grid_loc='3111',           &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_HCO3,'HCO3',3,                          &
!c                          long_name='Bicarbonate Ion Concentration',   &
!c                          units='mmol/m^3', grid_loc='3111',           &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_H2CO3,'H2CO3',3,                        &
!c                          long_name='Carbonic Acid Concentration',     &
!c                          units='mmol/m^3', grid_loc='3111',           &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_pH_3D,'pH_3D',3,                        &
!c                          long_name='pH',                              &
!c                          units='none', grid_loc='3111',               &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_CO3_ALT_CO2,'CO3_ALT_CO2',3,            &
!c                          long_name='Carbonate Ion Concentration, Alternative CO2', &
!c                          units='mmol/m^3', grid_loc='3111',           &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_HCO3_ALT_CO2,'HCO3_ALT_CO2',3,          &
!c                          long_name='Bicarbonate Ion Concentration, Alternative CO2', &
!c                          units='mmol/m^3', grid_loc='3111',           &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_H2CO3_ALT_CO2,'H2CO3_ALT_CO2',3,        &
!c                          long_name='Carbonic Acid Concentration, Alternative CO2', &
!c                          units='mmol/m^3', grid_loc='3111',           &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_pH_3D_ALT_CO2,'pH_3D_ALT_CO2',3,        &
!c                          long_name='pH, Alternative CO2',             &
!c                          units='none', grid_loc='3111',               &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_co3_sat_calc,'co3_sat_calc',3,          &
!c                          long_name='CO3 concentration at calcite saturation', &
!c                          units='mmol/m^3', grid_loc='3111',           &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_zsatcalc,'zsatcalc',2,                  &
!c                          long_name='Calcite Saturation Depth',        &
!c                          units='cm', grid_loc='2110',                 &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_co3_sat_arag,'co3_sat_arag',3,          &
!c                          long_name='CO3 concentration at aragonite saturation', &
!c                          units='mmol/m^3', grid_loc='3111',           &
!c                          coordinates='TLONG TLAT z_t time')
!c
!c   call define_tavg_field(tavg_zsatarag,'zsatarag',2,                  &
!c                          long_name='Aragonite Saturation Depth',      &
!c                          units='cm', grid_loc='2110',                 &
!c                          coordinates='TLONG TLAT time')
!c
!c!-----------------------------------------------------------------------
!c!  Vars to sum up burial in sediments and sed Denitrif N losses
!c!-----------------------------------------------------------------------
!c
!c   call define_tavg_field(tavg_calcToSed,'calcToSed',2,                &
!c                          long_name='CaCO3 Flux to Sediments',         &
!c                          units='nmolC/cm^2/s', grid_loc='2110',       &
!c                          coordinates='TLONG TLAT time')
!c   call define_tavg_field(tavg_pocToSed,'pocToSed',2,                  &
!c                          long_name='POC Flux to Sediments',           &
!c                          units='nmolC/cm^2/s', grid_loc='2110',       &
!c                          coordinates='TLONG TLAT time')
!c   call define_tavg_field(tavg_ponToSed,'ponToSed',2,                  &
!c                          long_name='nitrogen burial Flux to Sediments',&
!c                          units='nmolN/cm^2/s', grid_loc='2110',       &
!c                          coordinates='TLONG TLAT time')
!c   call define_tavg_field(tavg_SedDenitrif,'SedDenitrif',2,            &
!c                          long_name='nitrogen loss in Sediments',&
!c                          units='nmolN/cm^2/s', grid_loc='2110',       &
!c                          coordinates='TLONG TLAT time')
!c   call define_tavg_field(tavg_OtherRemin,'OtherRemin',2,              &
!c                          long_name='non-oxic,non-dentr remin in Sediments', &
!c                          units='nmolC/cm^2/s', grid_loc='2110',       &
!c                          coordinates='TLONG TLAT time')
!c   call define_tavg_field(tavg_popToSed,'popToSed',2,                  &
!c                          long_name='phosporus Flux to Sediments',     &
!c                          units='nmolP/cm^2/s', grid_loc='2110',       &
!c                          coordinates='TLONG TLAT time')
!c   call define_tavg_field(tavg_bsiToSed,'bsiToSed',2,                  &
!c                          long_name='biogenic Si Flux to Sediments',   &
!c                          units='nmolSi/cm^2/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c   call define_tavg_field(tavg_dustToSed,'dustToSed',2,                &
!c                          long_name='dust Flux to Sediments',          &
!c                          units='g/cm^2/s', grid_loc='2110',           &
!c                          coordinates='TLONG TLAT time')
!c   call define_tavg_field(tavg_pfeToSed,'pfeToSed',2,                  &
!c                          long_name='pFe Flux to Sediments',           &
!c                          units='nmolFe/cm^2/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c!-----------------------------------------------------------------------
!c!  fields related to conservation of total C, N, P, Si
!c!-----------------------------------------------------------------------
!c
!c   call define_tavg_field(tavg_Jint_Ctot,'Jint_Ctot',2,                &
!c                          long_name='Vertical Integral of Conservative Subterms of Source Sink Term for Ctot', &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_Jint_100m_Ctot,'Jint_100m_Ctot',2,      &
!c                          long_name='Vertical Integral of Conservative Subterms of Source Sink Term for Ctot, 0-100m', &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_Jint_Ntot,'Jint_Ntot',2,                &
!c                          long_name='Vertical Integral of Conservative Subterms of Source Sink Term for Ntot', &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_Jint_100m_Ntot,'Jint_100m_Ntot',2,      &
!c                          long_name='Vertical Integral of Conservative Subterms of Source Sink Term for Ntot, 0-100m', &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_Jint_Ptot,'Jint_Ptot',2,                &
!c                          long_name='Vertical Integral of Conservative Subterms of Source Sink Term for Ptot', &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_Jint_100m_Ptot,'Jint_100m_Ptot',2,      &
!c                          long_name='Vertical Integral of Conservative Subterms of Source Sink Term for Ptot, 0-100m', &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_Jint_Sitot,'Jint_Sitot',2,              &
!c                          long_name='Vertical Integral of Conservative Subterms of Source Sink Term for Sitot', &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c   call define_tavg_field(tavg_Jint_100m_Sitot,'Jint_100m_Sitot',2,    &
!c                          long_name='Vertical Integral of Conservative Subterms of Source Sink Term for Sitot, 0-100m', &
!c                          units='mmol/m^3 cm/s', grid_loc='2110',      &
!c                          coordinates='TLONG TLAT time')
!c
!c
!c   call ecosys_restore%define_tavg_fields()
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end subroutine ecosys_init_tavg
!c
!***********************************************************************
!BOP
! !IROUTINE: ecosys_set_interior
! !INTERFACE:


 subroutine ecosys_set_interior(k, TEMP_OLD, TEMP_CUR, SALT_OLD, SALT_CUR, &
#ifdef BEC
    TRACER_MODULE_OLD, TRACER_MODULE_CUR, DTRACER_MODULE, &
# ifdef LIGAND3D
    lig3d, &
# endif
# ifdef DICE
    ice,   &
# endif
    lexport_shared_vars)
#else
    TRACER_MODULE_OLD, TRACER_MODULE_CUR, DTRACER_MODULE, lexport_shared_vars, this_block)
#endif

! !DESCRIPTION:
!  Compute time derivatives for ecosystem state variables
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   integer (int_kind), intent(in) :: &
      k                   ! vertical level index

   real (r8), dimension(:,:), intent(in) :: &
      TEMP_OLD,          &! old potential temperature (C)
      TEMP_CUR,          &! current potential temperature (C)
      SALT_OLD,          &! old salinity (msu)
      SALT_CUR            ! current salinity (msu)

   real (r8), dimension(:,:,:), intent(in) :: &
      TRACER_MODULE_OLD, &! old tracer values
      TRACER_MODULE_CUR   ! current tracer values
#ifdef LIGAND3D
   real (r8), dimension(:,:), intent(in) :: &
      lig3d               ! 3D ligand
#endif
#ifdef DICE
   real (r8), dimension(:,:), intent(in) :: &
      ice                 ! data ice
#endif

   logical (log_kind), intent(in) :: &
      lexport_shared_vars ! flag to save shared_vars or not

#ifndef BEC
   type (block), intent(in) :: &
      this_block          ! block info for the current block
#endif

! !OUTPUT PARAMETERS:

   real (r8), dimension(:,:,:), intent(out) :: &
      DTRACER_MODULE      ! computed source/sink terms

!EOP
!BOC
!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

   character(*), parameter :: &
      subname = 'ecosys_mod:ecosys_set_interior'

   real (r8), parameter :: &
      epsC      = 1.00e-8, & ! small C concentration (mmol C/m^3)
      epsTinv   = 3.17e-8, & ! small inverse time scale (1/year) (1/sec)
      epsnondim = 1.00e-6    ! small non-dimensional number (non-dim)

#ifdef BEC
   real (r8), dimension(:,:,:), allocatable, save :: &
      QA_dust_def,      & ! incoming deficit in the QA(dust) POC flux
      SED_DENITRIF,     & ! sedimentary denitrification (nmol N/cm^3/sec)
      OTHER_REMIN,      & ! organic C remin not due oxic or denitrif (nmolC/cm^3/sec)
      ZSATCALC,         & ! Calcite Saturation Depth
      ZSATARAG,         & ! Aragonite Saturation Depth
      CO3_CALC_ANOM_km1,& ! CO3 concentration above calcite saturation at k-1
      CO3_ARAG_ANOM_km1   ! CO3 concentration above aragonite saturation at k-1
#else
   real (r8), dimension(nx_block,ny_block,max_blocks_clinic), save :: &
      QA_dust_def,      & ! incoming deficit in the QA(dust) POC flux
      SED_DENITRIF,     & ! sedimentary denitrification (nmol N/cm^3/sec)
      OTHER_REMIN,      & ! organic C remin not due oxic or denitrif (nmolC/cm^3/sec)
      ZSATCALC,         & ! Calcite Saturation Depth
      ZSATARAG,         & ! Aragonite Saturation Depth
      CO3_CALC_ANOM_km1,& ! CO3 concentration above calcite saturation at k-1
      CO3_ARAG_ANOM_km1   ! CO3 concentration above aragonite saturation at k-1
#endif

   ! FIXME(bja, 2014-10) size should be (nx, ny, non_autotroph_ecosys_tracer_cnt)
   real (r8), dimension(nx_block, ny_block, ecosys_tracer_cnt) :: tracer_local ! local copies of model tracer concentrations
   real (r8), dimension(nx_block, ny_block, ecosys_tracer_cnt) :: restore_local ! local restoring terms for nutrients (mmol ./m^3/sec)

   real (r8), dimension(nx_block,ny_block) :: &
      TEMP,           & ! local copy of model TEMP
      SALT,           & ! local copy of model SALT
      DIC_loc,        & ! local copy of model DIC
      DIC_ALT_CO2_loc,& ! local copy of model DIC_ALT_CO2
      ALK_loc,        & ! local copy of model ALK
      PO4_loc,        & ! local copy of model PO4
      NO3_loc,        & ! local copy of model NO3
      SiO3_loc,       & ! local copy of model SiO3
      NH4_loc,        & ! local copy of model NH4
      O2_loc,         & ! local copy of model O2
      DOC_loc,        & ! local copy of model DOC
      DON_loc,        & ! local copy of model DON
      DOP_loc,        & ! local copy of model DOP
      DOPr_loc,       & ! local copy of model DOPr
      DONr_loc          ! local copy of model DONr

   real (r8), dimension(nx_block,ny_block) :: &
      Fe_loc,         & ! local copy of model Fe
      DOFe_loc          ! local copy of model DOFe

#ifdef FE_TAG
   real (r8), dimension(nx_block,ny_block) :: &
      Fe0_loc,         & ! local copy of model Fe
      DOFe0_loc,       & ! local copy of model DOFe
      Fe1_loc,         & ! local copy of model Fe
      DOFe1_loc,       & ! local copy of model DOFe
      Fe2_loc,         & ! local copy of model Fe
      DOFe2_loc,       & ! local copy of model DOFe
      Fe3_loc,         & ! local copy of model Fe
      DOFe3_loc,       & ! local copy of model DOFe
      Fe4_loc,         & ! local copy of model Fe
      DOFe4_loc,       & ! local copy of model DOFe
      Fe5_loc,         & ! local copy of model Fe
      DOFe5_loc,       & ! local copy of model DOFe
      Fe6_loc,         & ! local copy of model Fe
      DOFe6_loc,       & ! local copy of model DOFe
      Fe7_loc,         & ! local copy of model Fe
      DOFe7_loc          ! local copy of model DOFe
#endif

   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
      zooC_loc          ! local copy of model zooplankton C

#ifdef FE_TAG
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
      zooFe_loc,      & ! local copy of model zooplankton Fe
      zooFe0_loc,     & ! local copy of model zooplankton Fe
      zooFe1_loc,     & ! local copy of model zooplankton Fe
      zooFe2_loc,     & ! local copy of model zooplankton Fe
      zooFe3_loc,     & ! local copy of model zooplankton Fe
      zooFe4_loc,     & ! local copy of model zooplankton Fe
      zooFe5_loc,     & ! local copy of model zooplankton Fe
      zooFe6_loc,     & ! local copy of model zooplankton Fe
      zooFe7_loc        ! local copy of model zooplankton Fe
#endif

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      autotrophChl_loc, & ! local copy of model autotroph Chl
      autotrophC_loc,   & ! local copy of model autotroph C
      autotrophSi_loc,  & ! local copy of model autotroph Si
      autotrophCaCO3_loc  ! local copy of model autotroph CaCO3

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      autotrophFe_loc     !

#ifdef FE_TAG
   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      autotrophFe0_loc, & !
      autotrophFe1_loc, & !
      autotrophFe2_loc, & !
      autotrophFe3_loc, & !
      autotrophFe4_loc, & !
      autotrophFe5_loc, & !
      autotrophFe6_loc, & !
      autotrophFe7_loc    !
#endif

   logical (log_kind), dimension(nx_block,ny_block) :: ZERO_MASK

   real (r8), dimension(nx_block,ny_block) :: &
      WORK1,WORK2,WORK3,WORK4 ! temporaries
#ifdef FE_TAG
   real (r8), dimension(nx_block,ny_block) :: &
      WORK1_Fe, WORK2_Fe
#endif

#ifdef BEC
   real (r8), dimension(nx_block,ny_block) :: &
      f_loss_thres,   & ! fraction of grazing loss reduction at depth
      C_loss_thres      ! bio-C threshold at which losses go to zero (mmol C/m^3) ! moved to ifdef block ( misumi )
#else
   real (r8) :: &
      f_loss_thres,   & ! fraction of grazing loss reduction at depth
      C_loss_thres      ! bio-C threshold at which losses go to zero (mmol C/m^3) ! moved to ifdef block ( misumi )
#endif

   real (r8) :: &
!      f_loss_thres,    &! fraction of grazing loss reduction at depth  ! moved to ifdef block ( misumi )
      ztop              ! depth of top of cell

   real (r8), dimension(nx_block,ny_block) :: &
      PAR_in,         & ! photosynthetically available radiation (W/m^2)
      KPARdz,         & ! PAR adsorption coefficient (non-dim)
      PAR_avg,        & ! average PAR over mixed layer depth (W/m^2)
      DOC_prod,       & ! production of DOC (mmol C/m^3/sec)
      DOC_remin,      & ! remineralization of DOC (mmol C/m^3/sec)
      DON_remin,      & ! portion of DON remineralized
      DOP_remin,      & ! portion of DOP remineralized
      NITRIF,         & ! nitrification (NH4 -> NO3) (mmol N/m^3/sec)
      DENITRIF          ! WC nitrification (NO3 -> N2) (mmol N/m^3/sec)

   real (r8), dimension(nx_block,ny_block) :: &
      DOFe_remin       ! portion of DOFe remineralized
#ifdef FE_TAG
   real (r8), dimension(nx_block,ny_block) :: &
      DOFe0_remin,    & ! portion of DOFe remineralized
      DOFe1_remin,    & ! portion of DOFe remineralized
      DOFe2_remin,    & ! portion of DOFe remineralized
      DOFe3_remin,    & ! portion of DOFe remineralized
      DOFe4_remin,    & ! portion of DOFe remineralized
      DOFe5_remin,    & ! portion of DOFe remineralized
      DOFe6_remin,    & ! portion of DOFe remineralized
      DOFe7_remin       ! portion of DOFe remineralized
#endif

   real (r8), dimension(nx_block,ny_block) :: &
      graze_rate           ! max. zoo growth rate at local T (1/sec)
!      C_loss_thres      ! bio-C threshold at which losses go to zero (mmol C/m^3) ! moved to ifdef block ( misumi )

   real (r8), dimension(nx_block,ny_block) :: &
      Tfunc,          & ! temp response function GD98 (non-dim)
      f_nut,          & ! nut limitation factor, modifies C fixation (non-dim)
      PCmax,          & ! max value of PCphoto at temperature TEMP (1/sec)
      light_lim,      & ! light limitation factor
      pChl              ! Chl synth. regulation term (mg Chl/mmol N)

   real (r8), dimension(nx_block,ny_block) :: & ! max of 39 continuation lines
      Fe_scavenge_rate  ! annual scavenging rate of iron as % of ambient

#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block) :: &
      Fe_scavenge       ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block) :: &
      Fe_scavenge ,    & ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
      Fe0_scavenge,    & ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
      Fe1_scavenge,    & ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
      Fe2_scavenge,    & ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
      Fe3_scavenge,    & ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
      Fe4_scavenge,    & ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
      Fe5_scavenge,    & ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
      Fe6_scavenge,    & ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
      Fe7_scavenge       ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)

   real (r8), dimension(nx_block,ny_block) :: &
      s, s0, s1, s2, s3, s4, s5, s6, s7
#endif

   real (r8), dimension(nx_block,ny_block) :: &
      VNC,            & ! C-specific N uptake rate (mmol N/mmol C/sec)
      VPO4,           & ! C-specific PO4 uptake (non-dim)
      VDOP,           & ! C-specific DOP uptake rate (non-dim)
      VPtot,          & ! total P uptake rate (non-dim)
      VFe,            & ! C-specific Fe uptake (non-dim)
      VSiO3             ! C-specific SiO3 uptake (non-dim)

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      PCphoto,        & ! C-specific rate of photosynth. (1/sec)
      thetaC,         & ! local Chl/C ratio (mg Chl/mmol C)
      QCaCO3,         & ! CaCO3/C ratio (mmol CaCO3/mmol C)
      VNO3,           & ! NO3 uptake rate (non-dim)
      VNH4,           & ! NH4 uptake rate (non-dim)
      VNtot,          & ! total N uptake rate (non-dim)
      NO3_V,          & ! nitrate uptake (mmol NO3/m^3/sec)
      NH4_V,          & ! ammonium uptake (mmol NH4/m^3/sec)
      PO4_V,          & ! PO4 uptake (mmol PO4/m^3/sec)
      DOP_V,          & ! DOP uptake (mmol DOP/m^3/sec)
      Qfe,            & ! init fe/C ratio (mmolFe/mmolC)
      gQfe,           & ! fe/C for growth
      Qsi,            & ! initial Si/C ratio (mmol Si/mmol C)
      gQsi,           & ! diatom Si/C ratio for growth (new biomass)
      Pprime,         & ! used to limit autotroph mort at low biomass (mmol C/m^3)
      photoC,         & ! C-fixation (mmol C/m^3/sec)
      photoSi,        & ! silicon uptake (mmol Si/m^3/sec)
      CaCO3_PROD,     & ! prod. of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
      photoacc,       & ! Chl synth. term in photoadapt. (GD98) (mg Chl/m^3/sec)
      Nfix,           & ! total Nitrogen fixation (mmol N/m^3/sec)
      Nexcrete          ! fixed N excretion

#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_graze,     & ! autotroph grazing rate (mmol C/m^3/sec)
      auto_graze_zoo, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_poc, & ! auto_graze routed to poc (mmol C/m^3/sec)
      auto_graze_doc, & ! auto_graze routed to doc (mmol C/m^3/sec)
      auto_graze_dic, & ! auto_graze routed to dic (mmol C/m^3/sec)
      auto_loss,      & ! autotroph non-grazing mort (mmol C/m^3/sec)
      auto_loss_poc,  & ! auto_loss routed to poc (mmol C/m^3/sec)
      auto_loss_doc,  & ! auto_loss routed to doc (mmol C/m^3/sec)
      auto_loss_dic,  & ! auto_loss routed to dic (mmol C/m^3/sec)
      auto_agg          ! autotroph aggregation (mmol C/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_graze_dic, &     ! auto_graze routed to dic (mmol C/m^3/sec)
      auto_graze_dic_Fe0, & ! auto_graze routed to dic (mmol C/m^3/sec)
      auto_graze_dic_Fe1, & ! auto_graze routed to dic (mmol C/m^3/sec)
      auto_graze_dic_Fe2, & ! auto_graze routed to dic (mmol C/m^3/sec)
      auto_graze_dic_Fe3, & ! auto_graze routed to dic (mmol C/m^3/sec)
      auto_graze_dic_Fe4, & ! auto_graze routed to dic (mmol C/m^3/sec)
      auto_graze_dic_Fe5, & ! auto_graze routed to dic (mmol C/m^3/sec)
      auto_graze_dic_Fe6, & ! auto_graze routed to dic (mmol C/m^3/sec)
      auto_graze_dic_Fe7    ! auto_graze routed to dic (mmol C/m^3/sec)

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_graze_poc, &     ! auto_graze routed to poc (mmol C/m^3/sec)
      auto_graze_poc_Fe0, & ! auto_graze routed to poc (mmol C/m^3/sec)
      auto_graze_poc_Fe1, & ! auto_graze routed to poc (mmol C/m^3/sec)
      auto_graze_poc_Fe2, & ! auto_graze routed to poc (mmol C/m^3/sec)
      auto_graze_poc_Fe3, & ! auto_graze routed to poc (mmol C/m^3/sec)
      auto_graze_poc_Fe4, & ! auto_graze routed to poc (mmol C/m^3/sec)
      auto_graze_poc_Fe5, & ! auto_graze routed to poc (mmol C/m^3/sec)
      auto_graze_poc_Fe6, & ! auto_graze routed to poc (mmol C/m^3/sec)
      auto_graze_poc_Fe7    ! auto_graze routed to poc (mmol C/m^3/sec)

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_loss_dic,     & ! auto_loss routed to dic (mmol C/m^3/sec)
      auto_loss_dic_Fe0, & ! auto_loss routed to dic (mmol C/m^3/sec)
      auto_loss_dic_Fe1, & ! auto_loss routed to dic (mmol C/m^3/sec)
      auto_loss_dic_Fe2, & ! auto_loss routed to dic (mmol C/m^3/sec)
      auto_loss_dic_Fe3, & ! auto_loss routed to dic (mmol C/m^3/sec)
      auto_loss_dic_Fe4, & ! auto_loss routed to dic (mmol C/m^3/sec)
      auto_loss_dic_Fe5, & ! auto_loss routed to dic (mmol C/m^3/sec)
      auto_loss_dic_Fe6, & ! auto_loss routed to dic (mmol C/m^3/sec)
      auto_loss_dic_Fe7    ! auto_loss routed to dic (mmol C/m^3/sec)

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_graze_doc,     & ! auto_graze routed to doc (mmol C/m^3/sec)
      auto_graze_doc_Fe0, & ! auto_graze routed to doc (mmol C/m^3/sec)
      auto_graze_doc_Fe1, & ! auto_graze routed to doc (mmol C/m^3/sec)
      auto_graze_doc_Fe2, & ! auto_graze routed to doc (mmol C/m^3/sec)
      auto_graze_doc_Fe3, & ! auto_graze routed to doc (mmol C/m^3/sec)
      auto_graze_doc_Fe4, & ! auto_graze routed to doc (mmol C/m^3/sec)
      auto_graze_doc_Fe5, & ! auto_graze routed to doc (mmol C/m^3/sec)
      auto_graze_doc_Fe6, & ! auto_graze routed to doc (mmol C/m^3/sec)
      auto_graze_doc_Fe7    ! auto_graze routed to doc (mmol C/m^3/sec)

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_loss_poc,     & ! auto_loss routed to poc (mmol C/m^3/sec)
      auto_loss_poc_Fe0, & ! auto_loss routed to poc (mmol C/m^3/sec)
      auto_loss_poc_Fe1, & ! auto_loss routed to poc (mmol C/m^3/sec)
      auto_loss_poc_Fe2, & ! auto_loss routed to poc (mmol C/m^3/sec)
      auto_loss_poc_Fe3, & ! auto_loss routed to poc (mmol C/m^3/sec)
      auto_loss_poc_Fe4, & ! auto_loss routed to poc (mmol C/m^3/sec)
      auto_loss_poc_Fe5, & ! auto_loss routed to poc (mmol C/m^3/sec)
      auto_loss_poc_Fe6, & ! auto_loss routed to poc (mmol C/m^3/sec)
      auto_loss_poc_Fe7    ! auto_loss routed to poc (mmol C/m^3/sec)

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_loss_doc,     & ! auto_loss routed to doc (mmol C/m^3/sec)
      auto_loss_doc_Fe0, & ! auto_loss routed to doc (mmol C/m^3/sec)
      auto_loss_doc_Fe1, & ! auto_loss routed to doc (mmol C/m^3/sec)
      auto_loss_doc_Fe2, & ! auto_loss routed to doc (mmol C/m^3/sec)
      auto_loss_doc_Fe3, & ! auto_loss routed to doc (mmol C/m^3/sec)
      auto_loss_doc_Fe4, & ! auto_loss routed to doc (mmol C/m^3/sec)
      auto_loss_doc_Fe5, & ! auto_loss routed to doc (mmol C/m^3/sec)
      auto_loss_doc_Fe6, & ! auto_loss routed to doc (mmol C/m^3/sec)
      auto_loss_doc_Fe7    ! auto_loss routed to doc (mmol C/m^3/sec)

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_agg,       & ! autotroph aggregation (mmol C/m^3/sec)
      auto_agg_Fe,    & ! autotroph aggregation (mmol C/m^3/sec)
      auto_agg_Fe0,   & ! autotroph aggregation (mmol C/m^3/sec)
      auto_agg_Fe1,   & ! autotroph aggregation (mmol C/m^3/sec)
      auto_agg_Fe2,   & ! autotroph aggregation (mmol C/m^3/sec)
      auto_agg_Fe3,   & ! autotroph aggregation (mmol C/m^3/sec)
      auto_agg_Fe4,   & ! autotroph aggregation (mmol C/m^3/sec)
      auto_agg_Fe5,   & ! autotroph aggregation (mmol C/m^3/sec)
      auto_agg_Fe6,   & ! autotroph aggregation (mmol C/m^3/sec)
      auto_agg_Fe7      ! autotroph aggregation (mmol C/m^3/sec)

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_loss,      & ! autotroph non-grazing mort (mmol C/m^3/sec)
      auto_loss_Fe,   & ! autotroph non-grazing mort (mmol C/m^3/sec)
      auto_loss_Fe0,  & ! autotroph non-grazing mort (mmol C/m^3/sec)
      auto_loss_Fe1,  & ! autotroph non-grazing mort (mmol C/m^3/sec)
      auto_loss_Fe2,  & ! autotroph non-grazing mort (mmol C/m^3/sec)
      auto_loss_Fe3,  & ! autotroph non-grazing mort (mmol C/m^3/sec)
      auto_loss_Fe4,  & ! autotroph non-grazing mort (mmol C/m^3/sec)
      auto_loss_Fe5,  & ! autotroph non-grazing mort (mmol C/m^3/sec)
      auto_loss_Fe6,  & ! autotroph non-grazing mort (mmol C/m^3/sec)
      auto_loss_Fe7     ! autotroph non-grazing mort (mmol C/m^3/sec)

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_graze,     & ! autotroph grazing rate (mmol C/m^3/sec)
      auto_graze_Fe , & !
      auto_graze_Fe0, & !
      auto_graze_Fe1, & !
      auto_graze_Fe2, & !
      auto_graze_Fe3, & !
      auto_graze_Fe4, & !
      auto_graze_Fe5, & !
      auto_graze_Fe6, & !
      auto_graze_Fe7    !

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_graze_zoo,     & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_Fe0, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_Fe1, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_Fe2, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_Fe3, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_Fe4, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_Fe5, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_Fe6, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_Fe7    ! auto_graze routed to zoo (mmol C/m^3/sec)

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      auto_graze_zoo_tot_Fe0, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_tot_Fe1, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_tot_Fe2, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_tot_Fe3, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_tot_Fe4, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_tot_Fe5, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_tot_Fe6, & ! auto_graze routed to zoo (mmol C/m^3/sec)
      auto_graze_zoo_tot_Fe7    ! auto_graze routed to zoo (mmol C/m^3/sec)

#endif

#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      photoFe           ! iron uptake
#else
   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      photoFe,        & ! iron uptake
      photoFe0,       & ! iron uptake
      photoFe1,       & ! iron uptake
      photoFe2,       & ! iron uptake
      photoFe3,       & ! iron uptake
      photoFe4,       & ! iron uptake
      photoFe5,       & ! iron uptake
      photoFe6,       & ! iron uptake
      photoFe7          ! iron uptake
#endif


   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        Zprime,         & ! used to limit zoo mort at low biomass (mmol C/m^3)
        f_zoo_detr        ! frac of zoo losses into large detrital pool (non-dim)

#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_loss_poc      ! zoo_loss routed to poc (mmol C/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_loss_poc,     & ! zoo_loss routed to poc (mmol C/m^3/sec)
        zoo_loss_poc_Fe0, & ! zoo_loss routed to poc (mmol C/m^3/sec)
        zoo_loss_poc_Fe1, & ! zoo_loss routed to poc (mmol C/m^3/sec)
        zoo_loss_poc_Fe2, & ! zoo_loss routed to poc (mmol C/m^3/sec)
        zoo_loss_poc_Fe3, & ! zoo_loss routed to poc (mmol C/m^3/sec)
        zoo_loss_poc_Fe4, & ! zoo_loss routed to poc (mmol C/m^3/sec)
        zoo_loss_poc_Fe5, & ! zoo_loss routed to poc (mmol C/m^3/sec)
        zoo_loss_poc_Fe6, & ! zoo_loss routed to poc (mmol C/m^3/sec)
        zoo_loss_poc_Fe7    ! zoo_loss routed to poc (mmol C/m^3/sec)
#endif

#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_graze_dic     ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_graze_dic,     & ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
        zoo_graze_dic_Fe0, & ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
        zoo_graze_dic_Fe1, & ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
        zoo_graze_dic_Fe2, & ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
        zoo_graze_dic_Fe3, & ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
        zoo_graze_dic_Fe4, & ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
        zoo_graze_dic_Fe5, & ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
        zoo_graze_dic_Fe6, & ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
        zoo_graze_dic_Fe7    ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
#endif
#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_graze_poc     ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_graze_poc,     & ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
        zoo_graze_poc_Fe0, & ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
        zoo_graze_poc_Fe1, & ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
        zoo_graze_poc_Fe2, & ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
        zoo_graze_poc_Fe3, & ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
        zoo_graze_poc_Fe4, & ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
        zoo_graze_poc_Fe5, & ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
        zoo_graze_poc_Fe6, & ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
        zoo_graze_poc_Fe7    ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
#endif
#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_graze_zoo     ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_graze_zoo    ,  & ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
        zoo_graze_zoo_Fe0,  & ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
        zoo_graze_zoo_Fe1,  & ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
        zoo_graze_zoo_Fe2,  & ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
        zoo_graze_zoo_Fe3,  & ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
        zoo_graze_zoo_Fe4,  & ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
        zoo_graze_zoo_Fe5,  & ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
        zoo_graze_zoo_Fe6,  & ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
        zoo_graze_zoo_Fe7     ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
#endif

#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_loss_dic      ! zoo_loss routed to dic (mmol C/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_loss_dic,     &  ! zoo_loss routed to dic (mmol C/m^3/sec)
        zoo_loss_dic_Fe0, &  ! zoo_loss routed to dic (mmol C/m^3/sec)
        zoo_loss_dic_Fe1, &  ! zoo_loss routed to dic (mmol C/m^3/sec)
        zoo_loss_dic_Fe2, &  ! zoo_loss routed to dic (mmol C/m^3/sec)
        zoo_loss_dic_Fe3, &  ! zoo_loss routed to dic (mmol C/m^3/sec)
        zoo_loss_dic_Fe4, &  ! zoo_loss routed to dic (mmol C/m^3/sec)
        zoo_loss_dic_Fe5, &  ! zoo_loss routed to dic (mmol C/m^3/sec)
        zoo_loss_dic_Fe6, &  ! zoo_loss routed to dic (mmol C/m^3/sec)
        zoo_loss_dic_Fe7     ! zoo_loss routed to dic (mmol C/m^3/sec)
#endif

#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_graze_doc     ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_graze_doc,     & ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
        zoo_graze_doc_Fe0, & ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
        zoo_graze_doc_Fe1, & ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
        zoo_graze_doc_Fe2, & ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
        zoo_graze_doc_Fe3, & ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
        zoo_graze_doc_Fe4, & ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
        zoo_graze_doc_Fe5, & ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
        zoo_graze_doc_Fe6, & ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
        zoo_graze_doc_Fe7    ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
#endif

#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_loss_doc      ! zoo_loss routed to doc (mmol C/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_loss_doc,     & ! zoo_loss routed to doc (mmol C/m^3/sec)
        zoo_loss_doc_Fe0, & ! zoo_loss routed to doc (mmol C/m^3/sec)
        zoo_loss_doc_Fe1, & ! zoo_loss routed to doc (mmol C/m^3/sec)
        zoo_loss_doc_Fe2, & ! zoo_loss routed to doc (mmol C/m^3/sec)
        zoo_loss_doc_Fe3, & ! zoo_loss routed to doc (mmol C/m^3/sec)
        zoo_loss_doc_Fe4, & ! zoo_loss routed to doc (mmol C/m^3/sec)
        zoo_loss_doc_Fe5, & ! zoo_loss routed to doc (mmol C/m^3/sec)
        zoo_loss_doc_Fe6, & ! zoo_loss routed to doc (mmol C/m^3/sec)
        zoo_loss_doc_Fe7    ! zoo_loss routed to doc (mmol C/m^3/sec)
#endif

#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_loss          ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_loss,       & ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
        zoo_loss_Fe,    & ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
        zoo_loss_Fe0,   & ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
        zoo_loss_Fe1,   & ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
        zoo_loss_Fe2,   & ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
        zoo_loss_Fe3,   & ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
        zoo_loss_Fe4,   & ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
        zoo_loss_Fe5,   & ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
        zoo_loss_Fe6,   & ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
        zoo_loss_Fe7      ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
#endif

#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_graze         ! zooplankton losses due to grazing (mmol C/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        zoo_graze,      & ! zooplankton losses due to grazing (mmol C/m^3/sec)
        zoo_graze_Fe,   & ! zooplankton losses due to grazing (mmol C/m^3/sec)
        zoo_graze_Fe0,  & ! zooplankton losses due to grazing (mmol C/m^3/sec)
        zoo_graze_Fe1,  & ! zooplankton losses due to grazing (mmol C/m^3/sec)
        zoo_graze_Fe2,  & ! zooplankton losses due to grazing (mmol C/m^3/sec)
        zoo_graze_Fe3,  & ! zooplankton losses due to grazing (mmol C/m^3/sec)
        zoo_graze_Fe4,  & ! zooplankton losses due to grazing (mmol C/m^3/sec)
        zoo_graze_Fe5,  & ! zooplankton losses due to grazing (mmol C/m^3/sec)
        zoo_graze_Fe6,  & ! zooplankton losses due to grazing (mmol C/m^3/sec)
        zoo_graze_Fe7     ! zooplankton losses due to grazing (mmol C/m^3/sec)
#endif

#ifndef FE_TAG
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        x_graze_zoo       ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
#else
   real (r8), dimension(nx_block,ny_block,zooplankton_cnt) :: &
        x_graze_zoo,     & ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
        x_graze_zoo_Fe,  & ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
        x_graze_zoo_Fe0, & ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
        x_graze_zoo_Fe1, & ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
        x_graze_zoo_Fe2, & ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
        x_graze_zoo_Fe3, & ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
        x_graze_zoo_Fe4, & ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
        x_graze_zoo_Fe5, & ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
        x_graze_zoo_Fe6, & ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
        x_graze_zoo_Fe7    ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
#endif

   real (r8), dimension(nx_block,ny_block) :: &
      remaining_P       ! used in routing P from autotrophs w/ Qp different from Qp_zoo_pom

   real (r8), dimension(nx_block,ny_block,autotroph_cnt) :: &
      remaining_P_dop,& ! remaining_P from mort routed to DOP pool
      remaining_P_dip   ! remaining_P from mort routed to remin

   real (r8), dimension(nx_block,ny_block) :: &
      DON_prod,       & ! production of dissolved organic N
      DOP_prod,       & ! production of dissolved organic P
      cks,            & ! constant used in Fe quota modification
      cksi,           & ! constant used in Si quota modification
      O2_PRODUCTION,  & ! O2 production
      O2_CONSUMPTION, & ! O2 consumption
      DONr_remin,     & ! portion of refractory DON remineralized
      DOPr_remin        ! portion of refractory DOP remineralized

   real (r8), dimension(nx_block,ny_block) :: &
      DOFe_prod        ! produciton of dissolved organic Fe

#ifdef FE_TAG
   real (r8), dimension(nx_block,ny_block) :: &
      DOFe0_prod,     & ! produciton of dissolved organic Fe
      DOFe1_prod,     & ! produciton of dissolved organic Fe
      DOFe2_prod,     & ! produciton of dissolved organic Fe
      DOFe3_prod,     & ! produciton of dissolved organic Fe
      DOFe4_prod,     & ! produciton of dissolved organic Fe
      DOFe5_prod,     & ! produciton of dissolved organic Fe
      DOFe6_prod,     & ! produciton of dissolved organic Fe
      DOFe7_prod        ! produciton of dissolved organic Fe
#endif

   real (r8), dimension(nx_block,ny_block) :: &
      CO3,            &! carbonate ion
      HCO3,           &! bicarbonate ion
      H2CO3,          &! carbonic acid
      CO3_ALT_CO2,    &! carbonate ion, alternative CO2
      HCO3_ALT_CO2,   &! bicarbonate ion, alternative CO2
      H2CO3_ALT_CO2,  &! carbonic acid, alternative CO2
      OMEGA_CALC,     &! solubility ratio for aragonite
      OMEGA_ARAG       ! solubility ratio for calcite

   integer (int_kind) :: &
      bid,            & ! local_block id
      n,              & ! tracer index
      auto_ind,       & ! autotroph functional group index
      auto_ind2,      & ! autotroph functional group index
      zoo_ind,        & ! zooplankton functional group index
      zoo_ind2,       & ! zooplankton functional group index
      prey_ind,       & ! grazee group index
      pred_ind,       & ! grazer group index
      kk,             & ! index for looping over k levels
      j                 ! index for looping over ny_block dimension

   logical (log_kind) :: &
      lalt_co2_terms    ! are any alt_co2 terms being time averaged

#ifdef BEC
   logical, save :: is_init = .true.
#endif

#ifndef FE_TAG
   associate( &
        DIC_loc  => tracer_local(:, :, dic_ind),                &
        DIC_ALT_CO2_loc => tracer_local(:, :, dic_alt_co2_ind), &
        ALK_loc  => tracer_local(:, :, alk_ind),                &
        PO4_loc  => tracer_local(:, :, po4_ind),                &
        NO3_loc  => tracer_local(:, :, no3_ind),                &
        SiO3_loc => tracer_local(:, :, sio3_ind),               &
        NH4_loc  => tracer_local(:, :, nh4_ind),                &
        O2_loc   => tracer_local(:, :, o2_ind),                 &
        DOC_loc  => tracer_local(:, :, doc_ind),                &
        DON_loc  => tracer_local(:, :, don_ind),                &
        DOP_loc  => tracer_local(:, :, dop_ind),                &
        DOPr_loc => tracer_local(:, :, dopr_ind),               &
        DONr_loc => tracer_local(:, :, donr_ind),               &
        Fe_loc   => tracer_local(:, :, fe_ind),                 &
        DOFe_loc => tracer_local(:, :, dofe_ind)                &
     )
#else
   associate( &
        DIC_loc  => tracer_local(:, :, dic_ind),                &
        DIC_ALT_CO2_loc => tracer_local(:, :, dic_alt_co2_ind), &
        ALK_loc  => tracer_local(:, :, alk_ind),                &
        PO4_loc  => tracer_local(:, :, po4_ind),                &
        NO3_loc  => tracer_local(:, :, no3_ind),                &
        SiO3_loc => tracer_local(:, :, sio3_ind),               &
        NH4_loc  => tracer_local(:, :, nh4_ind),                &
        O2_loc   => tracer_local(:, :, o2_ind),                 &
        DOC_loc  => tracer_local(:, :, doc_ind),                &
        DON_loc  => tracer_local(:, :, don_ind),                &
        DOP_loc  => tracer_local(:, :, dop_ind),                &
        DOPr_loc => tracer_local(:, :, dopr_ind),               &
        DONr_loc => tracer_local(:, :, donr_ind),               &
        Fe0_loc   => tracer_local(:, :, fe0_ind),               &
        DOFe0_loc => tracer_local(:, :, dofe0_ind),             &
        Fe1_loc   => tracer_local(:, :, fe1_ind),               &
        DOFe1_loc => tracer_local(:, :, dofe1_ind),             &
        Fe2_loc   => tracer_local(:, :, fe2_ind),               &
        DOFe2_loc => tracer_local(:, :, dofe2_ind),             &
        Fe3_loc   => tracer_local(:, :, fe3_ind),               &
        DOFe3_loc => tracer_local(:, :, dofe3_ind),             &
        Fe4_loc   => tracer_local(:, :, fe4_ind),               &
        DOFe4_loc => tracer_local(:, :, dofe4_ind),             &
        Fe5_loc   => tracer_local(:, :, fe5_ind),               &
        DOFe5_loc => tracer_local(:, :, dofe5_ind),             &
        Fe6_loc   => tracer_local(:, :, fe6_ind),               &
        DOFe6_loc => tracer_local(:, :, dofe6_ind),             &
        Fe7_loc   => tracer_local(:, :, fe7_ind),               &
        DOFe7_loc => tracer_local(:, :, dofe7_ind)              &
     )
#endif
!-----------------------------------------------------------------------
#ifdef BEC
   bid = 1
#else
   bid = this_block%local_id
#endif

#ifndef BEC
   call timer_start(ecosys_interior_timer, block_id=bid)
#endif

   DTRACER_MODULE = c0

#ifdef BEC
   if (is_init) then
          allocate( QA_dust_def(nx_block,ny_block,max_blocks_clinic) )
          allocate( SED_DENITRIF(nx_block,ny_block,max_blocks_clinic) )
          allocate( OTHER_REMIN(nx_block,ny_block,max_blocks_clinic) )
          allocate( ZSATCALC(nx_block,ny_block,max_blocks_clinic) )
          allocate( ZSATARAG(nx_block,ny_block,max_blocks_clinic) )
          allocate( CO3_CALC_ANOM_km1(nx_block,ny_block,max_blocks_clinic) )
          allocate( CO3_ARAG_ANOM_km1(nx_block,ny_block,max_blocks_clinic) )

          QA_dust_def       = c0
          SED_DENITRIF      = c0
          OTHER_REMIN       = c0
          ZSATCALC          = c0
          ZSATARAG          = c0
          CO3_CALC_ANOM_km1 = c0
          CO3_ARAG_ANOM_km1 = c0

          allocate( POC%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( POC%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( POC%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( POC%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( POC%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( POC%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( POC%remin(nx_block,ny_block,max_blocks_clinic) )

          POC%sflux_in      = c0
          POC%hflux_in      = c0
          POC%prod          = c0
          POC%sflux_out     = c0
          POC%hflux_out     = c0
          POC%sed_loss      = c0
          POC%remin         = c0

          allocate( P_CaCO3%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_CaCO3%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_CaCO3%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_CaCO3%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_CaCO3%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_CaCO3%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_CaCO3%remin(nx_block,ny_block,max_blocks_clinic) )

          P_CaCO3%sflux_in  = c0
          P_CaCO3%hflux_in  = c0
          P_CaCO3%prod      = c0
          P_CaCO3%sflux_out = c0
          P_CaCO3%hflux_out = c0
          P_CaCO3%sed_loss  = c0
          P_CaCO3%remin     = c0

          allocate( P_SiO2%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_SiO2%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_SiO2%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_SiO2%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_SiO2%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_SiO2%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_SiO2%remin(nx_block,ny_block,max_blocks_clinic) )

          P_SiO2%sflux_in  = c0
          P_SiO2%hflux_in  = c0
          P_SiO2%prod      = c0
          P_SiO2%sflux_out = c0
          P_SiO2%hflux_out = c0
          P_SiO2%sed_loss  = c0
          P_SiO2%remin     = c0

          allocate( dust%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( dust%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( dust%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( dust%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( dust%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( dust%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( dust%remin(nx_block,ny_block,max_blocks_clinic) )

          dust%sflux_in    = c0
          dust%hflux_in    = c0
          dust%prod        = c0
          dust%sflux_out   = c0
          dust%hflux_out   = c0
          dust%sed_loss    = c0
          dust%remin       = c0

#ifndef FE_TAG
          allocate( P_iron%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%remin(nx_block,ny_block,max_blocks_clinic) )

          P_iron%sflux_in   = c0
          P_iron%hflux_in   = c0
          P_iron%prod       = c0
          P_iron%sflux_out  = c0
          P_iron%hflux_out  = c0
          P_iron%sed_loss   = c0
          P_iron%remin      = c0
#else
          allocate( P_iron%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron%remin(nx_block,ny_block,max_blocks_clinic) )

          P_iron%sflux_in   = c0
          P_iron%hflux_in   = c0
          P_iron%prod       = c0
          P_iron%sflux_out  = c0
          P_iron%hflux_out  = c0
          P_iron%sed_loss   = c0
          P_iron%remin      = c0

          allocate( P_iron0%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron0%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron0%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron0%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron0%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron0%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron0%remin(nx_block,ny_block,max_blocks_clinic) )

          P_iron0%sflux_in   = c0
          P_iron0%hflux_in   = c0
          P_iron0%prod       = c0
          P_iron0%sflux_out  = c0
          P_iron0%hflux_out  = c0
          P_iron0%sed_loss   = c0
          P_iron0%remin      = c0

          allocate( P_iron1%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron1%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron1%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron1%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron1%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron1%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron1%remin(nx_block,ny_block,max_blocks_clinic) )

          P_iron1%sflux_in   = c0
          P_iron1%hflux_in   = c0
          P_iron1%prod       = c0
          P_iron1%sflux_out  = c0
          P_iron1%hflux_out  = c0
          P_iron1%sed_loss   = c0
          P_iron1%remin      = c0

          allocate( P_iron2%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron2%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron2%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron2%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron2%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron2%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron2%remin(nx_block,ny_block,max_blocks_clinic) )

          P_iron2%sflux_in   = c0
          P_iron2%hflux_in   = c0
          P_iron2%prod       = c0
          P_iron2%sflux_out  = c0
          P_iron2%hflux_out  = c0
          P_iron2%sed_loss   = c0
          P_iron2%remin      = c0

          allocate( P_iron3%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron3%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron3%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron3%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron3%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron3%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron3%remin(nx_block,ny_block,max_blocks_clinic) )

          P_iron3%sflux_in   = c0
          P_iron3%hflux_in   = c0
          P_iron3%prod       = c0
          P_iron3%sflux_out  = c0
          P_iron3%hflux_out  = c0
          P_iron3%sed_loss   = c0
          P_iron3%remin      = c0

          allocate( P_iron4%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron4%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron4%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron4%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron4%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron4%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron4%remin(nx_block,ny_block,max_blocks_clinic) )

          P_iron4%sflux_in   = c0
          P_iron4%hflux_in   = c0
          P_iron4%prod       = c0
          P_iron4%sflux_out  = c0
          P_iron4%hflux_out  = c0
          P_iron4%sed_loss   = c0
          P_iron4%remin      = c0

          allocate( P_iron5%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron5%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron5%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron5%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron5%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron5%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron5%remin(nx_block,ny_block,max_blocks_clinic) )

          P_iron5%sflux_in   = c0
          P_iron5%hflux_in   = c0
          P_iron5%prod       = c0
          P_iron5%sflux_out  = c0
          P_iron5%hflux_out  = c0
          P_iron5%sed_loss   = c0
          P_iron5%remin      = c0

          allocate( P_iron6%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron6%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron6%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron6%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron6%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron6%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron6%remin(nx_block,ny_block,max_blocks_clinic) )

          P_iron6%sflux_in   = c0
          P_iron6%hflux_in   = c0
          P_iron6%prod       = c0
          P_iron6%sflux_out  = c0
          P_iron6%hflux_out  = c0
          P_iron6%sed_loss   = c0
          P_iron6%remin      = c0

          allocate( P_iron7%sflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron7%hflux_in(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron7%prod(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron7%sflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron7%hflux_out(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron7%sed_loss(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_iron7%remin(nx_block,ny_block,max_blocks_clinic) )

          P_iron7%sflux_in   = c0
          P_iron7%hflux_in   = c0
          P_iron7%prod       = c0
          P_iron7%sflux_out  = c0
          P_iron7%hflux_out  = c0
          P_iron7%sed_loss   = c0
          P_iron7%remin      = c0
#endif

          allocate( DIC_SURF_fields         (nx_block,ny_block,max_blocks_clinic) )
          allocate( CO2STAR_SURF_fields     (nx_block,ny_block,max_blocks_clinic) )
          allocate( DCO2STAR_SURF_fields    (nx_block,ny_block,max_blocks_clinic) )
          allocate( PV_SURF_fields          (nx_block,ny_block,max_blocks_clinic) )
          allocate( CO3_fields              (nx_block,ny_block,max_blocks_clinic) )
          allocate( CO3_SURF_fields         (nx_block,ny_block,max_blocks_clinic) )
          allocate( HCO3_fields             (nx_block,ny_block,max_blocks_clinic) )
          allocate( H2CO3_fields            (nx_block,ny_block,max_blocks_clinic) )
          allocate( DIC_loc_fields          (nx_block,ny_block,max_blocks_clinic) )
          allocate( DOC_loc_fields          (nx_block,ny_block,max_blocks_clinic) )
          allocate( O2_loc_fields           (nx_block,ny_block,max_blocks_clinic) )
          allocate( NO3_loc_fields          (nx_block,ny_block,max_blocks_clinic) )
          allocate( decay_CaCO3_fields      (nx_block,ny_block,max_blocks_clinic) )
          allocate( DECAY_Hard_fields       (nx_block,ny_block,max_blocks_clinic) )
          allocate( decay_POC_E_fields      (nx_block,ny_block,max_blocks_clinic) )
          allocate( poc_diss_fields         (nx_block,ny_block,max_blocks_clinic) )
          allocate( caco3_diss_fields       (nx_block,ny_block,max_blocks_clinic) )
          allocate( POC_PROD_avail_fields   (nx_block,ny_block,max_blocks_clinic) )
          allocate( DOC_remin_fields        (nx_block,ny_block,max_blocks_clinic) )
          allocate( P_CaCO3_sflux_out_fields(nx_block,ny_block,max_blocks_clinic) )
          allocate( P_CaCO3_hflux_out_fields(nx_block,ny_block,max_blocks_clinic) )
          allocate( POC_sflux_out_fields    (nx_block,ny_block,max_blocks_clinic) )
          allocate( POC_hflux_out_fields    (nx_block,ny_block,max_blocks_clinic) )
          allocate( P_CaCO3_remin_fields    (nx_block,ny_block,max_blocks_clinic) )
          allocate( POC_remin_fields        (nx_block,ny_block,max_blocks_clinic) )
          allocate( dic_riv_flux_fields     (nx_block,ny_block,max_blocks_clinic) )
          allocate( doc_riv_flux_fields     (nx_block,ny_block,max_blocks_clinic) )

          DIC_SURF_fields          = c0
          CO2STAR_SURF_fields      = c0
          DCO2STAR_SURF_fields     = c0
          PV_SURF_fields           = c0
          CO3_fields               = c0
          CO3_SURF_fields          = c0
          HCO3_fields              = c0
          H2CO3_fields             = c0
          DIC_loc_fields           = c0
          DOC_loc_fields           = c0
          O2_loc_fields            = c0
          NO3_loc_fields           = c0
          decay_CaCO3_fields       = c0
          DECAY_Hard_fields        = c0
          decay_POC_E_fields       = c0
          poc_diss_fields          = c0
          caco3_diss_fields        = c0
          POC_PROD_avail_fields    = c0
          DOC_remin_fields         = c0
          P_CaCO3_sflux_out_fields = c0
          P_CaCO3_hflux_out_fields = c0
          POC_sflux_out_fields     = c0
          POC_hflux_out_fields     = c0
          P_CaCO3_remin_fields     = c0
          POC_remin_fields         = c0
          dic_riv_flux_fields      = c0
          doc_riv_flux_fields      = c0

          is_init = .false.
   end if
#endif

#ifdef DIAGNOSTICS_BIO
# ifndef FE_TAG
   aFe_brate =c0
   aFe_scav  =c0
   aFe_disag =c0
   aFe_pgen  =c0
   aFe_premin=c0
   aFe_hbio  =c0
# else
   aFe0_brate =c0
   aFe0_scav  =c0
   aFe0_disag =c0
   aFe0_pgen  =c0
   aFe0_premin=c0
   aFe0_hbio  =c0

   aFe1_brate =c0
   aFe1_scav  =c0
   aFe1_disag =c0
   aFe1_pgen  =c0
   aFe1_premin=c0
   aFe1_hbio  =c0

   aFe2_brate =c0
   aFe2_scav  =c0
   aFe2_disag =c0
   aFe2_pgen  =c0
   aFe2_premin=c0
   aFe2_hbio  =c0

   aFe3_brate =c0
   aFe3_scav  =c0
   aFe3_disag =c0
   aFe3_pgen  =c0
   aFe3_premin=c0
   aFe3_hbio  =c0

   aFe4_brate =c0
   aFe4_scav  =c0
   aFe4_disag =c0
   aFe4_pgen  =c0
   aFe4_premin=c0
   aFe4_hbio  =c0

   aFe5_brate =c0
   aFe5_scav  =c0
   aFe5_disag =c0
   aFe5_pgen  =c0
   aFe5_premin=c0
   aFe5_hbio  =c0

   aFe6_brate =c0
   aFe6_scav  =c0
   aFe6_disag =c0
   aFe6_pgen  =c0
   aFe6_premin=c0
   aFe6_hbio  =c0

   aFe7_brate =c0
   aFe7_scav  =c0
   aFe7_disag =c0
   aFe7_pgen  =c0
   aFe7_premin=c0
   aFe7_hbio  =c0
# endif
#endif

!-----------------------------------------------------------------------
!  exit immediately if computations are not to be performed
!-----------------------------------------------------------------------

#ifndef BEC
   if (.not. lsource_sink) then
      call timer_stop(ecosys_interior_timer, block_id=bid)
      return
   endif
#endif

!-----------------------------------------------------------------------
!  create local copies of model tracers
!  treat negative values as zero
!  apply mask to local copies
!-----------------------------------------------------------------------

   TEMP         = p5*(TEMP_OLD + TEMP_CUR)
   SALT         = p5*(SALT_OLD + SALT_CUR)*salt_to_ppt

   do n = 1, ecosys_tracer_cnt
      tracer_local(:,:,n) = max(c0, p5*(TRACER_MODULE_OLD(:,:,n) + &
                              TRACER_MODULE_CUR(:,:,n)))
   end do

#ifdef FE_TAG
   Fe_loc   = Fe0_loc+Fe1_loc+Fe2_loc+Fe3_loc+Fe4_loc+Fe5_loc+Fe6_loc+Fe7_loc
   DOFe_loc = DOFe0_loc+DOFe1_loc+DOFe2_loc+DOFe3_loc+DOFe4_loc+&
              DOFe5_loc+DOFe6_loc+DOFe7_loc
#endif

   do n = 1, ecosys_tracer_cnt
      where (.not. LAND_MASK(:, :, bid) .or. k > KMT(:, :, bid))
         tracer_local(:, :, n) = c0
      end where
   end do

   ! and now the living pools
   do zoo_ind = 1, zooplankton_cnt
      n = zooplankton(zoo_ind)%C_ind
      zooC_loc(:,:,zoo_ind) = max(c0, &
           p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      where (.not. LAND_MASK(:,:,bid) .or. k > KMT(:,:,bid))
         zooC_loc(:,:,zoo_ind) = c0
      end where
#ifdef FE_TAG
      n = zooplankton(zoo_ind)%Fe0_ind
      zooFe0_loc(:,:,zoo_ind) = max(c0, &
           p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      where (.not. LAND_MASK(:,:,bid) .or. k > KMT(:,:,bid))
         zooFe0_loc(:,:,zoo_ind) = c0
      end where

      n = zooplankton(zoo_ind)%Fe1_ind
      zooFe1_loc(:,:,zoo_ind) = max(c0, &
           p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      where (.not. LAND_MASK(:,:,bid) .or. k > KMT(:,:,bid))
         zooFe1_loc(:,:,zoo_ind) = c0
      end where

      n = zooplankton(zoo_ind)%Fe2_ind
      zooFe2_loc(:,:,zoo_ind) = max(c0, &
           p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      where (.not. LAND_MASK(:,:,bid) .or. k > KMT(:,:,bid))
         zooFe2_loc(:,:,zoo_ind) = c0
      end where

      n = zooplankton(zoo_ind)%Fe3_ind
      zooFe3_loc(:,:,zoo_ind) = max(c0, &
           p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      where (.not. LAND_MASK(:,:,bid) .or. k > KMT(:,:,bid))
         zooFe3_loc(:,:,zoo_ind) = c0
      end where

      n = zooplankton(zoo_ind)%Fe4_ind
      zooFe4_loc(:,:,zoo_ind) = max(c0, &
           p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      where (.not. LAND_MASK(:,:,bid) .or. k > KMT(:,:,bid))
         zooFe4_loc(:,:,zoo_ind) = c0
      end where

      n = zooplankton(zoo_ind)%Fe5_ind
      zooFe5_loc(:,:,zoo_ind) = max(c0, &
           p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      where (.not. LAND_MASK(:,:,bid) .or. k > KMT(:,:,bid))
         zooFe5_loc(:,:,zoo_ind) = c0
      end where

      n = zooplankton(zoo_ind)%Fe6_ind
      zooFe6_loc(:,:,zoo_ind) = max(c0, &
           p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      where (.not. LAND_MASK(:,:,bid) .or. k > KMT(:,:,bid))
         zooFe6_loc(:,:,zoo_ind) = c0
      end where

      n = zooplankton(zoo_ind)%Fe7_ind
      zooFe7_loc(:,:,zoo_ind) = max(c0, &
           p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      where (.not. LAND_MASK(:,:,bid) .or. k > KMT(:,:,bid))
         zooFe7_loc(:,:,zoo_ind) = c0
      end where
#endif
   end do
#ifdef FE_TAG
      zooFe_loc=zooFe0_loc+zooFe1_loc+zooFe2_loc+zooFe3_loc+&
                zooFe4_loc+zooFe5_loc+zooFe6_loc+zooFe7_loc
#endif

   do auto_ind = 1, autotroph_cnt

      n = autotrophs(auto_ind)%Chl_ind
      autotrophChl_loc(:,:,auto_ind) = max(c0, &
         p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))

      n = autotrophs(auto_ind)%C_ind
      autotrophC_loc(:,:,auto_ind) = max(c0, &
         p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))

      n = autotrophs(auto_ind)%Si_ind
      if (n > 0) then
         autotrophSi_loc(:,:,auto_ind) = max(c0, &
            p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      endif

      n = autotrophs(auto_ind)%CaCO3_ind
      if (n > 0) then
         autotrophCaCO3_loc(:,:,auto_ind) = max(c0, &
            p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      endif

#ifndef FE_TAG
      n = autotrophs(auto_ind)%Fe_ind
      autotrophFe_loc(:,:,auto_ind) = max(c0, &
         p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
#else
      n = autotrophs(auto_ind)%Fe0_ind
      autotrophFe0_loc(:,:,auto_ind) = max(c0, &
         p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      n = autotrophs(auto_ind)%Fe1_ind
      autotrophFe1_loc(:,:,auto_ind) = max(c0, &
         p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      n = autotrophs(auto_ind)%Fe2_ind
      autotrophFe2_loc(:,:,auto_ind) = max(c0, &
         p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      n = autotrophs(auto_ind)%Fe3_ind
      autotrophFe3_loc(:,:,auto_ind) = max(c0, &
         p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      n = autotrophs(auto_ind)%Fe4_ind
      autotrophFe4_loc(:,:,auto_ind) = max(c0, &
         p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      n = autotrophs(auto_ind)%Fe5_ind
      autotrophFe5_loc(:,:,auto_ind) = max(c0, &
         p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      n = autotrophs(auto_ind)%Fe6_ind
      autotrophFe6_loc(:,:,auto_ind) = max(c0, &
         p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
      n = autotrophs(auto_ind)%Fe7_ind
      autotrophFe7_loc(:,:,auto_ind) = max(c0, &
         p5*(TRACER_MODULE_OLD(:,:,n) + TRACER_MODULE_CUR(:,:,n)))
#endif

#ifdef FE_TAG
      autotrophFe_loc=autotrophFe0_loc+autotrophFe1_loc+&
                      autotrophFe2_loc+autotrophFe3_loc+&
                      autotrophFe4_loc+autotrophFe5_loc+&
                      autotrophFe6_loc+autotrophFe7_loc
#endif

      where (.not. LAND_MASK(:,:,bid) .or. k > KMT(:,:,bid))
         autotrophChl_loc(:,:,auto_ind) = c0
         autotrophC_loc(:,:,auto_ind) = c0
         autotrophSi_loc(:,:,auto_ind) = c0
         autotrophCaCO3_loc(:,:,auto_ind) = c0

#ifndef FE_TAG
         autotrophFe_loc(:,:,auto_ind) = c0
#else
         autotrophFe_loc(:,:,auto_ind) = c0
         autotrophFe0_loc(:,:,auto_ind) = c0
         autotrophFe1_loc(:,:,auto_ind) = c0
         autotrophFe2_loc(:,:,auto_ind) = c0
         autotrophFe3_loc(:,:,auto_ind) = c0
         autotrophFe4_loc(:,:,auto_ind) = c0
         autotrophFe5_loc(:,:,auto_ind) = c0
         autotrophFe6_loc(:,:,auto_ind) = c0
         autotrophFe7_loc(:,:,auto_ind) = c0
#endif
      end where
   end do

!-----------------------------------------------------------------------
!  set tracer restore fields
!-----------------------------------------------------------------------

#ifdef BEC
   do n = 1, ecosys_tracer_cnt
      restore_local(:, :, n) = c0
   end do
#else
   do n = 1, ecosys_tracer_cnt
      call ecosys_restore%restore_variable(tracer_index=n, &
           vert_level=k, block_id=bid, local_data=tracer_local(:, :, n), &
           restore_data=restore_local(:, :, n))

      call ecosys_restore%accumulate_tavg(tracer_index=n, &
           vert_level=k, block_id=bid, &
           restore_local=restore_local(:, :, n))
   end do
#endif
!-----------------------------------------------------------------------
!  If any phyto box are zero, set others to zeros.
!-----------------------------------------------------------------------

   do auto_ind = 1, autotroph_cnt
      ZERO_MASK = autotrophChl_loc(:,:,auto_ind) == c0 .or. &
                  autotrophC_loc(:,:,auto_ind) == c0 .or. &
                  autotrophFe_loc(:,:,auto_ind) == c0
      if (autotrophs(auto_ind)%Si_ind > 0) &
         ZERO_MASK = ZERO_MASK .or. autotrophSi_loc(:,:,auto_ind) == c0
      where (ZERO_MASK)
         autotrophChl_loc(:,:,auto_ind) = c0
         autotrophC_loc(:,:,auto_ind) = c0
         autotrophFe_loc(:,:,auto_ind) = c0
      end where
      if (autotrophs(auto_ind)%Si_ind > 0) &
         where (ZERO_MASK) autotrophSi_loc(:,:,auto_ind) = c0
      if (autotrophs(auto_ind)%CaCO3_ind > 0) &
         where (ZERO_MASK) autotrophCaCO3_loc(:,:,auto_ind) = c0
   end do

!-----------------------------------------------------------------------
!  set local variables, with incoming ratios
!-----------------------------------------------------------------------

   do auto_ind = 1, autotroph_cnt
      thetaC(:,:,auto_ind) = autotrophChl_loc(:,:,auto_ind) / (autotrophC_loc(:,:,auto_ind) + epsC)
      Qfe(:,:,auto_ind) = autotrophFe_loc(:,:,auto_ind) / (autotrophC_loc(:,:,auto_ind) + epsC)
      if (autotrophs(auto_ind)%Si_ind > 0) then
         Qsi(:,:,auto_ind) = min(autotrophSi_loc(:,:,auto_ind) / (autotrophC_loc(:,:,auto_ind) + epsC), gQsi_max)
      endif
   end do

!-----------------------------------------------------------------------
!  DETERMINE NEW ELEMENTAL RATIOS FOR GROWTH (NEW BIOMASS)
!  Modify these initial ratios under low ambient iron conditions
!  Modify the initial si/C ratio under low ambient Si conditions
!-----------------------------------------------------------------------

   cks       = 9._r8
   cksi      = 5._r8

   do auto_ind = 1, autotroph_cnt
      gQfe(:,:,auto_ind) = autotrophs(auto_ind)%gQfe_0
      where (Fe_loc < cks * autotrophs(auto_ind)%kFe)
         gQfe(:,:,auto_ind) = &
            max((gQfe(:,:,auto_ind) * Fe_loc / (cks * autotrophs(auto_ind)%kFe)), &
                autotrophs(auto_ind)%gQfe_min)
      end where

      if (autotrophs(auto_ind)%Si_ind > 0) then
         gQsi(:,:,auto_ind) = gQsi_0
         where ((Fe_loc < cksi * autotrophs(auto_ind)%kFe) .and. (Fe_loc > c0) .and. &
                (SiO3_loc > (cksi * autotrophs(auto_ind)%kSiO3)))
            gQsi(:,:,auto_ind) = min((gQsi(:,:,auto_ind) * cksi * autotrophs(auto_ind)%kFe / Fe_loc), gQsi_max)
         end where

         where (Fe_loc == c0)
            gQsi(:,:,auto_ind) = gQsi_max
         end where

         where (SiO3_loc < (cksi * autotrophs(auto_ind)%kSiO3))
            gQsi(:,:,auto_ind) = max((gQsi(:,:,auto_ind) * SiO3_loc / (cksi * autotrophs(auto_ind)%kSiO3)), &
                                          gQsi_min)
         end where
      endif

!-----------------------------------------------------------------------
!  QCaCO3 is the percentage of sp organic matter which is associated
!  with coccolithophores
!-----------------------------------------------------------------------

      if (autotrophs(auto_ind)%CaCO3_ind > 0) then
         QCaCO3(:,:,auto_ind) = autotrophCaCO3_loc(:,:,auto_ind) / (autotrophC_loc(:,:,auto_ind) + epsC)
         where (QCaCO3(:,:,auto_ind) > QCaCO3_max) QCaCO3(:,:,auto_ind) = QCaCO3_max
      endif
   end do

!-----------------------------------------------------------------------
!  various k==1 initializations
!
!  0.45   fraction of incoming SW -> PAR (non-dim)
!-----------------------------------------------------------------------

#ifndef FE_TAG
   if (k == 1) then
      call init_particulate_terms(POC, P_CaCO3, P_SiO2, dust, P_iron, &
# ifdef BEC
                                  QA_dust_def, dust_FLUX_IN)
# else
                                  QA_dust_def, dust_FLUX_IN, this_block)
# endif
   endif
#else
   if (k == 1) then
      call init_particulate_terms(POC, P_CaCO3, P_SiO2, dust, P_iron, &
                                  P_iron0, P_iron1, P_iron2, P_iron3, &
                                  P_iron4, P_iron5, P_iron6, P_iron7, &
# ifdef BEC
                                  QA_dust_def, dust_FLUX_IN)
# else
                                  QA_dust_def, dust_FLUX_IN, this_block)
# endif
   endif
#endif

!-----------------------------------------------------------------------
!  compute PAR related quantities
!  Morel, Maritorena, JGR, Vol 106, No. C4, pp 7163--7180, 2001
!-----------------------------------------------------------------------

   PAR_in = PAR_out(:,:,bid)
   where (.not. LAND_MASK(:,:,bid) .or. k > KMT(:,:,bid)) PAR_in = c0

   WORK1 = max(sum(autotrophChl_loc, dim=3), 0.02_r8)
   where (WORK1 < 0.13224_r8)
      KPARdz = 0.000919_r8*(WORK1**0.3536_r8)
   elsewhere
      KPARdz = 0.001131_r8*(WORK1**0.4562_r8)
   end where

   if (partial_bottom_cells) then
      KPARdz = KPARdz * DZT(:,:,k,bid)
   else
      KPARdz = KPARdz * dz(k)
   endif

   PAR_out(:,:,bid) = PAR_in * exp(-KPARdz)
   PAR_avg = PAR_in * (c1 - exp(-KPARdz)) / KPARdz

!-----------------------------------------------------------------------
!  compute terms of carbonate chemistry
!-----------------------------------------------------------------------
#if defined BEC && ( defined CISO || defined DIAGNOSTICS_BIO )
   lalt_co2_terms = popmini_tavg_now(tavg_CO3_ALT_CO2) .or. &
                    popmini_tavg_now(tavg_HCO3_ALT_CO2) .or. &
                    popmini_tavg_now(tavg_H2CO3_ALT_CO2) .or. &
                    popmini_tavg_now(tavg_pH_3D_ALT_CO2)

   if (popmini_tavg_now(tavg_CO3) .or. &
       popmini_tavg_now(tavg_HCO3) .or. &
       popmini_tavg_now(tavg_H2CO3) .or. &
       popmini_tavg_now(tavg_pH_3D) .or. &
       popmini_tavg_now(tavg_zsatcalc) .or. &
       popmini_tavg_now(tavg_zsatarag) .or. &
       lalt_co2_terms) then

!c      call timer_start(ecosys_comp_CO3terms_timer, block_id=bid)

      do j = 1,ny_block
         where (PH_PREV_3D(:,j,k,bid) /= c0)
            WORK1(:,j) = PH_PREV_3D(:,j,k,bid) - del_ph
            WORK2(:,j) = PH_PREV_3D(:,j,k,bid) + del_ph
         elsewhere
            WORK1(:,j) = phlo_3d_init
            WORK2(:,j) = phhi_3d_init
         end where
         call comp_CO3terms(bid, j, k, LAND_MASK(:,j,bid) .and. k <= KMT(:,j,bid), .true., &
                            TEMP(:,j), SALT(:,j), DIC_loc(:,j), ALK_loc(:,j), PO4_loc(:,j), SiO3_loc(:,j), &
                            WORK1(:,j), WORK2(:,j), WORK3(:,j), H2CO3(:,j), HCO3(:,j), CO3(:,j))
!c
!c         if (lalt_co2_terms) then
!c            where (PH_PREV_ALT_CO2_3D(:,j,k,bid) /= c0)
!c               WORK1(:,j) = PH_PREV_ALT_CO2_3D(:,j,k,bid) - del_ph
!c               WORK2(:,j) = PH_PREV_ALT_CO2_3D(:,j,k,bid) + del_ph
!c            elsewhere
!c               WORK1(:,j) = phlo_3d_init
!c               WORK2(:,j) = phhi_3d_init
!c            end where
!c            call comp_CO3terms(bid, j, k, LAND_MASK(:,j,bid) .and. k <= KMT(:,j,bid), .false., &
!c                               TEMP(:,j), SALT(:,j), DIC_ALT_CO2_loc(:,j), ALK_loc(:,j), PO4_loc(:,j), SiO3_loc(:,j), &
!c                               WORK1(:,j), WORK2(:,j), WORK4(:,j), H2CO3_ALT_CO2(:,j), HCO3_ALT_CO2(:,j), CO3_ALT_CO2(:,j))
!c         endif
      end do
!c      call timer_stop(ecosys_comp_CO3terms_timer, block_id=bid)
!c      call accumulate_tavg_field(CO3, tavg_CO3,bid,k)
!c      call accumulate_tavg_field(HCO3, tavg_HCO3,bid,k)
!c      call accumulate_tavg_field(H2CO3, tavg_H2CO3,bid,k)
!c      call accumulate_tavg_field(WORK3, tavg_pH_3D,bid,k)
!c      call accumulate_tavg_field(CO3_ALT_CO2, tavg_CO3_ALT_CO2,bid,k)
!c      call accumulate_tavg_field(HCO3_ALT_CO2, tavg_HCO3_ALT_CO2,bid,k)
!c      call accumulate_tavg_field(H2CO3_ALT_CO2, tavg_H2CO3_ALT_CO2,bid,k)
!c      call accumulate_tavg_field(WORK4, tavg_pH_3D_ALT_CO2,bid,k)
!c
      PH_PREV_3D(:,:,k,bid) = WORK3
!c
!c      if (lalt_co2_terms) then
!c         PH_PREV_ALT_CO2_3D(:,:,k,bid) = WORK4
!c      endif
   endif
!c
!c   if (accumulate_tavg_now(tavg_co3_sat_calc) .or. &
!c       accumulate_tavg_now(tavg_zsatcalc) .or. &
!c       accumulate_tavg_now(tavg_co3_sat_arag) .or. &
!c       accumulate_tavg_now(tavg_zsatarag)) then
!c      call comp_co3_sat_vals(k, LAND_MASK(:,:,bid) .and. k <= KMT(:,:,bid), &
!c                             TEMP, SALT, WORK1, WORK2)
!c      call accumulate_tavg_field(WORK1, tavg_co3_sat_calc,bid,k)
!c      if (accumulate_tavg_now(tavg_zsatcalc)) then
!c         if (k == 1) then
!c            ! set to -1, i.e. depth not found yet,
!c            ! if mask == .true. and surface supersaturated to -1
!c            ZSATCALC(:,:,bid) = merge(-c1, c0, LAND_MASK(:,:,bid) .and. CO3 > WORK1)
!c         else
!c            where (ZSATCALC(:,:,bid) == -c1 .and. CO3 <= WORK1)
!c               ZSATCALC(:,:,bid) = zt(k-1) + (zt(k) - zt(k-1)) * &
!c                  CO3_CALC_ANOM_km1(:,:,bid) / (CO3_CALC_ANOM_km1(:,:,bid) - (CO3 - WORK1))
!c            endwhere
!c            where (ZSATCALC(:,:,bid) == -c1 .and. KMT(:,:,bid) == k)
!c               ZSATCALC(:,:,bid) = zw(k)
!c            endwhere
!c         endif
!c         CO3_CALC_ANOM_km1(:,:,bid) = CO3 - WORK1
!c         if (k == km) then
!c            call accumulate_tavg_field(ZSATCALC(:,:,bid), tavg_zsatcalc,bid,k)
!c         endif
!c      endif
!c      call accumulate_tavg_field(WORK2, tavg_co3_sat_arag,bid,k)
!c      if (accumulate_tavg_now(tavg_zsatarag)) then
!c         if (k == 1) then
!c            ! set to -1, i.e. depth not found yet,
!c            ! if mask == .true. and surface supersaturated to -1
!c            ZSATARAG(:,:,bid) = merge(-c1, c0, LAND_MASK(:,:,bid) .and. CO3 > WORK2)
!c         else
!c            where (ZSATARAG(:,:,bid) == -c1 .and. CO3 <= WORK2)
!c               ZSATARAG(:,:,bid) = zt(k-1) + (zt(k) - zt(k-1)) * &
!c                  CO3_ARAG_ANOM_km1(:,:,bid) / (CO3_ARAG_ANOM_km1(:,:,bid) - (CO3 - WORK2))
!c            endwhere
!c            where (ZSATARAG(:,:,bid) == -c1 .and. KMT(:,:,bid) == k)
!c               ZSATARAG(:,:,bid) = zw(k)
!c            endwhere
!c         endif
!c         CO3_ARAG_ANOM_km1(:,:,bid) = CO3 - WORK2
!c         if (k == km) then
!c            call accumulate_tavg_field(ZSATARAG(:,:,bid), tavg_zsatarag,bid,k)
!c         endif
!c      endif
!c   endif

#endif

!-----------------------------------------------------------------------
!  Tref = 30.0 reference temperature (deg. C)
!
!  Using q10 formulation with Q10 value of 2.0 (Doney et al., 1996).
!  growth, mort and grazing rates scaled by Tfunc where they are computed
!-----------------------------------------------------------------------

   Tfunc = Q_10**(((TEMP + T0_Kelvin) - (Tref + T0_Kelvin)) / c10)

!-----------------------------------------------------------------------
!  calculate the loss threshold interpolation factor
!-----------------------------------------------------------------------

#ifdef BEC
   where (zt(:,:,k) > thres_z1)
      where (zt(:,:,k) < thres_z2)
         f_loss_thres = (thres_z2 - zt(:,:,k))/(thres_z2 - thres_z1)
      elsewhere
         f_loss_thres = c0
      end where
   elsewhere
      f_loss_thres = c1
   end where
#else
   if (zt(k) > thres_z1) then
      if (zt(k) < thres_z2) then
         f_loss_thres = (thres_z2 - zt(k))/(thres_z2 - thres_z1)
      else
         f_loss_thres = c0
      endif
   else
      f_loss_thres = c1
   endif
#endif

!-----------------------------------------------------------------------
!  Compute Pprime for all autotrophs, used for loss terms
!-----------------------------------------------------------------------

   do auto_ind = 1, autotroph_cnt
      C_loss_thres = f_loss_thres * autotrophs(auto_ind)%loss_thres
      where (TEMP < autotrophs(auto_ind)%temp_thres) C_loss_thres = f_loss_thres * autotrophs(auto_ind)%loss_thres2

      Pprime(:,:,auto_ind) = max(autotrophC_loc(:,:,auto_ind) - C_loss_thres, c0)
   end do

!-----------------------------------------------------------------------
!  Get relative nutrient uptake rates for autotrophs,
!  min. relative uptake rate modifies C fixation in the manner
!  that the min. cell quota does in GD98.
!-----------------------------------------------------------------------

#ifndef FE_TAG
   auto_loss = c0
   auto_agg  = c0
#else
   auto_loss     = c0
   auto_loss_Fe  = c0
   auto_loss_Fe0 = c0
   auto_loss_Fe1 = c0
   auto_loss_Fe2 = c0
   auto_loss_Fe3 = c0
   auto_loss_Fe4 = c0
   auto_loss_Fe5 = c0
   auto_loss_Fe6 = c0
   auto_loss_Fe7 = c0
   auto_agg      = c0
   auto_agg_Fe   = c0
   auto_agg_Fe0  = c0
   auto_agg_Fe1  = c0
   auto_agg_Fe2  = c0
   auto_agg_Fe3  = c0
   auto_agg_Fe4  = c0
   auto_agg_Fe5  = c0
   auto_agg_Fe6  = c0
   auto_agg_Fe7  = c0
#endif

   do auto_ind = 1, autotroph_cnt
      VNO3(:,:,auto_ind) = (NO3_loc / autotrophs(auto_ind)%kNO3) / &
         (c1 + (NO3_loc / autotrophs(auto_ind)%kNO3) + (NH4_loc / autotrophs(auto_ind)%kNH4))
      VNH4(:,:,auto_ind) = (NH4_loc / autotrophs(auto_ind)%kNH4) / &
         (c1 + (NO3_loc / autotrophs(auto_ind)%kNO3) + (NH4_loc / autotrophs(auto_ind)%kNH4))
      VNtot(:,:,auto_ind) = VNO3(:,:,auto_ind) + VNH4(:,:,auto_ind)
      if (autotrophs(auto_ind)%Nfixer) VNtot(:,:,auto_ind) = c1
!c      call accumulate_tavg_field(VNtot(:,:,auto_ind), tavg_N_lim(auto_ind),bid,k)

      VFe = Fe_loc / (Fe_loc + autotrophs(auto_ind)%kFe)
!c      call accumulate_tavg_field(VFe, tavg_Fe_lim(auto_ind),bid,k)

      f_nut = min(VNtot(:,:,auto_ind), VFe)

      VPO4 = (PO4_loc / autotrophs(auto_ind)%kPO4) / &
         (c1 + (PO4_loc / autotrophs(auto_ind)%kPO4) + (DOP_loc / autotrophs(auto_ind)%kDOP))
      VDOP = (DOP_loc / autotrophs(auto_ind)%kDOP) / &
         (c1 + (PO4_loc / autotrophs(auto_ind)%kPO4) + (DOP_loc / autotrophs(auto_ind)%kDOP))
      VPtot = VPO4 + VDOP
!c      call accumulate_tavg_field(VPtot, tavg_P_lim(auto_ind),bid,k)

      f_nut = min(f_nut, VPO4)

      if (autotrophs(auto_ind)%kSiO3 > c0) then
         VSiO3 = SiO3_loc / (SiO3_loc + autotrophs(auto_ind)%kSiO3)
!c         call accumulate_tavg_field(VSiO3, tavg_SiO3_lim(auto_ind),bid,k)
         f_nut = min(f_nut, VSiO3)
      endif


!-----------------------------------------------------------------------
!     get photosynth. rate, phyto C biomass change, photoadapt
!-----------------------------------------------------------------------

      PCmax = autotrophs(auto_ind)%PCref * f_nut * Tfunc
      where (TEMP < autotrophs(auto_ind)%temp_thres) PCmax = c0

      light_lim = (c1 - exp((-c1 * autotrophs(auto_ind)%alphaPI * thetaC(:,:,auto_ind) * PAR_avg) / &
                            (PCmax + epsTinv)))

#ifdef DICE
      light_lim = (c1 - ice) * light_lim
#endif
      PCphoto(:,:,auto_ind) = PCmax * light_lim

!c      call accumulate_tavg_field(light_lim, tavg_light_lim(auto_ind),bid,k)

#ifdef DIAGNOSTICS_BIO
     select case(auto_ind)
     case(1)
        call accumulate_tavg_field_3d(light_lim ,ilight_lim_sp  ,bid,k)
        call accumulate_tavg_field_3d(VNtot(:,:,auto_ind),iVNtot_sp  ,bid,k)
        call accumulate_tavg_field_3d(VFe(:,:)  ,iVFe_sp    ,bid,k)
        call accumulate_tavg_field_3d(VPtot(:,:),iVPtot_sp  ,bid,k)
     case(2)
        call accumulate_tavg_field_3d(light_lim ,ilight_lim_diat,bid,k)
        call accumulate_tavg_field_3d(VNtot(:,:,auto_ind),iVNtot_diat,bid,k)
        call accumulate_tavg_field_3d(VFe(:,:)  ,iVFe_diat  ,bid,k)
        call accumulate_tavg_field_3d(VPtot(:,:),iVPtot_diat,bid,k)
        call accumulate_tavg_field_3d(VSiO3(:,:),iVSiO3_diat,bid,k)
     case(3)
        call accumulate_tavg_field_3d(light_lim ,ilight_lim_diaz,bid,k)
        call accumulate_tavg_field_3d(VNtot(:,:,auto_ind),iVNtot_diaz,bid,k)
        call accumulate_tavg_field_3d(VFe(:,:)  ,iVFe_diaz  ,bid,k)
        call accumulate_tavg_field_3d(VPtot(:,:),iVPtot_diaz,bid,k)
     end select
#endif

      photoC(:,:,auto_ind) = PCphoto(:,:,auto_ind) * autotrophC_loc(:,:,auto_ind)

!-----------------------------------------------------------------------
!  Get nutrient uptakes by small phyto based on calculated C fixation
!  total N uptake VNC is used in photoadaption
!-----------------------------------------------------------------------

      where (VNtot(:,:,auto_ind) > c0)
         NO3_V(:,:,auto_ind) = (VNO3(:,:,auto_ind) / VNtot(:,:,auto_ind)) * photoC(:,:,auto_ind) * Q
         NH4_V(:,:,auto_ind) = (VNH4(:,:,auto_ind) / VNtot(:,:,auto_ind)) * photoC(:,:,auto_ind) * Q
         VNC = PCphoto(:,:,auto_ind) * Q
      elsewhere
         NO3_V(:,:,auto_ind) = c0
         NH4_V(:,:,auto_ind) = c0
         VNC = c0
      end where
!c      call accumulate_tavg_field(NO3_V(:,:,auto_ind), tavg_photoNO3(auto_ind),bid,k)
!c      call accumulate_tavg_field(NH4_V(:,:,auto_ind), tavg_photoNH4(auto_ind),bid,k)

      where (VPtot > c0)
         PO4_V(:,:,auto_ind) = (VPO4 / VPtot) * photoC(:,:,auto_ind) * autotrophs(auto_ind)%Qp
         DOP_V(:,:,auto_ind) = (VDOP / VPtot) * photoC(:,:,auto_ind) * autotrophs(auto_ind)%Qp
      elsewhere
         PO4_V(:,:,auto_ind) = c0
         DOP_V(:,:,auto_ind) = c0
      end where
!c      call accumulate_tavg_field(PO4_V(:,:,auto_ind), tavg_PO4_uptake(auto_ind),bid,k)
!c      call accumulate_tavg_field(DOP_V(:,:,auto_ind), tavg_DOP_uptake(auto_ind),bid,k)

#ifndef FE_TAG
      photoFe(:,:,auto_ind) = photoC(:,:,auto_ind) * gQfe(:,:,auto_ind)
#else
      photoFe(:,:,auto_ind) = photoC(:,:,auto_ind) * gQfe(:,:,auto_ind)

      s0=Fe0_loc
      s1=Fe1_loc
      s2=Fe2_loc
      s3=Fe3_loc
      s4=Fe4_loc
      s5=Fe5_loc
      s6=Fe6_loc
      s7=Fe7_loc
      s=s0+s1+s2+s3+s4+s5+s6+s7

      where(s>c0)
         photoFe0(:,:,auto_ind) = photoFe(:,:,auto_ind) * s0/s
         photoFe1(:,:,auto_ind) = photoFe(:,:,auto_ind) * s1/s
         photoFe2(:,:,auto_ind) = photoFe(:,:,auto_ind) * s2/s
         photoFe3(:,:,auto_ind) = photoFe(:,:,auto_ind) * s3/s
         photoFe4(:,:,auto_ind) = photoFe(:,:,auto_ind) * s4/s
         photoFe5(:,:,auto_ind) = photoFe(:,:,auto_ind) * s5/s
         photoFe6(:,:,auto_ind) = photoFe(:,:,auto_ind) * s6/s
         photoFe7(:,:,auto_ind) = photoFe(:,:,auto_ind) * s7/s
      elsewhere
         photoFe0(:,:,auto_ind) = c0
         photoFe1(:,:,auto_ind) = c0
         photoFe2(:,:,auto_ind) = c0
         photoFe3(:,:,auto_ind) = c0
         photoFe4(:,:,auto_ind) = c0
         photoFe5(:,:,auto_ind) = c0
         photoFe6(:,:,auto_ind) = c0
         photoFe7(:,:,auto_ind) = c0
      endwhere
#endif

!c      call accumulate_tavg_field(photoFe(:,:,auto_ind), tavg_photoFe(auto_ind),bid,k)

!-----------------------------------------------------------------------
!  Get nutrient uptake by diatoms based on C fixation
!-----------------------------------------------------------------------

      if (autotrophs(auto_ind)%Si_ind > 0) then
         photoSi(:,:,auto_ind) = photoC(:,:,auto_ind) * gQsi(:,:,auto_ind)
!c         call accumulate_tavg_field(photoSi(:,:,auto_ind), tavg_bSi_form(auto_ind),bid,k)
!c         call accumulate_tavg_field(photoSi(:,:,auto_ind), tavg_tot_bSi_form,bid,k)
      endif

!-----------------------------------------------------------------------
!  calculate pChl, (used in photoadapt., GD98)
!  2.3   max value of thetaN (Chl/N ratio) (mg Chl/mmol N)
!  GD 98 Chl. synth. term
!-----------------------------------------------------------------------

      WORK1 = autotrophs(auto_ind)%alphaPI * thetaC(:,:,auto_ind) * PAR_avg
      where (WORK1 > c0)
         pChl = autotrophs(auto_ind)%thetaN_max * PCphoto(:,:,auto_ind) / WORK1
         photoacc(:,:,auto_ind) = (pChl * VNC / thetaC(:,:,auto_ind)) * autotrophChl_loc(:,:,auto_ind)
      elsewhere
         photoacc(:,:,auto_ind) = c0
      end where

!-----------------------------------------------------------------------
!  CaCO3 Production, parameterized as function of small phyto production
!  decrease CaCO3 as function of nutrient limitation decrease CaCO3 prod
!  at low temperatures increase CaCO3 prod under bloom conditions
!  maximum calcification rate is 40% of primary production
!-----------------------------------------------------------------------

      if (autotrophs(auto_ind)%imp_calcifier) then
         CaCO3_PROD(:,:,auto_ind) = parm_f_prod_sp_CaCO3 * photoC(:,:,auto_ind)
         CaCO3_PROD(:,:,auto_ind) = CaCO3_PROD(:,:,auto_ind) * f_nut

         where (TEMP < CaCO3_temp_thres1)  &
            CaCO3_PROD(:,:,auto_ind) = CaCO3_PROD(:,:,auto_ind) * max((TEMP-CaCO3_temp_thres2), c0) / &
                         (CaCO3_temp_thres1-CaCO3_temp_thres2)

         where (autotrophC_loc(:,:,auto_ind) > CaCO3_sp_thres)  &
            CaCO3_PROD(:,:,auto_ind) = min((CaCO3_PROD(:,:,auto_ind) * autotrophC_loc(:,:,auto_ind) / CaCO3_sp_thres), &
                             (f_photosp_CaCO3 * photoC(:,:,auto_ind)))

!c         call accumulate_tavg_field(CaCO3_PROD(:,:,auto_ind), tavg_CaCO3_form(auto_ind),bid,k)
!c         call accumulate_tavg_field(CaCO3_PROD(:,:,auto_ind), tavg_tot_CaCO3_form,bid,k)
!c
!c         if (accumulate_tavg_now(tavg_CaCO3_form_zint(auto_ind)) .or. &
!c             accumulate_tavg_now(tavg_tot_CaCO3_form_zint)) then
!c            if (partial_bottom_cells) then
!c               WORK1 = merge(DZT(:,:,k,bid) * CaCO3_PROD(:,:,auto_ind), c0,k<=KMT(:,:,bid))
!c            else
!c               WORK1 = merge(dz(k) * CaCO3_PROD(:,:,auto_ind), c0,k<=KMT(:,:,bid))
!c            endif
!c            call accumulate_tavg_field(WORK1, tavg_CaCO3_form_zint(auto_ind),bid,k)
!c            call accumulate_tavg_field(WORK1, tavg_tot_CaCO3_form_zint,bid,k)
!c         endif
      endif

!-----------------------------------------------------------------------
!  get autotroph loss (in C units)
!  autotroph agg loss
!-----------------------------------------------------------------------


#ifndef FE_TAG
      auto_loss(:,:,auto_ind) = autotrophs(auto_ind)%mort * Pprime(:,:,auto_ind) * Tfunc
#else
      auto_loss(:,:,auto_ind) = autotrophs(auto_ind)%mort * Pprime(:,:,auto_ind) * Tfunc

      auto_loss_Fe(:,:,auto_ind) = Qfe(:,:,auto_ind) * auto_loss(:,:,auto_ind)

      s0=autotrophFe0_loc(:,:,auto_ind)
      s1=autotrophFe1_loc(:,:,auto_ind)
      s2=autotrophFe2_loc(:,:,auto_ind)
      s3=autotrophFe3_loc(:,:,auto_ind)
      s4=autotrophFe4_loc(:,:,auto_ind)
      s5=autotrophFe5_loc(:,:,auto_ind)
      s6=autotrophFe6_loc(:,:,auto_ind)
      s7=autotrophFe7_loc(:,:,auto_ind)
      s=s0+s1+s2+s3+s4+s5+s6+s7

      where(s>c0)
         auto_loss_Fe0(:,:,auto_ind) = auto_loss_Fe(:,:,auto_ind) * s0/s
         auto_loss_Fe1(:,:,auto_ind) = auto_loss_Fe(:,:,auto_ind) * s1/s
         auto_loss_Fe2(:,:,auto_ind) = auto_loss_Fe(:,:,auto_ind) * s2/s
         auto_loss_Fe3(:,:,auto_ind) = auto_loss_Fe(:,:,auto_ind) * s3/s
         auto_loss_Fe4(:,:,auto_ind) = auto_loss_Fe(:,:,auto_ind) * s4/s
         auto_loss_Fe5(:,:,auto_ind) = auto_loss_Fe(:,:,auto_ind) * s5/s
         auto_loss_Fe6(:,:,auto_ind) = auto_loss_Fe(:,:,auto_ind) * s6/s
         auto_loss_Fe7(:,:,auto_ind) = auto_loss_Fe(:,:,auto_ind) * s7/s
      else where
         auto_loss_Fe0(:,:,auto_ind) = c0
         auto_loss_Fe1(:,:,auto_ind) = c0
         auto_loss_Fe2(:,:,auto_ind) = c0
         auto_loss_Fe3(:,:,auto_ind) = c0
         auto_loss_Fe4(:,:,auto_ind) = c0
         auto_loss_Fe5(:,:,auto_ind) = c0
         auto_loss_Fe6(:,:,auto_ind) = c0
         auto_loss_Fe7(:,:,auto_ind) = c0
      endwhere
#endif

#ifndef FE_TAG
      auto_agg(:,:,auto_ind) = min((autotrophs(auto_ind)%agg_rate_max * dps) * Pprime(:,:,auto_ind), &
                                   autotrophs(auto_ind)%mort2 * Pprime(:,:,auto_ind) * Pprime(:,:,auto_ind))
      auto_agg(:,:,auto_ind) = max((autotrophs(auto_ind)%agg_rate_min * dps) * Pprime(:,:,auto_ind), auto_agg(:,:,auto_ind))
#else
      auto_agg(:,:,auto_ind) = min((autotrophs(auto_ind)%agg_rate_max * dps) * Pprime(:,:,auto_ind), &
                                   autotrophs(auto_ind)%mort2 * Pprime(:,:,auto_ind) * Pprime(:,:,auto_ind))
      auto_agg(:,:,auto_ind) = max((autotrophs(auto_ind)%agg_rate_min * dps) * Pprime(:,:,auto_ind), auto_agg(:,:,auto_ind))

      auto_agg_Fe(:,:,auto_ind) = Qfe(:,:,auto_ind) * auto_agg(:,:,auto_ind)

      s0=autotrophFe0_loc(:,:,auto_ind)
      s1=autotrophFe1_loc(:,:,auto_ind)
      s2=autotrophFe2_loc(:,:,auto_ind)
      s3=autotrophFe3_loc(:,:,auto_ind)
      s4=autotrophFe4_loc(:,:,auto_ind)
      s5=autotrophFe5_loc(:,:,auto_ind)
      s6=autotrophFe6_loc(:,:,auto_ind)
      s7=autotrophFe7_loc(:,:,auto_ind)
      s=s0+s1+s2+s3+s4+s5+s6+s7

      where(s>c0)
         auto_agg_Fe0(:,:,auto_ind) = auto_agg_Fe(:,:,auto_ind) * s0/s
         auto_agg_Fe1(:,:,auto_ind) = auto_agg_Fe(:,:,auto_ind) * s1/s
         auto_agg_Fe2(:,:,auto_ind) = auto_agg_Fe(:,:,auto_ind) * s2/s
         auto_agg_Fe3(:,:,auto_ind) = auto_agg_Fe(:,:,auto_ind) * s3/s
         auto_agg_Fe4(:,:,auto_ind) = auto_agg_Fe(:,:,auto_ind) * s4/s
         auto_agg_Fe5(:,:,auto_ind) = auto_agg_Fe(:,:,auto_ind) * s5/s
         auto_agg_Fe6(:,:,auto_ind) = auto_agg_Fe(:,:,auto_ind) * s6/s
         auto_agg_Fe7(:,:,auto_ind) = auto_agg_Fe(:,:,auto_ind) * s7/s
      else where
         auto_agg_Fe0(:,:,auto_ind) = c0
         auto_agg_Fe1(:,:,auto_ind) = c0
         auto_agg_Fe2(:,:,auto_ind) = c0
         auto_agg_Fe3(:,:,auto_ind) = c0
         auto_agg_Fe4(:,:,auto_ind) = c0
         auto_agg_Fe5(:,:,auto_ind) = c0
         auto_agg_Fe6(:,:,auto_ind) = c0
         auto_agg_Fe7(:,:,auto_ind) = c0
      endwhere
#endif

!-----------------------------------------------------------------------
!  Get N fixation by diazotrophs based on C fixation,
!  Diazotrophs fix more than they need then 20% is excreted
!-----------------------------------------------------------------------

      if (autotrophs(auto_ind)%Nfixer) then
         WORK1 = photoC(:,:,auto_ind) * Q
         Nfix(:,:,auto_ind)     = (WORK1 * r_Nfix_photo) - NO3_V(:,:,auto_ind) - NH4_V(:,:,auto_ind)
         Nexcrete(:,:,auto_ind) = Nfix(:,:,auto_ind) + NO3_V(:,:,auto_ind) + NH4_V(:,:,auto_ind) - WORK1
!c         call accumulate_tavg_field(Nfix(:,:,auto_ind), tavg_Nfix(auto_ind),bid,k)
!c         call accumulate_tavg_field(Nfix(:,:,auto_ind), tavg_tot_Nfix,bid,k)
      endif

!-----------------------------------------------------------------------
!  routing of loss terms
!  all aggregation goes to POC
!  min.%C routed from sp_loss = 0.59 * QCaCO3, or P_CaCO3%rho
!-----------------------------------------------------------------------
#ifndef FE_TAG
      if (autotrophs(auto_ind)%imp_calcifier) then
         auto_loss_poc(:,:,auto_ind) = QCaCO3(:,:,auto_ind) * auto_loss(:,:,auto_ind)
      else
         auto_loss_poc(:,:,auto_ind) = autotrophs(auto_ind)%loss_poc * auto_loss(:,:,auto_ind)
      endif
#else
      if (autotrophs(auto_ind)%imp_calcifier) then
         auto_loss_poc(:,:,auto_ind) = QCaCO3(:,:,auto_ind) * auto_loss(:,:,auto_ind)

         auto_loss_poc_Fe0(:,:,auto_ind) = QCaCO3(:,:,auto_ind) * auto_loss_Fe0(:,:,auto_ind)
         auto_loss_poc_Fe1(:,:,auto_ind) = QCaCO3(:,:,auto_ind) * auto_loss_Fe1(:,:,auto_ind)
         auto_loss_poc_Fe2(:,:,auto_ind) = QCaCO3(:,:,auto_ind) * auto_loss_Fe2(:,:,auto_ind)
         auto_loss_poc_Fe3(:,:,auto_ind) = QCaCO3(:,:,auto_ind) * auto_loss_Fe3(:,:,auto_ind)
         auto_loss_poc_Fe4(:,:,auto_ind) = QCaCO3(:,:,auto_ind) * auto_loss_Fe4(:,:,auto_ind)
         auto_loss_poc_Fe5(:,:,auto_ind) = QCaCO3(:,:,auto_ind) * auto_loss_Fe5(:,:,auto_ind)
         auto_loss_poc_Fe6(:,:,auto_ind) = QCaCO3(:,:,auto_ind) * auto_loss_Fe6(:,:,auto_ind)
         auto_loss_poc_Fe7(:,:,auto_ind) = QCaCO3(:,:,auto_ind) * auto_loss_Fe7(:,:,auto_ind)
      else
         auto_loss_poc(:,:,auto_ind) = autotrophs(auto_ind)%loss_poc * auto_loss(:,:,auto_ind)

         auto_loss_poc_Fe0(:,:,auto_ind) = autotrophs(auto_ind)%loss_poc * auto_loss_Fe0(:,:,auto_ind)
         auto_loss_poc_Fe1(:,:,auto_ind) = autotrophs(auto_ind)%loss_poc * auto_loss_Fe1(:,:,auto_ind)
         auto_loss_poc_Fe2(:,:,auto_ind) = autotrophs(auto_ind)%loss_poc * auto_loss_Fe2(:,:,auto_ind)
         auto_loss_poc_Fe3(:,:,auto_ind) = autotrophs(auto_ind)%loss_poc * auto_loss_Fe3(:,:,auto_ind)
         auto_loss_poc_Fe4(:,:,auto_ind) = autotrophs(auto_ind)%loss_poc * auto_loss_Fe4(:,:,auto_ind)
         auto_loss_poc_Fe5(:,:,auto_ind) = autotrophs(auto_ind)%loss_poc * auto_loss_Fe5(:,:,auto_ind)
         auto_loss_poc_Fe6(:,:,auto_ind) = autotrophs(auto_ind)%loss_poc * auto_loss_Fe6(:,:,auto_ind)
         auto_loss_poc_Fe7(:,:,auto_ind) = autotrophs(auto_ind)%loss_poc * auto_loss_Fe7(:,:,auto_ind)
      endif
#endif

#ifndef FE_TAG
      auto_loss_doc(:,:,auto_ind) = (c1 - parm_labile_ratio) * (auto_loss(:,:,auto_ind) - auto_loss_poc(:,:,auto_ind))
#else
      auto_loss_doc(:,:,auto_ind) = (c1 - parm_labile_ratio) * (auto_loss(:,:,auto_ind) - auto_loss_poc(:,:,auto_ind))

      auto_loss_doc_Fe0(:,:,auto_ind) = (c1 - parm_labile_ratio) * (auto_loss_Fe0(:,:,auto_ind) - auto_loss_poc_Fe0(:,:,auto_ind))
      auto_loss_doc_Fe1(:,:,auto_ind) = (c1 - parm_labile_ratio) * (auto_loss_Fe1(:,:,auto_ind) - auto_loss_poc_Fe1(:,:,auto_ind))
      auto_loss_doc_Fe2(:,:,auto_ind) = (c1 - parm_labile_ratio) * (auto_loss_Fe2(:,:,auto_ind) - auto_loss_poc_Fe2(:,:,auto_ind))
      auto_loss_doc_Fe3(:,:,auto_ind) = (c1 - parm_labile_ratio) * (auto_loss_Fe3(:,:,auto_ind) - auto_loss_poc_Fe3(:,:,auto_ind))
      auto_loss_doc_Fe4(:,:,auto_ind) = (c1 - parm_labile_ratio) * (auto_loss_Fe4(:,:,auto_ind) - auto_loss_poc_Fe4(:,:,auto_ind))
      auto_loss_doc_Fe5(:,:,auto_ind) = (c1 - parm_labile_ratio) * (auto_loss_Fe5(:,:,auto_ind) - auto_loss_poc_Fe5(:,:,auto_ind))
      auto_loss_doc_Fe6(:,:,auto_ind) = (c1 - parm_labile_ratio) * (auto_loss_Fe6(:,:,auto_ind) - auto_loss_poc_Fe6(:,:,auto_ind))
      auto_loss_doc_Fe7(:,:,auto_ind) = (c1 - parm_labile_ratio) * (auto_loss_Fe7(:,:,auto_ind) - auto_loss_poc_Fe7(:,:,auto_ind))
#endif
#ifndef FE_TAG
      auto_loss_dic(:,:,auto_ind) = parm_labile_ratio * (auto_loss(:,:,auto_ind) - auto_loss_poc(:,:,auto_ind))
#else
      auto_loss_dic(:,:,auto_ind) = parm_labile_ratio * (auto_loss(:,:,auto_ind) - auto_loss_poc(:,:,auto_ind))

      auto_loss_dic_Fe0(:,:,auto_ind) = parm_labile_ratio * (auto_loss_Fe0(:,:,auto_ind) - auto_loss_poc_Fe0(:,:,auto_ind))
      auto_loss_dic_Fe1(:,:,auto_ind) = parm_labile_ratio * (auto_loss_Fe1(:,:,auto_ind) - auto_loss_poc_Fe1(:,:,auto_ind))
      auto_loss_dic_Fe2(:,:,auto_ind) = parm_labile_ratio * (auto_loss_Fe2(:,:,auto_ind) - auto_loss_poc_Fe2(:,:,auto_ind))
      auto_loss_dic_Fe3(:,:,auto_ind) = parm_labile_ratio * (auto_loss_Fe3(:,:,auto_ind) - auto_loss_poc_Fe3(:,:,auto_ind))
      auto_loss_dic_Fe4(:,:,auto_ind) = parm_labile_ratio * (auto_loss_Fe4(:,:,auto_ind) - auto_loss_poc_Fe4(:,:,auto_ind))
      auto_loss_dic_Fe5(:,:,auto_ind) = parm_labile_ratio * (auto_loss_Fe5(:,:,auto_ind) - auto_loss_poc_Fe5(:,:,auto_ind))
      auto_loss_dic_Fe6(:,:,auto_ind) = parm_labile_ratio * (auto_loss_Fe6(:,:,auto_ind) - auto_loss_poc_Fe6(:,:,auto_ind))
      auto_loss_dic_Fe7(:,:,auto_ind) = parm_labile_ratio * (auto_loss_Fe7(:,:,auto_ind) - auto_loss_poc_Fe7(:,:,auto_ind))
#endif

   end do  ! auto_ind = 1, autotroph_cnt

!-----------------------------------------------------------------------
!  0.01 small zoo threshold C concentration (mmol C/m^3)
!  zoo losses, scaled by Tfunc
!-----------------------------------------------------------------------

   do zoo_ind = 1, zooplankton_cnt
      C_loss_thres = f_loss_thres * zooplankton(zoo_ind)%loss_thres
      Zprime(:,:,zoo_ind) = max(zooC_loc(:,:,zoo_ind) - C_loss_thres, c0)

#ifndef FE_TAG
      zoo_loss(:,:,zoo_ind) = (zooplankton(zoo_ind)%z_mort2_0 * Zprime(:,:,zoo_ind)**1.5_r8 &
           + zooplankton(zoo_ind)%z_mort_0 * Zprime(:,:,zoo_ind)) * Tfunc
#else
      zoo_loss(:,:,zoo_ind) = (zooplankton(zoo_ind)%z_mort2_0 * Zprime(:,:,zoo_ind)**1.5_r8 &
           + zooplankton(zoo_ind)%z_mort_0 * Zprime(:,:,zoo_ind)) * Tfunc

      WORK1_Fe = Qfe_zoo &
           * ( (zooplankton(zoo_ind)%z_mort2_0 * Zprime(:,:,zoo_ind)**1.5_r8 &
           +    zooplankton(zoo_ind)%z_mort_0 * Zprime(:,:,zoo_ind)) * Tfunc )

      zoo_loss_Fe(:,:,zoo_ind) = WORK1_Fe
      s0 = zooFe0_loc(:,:,zoo_ind)
      s1 = zooFe1_loc(:,:,zoo_ind)
      s2 = zooFe2_loc(:,:,zoo_ind)
      s3 = zooFe3_loc(:,:,zoo_ind)
      s4 = zooFe4_loc(:,:,zoo_ind)
      s5 = zooFe5_loc(:,:,zoo_ind)
      s6 = zooFe6_loc(:,:,zoo_ind)
      s7 = zooFe7_loc(:,:,zoo_ind)
      s=s0+s1+s2+s3+s4+s5+s6+s7

      where(s>c0)
         zoo_loss_Fe0(:,:,zoo_ind) = WORK1_Fe * s0/s
         zoo_loss_Fe1(:,:,zoo_ind) = WORK1_Fe * s1/s
         zoo_loss_Fe2(:,:,zoo_ind) = WORK1_Fe * s2/s
         zoo_loss_Fe3(:,:,zoo_ind) = WORK1_Fe * s3/s
         zoo_loss_Fe4(:,:,zoo_ind) = WORK1_Fe * s4/s
         zoo_loss_Fe5(:,:,zoo_ind) = WORK1_Fe * s5/s
         zoo_loss_Fe6(:,:,zoo_ind) = WORK1_Fe * s6/s
         zoo_loss_Fe7(:,:,zoo_ind) = WORK1_Fe * s7/s
      else where
         zoo_loss_Fe0(:,:,zoo_ind) = c0
         zoo_loss_Fe1(:,:,zoo_ind) = c0
         zoo_loss_Fe2(:,:,zoo_ind) = c0
         zoo_loss_Fe3(:,:,zoo_ind) = c0
         zoo_loss_Fe4(:,:,zoo_ind) = c0
         zoo_loss_Fe5(:,:,zoo_ind) = c0
         zoo_loss_Fe6(:,:,zoo_ind) = c0
         zoo_loss_Fe7(:,:,zoo_ind) = c0
      endwhere
#endif
   end do


!-----------------------------------------------------------------------
!  CALCULATE GRAZING
!
!  Autotroph prey
!  routing of grazing terms
!  all aggregation goes to POC
!  currently assumes that 33% of grazed caco3 is remineralized
!  if autotrophs(sp_ind)%graze_zoo ever changes, coefficients on routing grazed sp must change!
!  min.%C routed to POC from grazing for ballast requirements = 0.4 * Qcaco3
!  NOTE: if autotrophs(diat_ind)%graze_zoo is changed, coeff.s for poc,doc and dic must change!
!-----------------------------------------------------------------------
#ifndef FE_TAG
   auto_graze_dic = c0 ! autotroph grazing losses routed to dic (computed by residual)
#else
   auto_graze_dic = c0     ! autotroph grazing losses routed to dic (computed by residual)
   auto_graze_dic_Fe0 = c0 ! autotroph grazing losses routed to dic (computed by residual)
   auto_graze_dic_Fe1 = c0 ! autotroph grazing losses routed to dic (computed by residual)
   auto_graze_dic_Fe2 = c0 ! autotroph grazing losses routed to dic (computed by residual)
   auto_graze_dic_Fe3 = c0 ! autotroph grazing losses routed to dic (computed by residual)
   auto_graze_dic_Fe4 = c0 ! autotroph grazing losses routed to dic (computed by residual)
   auto_graze_dic_Fe5 = c0 ! autotroph grazing losses routed to dic (computed by residual)
   auto_graze_dic_Fe6 = c0 ! autotroph grazing losses routed to dic (computed by residual)
   auto_graze_dic_Fe7 = c0 ! autotroph grazing losses routed to dic (computed by residual)
#endif
#ifndef FE_TAG
   auto_graze_poc = c0 ! autotroph grazing losses routed to poc
#else
   auto_graze_poc = c0     ! autotroph grazing losses routed to poc
   auto_graze_poc_Fe0 = c0 ! autotroph grazing losses routed to poc
   auto_graze_poc_Fe1 = c0 ! autotroph grazing losses routed to poc
   auto_graze_poc_Fe2 = c0 ! autotroph grazing losses routed to poc
   auto_graze_poc_Fe3 = c0 ! autotroph grazing losses routed to poc
   auto_graze_poc_Fe4 = c0 ! autotroph grazing losses routed to poc
   auto_graze_poc_Fe5 = c0 ! autotroph grazing losses routed to poc
   auto_graze_poc_Fe6 = c0 ! autotroph grazing losses routed to poc
   auto_graze_poc_Fe7 = c0 ! autotroph grazing losses routed to poc
#endif
#ifndef FE_TAG
   auto_graze_doc = c0 ! autotroph grazing losses routed to doc
#else
   auto_graze_doc = c0     ! autotroph grazing losses routed to doc
   auto_graze_doc_Fe0 = c0 ! autotroph grazing losses routed to doc
   auto_graze_doc_Fe1 = c0 ! autotroph grazing losses routed to doc
   auto_graze_doc_Fe2 = c0 ! autotroph grazing losses routed to doc
   auto_graze_doc_Fe3 = c0 ! autotroph grazing losses routed to doc
   auto_graze_doc_Fe4 = c0 ! autotroph grazing losses routed to doc
   auto_graze_doc_Fe5 = c0 ! autotroph grazing losses routed to doc
   auto_graze_doc_Fe6 = c0 ! autotroph grazing losses routed to doc
   auto_graze_doc_Fe7 = c0 ! autotroph grazing losses routed to doc
#endif
#ifndef FE_TAG
   auto_graze     = c0 ! total grazing losses from autotroph pool at auto_ind
#else
   auto_graze     = c0 ! total grazing losses from autotroph pool at auto_ind
   auto_graze_Fe  = c0 ! total grazing losses of Fe0 from autotroph pool at auto_ind
   auto_graze_Fe0 = c0 ! total grazing losses of Fe0 from autotroph pool at auto_ind
   auto_graze_Fe1 = c0 ! total grazing losses of Fe1 from autotroph pool at auto_ind
   auto_graze_Fe2 = c0 ! total grazing losses of Fe2 from autotroph pool at auto_ind
   auto_graze_Fe3 = c0 ! total grazing losses of Fe3 from autotroph pool at auto_ind
   auto_graze_Fe4 = c0 ! total grazing losses of Fe4 from autotroph pool at auto_ind
   auto_graze_Fe5 = c0 ! total grazing losses of Fe5 from autotroph pool at auto_ind
   auto_graze_Fe6 = c0 ! total grazing losses of Fe6 from autotroph pool at auto_ind
   auto_graze_Fe7 = c0 ! total grazing losses of Fe6 from autotroph pool at auto_ind
#endif
#ifndef FE_TAG
   auto_graze_zoo = c0 ! autotroph grazing losses routed to zooplankton at auto_ind
#else
   auto_graze_zoo = c0 ! autotroph grazing losses routed to zooplankton at auto_ind
   auto_graze_zoo_Fe0 = c0
   auto_graze_zoo_Fe1 = c0
   auto_graze_zoo_Fe2 = c0
   auto_graze_zoo_Fe3 = c0
   auto_graze_zoo_Fe4 = c0
   auto_graze_zoo_Fe5 = c0
   auto_graze_zoo_Fe6 = c0
   auto_graze_zoo_Fe7 = c0
   auto_graze_zoo_tot_Fe0 = c0
   auto_graze_zoo_tot_Fe1 = c0
   auto_graze_zoo_tot_Fe2 = c0
   auto_graze_zoo_tot_Fe3 = c0
   auto_graze_zoo_tot_Fe4 = c0
   auto_graze_zoo_tot_Fe5 = c0
   auto_graze_zoo_tot_Fe6 = c0
   auto_graze_zoo_tot_Fe7 = c0
#endif
#ifndef FE_TAG
   zoo_graze_zoo = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
#else
   zoo_graze_zoo     = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
   zoo_graze_zoo_Fe0 = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
   zoo_graze_zoo_Fe1 = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
   zoo_graze_zoo_Fe2 = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
   zoo_graze_zoo_Fe3 = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
   zoo_graze_zoo_Fe4 = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
   zoo_graze_zoo_Fe5 = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
   zoo_graze_zoo_Fe6 = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
   zoo_graze_zoo_Fe7 = c0 ! zooplankton grazing losses routed to zooplankton at zoo_ind
#endif
#ifndef FE_TAG
   zoo_graze_poc = c0 ! zooplankton grazing losses routed to poc
#else
   zoo_graze_poc     = c0 ! zooplankton grazing losses routed to poc
   zoo_graze_poc_Fe0 = c0 ! zooplankton grazing losses routed to poc
   zoo_graze_poc_Fe1 = c0 ! zooplankton grazing losses routed to poc
   zoo_graze_poc_Fe2 = c0 ! zooplankton grazing losses routed to poc
   zoo_graze_poc_Fe3 = c0 ! zooplankton grazing losses routed to poc
   zoo_graze_poc_Fe4 = c0 ! zooplankton grazing losses routed to poc
   zoo_graze_poc_Fe5 = c0 ! zooplankton grazing losses routed to poc
   zoo_graze_poc_Fe6 = c0 ! zooplankton grazing losses routed to poc
   zoo_graze_poc_Fe7 = c0 ! zooplankton grazing losses routed to poc
#endif

#ifndef FE_TAG
   zoo_graze_dic = c0 ! zooplankton grazing losses routed to dic (computed by residual)
#else
   zoo_graze_dic     = c0 ! zooplankton grazing losses routed to dic (computed by residual)
   zoo_graze_dic_Fe0 = c0 ! zooplankton grazing losses routed to dic (computed by residual)
   zoo_graze_dic_Fe1 = c0 ! zooplankton grazing losses routed to dic (computed by residual)
   zoo_graze_dic_Fe2 = c0 ! zooplankton grazing losses routed to dic (computed by residual)
   zoo_graze_dic_Fe3 = c0 ! zooplankton grazing losses routed to dic (computed by residual)
   zoo_graze_dic_Fe4 = c0 ! zooplankton grazing losses routed to dic (computed by residual)
   zoo_graze_dic_Fe5 = c0 ! zooplankton grazing losses routed to dic (computed by residual)
   zoo_graze_dic_Fe6 = c0 ! zooplankton grazing losses routed to dic (computed by residual)
   zoo_graze_dic_Fe7 = c0 ! zooplankton grazing losses routed to dic (computed by residual)
#endif

#ifndef FE_TAG
   zoo_graze_doc = c0 ! zooplankton grazing losses routed to doc
#else
   zoo_graze_doc     = c0 ! zooplankton grazing losses routed to doc
   zoo_graze_doc_Fe0 = c0 ! zooplankton grazing losses routed to doc
   zoo_graze_doc_Fe1 = c0 ! zooplankton grazing losses routed to doc
   zoo_graze_doc_Fe2 = c0 ! zooplankton grazing losses routed to doc
   zoo_graze_doc_Fe3 = c0 ! zooplankton grazing losses routed to doc
   zoo_graze_doc_Fe4 = c0 ! zooplankton grazing losses routed to doc
   zoo_graze_doc_Fe5 = c0 ! zooplankton grazing losses routed to doc
   zoo_graze_doc_Fe6 = c0 ! zooplankton grazing losses routed to doc
   zoo_graze_doc_Fe7 = c0 ! zooplankton grazing losses routed to doc
#endif

#ifndef FE_TAG
   zoo_graze     = c0 ! total grazing losses from zooplankton pool at zoo_ind
#else
   zoo_graze     = c0 ! total grazing losses from zooplankton pool at zoo_ind
   zoo_graze_Fe  = c0 ! total grazing losses from zooplankton pool at zoo_ind
   zoo_graze_Fe0 = c0 ! total grazing losses from zooplankton pool at zoo_ind
   zoo_graze_Fe1 = c0 ! total grazing losses from zooplankton pool at zoo_ind
   zoo_graze_Fe2 = c0 ! total grazing losses from zooplankton pool at zoo_ind
   zoo_graze_Fe3 = c0 ! total grazing losses from zooplankton pool at zoo_ind
   zoo_graze_Fe4 = c0 ! total grazing losses from zooplankton pool at zoo_ind
   zoo_graze_Fe5 = c0 ! total grazing losses from zooplankton pool at zoo_ind
   zoo_graze_Fe6 = c0 ! total grazing losses from zooplankton pool at zoo_ind
   zoo_graze_Fe7 = c0 ! total grazing losses from zooplankton pool at zoo_ind
#endif

#ifndef FE_TAG
   x_graze_zoo   = c0 ! grazing gains by zooplankton at zoo_ind
#else
   x_graze_zoo     = c0 ! grazing gains by zooplankton at zoo_ind
   x_graze_zoo_Fe  = c0 ! grazing gains by zooplankton at zoo_ind
   x_graze_zoo_Fe0 = c0 ! grazing gains by zooplankton at zoo_ind
   x_graze_zoo_Fe1 = c0 ! grazing gains by zooplankton at zoo_ind
   x_graze_zoo_Fe2 = c0 ! grazing gains by zooplankton at zoo_ind
   x_graze_zoo_Fe3 = c0 ! grazing gains by zooplankton at zoo_ind
   x_graze_zoo_Fe4 = c0 ! grazing gains by zooplankton at zoo_ind
   x_graze_zoo_Fe5 = c0 ! grazing gains by zooplankton at zoo_ind
   x_graze_zoo_Fe6 = c0 ! grazing gains by zooplankton at zoo_ind
   x_graze_zoo_Fe7 = c0 ! grazing gains by zooplankton at zoo_ind
#endif

   do pred_ind = 1, zooplankton_cnt
      WORK3 = c0
      WORK4 = c0

      do prey_ind = 1,grazer_prey_cnt

         !-----------------------------------------------------------------------
         !  compute sum of carbon in the grazee class, both autotrophs and zoop
         !-----------------------------------------------------------------------
         WORK1 = c0 ! biomass in prey class prey_ind
         do auto_ind2 = 1, grazing(prey_ind,pred_ind)%auto_ind_cnt
            auto_ind = grazing(prey_ind,pred_ind)%auto_ind(auto_ind2)
            WORK1 = WORK1 + Pprime(:,:,auto_ind)
         end do

         do zoo_ind2 = 1, grazing(prey_ind,pred_ind)%zoo_ind_cnt
            zoo_ind = grazing(prey_ind,pred_ind)%zoo_ind(zoo_ind2)
            WORK1 = WORK1 + Zprime(:,:,zoo_ind)
         end do

         ! compute grazing rate
         select case (grazing(prey_ind,pred_ind)%grazing_function)

         case (grz_fnc_michaelis_menten)

            where (WORK1 > c0)
               graze_rate = grazing(prey_ind,pred_ind)%z_umax_0 * Tfunc * zooC_loc(:,:,pred_ind) &
                    * ( WORK1 / (WORK1 + grazing(prey_ind,pred_ind)%z_grz) )
            elsewhere
               graze_rate = c0
            end where


         case (grz_fnc_sigmoidal)

            where (WORK1 > c0)
               graze_rate = grazing(prey_ind,pred_ind)%z_umax_0 * Tfunc * zooC_loc(:,:,pred_ind) &
                    * ( WORK1**2 / (WORK1**2 + grazing(prey_ind,pred_ind)%z_grz**2) )
            elsewhere
               graze_rate = c0
            end where

         end select

         !-----------------------------------------------------------------------
         !  autotroph prey
         !-----------------------------------------------------------------------
         do auto_ind2 = 1, grazing(prey_ind,pred_ind)%auto_ind_cnt
            auto_ind = grazing(prey_ind,pred_ind)%auto_ind(auto_ind2)

            ! scale by biomass from autotroph pool
            where (WORK1 > c0)
               WORK2 = (Pprime(:,:,auto_ind) / WORK1) * graze_rate ! total grazing loss from auto_ind
            elsewhere
               WORK2 = c0
            end where
#ifndef FE_TAG
            auto_graze(:,:,auto_ind) = auto_graze(:,:,auto_ind) + WORK2
#else
            auto_graze(:,:,auto_ind) = auto_graze(:,:,auto_ind) + WORK2

            WORK1_Fe = Qfe(:,:,auto_ind) * WORK2
            auto_graze_Fe(:,:,auto_ind) = auto_graze_Fe(:,:,auto_ind) + WORK1_Fe

            s0=autotrophFe0_loc(:,:,auto_ind)
            s1=autotrophFe1_loc(:,:,auto_ind)
            s2=autotrophFe2_loc(:,:,auto_ind)
            s3=autotrophFe3_loc(:,:,auto_ind)
            s4=autotrophFe4_loc(:,:,auto_ind)
            s5=autotrophFe5_loc(:,:,auto_ind)
            s6=autotrophFe6_loc(:,:,auto_ind)
            s7=autotrophFe7_loc(:,:,auto_ind)
            s=s0+s1+s2+s3+s4+s5+s6+s7

            where(s>c0)
               auto_graze_Fe0(:,:,auto_ind) = auto_graze_Fe0(:,:,auto_ind) &
                    + WORK1_Fe * s0/s
               auto_graze_Fe1(:,:,auto_ind) = auto_graze_Fe1(:,:,auto_ind) &
                    + WORK1_Fe * s1/s
               auto_graze_Fe2(:,:,auto_ind) = auto_graze_Fe2(:,:,auto_ind) &
                    + WORK1_Fe * s2/s
               auto_graze_Fe3(:,:,auto_ind) = auto_graze_Fe3(:,:,auto_ind) &
                    + WORK1_Fe * s3/s
               auto_graze_Fe4(:,:,auto_ind) = auto_graze_Fe4(:,:,auto_ind) &
                    + WORK1_Fe * s4/s
               auto_graze_Fe5(:,:,auto_ind) = auto_graze_Fe5(:,:,auto_ind) &
                    + WORK1_Fe * s5/s
               auto_graze_Fe6(:,:,auto_ind) = auto_graze_Fe6(:,:,auto_ind) &
                    + WORK1_Fe * s6/s
               auto_graze_Fe7(:,:,auto_ind) = auto_graze_Fe7(:,:,auto_ind) &
                    + WORK1_Fe * s7/s
            elsewhere
               auto_graze_Fe0(:,:,auto_ind) = c0
               auto_graze_Fe1(:,:,auto_ind) = c0
               auto_graze_Fe2(:,:,auto_ind) = c0
               auto_graze_Fe3(:,:,auto_ind) = c0
               auto_graze_Fe4(:,:,auto_ind) = c0
               auto_graze_Fe5(:,:,auto_ind) = c0
               auto_graze_Fe6(:,:,auto_ind) = c0
               auto_graze_Fe7(:,:,auto_ind) = c0
            end where
#endif

#ifndef FE_TAG
            ! routed to zooplankton
            auto_graze_zoo(:,:,auto_ind) = auto_graze_zoo(:,:,auto_ind) &
                 + grazing(prey_ind,pred_ind)%graze_zoo * WORK2
#else
            ! routed to zooplankton
            auto_graze_zoo(:,:,auto_ind) = auto_graze_zoo(:,:,auto_ind) &
                 + grazing(prey_ind,pred_ind)%graze_zoo * WORK2

            WORK1_Fe =(Qfe(:,:,auto_ind)-Qfe_zoo) &
                 * grazing(prey_ind,pred_ind)%graze_zoo * WORK2
            WORK2_Fe = Qfe(:,:,auto_ind) &
                 * grazing(prey_ind,pred_ind)%graze_zoo * WORK2

            s0 = autotrophFe0_loc(:,:,auto_ind)
            s1 = autotrophFe1_loc(:,:,auto_ind)
            s2 = autotrophFe2_loc(:,:,auto_ind)
            s3 = autotrophFe3_loc(:,:,auto_ind)
            s4 = autotrophFe4_loc(:,:,auto_ind)
            s5 = autotrophFe5_loc(:,:,auto_ind)
            s6 = autotrophFe6_loc(:,:,auto_ind)
            s7 = autotrophFe7_loc(:,:,auto_ind)
            s=s0+s1+s2+s3+s4+s5+s6+s7

            where(s>c0)
               auto_graze_zoo_Fe0(:,:,auto_ind) = auto_graze_zoo_Fe0(:,:,auto_ind) &
                    +WORK1_Fe * s0/s
               auto_graze_zoo_Fe1(:,:,auto_ind) = auto_graze_zoo_Fe1(:,:,auto_ind) &
                    +WORK1_Fe * s1/s
               auto_graze_zoo_Fe2(:,:,auto_ind) = auto_graze_zoo_Fe2(:,:,auto_ind) &
                    +WORK1_Fe * s2/s
               auto_graze_zoo_Fe3(:,:,auto_ind) = auto_graze_zoo_Fe3(:,:,auto_ind) &
                    +WORK1_Fe * s3/s
               auto_graze_zoo_Fe4(:,:,auto_ind) = auto_graze_zoo_Fe4(:,:,auto_ind) &
                    +WORK1_Fe * s4/s
               auto_graze_zoo_Fe5(:,:,auto_ind) = auto_graze_zoo_Fe5(:,:,auto_ind) &
                    +WORK1_Fe * s5/s
               auto_graze_zoo_Fe6(:,:,auto_ind) = auto_graze_zoo_Fe6(:,:,auto_ind) &
                    +WORK1_Fe * s6/s
               auto_graze_zoo_Fe7(:,:,auto_ind) = auto_graze_zoo_Fe7(:,:,auto_ind) &
                    +WORK1_Fe * s7/s

               auto_graze_zoo_tot_Fe0(:,:,auto_ind) = auto_graze_zoo_tot_Fe0(:,:,auto_ind) + WORK2_Fe * s0/s
               auto_graze_zoo_tot_Fe1(:,:,auto_ind) = auto_graze_zoo_tot_Fe1(:,:,auto_ind) + WORK2_Fe * s1/s
               auto_graze_zoo_tot_Fe2(:,:,auto_ind) = auto_graze_zoo_tot_Fe2(:,:,auto_ind) + WORK2_Fe * s2/s
               auto_graze_zoo_tot_Fe3(:,:,auto_ind) = auto_graze_zoo_tot_Fe3(:,:,auto_ind) + WORK2_Fe * s3/s
               auto_graze_zoo_tot_Fe4(:,:,auto_ind) = auto_graze_zoo_tot_Fe4(:,:,auto_ind) + WORK2_Fe * s4/s
               auto_graze_zoo_tot_Fe5(:,:,auto_ind) = auto_graze_zoo_tot_Fe5(:,:,auto_ind) + WORK2_Fe * s5/s
               auto_graze_zoo_tot_Fe6(:,:,auto_ind) = auto_graze_zoo_tot_Fe6(:,:,auto_ind) + WORK2_Fe * s6/s
               auto_graze_zoo_tot_Fe7(:,:,auto_ind) = auto_graze_zoo_tot_Fe7(:,:,auto_ind) + WORK2_Fe * s7/s
            elsewhere
               auto_graze_zoo_Fe0(:,:,auto_ind) = c0
               auto_graze_zoo_Fe1(:,:,auto_ind) = c0
               auto_graze_zoo_Fe2(:,:,auto_ind) = c0
               auto_graze_zoo_Fe3(:,:,auto_ind) = c0
               auto_graze_zoo_Fe4(:,:,auto_ind) = c0
               auto_graze_zoo_Fe5(:,:,auto_ind) = c0
               auto_graze_zoo_Fe6(:,:,auto_ind) = c0
               auto_graze_zoo_Fe7(:,:,auto_ind) = c0

               auto_graze_zoo_tot_Fe0(:,:,auto_ind) = c0
               auto_graze_zoo_tot_Fe1(:,:,auto_ind) = c0
               auto_graze_zoo_tot_Fe2(:,:,auto_ind) = c0
               auto_graze_zoo_tot_Fe3(:,:,auto_ind) = c0
               auto_graze_zoo_tot_Fe4(:,:,auto_ind) = c0
               auto_graze_zoo_tot_Fe5(:,:,auto_ind) = c0
               auto_graze_zoo_tot_Fe6(:,:,auto_ind) = c0
               auto_graze_zoo_tot_Fe7(:,:,auto_ind) = c0
            end where
#endif

#ifndef FE_TAG
            x_graze_zoo(:,:,pred_ind) = x_graze_zoo(:,:,pred_ind) &
                 + grazing(prey_ind,pred_ind)%graze_zoo * WORK2
#else
            x_graze_zoo(:,:,pred_ind) = x_graze_zoo(:,:,pred_ind) &
                 + grazing(prey_ind,pred_ind)%graze_zoo * WORK2

            WORK1_Fe = Qfe_zoo &
                 * grazing(prey_ind,pred_ind)%graze_zoo * WORK2

            x_graze_zoo_Fe(:,:,pred_ind) = x_graze_zoo_Fe(:,:,pred_ind) &
                 + WORK1_Fe

            s0 = autotrophFe0_loc(:,:,auto_ind)
            s1 = autotrophFe1_loc(:,:,auto_ind)
            s2 = autotrophFe2_loc(:,:,auto_ind)
            s3 = autotrophFe3_loc(:,:,auto_ind)
            s4 = autotrophFe4_loc(:,:,auto_ind)
            s5 = autotrophFe5_loc(:,:,auto_ind)
            s6 = autotrophFe6_loc(:,:,auto_ind)
            s7 = autotrophFe7_loc(:,:,auto_ind)
            s=s0+s1+s2+s3+s4+s5+s6+s7

            where(s>c0)
               x_graze_zoo_Fe0(:,:,pred_ind) = x_graze_zoo_Fe0(:,:,pred_ind) &
                    + WORK1_Fe * s0/s
               x_graze_zoo_Fe1(:,:,pred_ind) = x_graze_zoo_Fe1(:,:,pred_ind) &
                    + WORK1_Fe * s1/s
               x_graze_zoo_Fe2(:,:,pred_ind) = x_graze_zoo_Fe2(:,:,pred_ind) &
                    + WORK1_Fe * s2/s
               x_graze_zoo_Fe3(:,:,pred_ind) = x_graze_zoo_Fe3(:,:,pred_ind) &
                    + WORK1_Fe * s3/s
               x_graze_zoo_Fe4(:,:,pred_ind) = x_graze_zoo_Fe4(:,:,pred_ind) &
                    + WORK1_Fe * s4/s
               x_graze_zoo_Fe5(:,:,pred_ind) = x_graze_zoo_Fe5(:,:,pred_ind) &
                    + WORK1_Fe * s5/s
               x_graze_zoo_Fe6(:,:,pred_ind) = x_graze_zoo_Fe6(:,:,pred_ind) &
                    + WORK1_Fe * s6/s
               x_graze_zoo_Fe7(:,:,pred_ind) = x_graze_zoo_Fe7(:,:,pred_ind) &
                    + WORK1_Fe * s7/s
            elsewhere
               x_graze_zoo_Fe0(:,:,pred_ind) = c0
               x_graze_zoo_Fe1(:,:,pred_ind) = c0
               x_graze_zoo_Fe2(:,:,pred_ind) = c0
               x_graze_zoo_Fe3(:,:,pred_ind) = c0
               x_graze_zoo_Fe4(:,:,pred_ind) = c0
               x_graze_zoo_Fe5(:,:,pred_ind) = c0
               x_graze_zoo_Fe6(:,:,pred_ind) = c0
               x_graze_zoo_Fe7(:,:,pred_ind) = c0
            end where
#endif

#ifndef FE_TAG
            ! routed to POC
            if (autotrophs(auto_ind)%imp_calcifier) then
               auto_graze_poc(:,:,auto_ind) = auto_graze_poc(:,:,auto_ind) &
                    + WORK2 * max((caco3_poc_min * QCaCO3(:,:,auto_ind)),  &
                    min(spc_poc_fac * max(1.0_r8,Pprime(:,:,auto_ind)),    &
                    f_graze_sp_poc_lim))
            else
               auto_graze_poc(:,:,auto_ind) = auto_graze_poc(:,:,auto_ind) + grazing(prey_ind,pred_ind)%graze_poc * WORK2
            endif
#else
            ! routed to POC
            if (autotrophs(auto_ind)%imp_calcifier) then
               auto_graze_poc(:,:,auto_ind) = auto_graze_poc(:,:,auto_ind) &
                    + WORK2 * max((caco3_poc_min * QCaCO3(:,:,auto_ind)),  &
                    min(spc_poc_fac * max(1.0_r8,Pprime(:,:,auto_ind)),    &
                    f_graze_sp_poc_lim))
               WORK1_Fe = Qfe(:,:,auto_ind) * (WORK2 * max((caco3_poc_min * QCaCO3(:,:,auto_ind)),min(spc_poc_fac * max(1.0_r8,Pprime(:,:,auto_ind)),f_graze_sp_poc_lim)))
            else
               auto_graze_poc(:,:,auto_ind) = auto_graze_poc(:,:,auto_ind) + grazing(prey_ind,pred_ind)%graze_poc * WORK2
               WORK1_Fe = Qfe(:,:,auto_ind) * grazing(prey_ind,pred_ind)%graze_poc * WORK2
            endif

            s0 = autotrophFe0_loc(:,:,auto_ind)
            s1 = autotrophFe1_loc(:,:,auto_ind)
            s2 = autotrophFe2_loc(:,:,auto_ind)
            s3 = autotrophFe3_loc(:,:,auto_ind)
            s4 = autotrophFe4_loc(:,:,auto_ind)
            s5 = autotrophFe5_loc(:,:,auto_ind)
            s6 = autotrophFe6_loc(:,:,auto_ind)
            s7 = autotrophFe7_loc(:,:,auto_ind)
            s=s0+s1+s2+s3+s4+s5+s6+s7

            where(s>c0)
               auto_graze_poc_Fe0(:,:,auto_ind) = auto_graze_poc_Fe0(:,:,auto_ind) + WORK1_Fe * s0/s
               auto_graze_poc_Fe1(:,:,auto_ind) = auto_graze_poc_Fe1(:,:,auto_ind) + WORK1_Fe * s1/s
               auto_graze_poc_Fe2(:,:,auto_ind) = auto_graze_poc_Fe2(:,:,auto_ind) + WORK1_Fe * s2/s
               auto_graze_poc_Fe3(:,:,auto_ind) = auto_graze_poc_Fe3(:,:,auto_ind) + WORK1_Fe * s3/s
               auto_graze_poc_Fe4(:,:,auto_ind) = auto_graze_poc_Fe4(:,:,auto_ind) + WORK1_Fe * s4/s
               auto_graze_poc_Fe5(:,:,auto_ind) = auto_graze_poc_Fe5(:,:,auto_ind) + WORK1_Fe * s5/s
               auto_graze_poc_Fe6(:,:,auto_ind) = auto_graze_poc_Fe6(:,:,auto_ind) + WORK1_Fe * s6/s
               auto_graze_poc_Fe7(:,:,auto_ind) = auto_graze_poc_Fe7(:,:,auto_ind) + WORK1_Fe * s7/s
            else where
               auto_graze_poc_Fe0(:,:,auto_ind) = c0
               auto_graze_poc_Fe1(:,:,auto_ind) = c0
               auto_graze_poc_Fe2(:,:,auto_ind) = c0
               auto_graze_poc_Fe3(:,:,auto_ind) = c0
               auto_graze_poc_Fe4(:,:,auto_ind) = c0
               auto_graze_poc_Fe5(:,:,auto_ind) = c0
               auto_graze_poc_Fe6(:,:,auto_ind) = c0
               auto_graze_poc_Fe7(:,:,auto_ind) = c0
            endwhere
#endif

            ! routed to DOC
#ifndef FE_TAG
            auto_graze_doc(:,:,auto_ind) = auto_graze_doc(:,:,auto_ind) + grazing(prey_ind,pred_ind)%graze_doc * WORK2
#else
            auto_graze_doc(:,:,auto_ind) = auto_graze_doc(:,:,auto_ind) + grazing(prey_ind,pred_ind)%graze_doc * WORK2

            WORK1_Fe = Qfe(:,:,auto_ind) * grazing(prey_ind,pred_ind)%graze_doc * WORK2

            s0 = autotrophFe0_loc(:,:,auto_ind)
            s1 = autotrophFe1_loc(:,:,auto_ind)
            s2 = autotrophFe2_loc(:,:,auto_ind)
            s3 = autotrophFe3_loc(:,:,auto_ind)
            s4 = autotrophFe4_loc(:,:,auto_ind)
            s5 = autotrophFe5_loc(:,:,auto_ind)
            s6 = autotrophFe6_loc(:,:,auto_ind)
            s7 = autotrophFe7_loc(:,:,auto_ind)
            s=s0+s1+s2+s3+s4+s5+s6+s7

            where(s>c0)
               auto_graze_doc_Fe0(:,:,auto_ind) = auto_graze_doc_Fe0(:,:,auto_ind) + WORK1_Fe * s0/s
               auto_graze_doc_Fe1(:,:,auto_ind) = auto_graze_doc_Fe1(:,:,auto_ind) + WORK1_Fe * s1/s
               auto_graze_doc_Fe2(:,:,auto_ind) = auto_graze_doc_Fe2(:,:,auto_ind) + WORK1_Fe * s2/s
               auto_graze_doc_Fe3(:,:,auto_ind) = auto_graze_doc_Fe3(:,:,auto_ind) + WORK1_Fe * s3/s
               auto_graze_doc_Fe4(:,:,auto_ind) = auto_graze_doc_Fe4(:,:,auto_ind) + WORK1_Fe * s4/s
               auto_graze_doc_Fe5(:,:,auto_ind) = auto_graze_doc_Fe5(:,:,auto_ind) + WORK1_Fe * s5/s
               auto_graze_doc_Fe6(:,:,auto_ind) = auto_graze_doc_Fe6(:,:,auto_ind) + WORK1_Fe * s6/s
               auto_graze_doc_Fe7(:,:,auto_ind) = auto_graze_doc_Fe6(:,:,auto_ind) + WORK1_Fe * s7/s
            else where
               auto_graze_doc_Fe0(:,:,auto_ind) = c0
               auto_graze_doc_Fe1(:,:,auto_ind) = c0
               auto_graze_doc_Fe2(:,:,auto_ind) = c0
               auto_graze_doc_Fe3(:,:,auto_ind) = c0
               auto_graze_doc_Fe4(:,:,auto_ind) = c0
               auto_graze_doc_Fe5(:,:,auto_ind) = c0
               auto_graze_doc_Fe6(:,:,auto_ind) = c0
               auto_graze_doc_Fe7(:,:,auto_ind) = c0
            endwhere
#endif

            !  get fractional factor for routing of zoo losses, based on food supply
            WORK3 = WORK3 + grazing(prey_ind,pred_ind)%f_zoo_detr * (WORK2 + epsC * epsTinv)
            WORK4 = WORK4 + (WORK2 + epsC * epsTinv)

         end do

         !-----------------------------------------------------------------------
         !  Zooplankton prey
         !-----------------------------------------------------------------------
         do zoo_ind2 = 1, grazing(prey_ind,pred_ind)%zoo_ind_cnt
            zoo_ind = grazing(prey_ind,pred_ind)%zoo_ind(zoo_ind2)

            ! scale by biomass from zooplankton pool
            where (WORK1 > c0)
               WORK2 = (Zprime(:,:,zoo_ind) / WORK1) * graze_rate
            elsewhere
               WORK2 = c0
            end where

#ifndef FE_TAG
            ! grazing loss from zooplankton prey pool
            zoo_graze(:,:,zoo_ind) = zoo_graze(:,:,zoo_ind) + WORK2
#else
            ! grazing loss from zooplankton prey pool
            zoo_graze(:,:,zoo_ind) = zoo_graze(:,:,zoo_ind) + WORK2

            WORK1_Fe = Qfe_zoo * WORK2

            zoo_graze_Fe(:,:,zoo_ind) = zoo_graze_Fe(:,:,zoo_ind) &
                 + WORK1_Fe

            s0 = zooFe0_loc(:,:,zoo_ind)
            s1 = zooFe1_loc(:,:,zoo_ind)
            s2 = zooFe2_loc(:,:,zoo_ind)
            s3 = zooFe3_loc(:,:,zoo_ind)
            s4 = zooFe4_loc(:,:,zoo_ind)
            s5 = zooFe5_loc(:,:,zoo_ind)
            s6 = zooFe6_loc(:,:,zoo_ind)
            s7 = zooFe7_loc(:,:,zoo_ind)
            s=s0+s1+s2+s3+s4+s5+s6+s7

            where(s>c0)
               zoo_graze_Fe0(:,:,pred_ind) = zoo_graze_Fe0(:,:,pred_ind) &
                    + WORK1_Fe * s0/s
               zoo_graze_Fe1(:,:,pred_ind) = zoo_graze_Fe1(:,:,pred_ind) &
                    + WORK1_Fe * s1/s
               zoo_graze_Fe2(:,:,pred_ind) = zoo_graze_Fe2(:,:,pred_ind) &
                    + WORK1_Fe * s2/s
               zoo_graze_Fe3(:,:,pred_ind) = zoo_graze_Fe3(:,:,pred_ind) &
                    + WORK1_Fe * s3/s
               zoo_graze_Fe4(:,:,pred_ind) = zoo_graze_Fe4(:,:,pred_ind) &
                    + WORK1_Fe * s4/s
               zoo_graze_Fe5(:,:,pred_ind) = zoo_graze_Fe5(:,:,pred_ind) &
                    + WORK1_Fe * s5/s
               zoo_graze_Fe6(:,:,pred_ind) = zoo_graze_Fe6(:,:,pred_ind) &
                    + WORK1_Fe * s6/s
               zoo_graze_Fe7(:,:,pred_ind) = zoo_graze_Fe7(:,:,pred_ind) &
                    + WORK1_Fe * s7/s
            else where
               zoo_graze_Fe0(:,:,pred_ind) = c0
               zoo_graze_Fe1(:,:,pred_ind) = c0
               zoo_graze_Fe2(:,:,pred_ind) = c0
               zoo_graze_Fe3(:,:,pred_ind) = c0
               zoo_graze_Fe4(:,:,pred_ind) = c0
               zoo_graze_Fe5(:,:,pred_ind) = c0
               zoo_graze_Fe6(:,:,pred_ind) = c0
               zoo_graze_Fe7(:,:,pred_ind) = c0
            endwhere
#endif

#ifndef FE_TAG
            ! routed to zooplankton
            zoo_graze_zoo(:,:,zoo_ind) = zoo_graze_zoo(:,:,zoo_ind) &
                 + grazing(prey_ind,pred_ind)%graze_zoo * WORK2
#else
            ! routed to zooplankton
            zoo_graze_zoo(:,:,zoo_ind) = zoo_graze_zoo(:,:,zoo_ind) &
                 + grazing(prey_ind,pred_ind)%graze_zoo * WORK2

            WORK1_Fe = Qfe_zoo &
                 * grazing(prey_ind,pred_ind)%graze_zoo * WORK2

            s0 = zooFe0_loc(:,:,zoo_ind)
            s1 = zooFe1_loc(:,:,zoo_ind)
            s2 = zooFe2_loc(:,:,zoo_ind)
            s3 = zooFe3_loc(:,:,zoo_ind)
            s4 = zooFe4_loc(:,:,zoo_ind)
            s5 = zooFe5_loc(:,:,zoo_ind)
            s6 = zooFe6_loc(:,:,zoo_ind)
            s7 = zooFe7_loc(:,:,zoo_ind)
            s=s0+s1+s2+s3+s4+s5+s6+s7

            where(s>c0)
               zoo_graze_zoo_Fe0(:,:,zoo_ind) = zoo_graze_zoo_Fe0(:,:,zoo_ind) + WORK1_Fe * s0/s
               zoo_graze_zoo_Fe1(:,:,zoo_ind) = zoo_graze_zoo_Fe1(:,:,zoo_ind) + WORK1_Fe * s1/s
               zoo_graze_zoo_Fe2(:,:,zoo_ind) = zoo_graze_zoo_Fe2(:,:,zoo_ind) + WORK1_Fe * s2/s
               zoo_graze_zoo_Fe3(:,:,zoo_ind) = zoo_graze_zoo_Fe3(:,:,zoo_ind) + WORK1_Fe * s3/s
               zoo_graze_zoo_Fe4(:,:,zoo_ind) = zoo_graze_zoo_Fe4(:,:,zoo_ind) + WORK1_Fe * s4/s
               zoo_graze_zoo_Fe5(:,:,zoo_ind) = zoo_graze_zoo_Fe5(:,:,zoo_ind) + WORK1_Fe * s5/s
               zoo_graze_zoo_Fe6(:,:,zoo_ind) = zoo_graze_zoo_Fe6(:,:,zoo_ind) + WORK1_Fe * s6/s
               zoo_graze_zoo_Fe7(:,:,zoo_ind) = zoo_graze_zoo_Fe7(:,:,zoo_ind) + WORK1_Fe * s7/s
            else where
               zoo_graze_zoo_Fe0(:,:,zoo_ind) = c0
               zoo_graze_zoo_Fe1(:,:,zoo_ind) = c0
               zoo_graze_zoo_Fe2(:,:,zoo_ind) = c0
               zoo_graze_zoo_Fe3(:,:,zoo_ind) = c0
               zoo_graze_zoo_Fe4(:,:,zoo_ind) = c0
               zoo_graze_zoo_Fe5(:,:,zoo_ind) = c0
               zoo_graze_zoo_Fe6(:,:,zoo_ind) = c0
               zoo_graze_zoo_Fe7(:,:,zoo_ind) = c0
            endwhere
#endif

#ifndef FE_TAG
            x_graze_zoo(:,:,pred_ind) = x_graze_zoo(:,:,pred_ind) &
                 + grazing(prey_ind,pred_ind)%graze_zoo * WORK2
#else
            x_graze_zoo(:,:,pred_ind) = x_graze_zoo(:,:,pred_ind) &
                 + grazing(prey_ind,pred_ind)%graze_zoo * WORK2

            WORK1_Fe = Qfe_zoo &
                 * grazing(prey_ind,pred_ind)%graze_zoo * WORK2

            x_graze_zoo_Fe(:,:,pred_ind) = x_graze_zoo_Fe(:,:,pred_ind) &
                 + WORK1_Fe

            s0 = zooFe0_loc(:,:,zoo_ind)
            s1 = zooFe1_loc(:,:,zoo_ind)
            s2 = zooFe2_loc(:,:,zoo_ind)
            s3 = zooFe3_loc(:,:,zoo_ind)
            s4 = zooFe4_loc(:,:,zoo_ind)
            s5 = zooFe5_loc(:,:,zoo_ind)
            s6 = zooFe6_loc(:,:,zoo_ind)
            s7 = zooFe7_loc(:,:,zoo_ind)
            s=s0+s1+s2+s3+s4+s5+s6+s7

            where(s>c0)
               x_graze_zoo_Fe0(:,:,pred_ind) = x_graze_zoo_Fe0(:,:,pred_ind) &
                    + WORK1_Fe * s0/s
               x_graze_zoo_Fe1(:,:,pred_ind) = x_graze_zoo_Fe1(:,:,pred_ind) &
                    + WORK1_Fe * s1/s
               x_graze_zoo_Fe2(:,:,pred_ind) = x_graze_zoo_Fe2(:,:,pred_ind) &
                    + WORK1_Fe * s2/s
               x_graze_zoo_Fe3(:,:,pred_ind) = x_graze_zoo_Fe3(:,:,pred_ind) &
                    + WORK1_Fe * s3/s
               x_graze_zoo_Fe4(:,:,pred_ind) = x_graze_zoo_Fe4(:,:,pred_ind) &
                    + WORK1_Fe * s4/s
               x_graze_zoo_Fe5(:,:,pred_ind) = x_graze_zoo_Fe5(:,:,pred_ind) &
                    + WORK1_Fe * s5/s
               x_graze_zoo_Fe6(:,:,pred_ind) = x_graze_zoo_Fe6(:,:,pred_ind) &
                    + WORK1_Fe * s6/s
               x_graze_zoo_Fe7(:,:,pred_ind) = x_graze_zoo_Fe7(:,:,pred_ind) &
                    + WORK1_Fe * s7/s
            else where
               x_graze_zoo_Fe0(:,:,pred_ind) = c0
               x_graze_zoo_Fe1(:,:,pred_ind) = c0
               x_graze_zoo_Fe2(:,:,pred_ind) = c0
               x_graze_zoo_Fe3(:,:,pred_ind) = c0
               x_graze_zoo_Fe4(:,:,pred_ind) = c0
               x_graze_zoo_Fe5(:,:,pred_ind) = c0
               x_graze_zoo_Fe6(:,:,pred_ind) = c0
               x_graze_zoo_Fe7(:,:,pred_ind) = c0
            endwhere
#endif

#ifndef FE_TAG
            ! routed to POC/DOC
            zoo_graze_poc(:,:,zoo_ind) = zoo_graze_poc(:,:,zoo_ind) + grazing(prey_ind,pred_ind)%graze_poc * WORK2
#else
            zoo_graze_poc(:,:,zoo_ind) = zoo_graze_poc(:,:,zoo_ind) + grazing(prey_ind,pred_ind)%graze_poc * WORK2

            WORK1_Fe = Qfe_zoo * grazing(prey_ind,pred_ind)%graze_poc * WORK2

            s0 = zooFe0_loc(:,:,zoo_ind)
            s1 = zooFe1_loc(:,:,zoo_ind)
            s2 = zooFe2_loc(:,:,zoo_ind)
            s3 = zooFe3_loc(:,:,zoo_ind)
            s4 = zooFe4_loc(:,:,zoo_ind)
            s5 = zooFe5_loc(:,:,zoo_ind)
            s6 = zooFe6_loc(:,:,zoo_ind)
            s7 = zooFe7_loc(:,:,zoo_ind)
            s=s0+s1+s2+s3+s4+s5+s6+s7

            where(s>c0)
               zoo_graze_poc_Fe0(:,:,zoo_ind) = zoo_graze_poc_Fe0(:,:,zoo_ind) + WORK1_Fe * s0/s
               zoo_graze_poc_Fe1(:,:,zoo_ind) = zoo_graze_poc_Fe1(:,:,zoo_ind) + WORK1_Fe * s1/s
               zoo_graze_poc_Fe2(:,:,zoo_ind) = zoo_graze_poc_Fe2(:,:,zoo_ind) + WORK1_Fe * s2/s
               zoo_graze_poc_Fe3(:,:,zoo_ind) = zoo_graze_poc_Fe3(:,:,zoo_ind) + WORK1_Fe * s3/s
               zoo_graze_poc_Fe4(:,:,zoo_ind) = zoo_graze_poc_Fe4(:,:,zoo_ind) + WORK1_Fe * s4/s
               zoo_graze_poc_Fe5(:,:,zoo_ind) = zoo_graze_poc_Fe5(:,:,zoo_ind) + WORK1_Fe * s5/s
               zoo_graze_poc_Fe6(:,:,zoo_ind) = zoo_graze_poc_Fe6(:,:,zoo_ind) + WORK1_Fe * s6/s
               zoo_graze_poc_Fe7(:,:,zoo_ind) = zoo_graze_poc_Fe7(:,:,zoo_ind) + WORK1_Fe * s7/s
            else where
               zoo_graze_poc_Fe0(:,:,zoo_ind) = c0
               zoo_graze_poc_Fe1(:,:,zoo_ind) = c0
               zoo_graze_poc_Fe2(:,:,zoo_ind) = c0
               zoo_graze_poc_Fe3(:,:,zoo_ind) = c0
               zoo_graze_poc_Fe4(:,:,zoo_ind) = c0
               zoo_graze_poc_Fe5(:,:,zoo_ind) = c0
               zoo_graze_poc_Fe6(:,:,zoo_ind) = c0
               zoo_graze_poc_Fe7(:,:,zoo_ind) = c0
            endwhere
#endif
#ifndef FE_TAG
            zoo_graze_doc(:,:,zoo_ind) = zoo_graze_doc(:,:,zoo_ind) + grazing(prey_ind,pred_ind)%graze_doc * WORK2
#else
            zoo_graze_doc(:,:,zoo_ind) = zoo_graze_doc(:,:,zoo_ind) + grazing(prey_ind,pred_ind)%graze_doc * WORK2

            WORK1_Fe = Qfe_zoo * grazing(prey_ind,pred_ind)%graze_doc * WORK2

            s0 = zooFe0_loc(:,:,zoo_ind)
            s1 = zooFe1_loc(:,:,zoo_ind)
            s2 = zooFe2_loc(:,:,zoo_ind)
            s3 = zooFe3_loc(:,:,zoo_ind)
            s4 = zooFe4_loc(:,:,zoo_ind)
            s5 = zooFe5_loc(:,:,zoo_ind)
            s6 = zooFe6_loc(:,:,zoo_ind)
            s7 = zooFe7_loc(:,:,zoo_ind)
            s=s0+s1+s2+s3+s4+s5+s6+s7

            where(s>c0)
               zoo_graze_doc_Fe0(:,:,zoo_ind) = zoo_graze_doc_Fe0(:,:,zoo_ind) &
                    + WORK1_Fe * s0/s
               zoo_graze_doc_Fe1(:,:,zoo_ind) = zoo_graze_doc_Fe1(:,:,zoo_ind) &
                    + WORK1_Fe * s1/s
               zoo_graze_doc_Fe2(:,:,zoo_ind) = zoo_graze_doc_Fe2(:,:,zoo_ind) &
                    + WORK1_Fe * s2/s
               zoo_graze_doc_Fe3(:,:,zoo_ind) = zoo_graze_doc_Fe3(:,:,zoo_ind) &
                    + WORK1_Fe * s3/s
               zoo_graze_doc_Fe4(:,:,zoo_ind) = zoo_graze_doc_Fe4(:,:,zoo_ind) &
                    + WORK1_Fe * s4/s
               zoo_graze_doc_Fe5(:,:,zoo_ind) = zoo_graze_doc_Fe5(:,:,zoo_ind) &
                    + WORK1_Fe * s5/s
               zoo_graze_doc_Fe6(:,:,zoo_ind) = zoo_graze_doc_Fe6(:,:,zoo_ind) &
                    + WORK1_Fe * s6/s
               zoo_graze_doc_Fe7(:,:,zoo_ind) = zoo_graze_doc_Fe7(:,:,zoo_ind) &
                    + WORK1_Fe * s7/s
            else where
               zoo_graze_doc_Fe0(:,:,zoo_ind) = c0
               zoo_graze_doc_Fe1(:,:,zoo_ind) = c0
               zoo_graze_doc_Fe2(:,:,zoo_ind) = c0
               zoo_graze_doc_Fe3(:,:,zoo_ind) = c0
               zoo_graze_doc_Fe4(:,:,zoo_ind) = c0
               zoo_graze_doc_Fe5(:,:,zoo_ind) = c0
               zoo_graze_doc_Fe6(:,:,zoo_ind) = c0
               zoo_graze_doc_Fe7(:,:,zoo_ind) = c0
            endwhere
#endif
            !  get fractional factor for routing of zoo losses, based on food supply
            WORK3 = WORK3 + grazing(prey_ind,pred_ind)%f_zoo_detr * (WORK2 + epsC * epsTinv)
            WORK4 = WORK4 + (WORK2 + epsC * epsTinv)

         end do
      end do

      f_zoo_detr(:,:,pred_ind) = WORK3 / WORK4
   end do

!-----------------------------------------------------------------------
! compute routing to dic of grazed material
!-----------------------------------------------------------------------
#ifndef FE_TAG
   do auto_ind = 1, autotroph_cnt
      auto_graze_dic(:,:,auto_ind) = auto_graze(:,:,auto_ind)  &
           - (auto_graze_zoo(:,:,auto_ind) + auto_graze_poc(:,:,auto_ind) + auto_graze_doc(:,:,auto_ind))
   end do
#else
   do auto_ind = 1, autotroph_cnt
      auto_graze_dic(:,:,auto_ind) = auto_graze(:,:,auto_ind)  &
           - (auto_graze_zoo(:,:,auto_ind) + auto_graze_poc(:,:,auto_ind) + auto_graze_doc(:,:,auto_ind))
   end do

   do auto_ind = 1, autotroph_cnt
      auto_graze_dic_Fe0(:,:,auto_ind) = auto_graze_Fe0(:,:,auto_ind)  &
           - (auto_graze_zoo_tot_Fe0(:,:,auto_ind) + auto_graze_poc_Fe0(:,:,auto_ind) + auto_graze_doc_Fe0(:,:,auto_ind))
      auto_graze_dic_Fe1(:,:,auto_ind) = auto_graze_Fe1(:,:,auto_ind)  &
           - (auto_graze_zoo_tot_Fe1(:,:,auto_ind) + auto_graze_poc_Fe1(:,:,auto_ind) + auto_graze_doc_Fe1(:,:,auto_ind))
      auto_graze_dic_Fe2(:,:,auto_ind) = auto_graze_Fe2(:,:,auto_ind)  &
           - (auto_graze_zoo_tot_Fe2(:,:,auto_ind) + auto_graze_poc_Fe2(:,:,auto_ind) + auto_graze_doc_Fe2(:,:,auto_ind))
      auto_graze_dic_Fe3(:,:,auto_ind) = auto_graze_Fe3(:,:,auto_ind)  &
           - (auto_graze_zoo_tot_Fe3(:,:,auto_ind) + auto_graze_poc_Fe3(:,:,auto_ind) + auto_graze_doc_Fe3(:,:,auto_ind))
      auto_graze_dic_Fe4(:,:,auto_ind) = auto_graze_Fe4(:,:,auto_ind)  &
           - (auto_graze_zoo_tot_Fe4(:,:,auto_ind) + auto_graze_poc_Fe4(:,:,auto_ind) + auto_graze_doc_Fe4(:,:,auto_ind))
      auto_graze_dic_Fe5(:,:,auto_ind) = auto_graze_Fe5(:,:,auto_ind)  &
           - (auto_graze_zoo_tot_Fe5(:,:,auto_ind) + auto_graze_poc_Fe5(:,:,auto_ind) + auto_graze_doc_Fe5(:,:,auto_ind))
      auto_graze_dic_Fe6(:,:,auto_ind) = auto_graze_Fe6(:,:,auto_ind)  &
           - (auto_graze_zoo_tot_Fe6(:,:,auto_ind) + auto_graze_poc_Fe6(:,:,auto_ind) + auto_graze_doc_Fe6(:,:,auto_ind))
      auto_graze_dic_Fe7(:,:,auto_ind) = auto_graze_Fe7(:,:,auto_ind)  &
           - (auto_graze_zoo_tot_Fe7(:,:,auto_ind) + auto_graze_poc_Fe7(:,:,auto_ind) + auto_graze_doc_Fe7(:,:,auto_ind))
   end do
#endif

#ifndef FE_TAG
   do zoo_ind = 1, zooplankton_cnt
      zoo_graze_dic(:,:,zoo_ind) = zoo_graze(:,:,zoo_ind)  &
           - (zoo_graze_zoo(:,:,zoo_ind) + zoo_graze_poc(:,:,zoo_ind) + zoo_graze_doc(:,:,zoo_ind))
   end do
#else
   do zoo_ind = 1, zooplankton_cnt
      zoo_graze_dic(:,:,zoo_ind) = zoo_graze(:,:,zoo_ind)  &
           - (zoo_graze_zoo(:,:,zoo_ind) + zoo_graze_poc(:,:,zoo_ind) + zoo_graze_doc(:,:,zoo_ind))
   end do
   do zoo_ind = 1, zooplankton_cnt
      zoo_graze_dic_Fe0(:,:,zoo_ind) = zoo_graze_Fe0(:,:,zoo_ind)  &
           - (zoo_graze_zoo_Fe0(:,:,zoo_ind) + zoo_graze_poc_Fe0(:,:,zoo_ind) + zoo_graze_doc_Fe0(:,:,zoo_ind))
      zoo_graze_dic_Fe1(:,:,zoo_ind) = zoo_graze_Fe1(:,:,zoo_ind)  &
           - (zoo_graze_zoo_Fe1(:,:,zoo_ind) + zoo_graze_poc_Fe1(:,:,zoo_ind) + zoo_graze_doc_Fe1(:,:,zoo_ind))
      zoo_graze_dic_Fe2(:,:,zoo_ind) = zoo_graze_Fe2(:,:,zoo_ind)  &
           - (zoo_graze_zoo_Fe2(:,:,zoo_ind) + zoo_graze_poc_Fe2(:,:,zoo_ind) + zoo_graze_doc_Fe2(:,:,zoo_ind))
      zoo_graze_dic_Fe3(:,:,zoo_ind) = zoo_graze_Fe3(:,:,zoo_ind)  &
           - (zoo_graze_zoo_Fe3(:,:,zoo_ind) + zoo_graze_poc_Fe3(:,:,zoo_ind) + zoo_graze_doc_Fe3(:,:,zoo_ind))
      zoo_graze_dic_Fe4(:,:,zoo_ind) = zoo_graze_Fe4(:,:,zoo_ind)  &
           - (zoo_graze_zoo_Fe4(:,:,zoo_ind) + zoo_graze_poc_Fe4(:,:,zoo_ind) + zoo_graze_doc_Fe4(:,:,zoo_ind))
      zoo_graze_dic_Fe5(:,:,zoo_ind) = zoo_graze_Fe5(:,:,zoo_ind)  &
           - (zoo_graze_zoo_Fe5(:,:,zoo_ind) + zoo_graze_poc_Fe5(:,:,zoo_ind) + zoo_graze_doc_Fe5(:,:,zoo_ind))
      zoo_graze_dic_Fe6(:,:,zoo_ind) = zoo_graze_Fe6(:,:,zoo_ind)  &
           - (zoo_graze_zoo_Fe6(:,:,zoo_ind) + zoo_graze_poc_Fe6(:,:,zoo_ind) + zoo_graze_doc_Fe6(:,:,zoo_ind))
      zoo_graze_dic_Fe7(:,:,zoo_ind) = zoo_graze_Fe7(:,:,zoo_ind)  &
           - (zoo_graze_zoo_Fe7(:,:,zoo_ind) + zoo_graze_poc_Fe7(:,:,zoo_ind) + zoo_graze_doc_Fe7(:,:,zoo_ind))
   end do
#endif

!-----------------------------------------------------------------------
! compute zooplankton loss routing
!-----------------------------------------------------------------------
   do zoo_ind = 1, zooplankton_cnt
#ifndef FE_TAG
      zoo_loss_poc(:,:,zoo_ind) = f_zoo_detr(:,:,zoo_ind) * zoo_loss(:,:,zoo_ind)
#else
      zoo_loss_poc(:,:,zoo_ind) = f_zoo_detr(:,:,zoo_ind) * zoo_loss(:,:,zoo_ind)

      zoo_loss_poc_Fe0(:,:,zoo_ind) = f_zoo_detr(:,:,zoo_ind) * zoo_loss_Fe0(:,:,zoo_ind)
      zoo_loss_poc_Fe1(:,:,zoo_ind) = f_zoo_detr(:,:,zoo_ind) * zoo_loss_Fe1(:,:,zoo_ind)
      zoo_loss_poc_Fe2(:,:,zoo_ind) = f_zoo_detr(:,:,zoo_ind) * zoo_loss_Fe2(:,:,zoo_ind)
      zoo_loss_poc_Fe3(:,:,zoo_ind) = f_zoo_detr(:,:,zoo_ind) * zoo_loss_Fe3(:,:,zoo_ind)
      zoo_loss_poc_Fe4(:,:,zoo_ind) = f_zoo_detr(:,:,zoo_ind) * zoo_loss_Fe4(:,:,zoo_ind)
      zoo_loss_poc_Fe5(:,:,zoo_ind) = f_zoo_detr(:,:,zoo_ind) * zoo_loss_Fe5(:,:,zoo_ind)
      zoo_loss_poc_Fe6(:,:,zoo_ind) = f_zoo_detr(:,:,zoo_ind) * zoo_loss_Fe6(:,:,zoo_ind)
      zoo_loss_poc_Fe7(:,:,zoo_ind) = f_zoo_detr(:,:,zoo_ind) * zoo_loss_Fe7(:,:,zoo_ind)
#endif

#ifndef FE_TAG
      zoo_loss_doc(:,:,zoo_ind) = (c1 - parm_labile_ratio) * (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss(:,:,zoo_ind)
#else
      zoo_loss_doc(:,:,zoo_ind) = (c1 - parm_labile_ratio) * (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss(:,:,zoo_ind)

      zoo_loss_doc_Fe0(:,:,zoo_ind) = (c1 - parm_labile_ratio) * &
          (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe0(:,:,zoo_ind)
      zoo_loss_doc_Fe1(:,:,zoo_ind) = (c1 - parm_labile_ratio) * &
          (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe1(:,:,zoo_ind)
      zoo_loss_doc_Fe2(:,:,zoo_ind) = (c1 - parm_labile_ratio) * &
          (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe2(:,:,zoo_ind)
      zoo_loss_doc_Fe3(:,:,zoo_ind) = (c1 - parm_labile_ratio) * &
          (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe3(:,:,zoo_ind)
      zoo_loss_doc_Fe4(:,:,zoo_ind) = (c1 - parm_labile_ratio) * &
          (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe4(:,:,zoo_ind)
      zoo_loss_doc_Fe5(:,:,zoo_ind) = (c1 - parm_labile_ratio) * &
          (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe5(:,:,zoo_ind)
      zoo_loss_doc_Fe6(:,:,zoo_ind) = (c1 - parm_labile_ratio) * &
          (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe6(:,:,zoo_ind)
      zoo_loss_doc_Fe7(:,:,zoo_ind) = (c1 - parm_labile_ratio) * &
          (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe7(:,:,zoo_ind)
#endif
#ifndef FE_TAG
      zoo_loss_dic(:,:,zoo_ind) = parm_labile_ratio * (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss(:,:,zoo_ind)
#else
      zoo_loss_dic(:,:,zoo_ind) = parm_labile_ratio * (c1 - f_zoo_detr(:,:,zoo_ind)) * zoo_loss(:,:,zoo_ind)

      zoo_loss_dic_Fe0(:,:,zoo_ind) = parm_labile_ratio * (c1-f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe0(:,:,zoo_ind)
      zoo_loss_dic_Fe1(:,:,zoo_ind) = parm_labile_ratio * (c1-f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe1(:,:,zoo_ind)
      zoo_loss_dic_Fe2(:,:,zoo_ind) = parm_labile_ratio * (c1-f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe2(:,:,zoo_ind)
      zoo_loss_dic_Fe3(:,:,zoo_ind) = parm_labile_ratio * (c1-f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe3(:,:,zoo_ind)
      zoo_loss_dic_Fe4(:,:,zoo_ind) = parm_labile_ratio * (c1-f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe4(:,:,zoo_ind)
      zoo_loss_dic_Fe5(:,:,zoo_ind) = parm_labile_ratio * (c1-f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe5(:,:,zoo_ind)
      zoo_loss_dic_Fe6(:,:,zoo_ind) = parm_labile_ratio * (c1-f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe6(:,:,zoo_ind)
      zoo_loss_dic_Fe7(:,:,zoo_ind) = parm_labile_ratio * (c1-f_zoo_detr(:,:,zoo_ind)) * zoo_loss_Fe7(:,:,zoo_ind)
#endif
   end do

!-----------------------------------------------------------------------
! P from some autotrophs w/ Qp different from Qp_zoo_pom must be routed differently than other
! elements to ensure that sinking detritus and zooplankton pools get their fixed P/C ratios.
! The remaining P is split evenly between DOP and PO4.
!-----------------------------------------------------------------------
   do auto_ind = 1, autotroph_cnt
      if (autotrophs(auto_ind)%Qp /= Qp_zoo_pom) then
         remaining_P = ((auto_graze(:,:,auto_ind) + auto_loss(:,:,auto_ind) + auto_agg(:,:,auto_ind)) * autotrophs(auto_ind)%Qp) &
                       - ((auto_graze_zoo(:,:,auto_ind)) * Qp_zoo_pom) &
                       - ((auto_graze_poc(:,:,auto_ind) + auto_loss_poc(:,:,auto_ind) + auto_agg(:,:,auto_ind)) * Qp_zoo_pom)
         remaining_P_dop(:,:,auto_ind) = (c1 - parm_labile_ratio) * remaining_P
         remaining_P_dip(:,:,auto_ind) = parm_labile_ratio * remaining_P
      endif
   end do

!-----------------------------------------------------------------------
!  compute terms for DOM
!-----------------------------------------------------------------------

   DOC_prod = sum(zoo_loss_doc, dim=3) + sum(auto_loss_doc, dim=3) + sum(auto_graze_doc, dim=3) + sum(zoo_graze_doc, dim=3)
   DON_prod = Q * DOC_prod
   DOP_prod = Qp_zoo_pom * ( sum(zoo_loss_doc, dim=3) + sum(zoo_graze_doc, dim=3) )
   do auto_ind = 1, autotroph_cnt
      if (autotrophs(auto_ind)%Qp == Qp_zoo_pom) then
         DOP_prod = DOP_prod + autotrophs(auto_ind)%Qp * (auto_loss_doc(:,:,auto_ind) + auto_graze_doc(:,:,auto_ind))
      else
         DOP_prod = DOP_prod + remaining_P_dop(:,:,auto_ind)
      endif
   end do


#ifndef FE_TAG
   DOFe_prod = Qfe_zoo * ( sum(zoo_loss_doc, dim=3) + sum(zoo_graze_doc, dim=3) )
#else
   DOFe0_prod = sum(zoo_loss_doc_Fe0, dim=3) + sum(zoo_graze_doc_Fe0, dim=3)
   DOFe1_prod = sum(zoo_loss_doc_Fe1, dim=3) + sum(zoo_graze_doc_Fe1, dim=3)
   DOFe2_prod = sum(zoo_loss_doc_Fe2, dim=3) + sum(zoo_graze_doc_Fe2, dim=3)
   DOFe3_prod = sum(zoo_loss_doc_Fe3, dim=3) + sum(zoo_graze_doc_Fe3, dim=3)
   DOFe4_prod = sum(zoo_loss_doc_Fe4, dim=3) + sum(zoo_graze_doc_Fe4, dim=3)
   DOFe5_prod = sum(zoo_loss_doc_Fe5, dim=3) + sum(zoo_graze_doc_Fe5, dim=3)
   DOFe6_prod = sum(zoo_loss_doc_Fe6, dim=3) + sum(zoo_graze_doc_Fe6, dim=3)
   DOFe7_prod = sum(zoo_loss_doc_Fe7, dim=3) + sum(zoo_graze_doc_Fe7, dim=3)
#endif

#ifndef FE_TAG
   do auto_ind = 1, autotroph_cnt
      DOFe_prod = DOFe_prod + Qfe(:,:,auto_ind) * (auto_loss_doc(:,:,auto_ind) + auto_graze_doc(:,:,auto_ind))
   end do
#else
   do auto_ind = 1, autotroph_cnt
      DOFe0_prod = DOFe0_prod + (auto_loss_doc_Fe0(:,:,auto_ind) + auto_graze_doc_Fe0(:,:,auto_ind))
      DOFe1_prod = DOFe1_prod + (auto_loss_doc_Fe1(:,:,auto_ind) + auto_graze_doc_Fe1(:,:,auto_ind))
      DOFe2_prod = DOFe2_prod + (auto_loss_doc_Fe2(:,:,auto_ind) + auto_graze_doc_Fe2(:,:,auto_ind))
      DOFe3_prod = DOFe3_prod + (auto_loss_doc_Fe3(:,:,auto_ind) + auto_graze_doc_Fe3(:,:,auto_ind))
      DOFe4_prod = DOFe4_prod + (auto_loss_doc_Fe4(:,:,auto_ind) + auto_graze_doc_Fe4(:,:,auto_ind))
      DOFe5_prod = DOFe5_prod + (auto_loss_doc_Fe5(:,:,auto_ind) + auto_graze_doc_Fe5(:,:,auto_ind))
      DOFe6_prod = DOFe6_prod + (auto_loss_doc_Fe6(:,:,auto_ind) + auto_graze_doc_Fe6(:,:,auto_ind))
      DOFe7_prod = DOFe7_prod + (auto_loss_doc_Fe7(:,:,auto_ind) + auto_graze_doc_Fe7(:,:,auto_ind))
   end do
#endif

   DOC_remin  = DOC_loc  * DOC_reminR
   DON_remin  = DON_loc  * DON_reminR
   DOP_remin  = DOP_loc  * DOP_reminR
   DOFe_remin = DOFe_loc * DOFe_reminR
!-----------------------------------------------------------------------
!  Refractory remin rate due to photochemistry
!  below euphotic zone remin rate sharply decrease
!-----------------------------------------------------------------------

   where (PAR_avg > 1.0_r8)
      DONr_remin = DONr_loc * DONr_reminR
      DOPr_remin = DOPr_loc * DOPr_reminR
   elsewhere
      DONr_remin = DONr_loc * (c1/(365.0_r8*670.0_r8)) * dps  ! 1/670 yrs
      DOPr_remin = DOPr_loc * (c1/(365.0_r8*460.0_r8)) * dps  ! 1/460 yrs
      DOC_remin = DOC_remin * 0.0685_r8
      DON_remin = DON_remin * 0.1_r8
      DOFe_remin = DOFe_remin * 0.05_r8
      DOP_remin = DOP_remin * 0.05_r8
   end where

#ifdef FE_TAG
   s0=DOFe0_loc
   s1=DOFe1_loc
   s2=DOFe2_loc
   s3=DOFe3_loc
   s4=DOFe4_loc
   s5=DOFe5_loc
   s6=DOFe6_loc
   s7=DOFe7_loc
   s=s0+s1+s2+s3+s4+s5+s6+s7

   where(s>0)
      DOFe0_remin = DOFe_remin * s0/s
      DOFe1_remin = DOFe_remin * s1/s
      DOFe2_remin = DOFe_remin * s2/s
      DOFe3_remin = DOFe_remin * s3/s
      DOFe4_remin = DOFe_remin * s4/s
      DOFe5_remin = DOFe_remin * s5/s
      DOFe6_remin = DOFe_remin * s6/s
      DOFe7_remin = DOFe_remin * s7/s
   elsewhere
      DOFe0_remin = c0
      DOFe1_remin = c0
      DOFe2_remin = c0
      DOFe3_remin = c0
      DOFe4_remin = c0
      DOFe5_remin = c0
      DOFe6_remin = c0
      DOFe7_remin = c0
   endwhere
#endif

!-----------------------------------------------------------------------
!  large detritus C
!-----------------------------------------------------------------------

   POC%prod(:,:,bid) = sum(zoo_loss_poc, dim=3) &
        + sum(auto_graze_poc, dim=3) + sum(zoo_graze_poc, dim=3) &
        + sum(auto_agg, dim=3) + sum(auto_loss_poc, dim=3)

!-----------------------------------------------------------------------
!  large detrital CaCO3
!  33% of CaCO3 is remin when phyto are grazed
!-----------------------------------------------------------------------

   do auto_ind = 1, autotroph_cnt
      if (autotrophs(auto_ind)%CaCO3_ind > 0) then
         P_CaCO3%prod(:,:,bid) = ((c1 - f_graze_CaCO3_REMIN) * auto_graze(:,:,auto_ind) + &
                                  auto_loss(:,:,auto_ind) + auto_agg(:,:,auto_ind)) * QCaCO3(:,:,auto_ind)
      endif
   end do

!-----------------------------------------------------------------------
!  large detritus SiO2
!  grazed diatom SiO2, 60% is remineralized
!-----------------------------------------------------------------------

   do auto_ind = 1, autotroph_cnt
      if (autotrophs(auto_ind)%Si_ind > 0) then
         P_SiO2%prod(:,:,bid) = Qsi(:,:,auto_ind) &
            * ((c1 - f_graze_si_remin) * auto_graze(:,:,auto_ind) + auto_agg(:,:,auto_ind) &
               + autotrophs(auto_ind)%loss_poc * auto_loss(:,:,auto_ind))
      endif
   end do

   dust%prod(:,:,bid) = c0

!-----------------------------------------------------------------------
!  Compute iron scavenging :
!  1) compute in terms of loss per year per unit iron (%/year/fe)
!  2) scale by sinking POMx10 + Dust + bSi + CaCO3 flux
!  3) increase scavenging at higher iron (>0.6nM)
!  4) convert to net loss per second
!-----------------------------------------------------------------------

   Fe_scavenge_rate = parm_Fe_scavenge_rate0

   Fe_scavenge_rate = Fe_scavenge_rate * &
      ((POC%sflux_out(:,:,bid) + POC%hflux_out(:,:,bid)) * 120.1_r8 + &
       (P_CaCO3%sflux_out(:,:,bid) + P_CaCO3%hflux_out(:,:,bid)) * P_CaCO3%mass + &
       (P_SiO2%sflux_out(:,:,bid) + P_SiO2%hflux_out(:,:,bid)) * P_SiO2%mass + &
       (dust%sflux_out(:,:,bid) + dust%hflux_out(:,:,bid)) * dust_fescav_scale)

#ifdef LIGAND3D
   where (Fe_loc > lig3d) &
      Fe_scavenge_rate = Fe_scavenge_rate + &
                         (Fe_loc - lig3d) * fe_max_scale2
#else
   where (Fe_loc > Fe_scavenge_thres1) &
      Fe_scavenge_rate = Fe_scavenge_rate + &
                         (Fe_loc - Fe_scavenge_thres1) * fe_max_scale2
#endif

#ifndef FE_TAG
   Fe_scavenge = yps * Fe_loc * Fe_scavenge_rate
#else
   Fe_scavenge = yps * Fe_loc * Fe_scavenge_rate

   s0 = Fe0_loc
   s1 = Fe1_loc
   s2 = Fe2_loc
   s3 = Fe3_loc
   s4 = Fe4_loc
   s5 = Fe5_loc
   s6 = Fe6_loc
   s7 = Fe7_loc
   s = s0+s1+s2+s3+s4+s5+s6+s7

   where (s>c0)
      Fe0_scavenge = Fe_scavenge * s0/s
      Fe1_scavenge = Fe_scavenge * s1/s
      Fe2_scavenge = Fe_scavenge * s2/s
      Fe3_scavenge = Fe_scavenge * s3/s
      Fe4_scavenge = Fe_scavenge * s4/s
      Fe5_scavenge = Fe_scavenge * s5/s
      Fe6_scavenge = Fe_scavenge * s6/s
      Fe7_scavenge = Fe_scavenge * s7/s
   elsewhere
      Fe0_scavenge = c0
      Fe1_scavenge = c0
      Fe2_scavenge = c0
      Fe3_scavenge = c0
      Fe4_scavenge = c0
      Fe5_scavenge = c0
      Fe6_scavenge = c0
      Fe7_scavenge = c0
   endwhere

#endif

#ifndef FE_TAG
   P_iron%prod(:,:,bid) = ( sum(zoo_loss_poc, dim=3) + sum(zoo_graze_poc, dim=3) ) * Qfe_zoo + Fe_scavenge
#else
   P_iron%prod(:,:,bid) = ( sum(zoo_loss_poc, dim=3) + sum(zoo_graze_poc, dim=3) ) * Qfe_zoo + Fe_scavenge

   P_iron0%prod(:,:,bid) = sum(zoo_loss_poc_Fe0, dim=3) + sum(zoo_graze_poc_Fe0, dim=3) + Fe0_scavenge
   P_iron1%prod(:,:,bid) = sum(zoo_loss_poc_Fe1, dim=3) + sum(zoo_graze_poc_Fe1, dim=3) + Fe1_scavenge
   P_iron2%prod(:,:,bid) = sum(zoo_loss_poc_Fe2, dim=3) + sum(zoo_graze_poc_Fe2, dim=3) + Fe2_scavenge
   P_iron3%prod(:,:,bid) = sum(zoo_loss_poc_Fe3, dim=3) + sum(zoo_graze_poc_Fe3, dim=3) + Fe3_scavenge
   P_iron4%prod(:,:,bid) = sum(zoo_loss_poc_Fe4, dim=3) + sum(zoo_graze_poc_Fe4, dim=3) + Fe4_scavenge
   P_iron5%prod(:,:,bid) = sum(zoo_loss_poc_Fe5, dim=3) + sum(zoo_graze_poc_Fe5, dim=3) + Fe5_scavenge
   P_iron6%prod(:,:,bid) = sum(zoo_loss_poc_Fe6, dim=3) + sum(zoo_graze_poc_Fe6, dim=3) + Fe6_scavenge
   P_iron7%prod(:,:,bid) = sum(zoo_loss_poc_Fe7, dim=3) + sum(zoo_graze_poc_Fe7, dim=3) + Fe7_scavenge
#endif
#ifndef FE_TAG
   do auto_ind = 1, autotroph_cnt
      P_iron%prod(:,:,bid) = P_iron%prod(:,:,bid) &
         + Qfe(:,:,auto_ind) * (auto_agg(:,:,auto_ind) + auto_graze_poc(:,:,auto_ind) + auto_loss_poc(:,:,auto_ind))
   end do
# ifdef DIAGNOSTICS_BIO
   aFe_pgen(:,:)=-(P_iron%prod(:,:,bid) - Fe_scavenge)
# endif
#else
   do auto_ind = 1, autotroph_cnt
      P_iron%prod(:,:,bid) = P_iron%prod(:,:,bid) &
         + Qfe(:,:,auto_ind) * (auto_agg(:,:,auto_ind) + auto_graze_poc(:,:,auto_ind) + auto_loss_poc(:,:,auto_ind))

      P_iron0%prod(:,:,bid) = P_iron0%prod(:,:,bid) + auto_agg_Fe0(:,:,auto_ind) + auto_graze_poc_Fe0(:,:,auto_ind) + auto_loss_poc_Fe0(:,:,auto_ind)
      P_iron1%prod(:,:,bid) = P_iron1%prod(:,:,bid) + auto_agg_Fe1(:,:,auto_ind) + auto_graze_poc_Fe1(:,:,auto_ind) + auto_loss_poc_Fe1(:,:,auto_ind)
      P_iron2%prod(:,:,bid) = P_iron2%prod(:,:,bid) + auto_agg_Fe2(:,:,auto_ind) + auto_graze_poc_Fe2(:,:,auto_ind) + auto_loss_poc_Fe2(:,:,auto_ind)
      P_iron3%prod(:,:,bid) = P_iron3%prod(:,:,bid) + auto_agg_Fe3(:,:,auto_ind) + auto_graze_poc_Fe3(:,:,auto_ind) + auto_loss_poc_Fe3(:,:,auto_ind)
      P_iron4%prod(:,:,bid) = P_iron4%prod(:,:,bid) + auto_agg_Fe4(:,:,auto_ind) + auto_graze_poc_Fe4(:,:,auto_ind) + auto_loss_poc_Fe4(:,:,auto_ind)
      P_iron5%prod(:,:,bid) = P_iron5%prod(:,:,bid) + auto_agg_Fe5(:,:,auto_ind) + auto_graze_poc_Fe5(:,:,auto_ind) + auto_loss_poc_Fe5(:,:,auto_ind)
      P_iron6%prod(:,:,bid) = P_iron6%prod(:,:,bid) + auto_agg_Fe6(:,:,auto_ind) + auto_graze_poc_Fe6(:,:,auto_ind) + auto_loss_poc_Fe6(:,:,auto_ind)
      P_iron7%prod(:,:,bid) = P_iron7%prod(:,:,bid) + auto_agg_Fe7(:,:,auto_ind) + auto_graze_poc_Fe7(:,:,auto_ind) + auto_loss_poc_Fe7(:,:,auto_ind)
   end do
# ifdef DIAGNOSTICS_BIO
   aFe0_pgen(:,:)=-(P_iron0%prod(:,:,bid) - Fe0_scavenge)
   aFe1_pgen(:,:)=-(P_iron1%prod(:,:,bid) - Fe1_scavenge)
   aFe2_pgen(:,:)=-(P_iron2%prod(:,:,bid) - Fe2_scavenge)
   aFe3_pgen(:,:)=-(P_iron3%prod(:,:,bid) - Fe3_scavenge)
   aFe4_pgen(:,:)=-(P_iron4%prod(:,:,bid) - Fe4_scavenge)
   aFe5_pgen(:,:)=-(P_iron5%prod(:,:,bid) - Fe5_scavenge)
   aFe6_pgen(:,:)=-(P_iron6%prod(:,:,bid) - Fe6_scavenge)
   aFe7_pgen(:,:)=-(P_iron7%prod(:,:,bid) - Fe7_scavenge)
# endif
#endif

#ifndef FE_TAG
# ifdef BEC
   call compute_particulate_terms(k, &
                                  QA_dust_def, TEMP, O2_loc, NO3_loc, &
                                  SED_DENITRIF, OTHER_REMIN, lexport_shared_vars)
# else
   call compute_particulate_terms(k, &
                                  QA_dust_def, TEMP, O2_loc, NO3_loc, &
                                  SED_DENITRIF, OTHER_REMIN, lexport_shared_vars,&
                                  this_block)
# endif
#else
# ifdef BEC

   wgt(:,:,1) = Fe0_loc
   wgt(:,:,2) = Fe1_loc
   wgt(:,:,3) = Fe2_loc
   wgt(:,:,4) = Fe3_loc
   wgt(:,:,5) = Fe4_loc
   wgt(:,:,6) = Fe5_loc
   wgt(:,:,7) = Fe6_loc
   wgt(:,:,8) = Fe7_loc

   call compute_particulate_terms(k, &
                                  QA_dust_def, TEMP, O2_loc, NO3_loc, &
                                  SED_DENITRIF, OTHER_REMIN, lexport_shared_vars)
# else
   call compute_particulate_terms(k, &
                                  QA_dust_def, TEMP, O2_loc, NO3_loc, &
                                  SED_DENITRIF, OTHER_REMIN, lexport_shared_vars,&
                                  this_block)
# endif
#endif

!-----------------------------------------------------------------------
!  nitrate & ammonium
!  nitrification in low light
!  use exponential decay of PAR across model level to compute taper factor
!-----------------------------------------------------------------------

   where (PAR_out(:,:,bid) < parm_nitrif_par_lim)
      NITRIF = parm_kappa_nitrif * NH4_loc
      where (PAR_in > parm_nitrif_par_lim)
         NITRIF = NITRIF * log(PAR_out(:,:,bid) / parm_nitrif_par_lim) / (-KPARdz)
      end where
   elsewhere
      NITRIF = c0
   end where

!c   call accumulate_tavg_field(NITRIF, tavg_NITRIF,bid,k)

!-----------------------------------------------------------------------
!  Compute denitrification under low O2 conditions
!-----------------------------------------------------------------------

   WORK1 = ((parm_o2_min + parm_o2_min_delta) - O2_loc) / parm_o2_min_delta
   WORK1 = min(max(WORK1,c0),c1)

   WORK1 = merge(c0, WORK1, NO3_loc == c0)

   DENITRIF = WORK1 * ((DOC_remin + POC%remin(:,:,bid) - OTHER_REMIN(:,:,bid)) / denitrif_C_N &
                       - SED_DENITRIF(:,:,bid))

!c   call accumulate_tavg_field(DENITRIF, tavg_DENITRIF,bid,k)

!-----------------------------------------------------------------------
!  nitrate & ammonium
!-----------------------------------------------------------------------

   DTRACER_MODULE(:,:,no3_ind) = restore_local(:, :, no3_ind) + NITRIF - DENITRIF - SED_DENITRIF(:,:,bid) - sum(NO3_V, dim=3)

   DTRACER_MODULE(:,:,nh4_ind) = -sum(NH4_V, dim=3) - NITRIF + DON_remin + DONr_remin &
      + Q * (                                                                         &
      sum(zoo_loss_dic, dim=3) + sum(zoo_graze_dic, dim=3)                            &
      + sum(auto_loss_dic, dim=3) + sum(auto_graze_dic, dim=3)                        &
      + POC%remin(:,:,bid) * (c1 - DONrefract) )

   do auto_ind = 1, autotroph_cnt
      if (autotrophs(auto_ind)%Nfixer) &
         DTRACER_MODULE(:,:,nh4_ind) = DTRACER_MODULE(:,:,nh4_ind) + Nexcrete(:,:,auto_ind)
   end do

!-----------------------------------------------------------------------
!  dissolved iron
!-----------------------------------------------------------------------


#ifndef FE_TAG
     DTRACER_MODULE(:,:,fe_ind) = P_iron%remin(:,:,bid) &
       + (Qfe_zoo * ( sum(zoo_loss_dic, dim=3) + sum(zoo_graze_dic, dim=3) )) &
       + DOFe_remin - sum(photoFe, dim=3) - Fe_scavenge
#else
     DTRACER_MODULE(:,:,fe0_ind) = P_iron0%remin(:,:,bid) &
       + sum(zoo_loss_dic_Fe0, dim=3) + sum(zoo_graze_dic_Fe0, dim=3) &
       + DOFe0_remin - sum(photoFe0, dim=3) - Fe0_scavenge
     DTRACER_MODULE(:,:,fe1_ind) = P_iron1%remin(:,:,bid) &
       + sum(zoo_loss_dic_Fe1, dim=3) + sum(zoo_graze_dic_Fe1, dim=3) &
       + DOFe1_remin - sum(photoFe1, dim=3) - Fe1_scavenge
     DTRACER_MODULE(:,:,fe2_ind) = P_iron2%remin(:,:,bid) &
       + sum(zoo_loss_dic_Fe2, dim=3) + sum(zoo_graze_dic_Fe2, dim=3) &
       + DOFe2_remin - sum(photoFe2, dim=3) - Fe2_scavenge
     DTRACER_MODULE(:,:,fe3_ind) = P_iron3%remin(:,:,bid) &
       + sum(zoo_loss_dic_Fe3, dim=3) + sum(zoo_graze_dic_Fe3, dim=3) &
       + DOFe3_remin - sum(photoFe3, dim=3) - Fe3_scavenge
     DTRACER_MODULE(:,:,fe4_ind) = P_iron4%remin(:,:,bid) &
       + sum(zoo_loss_dic_Fe4, dim=3) + sum(zoo_graze_dic_Fe4, dim=3) &
       + DOFe4_remin - sum(photoFe4, dim=3) - Fe4_scavenge
     DTRACER_MODULE(:,:,fe5_ind) = P_iron5%remin(:,:,bid) &
       + sum(zoo_loss_dic_Fe5, dim=3) + sum(zoo_graze_dic_Fe5, dim=3) &
       + DOFe5_remin - sum(photoFe5, dim=3) - Fe5_scavenge
     DTRACER_MODULE(:,:,fe6_ind) = P_iron6%remin(:,:,bid) &
       + sum(zoo_loss_dic_Fe6, dim=3) + sum(zoo_graze_dic_Fe6, dim=3) &
       + DOFe6_remin - sum(photoFe6, dim=3) - Fe6_scavenge
     DTRACER_MODULE(:,:,fe7_ind) = P_iron7%remin(:,:,bid) &
       + sum(zoo_loss_dic_Fe7, dim=3) + sum(zoo_graze_dic_Fe7, dim=3) &
       + DOFe7_remin - sum(photoFe7, dim=3) - Fe7_scavenge
#endif
#ifndef FE_TAG
     do auto_ind = 1, autotroph_cnt
        DTRACER_MODULE(:,:,fe_ind) = DTRACER_MODULE(:,:,fe_ind) &
           + (Qfe(:,:,auto_ind) * (auto_loss_dic(:,:,auto_ind) + auto_graze_dic(:,:,auto_ind))) &
           + auto_graze_zoo(:,:,auto_ind) * (Qfe(:,:,auto_ind)-Qfe_zoo)
     end do
#else
     do auto_ind = 1, autotroph_cnt
        DTRACER_MODULE(:,:,fe0_ind) = DTRACER_MODULE(:,:,fe0_ind) &
           + auto_loss_dic_Fe0(:,:,auto_ind) + auto_graze_dic_Fe0(:,:,auto_ind) &
           + auto_graze_zoo_Fe0(:,:,auto_ind)
        DTRACER_MODULE(:,:,fe1_ind) = DTRACER_MODULE(:,:,fe1_ind) &
           + auto_loss_dic_Fe1(:,:,auto_ind) + auto_graze_dic_Fe1(:,:,auto_ind) &
           + auto_graze_zoo_Fe1(:,:,auto_ind)
        DTRACER_MODULE(:,:,fe2_ind) = DTRACER_MODULE(:,:,fe2_ind) &
           + auto_loss_dic_Fe2(:,:,auto_ind) + auto_graze_dic_Fe2(:,:,auto_ind) &
           + auto_graze_zoo_Fe2(:,:,auto_ind)
        DTRACER_MODULE(:,:,fe3_ind) = DTRACER_MODULE(:,:,fe3_ind) &
           + auto_loss_dic_Fe3(:,:,auto_ind) + auto_graze_dic_Fe3(:,:,auto_ind) &
           + auto_graze_zoo_Fe3(:,:,auto_ind)
        DTRACER_MODULE(:,:,fe4_ind) = DTRACER_MODULE(:,:,fe4_ind) &
           + auto_loss_dic_Fe4(:,:,auto_ind) + auto_graze_dic_Fe4(:,:,auto_ind) &
           + auto_graze_zoo_Fe4(:,:,auto_ind)
        DTRACER_MODULE(:,:,fe5_ind) = DTRACER_MODULE(:,:,fe5_ind) &
           + auto_loss_dic_Fe5(:,:,auto_ind) + auto_graze_dic_Fe5(:,:,auto_ind) &
           + auto_graze_zoo_Fe5(:,:,auto_ind)
        DTRACER_MODULE(:,:,fe6_ind) = DTRACER_MODULE(:,:,fe6_ind) &
           + auto_loss_dic_Fe6(:,:,auto_ind) + auto_graze_dic_Fe6(:,:,auto_ind) &
           + auto_graze_zoo_Fe6(:,:,auto_ind)
        DTRACER_MODULE(:,:,fe7_ind) = DTRACER_MODULE(:,:,fe7_ind) &
           + auto_loss_dic_Fe7(:,:,auto_ind) + auto_graze_dic_Fe7(:,:,auto_ind) &
           + auto_graze_zoo_Fe7(:,:,auto_ind)
     end do
#endif

#ifdef DIAGNOSTICS_BIO
# ifndef FE_TAG
     aFe_brate = DTRACER_MODULE(:,:,fe_ind)
     aFe_scav  = -Fe_scavenge
     aFe_hbio  = aFe_brate-(aFe_scav+aFe_disag)-(aFe_pgen+aFe_premin)
# else
     aFe0_brate = DTRACER_MODULE(:,:,fe0_ind)
     aFe0_scav  = -Fe0_scavenge
     aFe0_hbio  = aFe0_brate-(aFe0_scav+aFe0_disag)-(aFe0_pgen+aFe0_premin)

     aFe1_brate = DTRACER_MODULE(:,:,Fe1_ind)
     aFe1_scav  = -Fe1_scavenge
     aFe1_hbio  = aFe1_brate-(aFe1_scav+aFe1_disag)-(aFe1_pgen+aFe1_premin)

     aFe2_brate = DTRACER_MODULE(:,:,Fe2_ind)
     aFe2_scav  = -Fe2_scavenge
     aFe2_hbio  = aFe2_brate-(aFe2_scav+aFe2_disag)-(aFe2_pgen+aFe2_premin)

     aFe3_brate = DTRACER_MODULE(:,:,fe3_ind)
     aFe3_scav  = -Fe3_scavenge
     aFe3_hbio  = aFe3_brate-(aFe3_scav+aFe3_disag)-(aFe3_pgen+aFe3_premin)

     aFe4_brate = DTRACER_MODULE(:,:,fe4_ind)
     aFe4_scav  = -Fe4_scavenge
     aFe4_hbio  = aFe4_brate-(aFe4_scav+aFe4_disag)-(aFe4_pgen+aFe4_premin)

     aFe5_brate = DTRACER_MODULE(:,:,fe5_ind)
     aFe5_scav  = -Fe5_scavenge
     aFe5_hbio  = aFe5_brate-(aFe5_scav+aFe5_disag)-(aFe5_pgen+aFe5_premin)

     aFe6_brate = DTRACER_MODULE(:,:,fe6_ind)
     aFe6_scav  = -Fe6_scavenge
     aFe6_hbio  = aFe6_brate-(aFe6_scav+aFe6_disag)-(aFe6_pgen+aFe6_premin)

     aFe7_brate = DTRACER_MODULE(:,:,fe7_ind)
     aFe7_scav  = -Fe7_scavenge
     aFe7_hbio  = aFe7_brate-(aFe7_scav+aFe7_disag)-(aFe7_pgen+aFe7_premin)
# endif
#endif

!-----------------------------------------------------------------------
!  dissolved SiO3
!-----------------------------------------------------------------------

   DTRACER_MODULE(:,:,sio3_ind) = restore_local(:, :, sio3_ind) + P_SiO2%remin(:,:,bid)

   do auto_ind = 1, autotroph_cnt
      if (autotrophs(auto_ind)%Si_ind > 0) then
         DTRACER_MODULE(:,:,sio3_ind) = DTRACER_MODULE(:,:,sio3_ind) - photoSi(:,:,auto_ind) &
            + Qsi(:,:,auto_ind) * (f_graze_si_remin * auto_graze(:,:,auto_ind) &
                                   + (c1 - autotrophs(auto_ind)%loss_poc) * auto_loss(:,:,auto_ind))
      endif
   end do

!-----------------------------------------------------------------------
!  phosphate
!-----------------------------------------------------------------------

   DTRACER_MODULE(:,:,po4_ind) = restore_local(:, :, po4_ind) + DOP_remin + DOPr_remin - sum(PO4_V, dim=3) &
      + Qp_zoo_pom * ( &
      (c1 - DOPrefract) * POC%remin(:,:,bid) &
      + sum(zoo_loss_dic, dim=3) + sum(zoo_graze_dic, dim=3) )

   do auto_ind = 1, autotroph_cnt
      if (autotrophs(auto_ind)%Qp == Qp_zoo_pom) then
         DTRACER_MODULE(:,:,po4_ind) = DTRACER_MODULE(:,:,po4_ind) &
            + autotrophs(auto_ind)%Qp * (auto_loss_dic(:,:,auto_ind) + auto_graze_dic(:,:,auto_ind))
      else
         DTRACER_MODULE(:,:,po4_ind) = DTRACER_MODULE(:,:,po4_ind) + remaining_P_dip(:,:,auto_ind)
      endif
   end do


!-----------------------------------------------------------------------
!  zoo Carbon
!-----------------------------------------------------------------------
   do zoo_ind = 1,zooplankton_cnt
#ifndef FE_TAG
      n = zooplankton(zoo_ind)%C_ind
      DTRACER_MODULE(:,:,n) = x_graze_zoo(:,:,zoo_ind) - zoo_graze(:,:,zoo_ind) - zoo_loss(:,:,zoo_ind)
#else
      n = zooplankton(zoo_ind)%C_ind
      DTRACER_MODULE(:,:,n) = x_graze_zoo(:,:,zoo_ind) - zoo_graze(:,:,zoo_ind) - zoo_loss(:,:,zoo_ind)

      n = zooplankton(zoo_ind)%Fe0_ind
      DTRACER_MODULE(:,:,n) = x_graze_zoo_Fe0(:,:,zoo_ind) &
                            - zoo_graze_Fe0(:,:,zoo_ind) &
                            - zoo_loss_Fe0(:,:,zoo_ind)

      n = zooplankton(zoo_ind)%Fe1_ind
      DTRACER_MODULE(:,:,n) = x_graze_zoo_Fe1(:,:,zoo_ind) &
                            - zoo_graze_Fe1(:,:,zoo_ind) &
                            - zoo_loss_Fe1(:,:,zoo_ind)

      n = zooplankton(zoo_ind)%Fe2_ind
      DTRACER_MODULE(:,:,n) = x_graze_zoo_Fe2(:,:,zoo_ind) &
                            - zoo_graze_Fe2(:,:,zoo_ind) &
                            - zoo_loss_Fe2(:,:,zoo_ind)

      n = zooplankton(zoo_ind)%Fe3_ind
      DTRACER_MODULE(:,:,n) = x_graze_zoo_Fe3(:,:,zoo_ind) &
                            - zoo_graze_Fe3(:,:,zoo_ind) &
                            - zoo_loss_Fe3(:,:,zoo_ind)

      n = zooplankton(zoo_ind)%Fe4_ind
      DTRACER_MODULE(:,:,n) = x_graze_zoo_Fe4(:,:,zoo_ind) &
                            - zoo_graze_Fe4(:,:,zoo_ind) &
                            - zoo_loss_Fe4(:,:,zoo_ind)

      n = zooplankton(zoo_ind)%Fe5_ind
      DTRACER_MODULE(:,:,n) = x_graze_zoo_Fe5(:,:,zoo_ind) &
                            - zoo_graze_Fe5(:,:,zoo_ind) &
                            - zoo_loss_Fe5(:,:,zoo_ind)

      n = zooplankton(zoo_ind)%Fe6_ind
      DTRACER_MODULE(:,:,n) = x_graze_zoo_Fe6(:,:,zoo_ind) &
                            - zoo_graze_Fe6(:,:,zoo_ind) &
                            - zoo_loss_Fe6(:,:,zoo_ind)

      n = zooplankton(zoo_ind)%Fe7_ind
      DTRACER_MODULE(:,:,n) = x_graze_zoo_Fe7(:,:,zoo_ind) &
                            - zoo_graze_Fe7(:,:,zoo_ind) &
                            - zoo_loss_Fe7(:,:,zoo_ind)
#endif
   end do

!-----------------------------------------------------------------------
!  autotroph Carbon
!  autotroph Chlorophyll
!  autotroph Fe
!  autotroph Si
!  autotroph CaCO3
!-----------------------------------------------------------------------

   do auto_ind = 1, autotroph_cnt
      WORK1 = auto_graze(:,:,auto_ind) + auto_loss(:,:,auto_ind) + auto_agg(:,:,auto_ind)

      n = autotrophs(auto_ind)%C_ind
      DTRACER_MODULE(:,:,n) = photoC(:,:,auto_ind) - WORK1

      n = autotrophs(auto_ind)%Chl_ind
      DTRACER_MODULE(:,:,n) = photoacc(:,:,auto_ind) - thetaC(:,:,auto_ind) * WORK1

      n = autotrophs(auto_ind)%Si_ind
      if (n > 0) then
         DTRACER_MODULE(:,:,n) =  photoSi(:,:,auto_ind) - Qsi(:,:,auto_ind) * WORK1
      endif

      n = autotrophs(auto_ind)%CaCO3_ind
      if (n > 0) then
         DTRACER_MODULE(:,:,n) = CaCO3_PROD(:,:,auto_ind) - QCaCO3(:,:,auto_ind) * WORK1
      endif

#ifndef FE_TAG
      n = autotrophs(auto_ind)%Fe_ind
      DTRACER_MODULE(:,:,n) =  photoFe(:,:,auto_ind) - Qfe(:,:,auto_ind) * WORK1
#else
      WORK1 = auto_graze_Fe0(:,:,auto_ind) + auto_loss_Fe0(:,:,auto_ind) &
            + auto_agg_Fe0(:,:,auto_ind)
      n = autotrophs(auto_ind)%Fe0_ind
      DTRACER_MODULE(:,:,n) =  photoFe0(:,:,auto_ind) - WORK1

      WORK1 = auto_graze_Fe1(:,:,auto_ind) + auto_loss_Fe1(:,:,auto_ind) &
            + auto_agg_Fe1(:,:,auto_ind)
      n = autotrophs(auto_ind)%Fe1_ind
      DTRACER_MODULE(:,:,n) =  photoFe1(:,:,auto_ind) - WORK1

      WORK1 = auto_graze_Fe2(:,:,auto_ind) + auto_loss_Fe2(:,:,auto_ind) &
            + auto_agg_Fe2(:,:,auto_ind)
      n = autotrophs(auto_ind)%Fe2_ind
      DTRACER_MODULE(:,:,n) =  photoFe2(:,:,auto_ind) - WORK1

      WORK1 = auto_graze_Fe3(:,:,auto_ind) + auto_loss_Fe3(:,:,auto_ind) &
            + auto_agg_Fe3(:,:,auto_ind)
      n = autotrophs(auto_ind)%Fe3_ind
      DTRACER_MODULE(:,:,n) =  photoFe3(:,:,auto_ind) - WORK1

      WORK1 = auto_graze_Fe4(:,:,auto_ind) + auto_loss_Fe4(:,:,auto_ind) &
            + auto_agg_Fe4(:,:,auto_ind)
      n = autotrophs(auto_ind)%Fe4_ind
      DTRACER_MODULE(:,:,n) =  photoFe4(:,:,auto_ind) - WORK1

      WORK1 = auto_graze_Fe5(:,:,auto_ind) + auto_loss_Fe5(:,:,auto_ind) &
            + auto_agg_Fe5(:,:,auto_ind)
      n = autotrophs(auto_ind)%Fe5_ind
      DTRACER_MODULE(:,:,n) =  photoFe5(:,:,auto_ind) - WORK1

      WORK1 = auto_graze_Fe6(:,:,auto_ind) + auto_loss_Fe6(:,:,auto_ind) &
            + auto_agg_Fe6(:,:,auto_ind)
      n = autotrophs(auto_ind)%Fe6_ind
      DTRACER_MODULE(:,:,n) =  photoFe6(:,:,auto_ind) - WORK1

      WORK1 = auto_graze_Fe7(:,:,auto_ind) + auto_loss_Fe7(:,:,auto_ind) &
            + auto_agg_Fe7(:,:,auto_ind)
      n = autotrophs(auto_ind)%Fe7_ind
      DTRACER_MODULE(:,:,n) =  photoFe7(:,:,auto_ind) - WORK1
#endif
   end do

!-----------------------------------------------------------------------
!  dissolved organic Matter
!  from sinking remin small fraction to refractory pool
!-----------------------------------------------------------------------

   DTRACER_MODULE(:,:,doc_ind) = DOC_prod - DOC_remin

   DTRACER_MODULE(:,:,don_ind) = (DON_prod * (c1 - DONrefract)) - DON_remin

   DTRACER_MODULE(:,:,donr_ind) = (DON_prod * DONrefract) - DONr_remin &
      + (POC%remin(:,:,bid) * DONrefract * Q)

   DTRACER_MODULE(:,:,dop_ind) = (DOP_prod * (c1 - DOPrefract)) - DOP_remin &
      - sum(DOP_V, dim=3)

   DTRACER_MODULE(:,:,dopr_ind) = (DOP_prod * DOPrefract) - DOPr_remin &
      + (POC%remin(:,:,bid) * DOPrefract * Qp_zoo_pom)

#ifndef FE_TAG
   DTRACER_MODULE(:,:,dofe_ind) = DOFe_prod - DOFe_remin
#else
   DTRACER_MODULE(:,:,dofe0_ind) = DOFe0_prod - DOFe0_remin
   DTRACER_MODULE(:,:,dofe1_ind) = DOFe1_prod - DOFe1_remin
   DTRACER_MODULE(:,:,dofe2_ind) = DOFe2_prod - DOFe2_remin
   DTRACER_MODULE(:,:,dofe3_ind) = DOFe3_prod - DOFe3_remin
   DTRACER_MODULE(:,:,dofe4_ind) = DOFe4_prod - DOFe4_remin
   DTRACER_MODULE(:,:,dofe5_ind) = DOFe5_prod - DOFe5_remin
   DTRACER_MODULE(:,:,dofe6_ind) = DOFe6_prod - DOFe6_remin
   DTRACER_MODULE(:,:,dofe7_ind) = DOFe7_prod - DOFe7_remin
#endif

!-----------------------------------------------------------------------
!   dissolved inorganic Carbon
!-----------------------------------------------------------------------

   DTRACER_MODULE(:,:,dic_ind) = &
      sum(auto_loss_dic, dim=3) + sum(auto_graze_dic, dim=3) - sum(photoC, dim=3) &
      + DOC_remin + POC%remin(:,:,bid) + sum(zoo_loss_dic, dim=3) + sum(zoo_graze_dic, dim=3) &
      + P_CaCO3%remin(:,:,bid)

   do auto_ind = 1, autotroph_cnt
      if (autotrophs(auto_ind)%CaCO3_ind > 0) &
         DTRACER_MODULE(:,:,dic_ind) = DTRACER_MODULE(:,:,dic_ind) &
            + f_graze_CaCO3_REMIN * auto_graze(:,:,auto_ind) * QCaCO3(:,:,auto_ind) &
            - CaCO3_PROD(:,:,auto_ind)
   end do

   DTRACER_MODULE(:,:,dic_alt_co2_ind) = DTRACER_MODULE(:,:,dic_ind)

!-----------------------------------------------------------------------
!  alkalinity
!-----------------------------------------------------------------------

   DTRACER_MODULE(:,:,alk_ind) = -DTRACER_MODULE(:,:,no3_ind) + &
      DTRACER_MODULE(:,:,nh4_ind) + c2 * P_CaCO3%remin(:,:,bid)

   do auto_ind = 1, autotroph_cnt
      if (autotrophs(auto_ind)%CaCO3_ind > 0) &
         DTRACER_MODULE(:,:,alk_ind) = DTRACER_MODULE(:,:,alk_ind) &
            + c2 * (f_graze_CaCO3_REMIN * auto_graze(:,:,auto_ind) * QCaCO3(:,:,auto_ind) &
                    - CaCO3_PROD(:,:,auto_ind))
   end do

!-----------------------------------------------------------------------
!  oxygen
!-----------------------------------------------------------------------

   O2_PRODUCTION = c0

   do auto_ind = 1, autotroph_cnt
      if (.not. autotrophs(auto_ind)%Nfixer) then
         where (photoC(:,:,auto_ind) > c0)
            O2_PRODUCTION = O2_PRODUCTION + photoC(:,:,auto_ind) * &
               ((NO3_V(:,:,auto_ind) / (NO3_V(:,:,auto_ind) + NH4_V(:,:,auto_ind))) / parm_Red_D_C_O2 + &
                (NH4_V(:,:,auto_ind) / (NO3_V(:,:,auto_ind) + NH4_V(:,:,auto_ind))) / parm_Remin_D_C_O2)
         end where
      else
         where (photoC(:,:,auto_ind) > c0)
            O2_PRODUCTION = O2_PRODUCTION + photoC(:,:,auto_ind) * &
               ((NO3_V(:,:,auto_ind) / (NO3_V(:,:,auto_ind) + NH4_V(:,:,auto_ind) + Nfix(:,:,auto_ind))) / parm_Red_D_C_O2 + &
                (NH4_V(:,:,auto_ind) / (NO3_V(:,:,auto_ind) + NH4_V(:,:,auto_ind) + Nfix(:,:,auto_ind))) / parm_Remin_D_C_O2 + &
                (Nfix(:,:,auto_ind) / (NO3_V(:,:,auto_ind) + NH4_V(:,:,auto_ind) + Nfix(:,:,auto_ind))) / parm_Red_D_C_O2_diaz)
         end where
      endif
   end do

   WORK1 = (O2_loc - parm_o2_min) / parm_o2_min_delta
   WORK1 = min(max(WORK1,c0),c1)
   O2_CONSUMPTION = WORK1 * &
        ( (POC%remin(:,:,bid) + DOC_remin - (SED_DENITRIF(:,:,bid)*denitrif_C_N) - OTHER_REMIN(:,:,bid) &
        + sum(zoo_loss_dic, dim=3)  +  sum(zoo_graze_dic, dim=3) &
        + sum(auto_loss_dic, dim=3) + sum(auto_graze_dic, dim=3) ) &
        / parm_Remin_D_C_O2 + (c2 * NITRIF))

   DTRACER_MODULE(:,:,o2_ind) = O2_PRODUCTION - O2_CONSUMPTION

!c!-----------------------------------------------------------------------
!c!  various tavg/history variables
!c!-----------------------------------------------------------------------
!c
!c   if (k == 1) then
!c      if (accumulate_tavg_now(tavg_O2_ZMIN) .or. accumulate_tavg_now(tavg_O2_ZMIN_DEPTH)) then
!c         ! WORK1 = O2 at this level
!c         ! WORK2 = vertical min of O2
!c         ! WORK3 = depth of min
!c
!c         kk = 1
!c         WORK1 = p5*(TRACER_MODULE_OLD(:,:,kk,o2_ind) + &
!c                     TRACER_MODULE_CUR(:,:,kk,o2_ind))
!c         WORK2 = WORK1
!c         WORK3 = zt(kk)
!c
!c         do kk = 2,km
!c            WORK1 = p5*(TRACER_MODULE_OLD(:,:,kk,o2_ind) + &
!c                        TRACER_MODULE_CUR(:,:,kk,o2_ind))
!c            where (kk <= KMT(:,:,bid) .and. (WORK1 < WORK2))
!c               WORK2 = WORK1
!c               WORK3 = zt(kk)
!c            endwhere
!c         end do
!c
!c         call accumulate_tavg_field(WORK2, tavg_O2_ZMIN,bid,k)
!c
!c         call accumulate_tavg_field(WORK3, tavg_O2_ZMIN_DEPTH,bid,k)
!c      endif
!c   endif
!c
!c   call accumulate_tavg_field(O2_PRODUCTION, tavg_O2_PRODUCTION,bid,k)
!c
!c   call accumulate_tavg_field(O2_CONSUMPTION, tavg_O2_CONSUMPTION,bid,k)
!c
!c   if (accumulate_tavg_now(tavg_AOU)) then
!c      WORK1 = O2SAT(TEMP, SALT, &
!c                    LAND_MASK(:,:,bid) .and. (k <= KMT(:,:,bid)))
!c      WORK1 = WORK1 - p5*(TRACER_MODULE_OLD(:,:,k,o2_ind) + &
!c                          TRACER_MODULE_CUR(:,:,k,o2_ind))
!c      call accumulate_tavg_field(WORK1, tavg_AOU,bid,k)
!c   endif
!c
!c   call accumulate_tavg_field(PAR_avg, tavg_PAR_avg,bid,k)
!c
!c   do zoo_ind = 1, zooplankton_cnt
!c      call accumulate_tavg_field(zoo_loss(:,:,zoo_ind),    tavg_zoo_loss(zoo_ind),bid,k)
!c      call accumulate_tavg_field(zoo_loss_poc(:,:,zoo_ind), tavg_zoo_loss_poc(zoo_ind),bid,k)
!c      call accumulate_tavg_field(zoo_loss_doc(:,:,zoo_ind), tavg_zoo_loss_doc(zoo_ind),bid,k)
!c
!c      call accumulate_tavg_field(zoo_graze(:,:,zoo_ind),   tavg_zoo_graze(zoo_ind),bid,k)
!c      call accumulate_tavg_field(zoo_graze_poc(:,:,zoo_ind),   tavg_zoo_graze_poc(zoo_ind),bid,k)
!c      call accumulate_tavg_field(zoo_graze_doc(:,:,zoo_ind),   tavg_zoo_graze_doc(zoo_ind),bid,k)
!c      call accumulate_tavg_field(zoo_graze_zoo(:,:,zoo_ind),   tavg_zoo_graze_zoo(zoo_ind),bid,k)
!c
!c      call accumulate_tavg_field(x_graze_zoo(:,:,zoo_ind), tavg_x_graze_zoo(zoo_ind),bid,k)
!c   end do
!c
!c   if (accumulate_tavg_now(tavg_auto_graze_TOT)) then
!c      WORK1 = sum(auto_graze, dim=3)
!c      call accumulate_tavg_field(WORK1, tavg_auto_graze_TOT,bid,k)
!c   endif
!c
!c   do auto_ind = 1, autotroph_cnt
!c      call accumulate_tavg_field(auto_graze(:,:,auto_ind), tavg_auto_graze(auto_ind),bid,k)
!c      call accumulate_tavg_field(auto_graze_poc(:,:,auto_ind), tavg_auto_graze_poc(auto_ind),bid,k)
!c      call accumulate_tavg_field(auto_graze_doc(:,:,auto_ind), tavg_auto_graze_doc(auto_ind),bid,k)
!c      call accumulate_tavg_field(auto_graze_zoo(:,:,auto_ind), tavg_auto_graze_zoo(auto_ind),bid,k)
!c      call accumulate_tavg_field(auto_loss(:,:,auto_ind), tavg_auto_loss(auto_ind),bid,k)
!c      call accumulate_tavg_field(auto_loss_poc(:,:,auto_ind), tavg_auto_loss_poc(auto_ind),bid,k)
!c      call accumulate_tavg_field(auto_loss_doc(:,:,auto_ind), tavg_auto_loss_doc(auto_ind),bid,k)
!c      call accumulate_tavg_field(auto_agg(:,:,auto_ind), tavg_auto_agg(auto_ind),bid,k)
!c      call accumulate_tavg_field(photoC(:,:,auto_ind), tavg_photoC(auto_ind),bid,k)
!c   end do
#ifdef DIAGNOSTICS_BIO
     call accumulate_tavg_field_3d(photoC(:,:,1),iphotoC_sp  ,bid,k)
     call accumulate_tavg_field_3d(photoC(:,:,2),iphotoC_diat,bid,k)
     call accumulate_tavg_field_3d(photoC(:,:,3),iphotoC_diaz,bid,k)

# ifndef FE_TAG
     call accumulate_tavg_field_3d(aFe_brate ,iFe_brate ,bid,k)
     call accumulate_tavg_field_3d(aFe_scav  ,iFe_scav  ,bid,k)
     call accumulate_tavg_field_3d(aFe_hbio  ,iFe_hbio  ,bid,k)
     call accumulate_tavg_field_3d(aFe_disag ,iFe_disag ,bid,k)
     call accumulate_tavg_field_3d(aFe_pgen  ,iFe_pgen  ,bid,k)
     call accumulate_tavg_field_3d(aFe_premin,iFe_premin,bid,k)
# else
     call accumulate_tavg_field_3d(aFe0_brate ,iFe0_brate ,bid,k)
     call accumulate_tavg_field_3d(aFe0_scav  ,iFe0_scav  ,bid,k)
     call accumulate_tavg_field_3d(aFe0_hbio  ,iFe0_hbio  ,bid,k)
     call accumulate_tavg_field_3d(aFe0_disag ,iFe0_disag ,bid,k)
     call accumulate_tavg_field_3d(aFe0_pgen  ,iFe0_pgen  ,bid,k)
     call accumulate_tavg_field_3d(aFe0_premin,iFe0_premin,bid,k)

     call accumulate_tavg_field_3d(aFe1_brate ,iFe1_brate ,bid,k)
     call accumulate_tavg_field_3d(aFe1_scav  ,iFe1_scav  ,bid,k)
     call accumulate_tavg_field_3d(aFe1_hbio  ,iFe1_hbio  ,bid,k)
     call accumulate_tavg_field_3d(aFe1_disag ,iFe1_disag ,bid,k)
     call accumulate_tavg_field_3d(aFe1_pgen  ,iFe1_pgen  ,bid,k)
     call accumulate_tavg_field_3d(aFe1_premin,iFe1_premin,bid,k)

     call accumulate_tavg_field_3d(aFe2_brate ,iFe2_brate ,bid,k)
     call accumulate_tavg_field_3d(aFe2_scav  ,iFe2_scav  ,bid,k)
     call accumulate_tavg_field_3d(aFe2_hbio  ,iFe2_hbio  ,bid,k)
     call accumulate_tavg_field_3d(aFe2_disag ,iFe2_disag ,bid,k)
     call accumulate_tavg_field_3d(aFe2_pgen  ,iFe2_pgen  ,bid,k)
     call accumulate_tavg_field_3d(aFe2_premin,iFe2_premin,bid,k)

     call accumulate_tavg_field_3d(aFe3_brate ,iFe3_brate ,bid,k)
     call accumulate_tavg_field_3d(aFe3_scav  ,iFe3_scav  ,bid,k)
     call accumulate_tavg_field_3d(aFe3_hbio  ,iFe3_hbio  ,bid,k)
     call accumulate_tavg_field_3d(aFe3_disag ,iFe3_disag ,bid,k)
     call accumulate_tavg_field_3d(aFe3_pgen  ,iFe3_pgen  ,bid,k)
     call accumulate_tavg_field_3d(aFe3_premin,iFe3_premin,bid,k)

     call accumulate_tavg_field_3d(aFe4_brate ,iFe4_brate ,bid,k)
     call accumulate_tavg_field_3d(aFe4_scav  ,iFe4_scav  ,bid,k)
     call accumulate_tavg_field_3d(aFe4_hbio  ,iFe4_hbio  ,bid,k)
     call accumulate_tavg_field_3d(aFe4_disag ,iFe4_disag ,bid,k)
     call accumulate_tavg_field_3d(aFe4_pgen  ,iFe4_pgen  ,bid,k)
     call accumulate_tavg_field_3d(aFe4_premin,iFe4_premin,bid,k)

     call accumulate_tavg_field_3d(aFe5_brate ,iFe5_brate ,bid,k)
     call accumulate_tavg_field_3d(aFe5_scav  ,iFe5_scav  ,bid,k)
     call accumulate_tavg_field_3d(aFe5_hbio  ,iFe5_hbio  ,bid,k)
     call accumulate_tavg_field_3d(aFe5_disag ,iFe5_disag ,bid,k)
     call accumulate_tavg_field_3d(aFe5_pgen  ,iFe5_pgen  ,bid,k)
     call accumulate_tavg_field_3d(aFe5_premin,iFe5_premin,bid,k)

     call accumulate_tavg_field_3d(aFe6_brate ,iFe6_brate ,bid,k)
     call accumulate_tavg_field_3d(aFe6_scav  ,iFe6_scav  ,bid,k)
     call accumulate_tavg_field_3d(aFe6_hbio  ,iFe6_hbio  ,bid,k)
     call accumulate_tavg_field_3d(aFe6_disag ,iFe6_disag ,bid,k)
     call accumulate_tavg_field_3d(aFe6_pgen  ,iFe6_pgen  ,bid,k)
     call accumulate_tavg_field_3d(aFe6_premin,iFe6_premin,bid,k)

     call accumulate_tavg_field_3d(aFe7_brate ,iFe7_brate ,bid,k)
     call accumulate_tavg_field_3d(aFe7_scav  ,iFe7_scav  ,bid,k)
     call accumulate_tavg_field_3d(aFe7_hbio  ,iFe7_hbio  ,bid,k)
     call accumulate_tavg_field_3d(aFe7_disag ,iFe7_disag ,bid,k)
     call accumulate_tavg_field_3d(aFe7_pgen  ,iFe7_pgen  ,bid,k)
     call accumulate_tavg_field_3d(aFe7_premin,iFe7_premin,bid,k)
# endif
#endif
!c
!c   if (accumulate_tavg_now(tavg_photoC_TOT)) then
!c      WORK1 = sum(photoC, dim=3)
!c      call accumulate_tavg_field(WORK1, tavg_photoC_TOT,bid,k)
!c   endif
!c
!c   do auto_ind = 1, autotroph_cnt
!c      if (accumulate_tavg_now(tavg_photoC_zint(auto_ind))) then
!c         if (partial_bottom_cells) then
!c            WORK1 = merge(DZT(:,:,k,bid) * photoC(:,:,auto_ind), c0,k<=KMT(:,:,bid))
!c         else
!c            WORK1 = merge(dz(k) * photoC(:,:,auto_ind), c0,k<=KMT(:,:,bid))
!c         endif
!c         call accumulate_tavg_field(WORK1, tavg_photoC_zint(auto_ind),bid,k)
!c      endif
!c   end do
!c
!c   if (accumulate_tavg_now(tavg_photoC_TOT_zint)) then
!c      if (partial_bottom_cells) then
!c         WORK1 = merge(DZT(:,:,k,bid) * sum(photoC, dim=3), c0,k<=KMT(:,:,bid))
!c      else
!c         WORK1 = merge(dz(k) * sum(photoC, dim=3), c0,k<=KMT(:,:,bid))
!c      endif
!c      call accumulate_tavg_field(WORK1, tavg_photoC_TOT_zint,bid,k)
!c   endif
!c
!c   do auto_ind = 1, autotroph_cnt
!c      if (accumulate_tavg_now(tavg_photoC_NO3(auto_ind)) .or. &
!c          accumulate_tavg_now(tavg_photoC_NO3_zint(auto_ind))) then
!c         where (VNtot(:,:,auto_ind) > c0)
!c            WORK1 = (VNO3(:,:,auto_ind) / VNtot(:,:,auto_ind)) * photoC(:,:,auto_ind)
!c         elsewhere
!c            WORK1 = c0
!c         end where
!c
!c         call accumulate_tavg_field(WORK1, tavg_photoC_NO3(auto_ind),bid,k)
!c
!c         if (accumulate_tavg_now(tavg_photoC_NO3_zint(auto_ind))) then
!c            if (partial_bottom_cells) then
!c               WORK1 = merge(DZT(:,:,k,bid) * WORK1, c0,k<=KMT(:,:,bid))
!c            else
!c               WORK1 = merge(dz(k) * WORK1, c0,k<=KMT(:,:,bid))
!c            endif
!c            call accumulate_tavg_field(WORK1, tavg_photoC_NO3_zint(auto_ind),bid,k)
!c         endif
!c      endif
!c   end do
!c
!c   if (accumulate_tavg_now(tavg_photoC_NO3_TOT) .or. &
!c       accumulate_tavg_now(tavg_photoC_NO3_TOT_zint)) then
!c      WORK1 = c0
!c      do auto_ind = 1, autotroph_cnt
!c         where (VNtot(:,:,auto_ind) > c0)
!c            WORK1 = WORK1 + (VNO3(:,:,auto_ind) / VNtot(:,:,auto_ind)) * photoC(:,:,auto_ind)
!c         end where
!c      end do
!c
!c      call accumulate_tavg_field(WORK1, tavg_photoC_NO3_TOT,bid,k)
!c
!c      if (accumulate_tavg_now(tavg_photoC_NO3_TOT_zint)) then
!c         if (partial_bottom_cells) then
!c            WORK1 = merge(DZT(:,:,k,bid) * WORK1, c0,k<=KMT(:,:,bid))
!c         else
!c            WORK1 = merge(dz(k) * WORK1, c0,k<=KMT(:,:,bid))
!c         endif
!c         call accumulate_tavg_field(WORK1, tavg_photoC_NO3_TOT_zint,bid,k)
!c      endif
!c   endif
!c
!c   call accumulate_tavg_field(DOC_prod, tavg_DOC_prod,bid,k)
!c
!c   call accumulate_tavg_field(DOC_remin, tavg_DOC_remin,bid,k)
!c
!c   call accumulate_tavg_field(DON_prod, tavg_DON_prod,bid,k)
!c
!c   call accumulate_tavg_field(DON_remin, tavg_DON_remin,bid,k)
!c
!c   call accumulate_tavg_field(DOP_prod, tavg_DOP_prod,bid,k)
!c
!c   call accumulate_tavg_field(DOP_remin, tavg_DOP_remin,bid,k)
!c
!c   call accumulate_tavg_field(DOFe_prod, tavg_DOFe_prod,bid,k)
!c
!c   call accumulate_tavg_field(DOFe_remin, tavg_DOFe_remin,bid,k)
!c
!c   call accumulate_tavg_field(Fe_scavenge, tavg_Fe_scavenge,bid,k)
!c
!c   call accumulate_tavg_field(Fe_scavenge_rate, tavg_Fe_scavenge_rate,bid,k)
!c
!c   ztop = c0
!c   if (k > 1) ztop = zw(k-1)
!c
!c   if (accumulate_tavg_now(tavg_Jint_Ctot) .or. &
!c       (accumulate_tavg_now(tavg_Jint_100m_Ctot) .and. (ztop < 100.0e2_r8))) then
!c      WORK1 = DTRACER_MODULE(:,:,dic_ind) + DTRACER_MODULE(:,:,doc_ind) &
!c              + sum(DTRACER_MODULE(:,:,zooplankton(:)%C_ind), dim=3) &
!c              + sum(DTRACER_MODULE(:,:,autotrophs(:)%C_ind), dim=3)
!c      do auto_ind = 1, autotroph_cnt
!c         n = autotrophs(auto_ind)%CaCO3_ind
!c         if (n > 0) then
!c            WORK1 = WORK1 + DTRACER_MODULE(:,:,n)
!c         endif
!c      end do
!c      if (accumulate_tavg_now(tavg_Jint_Ctot)) then
!c         if (partial_bottom_cells) then
!c            WORK2 = merge(DZT(:,:,k,bid) * WORK1, c0, k<=KMT(:,:,bid))
!c         else
!c            WORK2 = merge(dz(k) * WORK1, c0, k<=KMT(:,:,bid))
!c         endif
!c         ! add back loss to sediments
!c         WORK2 = WORK2 + merge(POC%sed_loss(:,:,bid) + P_CaCO3%sed_loss(:,:,bid), &
!c                               c0, k<=KMT(:,:,bid))
!c         call accumulate_tavg_field(WORK2, tavg_Jint_Ctot,bid,k)
!c      endif
!c      if (accumulate_tavg_now(tavg_Jint_100m_Ctot) .and. (ztop < 100.0e2_r8)) then
!c         if (partial_bottom_cells) then
!c            WORK2 = merge(min(100.0e2_r8 - ztop, DZT(:,:,k,bid)) * WORK1, c0, k<=KMT(:,:,bid))
!c            ! add back loss to sediments
!c            WORK2 = WORK2 + merge(POC%sed_loss(:,:,bid) + P_CaCO3%sed_loss(:,:,bid), &
!c                                  c0, ztop + DZT(:,:,k,bid) <= 100.0e2_r8 .and. k<=KMT(:,:,bid))
!c         else
!c            WORK2 = merge(min(100.0e2_r8 - ztop, dz(k)) * WORK1, c0, k<=KMT(:,:,bid))
!c            ! add back loss to sediments
!c            WORK2 = WORK2 + merge(POC%sed_loss(:,:,bid) + P_CaCO3%sed_loss(:,:,bid), &
!c                                  c0, ztop + dz(k) <= 100.0e2_r8 .and. k<=KMT(:,:,bid))
!c         endif
!c         call accumulate_tavg_field(WORK2, tavg_Jint_100m_Ctot,bid,k)
!c      endif
!c   endif
!c
!c   if (accumulate_tavg_now(tavg_Jint_Ntot) .or. &
!c       (accumulate_tavg_now(tavg_Jint_100m_Ntot) .and. (ztop < 100.0e2_r8))) then
!c      WORK1 = DTRACER_MODULE(:,:,no3_ind) + DTRACER_MODULE(:,:,nh4_ind) &
!c              + DTRACER_MODULE(:,:,don_ind) + DTRACER_MODULE(:,:,donr_ind) &
!c              + Q * sum(DTRACER_MODULE(:,:,zooplankton(:)%C_ind), dim=3) &
!c              + Q * sum(DTRACER_MODULE(:,:,autotrophs(:)%C_ind), dim=3)
!c      ! add back column and sediment denitrification
!c      WORK1 = WORK1 + DENITRIF + SED_DENITRIF(:,:,bid)
!c      ! subtract out N fixation
!c      do auto_ind = 1, autotroph_cnt
!c         if (autotrophs(auto_ind)%Nfixer) WORK1 = WORK1 - Nfix(:,:,auto_ind)
!c      end do
!c      if (accumulate_tavg_now(tavg_Jint_Ntot)) then
!c         if (partial_bottom_cells) then
!c            WORK2 = merge(DZT(:,:,k,bid) * WORK1, c0, k<=KMT(:,:,bid))
!c         else
!c            WORK2 = merge(dz(k) * WORK1, c0, k<=KMT(:,:,bid))
!c         endif
!c         ! add back loss to sediments
!c         WORK2 = WORK2 + merge(POC%sed_loss(:,:,bid) * Q, &
!c                               c0, k<=KMT(:,:,bid))
!c         call accumulate_tavg_field(WORK2, tavg_Jint_Ntot,bid,k)
!c      endif
!c      if (accumulate_tavg_now(tavg_Jint_100m_Ntot) .and. (ztop < 100.0e2_r8)) then
!c         if (partial_bottom_cells) then
!c            WORK2 = merge(min(100.0e2_r8 - ztop, DZT(:,:,k,bid)) * WORK1, c0, k<=KMT(:,:,bid))
!c            ! add back loss to sediments
!c            WORK2 = WORK2 + merge(POC%sed_loss(:,:,bid) * Q, &
!c                                  c0, ztop + DZT(:,:,k,bid) <= 100.0e2_r8 .and. k<=KMT(:,:,bid))
!c         else
!c            WORK2 = merge(min(100.0e2_r8 - ztop, dz(k)) * WORK1, c0, k<=KMT(:,:,bid))
!c            ! add back loss to sediments
!c            WORK2 = WORK2 + merge(POC%sed_loss(:,:,bid) * Q, &
!c                                  c0, ztop + dz(k) <= 100.0e2_r8 .and. k<=KMT(:,:,bid))
!c         endif
!c         call accumulate_tavg_field(WORK2, tavg_Jint_100m_Ntot,bid,k)
!c      endif
!c   endif
!c
!c   if (accumulate_tavg_now(tavg_Jint_Ptot) .or. &
!c       (accumulate_tavg_now(tavg_Jint_100m_Ptot) .and. (ztop < 100.0e2_r8))) then
!c      WORK1 = DTRACER_MODULE(:,:,po4_ind) + DTRACER_MODULE(:,:,dop_ind) &
!c              + DTRACER_MODULE(:,:,dopr_ind)
!c      do zoo_ind =1, zooplankton_cnt
!c         n = zooplankton(zoo_ind)%C_ind
!c         WORK1 = WORK1 + Qp_zoo_pom * DTRACER_MODULE(:,:,n)
!c      end do
!c      do auto_ind = 1, autotroph_cnt
!c         n = autotrophs(auto_ind)%C_ind
!c         WORK1 = WORK1 + autotrophs(auto_ind)%Qp * DTRACER_MODULE(:,:,n)
!c      end do
!c      if (accumulate_tavg_now(tavg_Jint_Ptot)) then
!c         if (partial_bottom_cells) then
!c            WORK2 = merge(DZT(:,:,k,bid) * WORK1, c0, k<=KMT(:,:,bid))
!c         else
!c            WORK2 = merge(dz(k) * WORK1, c0, k<=KMT(:,:,bid))
!c         endif
!c         ! add back loss to sediments
!c         WORK2 = WORK2 + merge(POC%sed_loss(:,:,bid) * Qp_zoo_pom, &
!c                               c0, k<=KMT(:,:,bid))
!c         call accumulate_tavg_field(WORK2, tavg_Jint_Ptot,bid,k)
!c      endif
!c      if (accumulate_tavg_now(tavg_Jint_100m_Ptot) .and. (ztop < 100.0e2_r8)) then
!c         if (partial_bottom_cells) then
!c            WORK2 = merge(min(100.0e2_r8 - ztop, DZT(:,:,k,bid)) * WORK1, c0, k<=KMT(:,:,bid))
!c            ! add back loss to sediments
!c            WORK2 = WORK2 + merge(POC%sed_loss(:,:,bid) * Qp_zoo_pom, &
!c                                  c0, ztop + DZT(:,:,k,bid) <= 100.0e2_r8 .and. k<=KMT(:,:,bid))
!c         else
!c            WORK2 = merge(min(100.0e2_r8 - ztop, dz(k)) * WORK1, c0, k<=KMT(:,:,bid))
!c            ! add back loss to sediments
!c            WORK2 = WORK2 + merge(POC%sed_loss(:,:,bid) * Qp_zoo_pom, &
!c                                  c0, ztop + dz(k) <= 100.0e2_r8 .and. k<=KMT(:,:,bid))
!c         endif
!c         call accumulate_tavg_field(WORK2, tavg_Jint_100m_Ptot,bid,k)
!c      endif
!c   endif
!c
!c   if (accumulate_tavg_now(tavg_Jint_Sitot) .or. &
!c       (accumulate_tavg_now(tavg_Jint_100m_Sitot) .and. (ztop < 100.0e2_r8))) then
!c      WORK1 = DTRACER_MODULE(:,:,sio3_ind)
!c      do auto_ind = 1, autotroph_cnt
!c         n = autotrophs(auto_ind)%Si_ind
!c         if (n > 0) then
!c            WORK1 = WORK1 + DTRACER_MODULE(:,:,n)
!c         endif
!c      end do
!c      if (accumulate_tavg_now(tavg_Jint_Sitot)) then
!c         if (partial_bottom_cells) then
!c            WORK2 = merge(DZT(:,:,k,bid) * WORK1, c0, k<=KMT(:,:,bid))
!c         else
!c            WORK2 = merge(dz(k) * WORK1, c0, k<=KMT(:,:,bid))
!c         endif
!c         ! add back loss to sediments
!c         WORK2 = WORK2 + merge(P_SiO2%sed_loss(:,:,bid), &
!c                               c0, k<=KMT(:,:,bid))
!c         call accumulate_tavg_field(WORK2, tavg_Jint_Sitot,bid,k)
!c      endif
!c      if (accumulate_tavg_now(tavg_Jint_100m_Sitot) .and. (ztop < 100.0e2_r8)) then
!c         if (partial_bottom_cells) then
!c            WORK2 = merge(min(100.0e2_r8 - ztop, DZT(:,:,k,bid)) * WORK1, c0, k<=KMT(:,:,bid))
!c            ! add back loss to sediments
!c            WORK2 = WORK2 + merge(P_SiO2%sed_loss(:,:,bid), &
!c                                  c0, ztop + DZT(:,:,k,bid) <= 100.0e2_r8 .and. k<=KMT(:,:,bid))
!c         else
!c            WORK2 = merge(min(100.0e2_r8 - ztop, dz(k)) * WORK1, c0, k<=KMT(:,:,bid))
!c            ! add back loss to sediments
!c            WORK2 = WORK2 + merge(P_SiO2%sed_loss(:,:,bid), &
!c                                  c0, ztop + dz(k) <= 100.0e2_r8 .and. k<=KMT(:,:,bid))
!c         endif
!c         call accumulate_tavg_field(WORK2, tavg_Jint_100m_Sitot,bid,k)
!c      endif
!c   endif
!c
#ifdef CISO
   if (lexport_shared_vars) then
      DIC_loc_fields(:,:,bid) = DIC_loc
      DOC_loc_fields(:,:,bid) = DOC_loc
      O2_loc_fields(:,:,bid) = O2_loc
      NO3_loc_fields(:,:,bid) = NO3_loc


      zooC_loc_fields(:,:,:,bid)     = zooC_loc
      zoo_loss_fields(:,:,:,bid)     = zoo_loss
      zoo_loss_poc_fields(:,:,:,bid) = zoo_loss_poc
      zoo_loss_doc_fields(:,:,:,bid) = zoo_loss_doc
      zoo_loss_dic_fields(:,:,:,bid) = zoo_loss_dic


      CO3_fields(:,:,bid) = CO3
      HCO3_fields(:,:,bid) = HCO3
      H2CO3_fields(:,:,bid) = H2CO3
      DOC_remin_fields(:,:,bid) = DOC_remin

      autotrophChl_loc_fields(:,:,:,bid) = autotrophChl_loc
      autotrophC_loc_fields(:,:,:,bid) = autotrophC_loc
      autotrophFe_loc_fields(:,:,:,bid) = autotrophFe_loc
      do auto_ind = 1, autotroph_cnt
         if (autotrophs(auto_ind)%Si_ind > 0) &
            autotrophSi_loc_fields(:,:,auto_ind,bid) = autotrophSi_loc(:,:,auto_ind)
         if (autotrophs(auto_ind)%CaCO3_ind > 0) &
            autotrophCaCO3_loc_fields(:,:,auto_ind,bid) = autotrophCaCO3_loc(:,:,auto_ind)
      end do

      QCaCO3_fields(:,:,:,bid) = QCaCO3
      auto_graze_fields(:,:,:,bid) = auto_graze
      auto_graze_zoo_fields(:,:,:,bid) = auto_graze_zoo
      auto_graze_poc_fields(:,:,:,bid) = auto_graze_poc
      auto_graze_doc_fields(:,:,:,bid) = auto_graze_doc
      auto_graze_dic_fields(:,:,:,bid) = auto_graze_dic
      auto_loss_fields(:,:,:,bid) = auto_loss
      auto_loss_poc_fields(:,:,:,bid) = auto_loss_poc
      auto_loss_doc_fields(:,:,:,bid) = auto_loss_doc
      auto_loss_dic_fields(:,:,:,bid) = auto_loss_dic
      auto_agg_fields(:,:,:,bid) = auto_agg
      photoC_fields(:,:,:,bid) = photoC
      CaCO3_PROD_fields(:,:,:,bid) = CaCO3_PROD
      PCphoto_fields(:,:,:,bid) = PCphoto
   endif
#endif

!c   call timer_stop(ecosys_interior_timer, block_id=bid)

   end associate
!cj!c
!cj!-----------------------------------------------------------------------
!cj!EOC

 end subroutine ecosys_set_interior

!***********************************************************************
!BOP
! !IROUTINE: init_particulate_terms
! !INTERFACE:

#ifndef FE_TAG
 subroutine init_particulate_terms(POC, P_CaCO3, P_SiO2, dust, P_iron, &
# ifdef BEC
       QA_dust_def, NET_DUST_IN)
# else
       QA_dust_def, NET_DUST_IN, this_block)
# endif
#else
 subroutine init_particulate_terms(POC, P_CaCO3, P_SiO2, dust, P_iron, &
       P_iron0, P_iron1, P_iron2, P_iron3, P_iron4, P_iron5, P_iron6, P_iron7,&
# ifdef BEC
       QA_dust_def, NET_DUST_IN)
# else
       QA_dust_def, NET_DUST_IN, this_block)
# endif
#endif

! !DESCRIPTION:
!  Set incoming fluxes (put into outgoing flux for first level usage).
!  Set dissolution length, production fraction and mass terms.
!
!  The first 6 arguments are intent(inout) in
!  order to preserve contents on other blocks.

! !INPUT/OUTPUT PARAMETERS:

   type(sinking_particle), intent(inout) :: &
      POC,          & ! base units = nmol C
      P_CaCO3,      & ! base units = nmol CaCO3
      P_SiO2,       & ! base units = nmol SiO2
      dust            ! base units = g

#ifndef FE_TAG
   type(sinking_particle), intent(inout) :: &
      P_iron          ! base units = nmol Fe
#else
   type(sinking_particle), intent(inout) :: &
      P_iron ,      & ! base units = nmol Fe
      P_iron0,      & ! base units = nmol Fe
      P_iron1,      & ! base units = nmol Fe
      P_iron2,      & ! base units = nmol Fe
      P_iron3,      & ! base units = nmol Fe
      P_iron4,      & ! base units = nmol Fe
      P_iron5,      & ! base units = nmol Fe
      P_iron6,      & ! base units = nmol Fe
      P_iron7         ! base units = nmol Fe
#endif

   real (r8), dimension(nx_block,ny_block,max_blocks_clinic), intent(inout) :: &
      QA_dust_def     ! incoming deficit in the QA(dust) POC flux

! !INPUT PARAMETERS:

   real (r8), dimension(nx_block,ny_block,max_blocks_clinic), intent(in) :: &
      NET_DUST_IN     ! dust flux

#ifndef BEC
   type (block), intent(in) :: &
      this_block      ! block info for the current block
#endif

!EOP
!BOC
!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

   integer (int_kind) :: &
      bid                 ! local_block id

   character (char_len) :: &
      tracer_data_label   ! label for what is being updated

   character (char_len), dimension(:), allocatable :: &
      tracer_data_names   ! short names for input data fields

   integer (int_kind), dimension(:), allocatable :: &
      tracer_bndy_loc,  & ! location and field type for ghost
      tracer_bndy_type    !    cell updates

!-----------------------------------------------------------------------
!  parameters, from Armstrong et al. 2000
!
!  July 2002, length scale for excess POC and bSI modified by temperature
!  Value given here is at Tref of 30 deg. C, JKM
!-----------------------------------------------------------------------

    POC%diss      = parm_POC_diss   ! diss. length (cm), modified by TEMP
    POC%gamma     = c0              ! not used
    POC%mass      = 12.01_r8        ! molecular weight of POC
    POC%rho       = c0              ! not used

    P_CaCO3%diss  = parm_CaCO3_diss ! diss. length (cm)
    P_CaCO3%gamma = 0.30_r8         ! prod frac -> hard subclass
    P_CaCO3%mass  = 100.09_r8       ! molecular weight of CaCO
    P_CaCO3%rho   = 0.05_r8 * P_CaCO3%mass / POC%mass ! QA mass ratio for CaCO3

    P_SiO2%diss   = parm_SiO2_diss  ! diss. length (cm), modified by TEMP
    P_SiO2%gamma  = 0.03_r8        ! prod frac -> hard subclass
    P_SiO2%mass   = 60.08_r8        ! molecular weight of SiO2
    P_SiO2%rho    = 0.05_r8 * P_SiO2%mass / POC%mass ! QA mass ratio for SiO2

    dust%diss     = 20000.0_r8      ! diss. length (cm)
    dust%gamma    = 0.97_r8         ! prod frac -> hard subclass
    dust%mass     = 1.0e9_r8        ! base units are already grams
    dust%rho      = 0.05_r8 * dust%mass / POC%mass ! QA mass ratio for dust

#ifndef FE_TAG
    P_iron%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron%mass   = c0              ! not used
    P_iron%rho    = c0              ! not used
#else
    P_iron%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron%mass   = c0              ! not used
    P_iron%rho    = c0              ! not used

    P_iron0%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron0%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron0%mass   = c0              ! not used
    P_iron0%rho    = c0              ! not used

    P_iron1%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron1%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron1%mass   = c0              ! not used
    P_iron1%rho    = c0              ! not used

    P_iron2%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron2%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron2%mass   = c0              ! not used
    P_iron2%rho    = c0              ! not used

    P_iron3%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron3%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron3%mass   = c0              ! not used
    P_iron3%rho    = c0              ! not used

    P_iron4%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron4%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron4%mass   = c0              ! not used
    P_iron4%rho    = c0              ! not used

    P_iron5%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron5%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron5%mass   = c0              ! not used
    P_iron5%rho    = c0              ! not used

    P_iron6%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron6%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron6%mass   = c0              ! not used
    P_iron6%rho    = c0              ! not used

    P_iron7%diss   = 60000.0_r8      ! diss. length (cm) - not used
    P_iron7%gamma  = c0              ! prod frac -> hard subclass - not used
    P_iron7%mass   = c0              ! not used
    P_iron7%rho    = c0              ! not used
#endif

!-----------------------------------------------------------------------
!  Set incoming fluxes
!-----------------------------------------------------------------------

#ifdef BEC
    bid = 1
#else
    bid = this_block%local_id
#endif

    P_CaCO3%sflux_out(:,:,bid) = c0
    P_CaCO3%hflux_out(:,:,bid) = c0

    P_SiO2%sflux_out(:,:,bid) = c0
    P_SiO2%hflux_out(:,:,bid) = c0

#ifdef BEC
    dust%sflux_out(:,:,bid) = (c1 - dust%gamma) * NET_DUST_IN(:,:,bid)
    dust%hflux_out(:,:,bid) = dust%gamma * NET_DUST_IN(:,:,bid)
#else
    if (dust_flux%has_data) then
       dust%sflux_out(:,:,bid) = (c1 - dust%gamma) * NET_DUST_IN(:,:,bid)
       dust%hflux_out(:,:,bid) = dust%gamma * NET_DUST_IN(:,:,bid)
    else
       dust%sflux_out(:,:,bid) = c0
       dust%hflux_out(:,:,bid) = c0
    endif
#endif

#ifndef FE_TAG
    P_iron%sflux_out(:,:,bid) = c0
    P_iron%hflux_out(:,:,bid) = c0
#else
    P_iron%sflux_out(:,:,bid) = c0
    P_iron%hflux_out(:,:,bid) = c0

    P_iron0%sflux_out(:,:,bid) = c0
    P_iron0%hflux_out(:,:,bid) = c0

    P_iron1%sflux_out(:,:,bid) = c0
    P_iron1%hflux_out(:,:,bid) = c0

    P_iron2%sflux_out(:,:,bid) = c0
    P_iron2%hflux_out(:,:,bid) = c0

    P_iron3%sflux_out(:,:,bid) = c0
    P_iron3%hflux_out(:,:,bid) = c0

    P_iron4%sflux_out(:,:,bid) = c0
    P_iron4%hflux_out(:,:,bid) = c0

    P_iron5%sflux_out(:,:,bid) = c0
    P_iron5%hflux_out(:,:,bid) = c0

    P_iron6%sflux_out(:,:,bid) = c0
    P_iron6%hflux_out(:,:,bid) = c0

    P_iron7%sflux_out(:,:,bid) = c0
    P_iron7%hflux_out(:,:,bid) = c0
#endif

!-----------------------------------------------------------------------
!  Hard POC is QA flux and soft POC is excess POC.
!-----------------------------------------------------------------------

    POC%sflux_out(:,:,bid) = c0
    POC%hflux_out(:,:,bid) = c0

!-----------------------------------------------------------------------
!  Compute initial QA(dust) POC flux deficit.
!-----------------------------------------------------------------------

    QA_dust_def(:,:,bid) = dust%rho * &
       (dust%sflux_out(:,:,bid) + dust%hflux_out(:,:,bid))

!-----------------------------------------------------------------------
!EOC

 end subroutine init_particulate_terms

!***********************************************************************
!BOP
! !IROUTINE: compute_particulate_terms
! !INTERFACE:

#ifndef FE_TAG
 subroutine compute_particulate_terms(k, &
       QA_dust_def, TEMP, O2_loc, NO3_loc, SED_DENITRIF, OTHER_REMIN, &
# ifdef BEC
       lexport_shared_vars)
# else
       lexport_shared_vars, this_block)
# endif
#else
 subroutine compute_particulate_terms(k, &
       QA_dust_def, TEMP, O2_loc, NO3_loc, &
       SED_DENITRIF, OTHER_REMIN, &
# ifdef BEC
       lexport_shared_vars)
# else
       lexport_shared_vars, this_block)
# endif
#endif
! !DESCRIPTION:
!  Compute outgoing fluxes and remineralization terms. Assumes that
!  production terms have been set. Incoming fluxes are assumed to be the
!  outgoing fluxes from the previous level.
!
!  It is assumed that there is no production of dust.
!
!  Instantaneous remineralization in the bottom cell is implemented by
!  setting the outgoing flux to zero.
!
!  For POC, the hard subclass is the POC flux qualitatively associated
!  with the ballast flux. The soft subclass is the excess POC flux.
!
!  Remineralization for the non-iron particulate pools is computing
!  by first computing the outgoing flux and then computing the
!  remineralization from conservation, i.e.
!     flux_in - flux_out + prod * dz - remin * dz == 0.
!
!  For iron, remineralization is first computed from POC remineralization
!  and then flux_out is computed from conservation. If the resulting
!  flux_out is negative or should be zero because of the sea floor, the
!  remineralization is adjusted.
!  Note: all the sinking iron is in the P_iron%sflux pool, hflux Fe not
!        explicitly tracked, it is assumed that total iron remin is
!        proportional to total POC remin.
!
!  Based upon Armstrong et al. 2000
!
!  July 2002, added temperature effect on remin length scale of
!  excess POC (all soft POM& Iron) and on SiO2.
!  new variable passed into ballast, Tfunc, main Temperature function
!  computed in ecosystem routine.  scaling factor for dissolution
!  of excess POC, Fe, and Bsi now varies with location (f(temperature)).
!
!  Added diffusive iron flux from sediments at depths < 1100m,
!  based on Johnson et al., 1999, value of 5 umolFe/m2/day,
!      this value too high, using 2 umolFe/m2/day here
!
!  Allow hard fraction of ballast to remin with long length scale 40,000m
!     thus ~ 10% of hard ballast remins over 4000m water column.
!
!  Sinking dust flux is decreased by assumed instant solubility/dissolution
!     at ocean surface from the parm_Fe_bioavail.
!
!  Modified to allow different Q10 factors for soft POM and bSI remin,
!  water TEMP is now passed in instead of Tfunc (1/2005, JKM)

! !USES:

#ifdef CCSMCOUPLED
   use shr_sys_mod, only: shr_sys_abort
#endif

! !INPUT PARAMETERS:

   integer (int_kind), intent(in) :: k ! vertical model level

   real (r8), dimension(nx_block,ny_block), intent(in) :: &
      TEMP,         & ! temperature for scaling functions bsi%diss
      O2_loc,       & ! dissolved oxygen used to modify POC%diss, Sed fluxes
      NO3_loc         ! dissolved nitrate used to modify sed fluxes


   logical (log_kind), intent(in) :: &
      lexport_shared_vars ! flag to save shared_vars or not

#ifndef BEC
   type (block), intent(in) :: &
      this_block      ! block info for the current block
#endif

! !INPUT/OUTPUT PARAMETERS:

   real (r8), dimension(nx_block,ny_block,max_blocks_clinic), intent(inout) :: &
      QA_dust_def,  & ! incoming deficit in the QA(dust) POC flux
      SED_DENITRIF, & ! sedimentary denitrification (umolN/cm^2/s)
      OTHER_REMIN     ! sedimentary remin not due to oxic or denitrification

!EOP
!BOC
!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

   real (r8) :: poc_diss, & ! diss. length used (cm)
                sio2_diss,& ! diss. length varies spatially with O2
                caco3_diss,&
                dust_diss

   character(*), parameter :: &
      subname = 'ecosys_mod:compute_particulate_terms'

   real (r8), dimension(nx_block,ny_block) :: &
      WORK,               & ! temporary for summed quantities to be averaged
      TfuncS,             & ! temperature scaling from soft POM remin
      DECAY_Hard,         & ! scaling factor for dissolution of Hard Ballast
      DECAY_HardDust        ! scaling factor for dissolution of Hard dust

   real (r8) :: &
      decay_POC_E,        & ! scaling factor for dissolution of excess POC
      decay_SiO2,         & ! scaling factor for dissolution of SiO2
      decay_CaCO3,        & ! scaling factor for dissolution of CaCO3
      decay_dust,         & ! scaling factor for dissolution of dust
      POC_PROD_avail,     & ! POC production available for excess POC flux
      new_QA_dust_def,    & ! outgoing deficit in the QA(dust) POC flux
      flux, flux_alt,     & ! temp variables used to update sinking flux
      dz_loc, dzr_loc       ! dz, dzr at a particular i,j location

   real (r8) :: &
      desorp_a,           & ! original bec desorp rate
      desorp_f              ! fraction of desorption must be < 1.

#ifdef BEC
   real (r8), dimension(nx_block,ny_block) :: &
      scalelength           ! used to scale dissolution length scales
#else
   real (r8) :: &
      scalelength           ! used to scale dissolution length scales
#endif

#ifdef FE_TAG
   real (r8) :: &
      s, s0, s1, s2, s3, s4, s5, s6, s7
#endif

   integer (int_kind) :: &
      i, j, n,            & ! loop indices
      bid                   ! local_block id

   logical (log_kind) :: &
      poc_error             ! POC error flag

!-----------------------------------------------------------------------
!  incoming fluxes are outgoing fluxes from previous level
!-----------------------------------------------------------------------

#ifdef BEC
   bid = 1
#else
   bid = this_block%local_id
#endif

   P_CaCO3%sflux_in(:,:,bid) = P_CaCO3%sflux_out(:,:,bid)
   P_CaCO3%hflux_in(:,:,bid) = P_CaCO3%hflux_out(:,:,bid)

   P_SiO2%sflux_in(:,:,bid) = P_SiO2%sflux_out(:,:,bid)
   P_SiO2%hflux_in(:,:,bid) = P_SiO2%hflux_out(:,:,bid)

   dust%sflux_in(:,:,bid) = dust%sflux_out(:,:,bid)
   dust%hflux_in(:,:,bid) = dust%hflux_out(:,:,bid)

   POC%sflux_in(:,:,bid) = POC%sflux_out(:,:,bid)
   POC%hflux_in(:,:,bid) = POC%hflux_out(:,:,bid)

#ifndef FE_TAG
   P_iron%sflux_in(:,:,bid) = P_iron%sflux_out(:,:,bid)
   P_iron%hflux_in(:,:,bid) = P_iron%hflux_out(:,:,bid)
#else
   P_iron%sflux_in(:,:,bid)  = P_iron0%sflux_out(:,:,bid) &
                             + P_iron1%sflux_out(:,:,bid) &
                             + P_iron2%sflux_out(:,:,bid) &
                             + P_iron3%sflux_out(:,:,bid) &
                             + P_iron4%sflux_out(:,:,bid) &
                             + P_iron5%sflux_out(:,:,bid) &
                             + P_iron6%sflux_out(:,:,bid) &
                             + P_iron7%sflux_out(:,:,bid)

   P_iron%hflux_in(:,:,bid)  = P_iron0%hflux_out(:,:,bid) &
                             + P_iron1%hflux_out(:,:,bid) &
                             + P_iron2%hflux_out(:,:,bid) &
                             + P_iron3%hflux_out(:,:,bid) &
                             + P_iron4%hflux_out(:,:,bid) &
                             + P_iron5%hflux_out(:,:,bid) &
                             + P_iron6%hflux_out(:,:,bid) &
                             + P_iron7%hflux_out(:,:,bid)

   P_iron0%sflux_in(:,:,bid) = P_iron0%sflux_out(:,:,bid)
   P_iron0%hflux_in(:,:,bid) = P_iron0%hflux_out(:,:,bid)

   P_iron1%sflux_in(:,:,bid) = P_iron1%sflux_out(:,:,bid)
   P_iron1%hflux_in(:,:,bid) = P_iron1%hflux_out(:,:,bid)

   P_iron2%sflux_in(:,:,bid) = P_iron2%sflux_out(:,:,bid)
   P_iron2%hflux_in(:,:,bid) = P_iron2%hflux_out(:,:,bid)

   P_iron3%sflux_in(:,:,bid) = P_iron3%sflux_out(:,:,bid)
   P_iron3%hflux_in(:,:,bid) = P_iron3%hflux_out(:,:,bid)

   P_iron4%sflux_in(:,:,bid) = P_iron4%sflux_out(:,:,bid)
   P_iron4%hflux_in(:,:,bid) = P_iron4%hflux_out(:,:,bid)

   P_iron5%sflux_in(:,:,bid) = P_iron5%sflux_out(:,:,bid)
   P_iron5%hflux_in(:,:,bid) = P_iron5%hflux_out(:,:,bid)

   P_iron6%sflux_in(:,:,bid) = P_iron6%sflux_out(:,:,bid)
   P_iron6%hflux_in(:,:,bid) = P_iron6%hflux_out(:,:,bid)

   P_iron7%sflux_in(:,:,bid) = P_iron7%sflux_out(:,:,bid)
   P_iron7%hflux_in(:,:,bid) = P_iron7%hflux_out(:,:,bid)
#endif

!-----------------------------------------------------------------------
!  initialize loss to sediments = 0 and local copy of percent sed
!-----------------------------------------------------------------------

#ifndef FE_TAG
   P_iron%sed_loss(:,:,bid) = c0
#else
   P_iron %sed_loss(:,:,bid) = c0
   P_iron0%sed_loss(:,:,bid) = c0
   P_iron1%sed_loss(:,:,bid) = c0
   P_iron2%sed_loss(:,:,bid) = c0
   P_iron3%sed_loss(:,:,bid) = c0
   P_iron4%sed_loss(:,:,bid) = c0
   P_iron5%sed_loss(:,:,bid) = c0
   P_iron6%sed_loss(:,:,bid) = c0
   P_iron7%sed_loss(:,:,bid) = c0
#endif
   POC%sed_loss(:,:,bid) = c0
   P_CaCO3%sed_loss(:,:,bid) = c0
   P_SiO2%sed_loss(:,:,bid) = c0
   dust%sed_loss(:,:,bid) = c0
   SED_DENITRIF(:,:,bid)=c0
   OTHER_REMIN(:,:,bid)=c0

!-----------------------------------------------------------------------
!  compute scalelength and decay factors
!-----------------------------------------------------------------------
!  rewrite if blocks to where blocks ( misumi )

#ifdef BEC
   where (zw(:,:,k) < parm_scalelen_z(1))
      scalelength = parm_scalelen_vals(1)
   elsewhere (zw(:,:,k) >= parm_scalelen_z(size(parm_scalelen_z)))
      scalelength = parm_scalelen_vals(size(parm_scalelen_z))
   end where

   do n = 2, size(parm_scalelen_z)
      where (zw(:,:,k) >= parm_scalelen_z(n-1) .and. zw(:,:,k) < parm_scalelen_z(n) )
         scalelength = parm_scalelen_vals(n-1) &
            + (parm_scalelen_vals(n) - parm_scalelen_vals(n-1)) &
              * (zw(:,:,k) - parm_scalelen_z(n-1))/(parm_scalelen_z(n) - parm_scalelen_z(n-1))
      end where
   end do
#else
   if (zw(k) < parm_scalelen_z(1)) then
      scalelength = parm_scalelen_vals(1)
   else if (zw(k) >= parm_scalelen_z(size(parm_scalelen_z))) then
      scalelength = parm_scalelen_vals(size(parm_scalelen_z))
   else
      do n = 2, size(parm_scalelen_z)
         if (zw(k) < parm_scalelen_z(n)) then
            scalelength = parm_scalelen_vals(n-1) &
               + (parm_scalelen_vals(n) - parm_scalelen_vals(n-1)) &
                 * (zw(k) - parm_scalelen_z(n-1))/(parm_scalelen_z(n) - parm_scalelen_z(n-1))
            exit
         endif
      end do
   endif
#endif

   if (partial_bottom_cells) then
      DECAY_Hard     = exp(-DZT(:,:,k,bid) / 4.0e6_r8)
      DECAY_HardDust = exp(-DZT(:,:,k,bid) / 1.2e7_r8)
   else
      DECAY_Hard     = exp(-dz(k) / 4.0e6_r8)
      DECAY_HardDust = exp(-dz(k) / 1.2e7_r8)
   endif

!----------------------------------------------------------------------
!   Tref = 30.0 reference temperature (deg. C)
!-----------------------------------------------------------------------

   TfuncS = 1.5_r8**(((TEMP + T0_Kelvin) - (Tref + T0_Kelvin)) / c10)

   poc_error = .false.
   dz_loc = dz(k)

   do j = 1,ny_block
      do i = 1,nx_block

         if (LAND_MASK(i,j,bid) .and. k <= KMT(i,j,bid)) then

            if (partial_bottom_cells) then
               dz_loc = DZT(i,j,k,bid)
            endif
            dzr_loc = c1 / dz_loc

            poc_diss = POC%diss
            sio2_diss = P_SiO2%diss
            caco3_diss = P_CaCO3%diss
            dust_diss = dust%diss

!-----------------------------------------------------------------------
!  increase POC diss length scale where O2 concentrations are low
!-----------------------------------------------------------------------

            if ((O2_loc(i,j) >= 5.0_r8) .and. (O2_loc(i,j) < 40.0_r8)) then
               poc_diss = POC%diss*(c1+(3.3_r8-c1)*(40.0_r8 - O2_loc(i,j))/35.0_r8)
            else if (O2_loc(i,j) < 5.0_r8) then
               poc_diss = POC%diss * 3.3_r8
            endif

!-----------------------------------------------------------------------
!  apply scalelength factor to length scales
!-----------------------------------------------------------------------

            poc_diss = scalelength(i,j) * poc_diss
            sio2_diss = scalelength(i,j) * sio2_diss
            caco3_diss = scalelength(i,j) * caco3_diss
            dust_diss = scalelength(i,j) * dust_diss

!-----------------------------------------------------------------------
!  apply temperature dependence to sio2_diss length scale
!-----------------------------------------------------------------------

!            sio2_diss = sio2_diss / TfuncS(i,j)

!-----------------------------------------------------------------------
!  decay_POC_E and decay_SiO2 set locally, modified by O2
!-----------------------------------------------------------------------

            decay_POC_E = exp(-dz_loc / poc_diss)
            decay_SiO2  = exp(-dz_loc / sio2_diss)
            decay_CaCO3 = exp(-dz_loc / caco3_diss)
            decay_dust  = exp(-dz_loc / dust_diss)

!-----------------------------------------------------------------------
!  Set outgoing fluxes for non-iron pools.
!  The outoing fluxes for ballast materials are from the
!  solution of the coresponding continuous ODE across the model
!  level. The ODE has a constant source term and linear decay.
!  It is assumed that there is no sub-surface dust production.
!-----------------------------------------------------------------------

            P_CaCO3%sflux_out(i,j,bid) = P_CaCO3%sflux_in(i,j,bid) * decay_CaCO3 + &
               P_CaCO3%prod(i,j,bid) * ((c1 - P_CaCO3%gamma) * (c1 - decay_CaCO3) &
                  * caco3_diss)

            P_CaCO3%hflux_out(i,j,bid) = P_CaCO3%hflux_in(i,j,bid) * DECAY_Hard(i,j) + &
               P_CaCO3%prod(i,j,bid) * (P_CaCO3%gamma * dz_loc)

            P_SiO2%sflux_out(i,j,bid) = P_SiO2%sflux_in(i,j,bid) * decay_SiO2 + &
               P_SiO2%prod(i,j,bid) * ((c1 - P_SiO2%gamma) * (c1 - decay_SiO2) &
                  * sio2_diss)

            P_SiO2%hflux_out(i,j,bid) = P_SiO2%hflux_in(i,j,bid) * DECAY_Hard(i,j) + &
               P_SiO2%prod(i,j,bid) * (P_SiO2%gamma * dz_loc)

            dust%sflux_out(i,j,bid) = dust%sflux_in(i,j,bid) * decay_dust

            dust%hflux_out(i,j,bid) = dust%hflux_in(i,j,bid) * DECAY_HardDust(i,j)

!-----------------------------------------------------------------------
!  Compute how much POC_PROD is available for deficit reduction
!  and excess POC flux after subtracting off fraction of non-dust
!  ballast production from net POC_PROD.
!-----------------------------------------------------------------------

            POC_PROD_avail = POC%prod(i,j,bid) - &
               P_CaCO3%rho * P_CaCO3%prod(i,j,bid) - &
               P_SiO2%rho * P_SiO2%prod(i,j,bid)

!-----------------------------------------------------------------------
!  Check for POC production bounds violations
!-----------------------------------------------------------------------

            if (POC_PROD_avail < c0) then
               poc_error = .true.
            endif

!-----------------------------------------------------------------------
!  Compute 1st approximation to new QA_dust_def, the QA_dust
!  deficit leaving the cell. Ignore POC_PROD_avail at this stage.
!-----------------------------------------------------------------------

            if (QA_dust_def(i,j,bid) > 0) then
               new_QA_dust_def = QA_dust_def(i,j,bid) * &
                  (dust%sflux_out(i,j,bid) + dust%hflux_out(i,j,bid)) / &
                  (dust%sflux_in(i,j,bid) + dust%hflux_in(i,j,bid))
            else
               new_QA_dust_def = c0
            endif

!-----------------------------------------------------------------------
!  Use POC_PROD_avail to reduce new_QA_dust_def.
!-----------------------------------------------------------------------

            if (new_QA_dust_def > c0) then
               new_QA_dust_def = new_QA_dust_def - POC_PROD_avail * dz_loc
               if (new_QA_dust_def < c0) then
                  POC_PROD_avail = -new_QA_dust_def * dzr_loc
                  new_QA_dust_def = c0
               else
                  POC_PROD_avail = c0
               endif
            endif

            QA_dust_def(i,j,bid) = new_QA_dust_def

! Save certain fields for use by other modules
            if (lexport_shared_vars) then
               POC_PROD_avail_fields(i,j,bid) = POC_PROD_avail
               decay_POC_E_fields(i,j,bid)    = decay_POC_E
               decay_CaCO3_fields(i,j,bid)    = decay_CaCO3
               poc_diss_fields(i,j,bid)       = poc_diss
               caco3_diss_fields(i,j,bid)     = caco3_diss
            endif

!-----------------------------------------------------------------------
!  Compute outgoing POC fluxes. QA POC flux is computing using
!  ballast fluxes and new_QA_dust_def. If no QA POC flux came in
!  and no production occured, then no QA POC flux goes out. This
!  shortcut is present to avoid roundoff cancellation errors from
!  the dust%rho * dust_flux_out - QA_dust_def computation.
!  Any POC_PROD_avail still remaining goes into excess POC flux.
!-----------------------------------------------------------------------

            if (POC%hflux_in(i,j,bid) == c0 .and. POC%prod(i,j,bid) == c0) then
               POC%hflux_out(i,j,bid) = c0
            else
               POC%hflux_out(i,j,bid) = P_CaCO3%rho * &
                  (P_CaCO3%sflux_out(i,j,bid) + P_CaCO3%hflux_out(i,j,bid)) + &
                  P_SiO2%rho * &
                  (P_SiO2%sflux_out(i,j,bid) + P_SiO2%hflux_out(i,j,bid)) + &
                  dust%rho * &
                  (dust%sflux_out(i,j,bid) + dust%hflux_out(i,j,bid)) - &
                  new_QA_dust_def
               POC%hflux_out(i,j,bid) = max(POC%hflux_out(i,j,bid), c0)
            endif

            POC%sflux_out(i,j,bid) = POC%sflux_in(i,j,bid) * decay_POC_E + &
               POC_PROD_avail *((c1 - decay_POC_E) * &
               poc_diss)

!-----------------------------------------------------------------------
!  Compute remineralization terms. It is assumed that there is no
!  sub-surface dust production.
!-----------------------------------------------------------------------

            P_CaCO3%remin(i,j,bid) = P_CaCO3%prod(i,j,bid) + &
               ((P_CaCO3%sflux_in(i,j,bid) - P_CaCO3%sflux_out(i,j,bid)) + &
               (P_CaCO3%hflux_in(i,j,bid) - P_CaCO3%hflux_out(i,j,bid))) * dzr_loc

            P_SiO2%remin(i,j,bid) = P_SiO2%prod(i,j,bid) + &
               ((P_SiO2%sflux_in(i,j,bid) - P_SiO2%sflux_out(i,j,bid)) + &
               (P_SiO2%hflux_in(i,j,bid) - P_SiO2%hflux_out(i,j,bid))) * dzr_loc

            POC%remin(i,j,bid) = POC%prod(i,j,bid) + &
               ((POC%sflux_in(i,j,bid) - POC%sflux_out(i,j,bid)) + &
               (POC%hflux_in(i,j,bid) - POC%hflux_out(i,j,bid))) * dzr_loc

            dust%remin(i,j,bid) = &
               ((dust%sflux_in(i,j,bid) - dust%sflux_out(i,j,bid)) + &
               (dust%hflux_in(i,j,bid) - dust%hflux_out(i,j,bid))) * dzr_loc

!-----------------------------------------------------------------------
!  Compute iron remineralization and flux out.
!-----------------------------------------------------------------------

            desorp_a = 6.25e-5_r8
            desorp_f = desorp_a * dz_loc

            if (desorp_f .ge. 1.0e0_r8) then
              desorp_a = 1.0e0_r8 * dzr_loc
            end if

#ifndef FE_TAG
            if (POC%sflux_in(i,j,bid) + POC%hflux_in(i,j,bid) == c0) then
               P_iron%remin(i,j,bid) = (POC%remin(i,j,bid) * parm_Red_Fe_C)
            else
               P_iron%remin(i,j,bid) = (POC%remin(i,j,bid) * &
                  (P_iron%sflux_in(i,j,bid) + P_iron%hflux_in(i,j,bid)) / &
                  (POC%sflux_in(i,j,bid) + POC%hflux_in(i,j,bid)))
            endif
# ifdef DIAGNOSTICS_BIO
            aFe_premin(i,j)=P_iron%remin(i,j,bid)
# endif

            P_iron%remin(i,j,bid) = P_iron%remin(i,j,bid) +                &
                                    (P_iron%sflux_in(i,j,bid) * desorp_a)

            P_iron%sflux_out(i,j,bid) = P_iron%sflux_in(i,j,bid) + dz_loc * &
               ((c1 - P_iron%gamma) * P_iron%prod(i,j,bid) - P_iron%remin(i,j,bid))

            if (P_iron%sflux_out(i,j,bid) < c0) then
               P_iron%sflux_out(i,j,bid) = c0
               P_iron%remin(i,j,bid) = P_iron%sflux_in(i,j,bid) * dzr_loc + &
                  (c1 - P_iron%gamma) * P_iron%prod(i,j,bid)
            endif

# ifdef DIAGNOSTICS_BIO
            aFe_disag(i,j)=P_iron%remin(i,j,bid)-aFe_premin(i,j)
# endif
#else
            if (POC%sflux_in(i,j,bid) + POC%hflux_in(i,j,bid) == c0) then
               P_iron%remin(i,j,bid) = (POC%remin(i,j,bid) * parm_Red_Fe_C)

               ! This part cannot be scaled by P_ironX due to no flux.

               s0 = wgt(i,j,1)
               s1 = wgt(i,j,2)
               s2 = wgt(i,j,3)
               s3 = wgt(i,j,4)
               s4 = wgt(i,j,5)
               s5 = wgt(i,j,6)
               s6 = wgt(i,j,7)
               s7 = wgt(i,j,8)
               s=s0+s1+s2+s3+s4+s5+s6+s7

               if (s>c0) then
                  P_iron0%remin(i,j,bid) = P_iron%remin(i,j,bid) * s0/s
                  P_iron1%remin(i,j,bid) = P_iron%remin(i,j,bid) * s1/s
                  P_iron2%remin(i,j,bid) = P_iron%remin(i,j,bid) * s2/s
                  P_iron3%remin(i,j,bid) = P_iron%remin(i,j,bid) * s3/s
                  P_iron4%remin(i,j,bid) = P_iron%remin(i,j,bid) * s4/s
                  P_iron5%remin(i,j,bid) = P_iron%remin(i,j,bid) * s5/s
                  P_iron6%remin(i,j,bid) = P_iron%remin(i,j,bid) * s6/s
                  P_iron7%remin(i,j,bid) = P_iron%remin(i,j,bid) * s7/s
               else
                  P_iron0%remin(i,j,bid) = c0
                  P_iron1%remin(i,j,bid) = c0
                  P_iron2%remin(i,j,bid) = c0
                  P_iron3%remin(i,j,bid) = c0
                  P_iron4%remin(i,j,bid) = c0
                  P_iron5%remin(i,j,bid) = c0
                  P_iron6%remin(i,j,bid) = c0
                  P_iron7%remin(i,j,bid) = c0
               end if

            else
               P_iron%remin(i,j,bid) = (POC%remin(i,j,bid) * &
                  (P_iron%sflux_in(i,j,bid) + P_iron%hflux_in(i,j,bid)) / &
                  (POC%sflux_in(i,j,bid) + POC%hflux_in(i,j,bid)))

               s0 = P_iron0%sflux_in(i,j,bid) + P_iron0%hflux_in(i,j,bid)
               s1 = P_iron1%sflux_in(i,j,bid) + P_iron1%hflux_in(i,j,bid)
               s2 = P_iron2%sflux_in(i,j,bid) + P_iron2%hflux_in(i,j,bid)
               s3 = P_iron3%sflux_in(i,j,bid) + P_iron3%hflux_in(i,j,bid)
               s4 = P_iron4%sflux_in(i,j,bid) + P_iron4%hflux_in(i,j,bid)
               s5 = P_iron5%sflux_in(i,j,bid) + P_iron5%hflux_in(i,j,bid)
               s6 = P_iron6%sflux_in(i,j,bid) + P_iron6%hflux_in(i,j,bid)
               s7 = P_iron7%sflux_in(i,j,bid) + P_iron7%hflux_in(i,j,bid)
               s=s0+s1+s2+s3+s4+s5+s6+s7

               if (s>c0) then
                  P_iron0%remin(i,j,bid) = P_iron%remin(i,j,bid) * s0/s
                  P_iron1%remin(i,j,bid) = P_iron%remin(i,j,bid) * s1/s
                  P_iron2%remin(i,j,bid) = P_iron%remin(i,j,bid) * s2/s
                  P_iron3%remin(i,j,bid) = P_iron%remin(i,j,bid) * s3/s
                  P_iron4%remin(i,j,bid) = P_iron%remin(i,j,bid) * s4/s
                  P_iron5%remin(i,j,bid) = P_iron%remin(i,j,bid) * s5/s
                  P_iron6%remin(i,j,bid) = P_iron%remin(i,j,bid) * s6/s
                  P_iron7%remin(i,j,bid) = P_iron%remin(i,j,bid) * s7/s
               else
                  P_iron0%remin(i,j,bid) = c0
                  P_iron1%remin(i,j,bid) = c0
                  P_iron2%remin(i,j,bid) = c0
                  P_iron3%remin(i,j,bid) = c0
                  P_iron4%remin(i,j,bid) = c0
                  P_iron5%remin(i,j,bid) = c0
                  P_iron6%remin(i,j,bid) = c0
                  P_iron7%remin(i,j,bid) = c0
               end if

            endif

# ifdef DIAGNOSTICS_BIO
            aFe0_premin(i,j)=P_iron0%remin(i,j,bid)
            aFe1_premin(i,j)=P_iron1%remin(i,j,bid)
            aFe2_premin(i,j)=P_iron2%remin(i,j,bid)
            aFe3_premin(i,j)=P_iron3%remin(i,j,bid)
            aFe4_premin(i,j)=P_iron4%remin(i,j,bid)
            aFe5_premin(i,j)=P_iron5%remin(i,j,bid)
            aFe6_premin(i,j)=P_iron6%remin(i,j,bid)
            aFe7_premin(i,j)=P_iron7%remin(i,j,bid)
# endif

            P_iron%remin(i,j,bid) = P_iron%remin(i,j,bid) +                &
                  (P_iron%sflux_in(i,j,bid) * desorp_a)

            s0 = P_iron0%remin(i,j,bid) + P_iron0%sflux_in(i,j,bid) * desorp_a
            s1 = P_iron1%remin(i,j,bid) + P_iron1%sflux_in(i,j,bid) * desorp_a
            s2 = P_iron2%remin(i,j,bid) + P_iron2%sflux_in(i,j,bid) * desorp_a
            s3 = P_iron3%remin(i,j,bid) + P_iron3%sflux_in(i,j,bid) * desorp_a
            s4 = P_iron4%remin(i,j,bid) + P_iron4%sflux_in(i,j,bid) * desorp_a
            s5 = P_iron5%remin(i,j,bid) + P_iron5%sflux_in(i,j,bid) * desorp_a
            s6 = P_iron6%remin(i,j,bid) + P_iron6%sflux_in(i,j,bid) * desorp_a
            s7 = P_iron7%remin(i,j,bid) + P_iron7%sflux_in(i,j,bid) * desorp_a
            s=s0+s1+s2+s3+s4+s5+s6+s7

            if (s>c0) then
               P_iron0%remin(i,j,bid) = P_iron%remin(i,j,bid) * s0/s
               P_iron1%remin(i,j,bid) = P_iron%remin(i,j,bid) * s1/s
               P_iron2%remin(i,j,bid) = P_iron%remin(i,j,bid) * s2/s
               P_iron3%remin(i,j,bid) = P_iron%remin(i,j,bid) * s3/s
               P_iron4%remin(i,j,bid) = P_iron%remin(i,j,bid) * s4/s
               P_iron5%remin(i,j,bid) = P_iron%remin(i,j,bid) * s5/s
               P_iron6%remin(i,j,bid) = P_iron%remin(i,j,bid) * s6/s
               P_iron7%remin(i,j,bid) = P_iron%remin(i,j,bid) * s7/s
            else
               P_iron0%remin(i,j,bid) = c0
               P_iron1%remin(i,j,bid) = c0
               P_iron2%remin(i,j,bid) = c0
               P_iron3%remin(i,j,bid) = c0
               P_iron4%remin(i,j,bid) = c0
               P_iron5%remin(i,j,bid) = c0
               P_iron6%remin(i,j,bid) = c0
               P_iron7%remin(i,j,bid) = c0
            end if

            P_iron%sflux_out(i,j,bid) = P_iron%sflux_in(i,j,bid) + dz_loc * &
               ((c1 - P_iron%gamma) * P_iron%prod(i,j,bid) - P_iron%remin(i,j,bid))

            s0 = max(c0,P_iron0%sflux_in(i,j,bid) + dz_loc * ((c1 - P_iron0%gamma) * P_iron0%prod(i,j,bid) - P_iron0%remin(i,j,bid)))
            s1 = max(c0,P_iron1%sflux_in(i,j,bid) + dz_loc * ((c1 - P_iron1%gamma) * P_iron1%prod(i,j,bid) - P_iron1%remin(i,j,bid)))
            s2 = max(c0,P_iron2%sflux_in(i,j,bid) + dz_loc * ((c1 - P_iron2%gamma) * P_iron2%prod(i,j,bid) - P_iron2%remin(i,j,bid)))
            s3 = max(c0,P_iron3%sflux_in(i,j,bid) + dz_loc * ((c1 - P_iron3%gamma) * P_iron3%prod(i,j,bid) - P_iron3%remin(i,j,bid)))
            s4 = max(c0,P_iron4%sflux_in(i,j,bid) + dz_loc * ((c1 - P_iron4%gamma) * P_iron4%prod(i,j,bid) - P_iron4%remin(i,j,bid)))
            s5 = max(c0,P_iron5%sflux_in(i,j,bid) + dz_loc * ((c1 - P_iron5%gamma) * P_iron5%prod(i,j,bid) - P_iron5%remin(i,j,bid)))
            s6 = max(c0,P_iron6%sflux_in(i,j,bid) + dz_loc * ((c1 - P_iron6%gamma) * P_iron6%prod(i,j,bid) - P_iron6%remin(i,j,bid)))
            s7 = max(c0,P_iron7%sflux_in(i,j,bid) + dz_loc * ((c1 - P_iron7%gamma) * P_iron7%prod(i,j,bid) - P_iron7%remin(i,j,bid)))
            s=s0+s1+s2+s3+s4+s5+s6+s7

            if (s>c0) then
               P_iron0%sflux_out(i,j,bid) = P_iron%sflux_out(i,j,bid) * s0/s
               P_iron1%sflux_out(i,j,bid) = P_iron%sflux_out(i,j,bid) * s1/s
               P_iron2%sflux_out(i,j,bid) = P_iron%sflux_out(i,j,bid) * s2/s
               P_iron3%sflux_out(i,j,bid) = P_iron%sflux_out(i,j,bid) * s3/s
               P_iron4%sflux_out(i,j,bid) = P_iron%sflux_out(i,j,bid) * s4/s
               P_iron5%sflux_out(i,j,bid) = P_iron%sflux_out(i,j,bid) * s5/s
               P_iron6%sflux_out(i,j,bid) = P_iron%sflux_out(i,j,bid) * s6/s
               P_iron7%sflux_out(i,j,bid) = P_iron%sflux_out(i,j,bid) * s7/s
            else
               P_iron0%sflux_out(i,j,bid) = c0
               P_iron1%sflux_out(i,j,bid) = c0
               P_iron2%sflux_out(i,j,bid) = c0
               P_iron3%sflux_out(i,j,bid) = c0
               P_iron4%sflux_out(i,j,bid) = c0
               P_iron5%sflux_out(i,j,bid) = c0
               P_iron6%sflux_out(i,j,bid) = c0
               P_iron7%sflux_out(i,j,bid) = c0
            end if

            if (P_iron%sflux_out(i,j,bid) < c0) then
               P_iron%sflux_out(i,j,bid) = c0
               P_iron%remin(i,j,bid) = P_iron%sflux_in(i,j,bid) * dzr_loc + &
                  (c1 - P_iron%gamma) * P_iron%prod(i,j,bid)

               s0 = max(c0,P_iron0%sflux_in(i,j,bid) * dzr_loc + (c1 - P_iron0%gamma) * P_iron0%prod(i,j,bid))
               s1 = max(c0,P_iron1%sflux_in(i,j,bid) * dzr_loc + (c1 - P_iron1%gamma) * P_iron1%prod(i,j,bid))
               s2 = max(c0,P_iron2%sflux_in(i,j,bid) * dzr_loc + (c1 - P_iron2%gamma) * P_iron2%prod(i,j,bid))
               s3 = max(c0,P_iron3%sflux_in(i,j,bid) * dzr_loc + (c1 - P_iron3%gamma) * P_iron3%prod(i,j,bid))
               s4 = max(c0,P_iron4%sflux_in(i,j,bid) * dzr_loc + (c1 - P_iron4%gamma) * P_iron4%prod(i,j,bid))
               s5 = max(c0,P_iron5%sflux_in(i,j,bid) * dzr_loc + (c1 - P_iron5%gamma) * P_iron5%prod(i,j,bid))
               s6 = max(c0,P_iron6%sflux_in(i,j,bid) * dzr_loc + (c1 - P_iron6%gamma) * P_iron6%prod(i,j,bid))
               s7 = max(c0,P_iron7%sflux_in(i,j,bid) * dzr_loc + (c1 - P_iron7%gamma) * P_iron7%prod(i,j,bid))
               s=s0+s1+s2+s3+s4+s5+s6+s7

               P_iron0%sflux_out(i,j,bid) = c0
               P_iron1%sflux_out(i,j,bid) = c0
               P_iron2%sflux_out(i,j,bid) = c0
               P_iron3%sflux_out(i,j,bid) = c0
               P_iron4%sflux_out(i,j,bid) = c0
               P_iron5%sflux_out(i,j,bid) = c0
               P_iron6%sflux_out(i,j,bid) = c0
               P_iron7%sflux_out(i,j,bid) = c0

               if (s>c0) then
                  P_iron0%remin(i,j,bid) = P_iron%remin(i,j,bid) * s0/s
                  P_iron1%remin(i,j,bid) = P_iron%remin(i,j,bid) * s1/s
                  P_iron2%remin(i,j,bid) = P_iron%remin(i,j,bid) * s2/s
                  P_iron3%remin(i,j,bid) = P_iron%remin(i,j,bid) * s3/s
                  P_iron4%remin(i,j,bid) = P_iron%remin(i,j,bid) * s4/s
                  P_iron5%remin(i,j,bid) = P_iron%remin(i,j,bid) * s5/s
                  P_iron6%remin(i,j,bid) = P_iron%remin(i,j,bid) * s6/s
                  P_iron7%remin(i,j,bid) = P_iron%remin(i,j,bid) * s7/s
               else
                  P_iron0%remin(i,j,bid) = c0
                  P_iron1%remin(i,j,bid) = c0
                  P_iron2%remin(i,j,bid) = c0
                  P_iron3%remin(i,j,bid) = c0
                  P_iron4%remin(i,j,bid) = c0
                  P_iron5%remin(i,j,bid) = c0
                  P_iron6%remin(i,j,bid) = c0
                  P_iron7%remin(i,j,bid) = c0
               end if
            endif

# ifdef DIAGNOSTICS_BIO
            aFe0_disag(i,j)=P_iron0%remin(i,j,bid)-aFe0_premin(i,j)
            aFe1_disag(i,j)=P_iron1%remin(i,j,bid)-aFe1_premin(i,j)
            aFe2_disag(i,j)=P_iron2%remin(i,j,bid)-aFe2_premin(i,j)
            aFe3_disag(i,j)=P_iron3%remin(i,j,bid)-aFe3_premin(i,j)
            aFe4_disag(i,j)=P_iron4%remin(i,j,bid)-aFe4_premin(i,j)
            aFe5_disag(i,j)=P_iron5%remin(i,j,bid)-aFe5_premin(i,j)
            aFe6_disag(i,j)=P_iron6%remin(i,j,bid)-aFe6_premin(i,j)
            aFe7_disag(i,j)=P_iron7%remin(i,j,bid)-aFe7_premin(i,j)
# endif
#endif

!c!-----------------------------------------------------------------------
!c!  Compute iron release from dust remin/dissolution
!c!
!c!  dust remin gDust = 0.035 / 55.847 * 1.0e9 = 626712.0 nmolFe
!c!                      gFe     molFe     nmolFe
!c!  Also add in Fe source from sediments if applicable to this cell.
!c!-----------------------------------------------------------------------

            P_iron%remin(i,j,bid) = P_iron%remin(i,j,bid) &
               + dust%remin(i,j,bid) * dust_to_Fe &
               + (FESEDFLUX(i,j,k,bid) * dzr_loc)

            P_iron%hflux_out(i,j,bid) = P_iron%hflux_in(i,j,bid)

         else
            P_CaCO3%sflux_out(i,j,bid) = c0
            P_CaCO3%hflux_out(i,j,bid) = c0
            P_CaCO3%remin(i,j,bid) = c0

            P_SiO2%sflux_out(i,j,bid) = c0
            P_SiO2%hflux_out(i,j,bid) = c0
            P_SiO2%remin(i,j,bid) = c0

            dust%sflux_out(i,j,bid) = c0
            dust%hflux_out(i,j,bid) = c0
            dust%remin(i,j,bid) = c0

            POC%sflux_out(i,j,bid) = c0
            POC%hflux_out(i,j,bid) = c0
            POC%remin(i,j,bid) = c0
#ifndef FE_TAG
            P_iron%sflux_out(i,j,bid) = c0
            P_iron%hflux_out(i,j,bid) = c0
            P_iron%remin(i,j,bid) = c0
#else
            P_iron%sflux_out(i,j,bid) = c0
            P_iron%hflux_out(i,j,bid) = c0
            P_iron%remin(i,j,bid) = c0

            P_iron0%sflux_out(i,j,bid) = c0
            P_iron0%hflux_out(i,j,bid) = c0
            P_iron0%remin(i,j,bid) = c0

            P_iron1%sflux_out(i,j,bid) = c0
            P_iron1%hflux_out(i,j,bid) = c0
            P_iron1%remin(i,j,bid) = c0

            P_iron2%sflux_out(i,j,bid) = c0
            P_iron2%hflux_out(i,j,bid) = c0
            P_iron2%remin(i,j,bid) = c0

            P_iron3%sflux_out(i,j,bid) = c0
            P_iron3%hflux_out(i,j,bid) = c0
            P_iron3%remin(i,j,bid) = c0

            P_iron4%sflux_out(i,j,bid) = c0
            P_iron4%hflux_out(i,j,bid) = c0
            P_iron4%remin(i,j,bid) = c0

            P_iron5%sflux_out(i,j,bid) = c0
            P_iron5%hflux_out(i,j,bid) = c0
            P_iron5%remin(i,j,bid) = c0

            P_iron6%sflux_out(i,j,bid) = c0
            P_iron6%hflux_out(i,j,bid) = c0
            P_iron6%remin(i,j,bid) = c0

            P_iron7%sflux_out(i,j,bid) = c0
            P_iron7%hflux_out(i,j,bid) = c0
            P_iron7%remin(i,j,bid) = c0
#endif
         endif

! Save some fields for use by other modules before setting outgoing fluxes to 0.0 in bottom cell below
         if (lexport_shared_vars) then
            P_CaCO3_sflux_out_fields(i,j,bid) = P_CaCO3%sflux_out(i,j,bid)
            P_CaCO3_hflux_out_fields(i,j,bid) = P_CaCO3%hflux_out(i,j,bid)
            POC_sflux_out_fields(i,j,bid)     = POC%sflux_out(i,j,bid)
            POC_hflux_out_fields(i,j,bid)     = POC%hflux_out(i,j,bid)
            POC_remin_fields(i,j,bid)         = POC%remin(i,j,bid)
            P_CaCO3_remin_fields(i,j,bid)     = P_CaCO3%remin(i,j,bid)
         endif

!-----------------------------------------------------------------------
!  Bottom Sediments Cell?
!  If so compute sedimentary burial and denitrification N losses.
!  Using empirical relations from Bohlen et al., 2012 (doi:10.1029/2011GB004198) for Sed Denitrification
!  OTHER_REMIN estimates organic matter remineralized in the sediments
!      by the processes other than oxic remin and denitrification (SO4 and CO2,
!      etc..)
!      based on Soetaert et al., 1996, varies between 10% and 50%
!      0.4_r8 is a coefficient with units mmolC/cm2/yr sinking flux,
!      OTHER_REMIN is 50% above this high flux value,
!      In special case where bottom O2 has been depleted to < 1.0 uM,
!               all sedimentary remin is due to DENITRIFICATION + OTHER_REMIN
!  POC burial from Dunne et al. 2007 (doi:10.1029/2006GB002907), maximum of 80% burial efficiency imposed
!  Bsi preservation in sediments based on
!     Ragueneau et al. 2000 (doi:10.1016/S0921-8181(00)00052-7)
!  Calcite is preserved in sediments above the lysocline, dissolves below.
!       Here a constant depth is used for lysocline.
!-----------------------------------------------------------------------

         if (LAND_MASK(i,j,bid) .and. (k == KMT(i,j,bid))) then

            flux = POC%sflux_out(i,j,bid)+POC%hflux_out(i,j,bid)

            if (flux > c0) then
               flux_alt = flux*mpercm*spd ! convert to mmol/m^2/day

               POC%sed_loss(i,j,bid) = flux * min(0.8_r8, parm_POMbury &
                  * (0.013_r8 + 0.53_r8 * flux_alt*flux_alt / (7.0_r8 + flux_alt)**2))

               SED_DENITRIF(i,j,bid) = dzr_loc * flux &
                  * (0.06_r8 + 0.19_r8 * 0.99_r8**(O2_loc(i,j)-NO3_loc(i,j)))

               flux_alt = flux*1.0e-6_r8*spd*365.0_r8 ! convert to mmol/cm^2/year
               OTHER_REMIN(i,j,bid) = dzr_loc &
                  * min(min(0.1_r8 + flux_alt,0.5_r8) * (flux - POC%sed_loss(i,j,bid)), &
                        (flux - POC%sed_loss(i,j,bid) - (SED_DENITRIF(i,j,bid)*dz_loc*denitrif_C_N)))

!----------------------------------------------------------------------------------
!              if bottom water O2 is depleted, assume all remin is denitrif + other
!----------------------------------------------------------------------------------

               if (O2_loc(i,j) < c1) then
                  OTHER_REMIN(i,j,bid) = dzr_loc * &
                     (flux - POC%sed_loss(i,j,bid) - (SED_DENITRIF(i,j,bid)*dz_loc*denitrif_C_N))
               endif

            endif

            flux = P_SiO2%sflux_out(i,j,bid)+P_SiO2%hflux_out(i,j,bid)
            flux_alt = flux*mpercm*spd ! convert to mmol/m^2/day
            ! first compute burial efficiency, then compute loss to sediments
            if (flux_alt > c2) then
               P_SiO2%sed_loss(i,j,bid) = 0.2_r8
            else
               P_SiO2%sed_loss(i,j,bid) = 0.04_r8
            endif
            P_SiO2%sed_loss(i,j,bid) = flux * parm_BSIbury * P_SiO2%sed_loss(i,j,bid)

            if (zw(i,j,k) < 3300.0e2_r8) then
               flux = P_CaCO3%sflux_out(i,j,bid) + P_CaCO3%hflux_out(i,j,bid)
               P_CaCO3%sed_loss(i,j,bid) = flux
            endif

!----------------------------------------------------------------------------------
!  Update sinking fluxes and remin fluxes, accounting for sediments.
!  flux used to hold sinking fluxes before update.
!----------------------------------------------------------------------------------

            flux = P_CaCO3%sflux_out(i,j,bid) + P_CaCO3%hflux_out(i,j,bid)
            if (flux > c0) then
               P_CaCO3%remin(i,j,bid) = P_CaCO3%remin(i,j,bid) &
                  + ((flux - P_CaCO3%sed_loss(i,j,bid)) * dzr_loc)
            endif

            flux = P_SiO2%sflux_out(i,j,bid) + P_SiO2%hflux_out(i,j,bid)
            if (flux > c0) then
               P_SiO2%remin(i,j,bid) = P_SiO2%remin(i,j,bid) &
                  + ((flux - P_SiO2%sed_loss(i,j,bid)) * dzr_loc)
            endif

            flux = POC%sflux_out(i,j,bid) + POC%hflux_out(i,j,bid)
            if (flux > c0) then
               POC%remin(i,j,bid) = POC%remin(i,j,bid) &
                  + ((flux - POC%sed_loss(i,j,bid)) * dzr_loc)
            endif

!-----------------------------------------------------------------------
!   Remove all Piron and dust that hits bottom, sedimentary Fe source
!        accounted for by FESEDFLUX elsewhere.
!-----------------------------------------------------------------------

            flux = (P_iron%sflux_out(i,j,bid) + P_iron%hflux_out(i,j,bid))
            if (flux > c0) then
               P_iron%sed_loss(i,j,bid) = flux
            endif

            dust%sed_loss(i,j,bid) = dust%sflux_out(i,j,bid) + dust%hflux_out(i,j,bid)

!-----------------------------------------------------------------------
!   Set all outgoing fluxes to 0.0
!-----------------------------------------------------------------------

            if (k == KMT(i,j,bid)) then
               P_CaCO3%sflux_out(i,j,bid) = c0
               P_CaCO3%hflux_out(i,j,bid) = c0

               P_SiO2%sflux_out(i,j,bid) = c0
               P_SiO2%hflux_out(i,j,bid) = c0

               dust%sflux_out(i,j,bid) = c0
               dust%hflux_out(i,j,bid) = c0

               POC%sflux_out(i,j,bid) = c0
               POC%hflux_out(i,j,bid) = c0

               P_iron%sflux_out(i,j,bid) = c0
               P_iron%hflux_out(i,j,bid) = c0
            endif

         endif

      end do
   end do

#ifdef CCSMCOUPLED
    if (poc_error) then
       call shr_sys_abort(subname /&
                       &/ ': mass ratio of ballast ' /&
                       &/ 'production exceeds POC production')
    endif
#endif

!-----------------------------------------------------------------------
!  Set tavg variables.
!-----------------------------------------------------------------------

!c   if (accumulate_tavg_now(tavg_POC_FLUX_IN)) then
!c      WORK = POC%sflux_in(:,:,bid) + POC%hflux_in(:,:,bid)
!c      call accumulate_tavg_field(WORK, tavg_POC_FLUX_IN,bid,k)
!c   endif
!c
!c   call accumulate_tavg_field(POC%prod(:,:,bid), tavg_POC_PROD,bid,k)
!c
!c   call accumulate_tavg_field(POC%remin(:,:,bid), tavg_POC_REMIN,bid,k)
!c
!c   if (accumulate_tavg_now(tavg_CaCO3_FLUX_IN)) then
!c      WORK = P_CaCO3%sflux_in(:,:,bid) + P_CaCO3%hflux_in(:,:,bid)
!c      call accumulate_tavg_field(WORK, tavg_CaCO3_FLUX_IN,bid,k)
!c   endif
!c
!c   call accumulate_tavg_field(P_CaCO3%prod(:,:,bid), tavg_CaCO3_PROD,bid,k)
!c
!c   call accumulate_tavg_field(P_CaCO3%remin(:,:,bid), tavg_CaCO3_REMIN,bid,k)
!c
!c   if (accumulate_tavg_now(tavg_SiO2_FLUX_IN)) then
!c      WORK = P_SiO2%sflux_in(:,:,bid) + P_SiO2%hflux_in(:,:,bid)
!c      call accumulate_tavg_field(WORK, tavg_SiO2_FLUX_IN,bid,k)
!c   endif
!c
!c   call accumulate_tavg_field(P_SiO2%prod(:,:,bid), tavg_SiO2_PROD,bid,k)
!c
!c   call accumulate_tavg_field(P_SiO2%remin(:,:,bid), tavg_SiO2_REMIN,bid,k)
!c
!c   if (accumulate_tavg_now(tavg_dust_FLUX_IN)) then
!c      WORK = dust%sflux_in(:,:,bid) + dust%hflux_in(:,:,bid)
!c      call accumulate_tavg_field(WORK, tavg_dust_FLUX_IN,bid,k)
!c   endif
!c
!c   call accumulate_tavg_field(dust%remin(:,:,bid), tavg_dust_REMIN,bid,k)
!c
!c   if (accumulate_tavg_now(tavg_P_iron_FLUX_IN)) then
!c      WORK = P_iron%sflux_in(:,:,bid) + P_iron%hflux_in(:,:,bid)
!c      call accumulate_tavg_field(WORK, tavg_P_iron_FLUX_IN,bid,k)
!c   endif
!c
!c   call accumulate_tavg_field(P_iron%prod(:,:,bid), tavg_P_iron_PROD,bid,k)
!c
!c   call accumulate_tavg_field(P_iron%remin(:,:,bid), tavg_P_iron_REMIN,bid,k)
!c
!c! ***********************************************************************
!c! - Accumulte losses of BGC tracers to sediments
!c! ***********************************************************************
!c
!c   call accumulate_tavg_field(P_CaCO3%sed_loss(:,:,bid), tavg_calcToSed,bid,k)
!c
!c   call accumulate_tavg_field(P_SiO2%sed_loss(:,:,bid), tavg_bsiToSed,bid,k)
!c
!c   call accumulate_tavg_field(POC%sed_loss(:,:,bid), tavg_pocToSed,bid,k)
!c
!c   WORK = SED_DENITRIF(:,:,bid) * dz_loc
!c   call accumulate_tavg_field(WORK, tavg_SedDenitrif,bid,k)
!c
!c   WORK = OTHER_REMIN(:,:,bid) * dz_loc
!c   call accumulate_tavg_field(WORK, tavg_OtherRemin,bid,k)
!c
!c   WORK = (POC%sed_loss(:,:,bid) * Q)
!c   call accumulate_tavg_field(WORK, tavg_ponToSed,bid,k)
!c
!c   WORK = (POC%sed_loss(:,:,bid) * Qp_zoo_pom)
!c   call accumulate_tavg_field(WORK, tavg_popToSed,bid,k)
!c
!c   call accumulate_tavg_field(dust%sed_loss(:,:,bid), tavg_dustToSed,bid,k)
!c
!c   call accumulate_tavg_field(P_iron%sed_loss(:,:,bid), tavg_pfeToSed,bid,k)
!c
   if (lexport_shared_vars) then
      DECAY_Hard_fields(:,:,bid) = DECAY_Hard
   end if
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
 end subroutine compute_particulate_terms

!***********************************************************************
!BOP
! !IROUTINE: ecosys_init_sflux
! !INTERFACE:

 subroutine ecosys_init_sflux

! !DESCRIPTION:
!  Initialize surface flux computations for ecosys tracer module.
!
! !REVISION HISTORY:
!  same as module

!EOP
!BOC
!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

   character(*), parameter :: subname = 'ecosys_mod:ecosys_init_sflux'

   logical (log_kind) :: &
      luse_INTERP_WORK     ! does INTERP_WORK need to be allocated

   integer (int_kind) :: &
      n,                 & ! index for looping over tracers
      iblock               ! index for looping over blocks

   real (r8), dimension (nx_block,ny_block) :: WORK

   real (r8), dimension (nx_block,ny_block,12,max_blocks_clinic), target :: &
      WORK_READ            ! temporary space to read in fields

!-----------------------------------------------------------------------

   luse_INTERP_WORK = .false.

!c!-----------------------------------------------------------------------
!c!  read gas flux forcing (if required)
!c!-----------------------------------------------------------------------
!c
!c   if ((lflux_gas_o2 .or. lflux_gas_co2) .and. &
!c       gas_flux_forcing_iopt == gas_flux_forcing_iopt_file) then
!c
!c      luse_INTERP_WORK = .true.
!c
!c!-----------------------------------------------------------------------
!c!  first, read ice file
!c!-----------------------------------------------------------------------
!c
!c      allocate(fice_file%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c      if (trim(fice_file%input%filename) == 'unknown') &
!c         fice_file%input%filename = gas_flux_forcing_file
!c
!c      call read_field(fice_file%input%file_fmt, &
!c                      fice_file%input%filename, &
!c                      fice_file%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         fice_file%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            fice_file%DATA(:,:,iblock,1,n) = c0
!c         fice_file%DATA(:,:,iblock,1,n) = &
!c            fice_file%DATA(:,:,iblock,1,n) * fice_file%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(fice_file%data_time, &
!c                              fice_file%data_inc, fice_file%interp_type, &
!c                              fice_file%data_next, fice_file%data_time_min_loc, &
!c                              fice_file%data_update, fice_file%data_type)
!c
!c!-----------------------------------------------------------------------
!c!  next, read piston velocity file
!c!-----------------------------------------------------------------------
!c
!c      allocate(xkw_file%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c      if (trim(xkw_file%input%filename) == 'unknown') &
!c         xkw_file%input%filename = gas_flux_forcing_file
!c
!c      call read_field(xkw_file%input%file_fmt, &
!c                      xkw_file%input%filename, &
!c                      xkw_file%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         xkw_file%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            xkw_file%DATA(:,:,iblock,1,n) = c0
!c         xkw_file%DATA(:,:,iblock,1,n) = &
!c            xkw_file%DATA(:,:,iblock,1,n) * xkw_file%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(xkw_file%data_time, &
!c                              xkw_file%data_inc, xkw_file%interp_type, &
!c                              xkw_file%data_next, xkw_file%data_time_min_loc, &
!c                              xkw_file%data_update, xkw_file%data_type)
!c
!c!-----------------------------------------------------------------------
!c!  last, read atmospheric pressure file
!c!-----------------------------------------------------------------------
!c
!c      allocate(ap_file%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c      if (trim(ap_file%input%filename) == 'unknown') &
!c         ap_file%input%filename = gas_flux_forcing_file
!c
!c      call read_field(ap_file%input%file_fmt, &
!c                      ap_file%input%filename, &
!c                      ap_file%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         ap_file%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            ap_file%DATA(:,:,iblock,1,n) = c0
!c         ap_file%DATA(:,:,iblock,1,n) = &
!c            ap_file%DATA(:,:,iblock,1,n) * ap_file%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(ap_file%data_time, &
!c                              ap_file%data_inc, ap_file%interp_type, &
!c                              ap_file%data_next, ap_file%data_time_min_loc, &
!c                              ap_file%data_update, ap_file%data_type)
!c
!c    endif
!c
!c!-----------------------------------------------------------------------
!c!  load dust flux fields (if required)
!c!-----------------------------------------------------------------------
!c
!c   if (trim(dust_flux%input%filename) /= 'none' .and. &
!c       trim(dust_flux%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(dust_flux%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c      if (trim(dust_flux%input%filename) == 'unknown') &
!c         dust_flux%input%filename = gas_flux_forcing_file
!c
!c      call read_field(dust_flux%input%file_fmt, &
!c                      dust_flux%input%filename, &
!c                      dust_flux%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         dust_flux%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            dust_flux%DATA(:,:,iblock,1,n) = c0
!c         dust_flux%DATA(:,:,iblock,1,n) = &
!c            dust_flux%DATA(:,:,iblock,1,n) * dust_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(dust_flux%data_time, &
!c                              dust_flux%data_inc, dust_flux%interp_type, &
!c                              dust_flux%data_next, dust_flux%data_time_min_loc, &
!c                              dust_flux%data_update, dust_flux%data_type)
!c
!c      dust_flux%has_data = .true.
!c   else
!c      dust_flux%has_data = .false.
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!  load iron flux fields (if required)
!c!-----------------------------------------------------------------------
!c
!c   if (trim(iron_flux%input%filename) /= 'none' .and. &
!c       trim(iron_flux%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(iron_flux%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c      if (trim(iron_flux%input%filename) == 'unknown') &
!c         iron_flux%input%filename = gas_flux_forcing_file
!c
!c      call read_field(iron_flux%input%file_fmt, &
!c                      iron_flux%input%filename, &
!c                      iron_flux%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         iron_flux%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            iron_flux%DATA(:,:,iblock,1,n) = c0
!c         iron_flux%DATA(:,:,iblock,1,n) = &
!c            iron_flux%DATA(:,:,iblock,1,n) * iron_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(iron_flux%data_time, &
!c                              iron_flux%data_inc, iron_flux%interp_type, &
!c                              iron_flux%data_next, iron_flux%data_time_min_loc, &
!c                              iron_flux%data_update, iron_flux%data_type)
!c
!c      iron_flux%has_data = .true.
!c   else
!c      iron_flux%has_data = .false.
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!  load nox & noy flux fields (if required)
!c!-----------------------------------------------------------------------
!c
!c   if (trim(ndep_data_type) /= 'none' .and. &
!c       trim(ndep_data_type) /= 'monthly-calendar' .and. &
!c       trim(ndep_data_type) /= 'shr_stream') then
!c      call document(subname, 'ndep_data_type', ndep_data_type)
!c      call exit_POP(sigAbort, 'unknown ndep_data_type')
!c   endif
!c
!c   if (trim(ndep_data_type) == 'monthly-calendar' .and. &
!c       trim(nox_flux_monthly%input%filename) /= 'none' .and. &
!c       trim(nox_flux_monthly%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(nox_flux_monthly%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c      if (trim(nox_flux_monthly%input%filename) == 'unknown') &
!c         nox_flux_monthly%input%filename = gas_flux_forcing_file
!c
!c      call read_field(nox_flux_monthly%input%file_fmt, &
!c                      nox_flux_monthly%input%filename, &
!c                      nox_flux_monthly%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         nox_flux_monthly%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            nox_flux_monthly%DATA(:,:,iblock,1,n) = c0
!c         nox_flux_monthly%DATA(:,:,iblock,1,n) = &
!c            nox_flux_monthly%DATA(:,:,iblock,1,n) * nox_flux_monthly%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(nox_flux_monthly%data_time, &
!c                              nox_flux_monthly%data_inc, nox_flux_monthly%interp_type, &
!c                              nox_flux_monthly%data_next, nox_flux_monthly%data_time_min_loc, &
!c                              nox_flux_monthly%data_update, nox_flux_monthly%data_type)
!c
!c      nox_flux_monthly%has_data = .true.
!c   else
!c      nox_flux_monthly%has_data = .false.
!c   endif
!c
!c   if (trim(ndep_data_type) == 'monthly-calendar' .and. &
!c       trim(nhy_flux_monthly%input%filename) /= 'none' .and. &
!c       trim(nhy_flux_monthly%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(nhy_flux_monthly%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c      if (trim(nhy_flux_monthly%input%filename) == 'unknown') &
!c         nhy_flux_monthly%input%filename = gas_flux_forcing_file
!c
!c      call read_field(nhy_flux_monthly%input%file_fmt, &
!c                      nhy_flux_monthly%input%filename, &
!c                      nhy_flux_monthly%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         nhy_flux_monthly%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            nhy_flux_monthly%DATA(:,:,iblock,1,n) = c0
!c         nhy_flux_monthly%DATA(:,:,iblock,1,n) = &
!c            nhy_flux_monthly%DATA(:,:,iblock,1,n) * nhy_flux_monthly%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(nhy_flux_monthly%data_time, &
!c                              nhy_flux_monthly%data_inc, nhy_flux_monthly%interp_type, &
!c                              nhy_flux_monthly%data_next, nhy_flux_monthly%data_time_min_loc, &
!c                              nhy_flux_monthly%data_update, nhy_flux_monthly%data_type)
!c
!c      nhy_flux_monthly%has_data = .true.
!c   else
!c      nhy_flux_monthly%has_data = .false.
!c   endif
!c
!c!-----------------------------------------------------------------------
!c
!c#ifndef CCSMCOUPLED
!c   if (trim(ndep_data_type) == 'shr_stream') then
!c      call document(subname, 'ndep_data_type', ndep_data_type)
!c      call exit_POP(sigAbort, &
!c         'shr_stream option only supported when CCSMCOUPLED is defined')
!c   endif
!c#endif
!c
!c!-----------------------------------------------------------------------
!c!  load river nutrient flux fields (if required)
!c!-----------------------------------------------------------------------
!c
!c   if (trim(din_riv_flux%input%filename) /= 'none' .and. &
!c       trim(din_riv_flux%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(din_riv_flux%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c
!c      call read_field(din_riv_flux%input%file_fmt, &
!c                      din_riv_flux%input%filename, &
!c                      din_riv_flux%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         din_riv_flux%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            din_riv_flux%DATA(:,:,iblock,1,n) = c0
!c         din_riv_flux%DATA(:,:,iblock,1,n) = &
!c            din_riv_flux%DATA(:,:,iblock,1,n) * din_riv_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(din_riv_flux%data_time, &
!c                              din_riv_flux%data_inc, din_riv_flux%interp_type, &
!c                              din_riv_flux%data_next, din_riv_flux%data_time_min_loc, &
!c                              din_riv_flux%data_update, din_riv_flux%data_type)
!c
!c      din_riv_flux%has_data = .true.
!c   else
!c      din_riv_flux%has_data = .false.
!c   endif
!c
!c
!c   if (trim(dip_riv_flux%input%filename) /= 'none' .and. &
!c       trim(dip_riv_flux%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(dip_riv_flux%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c
!c      call read_field(dip_riv_flux%input%file_fmt, &
!c                      dip_riv_flux%input%filename, &
!c                      dip_riv_flux%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         dip_riv_flux%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            dip_riv_flux%DATA(:,:,iblock,1,n) = c0
!c         dip_riv_flux%DATA(:,:,iblock,1,n) = &
!c            dip_riv_flux%DATA(:,:,iblock,1,n) * dip_riv_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(dip_riv_flux%data_time, &
!c                              dip_riv_flux%data_inc, dip_riv_flux%interp_type, &
!c                              dip_riv_flux%data_next, dip_riv_flux%data_time_min_loc, &
!c                              dip_riv_flux%data_update, dip_riv_flux%data_type)
!c
!c      dip_riv_flux%has_data = .true.
!c   else
!c      dip_riv_flux%has_data = .false.
!c   endif
!c
!c
!c   if (trim(don_riv_flux%input%filename) /= 'none' .and. &
!c       trim(don_riv_flux%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(don_riv_flux%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c
!c      call read_field(don_riv_flux%input%file_fmt, &
!c                      don_riv_flux%input%filename, &
!c                      don_riv_flux%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         don_riv_flux%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            don_riv_flux%DATA(:,:,iblock,1,n) = c0
!c         don_riv_flux%DATA(:,:,iblock,1,n) = &
!c            don_riv_flux%DATA(:,:,iblock,1,n) * don_riv_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(don_riv_flux%data_time, &
!c                              don_riv_flux%data_inc, don_riv_flux%interp_type, &
!c                              don_riv_flux%data_next, don_riv_flux%data_time_min_loc, &
!c                              don_riv_flux%data_update, don_riv_flux%data_type)
!c
!c      don_riv_flux%has_data = .true.
!c   else
!c      don_riv_flux%has_data = .false.
!c   endif
!c
!c
!c   if (trim(dop_riv_flux%input%filename) /= 'none' .and. &
!c       trim(dop_riv_flux%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(dop_riv_flux%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c
!c      call read_field(dop_riv_flux%input%file_fmt, &
!c                      dop_riv_flux%input%filename, &
!c                      dop_riv_flux%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         dop_riv_flux%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            dop_riv_flux%DATA(:,:,iblock,1,n) = c0
!c         dop_riv_flux%DATA(:,:,iblock,1,n) = &
!c            dop_riv_flux%DATA(:,:,iblock,1,n) * dop_riv_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(dop_riv_flux%data_time, &
!c                              dop_riv_flux%data_inc, dop_riv_flux%interp_type, &
!c                              dop_riv_flux%data_next, dop_riv_flux%data_time_min_loc, &
!c                              dop_riv_flux%data_update, dop_riv_flux%data_type)
!c
!c      dop_riv_flux%has_data = .true.
!c   else
!c      dop_riv_flux%has_data = .false.
!c   endif
!c
!c   if (trim(dsi_riv_flux%input%filename) /= 'none' .and. &
!c       trim(dsi_riv_flux%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(dsi_riv_flux%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c
!c      call read_field(dsi_riv_flux%input%file_fmt, &
!c                      dsi_riv_flux%input%filename, &
!c                      dsi_riv_flux%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         dsi_riv_flux%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            dsi_riv_flux%DATA(:,:,iblock,1,n) = c0
!c         dsi_riv_flux%DATA(:,:,iblock,1,n) = &
!c            dsi_riv_flux%DATA(:,:,iblock,1,n) * dsi_riv_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(dsi_riv_flux%data_time, &
!c                              dsi_riv_flux%data_inc, dsi_riv_flux%interp_type, &
!c                              dsi_riv_flux%data_next, dsi_riv_flux%data_time_min_loc, &
!c                              dsi_riv_flux%data_update, dsi_riv_flux%data_type)
!c
!c      dsi_riv_flux%has_data = .true.
!c   else
!c      dsi_riv_flux%has_data = .false.
!c   endif
!c
!c
!c   if (trim(dfe_riv_flux%input%filename) /= 'none' .and. &
!c       trim(dfe_riv_flux%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(dfe_riv_flux%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c
!c      call read_field(dfe_riv_flux%input%file_fmt, &
!c                      dfe_riv_flux%input%filename, &
!c                      dfe_riv_flux%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         dfe_riv_flux%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            dfe_riv_flux%DATA(:,:,iblock,1,n) = c0
!c         dfe_riv_flux%DATA(:,:,iblock,1,n) = &
!c            dfe_riv_flux%DATA(:,:,iblock,1,n) * dfe_riv_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(dfe_riv_flux%data_time, &
!c                              dfe_riv_flux%data_inc, dfe_riv_flux%interp_type, &
!c                              dfe_riv_flux%data_next, dfe_riv_flux%data_time_min_loc, &
!c                              dfe_riv_flux%data_update, dfe_riv_flux%data_type)
!c
!c      dfe_riv_flux%has_data = .true.
!c   else
!c      dfe_riv_flux%has_data = .false.
!c   endif
!c
!c
!c   if (trim(dic_riv_flux%input%filename) /= 'none' .and. &
!c       trim(dic_riv_flux%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(dic_riv_flux%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c
!c      call read_field(dic_riv_flux%input%file_fmt, &
!c                      dic_riv_flux%input%filename, &
!c                      dic_riv_flux%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         dic_riv_flux%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            dic_riv_flux%DATA(:,:,iblock,1,n) = c0
!c         dic_riv_flux%DATA(:,:,iblock,1,n) = &
!c            dic_riv_flux%DATA(:,:,iblock,1,n) * dic_riv_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(dic_riv_flux%data_time, &
!c                              dic_riv_flux%data_inc, dic_riv_flux%interp_type, &
!c                              dic_riv_flux%data_next, dic_riv_flux%data_time_min_loc, &
!c                              dic_riv_flux%data_update, dic_riv_flux%data_type)
!c
!c      dic_riv_flux%has_data = .true.
!c   else
!c      dic_riv_flux%has_data = .false.
!c   endif
!c
!c
!c   if (trim(alk_riv_flux%input%filename) /= 'none' .and. &
!c       trim(alk_riv_flux%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(alk_riv_flux%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c
!c      call read_field(alk_riv_flux%input%file_fmt, &
!c                      alk_riv_flux%input%filename, &
!c                      alk_riv_flux%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         alk_riv_flux%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            alk_riv_flux%DATA(:,:,iblock,1,n) = c0
!c         alk_riv_flux%DATA(:,:,iblock,1,n) = &
!c            alk_riv_flux%DATA(:,:,iblock,1,n) * alk_riv_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(alk_riv_flux%data_time, &
!c                              alk_riv_flux%data_inc, alk_riv_flux%interp_type, &
!c                              alk_riv_flux%data_next, alk_riv_flux%data_time_min_loc, &
!c                              alk_riv_flux%data_update, alk_riv_flux%data_type)
!c
!c      alk_riv_flux%has_data = .true.
!c   else
!c      alk_riv_flux%has_data = .false.
!c   endif
!c
!c   if (trim(doc_riv_flux%input%filename) /= 'none' .and. &
!c       trim(doc_riv_flux%input%filename) /= 'unknown') then
!c
!c      luse_INTERP_WORK = .true.
!c
!c      allocate(doc_riv_flux%DATA(nx_block,ny_block,max_blocks_clinic,1,12))
!c
!c      call read_field(doc_riv_flux%input%file_fmt, &
!c                      doc_riv_flux%input%filename, &
!c                      doc_riv_flux%input%file_varname, &
!c                      WORK_READ)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         doc_riv_flux%DATA(:,:,iblock,1,n) = WORK_READ(:,:,n,iblock)
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            doc_riv_flux%DATA(:,:,iblock,1,n) = c0
!c         doc_riv_flux%DATA(:,:,iblock,1,n) = &
!c            doc_riv_flux%DATA(:,:,iblock,1,n) * doc_riv_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c      call find_forcing_times(doc_riv_flux%data_time, &
!c                              doc_riv_flux%data_inc, doc_riv_flux%interp_type, &
!c                              doc_riv_flux%data_next, doc_riv_flux%data_time_min_loc, &
!c                              doc_riv_flux%data_update, doc_riv_flux%data_type)
!c
!c      doc_riv_flux%has_data = .true.
!c   else
!c      doc_riv_flux%has_data = .false.
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!  allocate space for interpolate_forcing
!c!-----------------------------------------------------------------------
!c
!c   if (luse_INTERP_WORK) &
!c      allocate(INTERP_WORK(nx_block,ny_block,max_blocks_clinic,1))
!c
!c!-----------------------------------------------------------------------
!c!  load iron PATCH flux fields (if required)
!c!-----------------------------------------------------------------------
!c
!c   if (liron_patch) then
!c
!c!maltrud iron patch
!c!  assume patch file has same normalization and format as deposition file
!c
!c      allocate(IRON_PATCH_FLUX(nx_block,ny_block,max_blocks_clinic))
!c
!c      if (trim(iron_flux%input%filename) == 'unknown') &
!c         iron_flux%input%filename = gas_flux_forcing_file
!c
!c      call read_field(iron_flux%input%file_fmt, &
!c                      iron_flux%input%filename, &
!c                      iron_patch_flux_filename, &
!c                      IRON_PATCH_FLUX)
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock, n)
!c      do iblock=1,nblocks_clinic
!c      do n=1,12
!c         where (.not. LAND_MASK(:,:,iblock)) &
!c            IRON_PATCH_FLUX(:,:,iblock) = c0
!c         iron_flux%DATA(:,:,iblock,1,n) = &
!c            IRON_PATCH_FLUX(:,:,iblock) * iron_flux%input%scale_factor
!c      end do
!c      end do
!c      !$OMP END PARALLEL DO
!c
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!  register and set
!c!     fco2, the air-sea co2 gas flux
!c!-----------------------------------------------------------------------
!c
!c   call named_field_register('SFLUX_CO2', sflux_co2_nf_ind)
!c   !$OMP PARALLEL DO PRIVATE(iblock,WORK)
!c   do iblock=1,nblocks_clinic
!c      WORK = c0
!c      call named_field_set(sflux_co2_nf_ind, iblock, WORK)
!c   end do
!c   !$OMP END PARALLEL DO
!c
!c!-----------------------------------------------------------------------
!c!  verify running coupled if gas fluxes use coupler forcing
!c!-----------------------------------------------------------------------
!c
!c   if ((lflux_gas_o2 .or. lflux_gas_co2) .and. &
!c       (gas_flux_forcing_iopt == gas_flux_forcing_iopt_drv .or. &
!c        atm_co2_iopt == atm_co2_iopt_drv_prog .or. &
!c        atm_co2_iopt == atm_co2_iopt_drv_diag) .and. &
!c       .not. registry_match('lcoupled')) then
!c      call exit_POP(sigAbort, 'ecosys_init: ecosys module requires the ' /&
!c                           &/ 'flux coupler when gas_flux_forcing_opt=drv')
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!  get named field index for atmospheric CO2, if required
!c!-----------------------------------------------------------------------
!c
!c   if (lflux_gas_co2 .and. atm_co2_iopt == atm_co2_iopt_drv_prog) then
!c      call named_field_get_index('ATM_CO2_PROG', atm_co2_nf_ind, &
!c                                 exit_on_err=.false.)
!c      if (atm_co2_nf_ind == 0) then
!c         call exit_POP(sigAbort, 'ecosys_init: ecosys module requires ' /&
!c                              &/ 'atmopsheric CO2 from the flux coupler ' /&
!c                              &/ 'and it is not present')
!c      endif
!c   endif
!c
!c   if (lflux_gas_co2 .and. atm_co2_iopt == atm_co2_iopt_drv_diag) then
!c      call named_field_get_index('ATM_CO2_DIAG', atm_co2_nf_ind, &
!c                                 exit_on_err=.false.)
!c      if (atm_co2_nf_ind == 0) then
!c         call exit_POP(sigAbort, 'ecosys_init: ecosys module requires ' /&
!c                              &/ 'atmopsheric CO2 from the flux coupler ' /&
!c                              &/ 'and it is not present')
!c      endif
!c   endif
!c
!-----------------------------------------------------------------------
!EOC

 end subroutine ecosys_init_sflux
!c
!***********************************************************************
!BOP
! !IROUTINE: ecosys_init_interior_restore
! !INTERFACE:

 subroutine ecosys_init_interior_restore

! !DESCRIPTION:
!  Initialize interior restoring computations for ecosys tracer module.
!
! !REVISION HISTORY:
!  same as module

!EOP
!BOC
!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

   integer (int_kind) :: &
      n,                   & ! index for looping over tracers
      k,                   & ! index for looping over levels
      i,j,                 & ! index for looping over horiz. dims.
      iblock                 ! index for looping over blocks

   real (r8) :: &
      subsurf_fesed          ! sum of subsurface fesed values

!c!-----------------------------------------------------------------------
!c!  initialize restoring timescale (if required)
!c!-----------------------------------------------------------------------
!c
!c   call ecosys_restore%initialize_restoring_timescale(nml_filename, nml_in, zt)
!c
!c!-----------------------------------------------------------------------
!c!  load restoring fields (if required)
!c!-----------------------------------------------------------------------
!c
!c   call ecosys_restore%read_restoring_fields(LAND_MASK)
!c
!c!-----------------------------------------------------------------------
!c!  load fesedflux
!c!  add subsurface positives to 1 level shallower, to accomodate overflow pop-ups
!c!-----------------------------------------------------------------------
!c
   allocate(FESEDFLUX(nx_block,ny_block,km,max_blocks_clinic))
   FESEDFLUX = c0
!c
!c   call read_field(fesedflux_input%file_fmt, &
!c                   fesedflux_input%filename, &
!c                   fesedflux_input%file_varname, &
!c                   FESEDFLUX)
!c
!c   do iblock=1,nblocks_clinic
!c      do j=1,ny_block
!c      do i=1,nx_block
!c         if (KMT(i,j,iblock) > 0 .and. KMT(i,j,iblock) < km) then
!c            subsurf_fesed = c0
!c            do k=KMT(i,j,iblock)+1,km
!c               subsurf_fesed = subsurf_fesed + FESEDFLUX(i,j,k,iblock)
!c            enddo
!c            FESEDFLUX(i,j,KMT(i,j,iblock),iblock) = FESEDFLUX(i,j,KMT(i,j,iblock),iblock) + subsurf_fesed
!c         endif
!c      enddo
!c      enddo
!c
!c      do k = 1, km
!c         where (.not. LAND_MASK(:,:,iblock) .or. k > KMT(:,:,iblock)) &
!c            FESEDFLUX(:,:,k,iblock) = c0
!c         FESEDFLUX(:,:,k,iblock) = FESEDFLUX(:,:,k,iblock) * fesedflux_input%scale_factor
!c      enddo
!c   end do
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
 end subroutine ecosys_init_interior_restore
!c
!c!***********************************************************************
!c!BOP
!c! !IROUTINE: ecosys_set_sflux
!c! !INTERFACE:
!c

 subroutine ecosys_set_sflux(               &
                             SHF_QSW_RAW,   &
                             SHF_QSW,       &
                             U10_SQR,       &
                             IFRAC,         &
                             PRESS,         &
                             SST,           &
                             SSS,           &
                             SURF_VALS_OLD, &
                             SURF_VALS_CUR, &
                             STF_MODULE,    &
                             lexport_shared_vars)

!c   use shr_pio_mod, only : shr_pio_getiotype, shr_pio_getiosys
!c   use POP_IOUnitsMod, only: inst_name
!c
! !DESCRIPTION:
!  Compute surface fluxes for ecosys tracer module.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   real (r8), dimension(:,:,:), intent(in) :: &
      SHF_QSW_RAW,    &! penetrative solar heat flux, from coupler (degC*cm/s)
      SHF_QSW,      &! SHF_QSW used by physics, may have diurnal cylce imposed (degC*cm/s)
      U10_SQR,      &! 10m wind speed squared (cm/s)**2
      IFRAC,        &! sea ice fraction (non-dimensional)
      PRESS,        &! sea level atmospheric pressure (dyne/cm**2)
      SST,          &! sea surface temperature (C)
      SSS            ! sea surface salinity (psu)

   real (r8), dimension(:,:,:,:), &
      intent(in) :: SURF_VALS_OLD, SURF_VALS_CUR ! module tracers

   logical (log_kind), intent(in) :: &
      lexport_shared_vars ! flag to save shared_vars or not

! !INPUT/OUTPUT PARAMETERS:

   real (r8), dimension(:,:,:,:), &
      intent(inout) :: STF_MODULE

!EOP
!BOC
!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

   character(*), parameter :: subname = 'ecosys_mod:ecosys_set_sflux'

   logical (log_kind) :: first_call = .true.

!c   type (block) :: &
!c      this_block      ! block info for the current block

   integer (int_kind) :: &
      i,j,iblock,n, & ! loop indices
      auto_ind,     & ! autotroph functional group index
      mcdate,sec,   & ! date vals for shr_strdata_advance
      errorCode       ! errorCode from HaloUpdate call

   real (r8), dimension(nx_block,ny_block,max_blocks_clinic) :: &
      IFRAC_USED,   & ! used ice fraction (non-dimensional)
      XKW_USED,     & ! portion of piston velocity (cm/s)
      AP_USED,      & ! used atm pressure (converted from dyne/cm**2 to atm)
      IRON_FLUX_IN    ! iron flux

   real (r8), dimension(nx_block,ny_block,max_blocks_clinic) :: &
      SHR_STREAM_WORK

   real (r8), dimension(nx_block,ny_block) :: &
      XKW_ICE,      & ! common portion of piston vel., (1-fice)*xkw (cm/s)
      SCHMIDT_USED, & ! used Schmidt number
      PV,           & ! piston velocity (cm/s)
      O2SAT_1atm,   & ! O2 saturation @ 1 atm (mmol/m^3)
      O2SAT_USED,   & ! used O2 saturation (mmol/m^3)
      XCO2,         & ! atmospheric co2 conc. (dry-air, 1 atm)
      XCO2_ALT_CO2, & ! atmospheric alternative CO2 (dry-air, 1 atm)
      FLUX,         & ! tracer flux (nmol/cm^2/s)
      FLUX_ALT_CO2    ! tracer flux alternative CO2 (nmol/cm^2/s)


   real (r8), dimension(nx_block) :: &
      PHLO,         & ! lower bound for ph in solver
      PHHI,         & ! upper bound for ph in solver
      PH_NEW,       & ! computed PH from solver
      DIC_ROW,      & ! row of DIC values for solver
      ALK_ROW,      & ! row of ALK values for solver
      PO4_ROW,      & ! row of PO4 values for solver
      SiO3_ROW,     & ! row of SiO3 values for solver
      CO2STAR_ROW,  & ! CO2STAR from solver
      DCO2STAR_ROW, & ! DCO2STAR from solver
      pCO2SURF_ROW, & ! pCO2SURF from solver
      DpCO2_ROW,    & ! DpCO2 from solver
      CO3_ROW

   character (char_len) :: &
      tracer_data_label,       & ! label for what is being updated
      ndep_shr_stream_fldList

   character (char_len), dimension(1) :: &
      tracer_data_names          ! short names for input data fields

   integer (int_kind), dimension(1) :: &
      tracer_bndy_loc,         & ! location and field type for ghost
      tracer_bndy_type           !    cell updates

   real (r8), dimension(nx_block,ny_block) :: &
      WORK1, WORK2, WORK3, WORK4, WORK5 ! temporaries for averages

   real (r8) :: scalar_temp


!c!-----------------------------------------------------------------------
!c
!c   call timer_start(ecosys_sflux_timer)
!c
!c!-----------------------------------------------------------------------
!c
!c   if (check_time_flag(comp_surf_avg_flag))  &
!c      call comp_surf_avg(SURF_VALS_OLD,SURF_VALS_CUR,&
!c                         ecosys_tracer_cnt,vflux_flag,surf_avg)
!c
!-----------------------------------------------------------------------
!  fluxes initially set to 0
!  set Chl field for short-wave absorption
!  store incoming shortwave in PAR_out field, converting to W/m^2
!-----------------------------------------------------------------------

   scalar_temp = f_qsw_par / hflux_factor

   !$OMP PARALLEL DO PRIVATE(iblock,WORK1,auto_ind,n)
   do iblock = 1, nblocks_clinic
      STF_MODULE(:,:,:,iblock) = c0

      WORK1 = c0
      do auto_ind = 1, autotroph_cnt
         n = autotrophs(auto_ind)%Chl_ind
         WORK1 = WORK1 + max(c0,p5*(SURF_VALS_OLD(:,:,n,iblock) + &
                                    SURF_VALS_CUR(:,:,n,iblock)))
      end do
!c      call named_field_set(totChl_surf_nf_ind, iblock, WORK1)

      if (ecosys_qsw_distrb_const) then
         PAR_out(:,:,iblock) = SHF_QSW_RAW(:,:,iblock)
      else
         PAR_out(:,:,iblock) = SHF_QSW(:,:,iblock)
      endif

      where (LAND_MASK(:,:,iblock))
         PAR_out(:,:,iblock) = max(c0, scalar_temp * PAR_out(:,:,iblock))
      elsewhere
         PAR_out(:,:,iblock) = c0
      end where
   enddo
   !$OMP END PARALLEL DO

!c!-----------------------------------------------------------------------
!c!  Interpolate gas flux forcing data if necessary
!c!-----------------------------------------------------------------------
!c
!c   if ((lflux_gas_o2 .or. lflux_gas_co2) .and. &
!c        gas_flux_forcing_iopt == gas_flux_forcing_iopt_file) then
!c      if (thour00 >= fice_file%data_update) then
!c         tracer_data_names = fice_file%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'Ice Fraction'
!c         call update_forcing_data(fice_file%data_time,      &
!c            fice_file%data_time_min_loc,  fice_file%interp_type,    &
!c            fice_file%data_next,          fice_file%data_update,    &
!c            fice_file%data_type,          fice_file%data_inc,       &
!c            fice_file%DATA(:,:,:,:,1:12), fice_file%data_renorm,    &
!c            tracer_data_label,            tracer_data_names,        &
!c            tracer_bndy_loc,              tracer_bndy_type,         &
!c            fice_file%filename,           fice_file%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK, &
!c         fice_file%DATA(:,:,:,:,1:12), &
!c         fice_file%data_time,         fice_file%interp_type, &
!c         fice_file%data_time_min_loc, fice_file%interp_freq, &
!c         fice_file%interp_inc,        fice_file%interp_next, &
!c         fice_file%interp_last,       0)
!c      IFRAC_USED = INTERP_WORK(:,:,:,1)
!c
!c      if (thour00 >= xkw_file%data_update) then
!c         tracer_data_names = xkw_file%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'Piston Velocity'
!c         call update_forcing_data(xkw_file%data_time,      &
!c            xkw_file%data_time_min_loc,  xkw_file%interp_type,    &
!c            xkw_file%data_next,          xkw_file%data_update,    &
!c            xkw_file%data_type,          xkw_file%data_inc,       &
!c            xkw_file%DATA(:,:,:,:,1:12), xkw_file%data_renorm,    &
!c            tracer_data_label,           tracer_data_names,       &
!c            tracer_bndy_loc,             tracer_bndy_type,        &
!c            xkw_file%filename,           xkw_file%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         xkw_file%DATA(:,:,:,:,1:12), &
!c         xkw_file%data_time,         xkw_file%interp_type, &
!c         xkw_file%data_time_min_loc, xkw_file%interp_freq, &
!c         xkw_file%interp_inc,        xkw_file%interp_next, &
!c         xkw_file%interp_last,       0)
!c      XKW_USED = INTERP_WORK(:,:,:,1)
!c
!c      if (thour00 >= ap_file%data_update) then
!c         tracer_data_names = ap_file%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'Atmospheric Pressure'
!c         call update_forcing_data(ap_file%data_time,    &
!c            ap_file%data_time_min_loc,  ap_file%interp_type,    &
!c            ap_file%data_next,          ap_file%data_update,    &
!c            ap_file%data_type,          ap_file%data_inc,       &
!c            ap_file%DATA(:,:,:,:,1:12), ap_file%data_renorm,    &
!c            tracer_data_label,          tracer_data_names,      &
!c            tracer_bndy_loc,            tracer_bndy_type,       &
!c            ap_file%filename,           ap_file%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK, &
!c         ap_file%DATA(:,:,:,:,1:12), &
!c         ap_file%data_time,         ap_file%interp_type, &
!c         ap_file%data_time_min_loc, ap_file%interp_freq, &
!c         ap_file%interp_inc,        ap_file%interp_next, &
!c         ap_file%interp_last,       0)
!c      AP_USED = INTERP_WORK(:,:,:,1)
!c
!c   endif
!c
!-----------------------------------------------------------------------
!  calculate gas flux quantities if necessary
!-----------------------------------------------------------------------

   if (lflux_gas_o2 .or. lflux_gas_co2) then

      !$OMP PARALLEL DO PRIVATE(iblock,j,XKW_ICE,SCHMIDT_USED,PV,O2SAT_USED, &
      !$OMP                     O2SAT_1atm,FLUX,FLUX_ALT_CO2,XCO2,XCO2_ALT_CO2,&
      !$OMP                     PHLO,PHHI,DIC_ROW,ALK_ROW, &
      !$OMP                     PO4_ROW,SiO3_ROW,PH_NEW,CO2STAR_ROW, &
      !$OMP                     DCO2STAR_ROW,pCO2SURF_ROW,DpCO2_ROW, &
      !$OMP                     CO3_ROW)

      do iblock = 1, nblocks_clinic

!-----------------------------------------------------------------------
!  Apply OCMIP ice fraction mask when input is from a file.
!-----------------------------------------------------------------------

         if (gas_flux_forcing_iopt == gas_flux_forcing_iopt_file) then
            where (IFRAC_USED(:,:,iblock) < 0.2000_r8) &
               IFRAC_USED(:,:,iblock) = 0.2000_r8
            where (IFRAC_USED(:,:,iblock) > 0.9999_r8) &
               IFRAC_USED(:,:,iblock) = 0.9999_r8
         endif

         if (gas_flux_forcing_iopt == gas_flux_forcing_iopt_drv) then
            IFRAC_USED(:,:,iblock) = IFRAC(:,:,iblock)
            where (IFRAC_USED(:,:,iblock) < c0) IFRAC_USED(:,:,iblock) = c0
            where (IFRAC_USED(:,:,iblock) > c1) IFRAC_USED(:,:,iblock) = c1
            XKW_USED(:,:,iblock) = xkw_coeff * U10_SQR(:,:,iblock)
            AP_USED(:,:,iblock) = PRESS(:,:,iblock)
         endif

!-----------------------------------------------------------------------
!  assume PRESS is in cgs units (dyne/cm**2) since that is what is
!    required for pressure forcing in barotropic
!  want units to be atmospheres
!  convertion from dyne/cm**2 to Pascals is P(mks) = P(cgs)/10.
!  convertion from Pascals to atm is P(atm) = P(Pa)/101.325e+3_r8
!-----------------------------------------------------------------------

         AP_USED(:,:,iblock) = PRESS(:,:,iblock) / 101.325e+4_r8

!c         ECO_SFLUX_TAVG(:,:,buf_ind_ECOSYS_IFRAC,iblock) = IFRAC_USED(:,:,iblock)
!c         ECO_SFLUX_TAVG(:,:,buf_ind_ECOSYS_XKW,iblock) = XKW_USED(:,:,iblock)
!c         ECO_SFLUX_TAVG(:,:,buf_ind_ECOSYS_ATM_PRESS,iblock) = AP_USED(:,:,iblock)

!-----------------------------------------------------------------------
!  Compute XKW_ICE. XKW is zero over land, so XKW_ICE is too.
!-----------------------------------------------------------------------

         XKW_ICE = (c1 - IFRAC_USED(:,:,iblock)) * XKW_USED(:,:,iblock)

!-----------------------------------------------------------------------
!  compute O2 flux
!-----------------------------------------------------------------------

         if (lflux_gas_o2) then
            SCHMIDT_USED = SCHMIDT_O2(SST(:,:,iblock), LAND_MASK(:,:,iblock))

            O2SAT_1atm = O2SAT(SST(:,:,iblock),SSS(:,:,iblock),   &
                               LAND_MASK(:,:,iblock))

            where (LAND_MASK(:,:,iblock))
               PV = XKW_ICE * SQRT(660.0_r8 / SCHMIDT_USED)
               O2SAT_USED = AP_USED(:,:,iblock) * O2SAT_1atm
               FLUX = PV * (O2SAT_USED - p5*(SURF_VALS_OLD(:,:,o2_ind,iblock) + &
                                             SURF_VALS_CUR(:,:,o2_ind,iblock)))
               STF_MODULE(:,:,o2_ind,iblock) = STF_MODULE(:,:,o2_ind,iblock) + FLUX
            elsewhere
               O2SAT_USED = c0
            end where

!c            ECO_SFLUX_TAVG(:,:,buf_ind_PV_O2,iblock) = PV
!c            ECO_SFLUX_TAVG(:,:,buf_ind_SCHMIDT_O2,iblock) = SCHMIDT_USED
!c            ECO_SFLUX_TAVG(:,:,buf_ind_O2SAT,iblock) = O2SAT_USED

         endif  ! lflux_gas_o2

!-----------------------------------------------------------------------
!  compute CO2 flux, computing disequilibrium one row at a time
!-----------------------------------------------------------------------

         if (lflux_gas_co2) then
            SCHMIDT_USED = SCHMIDT_CO2(SST(:,:,iblock), LAND_MASK(:,:,iblock))

            where (LAND_MASK(:,:,iblock))
               PV = XKW_ICE * SQRT(660.0_r8 / SCHMIDT_USED)
            elsewhere
               PV = c0
            end where

! Save surface field of PV for use in other modules
            if (lexport_shared_vars) PV_SURF_fields(:,:,iblock) = PV

!-----------------------------------------------------------------------
!  Set XCO2
!-----------------------------------------------------------------------

            select case (atm_co2_iopt)
            case (atm_co2_iopt_const)
               XCO2 = atm_co2_const
!c            case (atm_co2_iopt_drv_prog, atm_co2_iopt_drv_diag)
!c               call named_field_get(atm_co2_nf_ind, iblock, XCO2)
            end select

            select case (atm_alt_co2_iopt)
            case (atm_co2_iopt_const)
               XCO2_ALT_CO2 = atm_alt_co2_const
            end select

            do j = 1,ny_block
               where (PH_PREV(:,j,iblock) /= c0)
                  PHLO = PH_PREV(:,j,iblock) - del_ph
                  PHHI = PH_PREV(:,j,iblock) + del_ph
               elsewhere
                  PHLO = phlo_surf_init
                  PHHI = phhi_surf_init
               end where

               DIC_ROW = p5*(SURF_VALS_OLD(:,j,dic_ind,iblock) + &
                             SURF_VALS_CUR(:,j,dic_ind,iblock))
               ALK_ROW = p5*(SURF_VALS_OLD(:,j,alk_ind,iblock) + &
                             SURF_VALS_CUR(:,j,alk_ind,iblock))
               PO4_ROW = p5*(SURF_VALS_OLD(:,j,po4_ind,iblock) + &
                             SURF_VALS_CUR(:,j,po4_ind,iblock))
               SiO3_ROW = p5*(SURF_VALS_OLD(:,j,sio3_ind,iblock) + &
                              SURF_VALS_CUR(:,j,sio3_ind,iblock))

               call co2calc_row(iblock, j, LAND_MASK(:,j,iblock), &
                                locmip_k1_k2_bug_fix, .true., &
                                SST(:,j,iblock), SSS(:,j,iblock), &
                                DIC_ROW, ALK_ROW, PO4_ROW, SiO3_ROW, &
                                PHLO, PHHI, PH_NEW, XCO2(:,j), &
                                AP_USED(:,j,iblock), CO2STAR_ROW, &
                                DCO2STAR_ROW, pCO2SURF_ROW, DpCO2_ROW,&
                                CO3_ROW)

               WORK3(:,j) = pCO2SURF_ROW
               WORK4(:,j) = PH_NEW

               PH_PREV(:,j,iblock) = PH_NEW

               FLUX(:,j) = PV(:,j) * DCO2STAR_ROW

!c               ECO_SFLUX_TAVG(:,j,buf_ind_CO2STAR,iblock)  = CO2STAR_ROW
!c               ECO_SFLUX_TAVG(:,j,buf_ind_DCO2STAR,iblock) = DCO2STAR_ROW
!c               ECO_SFLUX_TAVG(:,j,buf_ind_pCO2SURF,iblock) = pCO2SURF_ROW
!c               ECO_SFLUX_TAVG(:,j,buf_ind_DpCO2,iblock)    = DpCO2_ROW
!c
!-------------------------------------------------------------------
!  The following variables need to be shared with other modules, and
!  are now defined in ecosys_share as targets.
!-------------------------------------------------------------------
               if (lexport_shared_vars) then
                  DIC_SURF_fields(:,j,iblock)      = DIC_ROW
                  CO2STAR_SURF_fields(:,j,iblock)  = CO2STAR_ROW
                  DCO2STAR_SURF_fields(:,j,iblock) = DCO2STAR_ROW
                  CO3_SURF_fields(:,j,iblock)      = CO3_ROW
               endif

               where (PH_PREV_ALT_CO2(:,j,iblock) /= c0)
                  PHLO = PH_PREV_ALT_CO2(:,j,iblock) - del_ph
                  PHHI = PH_PREV_ALT_CO2(:,j,iblock) + del_ph
               elsewhere
                  PHLO = phlo_surf_init
                  PHHI = phhi_surf_init
               end where

               DIC_ROW = p5*(SURF_VALS_OLD(:,j,dic_alt_co2_ind,iblock) + &
                             SURF_VALS_CUR(:,j,dic_alt_co2_ind,iblock))

               call co2calc_row(iblock, j, LAND_MASK(:,j,iblock), &
                                locmip_k1_k2_bug_fix, .true., &
                                SST(:,j,iblock), SSS(:,j,iblock), &
                                DIC_ROW, ALK_ROW, PO4_ROW, SiO3_ROW, &
                                PHLO, PHHI, PH_NEW, XCO2_ALT_CO2(:,j), &
                                AP_USED(:,j,iblock), CO2STAR_ROW, &
                                DCO2STAR_ROW, pCO2SURF_ROW, DpCO2_ROW,&
                                CO3_ROW)

               PH_PREV_ALT_CO2(:,j,iblock) = PH_NEW

               FLUX_ALT_CO2(:,j) = PV(:,j) * DCO2STAR_ROW

!c               ECO_SFLUX_TAVG(:,j,buf_ind_CO2STAR_ALT_CO2,iblock) = CO2STAR_ROW
!c               ECO_SFLUX_TAVG(:,j,buf_ind_DCO2STAR_ALT_CO2,iblock) = DCO2STAR_ROW
!c               ECO_SFLUX_TAVG(:,j,buf_ind_pCO2SURF_ALT_CO2,iblock) = pCO2SURF_ROW
!c               ECO_SFLUX_TAVG(:,j,buf_ind_DpCO2_ALT_CO2,iblock) = DpCO2_ROW

            end do

!-----------------------------------------------------------------------
!  set air-sea co2 gas flux named field, converting units from
!  nmol/cm^2/s (positive down) to kg CO2/m^2/s (positive down)
!-----------------------------------------------------------------------

!c            call named_field_set(sflux_co2_nf_ind, iblock, 44.0e-8_r8 * FLUX)

            STF_MODULE(:,:,dic_ind,iblock) = STF_MODULE(:,:,dic_ind,iblock) + FLUX
            WORK5 = FLUX

!c            ECO_SFLUX_TAVG(:,:,buf_ind_PV_CO2,iblock) = PV
!c            ECO_SFLUX_TAVG(:,:,buf_ind_SCHMIDT_CO2,iblock) = SCHMIDT_USED
!c            ECO_SFLUX_TAVG(:,:,buf_ind_DIC_GAS_FLUX,iblock) = FLUX
!c            ECO_SFLUX_TAVG(:,:,buf_ind_PH,iblock) = PH_PREV(:,:,iblock)
!c            ECO_SFLUX_TAVG(:,:,buf_ind_ATM_CO2,iblock) = XCO2
!c
            STF_MODULE(:,:,dic_alt_co2_ind,iblock) = STF_MODULE(:,:,dic_alt_co2_ind,iblock) + FLUX_ALT_CO2
!c
!c            ECO_SFLUX_TAVG(:,:,buf_ind_DIC_GAS_FLUX_ALT_CO2,iblock) = FLUX_ALT_CO2
!c            ECO_SFLUX_TAVG(:,:,buf_ind_PH_ALT_CO2,iblock) = PH_PREV_ALT_CO2(:,:,iblock)
!c            ECO_SFLUX_TAVG(:,:,buf_ind_ATM_ALT_CO2,iblock) = XCO2_ALT_CO2
!c
         endif  !  lflux_gas_co2

      enddo
      !$OMP END PARALLEL DO
#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
      call accumulate_tavg_field_2d(WORK3,ipCO2    ,bid)
      call accumulate_tavg_field_2d(WORK4,ipH      ,bid)
      call accumulate_tavg_field_2d(WORK5,iCO2_flux,bid)
#endif

   endif  ! lflux_gas_o2 .or. lflux_gas_co2
!c
!c!-----------------------------------------------------------------------
!c!  calculate iron and dust fluxes if necessary
!c!-----------------------------------------------------------------------
!c
!c   if (iron_flux%has_data) then
!c      if (thour00 >= iron_flux%data_update) then
!c         tracer_data_names = iron_flux%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'Iron Flux'
!c         call update_forcing_data(iron_flux%data_time,    &
!c            iron_flux%data_time_min_loc,  iron_flux%interp_type,    &
!c            iron_flux%data_next,          iron_flux%data_update,    &
!c            iron_flux%data_type,          iron_flux%data_inc,       &
!c            iron_flux%DATA(:,:,:,:,1:12), iron_flux%data_renorm,    &
!c            tracer_data_label,            tracer_data_names,        &
!c            tracer_bndy_loc,              tracer_bndy_type,         &
!c            iron_flux%filename,           iron_flux%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         iron_flux%DATA(:,:,:,:,1:12), &
!c         iron_flux%data_time,         iron_flux%interp_type, &
!c         iron_flux%data_time_min_loc, iron_flux%interp_freq, &
!c         iron_flux%interp_inc,        iron_flux%interp_next, &
!c         iron_flux%interp_last,       0)
!c      if (liron_patch .and. imonth == iron_patch_month) then
!c         IRON_FLUX_IN = INTERP_WORK(:,:,:,1) + IRON_PATCH_FLUX
!c      else
!c         IRON_FLUX_IN = INTERP_WORK(:,:,:,1)
!c      endif
!c   else
!c      IRON_FLUX_IN = c0
!c   endif
!c
!c   IRON_FLUX_IN = IRON_FLUX_IN * parm_Fe_bioavail
!c
!c   STF_MODULE(:,:,fe_ind,:) = STF_MODULE(:,:,fe_ind,:) + IRON_FLUX_IN
!c   ECO_SFLUX_TAVG(:,:,buf_ind_IRON_FLUX,:) = IRON_FLUX_IN
!c
!c   if (dust_flux%has_data) then
!c      if (thour00 >= dust_flux%data_update) then
!c         tracer_data_names = dust_flux%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'Dust Flux'
!c         call update_forcing_data(dust_flux%data_time,    &
!c            dust_flux%data_time_min_loc,  dust_flux%interp_type,    &
!c            dust_flux%data_next,          dust_flux%data_update,    &
!c            dust_flux%data_type,          dust_flux%data_inc,       &
!c            dust_flux%DATA(:,:,:,:,1:12), dust_flux%data_renorm,    &
!c            tracer_data_label,            tracer_data_names,        &
!c            tracer_bndy_loc,              tracer_bndy_type,         &
!c            dust_flux%filename,           dust_flux%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK, &
!c         dust_flux%DATA(:,:,:,:,1:12),    &
!c         dust_flux%data_time,         dust_flux%interp_type, &
!c         dust_flux%data_time_min_loc, dust_flux%interp_freq, &
!c         dust_flux%interp_inc,        dust_flux%interp_next, &
!c         dust_flux%interp_last,       0)
!c      dust_FLUX_IN = INTERP_WORK(:,:,:,1)
!c
!c!-----------------------------------------------------------------------
!c!  Reduce surface dust flux due to assumed instant surface dissolution
!c!  Can't use parm_fe_bioavail when using solFe input files
!c!-----------------------------------------------------------------------
!c
!c!     dust_FLUX_IN = dust_FLUX_IN * (c1 - parm_Fe_bioavail)
!c      dust_FLUX_IN = dust_FLUX_IN * 0.98_r8
!c   else
!c      dust_FLUX_IN = c0
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!  calculate nox and nhy fluxes if necessary
!c!-----------------------------------------------------------------------
!c
!c   if (nox_flux_monthly%has_data) then
!c      if (thour00 >= nox_flux_monthly%data_update) then
!c         tracer_data_names = nox_flux_monthly%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'NOx Flux'
!c         call update_forcing_data(nox_flux_monthly%data_time,    &
!c            nox_flux_monthly%data_time_min_loc,  nox_flux_monthly%interp_type, &
!c            nox_flux_monthly%data_next,          nox_flux_monthly%data_update, &
!c            nox_flux_monthly%data_type,          nox_flux_monthly%data_inc,    &
!c            nox_flux_monthly%DATA(:,:,:,:,1:12), nox_flux_monthly%data_renorm, &
!c            tracer_data_label,                   tracer_data_names,            &
!c            tracer_bndy_loc,                     tracer_bndy_type,             &
!c            nox_flux_monthly%filename,           nox_flux_monthly%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         nox_flux_monthly%DATA(:,:,:,:,1:12), &
!c         nox_flux_monthly%data_time,         nox_flux_monthly%interp_type, &
!c         nox_flux_monthly%data_time_min_loc, nox_flux_monthly%interp_freq, &
!c         nox_flux_monthly%interp_inc,        nox_flux_monthly%interp_next, &
!c         nox_flux_monthly%interp_last,       0)
!c      STF_MODULE(:,:,no3_ind,:) = STF_MODULE(:,:,no3_ind,:) + INTERP_WORK(:,:,:,1)
!c      ECO_SFLUX_TAVG(:,:,buf_ind_NOx_FLUX,:) = INTERP_WORK(:,:,:,1)
!c   endif
!c
!c   if (nhy_flux_monthly%has_data) then
!c      if (thour00 >= nhy_flux_monthly%data_update) then
!c         tracer_data_names = nhy_flux_monthly%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'NHy Flux'
!c         call update_forcing_data(nhy_flux_monthly%data_time,    &
!c            nhy_flux_monthly%data_time_min_loc,  nhy_flux_monthly%interp_type, &
!c            nhy_flux_monthly%data_next,          nhy_flux_monthly%data_update, &
!c            nhy_flux_monthly%data_type,          nhy_flux_monthly%data_inc,    &
!c            nhy_flux_monthly%DATA(:,:,:,:,1:12), nhy_flux_monthly%data_renorm, &
!c            tracer_data_label,                   tracer_data_names,            &
!c            tracer_bndy_loc,                     tracer_bndy_type,             &
!c            nhy_flux_monthly%filename,           nhy_flux_monthly%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         nhy_flux_monthly%DATA(:,:,:,:,1:12), &
!c         nhy_flux_monthly%data_time,         nhy_flux_monthly%interp_type, &
!c         nhy_flux_monthly%data_time_min_loc, nhy_flux_monthly%interp_freq, &
!c         nhy_flux_monthly%interp_inc,        nhy_flux_monthly%interp_next, &
!c         nhy_flux_monthly%interp_last,       0)
!c      STF_MODULE(:,:,nh4_ind,:) = STF_MODULE(:,:,nh4_ind,:) + INTERP_WORK(:,:,:,1)
!c   endif
!c
!c#ifdef CCSMCOUPLED
!c   if (trim(ndep_data_type) == 'shr_stream') then
!c      if (first_call) then
!c
!c         ndep_shr_stream_fldList = ' '
!c         do n = 1, ndep_shr_stream_var_cnt
!c            if (n == ndep_shr_stream_no_ind) &
!c               ndep_shr_stream_fldList = trim(ndep_shr_stream_fldList) /&
!c                  &/ 'NOy_deposition'
!c            if (n == ndep_shr_stream_nh_ind) &
!c               ndep_shr_stream_fldList = trim(ndep_shr_stream_fldList) /&
!c                  &/ 'NHx_deposition'
!c            if (n < ndep_shr_stream_var_cnt) &
!c               ndep_shr_stream_fldList = trim(ndep_shr_stream_fldList) /&
!c                  &/ ':'
!c         end do
!c
!c         call shr_strdata_create(ndep_sdat,name='ndep data',                   &
!c                                 mpicom=POP_communicator,                      &
!c                                 compid=POP_MCT_OCNID,                         &
!c                                 gsmap=POP_MCT_gsMap_o, ggrid=POP_MCT_dom_o,   &
!c                                 nxg=nx_global, nyg=ny_global,                 &
!c                                 yearFirst=ndep_shr_stream_year_first,         &
!c                                 yearLast=ndep_shr_stream_year_last,           &
!c                                 yearAlign=ndep_shr_stream_year_align,         &
!c                                 offset=0,                                     &
!c                                 domFilePath='',                               &
!c                                 domFileName=ndep_shr_stream_file,             &
!c                                 domTvarName='time',                           &
!c                                 domXvarName='TLONG', domYvarName='TLAT',      &
!c                                 domAreaName='TAREA', domMaskName='KMT',       &
!c                                 FilePath='',                                  &
!c                                 FileName=(/trim(ndep_shr_stream_file)/),      &
!c                                 fldListFile=ndep_shr_stream_fldList,          &
!c                                 fldListModel=ndep_shr_stream_fldList,         &
!c                                 pio_subsystem=shr_pio_getiosys(inst_name),    &
!c                                 pio_iotype=shr_pio_getiotype(inst_name),      &
!c                                 fillalgo='none', mapalgo='none')
!c         if (my_task == master_task) then
!c            call shr_strdata_print(ndep_sdat)
!c         endif
!c         first_call = .false.
!c      endif
!c
!c      mcdate = iyear*10000 + imonth*100 + iday
!c      sec = isecond + 60 * (iminute + 60 * ihour)
!c
!c      call timer_start(ecosys_shr_strdata_advance_timer)
!c      call shr_strdata_advance(ndep_sdat, mcdate, sec, &
!c                               POP_communicator, 'ndep')
!c      call timer_stop(ecosys_shr_strdata_advance_timer)
!c
!c      !
!c      ! process NO3 flux, store results in SHR_STREAM_WORK array
!c      ! instead of directly into STF_MODULE
!c      ! to avoid argument copies in HaloUpdate calls
!c      !
!c      n = 0
!c      do iblock = 1, nblocks_clinic
!c         this_block = get_block(blocks_clinic(iblock),iblock)
!c         do j=this_block%jb,this_block%je
!c         do i=this_block%ib,this_block%ie
!c            n = n + 1
!c            SHR_STREAM_WORK(i,j,iblock) = &
!c               ndep_sdat%avs(1)%rAttr(ndep_shr_stream_no_ind,n)
!c         enddo
!c         enddo
!c      enddo
!c
!c      call POP_HaloUpdate(SHR_STREAM_WORK,POP_haloClinic, &
!c                          POP_gridHorzLocCenter,          &
!c                          POP_fieldKindScalar, errorCode, &
!c                          fillValue = 0.0_POP_r8)
!c      if (errorCode /= POP_Success) then
!c         call exit_POP(sigAbort, subname /&
!c            &/ ': error updating halo for Ndep fields')
!c      endif
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock)
!c      do iblock = 1, nblocks_clinic
!c         where (LAND_MASK(:,:,iblock))
!c            STF_MODULE(:,:,no3_ind,iblock) = STF_MODULE(:,:,no3_ind,iblock) &
!c               + ndep_shr_stream_scale_factor * SHR_STREAM_WORK(:,:,iblock)
!c         endwhere
!c         ECO_SFLUX_TAVG(:,:,buf_ind_NOx_FLUX,iblock) = &
!c            ndep_shr_stream_scale_factor * SHR_STREAM_WORK(:,:,iblock)
!c      enddo
!c      !$OMP END PARALLEL DO
!c
!c      !
!c      ! process NH4 flux, store results in SHR_STREAM_WORK array
!c      ! instead of directly into STF_MODULE
!c      ! to avoid argument copies in HaloUpdate calls
!c      !
!c      n = 0
!c      do iblock = 1, nblocks_clinic
!c         this_block = get_block(blocks_clinic(iblock),iblock)
!c         do j=this_block%jb,this_block%je
!c         do i=this_block%ib,this_block%ie
!c            n = n + 1
!c            SHR_STREAM_WORK(i,j,iblock) = &
!c               ndep_sdat%avs(1)%rAttr(ndep_shr_stream_nh_ind,n)
!c         enddo
!c         enddo
!c      enddo
!c
!c      call POP_HaloUpdate(SHR_STREAM_WORK,POP_haloClinic, &
!c                          POP_gridHorzLocCenter,          &
!c                          POP_fieldKindScalar, errorCode, &
!c                          fillValue = 0.0_POP_r8)
!c      if (errorCode /= POP_Success) then
!c         call exit_POP(sigAbort, subname /&
!c            &/ ': error updating halo for Ndep fields')
!c      endif
!c
!c      !$OMP PARALLEL DO PRIVATE(iblock)
!c      do iblock = 1, nblocks_clinic
!c         where (LAND_MASK(:,:,iblock))
!c            STF_MODULE(:,:,nh4_ind,iblock) = STF_MODULE(:,:,nh4_ind,iblock) &
!c               + ndep_shr_stream_scale_factor * SHR_STREAM_WORK(:,:,iblock)
!c         endwhere
!c      enddo
!c      !$OMP END PARALLEL DO
!c
!c   endif
!c#endif
!c
!c!-----------------------------------------------------------------------
!c!  calculate river bgc fluxes if necessary
!c!-----------------------------------------------------------------------
!c
!c   if (din_riv_flux%has_data) then
!c      if (thour00 >= din_riv_flux%data_update) then
!c         tracer_data_names = din_riv_flux%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'DIN River Flux'
!c         call update_forcing_data(din_riv_flux%data_time,    &
!c            din_riv_flux%data_time_min_loc,  din_riv_flux%interp_type,    &
!c            din_riv_flux%data_next,          din_riv_flux%data_update,    &
!c            din_riv_flux%data_type,          din_riv_flux%data_inc,       &
!c            din_riv_flux%DATA(:,:,:,:,1:12), din_riv_flux%data_renorm,    &
!c            tracer_data_label,           tracer_data_names,       &
!c            tracer_bndy_loc,             tracer_bndy_type,        &
!c            din_riv_flux%filename,           din_riv_flux%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         din_riv_flux%DATA(:,:,:,:,1:12), &
!c         din_riv_flux%data_time,         din_riv_flux%interp_type, &
!c         din_riv_flux%data_time_min_loc, din_riv_flux%interp_freq, &
!c         din_riv_flux%interp_inc,        din_riv_flux%interp_next, &
!c         din_riv_flux%interp_last,       0)
!c      STF_MODULE(:,:,no3_ind,:) = STF_MODULE(:,:,no3_ind,:) + INTERP_WORK(:,:,:,1)
!c      ECO_SFLUX_TAVG(:,:,buf_ind_DIN_RIV_FLUX,:) = INTERP_WORK(:,:,:,1)
!c   endif
!c
!c   if (dip_riv_flux%has_data) then
!c      if (thour00 >= dip_riv_flux%data_update) then
!c         tracer_data_names = dip_riv_flux%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'DIP River Flux'
!c         call update_forcing_data(dip_riv_flux%data_time,    &
!c            dip_riv_flux%data_time_min_loc,  dip_riv_flux%interp_type,    &
!c            dip_riv_flux%data_next,          dip_riv_flux%data_update,    &
!c            dip_riv_flux%data_type,          dip_riv_flux%data_inc,       &
!c            dip_riv_flux%DATA(:,:,:,:,1:12), dip_riv_flux%data_renorm,    &
!c            tracer_data_label,           tracer_data_names,       &
!c            tracer_bndy_loc,             tracer_bndy_type,        &
!c            dip_riv_flux%filename,           dip_riv_flux%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         dip_riv_flux%DATA(:,:,:,:,1:12), &
!c         dip_riv_flux%data_time,         dip_riv_flux%interp_type, &
!c         dip_riv_flux%data_time_min_loc, dip_riv_flux%interp_freq, &
!c         dip_riv_flux%interp_inc,        dip_riv_flux%interp_next, &
!c         dip_riv_flux%interp_last,       0)
!c      STF_MODULE(:,:,po4_ind,:) = STF_MODULE(:,:,po4_ind,:) + INTERP_WORK(:,:,:,1)
!c   endif
!c
!c   if (don_riv_flux%has_data) then
!c      if (thour00 >= don_riv_flux%data_update) then
!c         tracer_data_names = don_riv_flux%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'DON River Flux'
!c         call update_forcing_data(don_riv_flux%data_time,    &
!c            don_riv_flux%data_time_min_loc,  don_riv_flux%interp_type,    &
!c            don_riv_flux%data_next,          don_riv_flux%data_update,    &
!c            don_riv_flux%data_type,          don_riv_flux%data_inc,       &
!c            don_riv_flux%DATA(:,:,:,:,1:12), don_riv_flux%data_renorm,    &
!c            tracer_data_label,           tracer_data_names,       &
!c            tracer_bndy_loc,             tracer_bndy_type,        &
!c            don_riv_flux%filename,           don_riv_flux%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         don_riv_flux%DATA(:,:,:,:,1:12), &
!c         don_riv_flux%data_time,         don_riv_flux%interp_type, &
!c         don_riv_flux%data_time_min_loc, don_riv_flux%interp_freq, &
!c         don_riv_flux%interp_inc,        don_riv_flux%interp_next, &
!c         don_riv_flux%interp_last,       0)
!c      STF_MODULE(:,:,don_ind,:) = STF_MODULE(:,:,don_ind,:) + (INTERP_WORK(:,:,:,1) * 0.9_r8)
!c      STF_MODULE(:,:,donr_ind,:) = STF_MODULE(:,:,donr_ind,:) + (INTERP_WORK(:,:,:,1) * 0.1_r8)
!c   endif
!c
!c   if (dop_riv_flux%has_data) then
!c      if (thour00 >= dop_riv_flux%data_update) then
!c         tracer_data_names = dop_riv_flux%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'DOP River Flux'
!c         call update_forcing_data(dop_riv_flux%data_time,    &
!c            dop_riv_flux%data_time_min_loc,  dop_riv_flux%interp_type,    &
!c            dop_riv_flux%data_next,          dop_riv_flux%data_update,    &
!c            dop_riv_flux%data_type,          dop_riv_flux%data_inc,       &
!c            dop_riv_flux%DATA(:,:,:,:,1:12), dop_riv_flux%data_renorm,    &
!c            tracer_data_label,           tracer_data_names,       &
!c            tracer_bndy_loc,             tracer_bndy_type,        &
!c            dop_riv_flux%filename,           dop_riv_flux%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         dop_riv_flux%DATA(:,:,:,:,1:12), &
!c         dop_riv_flux%data_time,         dop_riv_flux%interp_type, &
!c         dop_riv_flux%data_time_min_loc, dop_riv_flux%interp_freq, &
!c         dop_riv_flux%interp_inc,        dop_riv_flux%interp_next, &
!c         dop_riv_flux%interp_last,       0)
!c      STF_MODULE(:,:,dop_ind,:) = STF_MODULE(:,:,dop_ind,:) + (INTERP_WORK(:,:,:,1) * 0.975_r8)
!c      STF_MODULE(:,:,dopr_ind,:) = STF_MODULE(:,:,dopr_ind,:) + (INTERP_WORK(:,:,:,1) * 0.025_r8)
!c   endif
!c
!c   if (dsi_riv_flux%has_data) then
!c      if (thour00 >= dsi_riv_flux%data_update) then
!c         tracer_data_names = dsi_riv_flux%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'DOP River Flux'
!c         call update_forcing_data(dsi_riv_flux%data_time,    &
!c            dsi_riv_flux%data_time_min_loc,  dsi_riv_flux%interp_type,    &
!c            dsi_riv_flux%data_next,          dsi_riv_flux%data_update,    &
!c            dsi_riv_flux%data_type,          dsi_riv_flux%data_inc,       &
!c            dsi_riv_flux%DATA(:,:,:,:,1:12), dsi_riv_flux%data_renorm,    &
!c            tracer_data_label,           tracer_data_names,       &
!c            tracer_bndy_loc,             tracer_bndy_type,        &
!c            dsi_riv_flux%filename,           dsi_riv_flux%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         dsi_riv_flux%DATA(:,:,:,:,1:12), &
!c         dsi_riv_flux%data_time,         dsi_riv_flux%interp_type, &
!c         dsi_riv_flux%data_time_min_loc, dsi_riv_flux%interp_freq, &
!c         dsi_riv_flux%interp_inc,        dsi_riv_flux%interp_next, &
!c         dsi_riv_flux%interp_last,       0)
!c      STF_MODULE(:,:,sio3_ind,:) = STF_MODULE(:,:,sio3_ind,:) + INTERP_WORK(:,:,:,1)
!c   endif
!c
!c   if (dfe_riv_flux%has_data) then
!c      if (thour00 >= dfe_riv_flux%data_update) then
!c         tracer_data_names = dfe_riv_flux%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'DOP River Flux'
!c         call update_forcing_data(dfe_riv_flux%data_time,    &
!c            dfe_riv_flux%data_time_min_loc,  dfe_riv_flux%interp_type,    &
!c            dfe_riv_flux%data_next,          dfe_riv_flux%data_update,    &
!c            dfe_riv_flux%data_type,          dfe_riv_flux%data_inc,       &
!c            dfe_riv_flux%DATA(:,:,:,:,1:12), dfe_riv_flux%data_renorm,    &
!c            tracer_data_label,           tracer_data_names,       &
!c            tracer_bndy_loc,             tracer_bndy_type,        &
!c            dfe_riv_flux%filename,           dfe_riv_flux%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         dfe_riv_flux%DATA(:,:,:,:,1:12), &
!c         dfe_riv_flux%data_time,         dfe_riv_flux%interp_type, &
!c         dfe_riv_flux%data_time_min_loc, dfe_riv_flux%interp_freq, &
!c         dfe_riv_flux%interp_inc,        dfe_riv_flux%interp_next, &
!c         dfe_riv_flux%interp_last,       0)
!c      STF_MODULE(:,:,fe_ind,:) = STF_MODULE(:,:,fe_ind,:) + INTERP_WORK(:,:,:,1)
!c      ECO_SFLUX_TAVG(:,:,buf_ind_DFE_RIV_FLUX,:) = INTERP_WORK(:,:,:,1)
!c   endif
!c
!c   if (dic_riv_flux%has_data) then
!c      if (thour00 >= dic_riv_flux%data_update) then
!c         tracer_data_names = dic_riv_flux%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'DOP River Flux'
!c         call update_forcing_data(dic_riv_flux%data_time,    &
!c            dic_riv_flux%data_time_min_loc,  dic_riv_flux%interp_type,    &
!c            dic_riv_flux%data_next,          dic_riv_flux%data_update,    &
!c            dic_riv_flux%data_type,          dic_riv_flux%data_inc,       &
!c            dic_riv_flux%DATA(:,:,:,:,1:12), dic_riv_flux%data_renorm,    &
!c            tracer_data_label,           tracer_data_names,       &
!c            tracer_bndy_loc,             tracer_bndy_type,        &
!c            dic_riv_flux%filename,           dic_riv_flux%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         dic_riv_flux%DATA(:,:,:,:,1:12), &
!c         dic_riv_flux%data_time,         dic_riv_flux%interp_type, &
!c         dic_riv_flux%data_time_min_loc, dic_riv_flux%interp_freq, &
!c         dic_riv_flux%interp_inc,        dic_riv_flux%interp_next, &
!c         dic_riv_flux%interp_last,       0)
!c      STF_MODULE(:,:,dic_ind,:) = STF_MODULE(:,:,dic_ind,:) + INTERP_WORK(:,:,:,1)
!c      STF_MODULE(:,:,dic_alt_co2_ind,:) = STF_MODULE(:,:,dic_alt_co2_ind,:) + INTERP_WORK(:,:,:,1)
!c      ECO_SFLUX_TAVG(:,:,buf_ind_DIC_RIV_FLUX,:) = INTERP_WORK(:,:,:,1)
!c      if (lexport_shared_vars) dic_riv_flux_fields=INTERP_WORK(:,:,:,1)
!c   endif
!c
!c   if (alk_riv_flux%has_data) then
!c      if (thour00 >= alk_riv_flux%data_update) then
!c         tracer_data_names = alk_riv_flux%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'DOP River Flux'
!c         call update_forcing_data(alk_riv_flux%data_time,    &
!c            alk_riv_flux%data_time_min_loc,  alk_riv_flux%interp_type,    &
!c            alk_riv_flux%data_next,          alk_riv_flux%data_update,    &
!c            alk_riv_flux%data_type,          alk_riv_flux%data_inc,       &
!c            alk_riv_flux%DATA(:,:,:,:,1:12), alk_riv_flux%data_renorm,    &
!c            tracer_data_label,           tracer_data_names,       &
!c            tracer_bndy_loc,             tracer_bndy_type,        &
!c            alk_riv_flux%filename,           alk_riv_flux%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         alk_riv_flux%DATA(:,:,:,:,1:12), &
!c         alk_riv_flux%data_time,         alk_riv_flux%interp_type, &
!c         alk_riv_flux%data_time_min_loc, alk_riv_flux%interp_freq, &
!c         alk_riv_flux%interp_inc,        alk_riv_flux%interp_next, &
!c         alk_riv_flux%interp_last,       0)
!c      STF_MODULE(:,:,alk_ind,:) = STF_MODULE(:,:,alk_ind,:) + INTERP_WORK(:,:,:,1)
!c      ECO_SFLUX_TAVG(:,:,buf_ind_ALK_RIV_FLUX,:) = INTERP_WORK(:,:,:,1)
!c   endif
!c
!c   if (doc_riv_flux%has_data) then
!c      if (thour00 >= doc_riv_flux%data_update) then
!c         tracer_data_names = doc_riv_flux%input%file_varname
!c         tracer_bndy_loc   = field_loc_center
!c         tracer_bndy_type  = field_type_scalar
!c         tracer_data_label = 'PP River Flux'
!c         call update_forcing_data(doc_riv_flux%data_time,    &
!c            doc_riv_flux%data_time_min_loc,  doc_riv_flux%interp_type,    &
!c            doc_riv_flux%data_next,          doc_riv_flux%data_update,    &
!c            doc_riv_flux%data_type,          doc_riv_flux%data_inc,       &
!c            doc_riv_flux%DATA(:,:,:,:,1:12), doc_riv_flux%data_renorm,    &
!c            tracer_data_label,           tracer_data_names,       &
!c            tracer_bndy_loc,             tracer_bndy_type,        &
!c            doc_riv_flux%filename,           doc_riv_flux%input%file_fmt)
!c      endif
!c      call interpolate_forcing(INTERP_WORK,     &
!c         doc_riv_flux%DATA(:,:,:,:,1:12), &
!c         doc_riv_flux%data_time,         doc_riv_flux%interp_type, &
!c         doc_riv_flux%data_time_min_loc, doc_riv_flux%interp_freq, &
!c         doc_riv_flux%interp_inc,        doc_riv_flux%interp_next, &
!c         doc_riv_flux%interp_last,       0)
!c      STF_MODULE(:,:,doc_ind,:) = STF_MODULE(:,:,doc_ind,:) + INTERP_WORK(:,:,:,1)
!c      if (lexport_shared_vars) doc_riv_flux_fields=INTERP_WORK(:,:,:,1)
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!  Apply NO & NH fluxes to alkalinity
!c!-----------------------------------------------------------------------
!c
!c   STF_MODULE(:,:,alk_ind,:) = STF_MODULE(:,:,alk_ind,:) &
!c      + STF_MODULE(:,:,nh4_ind,:) - STF_MODULE(:,:,no3_ind,:)
!c
!c!-----------------------------------------------------------------------
!c
!c   call timer_stop(ecosys_sflux_timer)
!c
!-----------------------------------------------------------------------
!EOC

 end subroutine ecosys_set_sflux
!c
!*****************************************************************************
!BOP
! !IROUTINE: SCHMIDT_O2
! !INTERFACE:

 function SCHMIDT_O2(SST, LAND_MASK)

! !DESCRIPTION:
!  Compute Schmidt number of O2 in seawater as function of SST
!  where LAND_MASK is true. Give zero where LAND_MASK is false.
!
!  ref : Keeling et al, Global Biogeochem. Cycles, Vol. 12,
!        No. 1, pp. 141-163, March 1998
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   real (r8), dimension(nx_block,ny_block), intent(in) :: SST

   logical (log_kind), dimension(nx_block,ny_block), intent(in) :: &
      LAND_MASK

! !OUTPUT PARAMETERS:

   real (r8), dimension(nx_block,ny_block) :: SCHMIDT_O2

!EOP
!BOC
!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

   real (r8), parameter :: &
      a = 1638.0_r8, &
      b = 81.83_r8, &
      c = 1.483_r8, &
      d = 0.008004_r8

!-----------------------------------------------------------------------

   where (LAND_MASK)
      SCHMIDT_O2 = a + SST * (-b + SST * (c + SST * (-d)))
   elsewhere
      SCHMIDT_O2 = c0
   end where

!-----------------------------------------------------------------------
!EOC

 end function SCHMIDT_O2

!*****************************************************************************
!BOP
! !IROUTINE: O2SAT
! !INTERFACE:

 function O2SAT(SST, SSS, LAND_MASK)

! !DESCRIPTION:
!
!  Computes oxygen saturation concentration at 1 atm total pressure
!  in mmol/m^3 given the temperature (t, in deg C) and the salinity (s,
!  in permil) where LAND_MASK is true. Give zero where LAND_MASK is false.
!
!  FROM GARCIA AND GORDON (1992), LIMNOLOGY and OCEANOGRAPHY.
!  THE FORMULA USED IS FROM PAGE 1310, EQUATION (8).
!
!  *** NOTE: THE "A_3*TS^2" TERM (IN THE PAPER) IS INCORRECT. ***
!  *** IT SHOULD NOT BE THERE.                                ***
!
!  O2SAT IS DEFINED BETWEEN T(freezing) <= T <= 40(deg C) AND
!  0 permil <= S <= 42 permil
!  CHECK VALUE:  T = 10.0 deg C, S = 35.0 permil,
!  O2SAT = 282.015 mmol/m^3
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   real (r8), dimension(nx_block,ny_block), intent(in) :: &
      SST, & ! sea surface temperature (C)
      SSS    ! sea surface salinity (psu)

   logical (log_kind), dimension(nx_block,ny_block), intent(in) :: &
      LAND_MASK

! !OUTPUT PARAMETERS:

    real (r8), dimension(nx_block,ny_block) :: O2SAT

!EOP
!BOC
!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

   real (r8), dimension(nx_block,ny_block) :: TS

!-----------------------------------------------------------------------
!  coefficients in expansion
!-----------------------------------------------------------------------

   real (r8), parameter :: &
      a_0 = 2.00907_r8, &
      a_1 = 3.22014_r8, &
      a_2 = 4.05010_r8, &
      a_3 = 4.94457_r8, &
      a_4 = -2.56847E-1_r8, &
      a_5 = 3.88767_r8, &
      b_0 = -6.24523E-3_r8, &
      b_1 = -7.37614E-3_r8, &
      b_2 = -1.03410E-2_r8, &
      b_3 = -8.17083E-3_r8, &
      c_0 = -4.88682E-7_r8

   where (LAND_MASK)
      TS = log( ((T0_Kelvin + 25.0_r8) - SST) / (T0_Kelvin + SST) )

      O2SAT = exp(a_0+TS*(a_1+TS*(a_2+TS*(a_3+TS*(a_4+TS*a_5)))) + &
         SSS*( (b_0+TS*(b_1+TS*(b_2+TS*b_3))) + SSS*c_0 ))
   elsewhere
      O2SAT = c0
   end where

!-----------------------------------------------------------------------
!  Convert from ml/l to mmol/m^3
!-----------------------------------------------------------------------

   O2SAT = O2SAT / 0.0223916_r8

!-----------------------------------------------------------------------
!EOC

 end function O2SAT

!c!*****************************************************************************
!c!BOP
!c! !IROUTINE: ecosys_tavg_forcing
!c! !INTERFACE:
!c
!c subroutine ecosys_tavg_forcing(STF_MODULE)
!c
!c! !DESCRIPTION:
!c!  Accumulate non-standard forcing related tavg variables.
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !INPUT PARAMETERS:
!c
!c  real (r8), dimension(:,:,:,:), &
!c     intent(in) :: STF_MODULE
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   integer (int_kind) :: &
!c      iblock              ! block loop index
!c
!c!-----------------------------------------------------------------------
!c! accumulate surface flux related fields in the order in which they are declared
!c!
!c!  multiply IRON, DUST fluxes by mpercm (.01) to convert from model
!c!    units (cm/s)(mmol/m^3) to mmol/s/m^2
!c!-----------------------------------------------------------------------
!c
!c   !$OMP PARALLEL DO PRIVATE(iblock)
!c
!c   do iblock = 1,nblocks_clinic
!c
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_ECOSYS_IFRAC,iblock), &
!c                                 tavg_ECOSYS_IFRAC,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_ECOSYS_IFRAC,iblock), &
!c                                 tavg_ECOSYS_IFRAC_2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_ECOSYS_XKW,iblock), &
!c                                 tavg_ECOSYS_XKW,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_ECOSYS_XKW,iblock), &
!c                                 tavg_ECOSYS_XKW_2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_ECOSYS_ATM_PRESS,iblock), &
!c                                 tavg_ECOSYS_ATM_PRESS,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_PV_O2,iblock), &
!c                                 tavg_PV_O2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_SCHMIDT_O2,iblock), &
!c                                 tavg_SCHMIDT_O2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_O2SAT,iblock), &
!c                                 tavg_O2SAT,iblock,1)
!c      call accumulate_tavg_field(STF_MODULE(:,:,o2_ind,iblock), &
!c                                 tavg_O2_GAS_FLUX_2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_CO2STAR,iblock), &
!c                                 tavg_CO2STAR,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_DCO2STAR,iblock), &
!c                                 tavg_DCO2STAR,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_pCO2SURF,iblock), &
!c                                 tavg_pCO2SURF,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_DpCO2,iblock), &
!c                                 tavg_DpCO2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_DpCO2,iblock), &
!c                                 tavg_DpCO2_2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_PV_CO2,iblock), &
!c                                 tavg_PV_CO2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_SCHMIDT_CO2,iblock), &
!c                                 tavg_SCHMIDT_CO2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_DIC_GAS_FLUX,iblock), &
!c                                 tavg_DIC_GAS_FLUX,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_DIC_GAS_FLUX,iblock), &
!c                                 tavg_DIC_GAS_FLUX_2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_PH,iblock), &
!c                                 tavg_PH,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_ATM_CO2,iblock), &
!c                                 tavg_ATM_CO2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_CO2STAR_ALT_CO2,iblock), &
!c                                 tavg_CO2STAR_ALT_CO2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_DCO2STAR_ALT_CO2,iblock), &
!c                                 tavg_DCO2STAR_ALT_CO2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_pCO2SURF_ALT_CO2,iblock), &
!c                                 tavg_pCO2SURF_ALT_CO2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_DpCO2_ALT_CO2,iblock), &
!c                                 tavg_DpCO2_ALT_CO2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_DIC_GAS_FLUX_ALT_CO2,iblock), &
!c                                 tavg_DIC_GAS_FLUX_ALT_CO2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_PH_ALT_CO2,iblock), &
!c                                 tavg_PH_ALT_CO2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_ATM_ALT_CO2,iblock), &
!c                                 tavg_ATM_ALT_CO2,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_IRON_FLUX,iblock)*mpercm, &
!c                                 tavg_IRON_FLUX,iblock,1)
!c      call accumulate_tavg_field(dust_FLUX_IN(:,:,iblock)*mpercm, &
!c                                 tavg_DUST_FLUX,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_NOx_FLUX,iblock), &
!c                                 tavg_NOx_FLUX,iblock,1)
!c      call accumulate_tavg_field(STF_MODULE(:,:,nh4_ind,iblock), &
!c                                 tavg_NHy_FLUX,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_DIN_RIV_FLUX,iblock), &
!c                                 tavg_DIN_RIV_FLUX,iblock,1)
!c      call accumulate_tavg_field(STF_MODULE(:,:,po4_ind,iblock), &
!c                                 tavg_DIP_RIV_FLUX,iblock,1)
!c      call accumulate_tavg_field(STF_MODULE(:,:,don_ind,iblock), &
!c                                 tavg_DON_RIV_FLUX,iblock,1)
!c      call accumulate_tavg_field(STF_MODULE(:,:,donr_ind,iblock), &
!c                                 tavg_DONr_RIV_FLUX,iblock,1)
!c      call accumulate_tavg_field(STF_MODULE(:,:,dop_ind,iblock), &
!c                                 tavg_DOP_RIV_FLUX,iblock,1)
!c      call accumulate_tavg_field(STF_MODULE(:,:,dopr_ind,iblock), &
!c                                 tavg_DOPr_RIV_FLUX,iblock,1)
!c      call accumulate_tavg_field(STF_MODULE(:,:,sio3_ind,iblock), &
!c                                 tavg_DSI_RIV_FLUX,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_DFE_RIV_FLUX,iblock), &
!c                                 tavg_DFE_RIV_FLUX,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_DIC_RIV_FLUX,iblock), &
!c                                 tavg_DIC_RIV_FLUX,iblock,1)
!c      call accumulate_tavg_field(ECO_SFLUX_TAVG(:,:,buf_ind_ALK_RIV_FLUX,iblock), &
!c                                 tavg_ALK_RIV_FLUX,iblock,1)
!c      call accumulate_tavg_field(STF_MODULE(:,:,doc_ind,iblock), &
!c                                 tavg_DOC_RIV_FLUX,iblock,1)
!c
!c   end do
!c
!c   !$OMP END PARALLEL DO
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end subroutine ecosys_tavg_forcing
!c
!c!*****************************************************************************
!c!BOP
!c! !IROUTINE: ecosys_write_restart
!c! !INTERFACE:
!c
!c subroutine ecosys_write_restart(restart_file, action)
!c
!c! !DESCRIPTION:
!c!  write auxiliary fields & scalars to restart files
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !INPUT PARAMETERS:
!c
!c   character(*), intent(in) :: action
!c
!c! !INPUT/OUTPUT PARAMETERS:
!c
!c   type (datafile), intent (inout)  :: restart_file
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   character (char_len) :: &
!c      short_name  ! tracer name temporaries
!c
!c   type (io_dim) :: &
!c      i_dim, j_dim, & ! dimension descriptors
!c      k_dim           ! dimension descriptor for vertical levels
!c
!c   integer (int_kind) :: n
!c
!c   type (io_field_desc), save :: PH_SURF, PH_SURF_ALT_CO2, &
!c                                 PH_3D_ALT_CO2,  PH_3D
!c
!c!-----------------------------------------------------------------------
!c
!c   if (trim(action) == 'add_attrib_file') then
!c      short_name = char_blank
!c      do n=1,ecosys_tracer_cnt
!c         if (vflux_flag(n)) then
!c            short_name = 'surf_avg_' /&
!c                      &/ ind_name_table(n)%name
!c            call add_attrib_file(restart_file,trim(short_name),surf_avg(n))
!c         endif
!c      end do
!c   endif
!c
!c   if (trim(action) == 'define') then
!c      i_dim = construct_io_dim('i', nx_global)
!c      j_dim = construct_io_dim('j', ny_global)
!c      k_dim = construct_io_dim('k', km)
!c
!c      PH_SURF = construct_io_field('PH_SURF', i_dim, j_dim,     &
!c                   long_name='surface pH at current time',      &
!c                   units='pH', grid_loc='2110',            &
!c                   field_loc = field_loc_center,                &
!c                   field_type = field_type_scalar,              &
!c                   d2d_array = PH_PREV)
!c      call data_set (restart_file, 'define', PH_SURF)
!c
!c      PH_SURF_ALT_CO2 = construct_io_field('PH_SURF_ALT_CO2', i_dim, j_dim, &
!c                   long_name='surface pH, alternate CO2, at current time', &
!c                   units='pH', grid_loc='2110',            &
!c                   field_loc = field_loc_center,                &
!c                   field_type = field_type_scalar,              &
!c                   d2d_array = PH_PREV_ALT_CO2)
!c      call data_set (restart_file, 'define', PH_SURF_ALT_CO2)
!c
!c      PH_3D_ALT_CO2 = construct_io_field('PH_3D_ALT_CO2', i_dim, j_dim, k_dim, &
!c                   long_name='3D pH, alternate CO2, at current time', &
!c                   units='pH', grid_loc='3111',            &
!c                   field_loc = field_loc_center,                &
!c                   field_type = field_type_scalar,              &
!c                   d3d_array = PH_PREV_ALT_CO2_3D)
!c      call data_set (restart_file, 'define', PH_3D_ALT_CO2)
!c
!c      PH_3D = construct_io_field('PH_3D', i_dim, j_dim, k_dim, &
!c                   long_name='3D pH at current time', &
!c                   units='pH', grid_loc='3111',            &
!c                   field_loc = field_loc_center,                &
!c                   field_type = field_type_scalar,              &
!c                   d3d_array = PH_PREV_3D)
!c      call data_set (restart_file, 'define', PH_3D)
!c
!c   endif
!c
!c   if (trim(action) == 'write') then
!c      call data_set (restart_file, 'write', PH_SURF)
!c      call data_set (restart_file, 'write', PH_SURF_ALT_CO2)
!c      call data_set (restart_file, 'write', PH_3D)
!c      call data_set (restart_file, 'write', PH_3D_ALT_CO2)
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end subroutine ecosys_write_restart
!c
!c!*****************************************************************************
!c!BOP
!c! !IROUTINE: ecosys_tracer_ref_val
!c! !INTERFACE:
!c
!c function ecosys_tracer_ref_val(ind)
!c
!c! !DESCRIPTION:
!c!  return reference value for tracers using virtual fluxes
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !INPUT PARAMETERS:
!c
!c   integer (int_kind), intent(in) :: ind
!c
!c! !OUTPUT PARAMETERS:
!c
!c   real (r8) :: ecosys_tracer_ref_val
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c
!c   if (vflux_flag(ind)) then
!c      ecosys_tracer_ref_val = surf_avg(ind)
!c   else
!c      ecosys_tracer_ref_val = c0
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end function ecosys_tracer_ref_val
!c
!c!***********************************************************************
!BOP
! !IROUTINE: ecosys_roms_init
! !INTERFACE:

!c#ifdef BEC
!c subroutine ecosys_roms_init(nx_block, ny_block, km)
!c
!c   integer, intent(in) :: &
!c      nx_block, ny_block, km
!c
!c   integer, parameter :: bid = 1
!c
!c!-----------------------------------------------------------------------
!c!  allocate various ecosys module variables
!c!-----------------------------------------------------------------------
!c
!c   allocate(LAND_MASK(nx_block,ny_block,nblocks_clinic))
!c   allocate(dust_FLUX_IN(nx_block,ny_block,max_blocks_clinic))
!c   allocate(FESEDFLUX(nx_block,ny_block,km,max_blocks_clinic))
!c   allocate(PAR_out(nx_block,ny_block,max_blocks_clinic))
!c#ifdef DIAGNOSTICS_BIO
!c   allocate(PH_PREV_3D(nx_block,ny_block,km,max_blocks_clinic))
!c#endif
!c!-----------------------------------------------------------------------
!c!  initialize flux value
!c!-----------------------------------------------------------------------
!c
!c!c   dust_FLUX_IN(:,:,bid) = c0
!c!c   FESEDFLUX(:,:,:,bid) = c0
!c
!c!-----------------------------------------------------------------------
!c!  initialize diagnostic value
!c!-----------------------------------------------------------------------
!c
!c#ifdef DIAGNOSTICS_BIO
!c   PH_PREV_3d(:,:,:,bid) = c0
!c   tavg_CO3 = 1
!c   tavg_HCO3 = 1
!c   tavg_H2CO3 = 1
!c   tavg_pH_3D = 1
!c   tavg_zsatcalc = 1
!c   tavg_zsatarag = 1
!c#endif
!c
!c!-----------------------------------------------------------------------
!c!  set logical switch
!c!-----------------------------------------------------------------------
!c
!c#ifndef BEC
!c   lrest_po4 = .false.
!c   lrest_no3 = .false.
!c   lrest_sio3 = .false.
!c   lsource_sink = .true.
!c#endif
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c return
!c
!c end subroutine ecosys_roms_init
!c#endif
!***********************************************************************
!BOP
! !IROUTINE: ecosys_roms_set_boundary
! !INTERFACE:

#ifdef BEC
# ifdef DUST
 subroutine ecosys_roms_set_boundary(rho0, Cp, rmask, dust)
# else
 subroutine ecosys_roms_set_boundary(rho0, Cp, rmask)
# endif

   real(8), intent(in) :: &
      rho0, Cp

   real(r8), intent(in) :: &
      rmask(:,:)

# ifdef DUST
   real(r8), intent(in) :: &
      dust(:,:)
# endif

   integer, parameter :: bid = 1

!-----------------------------------------------------------------------
!  set LAND_MASK
!        sea : rmask=1., LAND_MASK=T
!       land : rmask=0., LAND_MASK=F
!-----------------------------------------------------------------------

   where ( rmask == 0. )
      LAND_MASK(:,:,bid) = .false.
   elsewhere
      LAND_MASK(:,:,bid) = .true.
   end where

!-----------------------------------------------------------------------
!  compute dust_FLUX_IN
!-----------------------------------------------------------------------

# ifdef DUST
   dust_FLUX_IN(:,:,bid) = dust * 1.0d-1 ! kg/m2/sec -> g/cm2/sec
# else
   dust_FLUX_IN(:,:,bid) = c0
# endif

!-----------------------------------------------------------------------
!EOC
 return

 end subroutine ecosys_roms_set_boundary
#endif

!***********************************************************************

#endif
 end module ecosys_mod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
