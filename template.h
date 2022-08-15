/*
** svn $Id: bio_toy.h 645 2013-01-22 23:21:54Z arango $
*******************************************************************************
** Copyright (c) 2002-2013 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options one-dimensional (vertical) Biology Toy.
**
** Application flag:   BIO_TOY
** Input script:       ocean_bio_toy.in
**                     bioFennel.in, ecosim.in, npzd_Franks.in, npzd_Powell.in
*/

#define ANA_BSFLUX
#define ANA_BTFLUX
#define ANA_BPFLUX
#define ASSUMED_SHAPE
#define AVERAGES
#define CURVGRID
#define DIFF_GRID
#define DJ_GRADPS
#define DOUBLE_PRECISION
#define MASKING
#define MIX_GEO_TS
#define MIX_S_UV
#define MPI
#define NONLINEAR
#define NONLIN_EOS
#define NO_LBC_ATT
#define NO_WRITE_GRID
#define POWER_LAW
#define PROFILE
#define QCORRECTION
#define RADIATION_2D
#undef  RST_SINGLE
#define SALINITY
#undef  SCORRECTION
#define SOLAR_SOURCE
#define SOLVE3D
#define SPLINES
#define SPHERICAL
#define SPONGE
#undef  STATIONS
#define TS_MPDATA
#define TS_DIF2
#define UV_ADV
#define UV_COR
#define UV_C4ADVECTION
#define UV_QDRAG
#define UV_VIS2
#undef  UV_VIS4
#define VAR_RHO_2D
#define VISC_GRID

#undef  Q_PSOURCE
#undef  Q_TONLY
#undef  TS_PSOURCE

#define BULK_FLUXES
#ifdef BULK_FLUXES
# define ATM_PRESS
# define EMINUSP
# define LONGWAVE_OUT
#else
# define ANA_SMFLUX
# define ANA_STFLUX
#endif

#define LMD_MIXING
#ifdef LMD_MIXING
# define LMD_CONVEC
# define LMD_DDMIX
# undef  LMD_TMIX
# define LMD_NONLOCAL
# define LMD_RIMIX
# define LMD_SHAPIRO
# define LMD_SKPP
#endif

#undef  TINFLUX
#undef  TCLIMATOLOGY
#undef  TCLM_NUDGING

#undef  DICE
#undef  DUST

/*
**  Biological model options.
*/

#undef  BIO_FENNEL
#undef  ECOSIM
#undef  NEMURO
#undef  NPZD_FRANKS
#undef  NPZD_IRON
#undef  NPZD_POWELL
#undef  BEC

#undef  FE_TAG

#undef  DIAGNOSTICS
#undef  DIAGNOSTICS_UV
#undef  DIAGNOSTICS_TS
#undef  DIAGNOSTICS_BIO

#undef  COAG

#ifdef BIO_FENNEL
# define CARBON
# define DENITRIFICATION
# define BIO_SEDIMENT
# define DIAGNOSTICS_BIO
#endif

#if defined ECOSIM || defined BIO_FENNEL
# define ANA_SPFLUX
# define ANA_BPFLUX
# define ANA_CLOUD
#endif

#if defined NEMURO
# define HOLLING_GRAZING
# undef  IVLEV_EXPLICIT
# define ANA_SPFLUX
# define ANA_BPFLUX
#endif

#if defined NPZD_FRANKS || defined NPZD_POWELL
# define ANA_SPFLUX
# define ANA_BPFLUX
#endif

#if defined NPZD_IRON
# define ANA_SPFLUX
# define ANA_BPFLUX
# undef  IRON_LIMIT
# undef  IRON_RELAX
#endif

#if defined BULK_FLUXES || defined ECOSIM
# define ANA_CLOUD
# define PAPA_CLM
#endif

#if defined BEC
# undef  ANA_INITIAL
# undef  ANA_SPFLUX
# define ANA_BPFLUX
# undef  ANA_BIOLOGY
#endif
