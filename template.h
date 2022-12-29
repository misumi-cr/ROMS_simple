/*
** svn $Id: upwelling.h 830 2017-01-24 21:21:11Z arango $
*******************************************************************************
** Copyright (c) 2002-2017 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
*/
/* general */
#define CURVGRID
#define MASKING
#undef  WET_DRY
#define NONLIN_EOS
#define SOLVE3D
#define SALINITY
#define SPLINES 

/* output */ 
#undef  STATIONS
#define AVERAGES
#define NO_WRITE_GRID
#define OUT_DOUBLE

/* advection  */ 
#define UV_ADV
#define UV_COR
#define UV_VIS2
#define UV_QDRAG
#define UV_U3HADVECTION
#define UV_C4VADVECTION
#undef  TS_C4HADVECTION
#undef  TS_C4VADVECTION
#define TS_MPDATA
#define TS_DIF2
#define MIX_GEO_TS
#define MIX_S_UV
#define VISC_GRID
#define SPONGE

#define LMD_MIXING
#ifdef LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC
# define LMD_SKPP
# define LMD_BKPP
# define LMD_NONLOCAL
#endif

/* surface fluxes */ 
#define BULK_FLUXES
#ifdef BULK_FLUXES
# define SOLAR_SOURCE
# undef  DIURNAL_SRFLUX
# define LONGWAVE
# define ANA_CLOUD
# undef  ANA_RAIN
# define EMINUSP
#else
# define ANA_SMFLUX
# define ANA_STFLUX
#endif

/* tidal option */
#undef  RAMP_TIDES
#undef  SSH_TIDES
#undef  UV_TIDES
#define ANA_M2OBC
#define ANA_FSOBC

/* river option */
#ifdef SOLVE3D
# undef  UV_PSOURCE
# undef  Q_PSOURCE
# undef  TS_PSOURCE
# undef  T_PASSIVE
#endif 

/* biological model */
#undef BEC
#ifdef BEC
# undef CISO
# define DIAGNOSTICS_BIO
# ifdef  DIAGNOSTICS_BIO
#  define DIAGNOSTICS
# endif
#endif

/* analytical data */
#undef  ANA_INITIAL
#define ANA_BTFLUX
#define ANA_SPFLUX
#define ANA_BPFLUX
#define ANA_SSFLUX
#define ANA_BSFLUX

/* addition */
#define NO_LBC_ATT

#undef  TCLIMATOLOGY
#undef  TCLM_NUDGING