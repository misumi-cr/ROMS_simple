#include "cppdefs.h"
#ifdef COAG
!!
!!svn $Id: read_coagpar.F 645 2013-01-22 23:21:54Z arango $
!!================================================= Hernan G. Arango ===
!! Copyright (c) 2002-2013 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!!======================================================================
!!                                                                     !
!!  This routine reads and reports selected biological model input     !
!!  parameters.                                                        !
!!                                                                     !
!!======================================================================
!!
# include <coag_inp.h>
#else
      SUBROUTINE read_CoagPar
      END SUBROUTINE read_CoagPar
#endif
