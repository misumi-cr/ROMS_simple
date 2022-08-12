#include "cppdefs.h"

#include "cppdefs.h"
      MODULE coag_mod
#if defined NONLINEAR && defined COAG
!
!svn $Id: biology.F 645 2013-01-22 23:21:54Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2013 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the source and sink terms for selected        !
!   biology model.                                                     !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: coag

      CONTAINS

# if defined COAG
#  include <coag.h>
# endif

#endif
      END MODULE coag_mod
