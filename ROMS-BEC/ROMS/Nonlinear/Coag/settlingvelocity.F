#include "cppdefs.h"

#if defined COAG

subroutine settlingvelocity(rr, r, rcons, sett_const)
  use mod_coag
  real*8, dimension(param_n_sections) :: rr, r, rcons
  real*8       :: sett_const
  
  rr = sett_const * rcons**3 / r
  
  return
end subroutine settlingvelocity

#endif
