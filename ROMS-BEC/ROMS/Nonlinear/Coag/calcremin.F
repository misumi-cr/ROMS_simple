#include "cppdefs.h"

#if defined COAG

subroutine calcremin(remin)
  use mod_coag, only : param_n_sections, param_remin, param_n_rem
  implicit none
  real*8, dimension(param_n_sections,param_n_sections) :: remin
  integer :: irow, jcol, nz, krow, kcol

  remin = 0.d0

  do jcol = 1, param_n_sections
    do irow = 1, param_n_sections
      if ( irow.eq.jcol .and. irow.le.param_n_rem ) then
        remin(irow,jcol) = - 1.d0 * param_remin
      end if
    end do
  end do
  
  return
end subroutine calcremin

#endif
