#include "cppdefs.h"

#if defined COAG

subroutine calcdisagg(disagg,redist)
  use mod_coag, only : param_n_sections, param_disagg, param_remin
  implicit none
  real*8, dimension(param_n_sections,param_n_sections) :: disagg, redist
  integer :: irow, jcol, nz, krow, kcol
  real*8  :: disagg_rate

  disagg = 0.d0
  redist = 0.d0

!  do jcol=1,param_n_sections
!    do irow=1,param_n_sections
!      if (irow.eq.jcol) then
!        disagg(irow,jcol) = -1.d0 * param_disagg
!      end if
!    end do
!  end do
!
!  do jcol=1,param_n_sections
!    do irow=1,param_n_sections
!      if (irow.lt.jcol) then
!        redist(irow,jcol) = 2.d0**(jcol-irow)/(jcol-1)
!      end if 
!    end do
!  end do

  do jcol = 1, param_n_sections
    do irow = 1, param_n_sections
      if ( irow.eq.jcol) then
        disagg(irow,jcol) = - 1.d0 * param_disagg
      end if
      if ( irow+1.eq.jcol ) then
        disagg(irow,jcol) = 2.d0 * param_disagg
      end if
    end do
  end do
  
  return
end subroutine calcdisagg

#endif
