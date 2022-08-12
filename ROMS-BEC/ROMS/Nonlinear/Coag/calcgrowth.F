#include "cppdefs.h"

#if defined COAG

subroutine calcgrowth(growth)
  use mod_coag, only : param_n_sections, param_growth
  implicit none
  
  real*8, dimension(param_n_sections,param_n_sections) :: growth
  integer :: irow, jcol, nmax, n

! sequential growing, propagating from l=1

  do jcol=1,param_n_sections
    do irow=1,param_n_sections
      if (irow.eq.jcol.and.irow.ge.2) then
        growth(irow,jcol)=-1.d0 * param_growth
      end if
    end do
  end do
  growth(1,1) = param_growth

  do jcol=1,param_n_sections-1
    do irow=1,param_n_sections-1
      if (irow+1.eq.jcol) then
        growth(irow,jcol)=2.d0 * param_growth
      end if
    end do
  end do

! homogenerous growing
!  
!  do n = 1, param_n_sections
!    growth(n,n) = param_growth
!  end do

  return
end subroutine calcgrowth

#endif
