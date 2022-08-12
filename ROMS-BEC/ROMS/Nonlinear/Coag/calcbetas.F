#include "cppdefs.h"

#if defined COAG

subroutine calcbetas(b)
  use mod_coag, only: param_n_sections, param_v_lower, betas, bndry
  implicit none
!  
  type(betas) :: b
  real*8, dimension(param_n_sections,param_n_sections) :: b1d
  real*8, dimension(param_n_sections)                  :: mlo
  
  integer :: irow,jcol
  real*8  :: mj_lo, mj_up
  real*8  :: mi_lo, mi_up
  
  real*8  :: integr5a, integr5b, integr4a, integr4b, integr3a, integr3b, integr2a, integr2b, integr1a, integr1b
  external   integr5a, integr5b, integr4a, integr4b, integr3a, integr3b, integr2a, integr2b, integr1a, integr1b
  
  mlo        = param_v_lower

  b%b5  = 0.d0
  b%b4  = 0.d0
  b%b3  = 0.d0
  b%b2  = 0.d0
  b%b1  = 0.d0

  b1d = 0.d0
  b%b25 = 0.d0
  
  do jcol = 1, param_n_sections-1
    do irow = jcol+1, param_n_sections
  
      mj_lo = mlo(jcol)
      mj_up = 2.d0*mj_lo
      mi_lo = mlo(irow)
      mi_up = 2.d0*mi_lo 
  
      bndry%mi_lo = mi_lo
      bndry%mi_up = mi_up
      bndry%mj_lo = mj_lo
      bndry%mj_up = mj_up
      bndry%mjj   = 0.d0
      bndry%rjj   = 0.d0
      bndry%rvj   = 0.d0
  
      call quadl(b%b5(irow,jcol),integr5a,mi_lo,mi_up)
      b%b5(irow,jcol) = b%b5(irow,jcol)/(mi_lo*mj_lo)

    end do
  end do

  
  do jcol = 1, param_n_sections
  
    mj_lo = mlo(jcol)
    mj_up = 2.d0 * mj_lo
    mi_lo = mlo(jcol)
    mi_up = 2.d0 * mi_lo
  
    bndry%mi_lo = mi_lo
    bndry%mi_up = mi_up
    bndry%mj_lo = mj_lo
    bndry%mj_up = mj_up
    bndry%mjj   = 0.d0
    bndry%rjj   = 0.d0
    bndry%rvj   = 0.d0
  
    call quadl(b%b4(jcol,jcol),integr4a,mi_lo,mi_up)
    b%b4(jcol,jcol) = b%b4(jcol,jcol)/(mi_lo*mj_lo)
  
  end do
  
  b%b4 = b%b4 * 0.5d0
  
  do jcol = 2, param_n_sections
    do irow = 1, (jcol-1)
  
    mj_lo = mlo(jcol)
    mj_up = 2.d0 * mj_lo
    mi_lo = mlo(irow)
    mi_up = 2.d0 * mi_lo
  
    bndry%mi_lo = mi_lo
    bndry%mi_up = mi_up
    bndry%mj_lo = mj_lo
    bndry%mj_up = mj_up 
    bndry%mjj   = 0.d0
    bndry%rjj   = 0.d0
    bndry%rvj   = 0.d0
  
    call quadl(b%b3(irow,jcol),integr3a,mi_lo,mi_up)
    b%b3(irow,jcol) = b%b3(irow,jcol)/(mi_lo*mj_lo)
  
    end do 
  end do
  
  do jcol = 2, param_n_sections
    do irow = 1, (jcol-1)
  
      mj_lo = mlo(jcol)
      mj_up = 2.d0 * mj_lo
      mi_lo = mlo(irow)
      mi_up = 2.d0 * mi_lo
  
      bndry%mi_lo = mi_lo
      bndry%mi_up = mi_up
      bndry%mj_lo = mj_lo
      bndry%mj_up = mj_up
      bndry%mjj   = 0.d0
      bndry%rjj   = 0.d0
      bndry%rvj   = 0.d0
  
      call quadl(b%b2(irow,jcol),integr2a,mi_lo,mi_up)
      b%b2(irow,jcol) = b%b2(irow,jcol)/(mi_lo*mj_lo)
  
    end do
  end do
  
  do jcol = 2, param_n_sections
    do irow = 1, jcol-1
  
      mj_lo = mlo(jcol-1)
      mj_up = 2.d0 * mj_lo
      mi_lo = mlo(irow)
      mi_up = 2.d0 * mi_lo
  
      bndry%mi_lo = mi_lo
      bndry%mi_up = mi_up
      bndry%mj_lo = mj_lo
      bndry%mj_up = mj_up
      bndry%mjj = 0.d0
      bndry%rjj = 0.d0
      bndry%rvj = 0.d0
  
      call quadl(b%b1(irow,jcol),integr1a,mi_lo,mi_up)
      b%b1(irow,jcol) = b%b1(irow,jcol)/(mi_lo*mj_lo)
  
    end do
  end do 
  
  do jcol = 1, param_n_sections
    do irow = 1, param_n_sections
      if (irow .eq. jcol-1) then
        b1d(irow,jcol) = b%b1(irow,jcol)
      end if
    end do
  end do
  
  b%b1 = b%b1 - 0.5d0 * b1d
  
  b%b25 = b%b2-b%b3-b%b4-b%b5
  
  return
end subroutine calcbetas

#endif
