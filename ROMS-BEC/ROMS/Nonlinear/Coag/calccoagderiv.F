#include "cppdefs.h"

#if defined COAG

!subroutine finitial(n,x,fvec,iflag)
!  use mod_coag, only : param_n_sections, s1conc
!  implicit none
!  integer :: n, iflag
!  real*8, dimension(n) :: x, fvec, xx
!  external calccoagderiv0
!  
!  xx = x/1.d7
!  xx(1) = s1conc
!  call calccoagderiv0(fvec,xx)
!  fvec(2:param_n_sections) = fvec(2:param_n_sections) * 1.d7
!  
!  return
!end subroutine finitial
!
!subroutine calccoagderiv0(dvdt,vcon)
!  use mod_coag, only : param_n_sections, b
!  implicit none
!  integer :: i, j
!  real*8, dimension(param_n_sections) :: vcon, term1, term2, term3, dvdt, vcon_shift
!  
!  vcon_shift(1) = 0.d0
!  do j = 2, param_n_sections
!    vcon_shift(j) = vcon(j-1)
!  end do
!  
!  term1 = 0.d0
!  term2 = 0.d0
!  term3 = 0.d0
!  do i = 1, param_n_sections
!    do j = 1, param_n_sections
!      term1(j) = term1(j) + vcon(i) * b%b25(i,j)
!      term2(j) = term2(j) + vcon(i) * b%b1 (i,j)
!      term3(i) = term3(i) + vcon(j) * (b%growth(i,j) - b%settle(i,j))
!    end do
!  end do
!  
!  do i = 1, param_n_sections
!    term1(i) = vcon(i) * term1(i)
!    term2(i) = term2(i) * vcon_shift(i)
!  end do
!  
!  dvdt = term1+term2+term3
!  
!  return
!end subroutine calccoagderiv0
  
!subroutine calccoagderiv1(t,vcon,dvdt,delta,ires,rpar,ipar)
!  use mod_coag, only : param_n_sections, b
!  implicit none
!  real*8  :: t, rpar
!  real*8, dimension(param_n_sections) :: vcon, dvdt, delta, vcon_shift, term1, term2, term3
!  integer :: ires, ipar, i, j, n
!  
!  vcon_shift(1) = 0.d0
!  do j = 2, param_n_sections
!    vcon_shift(j) = vcon(j-1)
!  end do
!  
!  term1 = 0.d0
!  term2 = 0.d0
!  term3 = 0.d0
!  do j = 1, param_n_sections
!    do i = 1, param_n_sections
!      term1(j) = term1(j) + vcon(i) * b%b25(i,j)
!      term2(j) = term2(j) + vcon(i) * b%b1 (i,j)
!      term3(i) = term3(i) + vcon(j) * b%growth(i,j)
!    end do
!  end do
!
!  do i = 1, param_n_sections
!    term1(i) = vcon(i)  * term1(i)
!    term2(i) = term2(i) * vcon_shift(i)
!  end do
!  
!  do n = 1, param_n_sections
!    delta(n) = -dvdt(n)+term1(n)+term2(n)+term3(n)
!  end do
!  
!  return
!end subroutine calccoagderiv1
  
!subroutine calccoagderiv2(t,vcon,dvdt,delta,ires,rpar,ipar)
!  use mod_coag, only : param_n_sections, b
!  implicit none
!  real*8  :: t, rpar
!  real*8, dimension(param_n_sections) :: vcon, dvdt, delta, vcon_shift, term1, term2, term3
!  integer :: ires, ipar, i, j, n
!  
!!  vcon_shift(1) = 0.d0
!!  do j = 2, param_n_sections
!!    vcon_shift(j) = vcon(j-1)
!!  end do
!  
!  term1 = 0.d0
!  term2 = 0.d0
!  term3 = 0.d0
!  do j = 1, param_n_sections
!    do i = 1, param_n_sections
!      term3(i) = term3(i) + vcon(j) * (-b%settle(i,j))
!    end do
!  end do
!
!  do n = 1, param_n_sections
!    delta(n) = -dvdt(n)+term1(n)+term2(n)+term3(n)
!  end do
!  
!  return
!end subroutine calccoagderiv2

subroutine calccoagderiv0(t,vcon,dvdt,delta,ires,rpar,ipar)
  use mod_coag, only : param_n_sections, param_n_layers, ba, param_av_vol, param_dwidth, param_dcomb, param_day_to_sec
  implicit none
  real*8  :: t, rpar
  real*8, dimension(param_n_sections*param_n_layers) :: vcon, dvdt, delta, vcon_shift, term1, term2, term3, term4, term5
  integer :: ires, ipar, i, j, n
  
  vcon_shift(1) = 0.d0
  do j = 2, param_n_sections*param_n_layers
    vcon_shift(j) = vcon(j-1)
  end do
  
  term1 = 0.d0
  term2 = 0.d0
  term3 = 0.d0
  term4 = 0.d0
  term5 = 0.d0

  do j = 1, param_n_sections*param_n_layers
    do i = 1, param_n_sections*param_n_layers
      term1(j) = term1(j) + vcon(i) * ba%b25(i,j)
      term2(j) = term2(j) + vcon(i) * ba%b1 (i,j)
      term3(i) = term3(i) + vcon(j) * (ba%growth(i,j)+ba%settle(i,j)+ba%gain(i,j)+ba%disagg(i,j)+ba%remin(i,j))
    end do
  end do

  do i = 1, param_n_sections*param_n_layers
    term1(i) = vcon(i)  * term1(i)
    term2(i) = term2(i) * vcon_shift(i)
  end do

!  not used redist matrix
!  do j = 1, param_n_sections*param_n_layers  
!    do i = 1, param_n_sections*param_n_layers
!      term5(i) = term5(i) - term4(j) * ba%redist(i,j)
!    end do
!  end do

  do n = 1, param_n_sections*param_n_layers
    delta(n) = -dvdt(n)+term1(n)+term2(n)+term3(n)
  end do
  
  return
end subroutine calccoagderiv0

#endif
