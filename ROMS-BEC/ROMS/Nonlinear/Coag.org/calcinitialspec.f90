subroutine calcinitialspec(Vcon)
  use mod_setupcoag
  implicit none
  real*8, dimension(param_n_sections) :: Vcon, Vtry, diag, qtf, wa1, wa2, wa3, wa4
  integer :: iopt, n, ldfjac, maxfev, ml, mu, mode, nprint, info, nfev, njev, lr
  integer :: m, mm
  real*8  :: finitial, jac, d1mach, xtol, epsfcn, factor
  real*8, dimension(param_n_sections,param_n_sections) :: fjac
  real*8, dimension(param_n_sections) :: tfactor
  real*8, allocatable, dimension(:) :: r
  external finitial

  Vcon = 1.d0 * param_av_vol(1)

  do m = 1, param_n_sections
    mm = m-1
    tfactor(m) = 10.d0**(mm)
  end do
  Vcon = Vcon / tfactor

  where(Vcon < 1.d-30) Vcon = 1.d-30

  Vcon = Vcon * param_num_1

!   Vcon = 1.d0 * param_av_vol(1)
!   Vcon(2:param_n_sections) = Vcon(2:param_n_sections) / 10.d0
!   Vcon = param_num_1 * Vcon
!
!   s1conc = Vcon(1)
!
!   iopt   = 2
!   n      = param_n_sections
!   ldfjac = param_n_sections
!   xtol   = sqrt(d1mach(4))
!   maxfev = 2000
!   ml     = 1
!   mu     = 1
!   epsfcn = 0.d0
!   diag   = 1.d0
!   mode   = 2
!   factor = 100.d0
!   nprint = 0
!   lr     = (param_n_sections*(param_n_sections+1))/2.d0
!   allocate(r(lr))
!
!   Vcon(2:param_n_sections) = Vcon(2:param_n_sections) * 1.d7
!
!   call dnsq(finitial,jac,iopt,n,Vcon,Vtry,fjac,ldfjac,xtol,maxfev,ml,mu,epsfcn,diag,mode,factor,nprint,info,nfev,njev,r,lr,qtf,wa1,wa2,wa3,wa4) 
!
!   Vcon(2:param_n_sections) = Vcon(2:param_n_sections)/1.d7
!   Vcon(1) = s1conc

  return
end subroutine calcinitialspec
