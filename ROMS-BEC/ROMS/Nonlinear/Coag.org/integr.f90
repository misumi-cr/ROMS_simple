real*8 function integr5a(mj)
use mod_setupcoag
implicit none
real*8          :: mj, rj, rvj
integer         :: nj, iv
real*8          :: integr5b
external           integr5b

nj = 1
integr5a = 0.d0
rj = param_amfrac*mj**param_bmfrac
rvj = (0.75d0/pi*mj)**(1.d0/3.d0)

bndry%mjj = mj
bndry%rjj = rj
bndry%rvj = rvj

call quadl(integr5a,integr5b,bndry%mj_lo,bndry%mj_up)

integr5a = integr5a / mj

return
end function integr5a
!
real*8 function integr5b(mi)
use mod_setupcoag
implicit none
real*8          :: mi, mj, ri, rj
real*8          :: rvi, rvj
integer         :: ni
real*8          :: KernelBrown, KernelFracSh, KernelFracDS

ri = param_amfrac*mi**(param_bmfrac)
ni = 1
rj = bndry%rjj
mj = bndry%mjj

rvi = (0.75d0/pi*mi)**(1.d0/3.d0)
rvj = bndry%rvj

select case(coag_id)
  case('BR')
    integr5b = KernelBrown (ri, rj)
  case('SH')
    integr5b = KernelFracSh(ri, rj)
  case('DS')
    integr5b = KernelFracDS(ri, rj, rvi, rvj)
  case default
    write(*,*) 'unknown kernel'
    stop
end select

return
end function integr5b
!
real*8 function integr4a(mj)
use mod_setupcoag
implicit none
integer         :: nj
real*8          :: mj, rj, rvj
real*8          :: integr4b
external           integr4b

nj = 1
integr4a = 0.d0
rj = param_amfrac*mj**param_bmfrac
rvj = (0.75d0/pi*mj)**(1.d0/3.d0)

bndry%mjj = mj
bndry%rjj = rj
bndry%rvj = rvj

call quadl(integr4a,integr4b,bndry%mj_lo,bndry%mj_up)

return
end function integr4a
!
real*8 function integr4b(mi)
use mod_setupcoag
implicit none
real*8          :: ri, rj, mi, mj, rvi, rvj
real*8          :: KernelBrown, KernelFracSh, KernelFracDS

ri = param_amfrac*mi**(param_bmfrac)
rj = bndry%rjj
mj = bndry%mjj

rvi = (0.75d0/pi*mi)**(1.d0/3.d0)
rvj = bndry%rvj

select case(coag_id)
  case('BR')
    integr4b = KernelBrown (ri, rj)
  case('SH')
    integr4b = KernelFracSh(ri, rj)
  case('DS')
    integr4b = KernelFracDS(ri, rj, rvi, rvj)
  case default
    write(*,*) 'unknown kernel'
    stop
end select
integr4b = (mi+bndry%mjj)/mi/bndry%mjj * integr4b

return
end function integr4b
!
real*8 function integr3a(mj)
use mod_setupcoag
implicit none
integer         :: nj
real*8          :: mj, rj, rvj
real*8          :: integr3b
external           integr3b

nj = 1
integr3a = 0.d0
rj = param_amfrac*mj**param_bmfrac
rvj = (0.75d0/pi*mj)**(1.d0/3.d0)

bndry%mjj = mj
bndry%rjj = rj
bndry%rvj = rvj


call quadl(integr3a,integr3b,bndry%mj_up-bndry%mjj,bndry%mj_up)
integr3a = integr3a / mj

return
end function integr3a
!
real*8 function integr3b(mi)
use mod_setupcoag
implicit none
real*8          :: ri, mi, rj, mj
real*8          :: rvi, rvj
integer         :: ni
real*8          :: KernelBrown, KernelFracSh, KernelFracDS

ri = param_amfrac*mi**(param_bmfrac)
ni = 1
rj = bndry%rjj
mj = bndry%mjj

rvi = (0.75d0/pi*mi)**(1.d0/3.d0)
rvj = bndry%rvj

select case(coag_id)
  case('BR')
    integr3b = KernelBrown (ri, rj)
  case('SH')
    integr3b = KernelFracSh(ri, rj)
  case('DS')
    integr3b = KernelFracDS(ri, rj, rvi, rvj)
  case default
    write(*,*) 'unknown kernel'
    stop
end select

return
end function integr3b
!
real*8 function integr2a(mj)
use mod_setupcoag
implicit none
real*8          :: mj, rj, rvj
integer         :: nj
real*8          :: integr2b
external           integr2b

nj = 1
integr2a = 0.d0
rj = param_amfrac*mj**param_bmfrac
rvj = (0.75d0/pi*mj)**(1.d0/3.d0)

bndry%mjj = mj
bndry%rjj = rj
bndry%rvj = rvj

call quadl(integr2a,integr2b,bndry%mj_lo,bndry%mj_up-bndry%mjj)

return
end function integr2a
!
real*8 function integr2b(mi)
use mod_setupcoag
implicit none
real*8          :: ri, rj, mi, mj, rvi, rvj
integer         :: ni
real*8          :: KernelBrown, KernelFracSh, KernelFracDS

ri = param_amfrac*mi**param_bmfrac
ni = 1
rj = bndry%rjj
mj = bndry%mjj

rvi = (0.75d0/pi*mi)**(1.d0/3.d0)
rvj = bndry%rvj

select case(coag_id)
  case('BR')
    integr2b = KernelBrown (ri, rj)
  case('SH')
    integr2b = KernelFracSh(ri, rj)
  case('DS')
    integr2b = KernelFracDS(ri, rj, rvi, rvj)
  case default
    write(*,*) 'unknown kernel'
    stop
end select
integr2b = integr2b/mi

return
end function integr2b
!
real*8 function integr1a(mj)
use mod_setupcoag
implicit none
integer         :: nj
real*8          :: rj, rvj, mlow, mj
real*8          :: integr1b
external           integr1b

nj = 1
integr1a = 0.d0
rj = param_amfrac*mj**param_bmfrac
rvj = (0.75/pi*mj)**(1.d0/3.d0)

bndry%mjj = mj
bndry%rjj = rj
bndry%rvj = rvj
mlow      = max(bndry%mj_up-bndry%mjj, bndry%mj_lo)

call quadl(integr1a,integr1b,mlow,bndry%mj_up)     

return
end function integr1a
!
real*8 function integr1b(mi)
use mod_setupcoag
implicit none
real*8          :: mi, mj, ri, rj, rvi, rvj
integer         :: ni
real*8          :: KernelBrown, KernelFracSh, KernelFracDS

ri = param_amfrac*mi**param_bmfrac
ni = 1
rj = bndry%rjj
mj = bndry%mjj
rvi = (0.75d0/pi*mi)**(1.d0/3.d0)
rvj = bndry%rvj

select case(coag_id)
  case('BR')
    integr1b = KernelBrown (ri, rj)
  case('SH')
    integr1b = KernelFracSh(ri, rj)
  case('DS')
    integr1b = KernelFracDS(ri, rj, rvi, rvj)
  case default
    write(*,*) 'unknown kernel'
    stop
end select
integr1b = integr1b*(mi+mj)/mi/mj 

return
end function integr1b
!
real*8 function KernelBrown(r1, r2)
use mod_setupcoag
implicit none
real*8          :: r1, r2

KernelBrown = (2.d0 + r1/r2 + r2/r1)

return
end function KernelBrown
!
real*8 function KernelFracSh(r1, r2)
use mod_setupcoag
implicit none
real*8          :: r1, r2, c1, rg, r_ratio

c1 = 0.785d0
r_ratio = min(r1/r2,r2/r1)
rg = (r1+r2) * param_r_to_rg
KernelFracSh = 1.3d0 * rg * rg * rg
KernelFracSh = KernelFracSh * r_ratio ** c1

return
end function KernelFracSh  
!
real*8 function KernelFracDS(r1, r2, rv1, rv2)
use mod_setupcoag
implicit none
real*8          :: r1, r2, rv1, rv2
real*8          :: c1, rg, r_ratio, rcons1, rcons2

c1 = 0.984d0
rg = (r1+r2)*param_r_to_rg
r_ratio = min(r1/r2,r2/r1)
rcons1 = rv1*rv1*rv1
rcons2 = rv2*rv2*rv2

KernelFracDS = pi * abs(rcons1/r1-rcons2/r2)*rg*rg
KernelFracDS = KernelFracDS * r_ratio ** c1

return
end function KernelFracDS
