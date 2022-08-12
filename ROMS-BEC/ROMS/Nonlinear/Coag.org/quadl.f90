subroutine quadl(r,f,rlow,rhigh)
  implicit none
  real*8  :: f
  external   f
  real*8  :: r, rlow, rhigh
  real*8  :: abserr
  integer :: neval
  integer :: ier
  integer,parameter :: limit = 10000
  integer,parameter :: lenw  = limit*4
  integer :: last
  integer :: iwork(limit)
  real*8  :: work(lenw)
  integer,parameter :: leniw = 100
  integer,parameter :: maxpl = 100
  integer :: n
  real*8  :: dd
  integer,parameter :: npts2 = 30
  real*8  :: points(npts2)
  real*8  :: resabs, resasc
  
!  call dqags(f,rlow,rhigh,0.d0,1.d-3,r,abserr,neval,ier,limit,lenw,last,iwork,work)

!  call dqag (f,rlow,rhigh,1.d-21,1.d-10,2,r,abserr,neval,ier,limit,lenw,last,iwork,work)
!  call dqng(f,rlow,rhigh,0.d0,1.d-5,r,abserr,neval,ier)
!  call dqawo(f,rlow,rhigh,100.d0,1,0.d0,1.d-5,r,abserr,neval,ier,leniw,maxpl,lenw,last,iwork,work)
  call dqk15(f,rlow,rhigh,r,abserr,resabs,resasc)

!  dd = (rhigh-rlow)/npts2
!  points = 0.d0
!  do n = 1, npts2-2
!    points(n) = rlow + n*dd
!  end do
!  
!  call dqagp(f,rlow,rhigh,npts2,points,0.0,1.d-5,r,abserr,neval,ier,leniw,lenw,last,iwork,work)

  return
end subroutine quadl
