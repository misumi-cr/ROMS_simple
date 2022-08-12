      program main_test
      implicit none
      real*8  :: beta
      external   beta
      real*8  :: x
      real*8  :: result
      real*8  :: abserr
      integer :: neval
      integer :: ier
      integer,parameter :: limit = 1000
      integer,parameter :: lenw  = limit*4
      integer :: last
      integer :: iwork(limit)
      real*8  :: work(lenw)

      call dqags(beta,0.d0,1.d0,0.d0,1.d-5,result,abserr,neval,ier,
     &          limit,lenw,last,iwork,work)

      write(*,*) result
  
      stop
      end program main_test

      real*8 function beta(x)
      real*8 :: x

      beta = 4.d0 * sqrt(1.d0-x**2)
      return
      end function beta
