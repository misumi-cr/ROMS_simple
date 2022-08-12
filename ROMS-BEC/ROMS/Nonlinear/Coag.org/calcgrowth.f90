subroutine calcgrowth(growth)
  use mod_setupcoag
  implicit none
  
  real*8, dimension(param_n_sections,param_n_sections) :: growth, growth_loss, growth_gain, growth_add, growth_put
  integer :: irow, jcol, nmax, n
  
  growth_loss = 0.d0
  growth_gain = 0.d0
  growth_add  = 0.d0
  growth_put  = 0.d0
  growth      = 0.d0

  nmax = 15

  do n = 1, nmax
    growth_add(n,n) = param_av_vol(nmax) / param_av_vol(n)
  end do

  growth = growth_add
  
!  if (param_gro_sec > 0.d0) then
!    do jcol = 1, param_n_sections
!      do irow = 1, param_n_sections
!        if ( irow.eq.jcol .and. irow .lt. param_n_sections .and. irow .ge. param_gro_sec ) then
!          growth_loss(irow,jcol) = -1.0
!        end if
!        if ( irow-1.eq.jcol .and. irow-1 .ge. param_gro_sec ) then
!          growth_gain (irow,jcol) = 2.d0
!        end if
!      end do
!    end do
!  end if
!  
!  growth = growth_loss + growth_gain
!  growth(1,1) = 1.d0
!  
!  growth = param_growth * growth
  
  return
end subroutine calcgrowth
