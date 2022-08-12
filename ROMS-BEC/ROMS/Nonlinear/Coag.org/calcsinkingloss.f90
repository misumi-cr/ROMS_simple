subroutine calcsinkingloss(sink_loss)
use mod_setupcoag
implicit none
real*8, dimension(param_n_sections,param_n_sections) :: sink_loss
real*8, dimension(param_n_sections) :: fractal_radius, conserved_radius, settling_vely
integer :: irow, jcol

fractal_radius   = param_amfrac * param_av_vol ** param_bmfrac
conserved_radius = (0.75d0/pi * param_av_vol)**(1.d0/3.d0)

call settlingvelocity(settling_vely, fractal_radius, conserved_radius, param_setcon)

do jcol = 1, param_n_sections
  do irow = 1, param_n_sections
    if ( irow.eq.jcol ) then
      sink_loss(irow,jcol) = settling_vely(irow) / 100.d0 * param_day_to_sec / param_dz
    end if
  end do
end do

return
end subroutine calcsinkingloss
