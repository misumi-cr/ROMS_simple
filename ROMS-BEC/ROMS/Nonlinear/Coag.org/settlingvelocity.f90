subroutine settlingvelocity(rr, r, rcons, sett_const)
  use mod_setupcoag
  real*8, dimension(param_n_sections) :: rr, r, rcons
  real*8       :: sett_const
  
  rr = sett_const * rcons**3 / r
  
  return
end subroutine settlingvelocity
