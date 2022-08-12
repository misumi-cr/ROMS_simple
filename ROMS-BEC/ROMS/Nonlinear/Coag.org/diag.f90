subroutine diag(nn,b_brown,b_shear,b_ds,growth,sink_loss,spec)
  use mod_setupcoag
  use mod_betas
  use netcdf
  implicit none
  integer :: fo0
  integer, dimension(4)  :: dimid
  integer, dimension(23) :: varid
  integer, dimension(2)  :: dimids, x
  integer                :: n, nn

  real*8, dimension(param_n_sections,param_n_sections) :: growth, sink_loss
  real*8, dimension(nn,param_n_sections) :: spec, nspec_v, massspec_v

  real*8, dimension(param_n_sections)    :: r_i, r_v, set_vel, diam_i, diam_v
  real*8, dimension(nn,param_n_sections) :: diam_i_mat, diam_v_mat, fluxsect, fluxspec, diaratio
  real*8, dimension(nn,param_n_sections) :: nspec_i, masspec_i, fluxspec_i
  
  type(betas) :: b_brown, b_shear, b_ds

  call nc_check( nf90_create('coag_fortran.nc',nf90_clobber,fo0) )
  
  call nc_check( nf90_def_dim(fo0, "nsec" , param_n_sections, dimid(1) ) )
  call nc_check( nf90_def_dim(fo0, "ncol" , param_n_sections, dimid(2) ) )
  call nc_check( nf90_def_dim(fo0, "nrow" , param_n_sections, dimid(3) ) )
  call nc_check( nf90_def_dim(fo0, "ntime", nn              , dimid(4) ) )
  
  dimids = (/ dimid(2), dimid(3) /)
  
  call nc_check( nf90_def_var(fo0, "b_brown.b1" , nf90_double,dimids, varid( 1)) )
  call nc_check( nf90_def_var(fo0, "b_brown.b2" , nf90_double,dimids, varid( 2)) )
  call nc_check( nf90_def_var(fo0, "b_brown.b3" , nf90_double,dimids, varid( 3)) )
  call nc_check( nf90_def_var(fo0, "b_brown.b4" , nf90_double,dimids, varid( 4)) )
  call nc_check( nf90_def_var(fo0, "b_brown.b5" , nf90_double,dimids, varid( 5)) )
  call nc_check( nf90_def_var(fo0, "b_brown.b25", nf90_double,dimids, varid( 6)) )

  call nc_check( nf90_def_var(fo0, "b_shear.b1" , nf90_double,dimids, varid( 7)) )
  call nc_check( nf90_def_var(fo0, "b_shear.b2" , nf90_double,dimids, varid( 8)) )
  call nc_check( nf90_def_var(fo0, "b_shear.b3" , nf90_double,dimids, varid( 9)) )
  call nc_check( nf90_def_var(fo0, "b_shear.b4" , nf90_double,dimids, varid(10)) )
  call nc_check( nf90_def_var(fo0, "b_shear.b5" , nf90_double,dimids, varid(11)) )
  call nc_check( nf90_def_var(fo0, "b_shear.b25", nf90_double,dimids, varid(12)) )
 
  call nc_check( nf90_def_var(fo0, "b_ds.b1"    , nf90_double,dimids, varid(13)) )
  call nc_check( nf90_def_var(fo0, "b_ds.b2"    , nf90_double,dimids, varid(14)) )
  call nc_check( nf90_def_var(fo0, "b_ds.b3"    , nf90_double,dimids, varid(15)) )
  call nc_check( nf90_def_var(fo0, "b_ds.b4"    , nf90_double,dimids, varid(16)) )
  call nc_check( nf90_def_var(fo0, "b_ds.b5"    , nf90_double,dimids, varid(17)) )
  call nc_check( nf90_def_var(fo0, "b_ds.b25"   , nf90_double,dimids, varid(18)) )

  call nc_check( nf90_def_var(fo0, "growth"     , nf90_double,dimids, varid(19)) )
  call nc_check( nf90_def_var(fo0, "sink_loss"  , nf90_double,dimids, varid(20)) )

  dimids = (/ dimid(1), dimid(4) /)
  call nc_check( nf90_def_var(fo0, "spec"       , nf90_double,dimids, varid(21)) )
  call nc_check( nf90_def_var(fo0, "n_spec"     , nf90_double,dimids, varid(22)) )
  call nc_check( nf90_def_var(fo0, "fluxspec"   , nf90_double,dimids, varid(23)) )

  call nc_check( nf90_enddef(fo0) )



  call nc_check( nf90_put_var(fo0, varid( 1), transpose(b_brown%b1 )) )
  call nc_check( nf90_put_var(fo0, varid( 2), transpose(b_brown%b2 )) )
  call nc_check( nf90_put_var(fo0, varid( 3), transpose(b_brown%b3 )) )
  call nc_check( nf90_put_var(fo0, varid( 4), transpose(b_brown%b4 )) )
  call nc_check( nf90_put_var(fo0, varid( 5), transpose(b_brown%b5 )) )
  call nc_check( nf90_put_var(fo0, varid( 6), transpose(b_brown%b25)) )

  call nc_check( nf90_put_var(fo0, varid( 7), transpose(b_shear%b1 )) )
  call nc_check( nf90_put_var(fo0, varid( 8), transpose(b_shear%b2 )) )
  call nc_check( nf90_put_var(fo0, varid( 9), transpose(b_shear%b3 )) )
  call nc_check( nf90_put_var(fo0, varid(10), transpose(b_shear%b4 )) )
  call nc_check( nf90_put_var(fo0, varid(11), transpose(b_shear%b5 )) )
  call nc_check( nf90_put_var(fo0, varid(12), transpose(b_shear%b25)) )

  call nc_check( nf90_put_var(fo0, varid(13), transpose(b_ds%b1    )) )
  call nc_check( nf90_put_var(fo0, varid(14), transpose(b_ds%b2    )) )
  call nc_check( nf90_put_var(fo0, varid(15), transpose(b_ds%b3    )) )
  call nc_check( nf90_put_var(fo0, varid(16), transpose(b_ds%b4    )) )
  call nc_check( nf90_put_var(fo0, varid(17), transpose(b_ds%b5    )) )
  call nc_check( nf90_put_var(fo0, varid(18), transpose(b_ds%b25   )) )

  call nc_check( nf90_put_var(fo0, varid(19), transpose(growth     )) )
  call nc_check( nf90_put_var(fo0, varid(20), transpose(sink_loss  )) )

  call nc_check( nf90_put_var(fo0, varid(21), transpose(spec       )) )

! calculate additional diagnostics

  r_i = param_amfrac * param_av_vol ** param_bmfrac
  r_v = (0.75d0 / pi * param_av_vol)**(1.d0/3.d0)

  call settlingvelocity(set_vel, r_i, r_v, param_setcon)
  set_vel = set_vel / 100.d0 * param_day_to_sec

  diam_i = 2.d0 * param_r_to_rg * r_i
  diam_v = 2.d0 * r_v

  do n = 1, nn
    diam_i_mat(n,:) = diam_i(:)
    diam_v_mat(n,:) = diam_v(:)
  end do

  do n = 1, nn
    nspec_v   (n,:) = spec(n,:) / (1.5d0 * param_v_lower) / param_dwidth
    massspec_v(n,:) = spec(n,:) / param_dwidth
    fluxsect  (n,:) = spec(n,:)       * set_vel(:) * 1.d6
    fluxspec  (n,:) = massspec_v(n,:) * set_vel(:) * 1.d6
  end do

  diaratio = (param_fr_dim/3.d0) * diam_v_mat / diam_i_mat

  nspec_i    = nspec_v    * diaratio
  masspec_i  = massspec_v * diaratio
  fluxspec_i = fluxspec   * diaratio

  call nc_check( nf90_put_var(fo0, varid(22), transpose(nspec_i    )) )
  call nc_check( nf90_put_var(fo0, varid(23), transpose(fluxspec_i )) )

  call nc_check( nf90_close(fo0) )

  return
end subroutine diag

subroutine nc_check(status)
  use netcdf
  integer, intent (in) :: status
  if(status /= nf90_noerr) then 
    print *, trim(nf90_strerror(status))
    stop "Stopped"
  end if
  return
end subroutine nc_check
