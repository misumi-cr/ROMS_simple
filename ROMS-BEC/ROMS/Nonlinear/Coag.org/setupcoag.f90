module mod_setupcoag
  implicit none
  
  real*8      :: param_rho_fl, param_kvisc, param_g
  real*8      :: param_day_to_sec
  real*8      :: param_k, param_r_to_rg
  
  integer, parameter :: param_n_sections = 47
  integer     :: param_nl
  character*2 :: param_kernel
  real*8      :: param_d0, param_fr_dim
  
  real*8      :: param_temp, param_alpha, param_dz
  real*8      :: param_gamma, param_growth, param_gro_sec
  integer     :: param_num_1
  
  real*8      :: param_t_init, param_t_final, param_delta_t
  integer     :: param_t_cnt
  
  integer     :: opt_tracer
  
  real*8      :: param_dvisc, param_del_rho, param_conBr
  real*8      :: param_a0, param_v0, pi
  real*8      :: param_v_lower(param_n_sections)
  real*8      :: param_v_upper(param_n_sections)
  real*8      :: param_av_vol (param_n_sections)
  real*8      :: param_dcomb  (param_n_sections)
  real*8      :: param_dwidth (param_n_sections)
  
  real*8      :: amfrac, param_amfrac, param_bmfrac, param_setcon
  
  type bndry_int
  real*8      :: mi_lo, mi_up
  real*8      :: mj_lo, mj_up
  real*8      :: mjj, rjj, rvj
  end type bndry_int
  
  type(bndry_int) :: bndry
  
  character*2 :: coag_id
  
  real*8      :: s1conc
  
  contains
  subroutine setupcoag()
    integer :: n
    
    param_rho_fl     = 1.0275d0       ! Fluid density [g cm^-3]
    param_kvisc      = 0.01d0
    param_g          = 980.d0
    param_day_to_sec = 8.64d04
    param_k          = 1.3e-16
    param_r_to_rg    = 1.36d0 
    
    param_kernel     = 'BW'
    param_d0         = 1.d-6
    param_fr_dim     = 2.33d0
    param_nl         = 10000
    
    param_temp       = 20.0 + 273.0
    param_alpha      = 1.d0
    param_dz         = 10.d0
    param_gamma      = 0.1d0
    param_growth     = 0.15d0
    param_gro_sec    = 2.d0
    param_num_1      = 40.d0
    
    param_t_init     = 0.d0
    param_t_final    = 10.d0
    param_delta_t    = 1.d0/24.d0/6.d0
    param_t_cnt      = int((param_t_final - param_t_init) / param_delta_t)
    
    opt_tracer       = 0
    
    param_dvisc      = param_kvisc * param_rho_fl
    param_del_rho    = (4.5d0 * 2.48d0) * param_kvisc * param_rho_fl / param_g * (param_d0/2.d0)**(-0.83)
    param_conBr      = 2.d0 / 3.d0 * param_k * param_temp / param_dvisc
    
    param_a0         = param_d0/2.d0
    pi               = 3.1415926535d0
    param_v0         = (pi/6.d0) * param_d0**3
    
    do n = 0, param_n_sections-1
      param_v_lower(n+1) = param_v0*2.d0**(n)
      param_v_upper(n+1) = param_v_lower(n+1)*2.d0
    end do
    
    param_av_vol     = 1.5d0 * param_v_lower
    param_dcomb      = (param_v_lower*6.d0/pi)**(1.d0/3.d0)
    param_dwidth     = (2.d0**(1.d0/3.d0)-1.d0)*param_dcomb
    
    amfrac           = (4.0d0/3.d0*pi)**(-1.d0/param_fr_dim) * param_a0**(1.d0-3.d0/param_fr_dim)
    param_amfrac     = amfrac*sqrt(0.6)
    param_bmfrac     = 1.d0 / param_fr_dim
    
    param_setcon     = (2.d0/9.d0) * param_del_rho / param_rho_fl * param_g/param_kvisc
    
  end subroutine setupcoag
end module mod_setupcoag
