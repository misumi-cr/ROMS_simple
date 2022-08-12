program coag_driver
  use mod_setupcoag
  use mod_betas
  implicit none
  
  type(betas) :: b_brown, b_shear, b_ds
  
  real*8, dimension(param_n_sections,param_n_sections) :: growth, sink_loss
  
  real*8, dimension(param_n_sections) :: vcon, dvdt
  real*8 :: t, tout
  integer :: n, nn

  external calccoagderiv1
  real*8  :: rtol, atol, rpar, djac1
  integer :: idid, ipar 
  integer, parameter :: lrw = 100000, liw = 100000
  integer, dimension(15)     :: info
  real*8 , dimension(lrw) :: rwork
  integer, dimension(liw) :: iwork

  real*8, allocatable :: vcon_out(:,:)
  
  call setupcoag()

  allocate(vcon_out(param_t_cnt+1,param_n_sections))
  
  coag_id = 'BR'
  call calcbetas(b_brown)
  b_brown%b1 = b_brown%b1 * param_conBr * param_day_to_sec
  b_brown%b2 = b_brown%b2 * param_conBr * param_day_to_sec
  b_brown%b3 = b_brown%b3 * param_conBr * param_day_to_sec
  b_brown%b4 = b_brown%b4 * param_conBr * param_day_to_sec
  b_brown%b5 = b_brown%b5 * param_conBr * param_day_to_sec

  coag_id = 'SH'
  call calcbetas(b_shear)
  b_shear%b1  = b_shear%b1  * param_gamma * param_day_to_sec
  b_shear%b2  = b_shear%b2  * param_gamma * param_day_to_sec
  b_shear%b3  = b_shear%b3  * param_gamma * param_day_to_sec
  b_shear%b4  = b_shear%b4  * param_gamma * param_day_to_sec
  b_shear%b5  = b_shear%b5  * param_gamma * param_day_to_sec
  b_shear%b25 = b_shear%b25 * param_gamma * param_day_to_sec

  coag_id = 'DS'
  call calcbetas(b_ds)
  b_ds%b1     = b_ds%b1  * param_setcon * param_day_to_sec
  b_ds%b2     = b_ds%b2  * param_setcon * param_day_to_sec
  b_ds%b3     = b_ds%b3  * param_setcon * param_day_to_sec
  b_ds%b4     = b_ds%b4  * param_setcon * param_day_to_sec
  b_ds%b5     = b_ds%b5  * param_setcon * param_day_to_sec
  b_ds%b25    = b_ds%b25 * param_setcon * param_day_to_sec

  b%b1 = b_brown%b1 + b_shear%b1 + b_ds%b1
  b%b2 = b_brown%b2 + b_shear%b2 + b_ds%b2
  b%b3 = b_brown%b3 + b_shear%b3 + b_ds%b3
  b%b4 = b_brown%b4 + b_shear%b4 + b_ds%b4
  b%b5 = b_brown%b5 + b_shear%b5 + b_ds%b5

  b%b25 = b%b2 - b%b3 - b%b4 - b%b5

  call calcgrowth     (growth)
  call calcsinkingloss(sink_loss)

  b%linear = growth - sink_loss

  call calcinitialspec(vcon)

  t    = param_t_init
  tout = param_delta_t

  dvdt = 0.d0

  atol     = 0.d0
  rtol     = 1.d-2
  info     = 0
  info(11) = 1

  vcon_out(1,:) = vcon(:)
  do n = 1, param_t_cnt
    call ddassl(calccoagderiv1,param_n_sections,t,vcon,dvdt,tout,info,rtol,atol,idid,rwork,lrw,iwork,liw,rpar,ipar,djac1)
    vcon_out(n+1,:) = vcon(:)
    tout = tout + param_delta_t
  end do

  call diag(param_t_cnt+1, b_brown, b_shear, b_ds, growth, sink_loss, vcon_out)

end program coag_driver
