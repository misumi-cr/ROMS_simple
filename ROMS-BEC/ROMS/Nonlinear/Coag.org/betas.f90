module mod_betas
  use mod_setupcoag
  type betas
  real*8, dimension(param_n_sections,param_n_sections) :: b1, b2, b3, b4, b5, b25, linear
  end type betas
  type(betas) :: b
end module mod_betas
