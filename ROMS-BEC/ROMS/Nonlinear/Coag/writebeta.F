#include "cppdefs.h"

#if defined COAG

subroutine writebeta(b,nstr)
  use mod_param
  use mod_coag, only: param_n_sections
  implicit none
  real(r8), dimension(param_n_sections,param_n_sections) :: b
  integer :: i, j
  integer :: nstr, nend

  nend = nstr + 9
  do i = nstr, nend
    write(*,'(10(1PE10.3,X))')  (b(i,j),j=nstr,nend)
  end do

  return
end subroutine writebeta

subroutine writebeta_nc(ba)
  use netcdf
  use mod_param
  use mod_coag, only: param_n_sections, param_n_layers
  implicit none
  real(r8), dimension(param_n_sections*param_n_layers,param_n_sections*param_n_layers) ::ba
  integer :: i,j, mx
  integer :: fo0, varid, ncol_dimid, nrow_dimid, ncol, nrow
  integer, dimension(2) :: dimids

  ncol = param_n_sections * param_n_layers
  nrow = param_n_sections * param_n_layers

  call check(nf90_create('ba.nc',NF90_CLOBBER,fo0))
  call check(nf90_def_dim(fo0,"ncol",ncol,ncol_dimid))
  call check(nf90_def_dim(fo0,"nrow",nrow,nrow_dimid))

  dimids = (/ncol_dimid,nrow_dimid/)

  call check(nf90_def_var(fo0,"ba",NF90_DOUBLE,dimids,varid))
  call check(nf90_enddef(fo0))
  call check(nf90_put_var(fo0,varid,transpose(ba)))
  call check(nf90_close(fo0))

end subroutine writebeta_nc

subroutine check(status)
use netcdf

  integer, intent(in) :: status

  if (status /= nf90_noerr) then 
    print *, trim(nf90_strerror(status))
    stop "Stopped"
  end if
end subroutine check  
#endif
