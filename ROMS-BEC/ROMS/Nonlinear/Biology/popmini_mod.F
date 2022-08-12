#include "cppdefs.h"
  module popmini_mod
#if defined BEC
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! !MEMO
!
! popmini_mod.Fは、ecosys_mod.Fで他モジュールプログラムから
! 呼び出していた変数をまとめたもの。
!
!
! 各変数とPOPモジュールプログラムとの対応
! block.F:
!     nx_block, ny_block
!
! domain_size.F:
!     km, nt, max_blocks_clinic
!
! domain.F:
!     nblocks_clinic
!
! grid.F:
!     zt, dz, DZT, KMT, TAREA
!
! time_management.F:
!     seconds_in_day, days_in_year, days_in_leap_year, days_in_norm_year
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

! USES:
  use mod_kinds
  use kinds_mod
  use POP_KindsMod
  use ocean_control_mod, ONLY : ROMS_finalize
  use constants

  implicit none
  save
  private

  public :: &
       popmini_init, &
       popmini_set_boundary, &
       popmini_abort, &
       pop_ref_pressure, &
#ifdef DIAGNOSTICS_BIO
       popmini_tavg_now, &
       bec_diag_3d, &
       bec_diag_2d, &
       accumulate_tavg_field_3d, &
       accumulate_tavg_field_2d, &
#endif
#ifdef CISO
       seconds_in_day, seconds_in_year, &
       TAREA, &
#endif
       max_blocks_clinic, nblocks_clinic, &
       partial_bottom_cells, &
       DZT, dz, zt, zw, KMT, &
       nx_block, ny_block, km

#ifdef MPI
  include 'mpif.h'
#endif

  integer (int_kind), parameter :: max_blocks_clinic = 1
  integer (int_kind), parameter :: nblocks_clinic = 1

#ifdef CISO
!  integer (int_kind), parameter :: days_in_leap_year = 366
!  integer (int_kind), parameter :: days_in_norm_year = 365
  integer (int_kind)            :: days_in_year

  real (r8), parameter          :: days_in_grego_year = 365.25_r8
  real (r8), parameter          :: seconds_in_day = 86400.0_r8
  real (r8)                     :: seconds_in_year
#endif

  logical (POP_logical), parameter :: partial_bottom_cells = .true.

  real (POP_r8),  dimension(:,:,:,:), allocatable   :: DZT
  real (POP_r8),  dimension(:),       allocatable   :: dz
  real (POP_r8),  dimension(:,:,:),   allocatable   :: zt
  real (POP_r8),  dimension(:,:,:),   allocatable   :: zw
#ifdef CISO
  real (POP_r8),  dimension(:,:,:),   allocatable   :: TAREA
#endif

  integer (POP_i4), dimension(:,:,:), allocatable   :: KMT
  integer (int_kind) :: nx_block, ny_block, km

#ifdef DIAGNOSTICS_BIO
!-----------------------------------------------------------------------
!  Arrays to output diagnostic variables
!-----------------------------------------------------------------------
  real (r8), dimension(:,:,:,:,:), allocatable :: bec_diag_3d
  real (r8), dimension(:,:,:,:)  , allocatable :: bec_diag_2d
#endif



!*****************************************************************************

contains

!*****************************************************************************

#ifdef CISO
  subroutine popmini_init(Istr, Iend, Jstr, Jend, km_in, pm, pn)
#else
  subroutine popmini_init(Istr, Iend, Jstr, Jend, km_in)
#endif
#ifdef DIAGNOSTICS_BIO
   use mod_biology, only : NDbio2d, NDbio3d
#endif

! !INPUT PARAMETER

    integer (int_kind), intent(in) :: Istr, Iend, Jstr, Jend, km_in
#ifdef CISO
    real(r8), dimension(nx_block, ny_block), intent(in) :: pm, pn
#endif

! !LOCAL VARIABLES

    integer, parameter :: bid = 1

   nx_block = Iend - Istr + 1
   ny_block = Jend - Jstr + 1
   km       = km_in

!-----------------------------------------------------------------------
!   initialize KMT,DZT,dz,zt,TAREA
!-----------------------------------------------------------------------

    allocate( KMT(nx_block, ny_block , max_blocks_clinic) )
    allocate( DZT(nx_block, ny_block , 0:km+1, max_blocks_clinic) )
    allocate( dz(km) )
    allocate( zt(nx_block, ny_block, km) )
    allocate( zw(nx_block, ny_block, km) )
#ifdef CISO
    allocate( TAREA(nx_block, ny_block, max_blocks_clinic) )
#endif

#ifdef DIAGNOSTICS_BIO
!-----------------------------------------------------------------------
!  initialize arrays for diagnostic variable
!-----------------------------------------------------------------------

   if ( NDbio3d .lt. 1 ) then
      allocate( bec_diag_3d(nx_block,ny_block,km,      1,max_blocks_clinic) )
   else
      allocate( bec_diag_3d(nx_block,ny_block,km,NDbio3d,max_blocks_clinic) )
   end if
   if ( NDbio2d .lt. 1 ) then
      allocate( bec_diag_2d(nx_block,ny_block,         1,max_blocks_clinic) )
   else
      allocate( bec_diag_2d(nx_block,ny_block,   NDbio2d,max_blocks_clinic) )
   end if
#endif

!-----------------------------------------------------------------------
!   set KMT
!-----------------------------------------------------------------------

    KMT(:,:,bid) = km
#ifdef CISO
!-----------------------------------------------------------------------
!  set seconds_in_year
!-----------------------------------------------------------------------

!    if () then
!      days_in_year = days_in_leap_year
!    else
!      days_in_year = days_in_norm_year
!    endif

!    seconds_in_year = days_in_year * seconds_in_day
    seconds_in_year = days_in_grego_year * seconds_in_day
#endif

#ifdef CISO
!-----------------------------------------------------------------------
!   set TAREA
!-----------------------------------------------------------------------
    TAREA(:,:,bid) = (1.0d0 / pm)*(1.0d0 / pn) * 1.0d4! conv [m2] -> [cm2]
#endif

    return
  end subroutine popmini_init

!***********************************************************************

 subroutine popmini_set_boundary(roms_DZT, roms_zt, roms_zw)

! !INPUT PARAMETER

    real (r8), dimension(nx_block, ny_block, km), intent(in) :: roms_DZT
    real (r8), dimension(nx_block, ny_block, km), intent(in) :: roms_zt
    real (r8), dimension(nx_block, ny_block, km), intent(in) :: roms_zw

! !LOCAL VARIABLES

    integer (int_kind), parameter :: bid = 1

!-----------------------------------------------------------------------
!  set DZT,zt
!-----------------------------------------------------------------------
!   DZTについて
!  ecosys_mod.Fでは、k=1:kmしか参照しないので、
!  k=0とk=km+1は設定しない
!-----------------------------------------------------------------------

    DZT(:,:,1:km,bid) = roms_DZT(:,:,km:1:-1) * 100.0  !conv [m] -> [cm]
    zt(:,:,1:km) = -roms_zt(:,:,km:1:-1) * 100.0       !conv [m] -> [cm]
    zw(:,:,1:km) = -roms_zw(:,:,km:1:-1) * 100.0       !conv [m] -> [cm]

    return
 end subroutine popmini_set_boundary

!***********************************************************************

 subroutine popmini_abort(string)

! !INPUT PARAMETER

    character (*), intent(in) :: string !error_message

! !LOCAL VARIABLES

    integer (int_kind), parameter :: rcode = 1001
    integer (int_kind) :: ierr

!-----------------------------------------------------------------------
!  abort bec model
!-----------------------------------------------------------------------
!  poc_errorがTrueになったときのみ、callされるサブルーチン
!-----------------------------------------------------------------------
    write(*,*) "popmini_abort:", trim(string)

!    call ROMS_finalize

#ifdef MPI
    call MPI_ABORT(MPI_COMM_WORLD, rcode, ierr)
#else
    stop rcode
#endif

    return
 end subroutine popmini_abort
#endif

!***********************************************************************

#ifdef DIAGNOSTICS_BIO
 function popmini_tavg_now(field_id)

! !INPUT PARAMETER

    integer (int_kind), intent(in) :: &
      field_id

! !OUTPUT PARMETER

    logical (log_kind) :: &
      popmini_tavg_now

!-----------------------------------------------------------------------
! 診断変数を出力する時のみ使用される
!-----------------------------------------------------------------------

      popmini_tavg_now = .false.

      if (field_id > 0) then
         popmini_tavg_now = .true.
      end if

 end function popmini_tavg_now
#endif

!***********************************************************************

#ifdef BEC
 function pop_ref_pressure(k, j)

! !INPUT PARAMETER

    integer (int_kind), intent(in) :: &
      k, j

! !OUTPUT PARAMETER

    real (r8), dimension(nx_block) :: &
      pop_ref_pressure

! !LOCAL VARIABLES

    real(r8), dimension(nx_block) :: depth

!-----------------------------------------------------------------------
! 診断変数を出力する時のみ使用される
!-----------------------------------------------------------------------
!EOP
!BOC
!-----------------------------------------------------------------------
!
!  return pre-computed reference pressure at level k
!
!-----------------------------------------------------------------------

    depth = zt(:,j,k) * mpercm
    pop_ref_pressure = 0.059808_r8*(exp(-0.025_r8*depth) - c1)     &
            + 0.100766_r8*depth + 2.28405e-7_r8*depth**2

!-----------------------------------------------------------------------
!EOC

 end function pop_ref_pressure
#endif

#ifdef DIAGNOSTICS_BIO
 subroutine accumulate_tavg_field_3d(ary, inum, bid, k)

    integer(int_kind), intent(in) :: inum, bid, k
    real (r8), dimension(nx_block, ny_block), intent(in) :: ary

    bec_diag_3d(:,:,k,inum,bid) =  ary(:,:)

 end subroutine accumulate_tavg_field_3d

 subroutine accumulate_tavg_field_2d(ary, inum, bid)

    integer(int_kind), intent(in) :: inum, bid
    real (r8), dimension(nx_block, ny_block), intent(in) :: ary

    bec_diag_2d(:,:,inum,bid) =  ary(:,:)

 end subroutine accumulate_tavg_field_2d
#endif

end module popmini_mod
