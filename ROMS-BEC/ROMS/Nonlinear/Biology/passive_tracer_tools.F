!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#include "cppdefs.h"
 module passive_tracer_tools
#if defined BEC

!BOP
! !MODULE: passive_tracer_tools

! !DESCRIPTION:
!  This module provides support for passive tracers.
!  Subroutines in this module can be called by individual tracer modules.

! !REVISION HISTORY:
!  SVN:$Id:$

! !USES:
#ifdef BEC
   use mod_kinds
   use POP_KindsMod
   use kinds_mod
   use constants
   use ecosys_parms
   use popmini_mod          !DCC new module
#else
!c   use kinds_mod
!c   use constants, only: c0, c1, p5, char_blank, blank_fmt
!c   use domain_size, only: nx_global, ny_global, max_blocks_clinic,nt
!c   use domain, only: nblocks_clinic,distrb_clinic, blocks_clinic
!c   use exit_mod, only: sigAbort, exit_POP
!c   use communicate, only: my_task, master_task
!c   use constants, only: char_blank, field_loc_center, field_type_scalar,delim_fmt
!c   use prognostic, only: tracer_field
!c   use io_tools, only: document
!c   use io, only: data_set
!c   use io_types, only: datafile, io_dim, io_field_desc, rec_type_dbl, &
!c       construct_file, construct_io_dim, construct_io_field, &
!c       destroy_file, destroy_io_field, stdout,add_attrib_file,&
!c       extract_attrib_file
!c   use prognostic, only: curtime, oldtime
!c   use grid, only: TAREA, RCALCT, area_t
!c   use global_reductions, only: global_sum
!c   use blocks
#endif

   implicit none
   private
   save

! !PUBLIC TYPES:

   type, public :: ind_name_pair
      integer(int_kind) :: ind
      character(char_len) :: name
   end type ind_name_pair

!-----------------------------------------------------------------------
!  derived type for reading tracers from a file
!-----------------------------------------------------------------------

   type, public :: tracer_read
      character(char_len) :: mod_varname, filename, file_varname, file_fmt
      real(r8) :: scale_factor, default_val

   end type tracer_read

!-----------------------------------------------------------------------
!  monthly forcing variables
!-----------------------------------------------------------------------

   type, public :: forcing_monthly_every_ts
      type(tracer_read) :: input
      logical(log_kind) :: has_data
      real(r8), dimension(:,:,:,:,:), pointer :: DATA
      character(char_len) :: &
         interp_type,          & ! = 'linear'
         data_type,            & ! = 'monthly-calendar'
         interp_freq,          & ! = 'every-timestep'
         filename,             & ! = 'not-used-for-monthly'
         data_label              ! = 'not-used-for-monthly'
    real(r8), dimension(12) :: &
         data_time               ! times where DATA is given
    real(r8), dimension(20) :: &
         data_renorm             ! not used for monthly
    real(r8) :: &
         data_inc,             & ! not used for monthly data
         data_next,            & ! time that will be used for the next
                                 ! value of forcing data that is needed
         data_update,          & ! time when the a new forcing value
                                 ! needs to be added to interpolation set
         interp_inc,           & ! not used for 'every-timestep' interp
         interp_next,          & ! not used for 'every-timestep' interp
         interp_last             ! not used for 'every-timestep' interp
    integer(int_kind) :: &
         data_time_min_loc       ! index of the third dimension of data_time
                                 ! containing the minimum forcing time
  end type forcing_monthly_every_ts
!c
!c! !PUBLIC MEMBER FUNCTIONS:
!c
!c   public ::                                 &
!c      init_forcing_monthly_every_ts,         &
!c      field_exists_in_file,                  &
!c      file_read_tracer_block,                &
!c      rest_read_tracer_block,                &
!c      read_field,                            &
!c      name_to_ind,                           &
!c      extract_surf_avg,                      &
!c      comp_surf_avg,                         &
!c      set_tracer_indices,                    &
!c      tracer_read_init
!c
!c!EOP
!c!BOC
!c
!c   interface read_field
!c      module procedure read_field_2D,        &
!c                       read_field_3D
!c   end interface
!c
!c!EOC
!c!***********************************************************************
!c
!c contains
!c
!c!***********************************************************************
!c!BOP
!c! !IROUTINE: init_forcing_monthly_every_ts(var)
!c! !INTERFACE:
!c
!c subroutine init_forcing_monthly_every_ts(var)
!c
!c! !DESCRIPTION:
!c!  initialize fields of a variable of type forcing_monthly_every_ts
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !OUTPUT PARAMETERS:
!c
!c   type(forcing_monthly_every_ts), intent(out) :: var
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c
!c   var%interp_type = 'linear'
!c   var%data_type   = 'monthly-calendar'
!c   var%interp_freq = 'every-timestep'
!c   var%filename    = 'not-used-for-monthly'
!c   var%data_label  = 'not-used-for-monthly'
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end subroutine init_forcing_monthly_every_ts
!c
!c!***********************************************************************
!c!BOP
!c! !IROUTINE: name_to_ind
!c! !INTERFACE:
!c
!c function name_to_ind(name, ind_name_table)
!c
!c! !DESCRIPTION:
!c!  translate string into a tracer index
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !INPUT PARAMETERS:
!c
!c   character(*), intent(in) :: name
!c
!c   type(ind_name_pair), dimension(:), intent(in) :: ind_name_table
!c
!c! !OUTPUT PARAMETERS:
!c
!c   integer(int_kind) :: name_to_ind
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   integer(int_kind) :: i
!c
!c!-----------------------------------------------------------------------
!c
!c   do i = 1,size(ind_name_table)
!c      if (trim(name) == trim(ind_name_table(i)%name)) then
!c         name_to_ind = ind_name_table(i)%ind
!c         return
!c      end if
!c   end do
!c
!c   name_to_ind = 0
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end function name_to_ind
!c
!c!***********************************************************************
!c!BOP
!c! !IROUTINE: field_exists_in_file
!c! !INTERFACE:
!c
!c function field_exists_in_file(fmt, filename, fieldname)
!c
!c! !DESCRIPTION:
!c!  This routine determines if a field exists in a file.
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !INPUT PARAMETERS:
!c
!c   character (*), intent(in) ::  &
!c      fmt,                 & ! format (bin or nc)
!c      filename,            & ! file to read from
!c      fieldname              ! field to be read
!c
!c! !OUTPUT PARAMETERS:
!c
!c   logical (log_kind) :: field_exists_in_file
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   character(*), parameter :: &
!c      subname = 'passive_tracer_tools:field_exists_in_file'
!c
!c   type (datafile) :: &
!c      in_file         ! data file type for init ts file
!c
!c!-----------------------------------------------------------------------
!c
!c   in_file = construct_file(fmt, full_name=trim(filename), &
!c                            record_length=rec_type_dbl, &
!c                            recl_words=nx_global*ny_global)
!c
!c   call data_set(in_file, 'open_read')
!c
!c   call data_set (in_file, 'field_exists', &
!c                  fieldname=fieldname, field_exists=field_exists_in_file)
!c
!c   call data_set (in_file, 'close')
!c
!c   call destroy_file (in_file)
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end function field_exists_in_file
!c
!c!***********************************************************************
!c!BOP
!c! !IROUTINE: rest_read_tracer_block
!c! !INTERFACE:
!c
!c subroutine rest_read_tracer_block(fmt, filename, tracer_d_module, &
!c    TRACER_MODULE)
!c
!c! !DESCRIPTION:
!c!  read from a restart file all tracers for a tracer module
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !INPUT PARAMETERS:
!c
!c   character (*), intent(in) ::  &
!c      fmt,                 & ! format (bin or nc)
!c      filename               ! file name for restart file
!c
!c   type (tracer_field), dimension(:), intent(in) :: &
!c      tracer_d_module   ! descriptors for each tracer
!c
!c! !INPUT/OUTPUT PARAMETERS:
!c
!c   real(r8), dimension(:,:,:,:,:,:), intent(inout) :: &
!c      TRACER_MODULE     ! tracers to be read in
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   character(*), parameter :: &
!c      subname = 'passive_tracer_tools:rest_read_tracer_block'
!c
!c   integer (int_kind) :: &
!c      n               ! tracer index
!c
!c   character (char_len) ::  &
!c      fieldname       ! tracer name temporaries
!c
!c!-----------------------------------------------------------------------
!c
!c   call document(subname, 'reading tracer block from ' /&
!c                       &/ trim(filename))
!c
!c   do n=1,size(tracer_d_module)
!c
!c      fieldname = char_blank
!c      fieldname = trim(tracer_d_module(n)%short_name) /&
!c               &/ '_CUR'
!c
!c      call read_field(fmt, filename, fieldname, TRACER_MODULE(:,:,:,n,curtime,:))
!c
!c      fieldname = char_blank
!c      fieldname = trim(tracer_d_module(n)%short_name) /&
!c               &/ '_OLD'
!c
!c      call read_field(fmt, filename, fieldname, TRACER_MODULE(:,:,:,n,oldtime,:))
!c
!c   end do
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end subroutine rest_read_tracer_block
!c
!c!***********************************************************************
!c!BOP
!c! !IROUTINE: file_read_tracer_block
!c! !INTERFACE:
!c
!c subroutine file_read_tracer_block(init_filename_fmt, init_filename, &
!c   tracer_d_module, ind_name_table, tracer_init_ext, TRACER_MODULE)
!c
!c! !DESCRIPTION:
!c!  read from a file all tracers for a tracer module
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !INPUT PARAMETERS:
!c
!c   character (*), intent(in) ::  &
!c      init_filename_fmt,   & ! format (bin or nc)
!c      init_filename          ! default filename
!c
!c   type (tracer_field), dimension(:), intent(in) :: &
!c      tracer_d_module        ! descriptors for each tracer
!c
!c   type(tracer_read), dimension(:), intent(in) :: &
!c      tracer_init_ext        ! namelist variable for initializing tracers
!c
!c   type(ind_name_pair), dimension(:) :: &
!c      ind_name_table
!c
!c! !INPUT/OUTPUT PARAMETERS:
!c
!c   real(r8), dimension(:,:,:,:,:,:), intent(inout) :: &
!c      TRACER_MODULE          ! tracers to be read in
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   character(*), parameter :: &
!c      subname = 'passive_tracer_tools:file_read_tracer_block'
!c
!c   integer (int_kind) :: &
!c      tracer_cnt,   & ! number of tracers in module
!c      ind,          & ! tracer index for tracer name from namelist
!c      iblock,       & ! index for looping over blocks
!c      n               ! tracer index
!c
!c   type(tracer_read), dimension(:), allocatable :: &
!c      tracer_init_int ! namelist variable for initializing tracers
!c
!c   type (datafile) ::    &
!c      in_file         ! data file type for init ts file
!c
!c!-----------------------------------------------------------------------
!c
!c   tracer_cnt = size(tracer_d_module)
!c
!c!-----------------------------------------------------------------------
!c!  initialize internal tracer_init array
!c!-----------------------------------------------------------------------
!c
!c   allocate(tracer_init_int(tracer_cnt))
!c   do n = 1,tracer_cnt
!c      tracer_init_int(n)%mod_varname  = tracer_d_module(n)%short_name
!c      tracer_init_int(n)%filename     = init_filename
!c      tracer_init_int(n)%file_varname = tracer_d_module(n)%short_name
!c      tracer_init_int(n)%scale_factor = c1
!c      tracer_init_int(n)%default_val  = c0
!c      tracer_init_int(n)%file_fmt     = init_filename_fmt
!c   end do
!c
!c!-----------------------------------------------------------------------
!c!  copy non-default values from external tracer_init array
!c!-----------------------------------------------------------------------
!c
!c   do n = 1,tracer_cnt
!c      if (trim(tracer_init_ext(n)%mod_varname) /= 'unknown') then
!c         ind = name_to_ind(tracer_init_ext(n)%mod_varname, ind_name_table)
!c         if (ind == 0) then
!c            call document(subname, 'unknown external varname = ', &
!c                          trim(tracer_init_ext(n)%mod_varname))
!c            call exit_POP(sigAbort, 'stopping in ' /&
!c                                 &/ subname)
!c         end if
!c
!c         if (trim(tracer_init_ext(n)%filename) /= 'unknown') &
!c              tracer_init_int(ind)%filename = &
!c              tracer_init_ext(n)%filename
!c
!c         if (trim(tracer_init_ext(n)%file_varname) /= 'unknown') &
!c              tracer_init_int(ind)%file_varname = &
!c              tracer_init_ext(n)%file_varname
!c
!c         if (tracer_init_ext(n)%scale_factor /= c1) &
!c              tracer_init_int(ind)%scale_factor = &
!c              tracer_init_ext(n)%scale_factor
!c
!c         if (tracer_init_ext(n)%default_val /= c1) &
!c              tracer_init_int(ind)%default_val = &
!c              tracer_init_ext(n)%default_val
!c      end if
!c   end do
!c
!c!-----------------------------------------------------------------------
!c!  process internal tracer_init array
!c!-----------------------------------------------------------------------
!c
!c   do n = 1,tracer_cnt
!c      if (trim(tracer_init_int(n)%filename) == 'none' .or. &
!c          trim(tracer_init_int(n)%filename) == 'unknown') then
!c         call document(subname, 'initializing ' /&
!c                             &/ trim(tracer_init_int(n)%mod_varname) /&
!c                             &/ ' to default_val')
!c         do iblock = 1,nblocks_clinic
!c            TRACER_MODULE(:,:,:,n,curtime,iblock) =  &
!c               tracer_init_int(n)%default_val
!c         enddo
!c      else
!c         call document(subname, 'initializing ' /&
!c                             &/ trim(tracer_init_int(n)%mod_varname) /&
!c                             &/ ' with ' /&
!c                             &/ trim(tracer_init_int(n)%file_varname) /&
!c                             &/ ' from ' /&
!c                             &/ trim(tracer_init_int(n)%filename))
!c
!c         call read_field(tracer_init_int(n)%file_fmt,        &
!c                         tracer_init_int(n)%filename,        &
!c                         tracer_init_int(n)%file_varname,    &
!c                         TRACER_MODULE(:,:,:,n,curtime,:))
!c
!c         do iblock=1,nblocks_clinic
!c            TRACER_MODULE(:,:,:,n,curtime,iblock) = &
!c               TRACER_MODULE(:,:,:,n,curtime,iblock)*tracer_init_int(n)%scale_factor
!c            where (TRACER_MODULE(:,:,:,n,curtime,iblock) < c0) &
!c               TRACER_MODULE(:,:,:,n,curtime,iblock) = c0
!c         end do
!c
!c         if (my_task == master_task) then
!c            write(stdout,blank_fmt)
!c            write(stdout,'(a12,a)') ' file read: ', &
!c               trim(tracer_init_int(n)%filename)
!c         endif
!c
!c      end if
!c      do iblock=1,nblocks_clinic
!c         TRACER_MODULE(:,:,:,n,oldtime,iblock) = &
!c            TRACER_MODULE(:,:,:,n,curtime,iblock)
!c      enddo
!c   end do
!c
!c   deallocate(tracer_init_int)
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end subroutine file_read_tracer_block
!c
!c!***********************************************************************
!c!BOP
!c! !IROUTINE: read_field_2D
!c! !INTERFACE:
!c
!c subroutine read_field_2D(fmt, filename, fieldname, FIELD, record_length)
!c
!c! !DESCRIPTION:
!c!  read 2D field from a file
!c!  Assumes the field is (nx_global,ny_global), cell centered, and scalar.
!c!  For binary files, the default external precision is double precision.
!c!  This can be overridden by passing the desired precision into record_length.
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !INPUT PARAMETERS:
!c
!c   character (*), intent(in) ::  &
!c      fmt,                 & ! format (bin or nc)
!c      filename,            & ! file to read from
!c      fieldname              ! field to be read
!c
!c   integer(int_kind), intent(in), optional :: &
!c      record_length          ! record length type for binary files
!c
!c! !INPUT/OUTPUT PARAMETERS:
!c
!c   real(r8), dimension(:,:,:), intent(inout), target :: &
!c      FIELD                  ! field to be read in
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   character(*), parameter :: &
!c      subname = 'passive_tracer_tools:read_field_2D'
!c
!c   integer(int_kind) :: &
!c      record_length_loc    ! record length type for binary files
!c
!c   type (io_field_desc) :: &
!c      FIELD_DESC           ! IO field descriptors for FIELD
!c
!c   type (datafile) :: &
!c      restart_file         ! io file descriptor
!c
!c   type (io_dim) :: &
!c      i_dim, j_dim         ! dimension descriptors
!c
!c!-----------------------------------------------------------------------
!c
!c   call document(subname, 'reading ' /&
!c                       &/ trim(fieldname) /&
!c                       &/ ' from ' /&
!c                       &/ trim(filename))
!c
!c   if (present(record_length)) then
!c      record_length_loc = record_length
!c   else
!c      record_length_loc = rec_type_dbl
!c   endif
!c
!c   restart_file =                                     &
!c      construct_file(fmt,                             &
!c                     full_name=trim(filename),        &
!c                     record_length=record_length_loc, &
!c                     recl_words=nx_global*ny_global)
!c
!c   call data_set(restart_file, 'open_read')
!c
!c   i_dim = construct_io_dim('i', nx_global)
!c   j_dim = construct_io_dim('j', ny_global)
!c
!c   FIELD_DESC =                                       &
!c      construct_io_field(trim(fieldname),             &
!c                         dim1=i_dim,                  &
!c                         dim2=j_dim,                  &
!c                         d2d_array = FIELD)
!c
!c   call data_set (restart_file, 'define', FIELD_DESC)
!c
!c   call data_set (restart_file, 'read', FIELD_DESC)
!c
!c   call destroy_io_field (FIELD_DESC)
!c
!c   call data_set (restart_file, 'close')
!c
!c   call destroy_file (restart_file)
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end subroutine read_field_2D
!c
!c!***********************************************************************
!c!BOP
!c! !IROUTINE: read_field_3D
!c! !INTERFACE:
!c
!c subroutine read_field_3D(fmt, filename, fieldname, FIELD, record_length)
!c
!c! !DESCRIPTION:
!c!  read 3D field from a file
!c!  Assumes the field is (nx_global,ny_global), cell centered, and scalar.
!c!  The length of the 3rd dimension is determined by the dimension of FIELD.
!c!  For binary files, the default external precision is double precision.
!c!  This can be overridden by passing the desired precision into record_length.
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !INPUT PARAMETERS:
!c
!c   character (*), intent(in) ::  &
!c      fmt,                 & ! format (bin or nc)
!c      filename,            & ! file to read from
!c      fieldname              ! field to be read
!c
!c   integer(int_kind), intent(in), optional :: &
!c      record_length          ! record length type for binary files
!c
!c! !INPUT/OUTPUT PARAMETERS:
!c
!c   real(r8), dimension(:,:,:,:), intent(inout), target :: &
!c      FIELD                  ! field to be read in
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   character(*), parameter :: &
!c      subname = 'passive_tracer_tools:read_field_3D'
!c
!c   integer(int_kind) :: &
!c      record_length_loc    ! record length type for binary files
!c
!c   type (io_field_desc) :: &
!c      FIELD_DESC           ! IO field descriptors for FIELD
!c
!c   type (datafile) :: &
!c      restart_file         ! io file descriptor
!c
!c   type (io_dim) :: &
!c      i_dim, j_dim, k_dim  ! dimension descriptors
!c
!c!-----------------------------------------------------------------------
!c
!c   call document(subname, 'reading ' /&
!c                       &/ trim(fieldname) /&
!c                       &/ ' from ' /&
!c                       &/ trim(filename))
!c
!c   if (present(record_length)) then
!c      record_length_loc = record_length
!c   else
!c      record_length_loc = rec_type_dbl
!c   endif
!c
!c   restart_file =                                     &
!c      construct_file(fmt,                             &
!c                     full_name=trim(filename),        &
!c                     record_length=record_length_loc, &
!c                     recl_words=nx_global*ny_global)
!c
!c   call data_set(restart_file, 'open_read')
!c
!c   i_dim = construct_io_dim('i', nx_global)
!c   j_dim = construct_io_dim('j', ny_global)
!c   k_dim = construct_io_dim('k', size(FIELD,3))
!c
!c   FIELD_DESC =                                       &
!c      construct_io_field(trim(fieldname),             &
!c                         dim1=i_dim,                  &
!c                         dim2=j_dim,                  &
!c                         dim3=k_dim,                  &
!c                         d3d_array = FIELD)
!c
!c   call data_set (restart_file, 'define', FIELD_DESC)
!c
!c   call data_set (restart_file, 'read', FIELD_DESC)
!c
!c   call destroy_io_field (FIELD_DESC)
!c
!c   call data_set (restart_file, 'close')
!c
!c   call destroy_file (restart_file)
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end subroutine read_field_3D
!c
!c!***********************************************************************
!c!BOP
!c! !IROUTINE: extract_surf_avg
!c! !INTERFACE:
!c
!c subroutine extract_surf_avg(init_file_fmt,restart_filename,&
!c                             tracer_cnt,vflux_flag,ind_name_table,&
!c                             surf_avg)
!c
!c! !DESCRIPTION:
!c!  Extract average surface values from restart file.
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !INPUT PARAMETERS:
!c
!c   character (*), intent(in) :: &
!c      init_file_fmt, & ! file format (bin or nc)
!c      restart_filename ! file name for restart file
!c
!c   integer (int_kind),intent(in) :: &
!c      tracer_cnt
!c
!c   logical (log_kind), dimension(tracer_cnt),intent(in) :: &
!c      vflux_flag                ! which tracers get virtual fluxes applied
!c
!c   type(ind_name_pair), dimension(tracer_cnt),intent(in) :: &
!c      ind_name_table
!c
!c! !Output PARAMETERS:
!c
!c   real (r8), dimension(tracer_cnt),intent(out) :: &
!c      surf_avg         ! average surface tracer values
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   type (datafile) ::&
!c      restart_file    ! io file descriptor
!c
!c   integer (int_kind) :: &
!c      n               ! tracer index
!c
!c   character (char_len) :: &
!c      short_name      ! tracer name temporaries
!c
!c
!c!-----------------------------------------------------------------------
!c
!c   surf_avg = c0
!c
!c   restart_file = construct_file(init_file_fmt, &
!c                                 full_name=trim(restart_filename), &
!c                                 record_length=rec_type_dbl, &
!c                                 recl_words=nx_global*ny_global)
!c
!c   do n = 1, tracer_cnt
!c      if (vflux_flag(n)) then
!c         short_name = 'surf_avg_' /&
!c                   &/ ind_name_table(n)%name
!c         call add_attrib_file(restart_file, trim(short_name), surf_avg(n))
!c      endif
!c   end do
!c
!c   call data_set(restart_file, 'open_read')
!c
!c   do n = 1, tracer_cnt
!c      if (vflux_flag(n)) then
!c         short_name = 'surf_avg_' /&
!c                   &/ ind_name_table(n)%name
!c         call extract_attrib_file(restart_file, trim(short_name), surf_avg(n))
!c      endif
!c   end do
!c
!c   call data_set (restart_file, 'close')
!c
!c   call destroy_file (restart_file)
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end subroutine extract_surf_avg
!c!*****************************************************************************
!c!BOP
!c! !IROUTINE: comp_surf_avg
!c! !INTERFACE:
!c
!c subroutine comp_surf_avg(SURF_VALS_OLD,SURF_VALS_CUR,tracer_cnt,&
!c                         vflux_flag,surf_avg)
!c
!c! !DESCRIPTION:
!c!  compute average surface tracer values
!c!
!c!  avg = sum(SURF_VAL*TAREA) / sum(TAREA)
!c!  with the sum taken over ocean points only
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c
!c! !INPUT PARAMETERS:
!c!real (r8), dimension(nx_block,ny_block,tracer_cnt,max_blocks_clinic), &
!c   real (r8), dimension(:,:,:,:), &
!c      intent(in) :: SURF_VALS_OLD, SURF_VALS_CUR
!c
!c   integer (int_kind),intent(in) :: &
!c      tracer_cnt                ! number of tracers
!c
!c   logical (log_kind), dimension(tracer_cnt),intent(in) :: &
!c      vflux_flag                ! which tracers get virtual fluxes applied
!c
!c! !Output PARAMETERS:
!c
!c   real (r8), dimension(tracer_cnt),intent(out) :: &
!c      surf_avg         ! average surface tracer values
!c
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   integer (int_kind) :: &
!c      n,       & ! tracer index
!c      iblock,  & ! block index
!c      dcount,  & ! diag counter
!c      ib,ie,jb,je
!c
!c   real (r8), dimension(max_blocks_clinic,tracer_cnt) :: &
!c      local_sums ! array for holding block sums of each diagnostic
!c
!c   real (r8) :: &
!c      sum_tmp    ! temp for local sum
!c
!c   real (r8), dimension(nx_block,ny_block) :: &
!c      WORK1, &   ! local work space
!c      TFACT      ! factor for normalizing sums
!c
!c   type (block) :: &
!c      this_block ! block information for current block
!c
!c!-----------------------------------------------------------------------
!c
!c   local_sums = c0
!c
!c!jw   !$OMP PARALLEL DO PRIVATE(iblock,this_block,ib,ie,jb,je,TFACT,n,WORK1)
!c   do iblock = 1,nblocks_clinic
!c      this_block = get_block(blocks_clinic(iblock),iblock)
!c      ib = this_block%ib
!c      ie = this_block%ie
!c      jb = this_block%jb
!c      je = this_block%je
!c      TFACT = TAREA(:,:,iblock)*RCALCT(:,:,iblock)
!c
!c      do n = 1, tracer_cnt
!c         if (vflux_flag(n)) then
!c            WORK1 = p5*(SURF_VALS_OLD(:,:,n,iblock) + &
!c                        SURF_VALS_CUR(:,:,n,iblock))*TFACT
!c            local_sums(iblock,n) = sum(WORK1(ib:ie,jb:je))
!c         endif
!c      end do
!c   end do
!c!jw   !$OMP END PARALLEL DO
!c
!c   do n = 1, tracer_cnt
!c      if (vflux_flag(n)) then
!c         sum_tmp = sum(local_sums(:,n))
!c         surf_avg(n) = global_sum(sum_tmp,distrb_clinic)/area_t
!c      endif
!c   end do
!c
!c   if(my_task == master_task) then
!c      write(stdout,*)' Calculating surface tracer averages'
!c      do n = 1, tracer_cnt
!c         if (vflux_flag(n)) then
!c            write(stdout,*) n, surf_avg(n)
!c         endif
!c      end do
!c   endif
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end subroutine comp_surf_avg
!c!*****************************************************************************
!c!BOP
!c! !IROUTINE: set_tracer_indices
!c! !INTERFACE:
!c
!c subroutine set_tracer_indices(module_string, module_nt,  &
!c        cumulative_nt, ind_begin, ind_end)
!c
!c! !DESCRIPTION:
!c!  set the index bounds of a single passive tracer module
!c!
!c! !REVISION HISTORY:
!c!  same as module
!c
!c! !INPUT PARAMETERS:
!c
!c   character (*), intent(in) :: &
!c      module_string
!c
!c   integer (kind=int_kind), intent(in) ::  &
!c      module_nt
!c
!c! !INPUT/OUTPUT PARAMETERS:
!c
!c   integer (kind=int_kind), intent(inout) ::  &
!c      cumulative_nt
!c
!c   integer (kind=int_kind), intent(out) ::  &
!c      ind_begin, &
!c      ind_end
!c
!c!EOP
!c!BOC
!c!-----------------------------------------------------------------------
!c!  local variables
!c!-----------------------------------------------------------------------
!c
!c   character(*), parameter :: subname = 'passive_tracer_tools:set_tracer_indices'
!c
!c   character (char_len) ::  &
!c      error_string
!c
!c!-----------------------------------------------------------------------
!c
!c   ind_begin = cumulative_nt + 1
!c   ind_end = ind_begin + module_nt - 1
!c   cumulative_nt = ind_end
!c
!c   if (my_task == master_task) then
!c      write(stdout,delim_fmt)
!c      write(stdout,*) module_string /&
!c         &/ ' ind_begin = ', ind_begin
!c      write(stdout,*) module_string /&
!c         &/ ' ind_end   = ', ind_end
!c      write(stdout,delim_fmt)
!c   end if
!c
!c   if (cumulative_nt > nt) then
!c      call document(subname, 'nt', nt)
!c      call document(subname, 'cumulative_nt', cumulative_nt)
!c      error_string = 'nt too small for module ' /&
!c         &/ module_string
!c      call exit_POP(sigAbort, error_string)
!c   end if
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c
!c end subroutine set_tracer_indices
!c!***********************************************************************
!c
!c!*****************************************************************************
!c!BOP
!c! !IROUTINE: tracer_read_init
!c! !INTERFACE:
!c
!c subroutine tracer_read_init(tracer, mod_varname, filename, file_varname, file_fmt, &
!c      scale_factor, default_val)
!c
!c! !DESCRIPTION:
!c!  initialize a tracer_read type to common default values.
!c!
!c
!c! !OUTPUT PARAMETERS:
!c   type(tracer_read), intent (out) :: tracer
!c! !INPUT PARAMETERS:
!c   character(char_len), optional, intent(in) :: mod_varname
!c   character(char_len), optional,  intent(in) :: filename
!c   character(char_len), optional,  intent(in) :: file_varname
!c   character(char_len), optional,  intent(in) :: file_fmt
!c   real(r8), optional,  intent(in) :: scale_factor
!c   real(r8), optional,  intent(in) :: default_val
!c
!c!EOP
!c!BOC
!c
!c   tracer%mod_varname = 'unknown'
!c   if (present(mod_varname)) then
!c      tracer%mod_varname = mod_varname
!c   end if
!c
!c   tracer%filename = 'unknown'
!c   if (present(filename)) then
!c      tracer%filename = filename
!c   end if
!c
!c   tracer%file_varname = 'unknown'
!c   if (present(file_varname)) then
!c      tracer%file_varname = file_varname
!c   end if
!c
!c   tracer%file_fmt = 'bin'
!c   if (present(file_fmt)) then
!c      tracer%file_fmt = file_fmt
!c   end if
!c
!c   tracer%scale_factor = c1
!c   if (present(scale_factor)) then
!c      tracer%scale_factor = scale_factor
!c   end if
!c
!c   tracer%default_val = c0
!c   if (present(default_val)) then
!c      tracer%default_val = default_val
!c   end if
!c
!c!-----------------------------------------------------------------------
!c!EOC
!c end subroutine tracer_read_init
!c!***********************************************************************

#endif
 end module passive_tracer_tools

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
