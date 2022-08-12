      USE mod_param
!
      implicit none
!
!  Set coag particle identification indices.
!

      integer, parameter :: param_n_sections = 47
      integer, parameter :: param_n_layers   = 20 ! this should be set to the same value of N(ng)

      integer :: idcoag(param_n_sections) ! coag tracers
      integer :: p001_ind               !  1.  particle #001
      integer :: p002_ind               !  2.  particle #002
      integer :: p003_ind               !  3.  particle #003
      integer :: p004_ind               !  4.  particle #004
      integer :: p005_ind               !  5.  particle #005
      integer :: p006_ind               !  6.  particle #006
      integer :: p007_ind               !  7.  particle #007
      integer :: p008_ind               !  8.  particle #008
      integer :: p009_ind               !  9.  particle #009
      integer :: p010_ind               ! 10.  particle #010
      integer :: p011_ind               ! 11.  particle #011
      integer :: p012_ind               ! 12.  particle #012
      integer :: p013_ind               ! 13.  particle #013
      integer :: p014_ind               ! 14.  particle #014
      integer :: p015_ind               ! 15.  particle #015
      integer :: p016_ind               ! 16.  particle #016
      integer :: p017_ind               ! 17.  particle #017
      integer :: p018_ind               ! 18.  particle #018
      integer :: p019_ind               ! 19.  particle #019
      integer :: p020_ind               ! 20.  particle #020
      integer :: p021_ind               ! 21.  particle #021
      integer :: p022_ind               ! 22.  particle #022
      integer :: p023_ind               ! 23.  particle #023
      integer :: p024_ind               ! 24.  particle #024
      integer :: p025_ind               ! 25.  particle #025
      integer :: p026_ind               ! 26.  particle #026
      integer :: p027_ind               ! 27.  particle #027
      integer :: p028_ind               ! 28.  particle #028
      integer :: p029_ind               ! 29.  particle #029
      integer :: p030_ind               ! 30.  particle #030
      integer :: p031_ind               ! 31.  particle #031
      integer :: p032_ind               ! 32.  particle #032
      integer :: p033_ind               ! 33.  particle #033
      integer :: p034_ind               ! 34.  particle #034
      integer :: p035_ind               ! 35.  particle #035
      integer :: p036_ind               ! 36.  particle #036
      integer :: p037_ind               ! 37.  particle #037
      integer :: p038_ind               ! 38.  particle #038
      integer :: p039_ind               ! 39.  particle #039
      integer :: p040_ind               ! 40.  particle #040
      integer :: p041_ind               ! 41.  particle #041
      integer :: p042_ind               ! 42.  particle #042
      integer :: p043_ind               ! 43.  particle #043
      integer :: p044_ind               ! 44.  particle #044
      integer :: p045_ind               ! 45.  particle #045
      integer :: p046_ind               ! 46.  particle #046
      integer :: p047_ind               ! 47.  particle #047

#if defined DIAGNOSTICS && defined DIAGNOSTICS_COAG
!
!  Coag 2D diagnostic variable IDs.
!
      integer, allocatable :: iDcoag2(:)       ! 2D biological terms
!
!  Coag 3D diagnostic variable IDs.
!
      integer, allocatable :: iDcoag3(:)       ! 3D biological terms
!
!  Coag spectrum diagnostic variable IDs.
!
#endif

!!
!!  Coag parameters.
!!
!      integer, allocatable  :: BioIter(:)
!
!#ifdef ANA_BIOLOGY
!      real(r8), allocatable :: BioIni(:,:)
!#endif
!      real(r8), allocatable :: AttPhy(:)       ! m2/mmole
!      real(r8), allocatable :: AttSW(:)        ! 1/m
!      real(r8), allocatable :: DetRR(:)        ! 1/day
!      real(r8), allocatable :: K_NO3(:)        ! mmol/m3
!      real(r8), allocatable :: Ivlev(:)        ! nondimensional
!      real(r8), allocatable :: PARfrac(:)      ! nondimensional
!#ifdef TANGENT
!      real(r8), allocatable :: tl_PARfrac(:)
!#endif
!#ifdef ADJOINT
!      real(r8), allocatable :: ad_PARfrac(:)
!#endif
!      real(r8), allocatable :: PhyIS(:)        ! m2/W
!      real(r8), allocatable :: PhyMRD(:)       ! 1/day
!      real(r8), allocatable :: PhyMRN(:)       ! 1/day
!      real(r8), allocatable :: Vm_NO3(:)       ! 1/day
!      real(r8), allocatable :: wDet(:)         ! m/day
!#ifdef TANGENT
!      real(r8), allocatable :: tl_wDet(:)
!#endif
!#ifdef ADJOINT
!      real(r8), allocatable :: ad_wDet(:)
!#endif
!      real(r8), allocatable :: wPhy(:)         ! m/day
!#ifdef TANGENT
!      real(r8), allocatable :: tl_wPhy(:)
!#endif
!#ifdef ADJOINT
!      real(r8), allocatable :: ad_wPhy(:)
!#endif
!      real(r8), allocatable :: ZooEED(:)       ! nondimensional
!      real(r8), allocatable :: ZooEEN(:)       ! nondimensional
!      real(r8), allocatable :: ZooGR(:)        ! 1/day
!      real(r8), allocatable :: ZooMRD(:)       ! 1/day
!      real(r8), allocatable :: ZooMRN(:)       ! 1/day

!
!     define structure variables
!

      type betas
        real(r8), dimension(param_n_sections,param_n_sections) :: b1, b2, b3, b4, b5, b25, growth, settle, gain, disagg, redist, remin
      end type betas

      type betas_all
        real(r8), dimension(param_n_sections*param_n_layers,param_n_sections*param_n_layers):: b1, b25, growth, settle, gain, disagg, redist, remin
      end type betas_all

      type bndry_int
        real(r8)     :: mi_lo, mi_up
        real(r8)     :: mj_lo, mj_up
        real(r8)     :: mjj, rjj, rvj
      end type bndry_int

      type(betas)     :: b(param_n_layers,param_n_layers)
      type(betas_all) :: ba
      type(bndry_int) :: bndry

!
!     coag parameters
!

      real(r8), parameter :: pi = 3.1415926535d0
      real(r8), parameter :: param_g = 980.d0          ! Accel. due to gravity [cm s^-2]
      real(r8), parameter :: param_day_to_sec = 8.64d4 ! Seconds in a day [s d^-1]
      real(r8), parameter :: param_k = 1.3d-16         ! Boltzmann's constant [erg K^-1]
      real(r8), parameter :: param_r_to_rg = 1.36d0    ! Interaction radius to radius of gyration

      real(r8), parameter :: param_diffthor = 1.0d-5   ! Diffusion const for thorium

      real(r8), parameter :: param_d0 = 1.d-6          ! Diameter of unit particles [cm]
      real(r8), parameter :: param_a0 = param_d0/2.d0
      real(r8), parameter :: param_v0 = (pi/6.d0) * param_d0**3.d0
      real(r8), parameter :: param_fr_dim = 3.00d0     ! Particle fractal dimension

      character(2),parameter :: param_kernel = 'BW'    ! Kernel name (currently not used)

      real(r8), parameter :: param_gro_sec = 2.d0      ! Section at which growth in aggregates starts (currently not used)
      integer, parameter  :: param_num_1 = 40          ! Number of particle cm^-3 in the first section (used only in calculation in INI)

!
!     coag parameters depending on the state variables
!

      real(r8)     :: param_rho_fl                     ! Fluid density [g cm^-3]
      real(r8)     :: param_kvisc                      ! Kinematic viscosity [cm^2 s^-1]

      real(r8)     :: param_temp                       ! Temperature [K]
      real(r8)     :: param_alpha                      ! Stickiness
      real(r8)     :: param_dz                         ! Layer thickness [m]

      real(r8)     :: param_gamma                      ! Average shear rate [s^-1]
      real(r8)     :: param_growth                     ! Specific growth rate [d-1]
      integer      :: param_n_grw                      ! Layers <= param_n_grw are applied to growth
      real(r8)     :: param_disagg                     ! Disaggregation rate [d-1]
      real(r8)     :: param_remin                      ! Remin [d-1]
      integer      :: param_n_rem                      ! Sectional threshold for remin
      real(r8)     :: param_f_rem  (param_n_layers)

      real(r8)     :: param_dvisc
      real(r8)     :: param_del_rho
      real(r8)     :: param_conBr

      real(r8)     :: param_v_lower(param_n_sections)
      real(r8)     :: param_v_upper(param_n_sections)
      real(r8)     :: param_av_vol (param_n_sections)
      real(r8)     :: param_dcomb  (param_n_sections)
      real(r8)     :: param_dwidth (param_n_sections)

      real(r8)     :: amfrac, param_amfrac, param_bmfrac, param_setcon

      character(2) :: coag_id

      real(r8)     :: s1conc


      CONTAINS

      SUBROUTINE initialize_coag
      USE mod_param
      USE mod_grid
!
! Local variable declarations
!
      type(betas) :: b_brown, b_shear, b_ds
      real*8, dimension(param_n_sections,param_n_sections) :: growth, sink_loss

      integer :: i, ic
      integer :: m
      integer :: krow, kcol, istr, iend, jstr, jend

!-----------------------------------------------------------------------
!  Set number of biological tracers.
!-----------------------------------------------------------------------
!
      NCT=param_n_sections
#if defined DIAGNOSTICS && defined DIAGNOSTICS_COAG
!
!-----------------------------------------------------------------------
!  Set sources and sinks biology diagnostic parameters.
!-----------------------------------------------------------------------
!
!  Set number of diagnostics terms.
!
      NDcoag3d=0
      NDcoag2d=0
#endif

!!
!!-----------------------------------------------------------------------
!!  Allocate various module variables.
!!-----------------------------------------------------------------------
!!
!      IF (.not.allocated(BioIter)) THEN
!         allocate ( BioIter(Ngrids) )
!      END IF
!      IF (.not.allocated(AttPhy)) THEN
!         allocate ( AttPhy(Ngrids) )
!      END IF
!      IF (.not.allocated(AttSW)) THEN
!         allocate ( AttSW(Ngrids) )
!      END IF
!      IF (.not.allocated(DetRR)) THEN
!         allocate ( DetRR(Ngrids) )
!      END IF
!      IF (.not.allocated(K_NO3)) THEN
!         allocate ( K_NO3(Ngrids) )
!      END IF
!      IF (.not.allocated(Ivlev)) THEN
!         allocate ( Ivlev(Ngrids) )
!      END IF
!      IF (.not.allocated(PARfrac)) THEN
!         allocate ( PARfrac(Ngrids) )
!      END IF
!#ifdef TANGENT
!      IF (.not.allocated(tl_PARfrac)) THEN
!         allocate ( tl_PARfrac(Ngrids) )
!      END IF
!#endif
!#ifdef ADJOINT
!      IF (.not.allocated(ad_PARfrac)) THEN
!         allocate ( ad_PARfrac(Ngrids) )
!      END IF
!#endif
!      IF (.not.allocated(PhyIS)) THEN
!         allocate ( PhyIS(Ngrids) )
!      END IF
!      IF (.not.allocated(PhyMRD)) THEN
!         allocate ( PhyMRD(Ngrids) )
!      END IF
!      IF (.not.allocated(PhyMRN)) THEN
!         allocate ( PhyMRN(Ngrids) )
!      END IF
!      IF (.not.allocated(Vm_NO3)) THEN
!         allocate ( Vm_NO3(Ngrids) )
!      END IF
!      IF (.not.allocated(wDet)) THEN
!         allocate ( wDet(Ngrids) )
!      END IF
!#ifdef TANGENT
!      IF (.not.allocated(tl_wDet)) THEN
!         allocate ( tl_wDet(Ngrids) )
!      END IF
!#endif
!#ifdef ADJOINT
!      IF (.not.allocated(ad_wDet)) THEN
!         allocate ( ad_wDet(Ngrids) )
!      END IF
!#endif
!      IF (.not.allocated(wPhy)) THEN
!         allocate ( wPhy(Ngrids) )
!      END IF
!#ifdef TANGENT
!      IF (.not.allocated(tl_wPhy)) THEN
!         allocate ( tl_wPhy(Ngrids) )
!      END IF
!#endif
!#ifdef ADJOINT
!      IF (.not.allocated(ad_wPhy)) THEN
!         allocate ( ad_wPhy(Ngrids) )
!      END IF
!#endif
!      IF (.not.allocated(ZooEED)) THEN
!         allocate ( ZooEED(Ngrids) )
!      END IF
!      IF (.not.allocated(ZooEEN)) THEN
!         allocate ( ZooEEN(Ngrids) )
!      END IF
!      IF (.not.allocated(ZooGR)) THEN
!         allocate ( ZooGR(Ngrids) )
!      END IF
!      IF (.not.allocated(ZooMRD)) THEN
!         allocate ( ZooMRD(Ngrids) )
!      END IF
!      IF (.not.allocated(ZooMRN)) THEN
!         allocate ( ZooMRN(Ngrids) )
!      END IF
!
!  Allocate coag tracer vector.
!

#if defined DIAGNOSTICS && defined DIAGNOSTICS_COAG
!
!  Allocate biological diagnostics vectors
!
      IF (.not.allocated(iDcoag2)) THEN
        allocate ( iDcoag2(NDcoag2d) )
      END IF
      IF (.not.allocated(iDcoag3)) THEN
        allocate ( iDcoag3(NDcoag3d) )
      END IF
#endif

!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!
      ic=NAT+NPT+NCS+NNS+NBT
      DO i=1,NCT
         idcoag(i)=ic+i
      END DO

      p001_ind        = ic+1
      p002_ind        = ic+2
      p003_ind        = ic+3
      p004_ind        = ic+4
      p005_ind        = ic+5
      p006_ind        = ic+6
      p007_ind        = ic+7
      p008_ind        = ic+8
      p009_ind        = ic+9
      p010_ind        = ic+10
      p011_ind        = ic+11
      p012_ind        = ic+12
      p013_ind        = ic+13
      p014_ind        = ic+14
      p015_ind        = ic+15
      p016_ind        = ic+16
      p017_ind        = ic+17
      p018_ind        = ic+18
      p019_ind        = ic+19
      p020_ind        = ic+20
      p021_ind        = ic+21
      p022_ind        = ic+22
      p023_ind        = ic+23
      p024_ind        = ic+24
      p025_ind        = ic+25
      p026_ind        = ic+26
      p027_ind        = ic+27
      p028_ind        = ic+28
      p029_ind        = ic+29
      p030_ind        = ic+30
      p031_ind        = ic+31
      p032_ind        = ic+32
      p033_ind        = ic+33
      p034_ind        = ic+34
      p035_ind        = ic+35
      p036_ind        = ic+36
      p037_ind        = ic+37
      p038_ind        = ic+38
      p039_ind        = ic+39
      p040_ind        = ic+40
      p041_ind        = ic+41
      p042_ind        = ic+42
      p043_ind        = ic+43
      p044_ind        = ic+44
      p045_ind        = ic+45
      p046_ind        = ic+46
      p047_ind        = ic+47

      do m = 0, param_n_sections-1
        param_v_lower(m+1) = param_v0*2.d0**(m)
        param_v_upper(m+1) = param_v_lower(m+1)*2.d0
      end do

      param_av_vol     = 1.5d0 * param_v_lower
      param_dcomb      = (param_v_lower*6.d0/pi)**(1.d0/3.d0)
      param_dwidth     = (2.d0**(1.d0/3.d0)-1.d0)*param_dcomb

      amfrac           = (4.0d0/3.d0*pi)**(-1.d0/param_fr_dim) * param_a0**(1.d0-3.d0/param_fr_dim)
!      param_amfrac     = amfrac*sqrt(0.6)
      param_amfrac     = amfrac
      param_bmfrac     = 1.d0 / param_fr_dim


!
! set coag parameters depending on the state variables
!

      param_rho_fl     = 1.0275d0       ! Fluid density [g cm^-3]
      param_kvisc      = 0.01d0         ! Kinematic viscosity [cm^2 s^-1]

      param_temp       = 20.0 + 273.0
      param_alpha      = 0.5d0
      param_dz         = 8.167d0

      param_n_grw      = 5
      param_growth     = 1.0d1
      param_disagg     = 0.2d0
      param_remin      = 1.0d0
      param_n_rem      = 47

      param_dvisc      = param_kvisc * param_rho_fl
!      param_del_rho    = (4.5d0 * 2.48d0) * param_kvisc * param_rho_fl / param_g * (param_d0/2.d0)**(-0.83) ! original value (19.864 g/cm3, too heavy for our application)
      param_del_rho    = 1.e-3 ! [g/cm3]

      param_conBr      = 2.d0 / 3.d0 * param_k * param_temp / param_dvisc
      param_gamma      = 0.1d0
      param_setcon     = (2.d0/9.d0) * param_del_rho / param_rho_fl * param_g/param_kvisc


! values need to positive, negative sign is applied on calcremin
      param_f_rem( 1)=2.33348e-02
      param_f_rem( 2)=2.33348e-02
      param_f_rem( 3)=2.33348e-02
      param_f_rem( 4)=2.33348e-02
      param_f_rem( 5)=2.33348e-02
      param_f_rem( 6)=2.33348e-02
      param_f_rem( 7)=2.33348e-02
      param_f_rem( 8)=1.72082e-02
      param_f_rem( 9)=1.29987e-02
      param_f_rem(10)=9.94369e-03
      param_f_rem(11)=7.62651e-03
      param_f_rem(12)=5.80891e-03
      param_f_rem(13)=4.35262e-03
      param_f_rem(14)=3.17885e-03
      param_f_rem(15)=2.24360e-03
      param_f_rem(16)=1.51980e-03
      param_f_rem(17)=9.84000e-04
      param_f_rem(18)=6.08609e-04
      param_f_rem(19)=3.60865e-04
      param_f_rem(20)=2.06653e-04

      write(*,210) '= Coag parameters ====================================================='
      write(*,*)
      write(*,100) 'amfrac' ,':', param_amfrac
      write(*,100) 'bmfrac' ,':', param_bmfrac
      write(*,100) 'fr_dim' ,':', param_fr_dim
      write(*,100) 'rho_fl' ,':', param_rho_fl
      write(*,100) 'del_rho',':', param_del_rho
      write(*,100) 'kvisc'  ,':', param_kvisc
      write(*,100) 'dvisc'  ,':', param_dvisc
      write(*,100) 'temp '  ,':', param_temp
      write(*,100) 'alpha'  ,':', param_alpha
      write(*,100) 'gamma'  ,':', param_gamma
      write(*,100) 'setcon' ,':', param_setcon
      write(*,100) 'conBr'  ,':', param_conBr
      write(*,110) 'n_grw'  ,':', param_n_grw
      write(*,100) 'growth' ,':', param_growth
      write(*,100) 'disagg' ,':', param_disagg
      write(*,100) 'remin'  ,':', param_remin
      write(*,110) 'n_rem'  ,':', param_n_rem
      write(*,*)
      write(*,210) '-----------------------------------------------------------------------'
      write(*,200) 'v_lower','av_vol','dcomb','dwidth'
      write(*,210) '-----------------------------------------------------------------------'
      do m = 1, param_n_sections
        write(*,220) m, param_v_lower(m), param_av_vol(m), param_dcomb(m), param_dwidth(m)
      end do
      write(*,210) '-----------------------------------------------------------------------'
 100  format(X,A15,X,A,5X,1PE15.5)
 110  format(X,A15,X,A,5X,I15)
 200  format(X,5X,A15,2X,A15,2X,A15,2X,A15)
 210  format(X,A71)
 220  format(X,I3,2X,1PE15.5,2X,1PE15.5,2X,1PE15.5,2X,1PE15.5)

!
! calculate betas
!

      coag_id = 'BR'
      call calcbetas(b_brown)
      b_brown%b1  = b_brown%b1  * param_alpha * param_conBr * param_day_to_sec
      b_brown%b2  = b_brown%b2  * param_alpha * param_conBr * param_day_to_sec
      b_brown%b3  = b_brown%b3  * param_alpha * param_conBr * param_day_to_sec
      b_brown%b4  = b_brown%b4  * param_alpha * param_conBr * param_day_to_sec
      b_brown%b5  = b_brown%b5  * param_alpha * param_conBr * param_day_to_sec
      b_brown%b25 = b_brown%b25 * param_alpha * param_conBr * param_day_to_sec

      coag_id = 'SH'
      call calcbetas(b_shear)
      b_shear%b1  = b_shear%b1  * param_alpha * param_gamma * param_day_to_sec
      b_shear%b2  = b_shear%b2  * param_alpha * param_gamma * param_day_to_sec
      b_shear%b3  = b_shear%b3  * param_alpha * param_gamma * param_day_to_sec
      b_shear%b4  = b_shear%b4  * param_alpha * param_gamma * param_day_to_sec
      b_shear%b5  = b_shear%b5  * param_alpha * param_gamma * param_day_to_sec
      b_shear%b25 = b_shear%b25 * param_alpha * param_gamma * param_day_to_sec

      coag_id = 'DS'
      call calcbetas(b_ds)
      b_ds%b1  = b_ds%b1  * param_alpha * param_setcon * param_day_to_sec
      b_ds%b2  = b_ds%b2  * param_alpha * param_setcon * param_day_to_sec
      b_ds%b3  = b_ds%b3  * param_alpha * param_setcon * param_day_to_sec
      b_ds%b4  = b_ds%b4  * param_alpha * param_setcon * param_day_to_sec
      b_ds%b5  = b_ds%b5  * param_alpha * param_setcon * param_day_to_sec
      b_ds%b25 = b_ds%b25 * param_alpha * param_setcon * param_day_to_sec




      do krow = 1, param_n_layers
        do kcol = 1, param_n_layers
          b(krow,kcol)%b1 = 0.d0
          b(krow,kcol)%b2 = 0.d0
          b(krow,kcol)%b3 = 0.d0
          b(krow,kcol)%b4 = 0.d0
          b(krow,kcol)%b5 = 0.d0
          b(krow,kcol)%growth = 0.d0
          b(krow,kcol)%settle = 0.d0
          b(krow,kcol)%gain   = 0.d0
          b(krow,kcol)%disagg = 0.d0
          b(krow,kcol)%redist = 0.d0
        end do
      end do


      do krow = 1, param_n_layers
        do kcol = 1, param_n_layers

          if (krow .eq. kcol) then
            b(krow,kcol)%b1 = b_brown%b1 + b_shear%b1 + b_ds%b1
            b(krow,kcol)%b2 = b_brown%b2 + b_shear%b2 + b_ds%b2
            b(krow,kcol)%b3 = b_brown%b3 + b_shear%b3 + b_ds%b3
            b(krow,kcol)%b4 = b_brown%b4 + b_shear%b4 + b_ds%b4
            b(krow,kcol)%b5 = b_brown%b5 + b_shear%b5 + b_ds%b5
            b(krow,kcol)%b25 = b(krow,kcol)%b2 - b(krow,kcol)%b3 - b(krow,kcol)%b4 - b(krow,kcol)%b5
          end if

        end do
      end do

      do krow = 1, param_n_layers
        do kcol = 1, param_n_layers

          istr = 1                + param_n_sections * (krow-1)
          iend = param_n_sections + param_n_sections * (krow-1)
          jstr = 1                + param_n_sections * (kcol-1)
          jend = param_n_sections + param_n_sections * (kcol-1)

          ba%b1    (istr:iend,jstr:jend) = b(krow,kcol)%b1
          ba%b25   (istr:iend,jstr:jend) = b(krow,kcol)%b25

        end do
      end do

      RETURN
      END SUBROUTINE initialize_coag
