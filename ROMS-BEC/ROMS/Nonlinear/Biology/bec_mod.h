!
!============================================================== @DCC ===
!  BEC model tracer identification indices.
!=======================================================================
!-----------------------------------------------------------------------
!  relative tracer indices
!-----------------------------------------------------------------------
!
!   integer (int_kind), parameter :: &
!      po4_ind          =  1,  & ! dissolved inorganic phosphate
!      no3_ind          =  2,  & ! dissolved inorganic nitrate
!      sio3_ind         =  3,  & ! dissolved inorganic silicate
!      nh4_ind          =  4,  & ! dissolved ammonia
!      fe_ind           =  5,  & ! dissolved inorganic iron
!      o2_ind           =  6,  & ! dissolved oxygen
!      dic_ind          =  7,  & ! dissolved inorganic carbon
!      alk_ind          =  8,  & ! alkalinity
!      doc_ind          =  9,  & ! dissolved organic carbon
!      spC_ind          = 10,  & ! small phytoplankton carbon
!      spChl_ind        = 11,  & ! small phytoplankton chlorophyll
!      spCaCO3_ind      = 12,  & ! small phytoplankton caco3
!      diatC_ind        = 13,  & ! diatom carbon
!      diatChl_ind      = 14,  & ! diatom chlorophyll
!      zooC_ind         = 15,  & ! zooplankton carbon
!      spFe_ind         = 16,  & ! small phytoplankton iron
!      diatSi_ind       = 17,  & ! diatom silicon
!      diatFe_ind       = 18,  & ! diatom iron
!      diazC_ind        = 19,  & ! diazotroph carbon
!      diazChl_ind      = 20,  & ! diazotroph Chlorophyll
!      diazFe_ind       = 21,  & ! diazotroph iron
!      don_ind          = 22,  & ! dissolved organic nitrogen
!      dofe_ind         = 23,  & ! dissolved organic iron
!      dop_ind          = 24     ! dissolved organic phosphorus
!
!-----------------------------------------------------------------------
!  derived type & parameter for tracer index lookup
!-----------------------------------------------------------------------
!      ind_name_table = (/ &
!      ind_name_pair(po4_ind,         'PO4'), &
!      ind_name_pair(no3_ind,         'NO3'), &
!      ind_name_pair(sio3_ind,        'SiO3'), &
!      ind_name_pair(nh4_ind,         'NH4'), &
!      ind_name_pair(fe_ind,          'Fe'), &
!      ind_name_pair(o2_ind,          'O2'), &
!      ind_name_pair(dic_ind,         'DIC'), &
!      ind_name_pair(alk_ind,         'ALK'), &
!      ind_name_pair(doc_ind,         'DOC'), &
!      ind_name_pair(spC_ind,         'spC'), &
!      ind_name_pair(spChl_ind,       'spChl'), &
!      ind_name_pair(spCaCO3_ind,     'spCaCO3'), &
!      ind_name_pair(diatC_ind,       'diatC'), &
!      ind_name_pair(diatChl_ind,     'diatChl'), &
!      ind_name_pair(zooC_ind,        'zooC'), &
!      ind_name_pair(spFe_ind,        'spFe'), &
!      ind_name_pair(diatSi_ind,      'diatSi'), &
!      ind_name_pair(diatFe_ind,      'diatFe'), &
!      ind_name_pair(diazC_ind,       'diazC'), &
!      ind_name_pair(diazChl_ind,     'diazChl'), &
!      ind_name_pair(diazFe_ind,      'diazFe'), &
!      ind_name_pair(don_ind,         'DON'), &
!      ind_name_pair(dofe_ind,        'DOFe'), &
!      ind_name_pair(dop_ind,         'DOP') /)
!
!-----------------------------------------------------------------------

      USE mod_param
!
      implicit none
!
!  Set biological tracer identification indices.
!
      integer, allocatable :: idbio(:)  ! Biological tracers
      integer :: po4_ind                ! 1.  dissolved inorganic phosphate
      integer :: no3_ind                ! 2.  dissolved inorganic nitrate
      integer :: sio3_ind               ! 3.  dissolved inorganic silicate
      integer :: nh4_ind                ! 4.  dissolved ammonia
      integer :: o2_ind                 ! 5.  dissolved oxygen
      integer :: dic_ind                ! 6.  dissolved inorganic carbon
      integer :: dic_alt_co2_ind        ! 7.  dissolved inorganic carbon with alternative CO2
      integer :: alk_ind                ! 8.  alkalinity
      integer :: doc_ind                ! 9.  dissolved organic carbon
      integer :: don_ind                ! 10. dissolved organic nitrogen
      integer :: dop_ind                ! 11. dissolved organic phosphorus
      integer :: dopr_ind               ! 12. refractory DOP
      integer :: donr_ind               ! 13. refractory DON
      integer :: zooC_ind               ! 14.
      integer :: spChl_ind              ! 15.
      integer :: spC_ind                ! 16.
      integer :: spCaCO3_ind            ! 17.
      integer :: diatChl_ind            ! 18.
      integer :: diatC_ind              ! 19.
      integer :: diatSi_ind             ! 20.
      integer :: diazChl_ind            ! 21.
      integer :: diazC_ind              ! 22.

#ifndef FE_TAG
      integer :: fe_ind                 ! 23. dfe init
      integer :: dofe_ind               ! 24. dofe init
      integer :: spFe_ind               ! 25. spfe init
      integer :: diatFe_ind             ! 26. diatfe init
      integer :: diazFe_ind             ! 27. diazfe init
#else
      integer :: fe0_ind                ! 23. dfe init
      integer :: dofe0_ind              ! 24. dofe init
      integer :: spFe0_ind              ! 25. spfe init
      integer :: diatFe0_ind            ! 26. diatfe init
      integer :: diazFe0_ind            ! 27. diazfe init
      integer :: zooFe0_ind             ! 28. zooFe init

      integer :: fe1_ind                ! 29. dfe dust
      integer :: dofe1_ind              ! 30. dofe dust
      integer :: spFe1_ind              ! 31. spfe dust
      integer :: diatFe1_ind            ! 32. diatfe dust
      integer :: diazFe1_ind            ! 33. diazfe dust
      integer :: zooFe1_ind             ! 34. zooFe dust

      integer :: fe2_ind                ! 35. dfe sedi
      integer :: dofe2_ind              ! 36. dofe sedi
      integer :: spFe2_ind              ! 37. spfe sedi
      integer :: diatFe2_ind            ! 38. diatfe sedi
      integer :: diazFe2_ind            ! 39. diazfe sedi
      integer :: zooFe2_ind             ! 40. zooFe sedi

      integer :: fe3_ind                ! 41. dfe okh
      integer :: dofe3_ind              ! 42. dofe okh
      integer :: spFe3_ind              ! 43. spfe okh
      integer :: diatFe3_ind            ! 44. diatfe okh
      integer :: diazFe3_ind            ! 45. diazfe okh
      integer :: zooFe3_ind             ! 46. zooFe okh

      integer :: fe4_ind                ! 47. dfe bering
      integer :: dofe4_ind              ! 48. dofe bering
      integer :: spFe4_ind              ! 49. spfe bering
      integer :: diatFe4_ind            ! 50. diatfe bering
      integer :: diazFe4_ind            ! 51. diazfe bering
      integer :: zooFe4_ind             ! 52. zooFe okh

      integer :: fe5_ind                ! 53. dfe east china
      integer :: dofe5_ind              ! 54. dofe east china
      integer :: spFe5_ind              ! 55. spfe east china
      integer :: diatFe5_ind            ! 56. diatfe east china
      integer :: diazFe5_ind            ! 57. diazfe east china
      integer :: zooFe5_ind             ! 58. zooFe okh

      integer :: fe6_ind                ! 59. dfe japan sea
      integer :: dofe6_ind              ! 60. dofe japan sea
      integer :: spFe6_ind              ! 61. spfe japan sea
      integer :: diatFe6_ind            ! 62. diatfe japan sea
      integer :: diazFe6_ind            ! 63. diazfe japan sea
      integer :: zooFe6_ind             ! 64. zooFe okh

      integer :: fe7_ind                ! 65. dfe japan sea
      integer :: dofe7_ind              ! 66. dofe japan sea
      integer :: spFe7_ind              ! 67. spfe japan sea
      integer :: diatFe7_ind            ! 68. diatfe japan sea
      integer :: diazFe7_ind            ! 69. diazfe japan sea
      integer :: zooFe7_ind             ! 70. zooFe okh
#endif

#if defined CISO
      integer :: di13c_ind              ! 28.
      integer :: do13c_ind              ! 29.
      integer :: zoo13C_ind             ! 30.
      integer :: di14c_ind              ! 31.
      integer :: do14c_ind              ! 32.
      integer :: zoo14C_ind             ! 33.
      integer :: spC13_ind              ! 34.
      integer :: spC14_ind              ! 35.
      integer :: spCa13CO3_ind          ! 36.
      integer :: spCa14CO3_ind          ! 37.
      integer :: diatC13_ind            ! 38.
      integer :: diatC14_ind            ! 39.
      integer :: diazC13_ind            ! 40.
      integer :: diazC14_ind            ! 41.
#endif

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!  Biological 2D diagnostic variable IDs.
!
      integer, allocatable :: iDbio2(:)       ! 2D biological terms
!
!  Biological 3D diagnostic variable IDs.
!
      integer, allocatable :: iDbio3(:)       ! 3D biological terms

      integer  :: iphotoC_sp      = 1
      integer  :: iphotoC_diat    = 2
      integer  :: iphotoC_diaz    = 3

      integer  :: ilight_lim_sp   = 4
      integer  :: ilight_lim_diat = 5
      integer  :: ilight_lim_diaz = 6

      integer  :: iVNtot_sp       = 7
      integer  :: iVFe_sp         = 8
      integer  :: iVPtot_sp       = 9

      integer  :: iVNtot_diat     = 10
      integer  :: iVFe_diat       = 11
      integer  :: iVPtot_diat     = 12
      integer  :: iVSiO3_diat     = 13

      integer  :: iVNtot_diaz     = 14
      integer  :: iVFe_diaz       = 15
      integer  :: iVPtot_diaz     = 16

# if !defined FE_TAG
      integer  :: iFe_brate       = 17
      integer  :: iFe_scav        = 18
      integer  :: iFe_disag       = 19
      integer  :: iFe_pgen        = 20
      integer  :: iFe_premin      = 21
      integer  :: iFe_hbio        = 22
# else
      integer  :: iFe0_brate      = 17
      integer  :: iFe0_scav       = 18
      integer  :: iFe0_disag      = 19
      integer  :: iFe0_pgen       = 20
      integer  :: iFe0_premin     = 21
      integer  :: iFe0_hbio       = 22

      integer  :: iFe1_brate      = 23
      integer  :: iFe1_scav       = 24
      integer  :: iFe1_disag      = 25
      integer  :: iFe1_pgen       = 26
      integer  :: iFe1_premin     = 27
      integer  :: iFe1_hbio       = 28

      integer  :: iFe2_brate      = 29
      integer  :: iFe2_scav       = 30
      integer  :: iFe2_disag      = 31
      integer  :: iFe2_pgen       = 32
      integer  :: iFe2_premin     = 33
      integer  :: iFe2_hbio       = 34

      integer  :: iFe3_brate      = 35
      integer  :: iFe3_scav       = 36
      integer  :: iFe3_disag      = 37
      integer  :: iFe3_pgen       = 38
      integer  :: iFe3_premin     = 39
      integer  :: iFe3_hbio       = 40

      integer  :: iFe4_brate      = 41
      integer  :: iFe4_scav       = 42
      integer  :: iFe4_disag      = 43
      integer  :: iFe4_pgen       = 44
      integer  :: iFe4_premin     = 45
      integer  :: iFe4_hbio       = 46

      integer  :: iFe5_brate      = 47
      integer  :: iFe5_scav       = 48
      integer  :: iFe5_disag      = 49
      integer  :: iFe5_pgen       = 50
      integer  :: iFe5_premin     = 51
      integer  :: iFe5_hbio       = 52

      integer  :: iFe6_brate      = 53
      integer  :: iFe6_scav       = 54
      integer  :: iFe6_disag      = 55
      integer  :: iFe6_pgen       = 56
      integer  :: iFe6_premin     = 57
      integer  :: iFe6_hbio       = 58

      integer  :: iFe7_brate      = 59
      integer  :: iFe7_scav       = 60
      integer  :: iFe7_disag      = 61
      integer  :: iFe7_pgen       = 62
      integer  :: iFe7_premin     = 63
      integer  :: iFe7_hbio       = 64
# endif
# if defined CISO
      integer  :: iDIC_d13C       = 22
      integer  :: iDIC_d14C       = 23
# endif

      integer  :: ipCO2           = 1
      integer  :: ipH             = 2
      integer  :: iCO2_flux       = 3

#endif


!
!  Biological parameters.
!
      integer, allocatable :: BioIter(:)

#if defined ANA_BIOLOGY || defined ANA_CISO
      real(r8), allocatable :: BioIni(:,:)
#endif
      real(r8), allocatable :: AttPhy(:)       ! m2/mmole
      real(r8), allocatable :: AttSW(:)        ! 1/m
      real(r8), allocatable :: DetRR(:)        ! 1/day
      real(r8), allocatable :: K_NO3(:)        ! mmol/m3
      real(r8), allocatable :: Ivlev(:)        ! nondimensional
      real(r8), allocatable :: PARfrac(:)      ! nondimensional
#ifdef TANGENT
      real(r8), allocatable :: tl_PARfrac(:)
#endif
#ifdef ADJOINT
      real(r8), allocatable :: ad_PARfrac(:)
#endif
      real(r8), allocatable :: PhyIS(:)        ! m2/W
      real(r8), allocatable :: PhyMRD(:)       ! 1/day
      real(r8), allocatable :: PhyMRN(:)       ! 1/day
      real(r8), allocatable :: Vm_NO3(:)       ! 1/day
      real(r8), allocatable :: wDet(:)         ! m/day
#ifdef TANGENT
      real(r8), allocatable :: tl_wDet(:)
#endif
#ifdef ADJOINT
      real(r8), allocatable :: ad_wDet(:)
#endif
      real(r8), allocatable :: wPhy(:)         ! m/day
#ifdef TANGENT
      real(r8), allocatable :: tl_wPhy(:)
#endif
#ifdef ADJOINT
      real(r8), allocatable :: ad_wPhy(:)
#endif
      real(r8), allocatable :: ZooEED(:)       ! nondimensional
      real(r8), allocatable :: ZooEEN(:)       ! nondimensional
      real(r8), allocatable :: ZooGR(:)        ! 1/day
      real(r8), allocatable :: ZooMRD(:)       ! 1/day
      real(r8), allocatable :: ZooMRN(:)       ! 1/day

      CONTAINS


      SUBROUTINE initialize_biology
!
!  Local variable declarations
!
      integer :: i, ic

!-----------------------------------------------------------------------
!  Set number of biological tracers.
!-----------------------------------------------------------------------
!
#if   defined FE_TAG
     NBT=70
#elif defined CISO
     NBT=41
#else
     NBT=27
#endif

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
!  Set sources and sinks biology diagnostic parameters.
!-----------------------------------------------------------------------
!
!  Set number of diagnostics terms.
!
# if !defined FE_TAG
      NDbio3d=22
      NDbio2d=3
# else
      NDbio3d=64
      NDbio2d=3
# endif
# if defined CISO
      NDbio3d=23
      NDbio2d=3
# endif

#endif
!
!-----------------------------------------------------------------------
!  Allocate various module variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(BioIter)) THEN
         allocate ( BioIter(Ngrids) )
      END IF
      IF (.not.allocated(AttPhy)) THEN
         allocate ( AttPhy(Ngrids) )
      END IF
      IF (.not.allocated(AttSW)) THEN
         allocate ( AttSW(Ngrids) )
      END IF
      IF (.not.allocated(DetRR)) THEN
         allocate ( DetRR(Ngrids) )
      END IF
      IF (.not.allocated(K_NO3)) THEN
         allocate ( K_NO3(Ngrids) )
      END IF
      IF (.not.allocated(Ivlev)) THEN
         allocate ( Ivlev(Ngrids) )
      END IF
      IF (.not.allocated(PARfrac)) THEN
         allocate ( PARfrac(Ngrids) )
      END IF
#ifdef TANGENT
      IF (.not.allocated(tl_PARfrac)) THEN
         allocate ( tl_PARfrac(Ngrids) )
      END IF
#endif
#ifdef ADJOINT
      IF (.not.allocated(ad_PARfrac)) THEN
         allocate ( ad_PARfrac(Ngrids) )
      END IF
#endif
      IF (.not.allocated(PhyIS)) THEN
         allocate ( PhyIS(Ngrids) )
      END IF
      IF (.not.allocated(PhyMRD)) THEN
         allocate ( PhyMRD(Ngrids) )
      END IF
      IF (.not.allocated(PhyMRN)) THEN
         allocate ( PhyMRN(Ngrids) )
      END IF
      IF (.not.allocated(Vm_NO3)) THEN
         allocate ( Vm_NO3(Ngrids) )
      END IF
      IF (.not.allocated(wDet)) THEN
         allocate ( wDet(Ngrids) )
      END IF
#ifdef TANGENT
      IF (.not.allocated(tl_wDet)) THEN
         allocate ( tl_wDet(Ngrids) )
      END IF
#endif
#ifdef ADJOINT
      IF (.not.allocated(ad_wDet)) THEN
         allocate ( ad_wDet(Ngrids) )
      END IF
#endif
      IF (.not.allocated(wPhy)) THEN
         allocate ( wPhy(Ngrids) )
      END IF
#ifdef TANGENT
      IF (.not.allocated(tl_wPhy)) THEN
         allocate ( tl_wPhy(Ngrids) )
      END IF
#endif
#ifdef ADJOINT
      IF (.not.allocated(ad_wPhy)) THEN
         allocate ( ad_wPhy(Ngrids) )
      END IF
#endif
      IF (.not.allocated(ZooEED)) THEN
         allocate ( ZooEED(Ngrids) )
      END IF
      IF (.not.allocated(ZooEEN)) THEN
         allocate ( ZooEEN(Ngrids) )
      END IF
      IF (.not.allocated(ZooGR)) THEN
         allocate ( ZooGR(Ngrids) )
      END IF
      IF (.not.allocated(ZooMRD)) THEN
         allocate ( ZooMRD(Ngrids) )
      END IF
      IF (.not.allocated(ZooMRN)) THEN
         allocate ( ZooMRN(Ngrids) )
      END IF
!
!  Allocate biological tracer vector.
!
      IF (.not.allocated(idbio)) THEN
         allocate ( idbio(NBT) )
      END IF

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!  Allocate biological diagnostics vectors
!
      IF (.not.allocated(iDbio2)) THEN
        allocate ( iDbio2(NDbio2d) )
      END IF
      IF (.not.allocated(iDbio3)) THEN
        allocate ( iDbio3(NDbio3d) )
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!
      ic=NAT+NPT+NCS+NNS
      DO i=1,NBT
         idbio(i)=ic+i
      END DO

      po4_ind           = ic+1
      no3_ind           = ic+2
      sio3_ind          = ic+3
      nh4_ind           = ic+4
      o2_ind            = ic+5
      dic_ind           = ic+6
      dic_alt_co2_ind   = ic+7
      alk_ind           = ic+8
      doc_ind           = ic+9
      don_ind           = ic+10
      dop_ind           = ic+11
      dopr_ind          = ic+12
      donr_ind          = ic+13
      zooC_ind          = ic+14
      spChl_ind         = ic+15
      spC_ind           = ic+16
      spCaCO3_ind       = ic+17
      diatChl_ind       = ic+18
      diatC_ind         = ic+19
      diatSi_ind        = ic+20
      diazChl_ind       = ic+21
      diazC_ind         = ic+22

#ifndef FE_TAG
      fe_ind            = ic+23
      dofe_ind          = ic+24
      spFe_ind          = ic+25
      diatFe_ind        = ic+26
      diazFe_ind        = ic+27
#else
      fe0_ind           = ic+23
      dofe0_ind         = ic+24
      spFe0_ind         = ic+25
      diatFe0_ind       = ic+26
      diazFe0_ind       = ic+27
      zooFe0_ind        = ic+28

      fe1_ind           = ic+29
      dofe1_ind         = ic+30
      spFe1_ind         = ic+31
      diatFe1_ind       = ic+32
      diazFe1_ind       = ic+33
      zooFe1_ind        = ic+34

      fe2_ind           = ic+35
      dofe2_ind         = ic+36
      spFe2_ind         = ic+37
      diatFe2_ind       = ic+38
      diazFe2_ind       = ic+39
      zooFe2_ind        = ic+40

      fe3_ind           = ic+41
      dofe3_ind         = ic+42
      spFe3_ind         = ic+43
      diatFe3_ind       = ic+44
      diazFe3_ind       = ic+45
      zooFe3_ind        = ic+46

      fe4_ind           = ic+47
      dofe4_ind         = ic+48
      spFe4_ind         = ic+49
      diatFe4_ind       = ic+50
      diazFe4_ind       = ic+51
      zooFe4_ind        = ic+52

      fe5_ind           = ic+53
      dofe5_ind         = ic+54
      spFe5_ind         = ic+55
      diatFe5_ind       = ic+56
      diazFe5_ind       = ic+57
      zooFe5_ind        = ic+58

      fe6_ind           = ic+59
      dofe6_ind         = ic+60
      spFe6_ind         = ic+61
      diatFe6_ind       = ic+62
      diazFe6_ind       = ic+63
      zooFe6_ind        = ic+64

      fe7_ind           = ic+65
      dofe7_ind         = ic+66
      spFe7_ind         = ic+67
      diatFe7_ind       = ic+68
      diazFe7_ind       = ic+69
      zooFe7_ind        = ic+70
#endif

#if defined CISO
      di13c_ind       = ic+28
      do13c_ind       = ic+29
      zoo13C_ind      = ic+30
      di14c_ind       = ic+31
      do14c_ind       = ic+32
      zoo14C_ind      = ic+33
      spC13_ind       = ic+34
      spC14_ind       = ic+35
      spCa13CO3_ind   = ic+36
      spCa14CO3_ind   = ic+37
      diatC13_ind     = ic+38
      diatC14_ind     = ic+39
      diazC13_ind     = ic+40
      diazC14_ind     = ic+41
#endif

      RETURN
      END SUBROUTINE initialize_biology
