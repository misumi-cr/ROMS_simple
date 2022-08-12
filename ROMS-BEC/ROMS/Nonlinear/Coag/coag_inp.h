      SUBROUTINE read_CoagPar (model, inp, out, Lwrite)
!!
!!=======================================================================
!!                                                                      !
!!  This routine reads COAG model input parameters.                     !
!!                                                                      !
!!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_coag
      USE mod_ncparam
      USE mod_scalars

      implicit none
!
!  Imported variable declarations
!
      logical, intent(in) :: Lwrite
      integer, intent(in) :: model, inp, out
!
!  Local variable declarations.
!
      integer :: Npts, Nval
      integer :: iTrcStr, iTrcEnd
      integer :: i, ifield, igrid, itracer, itrc, ng, nline, status

      integer :: decode_line, load_i, load_l, load_lbc, load_r

      logical, dimension(Ngrids) :: Lbio
      logical, dimension(NBT,Ngrids) :: Ltrc

      real(r8), dimension(NBT,Ngrids) :: Rbio

      real(r8), dimension(100) :: Rval

      character (len=40 ) :: KeyWord
      character (len=256) :: line
      character (len=256), dimension(200) :: Cval

!-----------------------------------------------------------------------
!  Initialize.
!-----------------------------------------------------------------------
!
      igrid=1                            ! nested grid counter
      itracer=0                          ! LBC tracer counter
      iTrcStr=1   + NAT+NPT+NCS+NNS+NBT  ! first LBC tracer to process
      iTrcEnd=NCT + NAT+NPT+NCS+NNS+NBT  ! last  LBC tracer to process
      nline=0                            ! LBC multi-line counter

!-----------------------------------------------------------------------
!  Read in NPZD biological model (Powell et al., 2006) parameters.
!-----------------------------------------------------------------------
!
!#ifdef ANA_BIOLOGY
!      IF (.not.allocated(BioIni)) allocate ( BioIni(MT,Ngrids) )
!#endif
      DO WHILE (.TRUE.)
        READ (inp,'(a)',ERR=10,END=20) line
        status=decode_line(line, KeyWord, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          SELECT CASE (TRIM(KeyWord))
            CASE ('Lcoag')
              Npts=load_l(Nval, Cval, Ngrids, Lcoag)
!            CASE ('BioIter')
!              Npts=load_i(Nval, Rval, Ngrids, BioIter)
!#ifdef ANA_BIOLOGY
!            CASE ('BioIni(po4_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(po4_ind,1))
!            CASE ('BioIni(no3_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(no3_ind,1))
!            CASE ('BioIni(sio3_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(sio3_ind,1))
!            CASE ('BioIni(nh4_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(nh4_ind,1))
!            CASE ('BioIni(fe_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(fe_ind,1))
!            CASE ('BioIni(o2_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(o2_ind,1))
!            CASE ('BioIni(dic_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(dic_ind,1))
!            CASE ('BioIni(dic_alt_co2_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(dic_alt_co2_ind,1))
!            CASE ('BioIni(alk_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(alk_ind,1))
!            CASE ('BioIni(doc_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(doc_ind,1))
!            CASE ('BioIni(spC_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(spC_ind,1))
!            CASE ('BioIni(spChl_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(spChl_ind,1))
!            CASE ('BioIni(spCaCO3_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(spCaCO3_ind,1))
!            CASE ('BioIni(diatC_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(diatC_ind,1))
!            CASE ('BioIni(diatChl_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(diatChl_ind,1))
!            CASE ('BioIni(zooC_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(zooC_ind,1))
!            CASE ('BioIni(spFe_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(spFe_ind,1))
!            CASE ('BioIni(diatSi_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(diatSi_ind,1))
!            CASE ('BioIni(diatFe_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(diatFe_ind,1))
!            CASE ('BioIni(diazC_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(diazC_ind,1))
!            CASE ('BioIni(diazChl_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(diazChl_ind,1))
!            CASE ('BioIni(diazFe_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(diazFe_ind,1))
!            CASE ('BioIni(don_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(don_ind,1))
!            CASE ('BioIni(dofe_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(dofe_ind,1))
!            CASE ('BioIni(dop_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(dop_ind,1))
!            CASE ('BioIni(dopr_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(dopr_ind,1))
!            CASE ('BioIni(donr_ind)')
!              Npts=load_r(Nval, Rval, Ngrids, BioIni(donr_ind,1))
!#endif
!            CASE ('PARfrac')
!              Npts=load_r(Nval, Rval, Ngrids, PARfrac)
!            CASE ('TNU2')
!              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
!              DO ng=1,Ngrids
!                DO itrc=1,NBT
!                  i=idbio(itrc)
!                  nl_tnu2(i,ng)=Rbio(itrc,ng)
!                END DO
!              END DO
!            CASE ('TNU4')
!              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
!              DO ng=1,Ngrids
!                DO itrc=1,NBT
!                  i=idbio(itrc)
!                  nl_tnu4(i,ng)=Rbio(itrc,ng)
!                END DO
!              END DO
!            CASE ('ad_TNU2')
!              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
!              DO ng=1,Ngrids
!                DO itrc=1,NBT
!                  i=idbio(itrc)
!                  ad_tnu2(i,ng)=Rbio(itrc,ng)
!                  tl_tnu2(i,ng)=Rbio(itrc,ng)
!                END DO
!              END DO
!            CASE ('ad_TNU4')
!              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
!              DO ng=1,Ngrids
!                DO itrc=1,NBT
!                  i=idbio(itrc)
!                  ad_tnu4(i,ng)=Rbio(itrc,ng)
!                  ad_tnu4(i,ng)=Rbio(itrc,ng)
!                END DO
!              END DO
!            CASE ('AKT_BAK')
!              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
!              DO ng=1,Ngrids
!                DO itrc=1,NBT
!                  i=idbio(itrc)
!                  Akt_bak(i,ng)=Rbio(itrc,ng)
!                END DO
!              END DO
!            CASE ('ad_AKT_fac')
!              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
!              DO ng=1,Ngrids
!                DO itrc=1,NBT
!                  i=idbio(itrc)
!                  ad_Akt_fac(i,ng)=Rbio(itrc,ng)
!                  tl_Akt_fac(i,ng)=Rbio(itrc,ng)
!                END DO
!              END DO
!            CASE ('TNUDG')
!              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
!              DO ng=1,Ngrids
!                DO itrc=1,NBT
!                  i=idbio(itrc)
!                  Tnudg(i,ng)=Rbio(itrc,ng)
!                END DO
!              END DO
!            CASE ('LBC(isTvar)')
!              IF (itracer.lt.NBT) THEN
!                itracer=itracer+1
!              ELSE
!                itracer=1                      ! next nested grid
!              END IF
!              ifield=isTvar(idbio(itracer))
!              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
!     &                      iTrcStr, iTrcEnd, LBC)
!#if defined ADJOINT || defined TANGENT || defined TL_IOMS
!            CASE ('ad_LBC(isTvar)')
!              IF (itracer.lt.NBT) THEN
!                itracer=itracer+1
!              ELSE
!                itracer=1                      ! next nested grid
!              END IF
!              ifield=isTvar(idbio(itracer))
!              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
!     &                      iTrcStr, iTrcEnd, ad_LBC)
!#endif
!#ifdef TCLIMATOLOGY
!            CASE ('LtracerCLM')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO itrc=1,NBT
!                  i=idbio(itrc)
!                  LtracerCLM(i,ng)=Ltrc(itrc,ng)
!                END DO
!              END DO
!#endif
!#ifdef TS_PSOURCE
!            CASE ('LtracerSrc')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO itrc=1,NBT
!                  i=idbio(itrc)
!                  LtracerSrc(i,ng)=Ltrc(itrc,ng)
!                END DO
!              END DO
!#endif
!            CASE ('Hout(idTvar)')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO itrc=1,NBT
!                  i=idTvar(idbio(itrc))
!                  IF (i.eq.0) THEN
!                    IF (Master) WRITE (out,30)                          &
!     &                                'idTvar(idbio(', itrc, '))'
!                    exit_flag=5
!                    RETURN
!                  END IF
!                  Hout(i,ng)=Ltrc(itrc,ng)
!                END DO
!              END DO
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            CASE ('Aout(idTvar)')
              Npts=load_l(Nval, Cval, NCT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NCT
                  i=idTvar(idcoag(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
!#ifdef DIAGNOSTICS_TS
!            CASE ('Dout(iTrate)')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO i=1,NBT
!                  itrc=idbio(i)
!                  Dout(idDtrc(itrc,iTrate),ng)=Ltrc(i,ng)
!                END DO
!              END DO
!            CASE ('Dout(iThadv)')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO i=1,NBT
!                  itrc=idbio(i)
!                  Dout(idDtrc(itrc,iThadv),ng)=Ltrc(i,ng)
!                END DO
!              END DO
!            CASE ('Dout(iTxadv)')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO i=1,NBT
!                  itrc=idbio(i)
!                  Dout(idDtrc(itrc,iTxadv),ng)=Ltrc(i,ng)
!                END DO
!              END DO
!            CASE ('Dout(iTyadv)')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO i=1,NBT
!                  itrc=idbio(i)
!                  Dout(idDtrc(itrc,iTyadv),ng)=Ltrc(i,ng)
!                END DO
!              END DO
!            CASE ('Dout(iTvadv)')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO i=1,NBT
!                  itrc=idbio(i)
!                  Dout(idDtrc(itrc,iTvadv),ng)=Ltrc(i,ng)
!                END DO
!              END DO
!# if defined TS_DIF2 || defined TS_DIF4
!            CASE ('Dout(iThdif)')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO i=1,NBT
!                  itrc=idbio(i)
!                  Dout(idDtrc(itrc,iThdif),ng)=Ltrc(i,ng)
!                END DO
!              END DO
!            CASE ('Dout(iTxdif)')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO i=1,NBT
!                  itrc=idbio(i)
!                  Dout(idDtrc(itrc,iTxdif),ng)=Ltrc(i,ng)
!                END DO
!              END DO
!            CASE ('Dout(iTydif)')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO i=1,NBT
!                  itrc=idbio(i)
!                  Dout(idDtrc(itrc,iTydif),ng)=Ltrc(i,ng)
!                END DO
!              END DO
!#  if defined MIX_GEO_TS || defined MIX_ISO_TS
!            CASE ('Dout(iTsdif)')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO i=1,NBT
!                  itrc=idbio(i)
!                  Dout(idDtrc(itrc,iTsdif),ng)=Ltrc(i,ng)
!                END DO
!              END DO
!#  endif
!# endif
!            CASE ('Dout(iTvdif)')
!              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
!              DO ng=1,Ngrids
!                DO i=1,NBT
!                  itrc=idbio(i)
!                  Dout(idDtrc(itrc,iTvdif),ng)=Ltrc(i,ng)
!                END DO
!              END DO
!#endif
          END SELECT
        END IF
      END DO
  10  IF (Master) WRITE (out,50) line
      exit_flag=4
      RETURN
  20  CONTINUE
!!
!!-----------------------------------------------------------------------
!!  Report input parameters.
!!-----------------------------------------------------------------------
!!
      IF (Lwrite) THEN
        DO ng=1,Ngrids
          IF (Lcoag(ng)) THEN
            WRITE (out,60) ng
            write (out,*) '--- coag par write ---'
!            WRITE (out,70) BioIter(ng), 'BioIter',                      &
!     &            'Number of iterations for nonlinear convergence.'
!#ifdef ANA_BIOLOGY
!            WRITE (out,80) BioIni(po4_ind,ng), 'BioIni(po4_ind_)',           &
!     &            'Dissolved inorganic phosphate (mmol/m3).'
!            WRITE (out,80) BioIni(no3_ind,ng), 'BioIni(no3_ind)',           &
!     &            'Dissolved Inorganic Nitrate (mmol/m3).'
!            WRITE (out,80) BioIni(sio3_ind,ng), 'BioIni(sio3_ind)',           &
!     &            'Dissolved Inorganic Silicate (mmol/m3).'
!            WRITE (out,80) BioIni(nh4_ind,ng), 'BioIni(nh4_ind)',           &
!     &            'Dissolved Ammonia (mmol/m3).'
!            WRITE (out,80) BioIni(fe_ind,ng), 'BioIni(fe_ind)',           &
!     &            'Dissolved Inorganic Iron (mmol/m3).'
!            WRITE (out,80) BioIni(o2_ind,ng), 'BioIni(o2_ind)',           &
!     &            'Dissolved Oxygen (mmol/m3).'
!            WRITE (out,80) BioIni(dic_ind,ng), 'BioIni(dic_ind)',           &
!     &            'Dissolved Inorganic Carbon (mmol/m3).'
!            WRITE (out,80) BioIni(dic_alt_co2_ind,ng), 'BioIni(dic_alt_co2_ind)',           &
!     &            'DIC_ALT_CO2 (mmol/m3).'
!            WRITE (out,80) BioIni(alk_ind,ng), 'BioIni(alk_ind)',           &
!     &            'Alkalinity (meq/m3).'
!            WRITE (out,80) BioIni(doc_ind,ng), 'BioIni(doc_ind)',           &
!     &            'Dissolved Organic Carbon (mmol/m3).'
!            WRITE (out,80) BioIni(spC_ind,ng), 'BioIni(spC_ind)',           &
!     &            'Small Phytoplankton Carbon (mmol/m3).'
!            WRITE (out,80) BioIni(spChl_ind,ng), 'BioIni(spChl_ind)',           &
!     &            'Small phytoplankton Chlorophyll (mgl/m3).'
!            WRITE (out,80) BioIni(spCaCO3_ind,ng),'BioIni(spCaCO3_ind)',           &
!     &            'Small Phytoplankton CaCO3 (mmol/m3).'
!            WRITE (out,80) BioIni(diatC_ind,ng), 'BioIni(diatC_ind)',           &
!     &            'Diatom Carbon (mmol/m3).'
!            WRITE (out,80) BioIni(diatChl_ind,ng),'BioIni(diatChl_ind)',           &
!     &            'Diatom Chlorophyll (mg/m3).'
!            WRITE (out,80) BioIni(zooC_ind,ng), 'BioIni(zooC_ind)',           &
!     &            'Zooplankton Carbon (mmol/m3).'
!            WRITE (out,80) BioIni(spFe_ind,ng), 'BioIni(spFe_ind)',           &
!     &            'Small Phytoplankton Iron (mmol/m3).'
!            WRITE (out,80) BioIni(diatSi_ind,ng), 'BioIni(diatSi_ind)',           &
!     &            'Diatom Silicon (mmol/m3).'
!            WRITE (out,80) BioIni(diatFe_ind,ng), 'BioIni(diatFe_ind)',           &
!     &            'Diatom Iron (mmol/m3).'
!            WRITE (out,80) BioIni(diazC_ind,ng), 'BioIni(diazC_ind)',           &
!     &            'Diazotroph Carbon (mmol/m3).'
!            WRITE (out,80) BioIni(diazChl_ind,ng),'BioIni(diazChl_ind)',           &
!     &            'Diazotroph Chlorophy (mg/m3).'
!            WRITE (out,80) BioIni(diazFe_ind,ng), 'BioIni(diazFe_ind)',           &
!     &            'Diazotroph Iron (mmol/m3).'
!            WRITE (out,80) BioIni(don_ind,ng), 'BioIni(don_ind)',           &
!     &            'Dissolved Organic Nitrogen (mmol/m3).'
!            WRITE (out,80) BioIni(dofe_ind,ng), 'BioIni(dofe_ind)',           &
!     &            'Dissolved Organic Iron (mmol/m3).'
!            WRITE (out,80) BioIni(dop_ind,ng), 'BioIni(dop_ind)',           &
!     &            'Dissolved Organic Phosphorus (mmol/m3).'
!            WRITE (out,80) BioIni(dopr_ind,ng), 'BioIni(dopr_ind)',           &
!     &            'Refractory Dissolved Organic Phosphorus (mmol/m3).'
!            WRITE (out,80) BioIni(donr_ind,ng), 'BioIni(donr_ind)',           &
!     &            'Refractory Dissolved Organic Nitrogen (mmol/m3).'
!#endif
!            WRITE (out,90) PARfrac(ng), 'PARfrac',                      &
!     &            'Fraction of shortwave radiation that is',            &
!     &            'photosynthetically active (nondimensional).'
!#ifdef TS_DIF2
!            DO itrc=1,NBT
!              i=idbio(itrc)
!              WRITE (out,100) nl_tnu2(i,ng), 'nl_tnu2', i,               &
!     &              'NLM Horizontal, harmonic mixing coefficient',      &
!     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
!# ifdef ADJOINT
!              WRITE (out,100) ad_tnu2(i,ng), 'ad_tnu2', i,               &
!     &              'ADM Horizontal, harmonic mixing coefficient',      &
!     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
!# endif
!# if defined TANGENT || defined TL_IOMS
!              WRITE (out,100) tl_tnu2(i,ng), 'tl_tnu2', i,               &
!     &              'TLM Horizontal, harmonic mixing coefficient',      &
!     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
!# endif
!            END DO
!#endif
!#ifdef TS_DIF4
!            DO itrc=1,NBT
!              i=idbio(itrc)
!              WRITE (out,100) nl_tnu4(i,ng), 'nl_tnu4', i,               &
!     &              'NLM Horizontal, biharmonic mixing coefficient',    &
!     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
!# ifdef ADJOINT
!              WRITE (out,100) ad_tnu4(i,ng), 'ad_tnu4', i,               &
!     &              'ADM Horizontal, biharmonic mixing coefficient',    &
!     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
!# endif
!# if defined TANGENT || defined TL_IOMS
!              WRITE (out,100) tl_tnu4(i,ng), 'tl_tnu4', i,               &
!     &              'TLM Horizontal, biharmonic mixing coefficient',    &
!     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
!# endif
!            END DO
!#endif
!            DO itrc=1,NBT
!              i=idbio(itrc)
!              WRITE(out,100) Akt_bak(i,ng), 'Akt_bak', i,                &
!     &             'Background vertical mixing coefficient (m2/s)',     &
!     &             'for tracer ', i, TRIM(Vname(1,idTvar(i)))
!            END DO
!#ifdef FORWARD_MIXING
!            DO itrc=1,NBT
!              i=idbio(itrc)
!# ifdef ADJOINT
!              WRITE (out,100) ad_Akt_fac(i,ng), 'ad_Akt_fac', i,         &
!     &              'ADM basic state vertical mixing scale factor',     &
!     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
!# endif
!# if defined TANGENT || defined TL_IOMS
!              WRITE (out,100) tl_Akt_fac(i,ng), 'tl_Akt_fac', i,         &
!     &              'TLM basic state vertical mixing scale factor',     &
!     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
!# endif
!            END DO
!#endif
!            DO itrc=1,NBT
!              i=idbio(itrc)
!              WRITE (out,100) Tnudg(i,ng), 'Tnudg', i,                   &
!     &              'Nudging/relaxation time scale (days)',             &
!     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
!            END DO
!#ifdef TCLIMATOLOGY
!            DO itrc=1,NBT
!              i=idbio(itrc)
!              WRITE (out,110) LtracerCLM(i,ng), 'LtracerCLM',           &
!     &              i, 'Processing climatology on tracer ', i,          &
!     &              TRIM(Vname(1,idTvar(i)))
!            END DO
!#endif
!#ifdef TS_PSOURCE
!            DO itrc=1,NBT
!              i=idbio(itrc)
!              WRITE (out,110) LtracerSrc(i,ng), 'LtracerSrc',           &
!     &              i, 'Processing point sources/Sink on tracer ', i,   &
!     &              TRIM(Vname(1,idTvar(i)))
!            END DO
!#endif
!            DO itrc=1,NBT
!              i=idbio(itrc)
!              IF (Hout(idTvar(i),ng)) WRITE (out,120)                   &
!     &            Hout(idTvar(i),ng), 'Hout(idTvar)',                   &
!     &            'Write out tracer ', i, TRIM(Vname(1,idTvar(i)))
!            END DO
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            WRITE (out,'(1x)')
            DO itrc=1,NCT
              i=idcoag(itrc)
              IF (Aout(idTvar(i),ng)) WRITE (out,120)                   &
     &            Aout(idTvar(i),ng), 'Aout(idTvar)',                   &
     &            'Write out averaged tracer ', i,                      &
     &            TRIM(Vname(1,idTvar(i)))
            END DO
#endif
!#ifdef DIAGNOSTICS_TS
!            WRITE (out,'(1x)')
!            DO i=1,NBT
!              itrc=idbio(i)
!              IF (Dout(idDtrc(itrc,iTrate),ng))                         &
!     &          WRITE (out,120) .TRUE., 'Dout(iTrate)',                 &
!     &            'Write out rate of change of tracer ', itrc,          &
!     &            TRIM(Vname(1,idTvar(itrc)))
!            END DO
!            DO i=1,NBT
!              itrc=idbio(i)
!              IF (Dout(idDtrc(itrc,iThadv),ng))                         &
!     &          WRITE (out,120) .TRUE., 'Dout(iThadv)',                 &
!     &            'Write out horizontal advection, tracer ', itrc,      &
!     &            TRIM(Vname(1,idTvar(itrc)))
!            END DO
!            DO i=1,NBT
!              itrc=idbio(i)
!              IF (Dout(idDtrc(itrc,iTxadv),ng))                         &
!     &          WRITE (out,120) .TRUE., 'Dout(iTxadv)',                 &
!     &            'Write out horizontal X-advection, tracer ', itrc,    &
!     &            TRIM(Vname(1,idTvar(itrc)))
!            END DO
!            DO i=1,NBT
!              itrc=idbio(i)
!              IF (Dout(idDtrc(itrc,iTyadv),ng))                         &
!     &          WRITE (out,120) .TRUE., 'Dout(iTyadv)',                 &
!     &            'Write out horizontal Y-advection, tracer ', itrc,    &
!     &            TRIM(Vname(1,idTvar(itrc)))
!            END DO
!            DO i=1,NBT
!              itrc=idbio(i)
!              IF (Dout(idDtrc(itrc,iTvadv),ng))                         &
!     &          WRITE (out,120) .TRUE., 'Dout(iTvadv)',                 &
!     &            'Write out vertical advection, tracer ', itrc,        &
!     &            TRIM(Vname(1,idTvar(itrc)))
!            END DO
!# if defined TS_DIF2 || defined TS_DIF4
!            DO i=1,NBT
!              itrc=idbio(i)
!              IF (Dout(idDtrc(itrc,iThdif),ng))                         &
!     &          WRITE (out,120) .TRUE., 'Dout(iThdif)',                 &
!     &            'Write out horizontal diffusion, tracer ', itrc,      &
!     &            TRIM(Vname(1,idTvar(itrc)))
!            END DO
!            DO i=1,NBT
!              itrc=idbio(i)
!              IF (Dout(idDtrc(i,iTxdif),ng))                            &
!     &          WRITE (out,120) .TRUE., 'Dout(iTxdif)',                 &
!     &            'Write out horizontal X-diffusion, tracer ', itrc,    &
!     &            TRIM(Vname(1,idTvar(itrc)))
!            END DO
!            DO i=1,NBT
!              itrc=idbio(i)
!              IF (Dout(idDtrc(itrc,iTydif),ng))                         &
!     &          WRITE (out,120) .TRUE., 'Dout(iTydif)',                 &
!     &            'Write out horizontal Y-diffusion, tracer ', itrc,    &
!     &            TRIM(Vname(1,idTvar(itrc)))
!            END DO
!#  if defined MIX_GEO_TS || defined MIX_ISO_TS
!            DO i=1,NBT
!              itrc=idbio(i)
!              IF (Dout(idDtrc(itrc,iTsdif),ng))                         &
!     &          WRITE (out,120) .TRUE., 'Dout(iTsdif)',                 &
!     &            'Write out horizontal S-diffusion, tracer ', itrc,    &
!     &            TRIM(Vname(1,idTvar(itrc)))
!            END DO
!#  endif
!# endif
!            DO i=1,NBT
!              itrc=idbio(i)
!              IF (Dout(idDtrc(itrc,iTvdif),ng))                         &
!     &          WRITE (out,120) .TRUE., 'Dout(iTvdif)',                 &
!     &            'Write out vertical diffusion, tracer ', itrc,        &
!     &            TRIM(Vname(1,idTvar(itrc)))
!            END DO
!#endif
!#ifdef DIAGNOSTICS_BIO
!            IF (NDbio2d.gt.0) THEN
!              DO itrc=1,NDbio2d
!                i=iDbio2(itrc)
!                IF (Dout(i,ng)) WRITE (out,130)                         &
!     &              Dout(i,ng), 'Hout(iDbio2)',                         &
!     &              'Write out diagnostics for', TRIM(Vname(1,i))
!              END DO
!            END IF
!            DO itrc=1,NDbio3d
!              i=iDbio3(itrc)
!              IF (Dout(i,ng)) WRITE (out,130)                           &
!     &            Dout(i,ng), 'Hout(iDbio3)',                           &
!     &            'Write out diagnostics for', TRIM(Vname(1,i))
!            END DO
!#endif
          END IF
        END DO
      END IF
!!
!!-----------------------------------------------------------------------
!!  Rescale biological tracer parameters.
!!-----------------------------------------------------------------------
!!
!!  Take the square root of the biharmonic coefficients so it can
!!  be applied to each harmonic operator.
!!
!      DO ng=1,Ngrids
!        DO itrc=1,NBT
!          i=idbio(itrc)
!          nl_tnu4(i,ng)=SQRT(ABS(nl_tnu4(i,ng)))
!#ifdef ADJOINT
!          ad_tnu4(i,ng)=SQRT(ABS(ad_tnu4(i,ng)))
!#endif
!#if defined TANGENT || defined TL_IOMS
!          tl_tnu4(i,ng)=SQRT(ABS(tl_tnu4(i,ng)))
!#endif
!!
!!  Compute inverse nudging coefficients (1/s) used in various tasks.
!!
!          IF (Tnudg(i,ng).gt.0.0_r8) THEN
!            Tnudg(i,ng)=1.0_r8/(Tnudg(i,ng)*86400.0_r8)
!          ELSE
!            Tnudg(i,ng)=0.0_r8
!          END IF
!        END DO
!      END DO
!
  30  FORMAT (/,' read_BioPar - variable info not yet loaded, ',        &
     &        a,i2.2,a)
  40  FORMAT (/,' read_BioPar - variable info not yet loaded, ',a)
  50  FORMAT (/,' read_BioPar - Error while processing line: ',/,a)
  60  FORMAT (/,/,' Coag Model Parameters, Grid: ',i2.2,              &
     &        /,  ' =================================',/)
  70  FORMAT (1x,i10,2x,a,t30,a)
  80  FORMAT (1p,e11.4,2x,a,t30,a)
  90  FORMAT (1p,e11.4,2x,a,t30,a,/,t32,a)
 100  FORMAT (1p,e11.4,2x,a,'(',i2.2,')',t30,a,/,t32,a,i2.2,':',1x,a)
 110  FORMAT (10x,l1,2x,a,'(',i2.2,')',t30,a,i2.2,':',1x,a)
 120  FORMAT (10x,l1,2x,a,t30,a,i2.2,':',1x,a)
 130  FORMAT (10x,l1,2x,a,t30,a,1x,a)

      RETURN
      END SUBROUTINE read_CoagPar
