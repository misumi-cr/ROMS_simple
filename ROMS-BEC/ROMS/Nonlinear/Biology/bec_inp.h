      SUBROUTINE read_BioPar (model, inp, out, Lwrite)
!
!=======================================================================
!                                                                      !
!  This routine reads BEC model input parameters.                      !
!  They are specified in input script "ocean_bec.in".                  !
!                                                                 @DCC !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_ncparam
      USE mod_scalars
#if defined(LIGAND3D) || defined(DICE) || defined(DUST)
      USE mod_iounits
#endif
!
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

      integer, allocatable :: Nfiles(:)

      logical, dimension(Ngrids) :: Lbio
      logical, dimension(NBT,Ngrids) :: Ltrc

      real(r8), dimension(NBT,Ngrids) :: Rbio

      real(r8), dimension(100) :: Rval

      integer :: load_s1d
      character (len=40 ) :: KeyWord
      character (len=50)  :: label
      character (len=256) :: line
      character (len=256), dimension(200) :: Cval
!
!-----------------------------------------------------------------------
!  Initialize.
!-----------------------------------------------------------------------
!
      igrid=1                            ! nested grid counter
      itracer=0                          ! LBC tracer counter
      iTrcStr=1   + NAT+NPT+NCS+NNS      ! first LBC tracer to process
      iTrcEnd=NBT + NAT+NPT+NCS+NNS      ! last  LBC tracer to process
      nline=0                            ! LBC multi-line counter
!
!-----------------------------------------------------------------------
!  Read in BEC model parameters & analytical vales.
!-----------------------------------------------------------------------
!
#if defined  ANA_BIOLOGY || defined ANA_CISO
      IF (.not.allocated(BioIni)) allocate ( BioIni(MT,Ngrids) )
#endif
      DO WHILE (.TRUE.)
        READ (inp,'(a)',ERR=10,END=20) line
        status=decode_line(line, KeyWord, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          IF (.not.allocated(Nfiles)) THEN
            allocate( Nfiles(Ngrids) )
            Nfiles(1:Ngrids) = 0
          END IF
          SELECT CASE (TRIM(KeyWord))
            CASE ('Lbiology')
              Npts=load_l(Nval, Cval, Ngrids, Lbiology)
            CASE ('BioIter')
              Npts=load_i(Nval, Rval, Ngrids, BioIter)
#ifdef ANA_BIOLOGY
            CASE ('BioIni(po4_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(po4_ind,1))
            CASE ('BioIni(no3_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(no3_ind,1))
            CASE ('BioIni(sio3_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(sio3_ind,1))
            CASE ('BioIni(nh4_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(nh4_ind,1))
            CASE ('BioIni(fe_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(fe_ind,1))
            CASE ('BioIni(o2_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(o2_ind,1))
            CASE ('BioIni(dic_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(dic_ind,1))
            CASE ('BioIni(dic_alt_co2_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(dic_alt_co2_ind,1))
            CASE ('BioIni(alk_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(alk_ind,1))
            CASE ('BioIni(doc_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(doc_ind,1))
            CASE ('BioIni(spC_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(spC_ind,1))
            CASE ('BioIni(spChl_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(spChl_ind,1))
            CASE ('BioIni(spCaCO3_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(spCaCO3_ind,1))
            CASE ('BioIni(diatC_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(diatC_ind,1))
            CASE ('BioIni(diatChl_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(diatChl_ind,1))
            CASE ('BioIni(zooC_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(zooC_ind,1))
            CASE ('BioIni(spFe_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(spFe_ind,1))
            CASE ('BioIni(diatSi_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(diatSi_ind,1))
            CASE ('BioIni(diatFe_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(diatFe_ind,1))
            CASE ('BioIni(diazC_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(diazC_ind,1))
            CASE ('BioIni(diazChl_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(diazChl_ind,1))
            CASE ('BioIni(diazFe_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(diazFe_ind,1))
            CASE ('BioIni(don_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(don_ind,1))
            CASE ('BioIni(dofe_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(dofe_ind,1))
            CASE ('BioIni(dop_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(dop_ind,1))
            CASE ('BioIni(dopr_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(dopr_ind,1))
            CASE ('BioIni(donr_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(donr_ind,1))
#endif
#ifdef ANA_CISO
            CASE ('BioIni(di13c_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(di13c_ind,1))
            CASE ('BioIni(do13c_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(do13c_ind,1))
            CASE ('BioIni(zoo13C_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(zoo13C_ind,1))
            CASE ('BioIni(di14c_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(di14c_ind,1))
            CASE ('BioIni(do14c_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(do14c_ind,1))
            CASE ('BioIni(zoo14C_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(zoo14C_ind,1))
            CASE ('BioIni(spC13_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(spC13_ind,1))
            CASE ('BioIni(spC14_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(spC14_ind,1))
            CASE ('BioIni(spCa13CO3_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(spCa13CO3_ind,1))
            CASE ('BioIni(spCa14CO3_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(spCa14CO3_ind,1))
            CASE ('BioIni(diatC13_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(diatC13_ind,1))
            CASE ('BioIni(diatC14_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(diatC14_ind,1))
            CASE ('BioIni(diazC13_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(diazC13_ind,1))
            CASE ('BioIni(diaz14C_ind)')
              Npts=load_r(Nval, Rval, Ngrids, BioIni(diazC14_ind,1))
#endif
            CASE ('PARfrac')
              Npts=load_r(Nval, Rval, Ngrids, PARfrac)
            CASE ('TNU2')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  nl_tnu2(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('TNU4')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  nl_tnu4(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_TNU2')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_tnu2(i,ng)=Rbio(itrc,ng)
                  tl_tnu2(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_TNU4')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_tnu4(i,ng)=Rbio(itrc,ng)
                  ad_tnu4(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('AKT_BAK')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  Akt_bak(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_AKT_fac')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_Akt_fac(i,ng)=Rbio(itrc,ng)
                  tl_Akt_fac(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('TNUDG')
              Npts=load_r(Nval, Rval, NBT*Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  Tnudg(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('LBC(isTvar)')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              ifield=isTvar(idbio(itracer))
              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
     &                      iTrcStr, iTrcEnd, LBC)
#if defined ADJOINT || defined TANGENT || defined TL_IOMS
            CASE ('ad_LBC(isTvar)')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              ifield=isTvar(idbio(itracer))
              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
     &                      iTrcStr, iTrcEnd, ad_LBC)
#endif
#ifdef TCLIMATOLOGY
            CASE ('LtracerCLM')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerCLM(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
#ifdef TINFLUX
            CASE ('LtracerFLX')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerFLX(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
#ifdef LIGAND3D
            CASE ('LIGNAME')
              label='LIG - ligand 3d fields'
              Npts=load_s1d(Nval, Cval, line, label, igrid, Nfiles, LIG)
#endif
#ifdef DICE
            CASE ('DICENAME')
              label='DIC - data ice fields'
              Npts=load_s1d(Nval, Cval, line, label, igrid, Nfiles, DIC)
#endif
#ifdef DUST
            CASE ('DUSTNAME')
              label='DST - dust data'
              Npts=load_s1d(Nval, Cval, line, label, igrid, Nfiles, DST)
#endif
#ifdef TS_PSOURCE
            CASE ('LtracerSrc')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerSrc(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
            CASE ('Hout(idTvar)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idTvar(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            CASE ('Aout(idTvar)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idTTav)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idUTav)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idUTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idVTav)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idVTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(iHUTav)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=iHUTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(iHVTav)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=iHVTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
#ifdef DIAGNOSTICS_TS
            CASE ('Dout(iTrate)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTrate),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iThadv)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iThadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTxadv)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTxadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTyadv)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTyadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTvadv)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTvadv),ng)=Ltrc(i,ng)
                END DO
              END DO
# if defined TS_DIF2 || defined TS_DIF4
            CASE ('Dout(iThdif)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iThdif),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTxdif)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTxdif),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTydif)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTydif),ng)=Ltrc(i,ng)
                END DO
              END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
            CASE ('Dout(iTsdif)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTsdif),ng)=Ltrc(i,ng)
                END DO
              END DO
#  endif
# endif
            CASE ('Dout(iTvdif)')
              Npts=load_l(Nval, Cval, NBT*Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTvdif),ng)=Ltrc(i,ng)
                END DO
              END DO
#endif
#ifdef DIAGNOSTICS_BIO
            CASE ('Dout(iphotoC_sp)')
              IF (iDbio3(iphotoC_sp).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iphotoC_sp)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iphotoC_sp)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iphotoC_diat)')
              IF (iDbio3(iphotoC_diat).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iphotoC_diat)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iphotoC_diat)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iphotoC_diaz)')
              IF (iDbio3(iphotoC_diaz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iphotoC_diaz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iphotoC_diaz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(ilight_lim_sp)')
              IF (iDbio3(ilight_lim_sp).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ilight_lim_sp)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ilight_lim_sp)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(ilight_lim_diat)')
              IF (iDbio3(ilight_lim_diat).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ilight_lim_diat)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ilight_lim_diat)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(ilight_lim_diaz)')
              IF (iDbio3(ilight_lim_diaz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(ilight_lim_diaz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(ilight_lim_diaz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iVNtot_sp)')
              IF (iDbio3(iVNtot_sp).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iVNtot_sp)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iVNtot_sp)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iVFe_sp)')
              IF (iDbio3(iVFe_sp).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iVFe_sp)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iVFe_sp)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iVPtot_sp)')
              IF (iDbio3(iVPtot_sp).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iVPtot_sp)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iVPtot_sp)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iVNtot_diat)')
              IF (iDbio3(iVNtot_diat).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iVNtot_diat)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iVNtot_diat)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iVFe_diat)')
              IF (iDbio3(iVFe_diat).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iVFe_diat)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iVFe_diat)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iVPtot_diat)')
              IF (iDbio3(iVPtot_diat).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iVPtot_diat)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iVPtot_diat)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iVSiO3_diat)')
              IF (iDbio3(iVSiO3_diat).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iVSiO3_diat)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iVSiO3_diat)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iVNtot_diaz)')
              IF (iDbio3(iVNtot_diaz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iVNtot_diaz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iVNtot_diaz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iVFe_diaz)')
              IF (iDbio3(iVFe_diaz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iVFe_diaz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iVFe_diaz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iVPtot_diaz)')
              IF (iDbio3(iVPtot_diaz).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iVPtot_diaz)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iVPtot_diaz)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

# ifndef FE_TAG
            CASE ('Dout(iFe_brate)')
              IF (iDbio3(iFe_brate).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe_brate)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe_brate)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe_scav)')
              IF (iDbio3(iFe_scav).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe_scav)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe_scav)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe_disag)')
              IF (iDbio3(iFe_disag).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe_disag)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe_disag)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe_pgen)')
              IF (iDbio3(iFe_pgen).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe_pgen)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe_pgen)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe_premin)')
              IF (iDbio3(iFe_premin).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe_premin)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe_premin)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe_hbio)')
              IF (iDbio3(iFe_hbio).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe_hbio)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe_hbio)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# else
            CASE ('Dout(iFe0_brate)')
              IF (iDbio3(iFe0_brate).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe0_brate)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe0_brate)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe0_scav)')
              IF (iDbio3(iFe0_scav).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe0_scav)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe0_scav)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe0_disag)')
              IF (iDbio3(iFe0_disag).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe0_disag)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe0_disag)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe0_pgen)')
              IF (iDbio3(iFe0_pgen).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe0_pgen)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe0_pgen)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe0_premin)')
              IF (iDbio3(iFe0_premin).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe0_premin)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe0_premin)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe0_hbio)')
              IF (iDbio3(iFe0_hbio).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe0_hbio)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe0_hbio)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe1_brate)')
              IF (iDbio3(iFe1_brate).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe1_brate)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe1_brate)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe1_scav)')
              IF (iDbio3(iFe1_scav).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe1_scav)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe1_scav)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe1_disag)')
              IF (iDbio3(iFe1_disag).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe1_disag)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe1_disag)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe1_pgen)')
              IF (iDbio3(iFe1_pgen).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe1_pgen)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe1_pgen)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe1_premin)')
              IF (iDbio3(iFe1_premin).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe1_premin)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe1_premin)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe1_hbio)')
              IF (iDbio3(iFe1_hbio).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe1_hbio)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe1_hbio)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe2_brate)')
              IF (iDbio3(iFe2_brate).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe2_brate)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe2_brate)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe2_scav)')
              IF (iDbio3(iFe2_scav).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe2_scav)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe2_scav)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe2_disag)')
              IF (iDbio3(iFe2_disag).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe2_disag)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe2_disag)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe2_pgen)')
              IF (iDbio3(iFe2_pgen).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe2_pgen)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe2_pgen)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe2_premin)')
              IF (iDbio3(iFe2_premin).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe2_premin)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe2_premin)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe2_hbio)')
              IF (iDbio3(iFe2_hbio).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe2_hbio)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe2_hbio)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe3_brate)')
              IF (iDbio3(iFe3_brate).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe3_brate)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe3_brate)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe3_scav)')
              IF (iDbio3(iFe3_scav).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe3_scav)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe3_scav)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe3_disag)')
              IF (iDbio3(iFe3_disag).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe3_disag)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe3_disag)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe3_pgen)')
              IF (iDbio3(iFe3_pgen).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe3_pgen)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe3_pgen)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe3_premin)')
              IF (iDbio3(iFe3_premin).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe3_premin)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe3_premin)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe3_hbio)')
              IF (iDbio3(iFe3_hbio).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe3_hbio)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe3_hbio)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe4_brate)')
              IF (iDbio3(iFe4_brate).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe4_brate)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe4_brate)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe4_scav)')
              IF (iDbio3(iFe4_scav).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe4_scav)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe4_scav)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe4_disag)')
              IF (iDbio3(iFe4_disag).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe4_disag)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe4_disag)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe4_pgen)')
              IF (iDbio3(iFe4_pgen).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe4_pgen)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe4_pgen)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe4_premin)')
              IF (iDbio3(iFe4_premin).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe4_premin)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe4_premin)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe4_hbio)')
              IF (iDbio3(iFe4_hbio).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe4_hbio)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe4_hbio)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe5_brate)')
              IF (iDbio3(iFe5_brate).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe5_brate)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe5_brate)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe5_scav)')
              IF (iDbio3(iFe5_scav).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe5_scav)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe5_scav)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe5_disag)')
              IF (iDbio3(iFe5_disag).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe5_disag)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe5_disag)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe5_pgen)')
              IF (iDbio3(iFe5_pgen).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe5_pgen)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe5_pgen)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe5_premin)')
              IF (iDbio3(iFe5_premin).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe5_premin)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe5_premin)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe5_hbio)')
              IF (iDbio3(iFe5_hbio).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe5_hbio)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe5_hbio)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe6_brate)')
              IF (iDbio3(iFe6_brate).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe6_brate)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe6_brate)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe6_scav)')
              IF (iDbio3(iFe6_scav).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe6_scav)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe6_scav)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe6_disag)')
              IF (iDbio3(iFe6_disag).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe6_disag)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe6_disag)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe6_pgen)')
              IF (iDbio3(iFe6_pgen).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe6_pgen)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe6_pgen)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe6_premin)')
              IF (iDbio3(iFe6_premin).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe6_premin)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe6_premin)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe6_hbio)')
              IF (iDbio3(iFe6_hbio).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe6_hbio)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe6_hbio)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe7_brate)')
              IF (iDbio3(iFe7_brate).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe7_brate)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe7_brate)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe7_scav)')
              IF (iDbio3(iFe7_scav).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe7_scav)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe7_scav)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe7_disag)')
              IF (iDbio3(iFe7_disag).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe7_disag)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe7_disag)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe7_pgen)')
              IF (iDbio3(iFe7_pgen).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe7_pgen)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe7_pgen)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe7_premin)')
              IF (iDbio3(iFe7_premin).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe7_premin)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe7_premin)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iFe7_hbio)')
              IF (iDbio3(iFe7_hbio).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iFe7_hbio)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iFe7_hbio)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# endif

            CASE ('Dout(ipCO2)')
              IF (iDbio2(ipCO2).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ipCO2)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ipCO2)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(ipH)')
              IF (iDbio2(ipH).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ipH)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ipH)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iCO2_flux)')
              IF (iDbio2(iCO2_flux).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iCO2_flux)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iCO2_flux)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

# if defined CISO
            CASE ('Dout(iDIC_d13C)')
              IF (iDbio3(iDIC_d13C).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iDIC_d13C)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iDIC_d13C)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO

            CASE ('Dout(iDIC_d14C)')
              IF (iDbio3(iDIC_d14C).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iDIC_d14C)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iDIC_d14C)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# endif
#endif
          END SELECT
        END IF
      END DO
  10  IF (Master) WRITE (out,50) line
      exit_flag=4
      RETURN
  20  CONTINUE
!
!-----------------------------------------------------------------------
!  Report input parameters.
!-----------------------------------------------------------------------
!
      IF (Lwrite) THEN
        DO ng=1,Ngrids
          IF (Lbiology(ng)) THEN
            WRITE (out,60) ng
            WRITE (out,70) BioIter(ng), 'BioIter',                      &
     &            'Number of iterations for nonlinear convergence.'
#ifdef ANA_BIOLOGY
            WRITE (out,80) BioIni(po4_ind,ng), 'BioIni(po4_ind)',           &
     &            'Dissolved inorganic phosphate (mmol/m3).'
            WRITE (out,80) BioIni(no3_ind,ng), 'BioIni(no3_ind)',           &
     &            'Dissolved Inorganic Nitrate (mmol/m3).'
            WRITE (out,80) BioIni(sio3_ind,ng), 'BioIni(sio3_ind)',           &
     &            'Dissolved Inorganic Silicate (mmol/m3).'
            WRITE (out,80) BioIni(nh4_ind,ng), 'BioIni(nh4_ind)',           &
     &            'Dissolved Ammonia (mmol/m3).'
            WRITE (out,80) BioIni(fe_ind,ng), 'BioIni(fe_ind)',           &
     &            'Dissolved Inorganic Iron (mmol/m3).'
            WRITE (out,80) BioIni(o2_ind,ng), 'BioIni(o2_ind)',           &
     &            'Dissolved Oxygen (mmol/m3).'
            WRITE (out,80) BioIni(dic_ind,ng), 'BioIni(dic_ind)',           &
     &            'Dissolved Inorganic Carbon (mmol/m3).'
            WRITE (out,80) BioIni(dic_alt_co2_ind,ng), 'BioIni(dic_alt_co2_ind)',           &
     &            'DIC_ALT_CO2 (mmol/m3).'
            WRITE (out,80) BioIni(alk_ind,ng), 'BioIni(alk_ind)',           &
     &            'Alkalinity (meq/m3).'
            WRITE (out,80) BioIni(doc_ind,ng), 'BioIni(doc_ind)',           &
     &            'Dissolved Organic Carbon (mmol/m3).'
            WRITE (out,80) BioIni(spC_ind,ng), 'BioIni(spC_ind)',           &
     &            'Small Phytoplankton Carbon (mmol/m3).'
            WRITE (out,80) BioIni(spChl_ind,ng), 'BioIni(spChl_ind)',           &
     &            'Small phytoplankton Chlorophyll (mgl/m3).'
            WRITE (out,80) BioIni(spCaCO3_ind,ng),'BioIni(spCaCO3_ind)',           &
     &            'Small Phytoplankton CaCO3 (mmol/m3).'
            WRITE (out,80) BioIni(diatC_ind,ng), 'BioIni(diatC_ind)',           &
     &            'Diatom Carbon (mmol/m3).'
            WRITE (out,80) BioIni(diatChl_ind,ng),'BioIni(diatChl_ind)',           &
     &            'Diatom Chlorophyll (mg/m3).'
            WRITE (out,80) BioIni(zooC_ind,ng), 'BioIni(zooC_ind)',           &
     &            'Zooplankton Carbon (mmol/m3).'
            WRITE (out,80) BioIni(spFe_ind,ng), 'BioIni(spFe_ind)',           &
     &            'Small Phytoplankton Iron (mmol/m3).'
            WRITE (out,80) BioIni(diatSi_ind,ng), 'BioIni(diatSi_ind)',           &
     &            'Diatom Silicon (mmol/m3).'
            WRITE (out,80) BioIni(diatFe_ind,ng), 'BioIni(diatFe_ind)',           &
     &            'Diatom Iron (mmol/m3).'
            WRITE (out,80) BioIni(diazC_ind,ng), 'BioIni(diazC_ind)',           &
     &            'Diazotroph Carbon (mmol/m3).'
            WRITE (out,80) BioIni(diazChl_ind,ng),'BioIni(diazChl_ind)',           &
     &            'Diazotroph Chlorophy (mg/m3).'
            WRITE (out,80) BioIni(diazFe_ind,ng), 'BioIni(diazFe_ind)',           &
     &            'Diazotroph Iron (mmol/m3).'
            WRITE (out,80) BioIni(don_ind,ng), 'BioIni(don_ind)',           &
     &            'Dissolved Organic Nitrogen (mmol/m3).'
            WRITE (out,80) BioIni(dofe_ind,ng), 'BioIni(dofe_ind)',           &
     &            'Dissolved Organic Iron (mmol/m3).'
            WRITE (out,80) BioIni(dop_ind,ng), 'BioIni(dop_ind)',           &
     &            'Dissolved Organic Phosphorus (mmol/m3).'
            WRITE (out,80) BioIni(dopr_ind,ng), 'BioIni(dopr_ind)',           &
     &            'Refractory Dissolved Organic Phosphorus (mmol/m3).'
            WRITE (out,80) BioIni(donr_ind,ng), 'BioIni(donr_ind)',           &
     &            'Refractory Dissolved Organic Nitrogen (mmol/m3).'
#endif
#ifdef ANA_CISO
            WRITE (out,80) BioIni(di13c_ind,ng), 'BioIni(di13c_ind)',           &
     &            'Dissolved Inorganic Carbon-13 (mmol/m3).'
            WRITE (out,80) BioIni(do13c_ind,ng), 'BioIni(do13c_ind)',           &
     &            'Dissolved Organic Carbon-13 (mmol/m3).'
            WRITE (out,80) BioIni(zoo13C_ind,ng), 'BioIni(zoo13C_ind)',           &
     &            'Zooplankton Carbon-13 (mmol/m3).'
            WRITE (out,80) BioIni(di14c_ind,ng), 'BioIni(di14c_ind)',           &
     &            'Dissolved Inorganic Carbon-14 (mmol/m3).'
            WRITE (out,80) BioIni(do14c_ind,ng), 'BioIni(do14c_ind)',           &
     &            'Dissolved Organic Carbon-14 (mmol/m3).'
            WRITE (out,80) BioIni(zoo14C_ind,ng), 'BioIni(zoo14C_ind)',           &
     &            'Zooplankton Carbon-14 (mmol/m3).'
            WRITE (out,80) BioIni(spC13_ind,ng), 'BioIni(spC13_ind)',           &
     &            'Small Phytoplankton Carbon-13 (mmol/m3).'
            WRITE (out,80) BioIni(spC14_ind,ng), 'BioIni(spC14_ind)',           &
     &            'Small Phytoplankton Carbon-14 (mmol/m3).'
            WRITE (out,80) BioIni(spCa13CO3_ind,ng), 'BioIni(spCa13CO3_ind)',           &
     &            'Small Phytoplankton Ca13CO3 (mmol/m3).'
            WRITE (out,80) BioIni(spCa14CO3_ind,ng), 'BioIni(spCa14CO3_ind)',           &
     &            'Small Phytoplankton Ca14CO3 (mmol/m3).'
            WRITE (out,80) BioIni(diatC13_ind,ng), 'BioIni(diatC13_ind)',           &
     &            'Diatom Carbon-13 (mmol/m3).'
            WRITE (out,80) BioIni(diatC14_ind,ng), 'BioIni(diatC14_ind)',           &
     &            'Diatom Carbon-14 (mmol/m3).'
            WRITE (out,80) BioIni(diazC13_ind,ng), 'BioIni(diazC13_ind)',           &
     &            'Diazotroph Carbon-13 (mmol/m3).'
            WRITE (out,80) BioIni(diazC14_ind,ng), 'BioIni(diaz14C_ind)',           &
     &            'Diazotroph Carbon-14 (mmol/m3).'
#endif
            WRITE (out,90) PARfrac(ng), 'PARfrac',                      &
     &            'Fraction of shortwave radiation that is',            &
     &            'photosynthetically active (nondimensional).'
#ifdef TS_DIF2
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) nl_tnu2(i,ng), 'nl_tnu2', i,               &
     &              'NLM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# ifdef ADJOINT
              WRITE (out,100) ad_tnu2(i,ng), 'ad_tnu2', i,               &
     &              'ADM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_tnu2(i,ng), 'tl_tnu2', i,               &
     &              'TLM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
#ifdef TS_DIF4
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) nl_tnu4(i,ng), 'nl_tnu4', i,               &
     &              'NLM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# ifdef ADJOINT
              WRITE (out,100) ad_tnu4(i,ng), 'ad_tnu4', i,               &
     &              'ADM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_tnu4(i,ng), 'tl_tnu4', i,               &
     &              'TLM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE(out,100) Akt_bak(i,ng), 'Akt_bak', i,                &
     &             'Background vertical mixing coefficient (m2/s)',     &
     &             'for tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
#ifdef FORWARD_MIXING
            DO itrc=1,NBT
              i=idbio(itrc)
# ifdef ADJOINT
              WRITE (out,100) ad_Akt_fac(i,ng), 'ad_Akt_fac', i,         &
     &              'ADM basic state vertical mixing scale factor',     &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_Akt_fac(i,ng), 'tl_Akt_fac', i,         &
     &              'TLM basic state vertical mixing scale factor',     &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) Tnudg(i,ng), 'Tnudg', i,                   &
     &              'Nudging/relaxation time scale (days)',             &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
#ifdef TCLIMATOLOGY
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,110) LtracerCLM(i,ng), 'LtracerCLM',           &
     &              i, 'Processing climatology on tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
            END DO
#endif
#ifdef TINFLUX
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,110) LtracerFLX(i,ng), 'LtracerFLX',           &
     &              i, 'Processing 3d influx on tracer ', i,            &
     &              TRIM(Vname(1,idTvar(i)))
            END DO
#endif
#ifdef LIGAND3D
              i=idTlig
              WRITE (out,110) .TRUE. , 'LtracerFLX',                    &
     &              i, 'Processing 3d ligand ', i,                      &
     &              TRIM(Vname(1,idTlig))
#endif
#ifdef DICE
              i=idDice
              WRITE (out,110) .TRUE. , 'DICE',                          &
     &              i, 'Processing data ice', i,                        &
     &              TRIM(Vname(1,idDice))
#endif
#ifdef DUST
              i=idDust
              WRITE (out,110) .TRUE. , 'DUST',                          &
     &              i, 'Processing dust data', i,                       &
     &              TRIM(Vname(1,idDust))
#endif
#ifdef TS_PSOURCE
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,110) LtracerSrc(i,ng), 'LtracerSrc',           &
     &              i, 'Processing point sources/Sink on tracer ', i,   &
     &              TRIM(Vname(1,idTvar(i)))
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Hout(idTvar(i),ng)) WRITE (out,120)                   &
     &            Hout(idTvar(i),ng), 'Hout(idTvar)',                   &
     &            'Write out tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            WRITE (out,'(1x)')
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Aout(idTvar(i),ng)) WRITE (out,120)                   &
     &            Aout(idTvar(i),ng), 'Aout(idTvar)',                   &
     &            'Write out averaged tracer ', i,                      &
     &            TRIM(Vname(1,idTvar(i)))
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Aout(idTTav(i),ng)) WRITE (out,120)                   &
     &            Aout(idTTav(i),ng), 'Aout(idTTav)',                   &
     &            'Write out averaged <t*t> for tracer ', i,            &
     &            TRIM(Vname(1,idTvar(i)))
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Aout(idUTav(i),ng)) WRITE (out,120)                   &
     &            Aout(idUTav(i),ng), 'Aout(idUTav)',                   &
     &            'Write out averaged <u*t> for tracer ', i,            &
     &            TRIM(Vname(1,idTvar(i)))
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Aout(idVTav(i),ng)) WRITE (out,120)                   &
     &            Aout(idVTav(i),ng), 'Aout(idVTav)',                   &
     &            'Write out averaged <v*t> for tracer ', i,            &
     &            TRIM(Vname(1,idTvar(i)))
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Aout(iHUTav(i),ng)) WRITE (out,120)                   &
     &            Aout(iHUTav(i),ng), 'Aout(iHUTav)',                   &
     &            'Write out averaged <Huon*t> for tracer ', i,         &
     &            TRIM(Vname(1,idTvar(i)))
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (Aout(iHVTav(i),ng)) WRITE (out,120)                  &
     &            Aout(iHVTav(i),ng), 'Aout(iHVTav)',                  &
     &            'Write out averaged <Hvom*t> for tracer ', i,        &
     &            TRIM(Vname(1,idTvar(i)))
            END DO
#endif
#ifdef DIAGNOSTICS_TS
            WRITE (out,'(1x)')
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTrate),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTrate)',                 &
     &            'Write out rate of change of tracer ', itrc,          &
     &            TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iThadv),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iThadv)',                 &
     &            'Write out horizontal advection, tracer ', itrc,      &
     &            TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTxadv),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTxadv)',                 &
     &            'Write out horizontal X-advection, tracer ', itrc,    &
     &            TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTyadv),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTyadv)',                 &
     &            'Write out horizontal Y-advection, tracer ', itrc,    &
     &            TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTvadv),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTvadv)',                 &
     &            'Write out vertical advection, tracer ', itrc,        &
     &            TRIM(Vname(1,idTvar(itrc)))
            END DO
# if defined TS_DIF2 || defined TS_DIF4
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iThdif),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iThdif)',                 &
     &            'Write out horizontal diffusion, tracer ', itrc,      &
     &            TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(i,iTxdif),ng))                            &
     &          WRITE (out,120) .TRUE., 'Dout(iTxdif)',                 &
     &            'Write out horizontal X-diffusion, tracer ', itrc,    &
     &            TRIM(Vname(1,idTvar(itrc)))
            END DO
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTydif),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTydif)',                 &
     &            'Write out horizontal Y-diffusion, tracer ', itrc,    &
     &            TRIM(Vname(1,idTvar(itrc)))
            END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTsdif),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTsdif)',                 &
     &            'Write out horizontal S-diffusion, tracer ', itrc,    &
     &            TRIM(Vname(1,idTvar(itrc)))
            END DO
#  endif
# endif
            DO i=1,NBT
              itrc=idbio(i)
              IF (Dout(idDtrc(itrc,iTvdif),ng))                         &
     &          WRITE (out,120) .TRUE., 'Dout(iTvdif)',                 &
     &            'Write out vertical diffusion, tracer ', itrc,        &
     &            TRIM(Vname(1,idTvar(itrc)))
            END DO
#endif
#ifdef DIAGNOSTICS_BIO
            IF (NDbio2d.gt.0) THEN
              DO itrc=1,NDbio2d
                i=iDbio2(itrc)
                IF (Dout(i,ng)) WRITE (out,130)                         &
     &              Dout(i,ng), 'Hout(iDbio2)',                         &
     &              'Write out diagnostics for', TRIM(Vname(1,i))
              END DO
            END IF
            IF (NDbio3d.gt.0) THEN
              DO itrc=1,NDbio3d
                i=iDbio3(itrc)
                IF (Dout(i,ng)) WRITE (out,130)                           &
     &            Dout(i,ng), 'Hout(iDbio3)',                           &
     &            'Write out diagnostics for', TRIM(Vname(1,i))
              END DO
            END IF
#endif
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Rescale biological tracer parameters.
!-----------------------------------------------------------------------
!
!  Take the square root of the biharmonic coefficients so it can
!  be applied to each harmonic operator.
!
      DO ng=1,Ngrids
        DO itrc=1,NBT
          i=idbio(itrc)
          nl_tnu4(i,ng)=SQRT(ABS(nl_tnu4(i,ng)))
#ifdef ADJOINT
          ad_tnu4(i,ng)=SQRT(ABS(ad_tnu4(i,ng)))
#endif
#if defined TANGENT || defined TL_IOMS
          tl_tnu4(i,ng)=SQRT(ABS(tl_tnu4(i,ng)))
#endif
!
!  Compute inverse nudging coefficients (1/s) used in various tasks.
!
          IF (Tnudg(i,ng).gt.0.0_r8) THEN
            Tnudg(i,ng)=1.0_r8/(Tnudg(i,ng)*86400.0_r8)
          ELSE
            Tnudg(i,ng)=0.0_r8
          END IF
        END DO
      END DO

  30  FORMAT (/,' read_BioPar - variable info not yet loaded, ',        &
     &        a,i2.2,a)
  40  FORMAT (/,' read_BioPar - variable info not yet loaded, ',a)
  50  FORMAT (/,' read_BioPar - Error while processing line: ',/,a)
  60  FORMAT (/,/,' BEC Model Parameters, Grid: ',i2.2,              &
     &        /,  ' =================================',/)
  70  FORMAT (1x,i10,2x,a,t30,a)
  80  FORMAT (1p,e11.4,2x,a,t30,a)
  90  FORMAT (1p,e11.4,2x,a,t30,a,/,t32,a)
 100  FORMAT (1p,e11.4,2x,a,'(',i2.2,')',t30,a,/,t32,a,i2.2,':',1x,a)
 110  FORMAT (10x,l1,2x,a,'(',i2.2,')',t30,a,i2.2,':',1x,a)
 120  FORMAT (10x,l1,2x,a,t30,a,i2.2,':',1x,a)
 130  FORMAT (10x,l1,2x,a,t30,a,1x,a)

      RETURN
      END SUBROUTINE read_BioPar
