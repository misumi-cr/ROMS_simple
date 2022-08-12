/*
**
**************************************************************************
**                                                                      **
**  Coag model variables are used in input and output NetCDF files.     **
**  The metadata information is read from "varinfo.dat".                **
**                                                                      **
**  This file is included in file "mod_ncparam.F", routine              **
**  "initialize_ncparm".                                                **
**                                                    Kaz               **
**************************************************************************
*/

/*
**  Model state coag tracers.
*/

              CASE ('idTvar(p001_ind)')
                idTvar(p001_ind)=varid
              CASE ('idTvar(p002_ind)')
                idTvar(p002_ind)=varid
              CASE ('idTvar(p003_ind)')
                idTvar(p003_ind)=varid
              CASE ('idTvar(p004_ind)')
                idTvar(p004_ind)=varid
              CASE ('idTvar(p005_ind)')
                idTvar(p005_ind)=varid
              CASE ('idTvar(p006_ind)')
                idTvar(p006_ind)=varid
              CASE ('idTvar(p007_ind)')
                idTvar(p007_ind)=varid
              CASE ('idTvar(p008_ind)')
                idTvar(p008_ind)=varid
              CASE ('idTvar(p009_ind)')
                idTvar(p009_ind)=varid
              CASE ('idTvar(p010_ind)')
                idTvar(p010_ind)=varid
              CASE ('idTvar(p011_ind)')
                idTvar(p011_ind)=varid
              CASE ('idTvar(p012_ind)')
                idTvar(p012_ind)=varid
              CASE ('idTvar(p013_ind)')
                idTvar(p013_ind)=varid
              CASE ('idTvar(p014_ind)')
                idTvar(p014_ind)=varid
              CASE ('idTvar(p015_ind)')
                idTvar(p015_ind)=varid
              CASE ('idTvar(p016_ind)')
                idTvar(p016_ind)=varid
              CASE ('idTvar(p017_ind)')
                idTvar(p017_ind)=varid
              CASE ('idTvar(p018_ind)')
                idTvar(p018_ind)=varid
              CASE ('idTvar(p019_ind)')
                idTvar(p019_ind)=varid
              CASE ('idTvar(p020_ind)')
                idTvar(p020_ind)=varid
              CASE ('idTvar(p021_ind)')
                idTvar(p021_ind)=varid
              CASE ('idTvar(p022_ind)')
                idTvar(p022_ind)=varid
              CASE ('idTvar(p023_ind)')
                idTvar(p023_ind)=varid
              CASE ('idTvar(p024_ind)')
                idTvar(p024_ind)=varid
              CASE ('idTvar(p025_ind)')
                idTvar(p025_ind)=varid
              CASE ('idTvar(p026_ind)')
                idTvar(p026_ind)=varid
              CASE ('idTvar(p027_ind)')
                idTvar(p027_ind)=varid
              CASE ('idTvar(p028_ind)')
                idTvar(p028_ind)=varid
              CASE ('idTvar(p029_ind)')
                idTvar(p029_ind)=varid
              CASE ('idTvar(p030_ind)')
                idTvar(p030_ind)=varid
              CASE ('idTvar(p031_ind)')
                idTvar(p031_ind)=varid
              CASE ('idTvar(p032_ind)')
                idTvar(p032_ind)=varid
              CASE ('idTvar(p033_ind)')
                idTvar(p033_ind)=varid
              CASE ('idTvar(p034_ind)')
                idTvar(p034_ind)=varid
              CASE ('idTvar(p035_ind)')
                idTvar(p035_ind)=varid
              CASE ('idTvar(p036_ind)')
                idTvar(p036_ind)=varid
              CASE ('idTvar(p037_ind)')
                idTvar(p037_ind)=varid
              CASE ('idTvar(p038_ind)')
                idTvar(p038_ind)=varid
              CASE ('idTvar(p039_ind)')
                idTvar(p039_ind)=varid
              CASE ('idTvar(p040_ind)')
                idTvar(p040_ind)=varid
              CASE ('idTvar(p041_ind)')
                idTvar(p041_ind)=varid
              CASE ('idTvar(p042_ind)')
                idTvar(p042_ind)=varid
              CASE ('idTvar(p043_ind)')
                idTvar(p043_ind)=varid
              CASE ('idTvar(p044_ind)')
                idTvar(p044_ind)=varid
              CASE ('idTvar(p045_ind)')
                idTvar(p045_ind)=varid
              CASE ('idTvar(p046_ind)')
                idTvar(p046_ind)=varid
              CASE ('idTvar(p047_ind)')
                idTvar(p047_ind)=varid

!!#if defined AD_SENSITIVITY   || defined IS4DVAR_SENSITIVITY || \
!!    defined OPT_OBSERVATIONS || defined SENSITIVITY_4DVAR   || \
!!    defined SO_SEMI
!!
!!/*
!!**  Adjoint sensitivity state coag tracers.
!!*/
!!
!!              CASE ('idTads(p001_ind)')
!!                idTads(p001_ind)=varid
!!#endif
!
!/*
!**  Coag tracers open boundary conditions.
!*/
!
!              CASE ('idTbry(iwest,p001_ind)')
!                idTbry(iwest,p001_ind)=varid
!              CASE ('idTbry(ieast,p001_ind)')
!                idTbry(ieast,p001_ind)=varid
!              CASE ('idTbry(isouth,p001_ind)')
!                idTbry(isouth,p001_ind)=varid
!              CASE ('idTbry(inorth,p001_ind)')
!                idTbry(inorth,p001_ind)=varid
!
!#ifdef TS_PSOURCE
!
!/*
!**  Coag tracers point Source/Sinks (river runoff).
!*/
!
!
!              CASE ('idRtrc(p001_ind)')
!                idRtrc(p001_ind)=varid
!
!#endif
