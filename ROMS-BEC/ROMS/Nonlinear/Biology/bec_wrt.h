/*
**
**************************************************************************
**                                                                      **
**  BEC model variables input parameters in output NetCDF file.         **
**  It is included in routine "wrt_info.F"                              **
**                                                                 @DCC **
**************************************************************************
*/

!
!  Write out BEC model parameters.
!
      CALL netcdf_put_ivar (ng, model, ncname, 'BioIter',               &
     &                      BioIter(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (exit_flag.ne.NoError) RETURN

