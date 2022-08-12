/*
**
**************************************************************************
**                                                                      **
**  BEC model variables input parameters in output NetCDF file.         **
**  It is included in routine "def_info.F"                              **
**                                                                 @DCC **
**************************************************************************
*/

!
!  Define BEC model parameters.
!
      Vinfo( 1)='BioIter'
      Vinfo( 2)='number of iterations to achieve convergence'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (exit_flag.ne.NoError) RETURN

