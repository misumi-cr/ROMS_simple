/*
**
**************************************************************************
**                                                                      **
**  BEC model variables are used in input and output NetCDF files.      **
**  The metadata information is read from "varinfo.dat".                **
**                                                                      **
**  This file is included in file "mod_ncparam.F", routine              **
**  "initialize_ncparm".                                                **
**                                                    Ryosuke Niwa @DCC **
**************************************************************************
*/

/*
**  Model state biological tracers.
*/

              CASE ('idTvar(po4_ind)')
                idTvar(po4_ind)=varid
              CASE ('idTvar(no3_ind)')
                idTvar(no3_ind)=varid
              CASE ('idTvar(sio3_ind)')
                idTvar(sio3_ind)=varid
              CASE ('idTvar(nh4_ind)')
                idTvar(nh4_ind)=varid
              CASE ('idTvar(o2_ind)')
                idTvar(o2_ind)=varid
              CASE ('idTvar(dic_ind)')
                idTvar(dic_ind)=varid
              CASE ('idTvar(dic_alt_co2_ind)')
                idTvar(dic_alt_co2_ind)=varid
              CASE ('idTvar(alk_ind)')
                idTvar(alk_ind)=varid
              CASE ('idTvar(doc_ind)')
                idTvar(doc_ind)=varid
              CASE ('idTvar(don_ind)')
                idTvar(don_ind)=varid
              CASE ('idTvar(dop_ind)')
                idTvar(dop_ind)=varid
              CASE ('idTvar(dopr_ind)')
                idTvar(dopr_ind)=varid
              CASE ('idTvar(donr_ind)')
                idTvar(donr_ind)=varid
              CASE ('idTvar(zooC_ind)')
                idTvar(zooC_ind)=varid
              CASE ('idTvar(spChl_ind)')
                idTvar(spChl_ind)=varid
              CASE ('idTvar(spC_ind)')
                idTvar(spC_ind)=varid
              CASE ('idTvar(spCaCO3_ind)')
                idTvar(spCaCO3_ind)=varid
              CASE ('idTvar(diatC_ind)')
                idTvar(diatC_ind)=varid
              CASE ('idTvar(diatSi_ind)')
                idTvar(diatSi_ind)=varid
              CASE ('idTvar(diatChl_ind)')
                idTvar(diatChl_ind)=varid
              CASE ('idTvar(diazC_ind)')
                idTvar(diazC_ind)=varid
              CASE ('idTvar(diazChl_ind)')
                idTvar(diazChl_ind)=varid

#ifndef FE_TAG
              CASE ('idTvar(fe_ind)')
                idTvar(fe_ind)=varid
              CASE ('idTvar(dofe_ind)')
                idTvar(dofe_ind)=varid
              CASE ('idTvar(spFe_ind)')
                idTvar(spFe_ind)=varid
              CASE ('idTvar(diatFe_ind)')
                idTvar(diatFe_ind)=varid
              CASE ('idTvar(diazFe_ind)')
                idTvar(diazFe_ind)=varid
#else
              CASE ('idTvar(fe0_ind)')
                idTvar(fe0_ind)=varid
              CASE ('idTvar(dofe0_ind)')
                idTvar(dofe0_ind)=varid
              CASE ('idTvar(spFe0_ind)')
                idTvar(spFe0_ind)=varid
              CASE ('idTvar(diatFe0_ind)')
                idTvar(diatFe0_ind)=varid
              CASE ('idTvar(diazFe0_ind)')
                idTvar(diazFe0_ind)=varid
              CASE ('idTvar(zooFe0_ind)')
                idTvar(zooFe0_ind)=varid

              CASE ('idTvar(fe1_ind)')
                idTvar(fe1_ind)=varid
              CASE ('idTvar(dofe1_ind)')
                idTvar(dofe1_ind)=varid
              CASE ('idTvar(spFe1_ind)')
                idTvar(spFe1_ind)=varid
              CASE ('idTvar(diatFe1_ind)')
                idTvar(diatFe1_ind)=varid
              CASE ('idTvar(diazFe1_ind)')
                idTvar(diazFe1_ind)=varid
              CASE ('idTvar(zooFe1_ind)')
                idTvar(zooFe1_ind)=varid

              CASE ('idTvar(fe2_ind)')
                idTvar(fe2_ind)=varid
              CASE ('idTvar(dofe2_ind)')
                idTvar(dofe2_ind)=varid
              CASE ('idTvar(spFe2_ind)')
                idTvar(spFe2_ind)=varid
              CASE ('idTvar(diatFe2_ind)')
                idTvar(diatFe2_ind)=varid
              CASE ('idTvar(diazFe2_ind)')
                idTvar(diazFe2_ind)=varid
              CASE ('idTvar(zooFe2_ind)')
                idTvar(zooFe2_ind)=varid

              CASE ('idTvar(fe3_ind)')
                idTvar(fe3_ind)=varid
              CASE ('idTvar(dofe3_ind)')
                idTvar(dofe3_ind)=varid
              CASE ('idTvar(spFe3_ind)')
                idTvar(spFe3_ind)=varid
              CASE ('idTvar(diatFe3_ind)')
                idTvar(diatFe3_ind)=varid
              CASE ('idTvar(diazFe3_ind)')
                idTvar(diazFe3_ind)=varid
              CASE ('idTvar(zooFe3_ind)')
                idTvar(zooFe3_ind)=varid

              CASE ('idTvar(fe4_ind)')
                idTvar(fe4_ind)=varid
              CASE ('idTvar(dofe4_ind)')
                idTvar(dofe4_ind)=varid
              CASE ('idTvar(spFe4_ind)')
                idTvar(spFe4_ind)=varid
              CASE ('idTvar(diatFe4_ind)')
                idTvar(diatFe4_ind)=varid
              CASE ('idTvar(diazFe4_ind)')
                idTvar(diazFe4_ind)=varid
              CASE ('idTvar(zooFe4_ind)')
                idTvar(zooFe4_ind)=varid

              CASE ('idTvar(fe5_ind)')
                idTvar(fe5_ind)=varid
              CASE ('idTvar(dofe5_ind)')
                idTvar(dofe5_ind)=varid
              CASE ('idTvar(spFe5_ind)')
                idTvar(spFe5_ind)=varid
              CASE ('idTvar(diatFe5_ind)')
                idTvar(diatFe5_ind)=varid
              CASE ('idTvar(diazFe5_ind)')
                idTvar(diazFe5_ind)=varid
              CASE ('idTvar(zooFe5_ind)')
                idTvar(zooFe5_ind)=varid

              CASE ('idTvar(fe6_ind)')
                idTvar(fe6_ind)=varid
              CASE ('idTvar(dofe6_ind)')
                idTvar(dofe6_ind)=varid
              CASE ('idTvar(spFe6_ind)')
                idTvar(spFe6_ind)=varid
              CASE ('idTvar(diatFe6_ind)')
                idTvar(diatFe6_ind)=varid
              CASE ('idTvar(diazFe6_ind)')
                idTvar(diazFe6_ind)=varid
              CASE ('idTvar(zooFe6_ind)')
                idTvar(zooFe6_ind)=varid

              CASE ('idTvar(fe7_ind)')
                idTvar(fe7_ind)=varid
              CASE ('idTvar(dofe7_ind)')
                idTvar(dofe7_ind)=varid
              CASE ('idTvar(spFe7_ind)')
                idTvar(spFe7_ind)=varid
              CASE ('idTvar(diatFe7_ind)')
                idTvar(diatFe7_ind)=varid
              CASE ('idTvar(diazFe7_ind)')
                idTvar(diazFe7_ind)=varid
              CASE ('idTvar(zooFe7_ind)')
                idTvar(zooFe7_ind)=varid
#endif

#if defined CISO
              CASE ('idTvar(di13c_ind)')
                idTvar(di13c_ind)=varid
              CASE ('idTvar(do13c_ind)')
                idTvar(do13c_ind)=varid
              CASE ('idTvar(zoo13C_ind)')
                idTvar(zoo13C_ind)=varid
              CASE ('idTvar(di14c_ind)')
                idTvar(di14c_ind)=varid
              CASE ('idTvar(do14c_ind)')
                idTvar(do14c_ind)=varid
              CASE ('idTvar(zoo14C_ind)')
                idTvar(zoo14C_ind)=varid
              CASE ('idTvar(spC13_ind)')
                idTvar(spC13_ind)=varid
              CASE ('idTvar(spC14_ind)')
                idTvar(spC14_ind)=varid
              CASE ('idTvar(spCa13CO3_ind)')
                idTvar(spCa13CO3_ind)=varid
              CASE ('idTvar(spCa14CO3_ind)')
                idTvar(spCa14CO3_ind)=varid
              CASE ('idTvar(diatC13_ind)')
                idTvar(diatC13_ind)=varid
              CASE ('idTvar(diatC14_ind)')
                idTvar(diatC14_ind)=varid
              CASE ('idTvar(diazC13_ind)')
                idTvar(diazC13_ind)=varid
              CASE ('idTvar(diazC14_ind)')
                idTvar(diazC14_ind)=varid
#endif

#if defined AD_SENSITIVITY   || defined IS4DVAR_SENSITIVITY || \
    defined OPT_OBSERVATIONS || defined SENSITIVITY_4DVAR   || \
    defined SO_SEMI

/*
**  Adjoint sensitivity state biological tracers.
*/

              CASE ('idTads(po4_ind)')
                idTads(po4_ind)=varid
              CASE ('idTads(no3_ind)')
                idTads(no3_ind)=varid
              CASE ('idTads(sio3_ind)')
                idTads(sio3_ind)=varid
              CASE ('idTads(nh4_ind)')
                idTads(nh4_ind)=varid
              CASE ('idTads(fe_ind)')
                idTads(fe_ind)=varid
              CASE ('idTads(o2_ind)')
                idTads(o2_ind)=varid
              CASE ('idTads(dic_ind)')
                idTads(dic_ind)=varid
              CASE ('idTads(dic_alt_co2_ind)')
                idTads(dic_alt_co2_ind)=varid
              CASE ('idTads(alk_ind)')
                idTads(alk_ind)=varid
              CASE ('idTads(doc_ind)')
                idTads(doc_ind)=varid
              CASE ('idTads(don_ind)')
                idTads(don_ind)=varid
              CASE ('idTads(dofe_ind)')
                idTads(dofe_ind)=varid
              CASE ('idTads(dop_ind)')
                idTads(dop_ind)=varid
              CASE ('idTads(dopr_ind)')
                idTads(dopr_ind)=varid
              CASE ('idTads(donr_ind)')
                idTads(donr_ind)=varid
              CASE ('idTads(zooC_ind)')
                idTads(zooC_ind)=varid
              CASE ('idTads(spChl_ind)')
                idTads(spChl_ind)=varid
              CASE ('idTads(spC_ind)')
                idTads(spC_ind)=varid
              CASE ('idTads(spFe_ind)')
                idTads(spFe_ind)=varid
              CASE ('idTads(spCaCO3_ind)')
                idTads(spCaCO3_ind)=varid
              CASE ('idTads(diatChl_ind)')
                idTads(diatChl_ind)=varid
              CASE ('idTads(diatC_ind)')
                idTads(diatC_ind)=varid
              CASE ('idTads(diatFe_ind)')
                idTads(diatFe_ind)=varid
              CASE ('idTads(diatSi_ind)')
                idTads(diatSi_ind)=varid
              CASE ('idTads(diazChl_ind)')
                idTads(diazChl_ind)=varid
              CASE ('idTads(diazC_ind)')
                idTads(diazC_ind)=varid
              CASE ('idTads(diazFe_ind)')
                idTads(diazFe_ind)=varid
#if defined CISO
              CASE ('idTabs(di13c_ind)')
                idTabs(di13c_ind)=varid
              CASE ('idTabs(do13c_ind)')
                idTabs(do13c_ind)=varid
              CASE ('idTabs(zoo13C_ind)')
                idTabs(zoo13C_ind)=varid
              CASE ('idTabs(di14c_ind)')
                idTabs(di14c_ind)=varid
              CASE ('idTabs(do14c_ind)')
                idTabs(do14c_ind)=varid
              CASE ('idTabs(zoo14C_ind)')
                idTabs(zoo14C_ind)=varid
              CASE ('idTabs(spC13_ind)')
                idTabs(spC13_ind)=varid
              CASE ('idTabs(spC14_ind)')
                idTabs(spC14_ind)=varid
              CASE ('idTabs(spCa13CO3_ind)')
                idTabs(spCa13CO3_ind)=varid
              CASE ('idTabs(spCa14CO3_ind)')
                idTabs(spCa14CO3_ind)=varid
              CASE ('idTabs(diatC13_ind)')
                idTabs(diatC13_ind)=varid
              CASE ('idTabs(diatC14_ind)')
                idTabs(diatC14_ind)=varid
              CASE ('idTabs(diazC13_ind)')
                idTabs(diazC13_ind)=varid
              CASE ('idTabs(diazC14_ind)')
                idTabs(diazC14_ind)=varid
#endif
#endif

/*
**  Biological tracers open boundary conditions.
*/

              CASE ('idTbry(iwest,po4_ind)')
                idTbry(iwest,po4_ind)=varid
              CASE ('idTbry(ieast,po4_ind)')
                idTbry(ieast,po4_ind)=varid
              CASE ('idTbry(isouth,po4_ind)')
                idTbry(isouth,po4_ind)=varid
              CASE ('idTbry(inorth,po4_ind)')
                idTbry(inorth,po4_ind)=varid

              CASE ('idTbry(iwest,no3_ind)')
                idTbry(iwest,no3_ind)=varid
              CASE ('idTbry(ieast,no3_ind)')
                idTbry(ieast,no3_ind)=varid
              CASE ('idTbry(isouth,no3_ind)')
                idTbry(isouth,no3_ind)=varid
              CASE ('idTbry(inorth,no3_ind)')
                idTbry(inorth,no3_ind)=varid

              CASE ('idTbry(iwest,sio3_ind)')
                idTbry(iwest,sio3_ind)=varid
              CASE ('idTbry(ieast,sio3_ind)')
                idTbry(ieast,sio3_ind)=varid
              CASE ('idTbry(isouth,sio3_ind)')
                idTbry(isouth,sio3_ind)=varid
              CASE ('idTbry(inorth,sio3_ind)')
                idTbry(inorth,sio3_ind)=varid

              CASE ('idTbry(iwest,nh4_ind)')
                idTbry(iwest,nh4_ind)=varid
              CASE ('idTbry(ieast,nh4_ind)')
                idTbry(ieast,nh4_ind)=varid
              CASE ('idTbry(isouth,nh4_ind)')
                idTbry(isouth,nh4_ind)=varid
              CASE ('idTbry(inorth,nh4_ind)')
                idTbry(inorth,nh4_ind)=varid

              CASE ('idTbry(iwest,o2_ind)')
                idTbry(iwest,o2_ind)=varid
              CASE ('idTbry(ieast,o2_ind)')
                idTbry(ieast,o2_ind)=varid
              CASE ('idTbry(isouth,o2_ind)')
                idTbry(isouth,o2_ind)=varid
              CASE ('idTbry(inorth,o2_ind)')
                idTbry(inorth,o2_ind)=varid

              CASE ('idTbry(iwest,dic_ind)')
                idTbry(iwest,dic_ind)=varid
              CASE ('idTbry(ieast,dic_ind)')
                idTbry(ieast,dic_ind)=varid
              CASE ('idTbry(isouth,dic_ind)')
                idTbry(isouth,dic_ind)=varid
              CASE ('idTbry(inorth,dic_ind)')
                idTbry(inorth,dic_ind)=varid

              CASE ('idTbry(iwest,dic_alt_co2_ind)')
                idTbry(iwest,dic_alt_co2_ind)=varid
              CASE ('idTbry(ieast,dic_alt_co2_ind)')
                idTbry(ieast,dic_alt_co2_ind)=varid
              CASE ('idTbry(isouth,dic_alt_co2_ind)')
                idTbry(isouth,dic_alt_co2_ind)=varid
              CASE ('idTbry(inorth,dic_alt_co2_ind)')
                idTbry(inorth,dic_alt_co2_ind)=varid

              CASE ('idTbry(iwest,alk_ind)')
                idTbry(iwest,alk_ind)=varid
              CASE ('idTbry(ieast,alk_ind)')
                idTbry(ieast,alk_ind)=varid
              CASE ('idTbry(isouth,alk_ind)')
                idTbry(isouth,alk_ind)=varid
              CASE ('idTbry(inorth,alk_ind)')
                idTbry(inorth,alk_ind)=varid

              CASE ('idTbry(iwest,doc_ind)')
                idTbry(iwest,doc_ind)=varid
              CASE ('idTbry(ieast,doc_ind)')
                idTbry(ieast,doc_ind)=varid
              CASE ('idTbry(isouth,doc_ind)')
                idTbry(isouth,doc_ind)=varid
              CASE ('idTbry(inorth,doc_ind)')
                idTbry(inorth,doc_ind)=varid

              CASE ('idTbry(iwest,don_ind)')
                idTbry(iwest,don_ind)=varid
              CASE ('idTbry(ieast,don_ind)')
                idTbry(ieast,don_ind)=varid
              CASE ('idTbry(isouth,don_ind)')
                idTbry(isouth,don_ind)=varid
              CASE ('idTbry(inorth,don_ind)')
                idTbry(inorth,don_ind)=varid

              CASE ('idTbry(iwest,dop_ind)')
                idTbry(iwest,dop_ind)=varid
              CASE ('idTbry(ieast,dop_ind)')
                idTbry(ieast,dop_ind)=varid
              CASE ('idTbry(isouth,dop_ind)')
                idTbry(isouth,dop_ind)=varid
              CASE ('idTbry(inorth,dop_ind)')
                idTbry(inorth,dop_ind)=varid

              CASE ('idTbry(iwest,dopr_ind)')
                idTbry(iwest,dopr_ind)=varid
              CASE ('idTbry(ieast,dopr_ind)')
                idTbry(ieast,dopr_ind)=varid
              CASE ('idTbry(isouth,dopr_ind)')
                idTbry(isouth,dopr_ind)=varid
              CASE ('idTbry(inorth,dopr_ind)')
                idTbry(inorth,dopr_ind)=varid

              CASE ('idTbry(iwest,donr_ind)')
                idTbry(iwest,donr_ind)=varid
              CASE ('idTbry(ieast,donr_ind)')
                idTbry(ieast,donr_ind)=varid
              CASE ('idTbry(isouth,donr_ind)')
                idTbry(isouth,donr_ind)=varid
              CASE ('idTbry(inorth,donr_ind)')
                idTbry(inorth,donr_ind)=varid

              CASE ('idTbry(iwest,zooC_ind)')
                idTbry(iwest,zooC_ind)=varid
              CASE ('idTbry(ieast,zooC_ind)')
                idTbry(ieast,zooC_ind)=varid
              CASE ('idTbry(isouth,zooC_ind)')
                idTbry(isouth,zooC_ind)=varid
              CASE ('idTbry(inorth,zooC_ind)')
                idTbry(inorth,zooC_ind)=varid

              CASE ('idTbry(iwest,spChl_ind)')
                idTbry(iwest,spChl_ind)=varid
              CASE ('idTbry(ieast,spChl_ind)')
                idTbry(ieast,spChl_ind)=varid
              CASE ('idTbry(isouth,spChl_ind)')
                idTbry(isouth,spChl_ind)=varid
              CASE ('idTbry(inorth,spChl_ind)')
                idTbry(inorth,spChl_ind)=varid

              CASE ('idTbry(iwest,spC_ind)')
                idTbry(iwest,spC_ind)=varid
              CASE ('idTbry(ieast,spC_ind)')
                idTbry(ieast,spC_ind)=varid
              CASE ('idTbry(isouth,spC_ind)')
                idTbry(isouth,spC_ind)=varid
              CASE ('idTbry(inorth,spC_ind)')
                idTbry(inorth,spC_ind)=varid

              CASE ('idTbry(iwest,spCaCO3_ind)')
                idTbry(iwest,spCaCO3_ind)=varid
              CASE ('idTbry(ieast,spCaCO3_ind)')
                idTbry(ieast,spCaCO3_ind)=varid
              CASE ('idTbry(isouth,spCaCO3_ind)')
                idTbry(isouth,spCaCO3_ind)=varid
              CASE ('idTbry(inorth,spCaCO3_ind)')
                idTbry(inorth,spCaCO3_ind)=varid

              CASE ('idTbry(iwest,diatChl_ind)')
                idTbry(iwest,diatChl_ind)=varid
              CASE ('idTbry(ieast,diatChl_ind)')
                idTbry(ieast,diatChl_ind)=varid
              CASE ('idTbry(isouth,diatChl_ind)')
                idTbry(isouth,diatChl_ind)=varid
              CASE ('idTbry(inorth,diatChl_ind)')
                idTbry(inorth,diatChl_ind)=varid

              CASE ('idTbry(iwest,diatC_ind)')
                idTbry(iwest,diatC_ind)=varid
              CASE ('idTbry(ieast,diatC_ind)')
                idTbry(ieast,diatC_ind)=varid
              CASE ('idTbry(isouth,diatC_ind)')
                idTbry(isouth,diatC_ind)=varid
              CASE ('idTbry(inorth,diatC_ind)')
                idTbry(inorth,diatC_ind)=varid

              CASE ('idTbry(iwest,diatSi_ind)')
                idTbry(iwest,diatSi_ind)=varid
              CASE ('idTbry(ieast,diatSi_ind)')
                idTbry(ieast,diatSi_ind)=varid
              CASE ('idTbry(isouth,diatSi_ind)')
                idTbry(isouth,diatSi_ind)=varid
              CASE ('idTbry(inorth,diatSi_ind)')
                idTbry(inorth,diatSi_ind)=varid

              CASE ('idTbry(iwest,diazChl_ind)')
                idTbry(iwest,diazChl_ind)=varid
              CASE ('idTbry(ieast,diazChl_ind)')
                idTbry(ieast,diazChl_ind)=varid
              CASE ('idTbry(isouth,diazChl_ind)')
                idTbry(isouth,diazChl_ind)=varid
              CASE ('idTbry(inorth,diazChl_ind)')
                idTbry(inorth,diazChl_ind)=varid

              CASE ('idTbry(iwest,diazC_ind)')
                idTbry(iwest,diazC_ind)=varid
              CASE ('idTbry(ieast,diazC_ind)')
                idTbry(ieast,diazC_ind)=varid
              CASE ('idTbry(isouth,diazC_ind)')
                idTbry(isouth,diazC_ind)=varid
              CASE ('idTbry(inorth,diazC_ind)')
                idTbry(inorth,diazC_ind)=varid

#ifndef FE_TAG
              CASE ('idTbry(iwest,fe_ind)')
                idTbry(iwest,fe_ind)=varid
              CASE ('idTbry(ieast,fe_ind)')
                idTbry(ieast,fe_ind)=varid
              CASE ('idTbry(isouth,fe_ind)')
                idTbry(isouth,fe_ind)=varid
              CASE ('idTbry(inorth,fe_ind)')
                idTbry(inorth,fe_ind)=varid

              CASE ('idTbry(iwest,dofe_ind)')
                idTbry(iwest,dofe_ind)=varid
              CASE ('idTbry(ieast,dofe_ind)')
                idTbry(ieast,dofe_ind)=varid
              CASE ('idTbry(isouth,dofe_ind)')
                idTbry(isouth,dofe_ind)=varid
              CASE ('idTbry(inorth,dofe_ind)')
                idTbry(inorth,dofe_ind)=varid

              CASE ('idTbry(iwest,spFe_ind)')
                idTbry(iwest,spFe_ind)=varid
              CASE ('idTbry(ieast,spFe_ind)')
                idTbry(ieast,spFe_ind)=varid
              CASE ('idTbry(isouth,spFe_ind)')
                idTbry(isouth,spFe_ind)=varid
              CASE ('idTbry(inorth,spFe_ind)')
                idTbry(inorth,spFe_ind)=varid

              CASE ('idTbry(iwest,diatFe_ind)')
                idTbry(iwest,diatFe_ind)=varid
              CASE ('idTbry(ieast,diatFe_ind)')
                idTbry(ieast,diatFe_ind)=varid
              CASE ('idTbry(isouth,diatFe_ind)')
                idTbry(isouth,diatFe_ind)=varid
              CASE ('idTbry(inorth,diatFe_ind)')
                idTbry(inorth,diatFe_ind)=varid

              CASE ('idTbry(iwest,diazFe_ind)')
                idTbry(iwest,diazFe_ind)=varid
              CASE ('idTbry(ieast,diazFe_ind)')
                idTbry(ieast,diazFe_ind)=varid
              CASE ('idTbry(isouth,diazFe_ind)')
                idTbry(isouth,diazFe_ind)=varid
              CASE ('idTbry(inorth,diazFe_ind)')
                idTbry(inorth,diazFe_ind)=varid

#else
! -- fe0 --
              CASE ('idTbry(iwest,fe0_ind)')
                idTbry(iwest,fe0_ind)=varid
              CASE ('idTbry(ieast,fe0_ind)')
                idTbry(ieast,fe0_ind)=varid
              CASE ('idTbry(isouth,fe0_ind)')
                idTbry(isouth,fe0_ind)=varid
              CASE ('idTbry(inorth,fe0_ind)')
                idTbry(inorth,fe0_ind)=varid

              CASE ('idTbry(iwest,dofe0_ind)')
                idTbry(iwest,dofe0_ind)=varid
              CASE ('idTbry(ieast,dofe0_ind)')
                idTbry(ieast,dofe0_ind)=varid
              CASE ('idTbry(isouth,dofe0_ind)')
                idTbry(isouth,dofe0_ind)=varid
              CASE ('idTbry(inorth,dofe0_ind)')
                idTbry(inorth,dofe0_ind)=varid

              CASE ('idTbry(iwest,spFe0_ind)')
                idTbry(iwest,spFe0_ind)=varid
              CASE ('idTbry(ieast,spFe0_ind)')
                idTbry(ieast,spFe0_ind)=varid
              CASE ('idTbry(isouth,spFe0_ind)')
                idTbry(isouth,spFe0_ind)=varid
              CASE ('idTbry(inorth,spFe0_ind)')
                idTbry(inorth,spFe0_ind)=varid

              CASE ('idTbry(iwest,diatFe0_ind)')
                idTbry(iwest,diatFe0_ind)=varid
              CASE ('idTbry(ieast,diatFe0_ind)')
                idTbry(ieast,diatFe0_ind)=varid
              CASE ('idTbry(isouth,diatFe0_ind)')
                idTbry(isouth,diatFe0_ind)=varid
              CASE ('idTbry(inorth,diatFe0_ind)')
                idTbry(inorth,diatFe0_ind)=varid

              CASE ('idTbry(iwest,diazFe0_ind)')
                idTbry(iwest,diazFe0_ind)=varid
              CASE ('idTbry(ieast,diazFe0_ind)')
                idTbry(ieast,diazFe0_ind)=varid
              CASE ('idTbry(isouth,diazFe0_ind)')
                idTbry(isouth,diazFe0_ind)=varid
              CASE ('idTbry(inorth,diazFe0_ind)')
                idTbry(inorth,diazFe0_ind)=varid

              CASE ('idTbry(iwest,zooFe0_ind)')
                idTbry(iwest,zooFe0_ind)=varid
              CASE ('idTbry(ieast,zooFe0_ind)')
                idTbry(ieast,zooFe0_ind)=varid
              CASE ('idTbry(isouth,zooFe0_ind)')
                idTbry(isouth,zooFe0_ind)=varid
              CASE ('idTbry(inorth,zooFe0_ind)')
                idTbry(inorth,zooFe0_ind)=varid

! -- fe1 --
              CASE ('idTbry(iwest,fe1_ind)')
                idTbry(iwest,fe1_ind)=varid
              CASE ('idTbry(ieast,fe1_ind)')
                idTbry(ieast,fe1_ind)=varid
              CASE ('idTbry(isouth,fe1_ind)')
                idTbry(isouth,fe1_ind)=varid
              CASE ('idTbry(inorth,fe1_ind)')
                idTbry(inorth,fe1_ind)=varid

              CASE ('idTbry(iwest,dofe1_ind)')
                idTbry(iwest,dofe1_ind)=varid
              CASE ('idTbry(ieast,dofe1_ind)')
                idTbry(ieast,dofe1_ind)=varid
              CASE ('idTbry(isouth,dofe1_ind)')
                idTbry(isouth,dofe1_ind)=varid
              CASE ('idTbry(inorth,dofe1_ind)')
                idTbry(inorth,dofe1_ind)=varid

              CASE ('idTbry(iwest,spFe1_ind)')
                idTbry(iwest,spFe1_ind)=varid
              CASE ('idTbry(ieast,spFe1_ind)')
                idTbry(ieast,spFe1_ind)=varid
              CASE ('idTbry(isouth,spFe1_ind)')
                idTbry(isouth,spFe1_ind)=varid
              CASE ('idTbry(inorth,spFe1_ind)')
                idTbry(inorth,spFe1_ind)=varid

              CASE ('idTbry(iwest,diatFe1_ind)')
                idTbry(iwest,diatFe1_ind)=varid
              CASE ('idTbry(ieast,diatFe1_ind)')
                idTbry(ieast,diatFe1_ind)=varid
              CASE ('idTbry(isouth,diatFe1_ind)')
                idTbry(isouth,diatFe1_ind)=varid
              CASE ('idTbry(inorth,diatFe1_ind)')
                idTbry(inorth,diatFe1_ind)=varid

              CASE ('idTbry(iwest,diazFe1_ind)')
                idTbry(iwest,diazFe1_ind)=varid
              CASE ('idTbry(ieast,diazFe1_ind)')
                idTbry(ieast,diazFe1_ind)=varid
              CASE ('idTbry(isouth,diazFe1_ind)')
                idTbry(isouth,diazFe1_ind)=varid
              CASE ('idTbry(inorth,diazFe1_ind)')
                idTbry(inorth,diazFe1_ind)=varid

              CASE ('idTbry(iwest,zooFe1_ind)')
                idTbry(iwest,zooFe1_ind)=varid
              CASE ('idTbry(ieast,zooFe1_ind)')
                idTbry(ieast,zooFe1_ind)=varid
              CASE ('idTbry(isouth,zooFe1_ind)')
                idTbry(isouth,zooFe1_ind)=varid
              CASE ('idTbry(inorth,zooFe1_ind)')
                idTbry(inorth,zooFe1_ind)=varid

! -- fe2 --
              CASE ('idTbry(iwest,fe2_ind)')
                idTbry(iwest,fe2_ind)=varid
              CASE ('idTbry(ieast,fe2_ind)')
                idTbry(ieast,fe2_ind)=varid
              CASE ('idTbry(isouth,fe2_ind)')
                idTbry(isouth,fe2_ind)=varid
              CASE ('idTbry(inorth,fe2_ind)')
                idTbry(inorth,fe2_ind)=varid

              CASE ('idTbry(iwest,dofe2_ind)')
                idTbry(iwest,dofe2_ind)=varid
              CASE ('idTbry(ieast,dofe2_ind)')
                idTbry(ieast,dofe2_ind)=varid
              CASE ('idTbry(isouth,dofe2_ind)')
                idTbry(isouth,dofe2_ind)=varid
              CASE ('idTbry(inorth,dofe2_ind)')
                idTbry(inorth,dofe2_ind)=varid

              CASE ('idTbry(iwest,spFe2_ind)')
                idTbry(iwest,spFe2_ind)=varid
              CASE ('idTbry(ieast,spFe2_ind)')
                idTbry(ieast,spFe2_ind)=varid
              CASE ('idTbry(isouth,spFe2_ind)')
                idTbry(isouth,spFe2_ind)=varid
              CASE ('idTbry(inorth,spFe2_ind)')
                idTbry(inorth,spFe2_ind)=varid

              CASE ('idTbry(iwest,diatFe2_ind)')
                idTbry(iwest,diatFe2_ind)=varid
              CASE ('idTbry(ieast,diatFe2_ind)')
                idTbry(ieast,diatFe2_ind)=varid
              CASE ('idTbry(isouth,diatFe2_ind)')
                idTbry(isouth,diatFe2_ind)=varid
              CASE ('idTbry(inorth,diatFe2_ind)')
                idTbry(inorth,diatFe2_ind)=varid

              CASE ('idTbry(iwest,diazFe2_ind)')
                idTbry(iwest,diazFe2_ind)=varid
              CASE ('idTbry(ieast,diazFe2_ind)')
                idTbry(ieast,diazFe2_ind)=varid
              CASE ('idTbry(isouth,diazFe2_ind)')
                idTbry(isouth,diazFe2_ind)=varid
              CASE ('idTbry(inorth,diazFe2_ind)')
                idTbry(inorth,diazFe2_ind)=varid

              CASE ('idTbry(iwest,zooFe2_ind)')
                idTbry(iwest,zooFe2_ind)=varid
              CASE ('idTbry(ieast,zooFe2_ind)')
                idTbry(ieast,zooFe2_ind)=varid
              CASE ('idTbry(isouth,zooFe2_ind)')
                idTbry(isouth,zooFe2_ind)=varid
              CASE ('idTbry(inorth,zooFe2_ind)')
                idTbry(inorth,zooFe2_ind)=varid

! -- fe3 --
              CASE ('idTbry(iwest,fe3_ind)')
                idTbry(iwest,fe3_ind)=varid
              CASE ('idTbry(ieast,fe3_ind)')
                idTbry(ieast,fe3_ind)=varid
              CASE ('idTbry(isouth,fe3_ind)')
                idTbry(isouth,fe3_ind)=varid
              CASE ('idTbry(inorth,fe3_ind)')
                idTbry(inorth,fe3_ind)=varid

              CASE ('idTbry(iwest,dofe3_ind)')
                idTbry(iwest,dofe3_ind)=varid
              CASE ('idTbry(ieast,dofe3_ind)')
                idTbry(ieast,dofe3_ind)=varid
              CASE ('idTbry(isouth,dofe3_ind)')
                idTbry(isouth,dofe3_ind)=varid
              CASE ('idTbry(inorth,dofe3_ind)')
                idTbry(inorth,dofe3_ind)=varid

              CASE ('idTbry(iwest,spFe3_ind)')
                idTbry(iwest,spFe3_ind)=varid
              CASE ('idTbry(ieast,spFe3_ind)')
                idTbry(ieast,spFe3_ind)=varid
              CASE ('idTbry(isouth,spFe3_ind)')
                idTbry(isouth,spFe3_ind)=varid
              CASE ('idTbry(inorth,spFe3_ind)')
                idTbry(inorth,spFe3_ind)=varid

              CASE ('idTbry(iwest,diatFe3_ind)')
                idTbry(iwest,diatFe3_ind)=varid
              CASE ('idTbry(ieast,diatFe3_ind)')
                idTbry(ieast,diatFe3_ind)=varid
              CASE ('idTbry(isouth,diatFe3_ind)')
                idTbry(isouth,diatFe3_ind)=varid
              CASE ('idTbry(inorth,diatFe3_ind)')
                idTbry(inorth,diatFe3_ind)=varid

              CASE ('idTbry(iwest,diazFe3_ind)')
                idTbry(iwest,diazFe3_ind)=varid
              CASE ('idTbry(ieast,diazFe3_ind)')
                idTbry(ieast,diazFe3_ind)=varid
              CASE ('idTbry(isouth,diazFe3_ind)')
                idTbry(isouth,diazFe3_ind)=varid
              CASE ('idTbry(inorth,diazFe3_ind)')
                idTbry(inorth,diazFe3_ind)=varid

              CASE ('idTbry(iwest,zooFe3_ind)')
                idTbry(iwest,zooFe3_ind)=varid
              CASE ('idTbry(ieast,zooFe3_ind)')
                idTbry(ieast,zooFe3_ind)=varid
              CASE ('idTbry(isouth,zooFe3_ind)')
                idTbry(isouth,zooFe3_ind)=varid
              CASE ('idTbry(inorth,zooFe3_ind)')
                idTbry(inorth,zooFe3_ind)=varid

! -- fe4 --
              CASE ('idTbry(iwest,fe4_ind)')
                idTbry(iwest,fe4_ind)=varid
              CASE ('idTbry(ieast,fe4_ind)')
                idTbry(ieast,fe4_ind)=varid
              CASE ('idTbry(isouth,fe4_ind)')
                idTbry(isouth,fe4_ind)=varid
              CASE ('idTbry(inorth,fe4_ind)')
                idTbry(inorth,fe4_ind)=varid

              CASE ('idTbry(iwest,dofe4_ind)')
                idTbry(iwest,dofe4_ind)=varid
              CASE ('idTbry(ieast,dofe4_ind)')
                idTbry(ieast,dofe4_ind)=varid
              CASE ('idTbry(isouth,dofe4_ind)')
                idTbry(isouth,dofe4_ind)=varid
              CASE ('idTbry(inorth,dofe4_ind)')
                idTbry(inorth,dofe4_ind)=varid

              CASE ('idTbry(iwest,spFe4_ind)')
                idTbry(iwest,spFe4_ind)=varid
              CASE ('idTbry(ieast,spFe4_ind)')
                idTbry(ieast,spFe4_ind)=varid
              CASE ('idTbry(isouth,spFe4_ind)')
                idTbry(isouth,spFe4_ind)=varid
              CASE ('idTbry(inorth,spFe4_ind)')
                idTbry(inorth,spFe4_ind)=varid

              CASE ('idTbry(iwest,diatFe4_ind)')
                idTbry(iwest,diatFe4_ind)=varid
              CASE ('idTbry(ieast,diatFe4_ind)')
                idTbry(ieast,diatFe4_ind)=varid
              CASE ('idTbry(isouth,diatFe4_ind)')
                idTbry(isouth,diatFe4_ind)=varid
              CASE ('idTbry(inorth,diatFe4_ind)')
                idTbry(inorth,diatFe4_ind)=varid

              CASE ('idTbry(iwest,diazFe4_ind)')
                idTbry(iwest,diazFe4_ind)=varid
              CASE ('idTbry(ieast,diazFe4_ind)')
                idTbry(ieast,diazFe4_ind)=varid
              CASE ('idTbry(isouth,diazFe4_ind)')
                idTbry(isouth,diazFe4_ind)=varid
              CASE ('idTbry(inorth,diazFe4_ind)')
                idTbry(inorth,diazFe4_ind)=varid

              CASE ('idTbry(iwest,zooFe4_ind)')
                idTbry(iwest,zooFe4_ind)=varid
              CASE ('idTbry(ieast,zooFe4_ind)')
                idTbry(ieast,zooFe4_ind)=varid
              CASE ('idTbry(isouth,zooFe4_ind)')
                idTbry(isouth,zooFe4_ind)=varid
              CASE ('idTbry(inorth,zooFe4_ind)')
                idTbry(inorth,zooFe4_ind)=varid

! -- fe5 --
              CASE ('idTbry(iwest,fe5_ind)')
                idTbry(iwest,fe5_ind)=varid
              CASE ('idTbry(ieast,fe5_ind)')
                idTbry(ieast,fe5_ind)=varid
              CASE ('idTbry(isouth,fe5_ind)')
                idTbry(isouth,fe5_ind)=varid
              CASE ('idTbry(inorth,fe5_ind)')
                idTbry(inorth,fe5_ind)=varid

              CASE ('idTbry(iwest,dofe5_ind)')
                idTbry(iwest,dofe5_ind)=varid
              CASE ('idTbry(ieast,dofe5_ind)')
                idTbry(ieast,dofe5_ind)=varid
              CASE ('idTbry(isouth,dofe5_ind)')
                idTbry(isouth,dofe5_ind)=varid
              CASE ('idTbry(inorth,dofe5_ind)')
                idTbry(inorth,dofe5_ind)=varid

              CASE ('idTbry(iwest,spFe5_ind)')
                idTbry(iwest,spFe5_ind)=varid
              CASE ('idTbry(ieast,spFe5_ind)')
                idTbry(ieast,spFe5_ind)=varid
              CASE ('idTbry(isouth,spFe5_ind)')
                idTbry(isouth,spFe5_ind)=varid
              CASE ('idTbry(inorth,spFe5_ind)')
                idTbry(inorth,spFe5_ind)=varid

              CASE ('idTbry(iwest,diatFe5_ind)')
                idTbry(iwest,diatFe5_ind)=varid
              CASE ('idTbry(ieast,diatFe5_ind)')
                idTbry(ieast,diatFe5_ind)=varid
              CASE ('idTbry(isouth,diatFe5_ind)')
                idTbry(isouth,diatFe5_ind)=varid
              CASE ('idTbry(inorth,diatFe5_ind)')
                idTbry(inorth,diatFe5_ind)=varid

              CASE ('idTbry(iwest,diazFe5_ind)')
                idTbry(iwest,diazFe5_ind)=varid
              CASE ('idTbry(ieast,diazFe5_ind)')
                idTbry(ieast,diazFe5_ind)=varid
              CASE ('idTbry(isouth,diazFe5_ind)')
                idTbry(isouth,diazFe5_ind)=varid
              CASE ('idTbry(inorth,diazFe5_ind)')
                idTbry(inorth,diazFe5_ind)=varid

              CASE ('idTbry(iwest,zooFe5_ind)')
                idTbry(iwest,zooFe5_ind)=varid
              CASE ('idTbry(ieast,zooFe5_ind)')
                idTbry(ieast,zooFe5_ind)=varid
              CASE ('idTbry(isouth,zooFe5_ind)')
                idTbry(isouth,zooFe5_ind)=varid
              CASE ('idTbry(inorth,zooFe5_ind)')
                idTbry(inorth,zooFe5_ind)=varid

! -- fe6 --
              CASE ('idTbry(iwest,fe6_ind)')
                idTbry(iwest,fe6_ind)=varid
              CASE ('idTbry(ieast,fe6_ind)')
                idTbry(ieast,fe6_ind)=varid
              CASE ('idTbry(isouth,fe6_ind)')
                idTbry(isouth,fe6_ind)=varid
              CASE ('idTbry(inorth,fe6_ind)')
                idTbry(inorth,fe6_ind)=varid

              CASE ('idTbry(iwest,dofe6_ind)')
                idTbry(iwest,dofe6_ind)=varid
              CASE ('idTbry(ieast,dofe6_ind)')
                idTbry(ieast,dofe6_ind)=varid
              CASE ('idTbry(isouth,dofe6_ind)')
                idTbry(isouth,dofe6_ind)=varid
              CASE ('idTbry(inorth,dofe6_ind)')
                idTbry(inorth,dofe6_ind)=varid

              CASE ('idTbry(iwest,spFe6_ind)')
                idTbry(iwest,spFe6_ind)=varid
              CASE ('idTbry(ieast,spFe6_ind)')
                idTbry(ieast,spFe6_ind)=varid
              CASE ('idTbry(isouth,spFe6_ind)')
                idTbry(isouth,spFe6_ind)=varid
              CASE ('idTbry(inorth,spFe6_ind)')
                idTbry(inorth,spFe6_ind)=varid

              CASE ('idTbry(iwest,diatFe6_ind)')
                idTbry(iwest,diatFe6_ind)=varid
              CASE ('idTbry(ieast,diatFe6_ind)')
                idTbry(ieast,diatFe6_ind)=varid
              CASE ('idTbry(isouth,diatFe6_ind)')
                idTbry(isouth,diatFe6_ind)=varid
              CASE ('idTbry(inorth,diatFe6_ind)')
                idTbry(inorth,diatFe6_ind)=varid

              CASE ('idTbry(iwest,diazFe6_ind)')
                idTbry(iwest,diazFe6_ind)=varid
              CASE ('idTbry(ieast,diazFe6_ind)')
                idTbry(ieast,diazFe6_ind)=varid
              CASE ('idTbry(isouth,diazFe6_ind)')
                idTbry(isouth,diazFe6_ind)=varid
              CASE ('idTbry(inorth,diazFe6_ind)')
                idTbry(inorth,diazFe6_ind)=varid

              CASE ('idTbry(iwest,zooFe6_ind)')
                idTbry(iwest,zooFe6_ind)=varid
              CASE ('idTbry(ieast,zooFe6_ind)')
                idTbry(ieast,zooFe6_ind)=varid
              CASE ('idTbry(isouth,zooFe6_ind)')
                idTbry(isouth,zooFe6_ind)=varid
              CASE ('idTbry(inorth,zooFe6_ind)')
                idTbry(inorth,zooFe6_ind)=varid

! -- fe7 --
              CASE ('idTbry(iwest,fe7_ind)')
                idTbry(iwest,fe7_ind)=varid
              CASE ('idTbry(ieast,fe7_ind)')
                idTbry(ieast,fe7_ind)=varid
              CASE ('idTbry(isouth,fe7_ind)')
                idTbry(isouth,fe7_ind)=varid
              CASE ('idTbry(inorth,fe7_ind)')
                idTbry(inorth,fe7_ind)=varid

              CASE ('idTbry(iwest,dofe7_ind)')
                idTbry(iwest,dofe7_ind)=varid
              CASE ('idTbry(ieast,dofe7_ind)')
                idTbry(ieast,dofe7_ind)=varid
              CASE ('idTbry(isouth,dofe7_ind)')
                idTbry(isouth,dofe7_ind)=varid
              CASE ('idTbry(inorth,dofe7_ind)')
                idTbry(inorth,dofe7_ind)=varid

              CASE ('idTbry(iwest,spFe7_ind)')
                idTbry(iwest,spFe7_ind)=varid
              CASE ('idTbry(ieast,spFe7_ind)')
                idTbry(ieast,spFe7_ind)=varid
              CASE ('idTbry(isouth,spFe7_ind)')
                idTbry(isouth,spFe7_ind)=varid
              CASE ('idTbry(inorth,spFe7_ind)')
                idTbry(inorth,spFe7_ind)=varid

              CASE ('idTbry(iwest,diatFe7_ind)')
                idTbry(iwest,diatFe7_ind)=varid
              CASE ('idTbry(ieast,diatFe7_ind)')
                idTbry(ieast,diatFe7_ind)=varid
              CASE ('idTbry(isouth,diatFe7_ind)')
                idTbry(isouth,diatFe7_ind)=varid
              CASE ('idTbry(inorth,diatFe7_ind)')
                idTbry(inorth,diatFe7_ind)=varid

              CASE ('idTbry(iwest,diazFe7_ind)')
                idTbry(iwest,diazFe7_ind)=varid
              CASE ('idTbry(ieast,diazFe7_ind)')
                idTbry(ieast,diazFe7_ind)=varid
              CASE ('idTbry(isouth,diazFe7_ind)')
                idTbry(isouth,diazFe7_ind)=varid
              CASE ('idTbry(inorth,diazFe7_ind)')
                idTbry(inorth,diazFe7_ind)=varid

              CASE ('idTbry(iwest,zooFe7_ind)')
                idTbry(iwest,zooFe7_ind)=varid
              CASE ('idTbry(ieast,zooFe7_ind)')
                idTbry(ieast,zooFe7_ind)=varid
              CASE ('idTbry(isouth,zooFe7_ind)')
                idTbry(isouth,zooFe7_ind)=varid
              CASE ('idTbry(inorth,zooFe7_ind)')
                idTbry(inorth,zooFe7_ind)=varid
#endif

#if defined CISO
              CASE ('idTbry(iwest,di13c_ind)')
                idTbry(iwest,di13c_ind)=varid
              CASE ('idTbry(ieast,di13c_ind)')
                idTbry(ieast,di13c_ind)=varid
              CASE ('idTbry(isouth,di13c_ind)')
                idTbry(isouth,di13c_ind)=varid
              CASE ('idTbry(inorth,di13c_ind)')
                idTbry(inorth,di13c_ind)=varid

              CASE ('idTbry(iwest,do13c_ind)')
                idTbry(iwest,do13c_ind)=varid
              CASE ('idTbry(ieast,do13c_ind)')
                idTbry(ieast,do13c_ind)=varid
              CASE ('idTbry(isouth,do13c_ind)')
                idTbry(isouth,do13c_ind)=varid
              CASE ('idTbry(inorth,do13c_ind)')
                idTbry(inorth,do13c_ind)=varid

              CASE ('idTbry(iwest,zoo13C_ind)')
                idTbry(iwest,zoo13C_ind)=varid
              CASE ('idTbry(ieast,zoo13C_ind)')
                idTbry(ieast,zoo13C_ind)=varid
              CASE ('idTbry(isouth,zoo13C_ind)')
                idTbry(isouth,zoo13C_ind)=varid
              CASE ('idTbry(inorth,zoo13C_ind)')
                idTbry(inorth,zoo13C_ind)=varid

              CASE ('idTbry(iwest,di14c_ind)')
                idTbry(iwest,di14c_ind)=varid
              CASE ('idTbry(ieast,di14c_ind)')
                idTbry(ieast,di14c_ind)=varid
              CASE ('idTbry(isouth,di14c_ind)')
                idTbry(isouth,di14c_ind)=varid
              CASE ('idTbry(inorth,di14c_ind)')
                idTbry(inorth,di14c_ind)=varid

              CASE ('idTbry(iwest,do14c_ind)')
                idTbry(iwest,do14c_ind)=varid
              CASE ('idTbry(ieast,do14c_ind)')
                idTbry(ieast,do14c_ind)=varid
              CASE ('idTbry(isouth,do14c_ind)')
                idTbry(isouth,do14c_ind)=varid
              CASE ('idTbry(inorth,do14c_ind)')
                idTbry(inorth,do14c_ind)=varid

              CASE ('idTbry(iwest,zoo14C_ind)')
                idTbry(iwest,zoo14C_ind)=varid
              CASE ('idTbry(ieast,zoo14C_ind)')
                idTbry(ieast,zoo14C_ind)=varid
              CASE ('idTbry(isouth,zoo14C_ind)')
                idTbry(isouth,zoo14C_ind)=varid
              CASE ('idTbry(inorth,zoo14C_ind)')
                idTbry(inorth,zoo14C_ind)=varid

              CASE ('idTbry(iwest,spC13_ind)')
                idTbry(iwest,spC13_ind)=varid
              CASE ('idTbry(ieast,spC13_ind)')
                idTbry(ieast,spC13_ind)=varid
              CASE ('idTbry(isouth,spC13_ind)')
                idTbry(isouth,spC13_ind)=varid
              CASE ('idTbry(inorth,spC13_ind)')
                idTbry(inorth,spC13_ind)=varid

              CASE ('idTbry(iwest,spC14_ind)')
                idTbry(iwest,spC14_ind)=varid
              CASE ('idTbry(ieast,spC14_ind)')
                idTbry(ieast,spC14_ind)=varid
              CASE ('idTbry(isouth,spC14_ind)')
                idTbry(isouth,spC14_ind)=varid
              CASE ('idTbry(inorth,spC14_ind)')
                idTbry(inorth,spC14_ind)=varid

              CASE ('idTbry(iwest,spCa13CO3_ind)')
                idTbry(iwest,spCa13CO3_ind)=varid
              CASE ('idTbry(ieast,spCa13CO3_ind)')
                idTbry(ieast,spCa13CO3_ind)=varid
              CASE ('idTbry(isouth,spCa13CO3_ind)')
                idTbry(isouth,spCa13CO3_ind)=varid
              CASE ('idTbry(inorth,spCa13CO3_ind)')
                idTbry(inorth,spCa13CO3_ind)=varid

              CASE ('idTbry(iwest,spCa14CO3_ind)')
                idTbry(iwest,spCa14CO3_ind)=varid
              CASE ('idTbry(ieast,spCa14CO3_ind)')
                idTbry(ieast,spCa14CO3_ind)=varid
              CASE ('idTbry(isouth,spCa14CO3_ind)')
                idTbry(isouth,spCa14CO3_ind)=varid
              CASE ('idTbry(inorth,spCa14CO3_ind)')
                idTbry(inorth,spCa14CO3_ind)=varid

              CASE ('idTbry(iwest,diatC13_ind)')
                idTbry(iwest,diatC13_ind)=varid
              CASE ('idTbry(ieast,diatC13_ind)')
                idTbry(ieast,diatC13_ind)=varid
              CASE ('idTbry(isouth,diatC13_ind)')
                idTbry(isouth,diatC13_ind)=varid
              CASE ('idTbry(inorth,diatC13_ind)')
                idTbry(inorth,diatC13_ind)=varid

              CASE ('idTbry(iwest,diatC14_ind)')
                idTbry(iwest,diatC14_ind)=varid
              CASE ('idTbry(ieast,diatC14_ind)')
                idTbry(ieast,diatC14_ind)=varid
              CASE ('idTbry(isouth,diatC14_ind)')
                idTbry(isouth,diatC14_ind)=varid
              CASE ('idTbry(inorth,diatC14_ind)')
                idTbry(inorth,diatC14_ind)=varid

              CASE ('idTbry(iwest,diazC13_ind)')
                idTbry(iwest,diazC13_ind)=varid
              CASE ('idTbry(ieast,diazC13_ind)')
                idTbry(ieast,diazC13_ind)=varid
              CASE ('idTbry(isouth,diazC13_ind)')
                idTbry(isouth,diazC13_ind)=varid
              CASE ('idTbry(inorth,diazC13_ind)')
                idTbry(inorth,diazC13_ind)=varid

              CASE ('idTbry(iwest,diazC14_ind)')
                idTbry(iwest,diazC14_ind)=varid
              CASE ('idTbry(ieast,diazC14_ind)')
                idTbry(ieast,diazC14_ind)=varid
              CASE ('idTbry(isouth,diazC14_ind)')
                idTbry(isouth,diazC14_ind)=varid
              CASE ('idTbry(inorth,diazC14_ind)')
                idTbry(inorth,diazC14_ind)=varid
#endif

#ifdef TS_PSOURCE

/*
**  Biological tracers point Source/Sinks (river runoff).
*/


              CASE ('idRtrc(po4_ind)')
                idRtrc(po4_ind)=varid
              CASE ('idRtrc(no3_ind)')
                idRtrc(no3_ind)=varid
              CASE ('idRtrc(sio3_ind)')
                idRtrc(sio3_ind)=varid
              CASE ('idRtrc(nh4_ind)')
                idRtrc(nh4_ind)=varid
              CASE ('idRtrc(o2_ind)')
                idRtrc(o2_ind)=varid
              CASE ('idRtrc(dic_ind)')
                idRtrc(dic_ind)=varid
              CASE ('idRtrc(dic_alt_co2_ind)')
                idRtrc(dic_alt_co2_ind)=varid
              CASE ('idRtrc(alk_ind)')
                idRtrc(alk_ind)=varid
              CASE ('idRtrc(doc_ind)')
                idRtrc(doc_ind)=varid
              CASE ('idRtrc(don_ind)')
                idRtrc(don_ind)=varid
              CASE ('idRtrc(dop_ind)')
                idRtrc(dop_ind)=varid
              CASE ('idRtrc(dopr_ind)')
                idRtrc(dopr_ind)=varid
              CASE ('idRtrc(donr_ind)')
                idRtrc(donr_ind)=varid
              CASE ('idRtrc(zooC_ind)')
                idRtrc(zooC_ind)=varid
              CASE ('idRtrc(spChl_ind)')
                idRtrc(spChl_ind)=varid
              CASE ('idRtrc(spC_ind)')
                idRtrc(spC_ind)=varid
              CASE ('idRtrc(spCaCO3_ind)')
                idRtrc(spCaCO3_ind)=varid
              CASE ('idRtrc(diatChl_ind)')
                idRtrc(diatChl_ind)=varid
              CASE ('idRtrc(diatC_ind)')
                idRtrc(diatC_ind)=varid
              CASE ('idRtrc(diatSi_ind)')
                idRtrc(diatSi_ind)=varid
              CASE ('idRtrc(diazChl_ind)')
                idRtrc(diazChl_ind)=varid
              CASE ('idRtrc(diazC_ind)')
                idRtrc(diazC_ind)=varid

# ifndef FE_TAG
              CASE ('idRtrc(fe_ind)')
                idRtrc(fe_ind)=varid
              CASE ('idRtrc(dofe_ind)')
                idRtrc(dofe_ind)=varid
              CASE ('idRtrc(spFe_ind)')
                idRtrc(spFe_ind)=varid
              CASE ('idRtrc(diatFe_ind)')
                idRtrc(diatFe_ind)=varid
              CASE ('idRtrc(diazFe_ind)')
                idRtrc(diazFe_ind)=varid
              CASE ('idRtrc(zooFe_ind)')
                idRtrc(zooFe_ind)=varid
# else
              CASE ('idRtrc(fe0_ind)')
                idRtrc(fe0_ind)=varid
              CASE ('idRtrc(dofe0_ind)')
                idRtrc(dofe0_ind)=varid
              CASE ('idRtrc(spFe0_ind)')
                idRtrc(spFe0_ind)=varid
              CASE ('idRtrc(diatFe0_ind)')
                idRtrc(diatFe0_ind)=varid
              CASE ('idRtrc(diazFe0_ind)')
                idRtrc(diazFe0_ind)=varid
              CASE ('idRtrc(zooFe0_ind)')
                idRtrc(zooFe0_ind)=varid

              CASE ('idRtrc(fe1_ind)')
                idRtrc(fe1_ind)=varid
              CASE ('idRtrc(dofe1_ind)')
                idRtrc(dofe1_ind)=varid
              CASE ('idRtrc(spFe1_ind)')
                idRtrc(spFe1_ind)=varid
              CASE ('idRtrc(diatFe1_ind)')
                idRtrc(diatFe1_ind)=varid
              CASE ('idRtrc(diazFe1_ind)')
                idRtrc(diazFe1_ind)=varid
              CASE ('idRtrc(zooFe1_ind)')
                idRtrc(zooFe1_ind)=varid

              CASE ('idRtrc(fe2_ind)')
                idRtrc(fe2_ind)=varid
              CASE ('idRtrc(dofe2_ind)')
                idRtrc(dofe2_ind)=varid
              CASE ('idRtrc(spFe2_ind)')
                idRtrc(spFe2_ind)=varid
              CASE ('idRtrc(diatFe2_ind)')
                idRtrc(diatFe2_ind)=varid
              CASE ('idRtrc(diazFe2_ind)')
                idRtrc(diazFe2_ind)=varid
              CASE ('idRtrc(zooFe2_ind)')
                idRtrc(zooFe2_ind)=varid

              CASE ('idRtrc(fe3_ind)')
                idRtrc(fe3_ind)=varid
              CASE ('idRtrc(dofe3_ind)')
                idRtrc(dofe3_ind)=varid
              CASE ('idRtrc(spFe3_ind)')
                idRtrc(spFe3_ind)=varid
              CASE ('idRtrc(diatFe3_ind)')
                idRtrc(diatFe3_ind)=varid
              CASE ('idRtrc(diazFe3_ind)')
                idRtrc(diazFe3_ind)=varid
              CASE ('idRtrc(zooFe3_ind)')
                idRtrc(zooFe3_ind)=varid

              CASE ('idRtrc(fe4_ind)')
                idRtrc(fe4_ind)=varid
              CASE ('idRtrc(dofe4_ind)')
                idRtrc(dofe4_ind)=varid
              CASE ('idRtrc(spFe4_ind)')
                idRtrc(spFe4_ind)=varid
              CASE ('idRtrc(diatFe4_ind)')
                idRtrc(diatFe4_ind)=varid
              CASE ('idRtrc(diazFe4_ind)')
                idRtrc(diazFe4_ind)=varid
              CASE ('idRtrc(zooFe4_ind)')
                idRtrc(zooFe4_ind)=varid

              CASE ('idRtrc(fe5_ind)')
                idRtrc(fe5_ind)=varid
              CASE ('idRtrc(dofe5_ind)')
                idRtrc(dofe5_ind)=varid
              CASE ('idRtrc(spFe5_ind)')
                idRtrc(spFe5_ind)=varid
              CASE ('idRtrc(diatFe5_ind)')
                idRtrc(diatFe5_ind)=varid
              CASE ('idRtrc(diazFe5_ind)')
                idRtrc(diazFe5_ind)=varid
              CASE ('idRtrc(zooFe5_ind)')
                idRtrc(zooFe5_ind)=varid

              CASE ('idRtrc(fe6_ind)')
                idRtrc(fe6_ind)=varid
              CASE ('idRtrc(dofe6_ind)')
                idRtrc(dofe6_ind)=varid
              CASE ('idRtrc(spFe6_ind)')
                idRtrc(spFe6_ind)=varid
              CASE ('idRtrc(diatFe6_ind)')
                idRtrc(diatFe6_ind)=varid
              CASE ('idRtrc(diazFe6_ind)')
                idRtrc(diazFe6_ind)=varid
              CASE ('idRtrc(zooFe6_ind)')
                idRtrc(zooFe6_ind)=varid

              CASE ('idRtrc(fe7_ind)')
                idRtrc(fe7_ind)=varid
              CASE ('idRtrc(dofe7_ind)')
                idRtrc(dofe7_ind)=varid
              CASE ('idRtrc(spFe7_ind)')
                idRtrc(spFe7_ind)=varid
              CASE ('idRtrc(diatFe7_ind)')
                idRtrc(diatFe7_ind)=varid
              CASE ('idRtrc(diazFe7_ind)')
                idRtrc(diazFe7_ind)=varid
              CASE ('idRtrc(zooFe7_ind)')
                idRtrc(zooFe7_ind)=varid
# endif
# if defined CISO
              CASE ('idRtrc(di13c_ind)')
                idRtrc(di13c_ind)=varid
              CASE ('idRtrc(do13c_ind)')
                idRtrc(do13c_ind)=varid
              CASE ('idRtrc(zoo13C_ind)')
                idRtrc(zoo13C_ind)=varid
              CASE ('idRtrc(di14c_ind)')
                idRtrc(di14c_ind)=varid
              CASE ('idRtrc(do14c_ind)')
                idRtrc(do14c_ind)=varid
              CASE ('idRtrc(zoo14C_ind)')
                idRtrc(zoo14C_ind)=varid
              CASE ('idRtrc(spC13_ind)')
                idRtrc(spC13_ind)=varid
              CASE ('idRtrc(spC14_ind)')
                idRtrc(spC14_ind)=varid
              CASE ('idRtrc(spCa13CO3_ind)')
                idRtrc(spCa13CO3_ind)=varid
              CASE ('idRtrc(spCa14CO3_ind)')
                idRtrc(spCa14CO3_ind)=varid
              CASE ('idRtrc(diatC13_ind)')
                idRtrc(diatC13_ind)=varid
              CASE ('idRtrc(diatC14_ind)')
                idRtrc(diatC14_ind)=varid
              CASE ('idRtrc(diazC13_ind)')
                idRtrc(diazC13_ind)=varid
              CASE ('idRtrc(diazC14_ind)')
                idRtrc(diazC14_ind)=varid
# endif
#endif

#ifdef LIGAND3D

              CASE ('idTlig')
                idTlig=varid

#endif

#ifdef DICE

              CASE ('idDice')
                idDice=varid

#endif

#ifdef DUST

              CASE ('idDust')
                idDust=varid

#endif

#ifdef DIAGNOSTICS_BIO
              CASE ('iDbio3(iphotoC_sp)')
                iDbio3(iphotoC_sp)=varid
              CASE ('iDbio3(iphotoC_diat)')
                iDbio3(iphotoC_diat)=varid
              CASE ('iDbio3(iphotoC_diaz)')
                iDbio3(iphotoC_diaz)=varid

              CASE ('iDbio3(ilight_lim_sp)')
                iDbio3(ilight_lim_sp)=varid
              CASE ('iDbio3(ilight_lim_diat)')
                iDbio3(ilight_lim_diat)=varid
              CASE ('iDbio3(ilight_lim_diaz)')
                iDbio3(ilight_lim_diaz)=varid

              CASE ('iDbio3(iVNtot_sp)')
                iDbio3(iVNtot_sp)=varid
              CASE ('iDbio3(iVFe_sp)')
                iDbio3(iVFe_sp)=varid
              CASE ('iDbio3(iVPtot_sp)')
                iDbio3(iVPtot_sp)=varid

              CASE ('iDbio3(iVNtot_diat)')
                iDbio3(iVNtot_diat)=varid
              CASE ('iDbio3(iVFe_diat)')
                iDbio3(iVFe_diat)=varid
              CASE ('iDbio3(iVPtot_diat)')
                iDbio3(iVPtot_diat)=varid
              CASE ('iDbio3(iVSiO3_diat)')
                iDbio3(iVSiO3_diat)=varid

              CASE ('iDbio3(iVNtot_diaz)')
                iDbio3(iVNtot_diaz)=varid
              CASE ('iDbio3(iVFe_diaz)')
                iDbio3(iVFe_diaz)=varid
              CASE ('iDbio3(iVPtot_diaz)')
                iDbio3(iVPtot_diaz)=varid
# ifndef FE_TAG
              CASE ('iDbio3(iFe_brate)')
                iDbio3(iFe_brate)=varid
              CASE ('iDbio3(iFe_scav)')
                iDbio3(iFe_scav)=varid
              CASE ('iDbio3(iFe_disag)')
                iDbio3(iFe_disag)=varid
              CASE ('iDbio3(iFe_pgen)')
                iDbio3(iFe_pgen)=varid
              CASE ('iDbio3(iFe_premin)')
                iDbio3(iFe_premin)=varid
              CASE ('iDbio3(iFe_hbio)')
                iDbio3(iFe_hbio)=varid
#else
              CASE ('iDbio3(iFe0_brate)')
                iDbio3(iFe0_brate)=varid
              CASE ('iDbio3(iFe0_scav)')
                iDbio3(iFe0_scav)=varid
              CASE ('iDbio3(iFe0_disag)')
                iDbio3(iFe0_disag)=varid
              CASE ('iDbio3(iFe0_pgen)')
                iDbio3(iFe0_pgen)=varid
              CASE ('iDbio3(iFe0_premin)')
                iDbio3(iFe0_premin)=varid
              CASE ('iDbio3(iFe0_hbio)')
                iDbio3(iFe0_hbio)=varid

              CASE ('iDbio3(iFe1_brate)')
                iDbio3(iFe1_brate)=varid
              CASE ('iDbio3(iFe1_scav)')
                iDbio3(iFe1_scav)=varid
              CASE ('iDbio3(iFe1_disag)')
                iDbio3(iFe1_disag)=varid
              CASE ('iDbio3(iFe1_pgen)')
                iDbio3(iFe1_pgen)=varid
              CASE ('iDbio3(iFe1_premin)')
                iDbio3(iFe1_premin)=varid
              CASE ('iDbio3(iFe1_hbio)')
                iDbio3(iFe1_hbio)=varid

              CASE ('iDbio3(iFe2_brate)')
                iDbio3(iFe2_brate)=varid
              CASE ('iDbio3(iFe2_scav)')
                iDbio3(iFe2_scav)=varid
              CASE ('iDbio3(iFe2_disag)')
                iDbio3(iFe2_disag)=varid
              CASE ('iDbio3(iFe2_pgen)')
                iDbio3(iFe2_pgen)=varid
              CASE ('iDbio3(iFe2_premin)')
                iDbio3(iFe2_premin)=varid
              CASE ('iDbio3(iFe2_hbio)')
                iDbio3(iFe2_hbio)=varid

              CASE ('iDbio3(iFe3_brate)')
                iDbio3(iFe3_brate)=varid
              CASE ('iDbio3(iFe3_scav)')
                iDbio3(iFe3_scav)=varid
              CASE ('iDbio3(iFe3_disag)')
                iDbio3(iFe3_disag)=varid
              CASE ('iDbio3(iFe3_pgen)')
                iDbio3(iFe3_pgen)=varid
              CASE ('iDbio3(iFe3_premin)')
                iDbio3(iFe3_premin)=varid
              CASE ('iDbio3(iFe3_hbio)')
                iDbio3(iFe3_hbio)=varid

              CASE ('iDbio3(iFe4_brate)')
                iDbio3(iFe4_brate)=varid
              CASE ('iDbio3(iFe4_scav)')
                iDbio3(iFe4_scav)=varid
              CASE ('iDbio3(iFe4_disag)')
                iDbio3(iFe4_disag)=varid
              CASE ('iDbio3(iFe4_pgen)')
                iDbio3(iFe4_pgen)=varid
              CASE ('iDbio3(iFe4_premin)')
                iDbio3(iFe4_premin)=varid
              CASE ('iDbio3(iFe4_hbio)')
                iDbio3(iFe4_hbio)=varid

              CASE ('iDbio3(iFe5_brate)')
                iDbio3(iFe5_brate)=varid
              CASE ('iDbio3(iFe5_scav)')
                iDbio3(iFe5_scav)=varid
              CASE ('iDbio3(iFe5_disag)')
                iDbio3(iFe5_disag)=varid
              CASE ('iDbio3(iFe5_pgen)')
                iDbio3(iFe5_pgen)=varid
              CASE ('iDbio3(iFe5_premin)')
                iDbio3(iFe5_premin)=varid
              CASE ('iDbio3(iFe5_hbio)')
                iDbio3(iFe5_hbio)=varid

              CASE ('iDbio3(iFe6_brate)')
                iDbio3(iFe6_brate)=varid
              CASE ('iDbio3(iFe6_scav)')
                iDbio3(iFe6_scav)=varid
              CASE ('iDbio3(iFe6_disag)')
                iDbio3(iFe6_disag)=varid
              CASE ('iDbio3(iFe6_pgen)')
                iDbio3(iFe6_pgen)=varid
              CASE ('iDbio3(iFe6_premin)')
                iDbio3(iFe6_premin)=varid
              CASE ('iDbio3(iFe6_hbio)')
                iDbio3(iFe6_hbio)=varid

              CASE ('iDbio3(iFe7_brate)')
                iDbio3(iFe7_brate)=varid
              CASE ('iDbio3(iFe7_scav)')
                iDbio3(iFe7_scav)=varid
              CASE ('iDbio3(iFe7_disag)')
                iDbio3(iFe7_disag)=varid
              CASE ('iDbio3(iFe7_pgen)')
                iDbio3(iFe7_pgen)=varid
              CASE ('iDbio3(iFe7_premin)')
                iDbio3(iFe7_premin)=varid
              CASE ('iDbio3(iFe7_hbio)')
                iDbio3(iFe7_hbio)=varid
# endif

              CASE ('iDbio2(ipCO2)')
                iDbio2(ipCO2)=varid
              CASE ('iDbio2(ipH)')
                iDbio2(ipH)=varid
              CASE ('iDbio2(iCO2_flux)')
                iDbio2(iCO2_flux)=varid


# if defined CISO
              CASE ('iDbio3(iDIC_d13C)')
                iDbio3(iDIC_d13C)=varid
              CASE ('iDbio3(iDIC_d14C)')
                iDbio3(iDIC_d14C)=varid
# endif
#endif
