#!/usr/bin/python

import os

case_name = os.environ['CASE_NAME']
r_num     = os.environ['R_NUM']
ntimes    = os.environ['D_NTIMES']
h_ntimes  = os.environ['H_NTIMES']
r_ntimes  = os.environ['R_NTIMES']
i_ntimes  = os.environ['I_NTIMES']
s_ntimes  = os.environ['S_NTIMES']
dt        = os.environ['DT']
r_dat     = os.environ['ROMS_DAT_DIR']
w_dir     = os.environ['WORK_DIR']
d_dstart  = os.environ['D_DSTART']

outfile=case_name+'_'+r_num+'.in'

fi0 = open('template.in','r')
fo0 = open(outfile,'a')

for line in fi0:
  line = line.replace('${CASE_NAME}'   ,case_name)
  line = line.replace('${R_NUM}'       ,r_num)
  line = line.replace('${NTIMES}'      ,ntimes)
  line = line.replace('${H_NTIMES}'    ,h_ntimes)
  line = line.replace('${R_NTIMES}'    ,r_ntimes)
  line = line.replace('${I_NTIMES}'    ,i_ntimes)
  line = line.replace('${S_NTIMES}'    ,s_ntimes)
  line = line.replace('${DT}'          ,dt)
  line = line.replace('${ROMS_DAT_DIR}',r_dat)
  line = line.replace('${WORK_DIR}'    ,w_dir)
  line = line.replace('${D_DSTART}'    ,'0.0')
  fo0.write(line)
  
fi0.close()
fo0.close()
