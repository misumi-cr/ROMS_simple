#!/usr/bin/python

import os
import sys
import re

def error_exit(msg):
  print "Error: "+msg
  sys.exit()

def mod_file(line,file):
  x = line.split("==")
  x[1] = " "+file
  line = "==".join(x)
  line = line + "\n"
  return line

def mod_ntimes(line,val):
  x = line.split("==")
  x[1] = " "+val
  line = "==".join(x)
  line = line + "\n"
  return line

def mod_dt(line,val):
  x = line.split("==")
  x[1] = " "+val
  line = "==".join(x)
  line = line + "\n"
  return line

def mod_dstart(line,val):
  x = line.split("=")
  y = x[1].split("!")
  x[1] = float(y[0]) + float(val)
  x[1] = " "+str(x[1])+" ! "+y[1]
  line = "=".join(x)
  return line

case_name = os.environ['CASE_NAME']
r_num = os.environ['R_NUM']
p_num = int(r_num)-1
p_num = str(p_num)
p_num = p_num.zfill(3)
ntimes = os.environ['D_NTIMES']
dt = os.environ['DT']
d_dstart = os.environ['D_DSTART']
r_ntimes = os.environ['R_NTIMES']
h_ntimes = os.environ['H_NTIMES']
i_ntimes = os.environ['I_NTIMES']

inifile = os.environ['WORK_DIR'] + "/out/" + case_name + ".r." + p_num + ".nc"
rstfile = os.environ['WORK_DIR'] + "/out/" + case_name + ".r." + r_num + ".nc"
hstfile = os.environ['WORK_DIR'] + "/out/" + case_name + ".h." + r_num + ".nc"
avgfile = os.environ['WORK_DIR'] + "/out/" + case_name + ".a." + r_num + ".nc"
diafile = os.environ['WORK_DIR'] + "/out/" + case_name + ".d." + r_num + ".nc"
stafile = os.environ['WORK_DIR'] + "/out/" + case_name + ".s." + r_num + ".nc"

infile =case_name+'_'+p_num+'.in'
outfile=case_name+'_'+r_num+'.in'

if not os.path.isfile(infile):
  msg = infile+" does not exist."
  error_exit(msg)
if os.path.isfile(outfile):
  msg = outfile+" has already exist."
  error_exit(msg)

fi0 = open(infile ,'r')
fo0 = open(outfile,'w')

for line in fi0:
  if not re.match(r'^!',line):
    if   re.match(r'^\s*ININAME\s*==',line):
      line = mod_file(line, inifile)
    elif re.match(r'^\s*RSTNAME\s*==',line):
      line = mod_file(line, rstfile)
    elif re.match(r'^\s*HISNAME\s*==',line):
      line = mod_file(line, hstfile)
    elif re.match(r'^\s*DIANAME\s*==',line):
      line = mod_file(line, diafile)
    elif re.match(r'^\s*AVGNAME\s*==',line):
      line = mod_file(line, avgfile)
    elif re.match(r'^\s*STANAME\s*==',line):
      line = mod_file(line, stafile)
    elif re.match(r'^\s*NTIMES\s*==',line):
      line = mod_ntimes(line, ntimes)
    elif re.match(r'^\s*NRST\s*==',line):
      line = mod_ntimes(line, r_ntimes) 
    elif re.match(r'^\s*NHIS\s*==',line):
      line = mod_ntimes(line, h_ntimes) 
    elif re.match(r'^\s*NAVG\s*==',line):
      line = mod_ntimes(line, h_ntimes) 
    elif re.match(r'^\s*NDIA\s*==',line):
      line = mod_ntimes(line, h_ntimes) 
    elif re.match(r'^\s*NINFO\s*==',line):
      line = mod_ntimes(line, i_ntimes) 
    elif re.match(r'^\s*DT\s*==',line):
      line = mod_dt(line, dt)
    elif re.match(r'^\s*DSTART\s*=',line):
      line = mod_dstart(line, d_dstart)
    elif re.match(r'^\s*NRREC\s*==',line):
      line = mod_file(line, "-1")
  fo0.write(line)
  
fi0.close()
fo0.close()
