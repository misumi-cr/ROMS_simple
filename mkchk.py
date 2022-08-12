#!/usr/bin/python

import os
import re

def line_strip1(l,n):
  l = l.rstrip()
  l = l.split("=")
  l = l[n].strip()
  l = l.split("!")
  l = l[0].strip()
#  if n == 1:
#    l = l.lower()
#    l = l.replace('d','e')
  return l

def line_strip2(l,n):
  l = l.rstrip()
  l = l.split("==")
  l = l[n].strip()
  l = l.split("!")
  l = l[0].strip()
#  if n == 1:
#    l = l.lower()
#    l = l.replace('d','e')
  return l

def conv_exp(val):
  val = val.lower()
  val = val.replace('d','e')
  val = float(val)
  return val

case_name = os.environ['CASE_NAME']
r_num = os.environ['R_NUM']

infile=case_name+'_'+r_num+'.in'

fi0 = open(infile,'r')

for line in fi0:
  if not re.match(r'^!',line):
    if 'TITLE' in line:
      item = line_strip1(line,0)
      val  = line_strip1(line,1)
      print "{0:<12}: {1:<12}".format(item, val)
    if 'NtileI' in line or 'NtileJ' in line or 'NTIMES' in line or 'DT' in line or 'NDTFAST' in line or 'NRST' in line or 'NINFO' in line or 'NHIS' in line or 'NAVG' in line or 'NDIA' in line or 'TNU2' in line or 'VISC' in line or 'AKT' in line or 'AKV' in line or 'NUDG' in line or 'RSTNAME' in line or 'HISNAME' in line or 'AVGNAME' in line:
      item = line_strip2(line,0)
      val  = line_strip2(line,1)
      print "{0:<12}: {1:<12}".format(item, val)
      if item == 'NTIMES':
        ntimes = conv_exp(val)
      if item == 'DT':
        dt = conv_exp(val)
      if item == 'NRST':
        nrst = conv_exp(val)
      if item == 'NHIS':
        nhis = conv_exp(val)
      if item == 'NAVG':
        navg = conv_exp(val)
      if item == 'NDIA':
        ndia = conv_exp(val)


ndays = ntimes * dt / 86400
nrst  = nrst   * dt / 86400
nhis  = nhis   * dt / 86400
ndia  = ndia   * dt / 86400

print
print "{0:<12}: {1:>12} {2:<12}".format('Sim. Period', ndays, 'days')
print "{0:<12}: {1:>12} {2:<12}".format('Restart'    , nrst , 'days')
print "{0:<12}: {1:>12} {2:<12}".format('Histroy'    , nhis , 'days')
print "{0:<12}: {1:>12} {2:<12}".format('Diag   '    , ndia , 'days')

fi0.close()
