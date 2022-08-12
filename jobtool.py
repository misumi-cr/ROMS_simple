import os
import subprocess
import re
import numpy as np
import sys

args = sys.argv
envvars=os.environ

JobName=envvars["JOB_NAME"]
CaseName=envvars["CASE_NAME"]

def RunJobGet():
  p0=subprocess.Popen(["qstat"],stdout=subprocess.PIPE,shell=True)
  CaseDic={}
  while True:
    line=p0.stdout.readline().decode("utf-8")
    if not line:
      break
    xx=re.compile(r".*{0}.*".format(JobName))
    if xx.match(line):
      line=line.rstrip("\n")
      x0=line.split()
      k0=int(x0[0].split(".")[0])
      v0=int(x0[1].split("_")[-1])
      CaseDic[k0]=v0
  return CaseDic

def LastFnumGet():
  p1=subprocess.Popen(["ls {0}*.in".format(CaseName)],stdout=subprocess.PIPE,shell=True)
  NumList=[]
  while True:
    line=p1.stdout.readline().decode("utf-8")
    if not line:
      break
    x0=line.split(".")[-2]
    NumList.append(int(x0.split("_")[-1]))
  return np.max(np.array(NumList))

RunJobs=RunJobGet()    # {Process Nums: Run Nums}
LastFnum=LastFnumGet() # Last run number

if args[1]=="pjob":
  if len(RunJobs)==0:
    print("")
  else:
    print(np.max(list(RunJobs.keys())))
elif args[1]=="rnum":
  if len(RunJobs)==0:
    print("{0:0>3}".format(LastFnum+1))
  else:
    print("{0:0>3}".format(np.max( list(RunJobs.values()))+1 ))
