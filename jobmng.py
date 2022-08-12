#!/criepi/ap/anaconda/3/bin/python
import time
import subprocess
import os
import re

def JobnGet():
  with open("build.config.sgi8600") as fi0:
    while True:
      line=fi0.readline()
      if not line:
        break
      line=line.rstrip("\n")
      xx=re.compile(r"^expo.*JOB\_NAME.*")
      if xx.match(line):
        x0=line.split("=")[-1]
        x0=x0.replace("\"","")
        return x0

def QcntGet(JobName):
  p0=subprocess.Popen(["qstat"],stdout=subprocess.PIPE,shell=True)
  Qcnt=0
  while True:
    line=p0.stdout.readline().decode("utf-8")
    if not line:
      break
    line=line.rstrip("\n")
    xx=re.compile(r".*{0}.*".format(JobName))
    if xx.match(line):
      Qcnt=Qcnt+1
  return Qcnt
  
while True:
  JobName=JobnGet()
  Qcnt=QcntGet(JobName)
  if Qcnt<2:
    subprocess.Popen(["./go.sgi8600"],shell=True)
  time.sleep(3600)
