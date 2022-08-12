FC    = ifort
#OBJS  = setupcoag.o calcbetas.o quadl.o integr.o misc.o calcgrowth.o settlingvelocity.o calcsinkingloss.o calcinitialspec.o betas.o calccoagderiv.o calcdae.o diag.o
OBJS  = setupcoag.o betas.o calcbetas.o integr.o quadl.o diag.o calcgrowth.o calcsinkingloss.o settlingvelocity.o calcinitialspec.o calccoagderiv.o

fflags= $(shell nc-config --fflags)
flibs = $(shell nc-config --flibs)

all: $(OBJS)
	$(FC) -o coag $(fflags) coag_driver.f90 $(OBJS) -L. -lslatec $(flibs) 
	./coag

check:
	$(FC) -o check ddassl_ex.F -L. -lslatec

clean:
	rm -rf *~ *.o *.mod coag check

.SUFFIXES: .f90 .o
.f90.o:
	$(FC) -c $(fflags) $<
