ffiles=$(wildcard *.f)
ofiles=$(ffiles:.f=.o)

all: lib
	$(FC) main_test.f -o a.out -L. -lslatec

lib: $(ofiles)
	ar rcv libslatec.a $(ofiles)

clean:
	rm -rf *~ *.o a.out libslatec.a

.SUFFIXES: .f .o
.f.o:
	$(FC) -c $<
