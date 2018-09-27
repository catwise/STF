FC = gfortran

FCFLAGS = -m64 -O3 -fdollar-ok

all: stf

stf.o:
	$(FC) $(FCFLAGS) -c src/stf.f

stf: stf.o bin
	$(FC) -o bin/stf stf.o

bin:
	mkdir bin/

clean:
	rm -f *.o
