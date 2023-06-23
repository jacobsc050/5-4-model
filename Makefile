main.out:  convert.o Lattice.o Lagrangian.o
	gfortran -o main.out *.o 

%.o:%.f90
	gfortran -c -o $@ $<