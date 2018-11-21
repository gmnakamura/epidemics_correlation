F90=ifort #gfortran
FFLAGS=-O2 -fopenmp

OBJ=subroutines.o statistics.o
SOURCE=subroutines.f90 statistics.f90

main: main.f90 $(OBJ)
	$(F90) $(FFLAGS) main.f90 $(OBJ) -o main
$(OBJ): $(SOURCE)
	$(F90) $(FFLAGS) -c $(SOURCE)
