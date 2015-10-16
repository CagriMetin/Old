FC = gfortran
PC = python
FFLAGS = -O3
LFLAGS =
OBJECTS = textToFoam.o test.o
.PHONY: clean help


blockMeshDict: run.exe
	./run.exe

run.exe: $(OBJECTS)
	$(FC) $(LFLAGS) $(OBJECTS) -o run.exe

%.o : %.f90
	$(FC) $(FFLAGS) -c $<


clean:
	rm -f $(OBJECTS) run.exe *.mod blockMeshDict

help:
	@echo "Valid targets:"
	@echo "run"
	@echo "test.o"
	@echo "textToFoam.o"
	@echo "clean: removes .o, .exe and blockMeshDict files"

