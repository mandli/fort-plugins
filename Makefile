SRC = plugin.f90
OBJECTS = plugin.o
MODULES = \
	pointer_module.mod \
	branching_module.mod \
	preprocessor_module.mod \
	random_module.mod

# FC = ifort
# FFLAGS = -O2 -ipo
# FFLAGS = -g -check all -traceback -fpe0 -ftrapuv -fp-model precise
FC = gfortran
FFLAGS = -O3 -funroll-loops -finline-functions
# FFLAGS = -O0 -W -Wall -Wunderflow -fbacktrace -ffpe-trap=invalid,zero,overflow -g
# FFLAGS += -DUSE_BLAS=1
LFLAGS ?= $(FFLAGS) -lblas

%.o : %.f90 ; $(FC) -c -cpp $< $(FFLAGS) -o $*.o
%.mod : %.f90 ; $(FC) -c -cpp $< $(FFLAGS) -o $*.o

.PHONY: clean, new, use_blas

plugin_test: $(OBJECTS) $(MODULES)
	$(FC) $(OBJECTS) $(LFLAGS) -o plugin_test

clean:
	-rm -f plugin_test
	-rm -f $(OBJECTS) 
	-rm -f $(MODULES)
	-rm -rf *.dSYM

new:
	$(MAKE) clean
	$(MAKE) plugin_test