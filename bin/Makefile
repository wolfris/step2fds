SHELL:=/bin/bash

BIN:= splines
FFLAGS:= -frange-check
FFLAGS+= -fbounds-check 
FFLAGS+= -fcheck-array-temporaries
FFLAGS+= -O
FC:=gfortran -g  
SRCROOT:=../src
VPATH:= $(SRCROOT) 
SRC:=$(filter %.f90, $(shell find $(VPATH) -type f))
SRC:=$(notdir $(subst .F90,.f90,$(SRC)))
SRCC:=$(filter %.f90~, $(shell find $(SRCROOT) -type f))

export LIBRARY_PATH:= $(LIBDIR)

OBJS = $(SRC:.f90=.o)

DEP:=$(BIN).mk



$(BIN): $(OBJS)
	$(FC) $(FFLAGS) $(OBJS) -o $@ 
	@echo 
	@echo PROGRAM SUCCESSFULLY BUILT
	@echo 

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

include $(DEP)

$(DEP): $(SRC)
	makedepf90  $^ >  $@
	@echo $<

clean:
	rm -f *.o *.mod *~ *.mk

rmsrc:
	rm $(SRCC)

purge: 
	$(MAKE) clean
	rm -f $(BIN)

purgeall:
	$(MAKE) purge
	$(MAKE) rmsrc
