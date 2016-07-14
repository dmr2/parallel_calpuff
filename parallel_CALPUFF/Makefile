
SRCDIR = $(PWD)
OBJDIR = $(SRCDIR)
FC = mpif90
PARLIB = ./f90_src/
#FFLAGS =  -O2 -Mpreprocess -Dlinux -module $(PARLIB)
#FFLAGS =  -O2 -Mpreprocess -Mlfs -Dlinux -module $(PARLIB)
#FFLAGS =  -check -static -traceback -p -fpp -module $(PARLIB) 
FFLAGS =  -O3 -static -traceback -p -fpp -module $(PARLIB) 
#LDFLAGS = -L$(PARLIB) -lpar
LDFLAGS = ${PARLIB}/*.o
#CPP = cpp
#CPPFLAGS =  -Dlinux -I.
AR = ar
ARFLAGS = rv
RM = rm -f 

FSRC = \
CALPUFF.FOR \

FSRC1 = \
calutils.for \
coordlib.for

EXE = calpuff.exe
OBJ = $(FSRC:.FOR=.o)  $(FSRC1:.for=.o)

# clean out all the suffixes
.SUFFIXES:
#list only what we use
.SUFFIXES:.FOR .for .o

#define suffix rule for .FOR to .o and .for to .o
.FOR.o: 
	$(FC) -c $(FFLAGS) $(SRCDIR)/$<

.for.o: 
	$(FC) -c $(FFLAGS) $(SRCDIR)/$<

#define target by convention is all
all : $(EXE)

$(EXE): $(OBJ)
	$(FC) -o $@ $(OBJ) $(LDFLAGS)

$(OBJ): params.puf

clean:
	cd $(OBJDIR); $(RM) $(OBJ) $(EXE)

