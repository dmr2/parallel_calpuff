
SRCDIR = $(PWD)
OBJDIR = $(SRCDIR)
#FC = pgf90
FC = mpif90
#FFLAGS =  -O2 -tp p6 -pc 32 -Mfree
#FFLAGS =  -O2 -tp x64 -pc 32 -Mfree -byteswapio
INCLUDE_DIR2 = /data/n000/home/dmr/CALPUFF/UCD_parallel/
#FFLAGS =  -O2 -Mpreprocess -Mfree -Mcpp=comment -Dlinux -I$(INCLUDE_DIR) -I$(INCLUDE_DIR2)
#FFLAGS =  -O2 -Mpreprocess -Mlfs -Dlinux -I$(INCLUDE_DIR) -I$(INCLUDE_DIR2)
#FFLAGS2 =  -O2 -Mpreprocess -Mlfs -Mfixed -Mextend -Dlinux -I$(INCLUDE_DIR) -I$(INCLUDE_DIR2)
#FFLAGS =  -check -traceback -extend_source -I$(INCLUDE_DIR) -I$(INCLUDE_DIR2)
#FFLAGS2 = -check -traceback -extend_source -I$(INCLUDE_DIR) -I$(INCLUDE_DIR2)
FFLAGS =  -O2 -traceback -extend_source  -I$(INCLUDE_DIR2)
FFLAGS2 =  -O2 -traceback -extend_source  -I$(INCLUDE_DIR2)
LDFLAGS = 
AR = ar
ARFLAGS = rv
RM = rm -f 

MOD_FSRC = \
mpif_mod.f90 \
mpiranktasks_mod.f90 \
mpifilesize_mod.f90 \
mpidecomp_mod.f90  


MOD_FSRC2 = \
#mpichiflx_mod.F \
#mpimflux_mod.F \
#mpimsbal_mod.F

FSRC = \
mpilaunch.f90 \
mpiopenlog.f90 

FSRC2 = \
mpiwrout1.F \
mpioutput.F \
mpiwrdatc.F \
mpiopenot.F \
mpicomprs.F \
mpiwrdatc.F \
mpixtract.F \
mpiwrdatu.F \
mpiwrdatucr.F \
mpiwrdatur.F \
mpioutsam.F \
mpidecomp1D.F

LIB = libpar.a

MOD_OBJS = $(MOD_FSRC:.f90=.o)
MOD_OBJS2 = $(MOD_FSRC2:.F=.o)

OBJS = $(FSRC:.f90=.o)
OBJS2 = $(FSRC2:.F=.o)

# clean out all the suffixes
.SUFFIXES:
#list only what we use
.SUFFIXES:.F .o .f90

#define suffix rule for .FOR to .o and .for to .o
.f90.o: 
	cd $(OBJDIR); $(FC) -c $(FFLAGS) $(SRCDIR)/$<
.F.o: 
	cd $(OBJDIR); $(FC) -c $(FFLAGS2) $(SRCDIR)/$<

#define target by convention is all
all : $(LIB)

$(LIB): $(OBJS) $(OBJS2)
	cd $(OBJDIR); $(AR) $(ARFLAGS) $(LIB) $(OBJS) $(OBJS2) 

$(OBJS): $(MOD_OBJS) $(MOD_OBJS2)
$(OBJS2): $(MOD_OBJS) $(MOD_OBJS2)

clean:
	cd $(OBJDIR); $(RM) *.mod  $(MOD_OBJS) $(MOD_OBJS2) $(OBJS) $(OBJS2) $(LIB)
