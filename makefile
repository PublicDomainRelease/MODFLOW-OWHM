# MAKEFILE EXAMPLE/TEMPLATE FOR COMPILING MODFLOW-OWHM
# DEVELOPED BY SCOTT E BOYCE 
#
# PROVIDED AS IS WITHOUT WARRANTY OR HELP.
# IF THIS SCRIPT HELPS YOU PLEASE CITE THE OWHM REPORT
# 
# DESIGNED TO USE BY DEFAULT INTEL FORTRAN AND INTEL C (ifort and icc) AS RELEASE x64 VERSION
#
# TO INVOKE/COMPILE TYPE FROM COMMAND PROMPT:
# make                  SAME AS make all
# make all              CLEANS OUT OBEJCT (.o) AND MODULE (.mod) FILES, THEN COMPILES OWHM [MAKE FILES ARE NOT SMART ENOUGH TO RECOMPILE THE APPROPIATE FILES BASED ON MODULE DEPENDENECY IF A SOURCE FILE IS MODIFIED]
# make OWHM             COMPILE OWHM WITH UPDATING OBJECT FILES AS NEEDED (IF NONE ARE PRESENT THEN FUNCTIONS THE SAME AS "make all")
# make clean            DELETE ALL OBEJCT (.o) AND MODULE (.mod) FILES
# make veryclean        DELETE OHWM BINARYS AND ALL OBEJCT (.o) AND MODULE (.mod) FILES
#
########################################################################
########################################################################
#     SET THE FOLLOWING 5 VARIABLES (OPTCFG, USEGMG, STATIC, F90, CC)
########################################################################
########################################################################
#
# COMPILATION OPTIMIZATION SCHEME
# ===> ANSWER EITHER "RELEASE" OR "DEBUG" --MUST BE UPPER CASE
OPTCFG=RELEASE
#
# DECIDE IF YOU WANT TO COMPILE WITH GMG
# ===> ANSWER EITHER "YES" OR "NO"        --MUST BE UPPER CASE
USEGMG=YES
#
# TO COMPILE STATICALLY (NO LIBRARY DEPENDENCE) 
# ===> ANSWER EITHER "YES" OR "NO" TO COMPILE WITH STATIC OPTION    --MUST BE UPPER CASE
STATIC=YES
#
# DEFINE THE FORTRAN COMPILER
# ===> ANSWER EITHER "ifort" or "gfortran"     --MUST BE LOWER CASE  
#                  ****Different gfortran versions may not support all the code features implemented in OWHM. Please validate with example problems or use Intel Fortran.
F90=ifort
#
# DEFINE THE C COMPILER
# ===> ANSWER EITHER "icc" or "gcc"            --MUST BE LOWER CASE
CC=icc
#
#
########################################################################
########################################################################
#             DO NOT MODIFY BELOW THIS LINE
#            UNLESS YOU KNOW WHAT YOUR DOING
########################################################################
########################################################################
#
#
#
#
#
########################################################################
#
#DEFINE FINAL BIN DIRECTORY, SOURCE DIRECTORY => ALL BE BLANK OR MUST HAVE TRAILING /
#
bin=bin/
src=src/
#
#ADDITIONAL LIBRARIES (LEAVE BLANK UNLESS NEEDED)
#
SYSLIBS= 
USRLIB= 
#
########################################################################
#
# SETUP SUFFIXES THAT WILL BE PROCESSES
.SUFFIXES: .f .F .f90 .F90 .o .c .mod .fpp  # .h 
#
# SET UP TARGETS TO BE RUN AS FUNCTIONS AND NOT PROCESS FILES
.PHONY: all  OWHM  veryclean  clean  preclean     \
        WARNINGS  GMG_PREPROCESS  GMG_POSTPROCESS \
        STARTUPMSG  DOUBLE_ECHO  MOVE_MODULES 
#
########################################################################
#
# DEFINE ALL FORTRAN OBJECT FILES WHICH MAKE UP MODFLOW-OWHM
#
OBJECTS = \
           $(src)mach_mod.o           \
           $(src)modules.o            \
           $(src)xy_grid_coordinate_interface.o \
           $(src)ExpressionParser.o   \
           $(src)utl7module.o         \
           $(src)gwf2bas7_NWT.o       \
           $(src)gwflgrmodule.o       \
           $(src)de47.o               \
           $(src)pcg7R.o              \
           $(src)sip7.o               \
           $(src)gmg7_NWT_dble.o      \
           $(src)gsol7.o              \
           $(src)mhc7.o               \
           $(src)pcgn_solve2.o        \
           $(src)pcgn2.o              \
           $(src)tabfilemodule.o      \
           $(src)gwf2bcf7.o           \
           $(src)gwf2lpf7.o           \
           $(src)gwf2huf7.o           \
           $(src)NWT1_module.o        \
           $(src)gwf2upw1.o           \
           $(src)NWT1_xmdlib.o        \
           $(src)NWT1_xmd.o           \
           $(src)NWT1_gmres.o         \
           $(src)NWT1_ilupc_mod.o     \
           $(src)Solver_RP.o          \
           $(src)NWT1_solver.o        \
           $(src)gwf2evt7.o           \
           $(src)gwf2res7.o           \
           $(src)gwf2rch7_NWT.o       \
           $(src)gwf2lgr2.o           \
           $(src)FMP3_NWT_GLOBAL.o    \
           $(src)gwfuzfmodule_NWT.o   \
           $(src)gwfsfrmodule_NWTL.o  \
           $(src)gwf2sub7L_parm.o     \
           $(src)gwf2lak7_NWTL.o      \
           $(src)gwf2sfr7_NWTRL.o     \
           $(src)gwf2uzf1_NWT.o       \
           $(src)gwf2gag7.o           \
           $(src)gwf2bfh2.o           \
           $(src)gwf2chd7.o           \
           $(src)gwf2drn7_NWT.o       \
           $(src)gwf2drt7.o           \
           $(src)gwf2ets7.o           \
           $(src)gwf2fhb7.o           \
           $(src)gwf2ghb7_NWT.o       \
           $(src)gwf2hfb7_NWT.o       \
           $(src)gwf2ibs7.o           \
           $(src)gwf2rip4.o           \
           $(src)gwf2riv7_NWT.o       \
           $(src)gwf2str7.o           \
           $(src)gwf2swt7.o           \
           $(src)gwf2wel7_NWT.o       \
           $(src)hufutl7.o            \
           $(src)obs2bas7.o           \
           $(src)obs2drn7.o           \
           $(src)obs2ghb7.o           \
           $(src)obs2riv7.o           \
           $(src)obs2chd7.o           \
           $(src)parutl7.o            \
           $(src)gwf2mnw17_NWT.o      \
           $(src)gwf2mnw27_NWTL.o     \
           $(src)gwf2mnw2i7.o         \
           $(src)gwf2swi27_NWT.o      \
           $(src)lmt7_NWT.o           \
           $(src)gwf2hydmod71.o       \
           $(src)gwf2swr7_NWT.o       \
           $(src)FMP3_NWT_SUBS.o      \
           $(src)utl7.o               \
           $(src)MF_OWHM.o
#
########################################################################
#
# DEFINE GMG OBJECTS IF GMG IS GOING TO BE INCLUDED IN COMPILATION
#
ifeq ($(strip $(USEGMG)), YES)
  GMG = $(src)r_vector.o     \
        $(src)solvers.o      \
        $(src)ccfd.o         \
        $(src)mf2kgmg_NWT.o
  #
  NOGMGSETUP= 
  #
  NOGMGCLEANUP= 
  #
else
  #
  GMG= 
  #
  define NOGMGSETUP
      @echo
      @echo "CREATING BACKUP OF GMG ROUTINES (gmg7_NWT_dble.f=>gmg7_NWT_dble.f.bak)"
      @mv -f $(src)gmg7_NWT_dble.f $(src)gmg7_NWT_dble.f.bak
      @echo
      @echo "AND NOW SETTING UP NOGMG ROUTINES"
      @cp -f $(src)nogmg.txt       $(src)gmg7_NWT_dble.f 
      @echo
      @echo
      @echo "BEGINING COMPILATION OF ONLY FORTRAN OBJECTS"
  endef
  #
  define NOGMGCLEANUP
      @echo
      @echo "RESTORING BACKUP OF GMG ROUTINES (gmg7_NWT_dble.f.bak=>gmg7_NWT_dble.f)"
      @mv -f $(src)gmg7_NWT_dble.f.bak $(src)gmg7_NWT_dble.f
      @echo
  endef
  #
endif
#
########################################################################
#
# SET UP/INITALIZE WARNING TRIGGERS (THEY WILL BE FILLED WITH A SET OF COMMANDS IF A WARNING IS TRIGGERED)
#
ICC2GCC= 
#
########################################################################
#
# SET UP NAMES AND OPTIMIZATIONS DEPENDING ON COMPILER AND CONFIGURATION
#
ifeq ($(strip $(OPTCFG)), DEBUG)
   #
   PROGRAM= OWHM_debug.nix
   #
   F90FLAGSINTEL=-O0 -debug -g -traceback -assume nocc_omp -fpe0 -fp-model source -nologo -warn nousage -check bounds,pointers,stack,format,output_conversion,uninit
   F90FLAGSGCC=  -O0        -g   #-fstack-usage  #<= THIS PROVIDES LOTS OF INFO
   #
   CFLAGSINTEL=  -O0 -debug -g -D_UF -wd810 -fbuiltin  # => "-D_UF" defines UNIX naming conventions for mixed language compilation. This fixes a lot of compilation problems since GMG was written before Fortran 2008 BIND(C) came about.
   CFLAGSGCC=    -O0        -g -D_UF -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast
   #
else
   #
   PROGRAM=OWHM.nix
   #
   F90FLAGSINTEL=-O3 -ip   -assume nocc_omp -fpe0 -fp-model source -threads -warn nousage -nologo
   F90FLAGSGCC=  -O3 -fno-backtrace
   #
   CFLAGSINTEL=  -O3 -D_UF -fbuiltin  -wd810
   CFLAGSGCC=    -O3 -D_UF -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast
   #
   # IF COMPILING WITH GFORTRAN AND ICC (¿WHY WOULD ANYONE DO THIS?) 
   # TRIGGER WARNING THAT LINKING ERRORS OCCUR WITH HIGHER OPTIMIZATIONS
   ifeq ($(strip $(F90)), gfortran)
     ifeq ($(strip $(CC)), icc)
       CFLAGSINTEL=  -O1 -D_UF -fbuiltin  -wd810
       define ICC2GCC
           @echo
           @echo
           @echo "*****************************************************************************"
           @echo "***  WARNING COMPILATION ERRORS WILL RESULT ON RELEASE, GFORTRAN AND ICC  ***"
           @echo "*** TO PREVENT ERRORS FROM OCCURING, THE C COMPILER OPTIMAZION IS LOWERED ***"
           @echo "***                         FROM -O3 TO -O1                               ***"
           @echo "*****************************************************************************"
           @echo
           @echo
           @echo
           @echo
       endef
     endif
   endif
   #
endif
#
########################################################################
#
#ESTABLISE PROPERT OPTIMIZATION FLAGS
#
ifeq ($(strip $(F90)), gfortran)
  F90FLAGS=$(F90FLAGSGCC)
endif
#
ifeq ($(strip $(F90)), ifort)
  F90FLAGS=$(F90FLAGSINTEL)
  SYSLIBS+= -static-intel
endif
#
ifeq ($(strip $(CC)), gcc)
  CFLAGS=$(CFLAGSGCC)
endif
#
ifeq ($(strip $(CC)), icc)
  CFLAGS=$(CFLAGSINTEL)
endif
#
########################################################################
#
# SET UP STATIC FLAG IF REQUESTED
#
ifeq ($(strip $(STATIC)), YES)
  STATIC=-static
  STATICLNK=                   #THIS CAN CAUSE PROBLEMS WITH GCC RUNTIME SO BEST NOT TO USE
else
  STATIC= 
  STATICLNK=
endif
#
########################################################################
#
#SET UP PROGRAM NAME
#
PROGRAMOUT=$(bin)$(PROGRAM)
#
########################################################################
#
# REMOVE VARIABLE BLANK SPACE FOR CLEANER OUTPUT
#
F90:=$(strip $(F90))
F90FLAGS:=$(strip $(F90FLAGS))
CC:=$(strip $(CC))
CFLAGS:=$(strip $(CFLAGS))
STATIC:=$(strip $(STATIC))
STATICLNK:=$(strip $(STATICLNK))
USRLIB:=$(strip $(USRLIB))
SYSLIBS:=$(strip $(SYSLIBS))
OPTCFG:=$(strip $(OPTCFG))
#
########################################################################
#
# DEFINE ALL TASK FUNCTIONS
#
all: STARTUPMSG preclean OWHM
#
OWHM:  WARNINGS GMG_PREPROCESS  $(GMG) DOUBLE_ECHO $(OBJECTS)  MOVE_MODULES GMG_POSTPROCESS
	@echo
	@echo "OBJECTS HAVE BEEN CREATED NOW LINKING FINAL BINARY"
	@echo
	$(F90) $(F90FLAGS) $(GMG) $(OBJECTS) $(USRLIB) $(SYSLIBS) $(STATICLNK)  -o $(PROGRAMOUT)
	@echo
	@echo
	@echo
	@echo
	@echo "   MAKEFILE COMPILATION COMPLETE"
	@echo
	@echo
#
preclean:
	@echo
	rm -rf     *.mod
	rm -rf     *.o
	rm -rf $(src)*.mod
	rm -rf $(src)*.o
	rm -rf $(PROGRAMOUT) 
	@echo
	@echo
#
clean: 
	@echo
	rm -rf     *.mod
	rm -rf     *.o
	rm -rf $(src)*.mod
	rm -rf $(src)*.o
	@echo
	@echo
#
veryclean: 
	@echo
	rm -rf       *.mod
	rm -rf       *.o
	rm -rf $(src)*.mod
	rm -rf $(src)*.o
	rm -rf $(bin)OWHM*.nix
	@echo
	@echo
#
WARNINGS:
	@$(ICC2GCC)
#
STARTUPMSG:
	@echo
	@echo
	@echo "                 $(OPTCFG) COMPILATION"
	@echo
	@echo "                        OF"
	@echo
	@echo "                      MF-OWHM"
	@echo
	@echo
	@echo "Do too potential module dependency conflicts all exisiting "
	@echo "objects and modules a will be removed and recompiled."
	@echo
#
DOUBLE_ECHO:
	@echo
	@echo
#
MOVE_MODULES:
	@mv *.mod $(src) 2>/dev/null  || true
#
GMG_PREPROCESS:; $(NOGMGSETUP)

GMG_POSTPROCESS:; $(NOGMGCLEANUP)
#
# OBJECT CODES
#
.f.o:
	$(F90) $(F90FLAGS) $(STATIC) $(SYSLIBS)  -c  $<		-o $@
	@echo
#
.F.o:
	$(F90) $(F90FLAGS) $(STATIC) $(SYSLIBS)  -c  $<		-o $@
	@echo
#
.f90.o:
	$(F90) $(F90FLAGS) $(STATIC) $(SYSLIBS)  -c  $<		-o $@
	@echo
#
.F90.o:
	$(F90) $(F90FLAGS) $(STATIC) $(SYSLIBS)  -c  $<		-o $@
	@echo
#
.fpp.o:
	$(F90) $(F90FLAGS) $(STATIC) $(SYSLIBS)  -c  $<		-o $@
	@echo
#
.c.o:
	$(CC) $(CFLAGS) $(STATIC)    -c  $<		-o $@
	@echo
#
#
# THE END
