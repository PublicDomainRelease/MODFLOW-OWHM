# MAKEFILE EXAMPLE/TEMPLATE FOR COMPILING HYDFMT
# DEVELOPED BY SCOTT E BOYCE 
#
# PROVIDED AS IS WITHOUT WARRANTY OR HELP.
# IF THIS SCRIPT HELPS YOU PLEASE CITE THE OWHM REPORT
# 
# DESIGNED TO USE BY DEFAULT INTEL FORTRAN AND INTEL C (ifort and icc) AS RELEASE x64 VERSION
#
# TO INVOKE/COMPILE TYPE FROM COMMAND PROMPT:
# make                  SAME AS make all
# make all              CLEANS OUT OBEJCT (.o), THEN COMPILES 
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
# TO COMPILE STATICALLY (NO LIBRARY DEPENDENCE) 
# ===> ANSWER EITHER "YES" OR "NO" TO COMPILE WITH STATIC OPTION    --MUST BE UPPER CASE
STATIC=YES
#
# DEFINE THE FORTRAN COMPILER
# ===> ANSWER EITHER "ifort" or "gfortran"     --MUST BE LOWER CASE  
F90=ifort
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
bin=../../bin/Postprocessors/
src=./
#
#ADDITIONAL LIBRARIES (LEAVE BLANK UNLESS NEEDED)
#
SYSLIBS= 
USRLIB= 
#
########################################################################
#
# SETUP SUFFIXES THAT WILL BE PROCESSES
.SUFFIXES: .f .F .f90 .F90 .o .mod .fpp
#
# SET UP TARGETS TO BE RUN AS FUNCTIONS AND NOT PROCESS FILES
.PHONY: all  veryclean  clean  STARTUPMSG  DOUBLE_ECHO hydfmt
#
########################################################################
#
# DEFINE ALL FORTRAN OBJECT FILES WHICH MAKE UP MODFLOW-OWHM
#
OBJECTS = $(src)hydfmt1_7.o
#
########################################################################
#
# SET UP NAMES AND OPTIMIZATIONS DEPENDING ON COMPILER AND CONFIGURATION
#
ifeq ($(strip $(OPTCFG)), DEBUG)
   #
   PROGRAM= hydfmt_debug.nix
   #
   F90FLAGSINTEL=-O0 -debug -g -traceback -assume nocc_omp -fpe0 -fp-model source -nologo -warn nousage -check bounds,pointers,stack,format,output_conversion,uninit
   F90FLAGSGCC=  -O0        -g   #-fstack-usage  #<= THIS PROVIDES LOTS OF INFO
   #
else
   #
   PROGRAM=hydfmt.nix
   #
   F90FLAGSINTEL=-O3 -ip   -assume nocc_omp -fpe0 -fp-model source -threads -warn nousage -nologo
   F90FLAGSGCC=  -O3 -fno-backtrace
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
########################################################################
#
# SET UP STATIC FLAG IF REQUESTED
#
ifeq ($(strip $(STATIC)), YES)
  STATIC=-static
else
  STATIC= 
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
STATIC:=$(strip $(STATIC))
USRLIB:=$(strip $(USRLIB))
SYSLIBS:=$(strip $(SYSLIBS))
OPTCFG:=$(strip $(OPTCFG))
#
########################################################################
#
# DEFINE ALL TASK FUNCTIONS
#
all: STARTUPMSG preclean hydfmt
#
hydfmt:  DOUBLE_ECHO $(OBJECTS)
	@echo
	@echo "OBJECTS HAVE BEEN CREATED NOW LINKING FINAL BINARY"
	@echo
	$(F90) $(F90FLAGS) $(GMG) $(OBJECTS) $(USRLIB) $(SYSLIBS)  -o $(PROGRAMOUT)
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
	rm -rf     *.o
	rm -rf $(PROGRAMOUT) 
	@echo
	@echo
#
clean: 
	@echo
	rm -rf     *.o
	@echo
	@echo
#
veryclean: 
	@echo
	rm -rf       *.o
	rm -rf $(bin)hydfmt*.nix
	@echo
	@echo
#
STARTUPMSG:
	@echo
	@echo
	@echo "                 $(OPTCFG) COMPILATION"
	@echo
	@echo "                        OF"
	@echo
	@echo "                   HYDFMT v1.7"
	@echo
	@echo
#
DOUBLE_ECHO:
	@echo
	@echo
#
# OBJECT CODES
#
.f.o:
	$(F90) $(F90FLAGS) $(STATIC) $(SYSLIBS)   -c  $<		-o $@
	@echo
#
.F.o:
	$(F90) $(F90FLAGS) $(STATIC) $(SYSLIBS)   -c  $<		-o $@
	@echo
#
.f90.o:
	$(F90) $(F90FLAGS) $(STATIC) $(SYSLIBS)   -c  $<		-o $@
	@echo
#
.F90.o:
	$(F90) $(F90FLAGS) $(STATIC) $(SYSLIBS)   -c  $<		-o $@
	@echo
#
.fpp.o:
	$(F90) $(F90FLAGS) $(STATIC) $(SYSLIBS)   -c  $<		-o $@
	@echo
#
#
# THE END
