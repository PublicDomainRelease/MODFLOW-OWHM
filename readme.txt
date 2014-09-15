README.TXT [best viewed with NO word wrap]


         MF-OWHM (Version 1.00.00 (11/05/2014) includes MODFLOW-2005 - Version: 1.11.0  08/08/2013)
         Three-dimensional finite-difference integrated hydrologic flow model


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

This version of MODFLOW is referred to as MF-OWHM because this is it is 
fundamentally different from older versions of the code and other 
versions of MODFLOW and represents a fully integrated hydrologic model. 
This version of MODFLOW-OWHM (MF-OWHM) is packaged for personal computers 
using the Microsoft Windows or Unix operating systems.  Executable files 
for Windows and Linux for personal computers are provided as well as the 
source code.  The source code can be compiled (see Section E. COMPILING) 
to run on other computers and the Linux/Unix makefile is provided that 
can compile with either Intel FORTRAN (v13+) or GFORTRAN (v4.9+). Please
note that GFORTRAN being open source is a continually changing compiler with
different versions being released constantly across different platforms.
There maybe issues with using this and some of the higher level Fortran
features implemented within MF-OWHM. Modification of the makefile is 
required to use other compilers.

IMPORTANT: Users should review the file MF_OWHM.txt for a description of, and
references for, this software. Users should also review the file release.txt,
which describes changes that have been introduced into MF-OWHM with each
official release; these changes may substantially affect users. Finally the users
should review the MF-OWHM Techniques and Methods report (http://pubs.usgs.gov/tm/tm6a51/)  
and the online user's manual at http://water.usgs.gov/ogw/modflow-owhm/Guide/index.html.

Instructions for installation, execution, and testing of MODFLOW-OWHM are
provided below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following self-extracting distribution file is for use on personal
computers:

         MF_OWHM_v1_0.zip (Windows)
         
or
         
         MF_OWHM_v1_0.tar.gz (UNIX)
         

The distribution file contains:

          Compiled run files and source code for MF-OWHM.
          Supplementary MF-OWHM documentation in PDF and text files.
          Test data sets.

The distribution file is a compressed file that includes numerous individual 
files.  The distribution file can be extracted to a user-specified directory.
For the remainder of this file it is assumed that the distribution file has
been extracted to C:\WRDAPP.  

The following directory structure will be created in C:\WRDAPP:


   |
   |--MF_OWHM_v1_0
   |  |--bin             ;     MODFLOW-OWHM executables for personal computers   **For linux users, binaries are provided (*.nix)
   |  |                  ;       as is and may not work for your distribution. In addition, you may have to make the binaries 
   |  |                  ;       executable via the chmod command, for example "chmod +x *.nix" will make all the .nix ending 
   |  |                  ;       binaries executable.
   |     |Postprocessors ;     Post-processing executables for personal computers
   |  |--doc             ;     Documentation files
   |  |--examples        ;     Input, output, true output, spreadsheets, and batch/bash scripts to run examples from all flavors
   |  |                  ;       of MODFLOW. This is provided to run verification tests and understand new features with 
   |  |                  ;       Example Models
   |     |bash_scripts_4_testing__Unix;   Scripts for running various examples on linux with BASH. Note that you may have to do 
   |     |                            ;     "chmod +x *.sh" (without quotes) to make the shell scripts executable within your
   |     |                            ;     distribution. This is also true for the *.nix binaries.
   |     |batch_scripts_4_testing__Win;   Scripts for running various examples on Windows XP+ with Batch. 
   |     |test-run-*     ;     Directories that contain the input files from the various versions of MODFLOW. Notably the NAME 
   |  |                  ;       file resides here of different models. (*note that some files are made within the test-run-* 
   |  |                  ;       folders when running the examples. 
   |  |                  ;       The * is a wildcard that is named after the MF distribution that the code originated from. 
   |  |                  ;       These are moved by the BASH/BATCH scripts to the test-out-* directory)
   |     |test-out-true-*;     Directories that contain the solution generated by the development team.
   |     |test-out-*     ;     Directories that contain the output that is generated by the example problems by the test-run-* 
   |  |                  ;       examples (*note that some files are made within the test-run-* folders and are moved by the 
   |  |                  ;       BASH/BATCH scripts to the test-out-* directory)
   |  |--src             ;     MF-OWHM source code for use on any computer
   |  |--src_Postprocessors;   Source for hydrograph post processing program and makefiles
   |     |src_hydfmt       ;   Source for hydrograph (HYDMOD) post processing program and a makefile
   |     |src_zonebudget   ;   Source for ZoneBudget post processing program and a makefile
   |  |--Examples_Description; Description of the MODFLOW-2005 example problems located in test-run and test-out-true
   |  |--makefile          ;   Makefile capable of compiling MF-OWHM 1.0 with either GFORTRAN or INTEL FORTRAN. It requires 
   |  |                    ;     configuring 5 input variables to run. GFORTRAN is not recommended since most versions 
   |  |                    ;     do not support all the code features currently located in MF-OWHM
   |  |--MF_OWHM.txt       ;   
   |  |--readme.txt        ;   An ingenious document containing an overview of what's in MF-OWHM (ie What you are reading :P )
   |  |--release.txt       ;   MF-OWHM Release Notes - See this for updates and changes.
   |  |--Stack Overflow or Segmentation Fault Info.txt ;  Discussion about those errors and how to overcome them.


It is recommended that no user files are kept in the MF_OWHM_v1_0 directory
structure.  If you do plan to put your own files in the MF_OWHM_v1_0
directory structure, we recommend creating separate additional subdirectories.

Included in directory MF_OWHM_v1_0\doc are various documentation files.  Some
of them are Portable Document Format (PDF) files. The PDF files are readable
and printable on various computer platforms using Acrobat Reader from Adobe.
The Acrobat Reader is freely available from the following World Wide Web
site:
      http://www.adobe.com/


B. INSTALLING

To make the executable versions of MODFLOW-OWHM accessible from any
directory, the directory containing the executable (./bin)
should be included in the PATH environment variable.  Also, if a
prior release of MODFLOW-OWHM is installed on your system, the
directory containing the executables for the prior release should
be removed from the PATH environment variable.

As an alternative, the executable files, MF_OWHM_Win32.exe and MF_OWHM.exe,
in the .\bin directory can be copied into a directory already included in 
the PATH environment variable. The MF_OWHM executable is for running on 
64-bit Windows operating systems. The Windows versions will also run under 
WINE (www.winehq.org) for UNIX OS

       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
                 WINDOWS 7 SYSTEM
             
From the Start menu, select Control Panel.  Select
System & Security followed by System.  Choose the Advanced System
option.  Select the Settings Task, and then select the Environmental
Variables button.  In the System Variables pane, select the PATH
variable followed by Edit.  In the Edit window, add
";C:\WRDAPP\MF_OWHM_v1_0\bin" to the end of the Variable Value (ensure
that the current contents of the User Value are not deleted) and click
OK. Click OK in the Environment Variables window and then exit from the
control panel windows.  Initiate and use a new Windows Command window.


C. EXECUTING MODFLOW-OWHM

Two MF-OWHM run files for use on personal computers are provided. 
MF_OWHM_Win32.exe is the 32-bit version of MODFLOW-OWHM (Version 1.0.00). 
It uses mixed single and double precision for computations and internal 
data storage, which was determined to be useful for a wide range of 
simulations.  There are situations in which speed and precision are 
inadequate, which can be indicated by difficulty attaining solver 
convergence or poor budget error and excessively long runtimes.
  
Accordingly, a run file that uses 64-bit precision is provided -- MF_OWHM.exe. 
If mixed precision is suspected of causing problems in a simulation or 
slow runtimes are problematic of parameter estimation settings, the same 
simulation can be run using the 64-bit run file. Input for the two run files 
is the same. In fact the source code is identical for both. The 64-bit precision
run file is created by using a compiler option that raises the precision and can 
take advantage of increased speed of the 64-bit operating system. Several new 
MF-OWHM features will not function properly if you perform a compilation with 
forced global double precision and we don't recommend it. We modified the source 
code and released a double precision executable version with this release package. 

The advantage for using 64-bit technology is additional precision and computational 
speed. The disadvantage are unformatted (binary) files that are doubled in size.  
(Binary files are used for saving head, drawdown, and budget data.) This penalty is 
frequently not very significant.  Typical computers have adequate memory to run most 
simulations in 64-bit mode, are 2 to 8 times as fast performing than 32-bit run file, 
and have abundant disk space for storing binary output files.

After the executable files in the MF_OWHM_v1_0\bin directory are installed
in a directory that is included in your PATH, MF-OWHM is initiated in a Windows
Command-Prompt window or windows batch file using one of the following commands:

          MF_OWHM_Win32 [Fname]
or
          MF_OWHM [Fname]

The optional Fname argument is the name file. If no argument is used,
the user is prompted to enter the name file. If the name file ends in
".nam", then the file name can be specified without including ".nam". 
For example, if the name file is named abc.nam, then the simulation can
be run using mixed precision by entering:

          MF_OWHM abc

The data arrays in MF-OWHM are dynamically allocated, so models
are not limited by hard-coded array limits. However, it is best to have
enough random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than this, the program will use
virtual memory, but this slows computations significantly.

Some of the files written by MF-OWHM are unformatted files. The structure
of these files depends on the precision of the data in the program,
the compiler, and options in the Fortran write statement.  Any program
that use the unformatted files produced by MF-OWHM must read the files
in the same way they were written. For example, Zonebudget and Modpath
use unformatted budget files produced by MF-OWHM.  Current versions of
Zonebudget and Modpath automatically detect the precision of the data in
unformatted files and the run files provided by the USGS are compatible
with the structure of the unformatted files produced by this release of
MF-OWHM.

Another example of unformatted files is head files that are generated by
one MF-OWHM simulation and used in a following simulation as initial
heads.  Both simulations must be run using an executable version of
MF-OWHM that uses the same unformatted file structure.  MF-OWHM does
not automatically detect precision of the data in these files, so both
simulations must be run using a run file having the same precision 
(32-bit or 64-bit).

This issue of unformatted files is described here so that users will
be aware of the possibility of problems caused by unformatted files. 
Older versions of MODFLOW executables provided by the U.S. Geological
Survey (USGS) may produce different kinds of unformatted files.  The
current form of unformatted file has been used in USGS MODFLOW
executables starting with version 1.2 of MODFLOW-2000.


D. TESTING

Example data sets are provided to verify that MF-OWHM is correctly
installed, running on the system, and to provide example input structure for the 
various packages. The example model may also be looked at as examples of how to 
use MF-OWHM.  The directory MF_OWHM_v1_0\examples contains the input data for 
running each example set. Each example set originated from the various versions 
of MODFLOW that have been released and include new examples. 

The file structure under the examples folder contains test-run-(example set) for 
input files (including *.nam files), test-out-(example set) for output files,  
and for comparison to out file files the folder test-out-true-(example set) contains 
the original output files. The example models are described in their respective USGS 
reports (eg test-run described in MF2005 report and test-run-swr is described in the 
Surface Water Routing report).

For convenience for running the example problems a series of BASH and BATCH scripts, 
which run on UNIX and Windows, respectively, are provided in the folders 
bash_scripts_4_testing__Unix and batch_scripts_4_testing__Win. Due to linux 
permissions the BASH files may lose their executable bit and may requiring executing 
the "chmod" command to make them executable. For example running the command
"chmod *.sh" without quotes in the bash_scripts_4_testing__Unix directory will 
make the scripts executable for all users, groups and you. 

The automatic scripts do all call a common script that identifies which version of 
OWHM to run. The files are 0_SelectProgramUsedByBATCH.sh and 0_SelectProgramUsedByBATCH.bat 
to select which version of MF-OWHM to run by all the subsequent scripts. The scripts then 
are identified by what set of examples they use. A clean up script, 
0_Delete_test-out_Results.sh and 0_Delete_test-out_Results.bat, are provided and when 
invoked will delete all the files located in the various test-out-(example set) folders. 

On windows systems that contain the program WinMerge (winmerge.org), 
which is not endorsed by the USGS, can use the provided scripts for comparing 
the test-out-(example set) results to the test-out-true-(example set).

E. COMPILING

The executable files provided in MF_OWHM_v1_0\bin were created using the Intel Visual 
Fortran (14.0.3.202) and C++ (14.0.3.202) compilers.  Although executable versions 
of the program are provided, the source code is provided in the MF_OWHM_v1_0\src 
directory so that MF-OWHM can be recompiled if necessary. However, the USGS cannot provide 
assistance to those compiling MODFLOW. In general, the requirements are a Fortran compiler, 
a compatible C compiler, and the knowledge of using the compilers. For Linux/Unix users a 
GNU makefile is provided that is capable of compiling with GFORTRAN/GCC versions >4.9.1 or 
Intel Fortran >13. Caution is advised when using GFORTRAN as there are several compiler bugs
that are triggered when portions of the MF-OWHM code are accesses. The best way to validate
your version of GFORTRAN is to run the example problems to see if there is a successful run.
If there is not, then either do not use that specific feature of MF-OWHM or switch to the 
Intel Fortran compiler.

The makefile has been designed to be as automated as possible, but requires setting 5 variables.
These variables are explained within the makefile and just set the C and FORTRAN compiler, the 
optimization configuration (Release/Debug), to statically build the binary, and to compile with 
GMG included (only C portion of MODFLOW, thus removing the C-compiler component).

For compiling with the Intel compiler on windows the following options were 
used to create the provided executable versions of MF-OWHM are:

Debug command line for MF-OWHM (32-bit):
Compiler:
/nologo /debug:full /Od /warn:interfaces /fpe:0 /fp:source /module:"MF_OWHM_PROJ\obj_Win32_Debug\\" /object:"MF_OWHM_PROJ\obj_Win32_Debug\\" /Fd"MF_OWHM_PROJ\obj_Win32_Debug\vc110.pdb" /gen-dep /traceback /check:bounds /check:format /check:stack /libs:static /threads /dbglibs /c
Linker:
/OUT:".\bin\MF_OWHM_Win32_Debug.exe" /INCREMENTAL:NO /NOLOGO /NODEFAULTLIB:"libcmt.lib" /NODEFAULTLIB:"libcmtd.lib" /MANIFEST /MANIFESTFILE:"MF_OWHM_PROJ\obj_Win32_Debug\MF_OWHM_Win32_Debug.intermediate.manifest" /MANIFESTUAC:"level='asInvoker' uiAccess='false'" /DEBUG /PDB:"MF_OWHM_PROJ\obj_Win32_Debug\MF_OWHM_Win32_Debug.pdb" /SUBSYSTEM:CONSOLE /STACK:10485760,10485760 /IMPLIB:".\bin\MF_OWHM_Win32_Debug.lib" .\GMG_LIB\bin\GMG_Win32_Debug.lib
Release command line used for MF-OWHM (32-bit):
Compiler:
/nologo /O3 /Qip /assume:nocc_omp /fpe:0 /fp:source /module:"MF_OWHM_PROJ\obj_Win32_Release\\" /object:"MF_OWHM_PROJ\obj_Win32_Release\\" /Fd"MF_OWHM_PROJ\obj_Win32_Release\vc110.pdb" /libs:static /threads /c
Linker:
/OUT:".\bin\MF_OWHM_Win32.exe" /NOLOGO /NODEFAULTLIB:"libcmt.lib" /NODEFAULTLIB:"libcmtd.lib" /MANIFEST /MANIFESTFILE:"MF_OWHM_PROJ\obj_Win32_Release\MF_OWHM_Win32.intermediate.manifest" /MANIFESTUAC:"level='asInvoker' uiAccess='false'" /SUBSYSTEM:CONSOLE /STACK:10485760,10485760 /IMPLIB:".\bin\MF_OWHM_Win32.lib" .\GMG_LIB\bin\GMG_Win32.lib
Debug command line for MF-OWHM (64-bit):
Compiler:
/nologo /debug:full /Od /fpe:0 /fp:source /module:"MF_OWHM_PROJ\obj_x64_Debug\\" /object:"MF_OWHM_PROJ\obj_x64_Debug\\" /Fd"MF_OWHM_PROJ\obj_x64_Debug\vc110.pdb" /gen-dep /traceback /check:bounds /check:format /check:stack /libs:static /threads /dbglibs /c
Linker:
/OUT:".\bin\MF_OWHM_x64_Debug.exe" /INCREMENTAL:NO /NOLOGO /MANIFEST /MANIFESTFILE:"MF_OWHM_PROJ\obj_x64_Debug\MF_OWHM_x64_Debug.intermediate.manifest" /MANIFESTUAC:"level='asInvoker' uiAccess='false'" /DEBUG /PDB:"MF_OWHM_PROJ\obj_x64_Debug\MF_OWHM_x64_Debug.pdb" /SUBSYSTEM:CONSOLE /STACK:10485760,10485760 /IMPLIB:".\bin\MF_OWHM_x64_Debug.lib" .\GMG_LIB\bin\GMG_x64_Debug.lib

The GMG solver is included in the executable version of MF-OWHM. 

When the GMG solver is to be included, a C compiler and mixed language code must be created.
If a compatible C compiler is not available, the GMG solver can be removed so that only a 
Fortran compiler is required.  File nogmg.txt in the src directory contains instructions for
removing GMG from MODFLOW. To compile without GMG, you must first remove from the compilation
gmg7_NWT_dble.f and rename nogmg.txt to nogmg.f and compile with that. That file then removes 
any fortran linkages to C.



F. REFERENCES

Anderman, E.R., and Hill, M.C., 2000, MODFLOW-2000, the U.S. Geological
Survey modular ground-water model -- Documentation of the Hydrogeologic-
Unit Flow (HUF) Package: U.S. Geological Survey Open-File Report 00-342,
89 p.

Anderman, E.R., Kipp, K.L., Hill, M.C., Valstar, Johan, and Neupauer,
R.M., 2002, MODFLOW-2000, the U.S. Geological Survey modular ground-water
model -- Documentation of the Model-Layer Variable-Direction Horizontal
Anisotropy (LVDA) capability of the Hydrogeologic-Unit Flow (HUF) Package:
U.S. Geological Survey Open-File Report 02-409, 60 p.

Anderman, E.R., and Hill, M.C., 2003, MODFLOW-2000, the U.S. Geological
Survey modular ground-water model -- Three additions to the
Hydrogeologic-Unit Flow (HUF) Package: Alternative storage for the
uppermost active cells, Flows in hydrogeologic units, and the
Hydraulic-coductivity depth-dependence (KDEP) capability:U.S. Geological
Survey Open-File Report 03-347, 36 p.

Bakker, Mark, Schaars, Frans, Hughes, J.D., Langevin, C.D., and Dausman, A.M., 2013, 
Documentation of the seawater intrusion (SWI2) package for MODFLOW: 
U.S. Geological Survey Techniques and Methods, book 6, chap. A46, 47 p. Available online at http://pubs.usgs.gov/tm/6a46/

Banta, E.R., 2000, MODFLOW-2000, the U.S. Geological Survey modular
ground-water model -- documentation of packages for simulating
evapotranspiration with a segmented function (ETS1) and drains with return
flow (DRT1): U.S. Geological Survey Open-File Report 00-466, 127 p.

Fenske, J.P., Leake, S.A., and Prudic, D.E., 1996, Documentation of a
computer program (RES1) to simulate leakage from reservoirs using the
modular finite-difference ground-water flow model (MODFLOW): U.S.
Geological Survey Open-File Report 96-364, 51 p.

Halford, K.J. and Hanson, R.T., 2002, User guide for the drawdown-limited,
multi-node well (MNW) package for the U.S. Geological Survey's modular
three-dimensional finite-difference ground-water flow model, versions
MODFLOW-96 and MODFLOW-2000: U.S. Geological Survey Open-File Report
02-293, 33 p.

Hanson, R.T. and Leake, S.A., 1999, Documentation of HYDMOD, a program for
extracting and processing time-series data from the U.S. Geological
Survey's modular three-dimensional finite-difference ground-water flow
model: U.S. Geological Survey Open-File Report 98-564, 57 p.

Hanson, R.T., Boyce, S.E., Schmid, Wolfgang, Hughes, J.D., Mehl, S.M., Leake, S.A., Maddock, Thomas, III, 
and Niswonger, R.G., 2014, One-Water Hydrologic Flow Model (MODFLOW-OWHM): U.S. Geological Survey  
Techniques and Methods 6-A51,120 p., http://dx.doi.org/10.3133/tm6A51.

Harbaugh, A.W., 2005, MODFLOW-2005, the U.S. Geological Survey modular
ground-water model -- the Ground-Water Flow Process: U.S. Geological Survey
Techniques and Methods 6-A16, variously p.

Hill, M.C., 1990, Preconditioned conjugate-gradient 2
(PCG2), a computer program for solving ground-water flow
equations: U.S. Geological Survey Water-Resources
Investigations Report 90?4048, 43 p.

Hill, M.C., Banta, E.R., Harbaugh, A.W., and Anderman, E.R., 2000,
MODFLOW-2000, the U.S. Geological Survey modular ground-water model --
User guide to the Observation, Sensitivity, and Parameter-Estimation
Processes and three post-processing programs: U.S. Geological Survey
Open-File Report 00-184, 209 p.

Hoffmann, Jorn, Leake, S.A., Galloway, D.L., and Wilson, A.M., 2003,
MODFLOW-2000 ground-water model -- User guide to the Subsidence and
Aquifer-System Compaction (SUB) Package: U.S. Geological Survey Open-File
Report 03-233, 46 p.

Hsieh, P.A., and Freckleton, J.R., 1993, Documentation of a
computer program to simulate horizontal-flow barriers
using the U.S. Geological Survey?s modular threedimensional
finite-difference ground-water flow model:
U.S. Geological Survey Open-File Report 92?477, 32 p.

Hughes, J.D., Langevin, C.D., Chartier, K.L., and White, J.T., 2012, Documentation 
of the Surface-Water Routing (SWR1) Process for modeling surface-water flow with the 
U.S. Geological Survey Modular Ground-Water Model (MODFLOW-2005): U.S. Geological 
Survey Techniques and Methods, book 6, chap. A40 (Version 1.0), 113 p. Available online at http://pubs.usgs.gov/tm/6a40/

Konikow, L.F., Hornberger, G.Z., Halford, K.J., and Hanson, K.J., 2009, Revised Multi-Node Well
(MNW2) Package for MODFLOW Groundwater Flow Model: U.S. Geological Survey Techniques in Water 
Resources Investigations, Book 6, Chapter A30, 67p. 

Leake, S.A. and Prudic, D.E., 1991, Documentation of a computer program
to simulate aquifer-system compaction using the modular
finite-difference ground-water flow model: U.S. Geological Survey
Techniques of Water-Resources Investigations, Book 6, Chapter A2, 68 p.

Leake, S.A., and Lilly, M.R., 1997, Documentation of a computer program
(FHB1) for assignment of transient specified-flow and specified-head
boundaries in applications of the modular finite- difference ground-water
flow model (MODFLOW): U.S. Geological Survey Open-File Report 97-571, 50 p.

Leake, S.A. and Galloway, D.L., 2007, MODFLOW ground-water model -- User
guide to the Subsidence and Aquifer-System Compaction Package (SUB-WT) for
water-table aquifers: U.S. Geological Survey Techniques and Methods 6-A23,
42 p.

Maddock III, T., Baird, K.J., Hanson, R.T., Schmid, Wolfgang, and Ajami, H., 
2012, RIP-ET: A riparian evapotranspiration package for MODFLOW-2005, U.S. 
Geological Survey Techniques and Methods 6-A39 p. 39

Merritt, M.L., and Konikow, K.F., 2000, Documentation of a computer program to simulate 
lake-aquifer interaction using the MODFLOW ground-water flow model and the MOC3D 
solute-transport model: Water-Resources Investigations Report 00-4167, 146 p. Available online at http://pubs.er.usgs.gov/publication/wri004167

Mehl, Steffen, and Hill, M.C., 2005, MODFLOW-2005, The U.S. Geological Survey Modular Groundwater Model- 
Documentation of shared node local grid refinement (LGR) and the boundary flow and head (BFH) package: 
U.S. Geological Survey Techniques and Methods book 6, chap. A12, 68 p.

Mehl, Steffen, and Hill, M.C., 2007, MODFLOW-2005, The U.S. Geological Survey modular groundwater model-
Documentation of the multiple-refined-areas capability of local grid refinement (LGR) and the boundary 
flow and head (BFH) package: U.S. Geological Survey Techniques and Methods book 6, chap. A21, 13 p., 
http://water.usgs.gov/nrp/gwsoftware/modflow2005_lgr/mflgr.html.

Mehl, S.W., and Hill, M.C., 2013, MODFLOW?LGR?Documentation of ghost node local grid 
refinement (LGR2) for multiple areas and the boundary flow and head (BFH2) package: 
U.S. Geological Survey Techniques and Methods book 6, chap. A44, 43 p., 
http://pubs.usgs.gov/tm/6A44/.

Naff, R.L. and Banta, E.R., 2008, The U.S. Geological Survey modular ground-water model-PCGN: 
A preconditioned conjugate gradient solver with improved nonlinear control: U.S. Geological Survey 
Open-File Report 2008?1331, 35 p.

Niswonger, R.G., and Prudic, D.E., 2005, Documentation of the
Streamflow-Routing (SFR2) Package to include unsaturated flow beneath
streams -- a modification to SFR1: U.S. Geological Techniques and Methods
Book 6, Chapter A13, 47 p.

Niswonger, R.G., Prudic, D.E., and Regan, R.S., 2006, Documentation of
the Unsaturated-Zone Flow (UZF1) Package for modeling unsaturated flow
between the land surface and the water table with MODFLOW-2005: U.S.
Geological Techniques and Methods Book 6, Chapter A19, 62 p.
  
Niswonger, R.G., Panday, Sorab, and Ibaraki, Motomu, 2011, MODFLOW-NWT, A Newton
formulation for MODFLOW-2005: U.S. Geological Survey Techniques and Methods 6-A37, 44 p. Available online at http://pubs.usgs.gov/tm/tm6a37/

Prudic, D.E., 1989, Documentation of a computer program to simulate
stream-aquifer relations using a modular, finite-difference, ground-water
flow model: U.S. Geological Survey Open-File Report 88-729, 113 p.

Prudic, D.E., Konikow, L.F., and Banta, E.R., 2004, A new Streamflow-Routing
(SFR1) Package to simulate stream-aquifer interaction with MODFLOW-2000:
U.S. Geological Survey Open File Report 2004-1042, 95 p.

Schmid, W., Hanson, R.T., Maddock III, T.M., and Leake, S.A., 2006, 
User?s guide for the Farm Package (FMP1) for the U.S. Geological Survey?s 
modular three-dimensional finite-difference ground-water flow model, MODFLOW-2000: 
U.S. Geological Survey Techniques and Scientific Methods Report Book 6, Chapter A17, 
127p. (http://pubs.usgs.gov/tm/2006/tm6A17/)

Schmid, Wolfgang, and Hanson R.T., 2009, The Farm Process Version 2 (FMP2) for 
MODFLOW-2005 - Modifications and Upgrades to FMP1: U.S. Geological Survey Techniques 
in Water Resources Investigations, Book 6, Chapter A32, 102p. (http://pubs.usgs.gov/tm/tm6a32/)

Wilson, J.D. and Naff, R.L., 2004, The U.S. Geological Survey modular ground-water model-
GMG linear equation solver package documentation: U.S. Geological Survey Open-File Report 
2004-1261, 47 p.

Zheng, Chunmiao, Hill, M.C., and Hsieh, P.A., 2001, MODFLOW-2000, the U.S.
Geological Survey modular ground-water model - User guide to the LMT6
package, the linkage with MT3DMS for multi-species mass transport modeling:
U.S. Geological Survey Open-File Report 01-82, 43 p.