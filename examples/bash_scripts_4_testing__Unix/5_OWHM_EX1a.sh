#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN TEST-OUT

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

source ./0_SelectProgramUsedByBASH.sh

cd  ../test-run-owhm

$OWHM OWHM_EX1a.nam

mv -f  ACR_OPT.OUT    ../test-out-owhm/A/FMPOUT/ACR_OPT.OUT
mv -f  ET_ARRAY.OUT   ../test-out-owhm/A/FMPOUT/ET_ARRAY.OUT
mv -f  FB_DETAILS.OUT ../test-out-owhm/A/FMPOUT/FB_DETAILS.OUT
mv -f  FDS.OUT        ../test-out-owhm/A/FMPOUT/FDS.OUT
mv -f  ROUT.OUT       ../test-out-owhm/A/FMPOUT/ROUT.OUT

mv -f  PARAM_HKC_G1_L1.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L1.txt
mv -f  PARAM_HKC_G1_L2.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L2.txt
mv -f  PARAM_HKC_G1_L3.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L3.txt
mv -f  PARAM_HKC_G1_L4.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L4.txt
mv -f  PARAM_HKC_G1_L5.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L5.txt
mv -f  PARAM_HKC_G1_L6.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L6.txt
mv -f  PARAM_HKC_G1_L7.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L7.txt
mv -f  PARAM_HKR_G1_L1.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L1.txt
mv -f  PARAM_HKR_G1_L2.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L2.txt
mv -f  PARAM_HKR_G1_L3.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L3.txt
mv -f  PARAM_HKR_G1_L4.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L4.txt
mv -f  PARAM_HKR_G1_L5.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L5.txt
mv -f  PARAM_HKR_G1_L6.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L6.txt
mv -f  PARAM_HKR_G1_L7.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L7.txt
mv -f  PARAM_Ss_G1_L1.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L1.txt 
mv -f  PARAM_Ss_G1_L2.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L2.txt 
mv -f  PARAM_Ss_G1_L3.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L3.txt 
mv -f  PARAM_Ss_G1_L4.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L4.txt 
mv -f  PARAM_Ss_G1_L5.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L5.txt 
mv -f  PARAM_Ss_G1_L6.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L6.txt 
mv -f  PARAM_Ss_G1_L7.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L7.txt 
mv -f  PARAM_Sy_G1_L1.txt      ../test-out-owhm/A/PARAM/PARAM_Sy_G1_L1.txt 
mv -f  PARAM_VKA_G1_L1.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L1.txt
mv -f  PARAM_VKA_G1_L2.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L2.txt
mv -f  PARAM_VKA_G1_L3.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L3.txt
mv -f  PARAM_VKA_G1_L4.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L4.txt
mv -f  PARAM_VKA_G1_L5.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L5.txt
mv -f  PARAM_VKA_G1_L6.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L6.txt
mv -f  PARAM_VKA_G1_L7.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L7.txt

cd "$SHELLDIR"

echo
echo  
echo "***MF-OWHM TEST NOW COMPLETE***"
echo " *CHECK LST FILES AND RESULTS*"
echo  
echo  

read -p "Press [Enter] to end script  "
