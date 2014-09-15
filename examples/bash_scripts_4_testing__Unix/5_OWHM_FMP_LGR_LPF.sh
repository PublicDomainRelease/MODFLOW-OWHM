#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN TEST-OUT

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

source ./0_SelectProgramUsedByBASH.sh

cd ../test-run-owhm-lgr/fmp-lgr-lpf

$OWHM OWHM_EX2.lgr

mv -f  FB_DETAILS.OUT      ../../test-out-owhm-lgr/fmp-lgr-lpf/FB_DETAILS.OUT
mv -f  FB_DETAILS2.OUT     ../../test-out-owhm-lgr/fmp-lgr-lpf/FB_DETAILS2.OUT
mv -f  FB_DETAILS4.OUT     ../../test-out-owhm-lgr/fmp-lgr-lpf/FB_DETAILS4.OUT
mv -f  FDS.OUT             ../../test-out-owhm-lgr/fmp-lgr-lpf/FDS.OUT
mv -f  FDS2.OUT            ../../test-out-owhm-lgr/fmp-lgr-lpf/FDS2.OUT
mv -f  FDS4.OUT            ../../test-out-owhm-lgr/fmp-lgr-lpf/FDS4.OUT
mv -f  FWELLS.OUT          ../../test-out-owhm-lgr/fmp-lgr-lpf/FWELLS.OUT
mv -f  ROUT.OUT            ../../test-out-owhm-lgr/fmp-lgr-lpf/ROUT.OUT
mv -f  ROUT2.OUT           ../../test-out-owhm-lgr/fmp-lgr-lpf/ROUT2.OUT
mv -f  ROUT4.OUT           ../../test-out-owhm-lgr/fmp-lgr-lpf/ROUT4.OUT
mv -f  t.ByNode            ../../test-out-owhm-lgr/fmp-lgr-lpf/t.ByNode
mv -f  t.Qsum              ../../test-out-owhm-lgr/fmp-lgr-lpf/t.Qsum
mv -f  t.wl1               ../../test-out-owhm-lgr/fmp-lgr-lpf/t.wl1

cd "$SHELLDIR"

echo
echo  
echo "***MF-OWHM-LGR TEST NOW COMPLETE***"
echo " **CHECK LST FILES AND RESULTS**"
echo  
echo  

read -p "Press [Enter] to end script  "
