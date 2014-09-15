#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN TEST-OUT

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

source ./0_SelectProgramUsedByBASH.sh

cd  ../test-run-owhm

$OWHM OWHM_EX1a.nam

mv -f  ET_ARRAY.OUT   ../test-out-owhm/A/FMPOUT/ET_ARRAY.OUT                       2>/dev/null
mv -f  FB_DETAILS.OUT ../test-out-owhm/A/FMPOUT/FB_DETAILS.OUT                     2>/dev/null
mv -f  FDS.OUT        ../test-out-owhm/A/FMPOUT/FDS.OUT                            2>/dev/null
mv -f  ROUT.OUT       ../test-out-owhm/A/FMPOUT/ROUT.OUT                           2>/dev/null

mv -f  PARAM_HKC_G1_L1.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L1.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L2.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L2.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L3.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L3.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L4.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L4.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L5.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L5.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L6.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L6.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L7.txt     ../test-out-owhm/A/PARAM/PARAM_HKC_G1_L7.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L1.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L1.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L2.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L2.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L3.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L3.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L4.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L4.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L5.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L5.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L6.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L6.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L7.txt     ../test-out-owhm/A/PARAM/PARAM_HKR_G1_L7.txt        2>/dev/null
mv -f  PARAM_Ss_G1_L1.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L1.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L2.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L2.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L3.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L3.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L4.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L4.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L5.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L5.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L6.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L6.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L7.txt      ../test-out-owhm/A/PARAM/PARAM_Ss_G1_L7.txt         2>/dev/null
mv -f  PARAM_Sy_G1_L1.txt      ../test-out-owhm/A/PARAM/PARAM_Sy_G1_L1.txt         2>/dev/null
mv -f  PARAM_VKA_G1_L1.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L1.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L2.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L2.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L3.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L3.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L4.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L4.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L5.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L5.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L6.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L6.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L7.txt     ../test-out-owhm/A/PARAM/PARAM_VKA_G1_L7.txt        2>/dev/null

$OWHM OWHM_EX1b.nam

mv -f  ET_ARRAY.OUT      ../test-out-owhm/B/FMPOUT/ET_ARRAY.OUT                    2>/dev/null
mv -f  FB_DETAILS.OUT    ../test-out-owhm/B/FMPOUT/FB_DETAILS.OUTL                 2>/dev/null
mv -f  FDS.OUT           ../test-out-owhm/B/FMPOUT/FDS.OUT                         2>/dev/null
mv -f  ROUT.OUT          ../test-out-owhm/B/FMPOUT/ROUT.OUT                        2>/dev/null
mv -f  RED_FMP_PMP.OUT   ../test-out-owhm/B/FMPOUT/RED_FMP_PMP.OUT                 2>/dev/null
#mv -f  ACR_OPT.OUT       ../test-out-owhm/B/FMPOUT/ACR_OPT.OUT                     2>/dev/null

mv -f  PARAM_HKC_G1_L1.txt     ../test-out-owhm/B/PARAM/PARAM_HKC_G1_L1.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L2.txt     ../test-out-owhm/B/PARAM/PARAM_HKC_G1_L2.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L3.txt     ../test-out-owhm/B/PARAM/PARAM_HKC_G1_L3.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L4.txt     ../test-out-owhm/B/PARAM/PARAM_HKC_G1_L4.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L5.txt     ../test-out-owhm/B/PARAM/PARAM_HKC_G1_L5.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L6.txt     ../test-out-owhm/B/PARAM/PARAM_HKC_G1_L6.txt        2>/dev/null
mv -f  PARAM_HKC_G1_L7.txt     ../test-out-owhm/B/PARAM/PARAM_HKC_G1_L7.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L1.txt     ../test-out-owhm/B/PARAM/PARAM_HKR_G1_L1.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L2.txt     ../test-out-owhm/B/PARAM/PARAM_HKR_G1_L2.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L3.txt     ../test-out-owhm/B/PARAM/PARAM_HKR_G1_L3.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L4.txt     ../test-out-owhm/B/PARAM/PARAM_HKR_G1_L4.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L5.txt     ../test-out-owhm/B/PARAM/PARAM_HKR_G1_L5.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L6.txt     ../test-out-owhm/B/PARAM/PARAM_HKR_G1_L6.txt        2>/dev/null
mv -f  PARAM_HKR_G1_L7.txt     ../test-out-owhm/B/PARAM/PARAM_HKR_G1_L7.txt        2>/dev/null
mv -f  PARAM_Ss_G1_L1.txt      ../test-out-owhm/B/PARAM/PARAM_Ss_G1_L1.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L2.txt      ../test-out-owhm/B/PARAM/PARAM_Ss_G1_L2.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L3.txt      ../test-out-owhm/B/PARAM/PARAM_Ss_G1_L3.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L4.txt      ../test-out-owhm/B/PARAM/PARAM_Ss_G1_L4.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L5.txt      ../test-out-owhm/B/PARAM/PARAM_Ss_G1_L5.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L6.txt      ../test-out-owhm/B/PARAM/PARAM_Ss_G1_L6.txt         2>/dev/null
mv -f  PARAM_Ss_G1_L7.txt      ../test-out-owhm/B/PARAM/PARAM_Ss_G1_L7.txt         2>/dev/null
mv -f  PARAM_Sy_G1_L1.txt      ../test-out-owhm/B/PARAM/PARAM_Sy_G1_L1.txt         2>/dev/null
mv -f  PARAM_VKA_G1_L1.txt     ../test-out-owhm/B/PARAM/PARAM_VKA_G1_L1.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L2.txt     ../test-out-owhm/B/PARAM/PARAM_VKA_G1_L2.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L3.txt     ../test-out-owhm/B/PARAM/PARAM_VKA_G1_L3.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L4.txt     ../test-out-owhm/B/PARAM/PARAM_VKA_G1_L4.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L5.txt     ../test-out-owhm/B/PARAM/PARAM_VKA_G1_L5.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L6.txt     ../test-out-owhm/B/PARAM/PARAM_VKA_G1_L6.txt        2>/dev/null
mv -f  PARAM_VKA_G1_L7.txt     ../test-out-owhm/B/PARAM/PARAM_VKA_G1_L7.txt        2>/dev/null



cd "$SHELLDIR"
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
cd ../test-run-owhm-lgr/fmp-lgr-nwt

$OWHM OWHM_EX2.lgr

mv -f  FB_DETAILS.OUT      ../../test-out-owhm-lgr/fmp-lgr-nwt/FB_DETAILS.OUT
mv -f  FB_DETAILS2.OUT     ../../test-out-owhm-lgr/fmp-lgr-nwt/FB_DETAILS2.OUT
mv -f  FB_DETAILS4.OUT     ../../test-out-owhm-lgr/fmp-lgr-nwt/FB_DETAILS4.OUT
mv -f  FDS.OUT             ../../test-out-owhm-lgr/fmp-lgr-nwt/FDS.OUT
mv -f  FDS2.OUT            ../../test-out-owhm-lgr/fmp-lgr-nwt/FDS2.OUT
mv -f  FDS4.OUT            ../../test-out-owhm-lgr/fmp-lgr-nwt/FDS4.OUT
mv -f  FWELLS.OUT          ../../test-out-owhm-lgr/fmp-lgr-nwt/FWELLS.OUT
mv -f  ROUT.OUT            ../../test-out-owhm-lgr/fmp-lgr-nwt/ROUT.OUT
mv -f  ROUT2.OUT           ../../test-out-owhm-lgr/fmp-lgr-nwt/ROUT2.OUT
mv -f  ROUT4.OUT           ../../test-out-owhm-lgr/fmp-lgr-nwt/ROUT4.OUT
mv -f  RED_FMP_PMP.OUT     ../../test-out-owhm-lgr/fmp-lgr-nwt/RED_FMP_PMP.OUT
mv -f  RED_FMP_PMP2.OUT    ../../test-out-owhm-lgr/fmp-lgr-nwt/RED_FMP_PMP2.OUT
mv -f  RED_FMP_PMP4.OUT    ../../test-out-owhm-lgr/fmp-lgr-nwt/RED_FMP_PMP4.OUT
mv -f  t.ByNode            ../../test-out-owhm-lgr/fmp-lgr-nwt/t.ByNode
mv -f  t.Qsum              ../../test-out-owhm-lgr/fmp-lgr-nwt/t.Qsum
mv -f  t.wl1               ../../test-out-owhm-lgr/fmp-lgr-nwt/t.wl1


cd "$SHELLDIR"

echo
echo 
echo "****  OWHM   TESTS   HAVE  FINISHED****"
echo " ****CHECK LST FILES AND RESULTS****   "
echo "   *******************************     "
echo "     ***************************       "
echo "       ***********************         "
echo "         *******************           "
echo "           ***************             "
echo "            ************               "
echo "              ********                 "
echo "                ****                   "
echo "                 **                    "
echo     
echo   

if [ "$1" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi
