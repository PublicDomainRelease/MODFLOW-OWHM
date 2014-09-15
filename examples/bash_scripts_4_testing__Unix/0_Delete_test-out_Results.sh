#!/bin/bash

# RUN EACH TEST CASE USING MODFLOW-OWHM
# RESULTS WILL BE STORED IN TEST-OUT

SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SHELLDIR"

cd ../test-out
rm -r * 2>/dev/null

cd ../test-out-nwt
rm -r * 2>/dev/null

cd ../test-out-swi
rm -r * 2>/dev/null

cd ../test-out-swr
rm -r * 2>/dev/null

cd ../test-out-fmp
rm -r * 2>/dev/null

cd ../test-out-rip
rm -r * 2>/dev/null

cd ../test-out-lgr
rm -r * 2>/dev/null

cd ../test-out-owhm/A
rm -r * 2>/dev/null
mkdir FMPOUT
mkdir SWROUT
mkdir PARAM

cd ../B
rm -r * 2>/dev/null
mkdir FMPOUT
mkdir SWROUT
mkdir PARAM
cd ../

cd ../test-out-owhm-lgr/fmp-lgr-lpf
rm -r * 2>/dev/null
cd ../

cd "$SHELLDIR"
    
echo 
echo "test-out directories have been cleaned"   
echo

if [ "$1" != "nopause" ]
then
   read -p "Press [Enter] to end script  "
fi
