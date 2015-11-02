#!/bin/bash

# This small bash script provides a nice way to check that
# FeynCalc is working properly using real-life examples.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

if [ -z ${MATH+x} ]; then MATH=math; else echo $MATH; fi

#ToTFI
$MATH -nopromt -script Checks/CheckToTFI.m &&
#Tensor decompositions
$MATH -nopromt -script ../FeynCalc/Examples/TID/VerifyTIDLOneLoop.m &&
$MATH -nopromt -script ../FeynCalc/Examples/TID/VerifyTIDLTwoLoop.m &&
$MATH -nopromt -script ../FeynCalc/Examples/TID/VerifyTIDLThreeLoop.m &&
$MATH -nopromt -script ../FeynCalc/Examples/TID/VerifyTIDLFourLoop.m
