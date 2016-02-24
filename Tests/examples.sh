#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2016 Rolf Mertig
# Copyright (C) 1997-2016 Frederik Orellana
# Copyright (C) 2014-2016 Vladyslav Shtabovenko

# Description:

# Checks FeynCalc using real-life calculations.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

if [ -z ${MATH+x} ]; then MATH=math; else echo $MATH; fi

#Misc Examples
$MATH -nopromt -script ../FeynCalc/Examples/Misc/Integrate2.m &&

#QED Examples
$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDBhabhaScatteringTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDComptonScatteringTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDElectronMuonScatteringTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDMoellerScatteringTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDMuonProductionTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDPairAnnihilationTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDOnePhotonTadpoleOneLoop.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDThreePhotonDiagramsOneLoop.m &&

#EW Examples
$MATH -nopromt -script ../FeynCalc/Examples/EW/EWMuonDecayTree.m &&

#QCD Examples
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQjToQiQjTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQBarjToQiQBarjTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQiToQiQiTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQBariToQjQBarjTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQBariToQiQBariTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQBariToGGTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDGGToQiQBariTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDGQiToGQi.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDGGToGGTree.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQuarkSelfEnergyOneLoop.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDGluonSelfEnergyOneLoop.m &&
$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDGhostSelfEnergyTwoLoops.m
