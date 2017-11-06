#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2018 Rolf Mertig
# Copyright (C) 1997-2018 Frederik Orellana
# Copyright (C) 2014-2018 Vladyslav Shtabovenko

# Description:

# Checks FeynCalc using real-life calculations.

# Stop if any of the examples fails
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

if [ -z ${MATH+x} ]; then MATH=math; else echo $MATH; fi



##Misc Examples
#$MATH -nopromt -script ../FeynCalc/Examples/Misc/Integrate2.m &&

##Tdec Examples
#$MATH -nopromt -script ../FeynCalc/Examples/Tdec/GrossNeveuMultiloopTID.m &&

#QED Examples

for exFile in 'ElAel-ElAel.m' 'ElAel-GaGa.m' 'ElAel-MuAmu.m' 'ElEl-ElEl.m' 'ElGa-ElGa.m' 'ElMu-ElMu.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/QED/Tree/$exFile
done

for exFile in 'El-El.m' 'El-GaEl.m' 'Ga.m' 'Ga-Ga.m' 'Ga-GaGa.m' 'PiToGaGa.m' 'Renormalization.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/QED/OneLoop/$exFile
done

#QCD Examples

for exFile in 'QiQibar-QiQibar.m' 'QiQibar-QjQjbar.m' 'QiQi-QiQi.m' 'QiQjbar-QiQjbar.m' 'QiQj-QiQj.m' 'QQbar-GaGa.m' 'QQbar-GlGl.m' \
'QGl-QGl.m' 'GlGl-QQbar.m' 'GlGl-GlGl.m' 'QQbar-ElAel.m' 'ElAel-QQbar.m' 'GaGl-QQbar.m' 'QGa-GlQ.m' 'QQbar-GaGl.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/QCD/Tree/$exFile
done

for exFile in 'Q-Q.m' 'Gl-Gl.m' 'Gh-Gh.m' 'Renormalization.m' 'GlGl-Gl.m' 'GhGl-Gh.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/QCD/OneLoop/$exFile
done

#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDABJAxialAnomaly.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDElectronGMinusTwoOneLoop.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDElectronSelfEnergyOneLoop.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDPhotonSelfEnergyOneLoop.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDBhabhaScatteringTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDComptonScatteringTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDElectronMuonScatteringTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDMoellerScatteringTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDMuonProductionTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDPairAnnihilationTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDOnePhotonTadpoleOneLoop.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QED/QEDThreePhotonDiagramsOneLoop.m &&

##EW Examples
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWMuonDecayTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWElectronMuonNeutrinoToMuonElectronNeutrinoTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWElectronAntineutrinoElectronToMuonAntineutrinoMuonTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWElectronAntineutrinoElectronToAntiupQuarkDownQuarkTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWNeutrinoDownQuarkToLeptonUpQuarkTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWWBosonToElectronElectronAntineutrino.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWHiggsToTwoFermionsTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWHiggsToWPlusWMinusTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWHiggsToZZTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWWBosonToQQbarp.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWT-QW.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWZ-ffbar.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWHiggsToTwoGluonsOneLoop.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWAntiupQuarkDownQuarkToElectronAntielectronNeutrinoTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWUpQuarkDownAntiquarkToPositronElectronNeutrinoTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/EW/EWQQBarToZZTree.m &&

##QCD Examples
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDGammaStarGToQiQBari.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiGammaStarToQiGTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQBariToGammaStarGTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQjToQiQjTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQBarjToQiQBarjTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQiToQiQiTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQBariToQjQBarjTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQBariToQiQBariTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQiQBariToGGTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDGGToQiQBariTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDGQiToGQi.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDGGToGGTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQQBarToElectronPositronTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQQBarToGammaGammaTree.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDQuarkSelfEnergyOneLoop.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDGluonSelfEnergyOneLoop.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/QCD/QCDGhostSelfEnergyTwoLoops.m &&
#$MATH -nopromt -script ../FeynCalc/Examples/FeynRules/QCDBGF/PureYMSelfEnergyInQCDBGFAtOneLoop.m

notify-send --urgency=low -i "$([ $? = 0 ] && echo sunny || echo error)" "Finished running examples for FeynCalc."
