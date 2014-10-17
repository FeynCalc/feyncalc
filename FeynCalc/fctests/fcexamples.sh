#!/bin/bash

# This small bash script provides a nice way to check that
# FeynCalc is working properly using real-life examples.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

#QED Examples
math -nopromt -script ../fcexamples/QED/QEDBhabhaScatteringTree.m
math -nopromt -script ../fcexamples/QED/QEDComptonScatteringTree.m
math -nopromt -script ../fcexamples/QED/QEDElectronMuonScatteringTree.m
math -nopromt -script ../fcexamples/QED/QEDMoellerScatteringTree.m
math -nopromt -script ../fcexamples/QED/QEDMuonProductionTree.m
math -nopromt -script ../fcexamples/QED/QEDPairAnnihilationTree.m

#QCD Examples
math -nopromt -script ../fcexamples/QCD/QCDTwoLoopGhostSelfEnergy.m
