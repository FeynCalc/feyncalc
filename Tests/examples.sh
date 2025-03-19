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



MATH=$1


##Misc Examples
#$MATH -nopromt -script ../FeynCalc/Examples/Misc/Integrate2.m &&

##Tdec Examples
#$MATH -nopromt -script ../FeynCalc/Examples/Tdec/GrossNeveuMultiloopTID.m &&

#MSSM Examples
#-------------------------------------------------------------------------------
for exFile in 'MnelEl-MnelEl.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/MSSM/Tree/Mathematica/$exFile
done

#Phi3 Examples
#-------------------------------------------------------------------------------
for exFile in 'Renormalization.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/Phi3/OneLoop/Mathematica/$exFile
done


#Phi^4 Examples
#-------------------------------------------------------------------------------
for exFile in 'PhiPhi-PhiPhi.m' 'Renormalization.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/Phi4/OneLoop/Mathematica/$exFile
done
#-------------------------------------------------------------------------------


#QED Examples
#-------------------------------------------------------------------------------
for exFile in 'ElAel-ElAel.m' 'ElAel-GaGa.m' 'ElAel-MuAmu.m' 'ElEl-ElEl.m' \
'ElGa-ElGa.m' 'ElMu-ElMu.m' 'Ga-MuAmu.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/QED/Tree/Mathematica/$exFile
done

for exFile in 'El-El.m' 'El-GaEl.m' 'Ga.m' 'Ga-Ga.m' 'Ga-GaGa.m' 'Ga-GaGaGaGa.m' \
'PiToGaGa.m' 'Renormalization.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/QED/OneLoop/Mathematica/$exFile
done
#-------------------------------------------------------------------------------


#QCD Examples
#-------------------------------------------------------------------------------
for exFile in 'ElAel-QQbar.m' 'GaGl-QQbar.m' 'Ga-QQbar.m' 'Ga-QQbarGl.m' \
'GlGl-GlGl.m' 'GlGl-QQbar.m' 'MuAmu-QQbar.m' 'QGa-GlQ.m' 'QGl-QGl.m' 'QGl-QGl-2.m' \
'QiQibar-QiQibar.m' 'QiQibar-QjQjbar.m' 'QiQi-QiQi.m' 'QiQjbar-QiQjbar.m' \
'QiQj-QiQj.m' 'QQbar-ElAel.m' 'QQbar-GaGa.m' 'QQbar-GaGl.m' 'QQbar-GlGl.m' 'QQbar-GlGl-2.m' \
'QQbar-MuAmu.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/QCD/Tree/Mathematica/$exFile
done

for exFile in 'Gh-Gh.m' 'GhGl-Gh.m' 'Gl-Gl.m' 'Gl-Gl-BackgroundFieldGauge.m' \
'Gl-GlGl.m' 'Q-Q.m' 'Renormalization.m' 'RenormalizationMassless.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/QCD/OneLoop/Mathematica/$exFile
done

for exFile in 'Gh-Gh.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/QCD/TwoLoops/Mathematica/$exFile
done
#-------------------------------------------------------------------------------


##EW Examples
#-------------------------------------------------------------------------------
for exFile in 'AnelEl-AnmuMu.m' 'AnelEl-QubarQd.m' 'AnelEl-WW.m' 'AnelEl-ZZ.m' \
'ElNmu-MuNel.m' 'H-FFbar.m' 'H-WW.m' 'H-ZZ.m' 'Mu-ElAnelNmu.m' 'NleQdt-LeQut.m' \
'QQbar-ZZ.m' 'Qt-QbW.m' 'QuQdbar-AelNel.m' 'QutbarQdt-NelAnel.m' 'W-ElAnel.m' \
'W-QiQjbar.m' 'Z-FFbar.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/EW/Tree/Mathematica/$exFile
done

for exFile in 'H-GG.m'

do
  echo
  echo -e "* \c"
  $MATH -nopromt -script ../FeynCalc/Examples/EW/OneLoop/Mathematica/$exFile
done
#-------------------------------------------------------------------------------


notify-send --urgency=low -i "$([ $? = 0 ] && echo sunny || echo error)" "Finished running examples for FeynCalc."
