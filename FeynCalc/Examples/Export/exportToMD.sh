#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2020 Rolf Mertig
# Copyright (C) 1997-2020 Frederik Orellana
# Copyright (C) 2014-2020 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc examples to MarkDown

# Usage examples

# ./exportToMD.sh math11.3 /media/Data/Projects/VS/feyncalc.github.io/FeynCalcExamplesMD
# ./exportToMD.sh math11.3 "/media/Data/Projects/VS/FeynCalc/FeynCalc/Examples/QCD/Tree/ElAel-QQbar.m" "/home/vs/Downloads/outputMD"

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
examplesDir="$(dirname $scriptDIR)"

MATH=$1
OUTDIR=$2



function runExamples() {
   local EXPATH="$1"
   mkdir -p "$OUTDIR"/"$EXPATH";
   shift
   local files=("$@")
   for i in "${files[@]}";
      do
        echo
        echo -e "* \c"        
        filename="${i%%.*}"        
        if [ -f "$OUTDIR"/"$EXPATH"/"$filename".md ]; then
          echo "Markdown for $i already exists, skipping"
        else        
          $MATH -nopromt -script "$scriptDIR"/ExportToMD.m -run inputNB="\"$examplesDir/$EXPATH/$i\""  -run outputDir="\"$OUTDIR"/"$EXPATH\""
        fi
      done
}

if [[ $# -eq 3 ]] ; then
    $MATH -nopromt -script "$scriptDIR"/ExportToMD.m -run inputNB="\"$2\"" -run outputDir="\"$3\""    
else

#MSSM Examples
#-------------------------------------------------------------------------------
exfiles=('MnelEl-MnelEl.m')
runExamples "MSSM/Tree" "${exfiles[@]}"

#Phi^3 Examples
#-------------------------------------------------------------------------------
exfiles=('Renormalization.m')
runExamples "Phi3/OneLoop" "${exfiles[@]}"

#Phi^4 Examples
#-------------------------------------------------------------------------------
exfiles=('PhiPhi-PhiPhi.m' 'Renormalization.m')
runExamples "Phi4/OneLoop" "${exfiles[@]}"


#QED Examples
#-------------------------------------------------------------------------------
exfiles=('ElAel-ElAel.m' 'ElAel-GaGa.m' 'ElAel-MuAmu.m' 'ElEl-ElEl.m' \
'ElGa-ElGa.m' 'ElMu-ElMu.m' 'Ga-MuAmu.m')
runExamples "QED/Tree" "${exfiles[@]}"

exfiles=('ElAel-ElAel.m' 'ElAel-MuAmu.m' 'El-El.m' 'El-GaEl.m' 'Ga.m' 'Ga-Ga.m' 'Ga-GaGa.m' 'Ga-GaGaGaGa.m' \
'Pi-GaGa.m' 'Renormalization.m')
runExamples "QED/OneLoop" "${exfiles[@]}"

#QCD Examples
#-------------------------------------------------------------------------------
exfiles=('ElAel-QQbar.m' 'GaGl-QQbar.m' 'Ga-QQbar.m' 'Ga-QQbarGl.m' \
'GlGl-GlGl.m' 'GlGl-QQbar.m' 'MuAmu-QQbar.m' 'QGa-GlQ.m' 'QGl-QGl.m' \
'QiQibar-QiQibar.m' 'QiQibar-QjQjbar.m' 'QiQi-QiQi.m' 'QiQjbar-QiQjbar.m' \
'QiQj-QiQj.m' 'QQbar-ElAel.m' 'QQbar-GaGa.m' 'QQbar-GaGl.m' 'QQbar-GlGl.m' \
'QQbar-MuAmu.m')
runExamples "QCD/Tree" "${exfiles[@]}"

exfiles=('Gh-Gh.m' 'GhGl-Gh.m' 'Gl-Gl.m' 'Gl-Gl-BackgroundFieldGauge.m' \
'Gl-GlGl.m' 'Q-Q.m' 'Renormalization.m' 'RenormalizationMassless.m')
runExamples "QCD/OneLoop" "${exfiles[@]}"


exfiles=('Gh-Gh.m')
runExamples "QCD/TwoLoops" "${exfiles[@]}"

#EW Examples
#-------------------------------------------------------------------------------
exfiles=('AnelEl-AnmuMu.m' 'AnelEl-QubarQd.m' 'AnelEl-WW.m' 'AnelEl-ZZ.m' \
'ElNmu-MuNel.m' 'H-FFbar.m' 'H-WW.m' 'H-ZZ.m' 'Mu-ElAnelNmu.m' 'NleQdt-LeQut.m' \
'QQbar-ZZ.m' 'Qt-QbW.m' 'QuQdbar-AelNel.m' 'QutbarQdt-NelAnel.m' 'W-ElAnel.m' \
'W-QiQjbar.m' 'Z-FFbar.m')
runExamples "EW/Tree" "${exfiles[@]}"


exfiles=('H-GG.m')
runExamples "EW/OneLoop" "${exfiles[@]}"


exit 0;

#-------------------------------------------------------------------------------
notify-send --urgency=low -i "$([ $? = 0 ] && echo sunny || echo error)" "Finished converting FeynCalc examples to markdown."
fi


