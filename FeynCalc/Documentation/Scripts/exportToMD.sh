#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2023 Rolf Mertig
# Copyright (C) 1997-2023 Frederik Orellana
# Copyright (C) 2014-2023 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc/add-on documentation from Mathematica to Markdown

# Usage examples

# ./exportToMD.sh math /media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation/Markdown


# export MAKE_DOCU_LOAD_ADDONS="{}"; DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./exportToMD.sh math "$DOCU_SOURCE_DIR"/Markdown
# export MAKE_DOCU_LOAD_ADDONS="{FeynHelpers}"; DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; ./exportToMD.sh math "$DOCU_SOURCE_DIR"/Markdown


# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./exportToMD.sh math "$DOCU_SOURCE_DIR/Mathematica/Shared/Symbols/LC.m" "$DOCU_SOURCE_DIR/Markdown"
# export MAKE_DOCU_LOAD_ADDONS="{FeynHelpers}"; export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; ./exportToMD.sh math "$DOCU_SOURCE_DIR/Mathematica/FIRE/Functions/FIRECreateConfigFile.m" "$DOCU_SOURCE_DIR/Markdown"

if [[ -z "${DOCU_SOURCE_DIR}" ]]; then
  echo "You need to set the environmental variable DOCU_SOURCE_DIR that contains the full path to the relevant Documentation directory"
  exit
else
  mainDir="${DOCU_SOURCE_DIR}"
fi

if [[ -z "${MAKE_DOCU_NTHREADS}" ]]; then
  nThreads=4
else
  nThreads="${MAKE_DOCU_NTHREADS}"
fi

if [[ -z "${MAKE_DOCU_LOAD_ADDONS}" ]]; then
  requestedAddOns=""
else
  requestedAddOns="${MAKE_DOCU_LOAD_ADDONS}"
fi

if [[ -z "${MAKE_DO_NOT_LOAD_FEYNCALC}" ]]; then  
  noFC="False"
else
  noFC="${MAKE_DO_NOT_LOAD_FEYNCALC}"
fi

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
MATH=$1
OUTDIR=$2

if [[ $# -eq 3 ]] ; then
    $MATH -nopromt -script "$scriptDIR"/ExportToMD.m -run inputNB="\"$2\"" -run outputDir="\"$3\"" -run loadAddOns="\"$requestedAddOns\""
else

allFilesRaw=$(find $mainDir/Mathematica/ -type f -name '*.m' ! -name 'ReductionTable-*' -print)
allFilesRaw=($(printf "%s\n" "${allFilesRaw[@]}" | sort -V))

declare -a allFiles
for i in "${allFilesRaw[@]}"; do  
  name=$(basename -s .m $i)
  fullPath=$OUTDIR/$name".md"
  if [ -f $fullPath ]; then
    true
    #echo "Skipping $name - file already exists."
  else
    #echo "Adding $name";
    allFiles+=($i) 
  fi
done
echo 

echo "Relevant files"
for value in "${allFiles[@]}"
do
     echo $value
done

if [ -z ${allFiles} ]; then
    echo "No files to process, leaving."
    exit 0;
fi

parallel -j "$nThreads" -u --eta --bar "$MATH -nopromt -script $scriptDIR/ExportToMD.m  -run DoNotLoadFeynCalc='\"$noFC\"' -run loadAddOns='\"$requestedAddOns\"' -run outputDir='\"$2\"'" -run inputNB='\"{}\"'  ::: ${allFiles[@]}
$scriptDIR/cleanUpMarkdown.sh $OUTDIR
$scriptDIR/pdfToSvg.sh $OUTDIR/img/
#-------------------------------------------------------------------------------
notify-send --urgency=low -i "$([ $? = 0 ] && echo sunny || echo error)" "Finished converting FeynHelpers documentation to markdown."
fi


