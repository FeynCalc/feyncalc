#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2024 Rolf Mertig
# Copyright (C) 1997-2024 Frederik Orellana
# Copyright (C) 2014-2024 Vladyslav Shtabovenko

# Description:

# Converts pdf vector graphics to svg
# This script is automatically called by exporToMD.sh

# Usage examples

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./pdfToSvg.sh $DOCU_SOURCE_DIR/Markdown/img/0gi2hdxwlvyo6.pdf ~/Downloads/TeX/img
# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./pdfToSvg.sh ~/Downloads/TeX/img

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; ./pdfToSvg.sh $DOCU_SOURCE_DIR/Markdown/img/0nwzbpg5gzgtm.pdf ~/Downloads/TeX/img
# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; ./pdfToSvg.sh ~/Downloads/TeX/img

if [[ -z "${DOCU_SOURCE_DIR}" ]]; then
  echo "You need to set the environmental variable DOCU_SOURCE_DIR that contains the full path to the relevant Documentation directory"
  exit
else
  mainDir="${DOCU_SOURCE_DIR}"
fi

if [[ -z "${MAKE_DOCU_NTHREADS}" ]]; then
  nThreads=6
else
  nThreads="${MAKE_DOCU_NTHREADS}"
fi

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
SOURCEDIR="$mainDir"/Markdown/img
OUTDIR=$1

if [[ $# -eq 2 ]] ; then
    inkscape --export-type=svg "$1" -o "$2"/$(basename -s .pdf "$1").svg    
else

allFilesRaw=$(find $SOURCEDIR -type f -name '*.pdf' -print)
allFilesRaw=($(printf "%s\n" "${allFilesRaw[@]}" | sort -V))

declare -a allFiles
for i in "${allFilesRaw[@]}"; do  
  name=$(basename -s .pdf $i)
  fullPath=$OUTDIR/$name".svg"
  # echo $fullPath
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

parallel -j $nThreads -u --eta --bar "$scriptDIR/pdfToSvg.sh {} $OUTDIR" ::: ${allFiles[@]};

fi
