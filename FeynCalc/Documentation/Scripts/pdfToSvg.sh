#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2022 Rolf Mertig
# Copyright (C) 1997-2022 Frederik Orellana
# Copyright (C) 2014-2022 Vladyslav Shtabovenko

# Description:

# Converts pdf vector graphics to svg
# This script is automatically called by exporToMD.sh

# Usage examples

# ./pdfToSvg.sh /media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown/img/0gi2hdxwlvyo6.svg ~/Downloads/TeX/img
# ./pdfToSvg.sh /media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown/img/

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

OUTDIR=$1

if [[ $# -eq 2 ]] ; then
    inkscape --export-type=svg "$1" -o "$2"/$(basename -s .pdf "$1").svg    
else
echo $mainDir/Markdown/img
allFilesRaw=$(find $mainDir/Markdown/img -type f -name '*.pdf' -print)
allFilesRaw=($(printf "%s\n" "${allFilesRaw[@]}" | sort -V))
echo $allFilesRaw
declare -a allFiles
for i in "${allFilesRaw[@]}"; do  
  name=$(basename -s .pdf $i)
  fullPath=$OUTDIR/$name".svg"
  if [ -f $fullPath ]; then
    true
    #echo "Skipping $name - file already exists."
    #echo
  else
#   echo "Adding $name";
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



parallel -j 6 -u --eta --bar "$scriptDIR/pdfToSvg.sh {} $OUTDIR" ::: ${allFiles[@]};


fi
