#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2022 Rolf Mertig
# Copyright (C) 1997-2022 Frederik Orellana
# Copyright (C) 2014-2022 Vladyslav Shtabovenko

# Description:

# Converts svg vector graphics to pdf
# This script is automatically called by generateTeX.sh

# Usage examples

# ./svgToPdf.sh /media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown/img/0gi2hdxwlvyo6.svg ~/Downloads/TeX/img
# ./svgToPdf.sh ~/Downloads/TeX/img

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

OUTDIR=$1

if [[ $# -eq 2 ]] ; then
    inkscape --export-type=pdf "$1" -o "$2"/$(basename -s .svg "$1").pdf    
else
 
allFilesRaw=$(find $mainDir/Markdown/img -type f -name '*.svg' -print)
allFilesRaw=($(printf "%s\n" "${allFilesRaw[@]}" | sort -V))

declare -a allFiles
for i in "${allFilesRaw[@]}"; do  
  name=$(basename -s .svg $i)
  fullPath=$OUTDIR/$name".pdf"
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



parallel -j 6 -u --eta --bar "$scriptDIR/svgToPdf.sh {} $OUTDIR" ::: ${allFiles[@]};
#mkdir $OUTDIR/Extra &> /dev/null;
#mv $OUTDIR/FeynCalc.html $OUTDIR/Extra/FeynCalc.html;
#rm -rf $OUTDIR/img;
#cp -a $mainDir/Markdown/img $OUTDIR/img;

fi
