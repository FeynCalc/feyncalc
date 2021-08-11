#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2021 Rolf Mertig
# Copyright (C) 1997-2021 Frederik Orellana
# Copyright (C) 2014-2021 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc documentation to Markdown

# Usage examples

# ./generateHTML.sh ~/Downloads/HTML
# ./generateHTML.sh "/media/Data/Projects/VS/FeynCalc/FeynCalc/DocumentationFiles/Markdown/CF.md" "/home/vs/Downloads/HTML"

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

OUTDIR=$1

if [[ $# -eq 2 ]] ; then
    pandoc "$1" -f markdown -t html --mathjax -s --metadata title=$(basename -s .md "$1") -o "$2"/$(basename -s .md "$1").html
else



allFilesRaw=$(find $mainDir/Markdown/ -type f -name '*.md' -print)
allFilesRaw=($(printf "%s\n" "${allFilesRaw[@]}" | sort -V))

declare -a allFiles
for i in "${allFilesRaw[@]}"; do  
  name=$(basename -s .m $i)
  fullPath=$OUTDIR/$name".html"
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


parallel -j 6 -u --eta --bar "$scriptDIR/generateHTML.sh {} $OUTDIR" ::: ${allFiles[@]}
exit
$scriptDIR/generateHTML.sh
#-------------------------------------------------------------------------------
notify-send --urgency=low -i "$([ $? = 0 ] && echo sunny || echo error)" "Finished generating HTML files from markdown."
fi


