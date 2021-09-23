#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2021 Rolf Mertig
# Copyright (C) 1997-2021 Frederik Orellana
# Copyright (C) 2014-2021 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc documentation to HTML

# Usage examples

# ./generateTeX.sh "~/Downloads/TeX"
# ./generateTeX.sh "/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown/ApartFF.md" "~/Downloads/TeX"

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

OUTDIR=$1

if [[ $# -eq 2 ]] ; then
    sed -i -e 's|\$\\\$\$|\\$|g' $1;
    sed -i -e 's|\^\*\^{|\^\{\*|g' $1;
    sed -i -e 's| \\text{| \\;\\text\{|g' $1;
    sed -i -e 's|}\\text{|}\\;\\text\{|g' $1;
    sed -i -e 's|}\\overline{\\text{|}\\;\\overline{\\text\{|g' $1;
    sed -i -e "s|\^'\(.*\)\$\\$|\^{'\1}\$\$|" $1;
    sed -i -e "s|unicode{f4a1}|to |g" $1;
    sed -i -e "s|unicode{f3d4}|leftrightarrow |g" $1;
    sed -i -e "s|unicode{f3d4}|leftrightarrow |g" $1;    
    sed -i -e "s|\^2\^2|\^4|g" $1;
    sed -i -e "s|\^2\^3|\^6|g" $1;
    sed -i -e 's|g\^{\\mu \\nu }^2|(g\^{\\mu \\nu})^2|' $1;
    sed -i -e 's|\\bar{\\delta }\^{ij}\^2|(\\bar{\\delta}\^{ij})^2|' $1;
    sed -i -e 's|\$\$\(!\[.*\)\$\$|\1|' $1;    
    pandoc "$1" -f markdown -t latex -s -o "$2"/$(basename -s .md "$1").tex
else
 
allFilesRaw=$(find $mainDir/Markdown/ -type f -name '*.md' -print)
allFilesRaw=($(printf "%s\n" "${allFilesRaw[@]}" | sort -V))

declare -a allFiles
for i in "${allFilesRaw[@]}"; do  
  name=$(basename -s .m $i)
  fullPath=$OUTDIR/$name".tex"
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



parallel -j 6 -u --eta --bar "$scriptDIR/generateTeX.sh {} $OUTDIR" ::: ${allFiles[@]};
#mkdir $OUTDIR/Extra &> /dev/null;
#mv $OUTDIR/FeynCalc.html $OUTDIR/Extra/FeynCalc.html;
#rm -rf $OUTDIR/img;
#cp -a $mainDir/Markdown/img $OUTDIR/img;

fi
