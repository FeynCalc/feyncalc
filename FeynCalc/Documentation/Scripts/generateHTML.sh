#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2022 Rolf Mertig
# Copyright (C) 1997-2022 Frederik Orellana
# Copyright (C) 2014-2022 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc documentation from Markdown to HTML

# Usage examples

# ./generateHTML.sh /media/Data/Projects/VS/feyncalc.github.io/FeynCalcBookDev
# ./generateHTML.sh "/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown/ApartFF.md" /media/Data/Projects/VS/feyncalc.github.io/FeynCalcBookDev

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

FILTERSDIR="filters"
TEMPLATESDIR="templates"

OUTDIR=$1

if [[ $# -eq 2 ]] ; then    
    pandoc "$1" -f markdown -t html5  --katex -fmarkdown-implicit_figures --lua-filter="$FILTERSDIR"/md2html.lua -s --metadata title="FeynCalc manual (development version)" -c "css/feyncalc.css" --metadata=classoption:fleqn -o "$2"/$(basename -s .md "$1").html
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



parallel -j 6 -u --eta --bar "$scriptDIR/generateHTML.sh {} $OUTDIR" ::: ${allFiles[@]};
mkdir $OUTDIR/Extra &> /dev/null;
mv $OUTDIR/FeynCalc.html $OUTDIR/Extra/FeynCalc.html;
mv $OUTDIR/MasterIntegrals.html $OUTDIR/Extra/MasterIntegrals.html;
mv $OUTDIR/Indices.html $OUTDIR/Extra/Indices.html;
mv $OUTDIR/FeynArtsSigns.html $OUTDIR/Extra/FeynArtsSigns.html;
rm -rf $OUTDIR/img;
mkdir -p $OUTDIR/img;
cp -a $mainDir/Markdown/img/*.svg $OUTDIR/img/;

sed -i -e "s|css/feyncalc.css|../css/feyncalc.css|g" $OUTDIR/Extra/FeynCalc.html;
sed -i -e "s|css/feyncalc.css|../css/feyncalc.css|g" $OUTDIR/Extra/MasterIntegrals.html;
sed -i -e "s|css/feyncalc.css|../css/feyncalc.css|g" $OUTDIR/Extra/FeynArtsSigns.html;
sed -i -e "s|css/feyncalc.css|../css/feyncalc.css|g" $OUTDIR/Extra/Indices.html;


fi
