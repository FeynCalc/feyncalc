#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2024 Rolf Mertig
# Copyright (C) 1997-2024 Frederik Orellana
# Copyright (C) 2014-2024 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc/add-on documentation from Markdown to HTML

# Use --to=native for debugging!

# Usage examples

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./generateHTML.sh /media/Data/Projects/VS/feyncalc.github.io/FeynCalcBookDev

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; ./generateHTML.sh /media/Data/Projects/VS/feyncalc.github.io/FeynHelpersBookDev

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./generateHTML.sh "/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown/ApartFF.md" /media/Data/Projects/VS/feyncalc.github.io/FeynCalcBookDev




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

if [[ -z "${MAKE_CHANGE_CSS_PATH}" ]]; then
  cssPath="css/feyncalc.css"
else
  cssPath="${MAKE_CHANGE_CSS_PATH}"
fi

if [[ -z "${MAKE_CHANGE_KATEX_PATH}" ]]; then
  katexPath="js/"
else
  katexPath="${MAKE_CHANGE_KATEX_PATH}"
fi


scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
docuTitle=$(basename $(dirname "$mainDir"))

FILTERSDIR="filters"
TEMPLATESDIR="templates"

OUTDIR=$1

if [[ $# -eq 2 ]] ; then    
    pandoc "$1" -f markdown -t html5 --template="$TEMPLATESDIR"/feyncalc.html5 --katex=${katexPath} -fmarkdown-implicit_figures --lua-filter="$FILTERSDIR"/md2html.lua -s --metadata title="$docuTitle manual (development version)" -c ${cssPath} --metadata=classoption:fleqn -o "$2"/$(basename -s .md "$1").html
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

parallel -j $nThreads -u --eta --bar "$scriptDIR/generateHTML.sh {} $OUTDIR" ::: ${allFiles[@]};


if [ -z ${mainDir}/Extra ]; then
	mkdir $OUTDIR/Extra &> /dev/null;
	allFilesExtra=$(find $mainDir/Markdown/Extra -type f -name '*.md' -print)
	allFilesExtra=($(printf "%s\n" "${allFilesExtra[@]}" | sort -V))
	for i in "${allFilesExtra[@]}"; do
	  name=$(basename -s .md $i);
	  mv $OUTDIR/$name.html $OUTDIR/Extra/;
	  sed -i -e "s|css/feyncalc.css|../css/feyncalc.css|g" $OUTDIR/Extra/$name.html;
	  sed -i -e "s|js/|../js/|g" $OUTDIR/Extra/$name.html;
	done
fi


rm -rf $OUTDIR/img;
mkdir -p $OUTDIR/img;
cp -a $mainDir/Markdown/img/*.svg $OUTDIR/img/;

fi
