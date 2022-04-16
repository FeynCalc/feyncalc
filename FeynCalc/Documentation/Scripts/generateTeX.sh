#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2022 Rolf Mertig
# Copyright (C) 1997-2022 Frederik Orellana
# Copyright (C) 2014-2022 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc documentation from Markdown to LaTeX

# Usage examples

# ./generateTeX.sh ~/Downloads/TeX
# ./generateTeX.sh "/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown/CSP.md" /media/Data/Projects/VS/feyncalc-manual/

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

PAGESDIR="pages"
IMGDIR="img"
FILTERSDIR="filters"
TEMPLATESDIR="templates"

OUTDIR=$1
SOURCEDIR="$mainDir"/Markdown/

if [[ $# -eq 2 ]] ; then    
    echo "Creating TeX file for $1"
    output="$2"/$(basename -s .md "$1")
    output=$(echo $output | sed 's/\$/Dollar/g')
    echo $output
    texname=$(basename -s .md "$1")
    texname=${texname/$/Dollar}   
    
    pandoc "$1" -f markdown -t latex  --lua-filter="$FILTERSDIR"/dmath.lua --lua-filter="$FILTERSDIR"/svg2pdf.lua --lua-filter="$FILTERSDIR"/allowbreak.lua --lua-filter="$FILTERSDIR"/href.lua  --lua-filter="$FILTERSDIR"/fixlabels.lua --metadata=subtitle:"$texname" --default-image-extension=pdf --top-level-division=chapter --template="$TEMPLATESDIR"/feyncalc.latex -o "$output".tex
else

mkdir -p "$OUTDIR"/"$PAGESDIR"
mkdir -p "$OUTDIR"/"$IMGDIR"

 
allFilesRaw=$(find $SOURCEDIR -type f -name '*.md' -print)
allFiles=($(printf "%s\n" "${allFilesRaw[@]}" | sort -V))




echo "Relevant files"
for value in "${allFiles[@]}"
do
     echo $value
done

if [ -z ${allFiles} ]; then
    echo "No files to process, leaving."
    exit 0;
fi

parallel -j 6 -u --eta --bar "$scriptDIR/generateTeX.sh {} $OUTDIR/$PAGESDIR/" ::: ${allFiles[@]};
$scriptDIR/generateSubfiles.sh math "$OUTDIR"

fi
