#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2023 Rolf Mertig
# Copyright (C) 1997-2023 Frederik Orellana
# Copyright (C) 2014-2023 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc/add-on  documentation from Markdown to LaTeX

# Usage examples

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; export DOCU_MANUAL_NAME="FeynCalcManual"; export DOCU_INDEX_FILE=$DOCU_SOURCE_DIR/Markdown/Extra/FeynCalc.md; ./generateTeX.sh /media/Data/Projects/VS/feyncalc-manual/

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; export DOCU_MANUAL_NAME="FeynHelpersManual"; export DOCU_INDEX_FILE=$DOCU_SOURCE_DIR/Markdown/Extra/FeynHelpers.md; ./generateTeX.sh /media/Data/Projects/VS/feynhelpers-manual/

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; export DOCU_MANUAL_NAME="FeynCalcManual"; export DOCU_INDEX_FILE=$DOCU_SOURCE_DIR/Markdown/Extra/FeynCalc.md; ./generateTeX.sh "$DOCU_SOURCE_DIR/Markdown/CSP.md" /media/Data/Projects/VS/feyncalc-manual/pages/

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; export DOCU_MANUAL_NAME="FeynHelpersManual"; export DOCU_INDEX_FILE=$DOCU_SOURCE_DIR/Markdown/Extra/FeynHelpers.md; ./generateTeX.sh "$DOCU_SOURCE_DIR/Markdown/Extra/Cite.md" /media/Data/Projects/VS/feynhelpers-manual/pages/


if [[ -z "${DOCU_SOURCE_DIR}" ]]; then
  echo "You need to set the environmental variable DOCU_SOURCE_DIR that contains the full path to the relevant Documentation directory"
  exit
else
  mainDir="${DOCU_SOURCE_DIR}"
fi

if [[ -z "${DOCU_MANUAL_NAME}" ]]; then
  echo "You need to set the environmental variable DOCU_MANUAL_NAME that specifies the name of the manual file"
  exit
else
  manualName="${DOCU_MANUAL_NAME}"
fi

if [[ -z "${MAKE_DOCU_NTHREADS}" ]]; then
  nThreads=6
else
  nThreads="${MAKE_DOCU_NTHREADS}"
fi

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

PAGESDIR="pages"
IMGDIR="img"
FILTERSDIR="filters"

OUTDIR=$1
SOURCEDIR="$mainDir"/Markdown/

if [[ $# -eq 2 ]] ; then    
    echo "Creating TeX file for $1"
    output="$2"/$(basename -s .md "$1")
    output=$(echo $output | sed 's/\$/Dollar/g')
    #echo $output
    texname=$(basename -s .md "$1")
    texname=${texname/$/Dollar}

    


    pandoc "$1" -f markdown -t latex  --lua-filter="$FILTERSDIR"/dmath.lua --lua-filter="$FILTERSDIR"/svg2pdf.lua --lua-filter="$FILTERSDIR"/allowbreak.lua --lua-filter="$FILTERSDIR"/href.lua  --lua-filter="$FILTERSDIR"/fixlabels.lua --metadata=subtitle:"$texname" --default-image-extension=pdf --top-level-division=chapter --template=template.latex -o "$output".tex;

    
else

mkdir -p "$OUTDIR"/"$PAGESDIR"
mkdir -p "$OUTDIR"/"$IMGDIR"

cat <<EOF > template.latex
% !TeX program = pdflatex
% !TeX root = \$subtitle\$.tex

\documentclass[../$manualName.tex]{subfiles}
\begin{document}
\$body\$
\end{document}
EOF

 
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

parallel -j $nThreads -u --eta --bar "$scriptDIR/generateTeX.sh {} $OUTDIR/$PAGESDIR/" ::: ${allFiles[@]};
$scriptDIR/generateSubfiles.sh math "$OUTDIR"
rm -rf template.latex;
fi
