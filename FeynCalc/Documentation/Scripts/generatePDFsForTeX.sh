#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2022 Rolf Mertig
# Copyright (C) 1997-2022 Frederik Orellana
# Copyright (C) 2014-2022 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc documentation images from SVG to PDF

# Usage examples

# ./generatePDFsForTeX.sh /media/Data/Projects/VS/feyncalc-manual/


scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

PAGESDIR="pages"
IMGDIR="img"
FILTERSDIR="filters"
TEMPLATESDIR="templates"

OUTDIR=$1
SOURCEDIR="$mainDir"/Markdown/

rm -rf "$OUTDIR"/"$IMGDIR"/*.pdf;
$scriptDIR/svgToPdf.sh "$OUTDIR"/"$IMGDIR";
