#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2022 Rolf Mertig
# Copyright (C) 1997-2022 Frederik Orellana
# Copyright (C) 2014-2022 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc/add-on documentation from SVG to PDF

# Usage examples

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./generatePDFsForTeX.sh /media/Data/Projects/VS/feyncalc-manual/

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; ./generatePDFsForTeX.sh /media/Data/Projects/VS/feynhelpers-manual/


if [[ -z "${DOCU_SOURCE_DIR}" ]]; then
  echo "You need to set the environmental variable DOCU_SOURCE_DIR that contains the full path to the relevant Documentation directory"
  exit
else
  mainDir="${DOCU_SOURCE_DIR}"
fi

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

PAGESDIR="pages"
IMGDIR="img"
FILTERSDIR="filters"
TEMPLATESDIR="templates"

OUTDIR=$1
SOURCEDIR="$mainDir"/Markdown/
rm -rf "$OUTDIR"/"$IMGDIR"/*.pdf;
$scriptDIR/svgToPdf.sh "$OUTDIR"/"$IMGDIR";
