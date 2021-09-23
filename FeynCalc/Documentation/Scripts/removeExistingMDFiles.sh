#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2021 Rolf Mertig
# Copyright (C) 1997-2021 Frederik Orellana
# Copyright (C) 2014-2021 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc documentation to Markdown

# Usage examples

# ./removeExistingMDFiles.sh math SUN /media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

MATH=$1
OUTDIR=$3


allFilesRaw=$(find $mainDir/Mathematica/$2 -type f -name '*.m' -print)
allFilesRaw=($(printf "%s\n" "${allFilesRaw[@]}" | sort -V))
allFiles=("")
for i in "${allFilesRaw[@]}"; do  
  name=$(basename -s .m $i)
  fullPath=$OUTDIR/$name".md"
  allFiles+=($fullPath)   
done
echo 


echo "Following files will be removed"
for value in "${allFiles[@]}"
do
     echo $value
done

read -p "Are you sure? To continue, please type 'yes':" -n 3 -r
echo    # (optional) move to a new line
if [[ $REPLY = "yes" ]]
then
    echo Deleting...

    for i in "${allFiles[@]}"; do
      rm -rf $i
    done
fi


