# Documentation scripts

This directory contains Mathematica/Bash scripts that help to ensure the quality of the FeynCalc documentaion

* To check for FeynCalc symbols that are not properly documented, use 

    ```
    export MAKE_DOCU_LOAD_ADDONS="{}"; export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; export DOCU_INDEX_FILE=$DOCU_SOURCE_DIR/Markdown/Extra/FeynCalc.md; ./checkMissingDocu.sh math
    ```
* To check for poorly formatted documentation files use

    ```
    export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation";  ./checkQuality.sh
    ```

* To synchronize the usage information strings of FeynCalc symbols with the documentation files use

    ```
    export MAKE_DOCU_LOAD_ADDONS="{}"; export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./updateUsageInformation.sh math
    ```

* To generate Markdown documentation from .m-files use

    ```
    export MAKE_DOCU_LOAD_ADDONS="{}"; DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./exportToMD.sh math "$DOCU_SOURCE_DIR"/Markdown
    ```
    
* To update the HTML documentation use

    ```
    export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./generateHTML.sh /media/Data/Projects/VS/feyncalc.github.io/FeynCalcBookDev
    ```
    
* To update the TeX documentation use

    ```
    export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; export DOCU_MANUAL_NAME="FeynCalcManual"; ./generateTeX.sh /media/Data/Projects/VS/feyncalc-manual/
    ```        
    
* To build the TeX documentation use

    ```
    cd /media/Data/Projects/VS/feyncalc-manual/
    latexmk -pdf FeynCalcManual.tex
    ```
