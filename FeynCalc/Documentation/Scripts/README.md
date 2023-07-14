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
    export MAKE_DOCU_LOAD_ADDONS="{}"; export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./exportToMD.sh math "$DOCU_SOURCE_DIR"/Markdown
    ```

* To update the HTML documentation use

    ```
    export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./generateHTML.sh /media/Data/Projects/VS/feyncalc.github.io/FeynCalcBookDev
    ```

* To update the TeX documentation use

    ```
    export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; export DOCU_MANUAL_NAME="FeynCalcManual"; export DOCU_INDEX_FILE=$DOCU_SOURCE_DIR/Markdown/Extra/FeynCalc.md; ./generateTeX.sh /media/Data/Projects/VS/feyncalc-manual/
    ```

* To build the TeX documentation use

    ```
    latexmk -cd /media/Data/Projects/VS/feyncalc-manual/FeynCalcManual.tex -pdf
    ```

    Notice that when there are new figures (svg files), one would need to run

    ```
    export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./svgToPdf.sh /media/Data/Projects/VS/feyncalc-manual/img/
    cp -n $DOCU_SOURCE_DIR/Markdown/img/*.pdf /media/Data/Projects/VS/feyncalc-manual/img/
    ```
   

* To check Markdown files using mdl (`gem install mdl`, cf. [GitHub repo](https://github.com/markdownlint/markdownlint))

    ```
    mdl -r ~MD009,~MD013,~MD002,~MD010,~MD047 ../Markdown
    ```
