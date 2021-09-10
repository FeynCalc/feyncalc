# Documentation scripts

This directory contains Mathematica/Bash scripts that help to ensure the quality
of FeynCalc documentaion

* To check for FeynCalc symbols that are not properly documented, use 

    ```
    ./checkMissingDocu.sh math
    ```
* To check for poorly formatted documentation files use

    ```
    ./checkQuality.sh math
    ```

* To synchronize the usage information strings of FeynCalc symbols with the documentation files use

    ```
    ./updateUsageInformation.sh math
    ```

* To generate Markdown documentation from .m-files use

    ```
    ./exportToMD.sh math ../Markdown/
    ```
    
* To update the HTML documentation use

    ```
    ./generateHTML.sh /media/Data/Projects/VS/feyncalc.github.io/FeynCalcBookDev
    ```    
