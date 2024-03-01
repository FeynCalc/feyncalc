## Installation

### See also

[Overview](FeynCalc.md).

The installation of FeynCalc can be done either automatically using the provided  Mathematica script or manually by copying the code to specific locations on your computer.

### Automatic installation

#### Stable version

The stable version is the latest official release of FeynCalc. We don't have fixed development cycles so that the stable version is released just when it's ready. The code of the stable version is located in the [hotfix-stable](https://github.com/FeynCalc/feyncalc/tree/hotfix-stable) branch of our main repository. Bugs that are discovered in the latest stable version will be fixed in that branch. When you install FeynCalc using the automatic installer you will automatically receive all the current fixes. Note that the stable branch will not contain any new features until the next stable release.

To install the stable version run the following instruction in a Kernel or Notebook session of Mathematica.

```Mathematica
Import["https://raw.githubusercontent.com/FeynCalc/feyncalc/master/install.m"]
InstallFeynCalc[]
```

#### Development version

The development version is the content of the master branch in the [Git repository](https://github.com/FeynCalc/feyncalc) of FeynCalc. It contains all the fixes and new features that were implemented since the last stable release. In the development process we use a growing number of [unit tests](https://github.com/FeynCalc/feyncalc/tree/master/Tests) to ensure that the changes in the code do not break existing behavior or introduce new bugs. **However, despite thorough testing the development version may still contain bugs and incomplete or untested features. You can greatly help FeynCalc developers by testing the development version and reporting issues with it!**

To install the development version run the following instruction in a Kernel or Notebook session of Mathematica

```Mathematica
Import["https://raw.githubusercontent.com/FeynCalc/feyncalc/master/install.m"]
InstallFeynCalc[InstallFeynCalcDevelopmentVersion -> True]
```

#### Troubleshooting

On Linux (possibly also Windows and macOS) the above code might fail when run with Mathematica 10 or 11. The error messages will look like `URLSave::invhttp: SSL connect error` or `URLSave::invhttp`. This is most likely caused by some library incompatibilities. A workaround for versions 10 and 11 is available [here](https://mathematica.stackexchange.com/questions/212453/urlsave-in-mathematica-10-and-11-on-linux). If nothing helps, you can still download the necessary files by yourself and run the automatic installer offline.

### Manual installation

Manual installation is also possible, but is slightly less convenient as compared to using the automatic installer.

* Download [this](https://github.com/FeynCalc/feyncalc/archive/hotfix-stable.zip) (for the stable version) or [this](https://github.com/FeynCalc/feyncalc/archive/master.zip) (for the development version) zip file.
* Copy the *FeynCalc* directory from the extracted archive to the *Applications* directory inside ```$UserBaseDirectory```  (evaluate ```FileNameJoin[{$UserBaseDirectory, "Applications"}]``` in Mathematica).
* If you want to allow `FeynCalc` to activate `TraditionalForm` typesetting when it is loaded, create "FCConfig.m" inside  *FeynCalc* directory and add there the following line

    ```Mathematica
    $FCTraditionalFormOutput=True;
    ```

    this change will affect only the current `FeynCalc` session and will not modify the default behavior of Mathematica.

### Offline automatic installation

* Download the following 3 files:

    * [master.zip](https://github.com/FeynCalc/feyncalc/archive/master.zip) (FeynCalc)
    * [master.zip](https://github.com/FeynCalc/feynarts-mirror/archive/master.zip) (FeynArts)
    * [install.m](https://github.com/FeynCalc/feyncalc/raw/master/install.m)

* Put these files to the same folder. In the following I assume that it is `"/home/vs/Downloads"` which will be of course different on your system. Then open Mathematica and run

```Mathematica
(*Change myPath accordingly! *)
myPath = "/home/vs/Downloads";
Get[FileNameJoin[{myPath,"install.m"}]]
$PathToFCArc = FileNameJoin[{myPath,"feyncalc-master.zip"}];
$PathToFAArc = FileNameJoin[{myPath,"feynarts-mirror-master.zip"}];
InstallFeynCalc[InstallFeynCalcDevelopmentVersion -> True]
```