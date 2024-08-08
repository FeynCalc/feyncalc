## SM model for FeynArts

### Load FeynRules

```mathematica
FR$Parallel = False;
$FeynRulesPath = FileNameJoin[{$UserBaseDirectory, "Applications", "FeynRules"}]
<< FeynRules`;
```

$$\text{/home/vs/.Mathematica/Applications/FeynRules}$$

$$\text{ - FeynRules - }$$

$$\text{Version: }\;\text{2.3.49}\;\text{ (} \;\text{29 September 2021}\;\text{).}$$

$$\text{Authors: A. Alloul, N. Christensen, C. Degrande, C. Duhr, B. Fuks}$$

$$$$

$$\text{Please cite:}$$

$$\text{    - Comput.Phys.Commun.185:2250-2300,2014 (arXiv:1310.1921);}$$

$$\text{    - Comput.Phys.Commun.180:1614-1641,2009 (arXiv:0806.4194).}$$

$$$$

$$\text{http://feynrules.phys.ucl.ac.be}$$

$$$$

$$\text{The FeynRules palette can be opened using the command FRPalette[].}$$

### Load FeynRules model

```mathematica
If[$FrontEnd === Null, 
   nbDir = DirectoryName[$InputFileName], 
   nbDir = NotebookDirectory[] 
  ];
```

```mathematica
frModelPath = FileNameJoin[{$FeynRulesPath, "Models", "SM", "SM.fr"}];
LoadModel[frModelPath];
```

$$\text{This model implementation was created by}$$

$$\text{N. Christensen}$$

$$\text{C. Duhr}$$

$$\text{B. Fuks}$$

$$\text{Model Version: }\;\text{1.4.7}$$

$$\text{http://feynrules.phys.ucl.ac.be/view/Main/StandardModel}$$

$$\text{For more information, type ModelInformation[].}$$

$$\text{}$$

$$\text{   - Loading particle classes.}$$

$$\text{   - Loading gauge group classes.}$$

$$\text{   - Loading parameter classes.}$$

$$\text{$\backslash $nModel }\;\text{Standard Model}\;\text{ loaded.}$$

### Create FeynArts model

```mathematica
SetDirectory[FileNameJoin[{$UserBaseDirectory, "Applications", "FeynCalc", "FeynArts", "Models"}]];
WriteFeynArtsOutput[LSM, Output -> "SM", CouplingRename -> False];

```mathematica

$$\text{ - - - FeynRules interface to FeynArts - - -}$$

$$\text{      C. Degrande C. Duhr, 2013}$$

$$\text{      Counterterms: B. Fuks, 2012}$$

$$\text{Calculating Feynman rules for }\;\text{L1}$$

$$\text{Starting Feynman rules calculation for L1.}$$

$$\text{Expanding the Lagrangian...}$$

$$\text{Collecting the different structures that enter the vertex.}$$

$$98\text{ possible non-zero vertices have been found -$>$ starting the computation: }\;\text{FeynRules$\grave{ }$FR\$FeynmanRules}\;\text{ / }98.$$

$$93\text{ vertices obtained.}$$

$$\text{mytimecheck,after LGC}$$

$$\text{Writing FeynArts model file into directory }\;\text{SM}$$

$$\text{Writing FeynArts generic file on }\;\text{SM.gen}.$$