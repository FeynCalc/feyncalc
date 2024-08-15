## QCD model for FeynArts

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
frModelPath = FileNameJoin[{nbDir, "QCD.fr"}];
LoadModel[frModelPath];
```

$$\text{This model implementation was created by}$$

$$\text{Vladyslav Shtabovenko}$$

$$\text{Model Version: }0$$

$$\text{For more information, type ModelInformation[].}$$

$$\text{}$$

$$\text{   - Loading particle classes.}$$

$$\text{   - Loading gauge group classes.}$$

$$\text{   - Loading parameter classes.}$$

$$\text{$\backslash $nModel }\;\text{QCD}\;\text{ loaded.}$$

```mathematica
FR$Loop = True;
SetDirectory[FileNameJoin[{$UserBaseDirectory, "Applications", "FeynCalc", "FeynArts", "Models"}]];
WriteFeynArtsOutput[LQCD, Output -> "QCD", CouplingRename -> False];
```

$$\text{ - - - FeynRules interface to FeynArts - - -}$$

$$\text{      C. Degrande C. Duhr, 2013}$$

$$\text{      Counterterms: B. Fuks, 2012}$$

$$\text{Calculating Feynman rules for }\;\text{L1}$$

$$\text{Starting Feynman rules calculation for L1.}$$

$$\text{Expanding the Lagrangian...}$$

$$\text{Collecting the different structures that enter the vertex.}$$

$$9\text{ possible non-zero vertices have been found -$>$ starting the computation: }\;\text{FeynRules$\grave{ }$FR\$FeynmanRules}\;\text{ / }9.$$

$$9\text{ vertices obtained.}$$

$$\text{mytimecheck,after LGC}$$

$$\text{Writing FeynArts model file into directory }\;\text{QCD}$$

$$\text{Writing FeynArts generic file on }\;\text{QCD.gen}.$$

```mathematica
input = FileNameJoin[{$UserBaseDirectory, "Applications", "FeynCalc", "FeynArts", "Models", "QCD", "QCD.gen"}];
tmp = Import[input, "Text"] <> "\n";
res = StringJoin[{tmp, "GaugeXi[V[5,___]]=GaugeXi[\"G\"]; \n GaugeXi[U[5,___]]=GaugeXi[\"G\"]; \n"}];
Export[input, res, "Text"];
```