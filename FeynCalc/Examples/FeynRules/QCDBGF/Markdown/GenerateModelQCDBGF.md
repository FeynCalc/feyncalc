## QCD BGF model for FeynArts

### Load FeynRules

```mathematica
FR$Parallel = False;
$FeynRulesPath = FileNameJoin[{$UserBaseDirectory, "Applications", "FeynRules"}];
<< FeynRules`;
```

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
frModelPath = FileNameJoin[{nbDir, "QCDBGF.fr"}];
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

$$\text{$\backslash $nModel }\;\text{QCD in the background field formalism}\;\text{ loaded.}$$

### Generate Feynman rules

```mathematica
fRules = FeynmanRules[LQCD]
```

$$\text{Starting Feynman rule calculation.}$$

$$\text{Expanding the Lagrangian...}$$

$$\text{Collecting the different structures that enter the vertex.}$$

$$17\text{ possible non-zero vertices have been found -$>$ starting the computation: }\;\text{FeynRules$\grave{ }$FR\$FeynmanRules}\;\text{ / }17.$$

$$17\text{ vertices obtained.}$$

$$\left(
\begin{array}{cc}
 \left(
\begin{array}{cc}
 B & 1 \\
 B & 2 \\
 B & 3 \\
\end{array}
\right) & -\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _2} \;\text{p}_1^{\mu _3}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _2} \;\text{p}_2^{\mu _3}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _3} \;\text{p}_1^{\mu _2}-\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _3} \;\text{p}_3^{\mu _2}-\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _2,\mu _3} \;\text{p}_2^{\mu _1}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _2,\mu _3} \;\text{p}_3^{\mu _1} \\
 \left(
\begin{array}{cc}
 B & 1 \\
 B & 2 \\
 G & 3 \\
\end{array}
\right) & \frac{\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _3} \;\text{p}_2^{\mu _2}}{\text{GaugeXi}(G)}-\frac{\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _2,\mu _3} \;\text{p}_1^{\mu _1}}{\text{GaugeXi}(G)}-\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _2} \;\text{p}_1^{\mu _3}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _2} \;\text{p}_2^{\mu _3}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _3} \;\text{p}_1^{\mu _2}-\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _3} \;\text{p}_3^{\mu _2}-\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _2,\mu _3} \;\text{p}_2^{\mu _1}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _2,\mu _3} \;\text{p}_3^{\mu _1} \\
 \left(
\begin{array}{cc}
 \;\text{ghG}^{\dagger } & 1 \\
 \;\text{ghG} & 2 \\
 B & 3 \\
\end{array}
\right) & \;\text{gs} f_{\text{a}_3,\text{a}_1,\text{a}_2} \;\text{p}_1^{\mu _3}-\text{gs} f_{\text{a}_3,\text{a}_1,\text{a}_2} \;\text{p}_2^{\mu _3} \\
 \left(
\begin{array}{cc}
 B & 1 \\
 B & 2 \\
 B & 3 \\
 B & 4 \\
\end{array}
\right) & i \;\text{gs}^2 \eta _{\mu _1,\mu _4} \eta _{\mu _2,\mu _3} f_{\text{a}_1,\text{a}_3,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_4,\text{Gluon\$1}}+i \;\text{gs}^2 \eta _{\mu _1,\mu _4} \eta _{\mu _2,\mu _3} f_{\text{a}_1,\text{a}_2,\text{Gluon\$1}} f_{\text{a}_3,\text{a}_4,\text{Gluon\$1}}+i \;\text{gs}^2 \eta _{\mu _1,\mu _3} \eta _{\mu _2,\mu _4} f_{\text{a}_1,\text{a}_4,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_3,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _3} \eta _{\mu _2,\mu _4} f_{\text{a}_1,\text{a}_2,\text{Gluon\$1}} f_{\text{a}_3,\text{a}_4,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _2} \eta _{\mu _3,\mu _4} f_{\text{a}_1,\text{a}_4,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_3,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _2} \eta _{\mu _3,\mu _4} f_{\text{a}_1,\text{a}_3,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_4,\text{Gluon\$1}} \\
 \left(
\begin{array}{cc}
 \;\text{ghG}^{\dagger } & 1 \\
 \;\text{ghG} & 2 \\
 B & 3 \\
 B & 4 \\
\end{array}
\right) & i \;\text{gs}^2 \eta _{\mu _3,\mu _4} f_{\text{a}_3,\text{a}_2,\text{Gluon\$1}} f_{\text{a}_4,\text{a}_1,\text{Gluon\$1}}+i \;\text{gs}^2 \eta _{\mu _3,\mu _4} f_{\text{a}_3,\text{a}_1,\text{Gluon\$1}} f_{\text{a}_4,\text{a}_2,\text{Gluon\$1}} \\
 \left(
\begin{array}{cc}
 B & 1 \\
 G & 2 \\
 G & 3 \\
\end{array}
\right) & \frac{\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _2} \;\text{p}_3^{\mu _3}}{\text{GaugeXi}(G)}-\frac{\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _3} \;\text{p}_2^{\mu _2}}{\text{GaugeXi}(G)}-\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _2} \;\text{p}_1^{\mu _3}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _2} \;\text{p}_2^{\mu _3}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _3} \;\text{p}_1^{\mu _2}-\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _3} \;\text{p}_3^{\mu _2}-\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _2,\mu _3} \;\text{p}_2^{\mu _1}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _2,\mu _3} \;\text{p}_3^{\mu _1} \\
 \left(
\begin{array}{cc}
 B & 1 \\
 B & 2 \\
 B & 3 \\
 G & 4 \\
\end{array}
\right) & i \;\text{gs}^2 \eta _{\mu _1,\mu _4} \eta _{\mu _2,\mu _3} f_{\text{a}_1,\text{a}_3,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_4,\text{Gluon\$1}}+i \;\text{gs}^2 \eta _{\mu _1,\mu _4} \eta _{\mu _2,\mu _3} f_{\text{a}_1,\text{a}_2,\text{Gluon\$1}} f_{\text{a}_3,\text{a}_4,\text{Gluon\$1}}+i \;\text{gs}^2 \eta _{\mu _1,\mu _3} \eta _{\mu _2,\mu _4} f_{\text{a}_1,\text{a}_4,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_3,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _3} \eta _{\mu _2,\mu _4} f_{\text{a}_1,\text{a}_2,\text{Gluon\$1}} f_{\text{a}_3,\text{a}_4,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _2} \eta _{\mu _3,\mu _4} f_{\text{a}_1,\text{a}_4,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_3,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _2} \eta _{\mu _3,\mu _4} f_{\text{a}_1,\text{a}_3,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_4,\text{Gluon\$1}} \\
 \left(
\begin{array}{cc}
 \;\text{ghG}^{\dagger } & 1 \\
 \;\text{ghG} & 2 \\
 G & 3 \\
\end{array}
\right) & \;\text{gs} f_{\text{a}_3,\text{a}_1,\text{a}_2} \;\text{p}_1^{\mu _3} \\
 \left(
\begin{array}{cc}
 \;\text{ghG}^{\dagger } & 1 \\
 \;\text{ghG} & 2 \\
 B & 3 \\
 G & 4 \\
\end{array}
\right) & i \;\text{gs}^2 \eta _{\mu _3,\mu _4} f_{\text{a}_3,\text{a}_1,\text{Gluon\$1}} f_{\text{a}_4,\text{a}_2,\text{Gluon\$1}} \\
 \left(
\begin{array}{cc}
 B & 1 \\
 B & 2 \\
 G & 3 \\
 G & 4 \\
\end{array}
\right) & -\frac{i \;\text{gs}^2 \eta _{\mu _1,\mu _4} \eta _{\mu _2,\mu _3} f_{\text{a}_1,\text{a}_4,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_3,\text{Gluon\$1}}}{\text{GaugeXi}(G)}-\frac{i \;\text{gs}^2 \eta _{\mu _1,\mu _3} \eta _{\mu _2,\mu _4} f_{\text{a}_1,\text{a}_3,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_4,\text{Gluon\$1}}}{\text{GaugeXi}(G)}+i \;\text{gs}^2 \eta _{\mu _1,\mu _4} \eta _{\mu _2,\mu _3} f_{\text{a}_1,\text{a}_3,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_4,\text{Gluon\$1}}+i \;\text{gs}^2 \eta _{\mu _1,\mu _4} \eta _{\mu _2,\mu _3} f_{\text{a}_1,\text{a}_2,\text{Gluon\$1}} f_{\text{a}_3,\text{a}_4,\text{Gluon\$1}}+i \;\text{gs}^2 \eta _{\mu _1,\mu _3} \eta _{\mu _2,\mu _4} f_{\text{a}_1,\text{a}_4,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_3,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _3} \eta _{\mu _2,\mu _4} f_{\text{a}_1,\text{a}_2,\text{Gluon\$1}} f_{\text{a}_3,\text{a}_4,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _2} \eta _{\mu _3,\mu _4} f_{\text{a}_1,\text{a}_4,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_3,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _2} \eta _{\mu _3,\mu _4} f_{\text{a}_1,\text{a}_3,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_4,\text{Gluon\$1}} \\
 \left(
\begin{array}{cc}
 G & 1 \\
 G & 2 \\
 G & 3 \\
\end{array}
\right) & -\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _2} \;\text{p}_1^{\mu _3}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _2} \;\text{p}_2^{\mu _3}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _3} \;\text{p}_1^{\mu _2}-\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _1,\mu _3} \;\text{p}_3^{\mu _2}-\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _2,\mu _3} \;\text{p}_2^{\mu _1}+\text{gs} f_{\text{a}_1,\text{a}_2,\text{a}_3} \eta _{\mu _2,\mu _3} \;\text{p}_3^{\mu _1} \\
 \left(
\begin{array}{cc}
 B & 1 \\
 G & 2 \\
 G & 3 \\
 G & 4 \\
\end{array}
\right) & i \;\text{gs}^2 \eta _{\mu _1,\mu _4} \eta _{\mu _2,\mu _3} f_{\text{a}_1,\text{a}_3,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_4,\text{Gluon\$1}}+i \;\text{gs}^2 \eta _{\mu _1,\mu _4} \eta _{\mu _2,\mu _3} f_{\text{a}_1,\text{a}_2,\text{Gluon\$1}} f_{\text{a}_3,\text{a}_4,\text{Gluon\$1}}+i \;\text{gs}^2 \eta _{\mu _1,\mu _3} \eta _{\mu _2,\mu _4} f_{\text{a}_1,\text{a}_4,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_3,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _3} \eta _{\mu _2,\mu _4} f_{\text{a}_1,\text{a}_2,\text{Gluon\$1}} f_{\text{a}_3,\text{a}_4,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _2} \eta _{\mu _3,\mu _4} f_{\text{a}_1,\text{a}_4,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_3,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _2} \eta _{\mu _3,\mu _4} f_{\text{a}_1,\text{a}_3,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_4,\text{Gluon\$1}} \\
 \left(
\begin{array}{cc}
 G & 1 \\
 G & 2 \\
 G & 3 \\
 G & 4 \\
\end{array}
\right) & i \;\text{gs}^2 \eta _{\mu _1,\mu _4} \eta _{\mu _2,\mu _3} f_{\text{a}_1,\text{a}_3,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_4,\text{Gluon\$1}}+i \;\text{gs}^2 \eta _{\mu _1,\mu _4} \eta _{\mu _2,\mu _3} f_{\text{a}_1,\text{a}_2,\text{Gluon\$1}} f_{\text{a}_3,\text{a}_4,\text{Gluon\$1}}+i \;\text{gs}^2 \eta _{\mu _1,\mu _3} \eta _{\mu _2,\mu _4} f_{\text{a}_1,\text{a}_4,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_3,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _3} \eta _{\mu _2,\mu _4} f_{\text{a}_1,\text{a}_2,\text{Gluon\$1}} f_{\text{a}_3,\text{a}_4,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _2} \eta _{\mu _3,\mu _4} f_{\text{a}_1,\text{a}_4,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_3,\text{Gluon\$1}}-i \;\text{gs}^2 \eta _{\mu _1,\mu _2} \eta _{\mu _3,\mu _4} f_{\text{a}_1,\text{a}_3,\text{Gluon\$1}} f_{\text{a}_2,\text{a}_4,\text{Gluon\$1}} \\
 \left(
\begin{array}{cc}
 \overset{-}{\text{dq}} & 1 \\
 \;\text{dq} & 2 \\
 B & 3 \\
\end{array}
\right) & -i \;\text{gs} \delta _{\text{f}_1,\text{f}_2} T_{\text{m}_1,\text{m}_2}^{\text{a}_3} \gamma _{\text{s}_1,\text{s}_2}{}^{\mu _3} \\
 \left(
\begin{array}{cc}
 \overset{-}{\text{uq}} & 1 \\
 \;\text{uq} & 2 \\
 B & 3 \\
\end{array}
\right) & -i \;\text{gs} \delta _{\text{f}_1,\text{f}_2} T_{\text{m}_1,\text{m}_2}^{\text{a}_3} \gamma _{\text{s}_1,\text{s}_2}{}^{\mu _3} \\
 \left(
\begin{array}{cc}
 \overset{-}{\text{dq}} & 1 \\
 \;\text{dq} & 2 \\
 G & 3 \\
\end{array}
\right) & -i \;\text{gs} \delta _{\text{f}_1,\text{f}_2} T_{\text{m}_1,\text{m}_2}^{\text{a}_3} \gamma _{\text{s}_1,\text{s}_2}{}^{\mu _3} \\
 \left(
\begin{array}{cc}
 \overset{-}{\text{uq}} & 1 \\
 \;\text{uq} & 2 \\
 G & 3 \\
\end{array}
\right) & -i \;\text{gs} \delta _{\text{f}_1,\text{f}_2} T_{\text{m}_1,\text{m}_2}^{\text{a}_3} \gamma _{\text{s}_1,\text{s}_2}{}^{\mu _3} \\
\end{array}
\right)$$

### Create FeynArts model

```mathematica
SetDirectory[FileNameJoin[{$UserBaseDirectory, "Applications", "FeynCalc", "FeynArts", "Models"}]];
WriteFeynArtsOutput[LQCD, Output -> "QCDBGF", CouplingRename -> False,SelectParticles -> {
     {ghG, ghGbar, B}, {ghG, ghGbar, B, B}, {B, G, G}, 
     {ghG, ghGbar, G}, {ghG, ghGbar, B, G}, {B, B, G, G}, 
     {G, G, G}, {B, G, G, G}, {G, G, G, G}, {uqbar, uq, G}, {dqbar, dq, G}, 
     {uqbar, uq, B}, {dqbar, dq, B}}];
```

$$\text{ - - - FeynRules interface to FeynArts - - -}$$

$$\text{      C. Degrande C. Duhr, 2013}$$

$$\text{      Counterterms: B. Fuks, 2012}$$

$$\text{Calculating Feynman rules for }\;\text{L1}$$

$$\text{Starting Feynman rules calculation for L1.}$$

$$\text{Expanding the Lagrangian...}$$

$$\text{Selecting specified field content. Warning! Only mass eigenstates should be selected!}$$

$$\text{Neglecting all terms with more than }4\text{ particles.}$$

$$\text{Neglecting all terms with less than }3\text{ particles.}$$

$$\text{Collecting the different structures that enter the vertex.}$$

$$13\text{ possible non-zero vertices have been found -$>$ starting the computation: }\;\text{FeynRules$\grave{ }$FR\$FeynmanRules}\;\text{ / }13.$$

$$13\text{ vertices obtained.}$$

$$\text{mytimecheck,after LGC}$$

$$\text{Writing FeynArts model file into directory }\;\text{QCDBGF}$$

$$\text{Writing FeynArts generic file on }\;\text{QCDBGF.gen}.$$