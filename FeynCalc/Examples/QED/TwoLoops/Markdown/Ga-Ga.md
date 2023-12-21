---
title: QED photon self-energy
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Ga -> Ga, massless QED, 2-loops";
If[ $FrontEnd === Null, 
  	$FeynCalcStartupMessages = False; 
  	Print[description]; 
  ];
If[ $Notebooks === False, 
  	$FeynCalcStartupMessages = False 
  ];
$LoadAddOns = {"FeynArts"};
<< FeynCalc`
$FAVerbose = 0; 
 
FCCheckVersion[10, 0, 0];
```

$$\text{FeynCalc }\;\text{10.0.0 (dev version, 2023-12-20 22:40:59 +01:00, dff3b835). For help, use the }\underline{\text{online} \;\text{documentation}}\;\text{, check out the }\underline{\text{wiki}}\;\text{ or visit the }\underline{\text{forum}.}$$

$$\text{Please check our }\underline{\text{FAQ}}\;\text{ for answers to some common FeynCalc questions and have a look at the supplied }\underline{\text{examples}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.11 (3 Aug 2020) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

## Generate Feynman diagrams

Nicer typesetting

```mathematica
diags = InsertFields[CreateTopologies[2, 1 -> 1, ExcludeTopologies -> {Tadpoles}], {V[1]} -> {V[1]}, 
    InsertionLevel -> {Particles}, ExcludeParticles -> {V[2 | 3], S[_], U[_], F[1 | 3 | 4]}];
```

```mathematica
Paint[DiagramExtract[diags, {1, 4, 7}], ColumnsXRows -> {3, 1}, SheetHeader -> False,   
   Numbering -> None, ImageSize -> {768, 256}];
```

![0szqcroaoy84c](img/0szqcroaoy84c.svg)

## Obtain the amplitude

```mathematica
ampRaw = FCFAConvert[CreateFeynAmp[DiagramExtract[diags, {1, 4, 7}], Truncated -> True, GaugeRules -> {}, 
     	PreFactor -> 1], IncomingMomenta -> {p}, OutgoingMomenta -> {p},LoopMomenta -> {q1, q2}, 
    	UndoChiralSplittings -> True, ChangeDimension -> D, List -> True, SMP -> True, 
    	DropSumOver -> True] // SMPToSymbol;
```

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
ScalarProduct[p, p] = pp;
```

## Calculate the amplitude

```mathematica
AbsoluteTiming[ampSimp = DiracSimplify[ampRaw /. me -> 0];]
```

$$\{0.571108,\text{Null}\}$$

## Identify and minimize the topologies

```mathematica
{amp, topos} = FCLoopFindTopologies[ampSimp, {q1, q2}];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }2$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }2$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

```mathematica
subtopos = FCLoopFindSubtopologies[topos];
```

```mathematica
mappings = FCLoopFindTopologyMappings[topos, PreferredTopologies -> subtopos];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }1\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

## Rewrite the amplitude in terms of GLIs

```mathematica
AbsoluteTiming[ampReduced = FCLoopTensorReduce[amp, topos];]
```

$$\{0.430326,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampPreFinal = FCLoopApplyTopologyMappings[ampReduced, mappings];]
```

$$\{0.344109,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampFinal = ampPreFinal // DiracSimplify;]
```

$$\{0.007818,\text{Null}\}$$

```mathematica
(*FCReloadAddOns[{"FeynHelpers"}];
FIREPrepareStartFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateStartFile[FCGetNotebookDirectory[],mappings[[2]]]
FIRECreateConfigFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateIntegralFile[Cases2[ampPreFinal,GLI],mappings[[2]],FCGetNotebookDirectory[]]
FIRERunReduction[FCGetNotebookDirectory[],mappings[[2]]]
tables=FIREImportResults[mappings[[2]],FCGetNotebookDirectory[]]//Flatten;
Put[tables,FileNameJoin[{FCGetNotebookDirectory[],"ReductionTable-Ga-Ga.m"}]];*)
```

```mathematica
reductionTable = Get[FileNameJoin[{FCGetNotebookDirectory[], "ReductionTable-Ga-Ga.m"}]];
```

```mathematica
resPreFinal = Collect2[Total[ampFinal /. reductionTable], GLI]
```

$$-\frac{1}{3 (D-4)^2 (D-1) \;\text{pp}}2 i (D-2) e^4 G^{\text{fctopology1}}(0,1,1,0,1) \left(3 D^3 \;\text{pp} \xi _A g^{\text{Lor1}\;\text{Lor2}}-4 D^3 \xi _A p^{\text{Lor1}} p^{\text{Lor2}}-23 D^2 \;\text{pp} \xi _A g^{\text{Lor1}\;\text{Lor2}}+32 D^2 \xi _A p^{\text{Lor1}} p^{\text{Lor2}}+52 D \;\text{pp} \xi _A g^{\text{Lor1}\;\text{Lor2}}-76 D \xi _A p^{\text{Lor1}} p^{\text{Lor2}}-32 \;\text{pp} \xi _A g^{\text{Lor1}\;\text{Lor2}}+48 \xi _A p^{\text{Lor1}} p^{\text{Lor2}}+6 D^3 \;\text{pp} g^{\text{Lor1}\;\text{Lor2}}-6 D^3 p^{\text{Lor1}} p^{\text{Lor2}}-42 D^2 \;\text{pp} g^{\text{Lor1}\;\text{Lor2}}+42 D^2 p^{\text{Lor1}} p^{\text{Lor2}}+120 D \;\text{pp} g^{\text{Lor1}\;\text{Lor2}}-120 D p^{\text{Lor1}} p^{\text{Lor2}}-144 \;\text{pp} g^{\text{Lor1}\;\text{Lor2}}+144 p^{\text{Lor1}} p^{\text{Lor2}}\right)+\frac{1}{3 (D-4)^2 (D-1) \;\text{pp}}2 i (D-2) e^4 G^{\text{fctopology1}}(1,0,1,1,0) \left(3 D^3 \;\text{pp} \xi _A g^{\text{Lor1}\;\text{Lor2}}-4 D^3 \xi _A p^{\text{Lor1}} p^{\text{Lor2}}-23 D^2 \;\text{pp} \xi _A g^{\text{Lor1}\;\text{Lor2}}+32 D^2 \xi _A p^{\text{Lor1}} p^{\text{Lor2}}+52 D \;\text{pp} \xi _A g^{\text{Lor1}\;\text{Lor2}}-76 D \xi _A p^{\text{Lor1}} p^{\text{Lor2}}-32 \;\text{pp} \xi _A g^{\text{Lor1}\;\text{Lor2}}+48 \xi _A p^{\text{Lor1}} p^{\text{Lor2}}-6 D^3 \;\text{pp} g^{\text{Lor1}\;\text{Lor2}}+6 D^3 p^{\text{Lor1}} p^{\text{Lor2}}+42 D^2 \;\text{pp} g^{\text{Lor1}\;\text{Lor2}}-42 D^2 p^{\text{Lor1}} p^{\text{Lor2}}-120 D \;\text{pp} g^{\text{Lor1}\;\text{Lor2}}+120 D p^{\text{Lor1}} p^{\text{Lor2}}+144 \;\text{pp} g^{\text{Lor1}\;\text{Lor2}}-144 p^{\text{Lor1}} p^{\text{Lor2}}\right)+\frac{2 i (D-2) \left(D^2-7 D+16\right) e^4 G^{\text{fctopology1}}(1,1,0,1,1) \left(\text{pp} g^{\text{Lor1}\;\text{Lor2}}-p^{\text{Lor1}} p^{\text{Lor2}}\right)}{(D-4) (D-1)}$$

```mathematica
integralMappings = FCLoopFindIntegralMappings[Cases2[resPreFinal, GLI], mappings[[2]]]
```

$$\left\{\left\{G^{\text{fctopology1}}(1,0,1,1,0)\to G^{\text{fctopology1}}(0,1,1,0,1)\right\},\left\{G^{\text{fctopology1}}(0,1,1,0,1),G^{\text{fctopology1}}(1,1,0,1,1)\right\}\right\}$$

```mathematica
resFinal = Collect2[resPreFinal /. integralMappings[[1]], GLI]
```

$$\frac{2 i (D-2) \left(D^2-7 D+16\right) e^4 G^{\text{fctopology1}}(1,1,0,1,1) \left(\text{pp} g^{\text{Lor1}\;\text{Lor2}}-p^{\text{Lor1}} p^{\text{Lor2}}\right)}{(D-4) (D-1)}-\frac{8 i (D-3) (D-2) \left(D^2-4 D+8\right) e^4 G^{\text{fctopology1}}(0,1,1,0,1) \left(\text{pp} g^{\text{Lor1}\;\text{Lor2}}-p^{\text{Lor1}} p^{\text{Lor2}}\right)}{(D-4)^2 (D-1) \;\text{pp}}$$

## Check the final results

```mathematica
resGrozinVacuumPol = -I FCI[e^4 2 (D - 2)/((D - 1) (D - 4)) (-(D^2 - 7 D + 16) GLI["fctopology1", {1, 1, 0, 1, 1}] + 
        4 (D - 3) (D^2 - 4 D + 8)/(D - 4) (1/SPD[p, p]) GLI["fctopology1", {0, 1, 1, 0, 1}]) (-(FVD[p, Lor1]*FVD[p, Lor2]) + 
        pp*MTD[Lor1, Lor2])];
FCCompareResults[resFinal, resGrozinVacuumPol, 
   Text -> {"\tCompare to Grozin's Lectures on QED and QCD, hep-ph/0508242, Eq. 5.18:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}, Factoring -> Simplify];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Grozin's Lectures on QED and QCD, hep-ph/0508242, Eq. 5.18:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }29.186\text{ s.}$$