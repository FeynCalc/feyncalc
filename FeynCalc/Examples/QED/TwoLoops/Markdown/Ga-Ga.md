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
LaunchKernels[4];
$LoadAddOns = {"FeynArts", "FeynHelpers"};
<< FeynCalc`
$FAVerbose = 0;
$ParallelizeFeynCalc = True; 
 
FCCheckVersion[10, 2, 0];
If[ToExpression[StringSplit[$FeynHelpersVersion, "."]][[1]] < 2, 
 	Print["You need at least FeynHelpers 2.0 to run this example."]; 
 	Abort[]; 
 ]
```

$$\text{FeynCalc }\;\text{10.2.0 (dev version, 2025-12-22 21:09:03 +01:00, fcd53f9b). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{ The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.12 (27 Mar 2025) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

$$\text{FeynHelpers }\;\text{2.0.0 (2025-12-22 19:07:44 +01:00, c92fb9f5). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{ If you use FeynHelpers in your research, please evaluate FeynHelpersHowToCite[] to learn how to cite this work.}$$

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

![0p7dijx30cq36](img/0p7dijx30cq36.svg)

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
AbsoluteTiming[ampSimp = DiracSimplify[ampRaw /. me -> 0, 
     FCParallelize -> True];]
```

$$\{0.286306,\text{Null}\}$$

## Identify and minimize the topologies

```mathematica
{amp, topos} = FCLoopFindTopologies[ampSimp, {q1, q2}, FCParallelize -> True];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }2$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }2$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

```mathematica
subtopos = FCLoopFindSubtopologies[topos, FCParallelize -> True];
```

```mathematica
mappings = FCLoopFindTopologyMappings[topos, PreferredTopologies -> subtopos, FCParallelize -> True];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }1\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

## Rewrite the amplitude in terms of GLIs

```mathematica
AbsoluteTiming[ampReduced = FCLoopTensorReduce[amp, topos];]
```

$$\{0.408321,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampPreFinal = FCLoopApplyTopologyMappings[ampReduced, mappings];]
```

$$\{0.322543,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampFinal = ampPreFinal // DiracSimplify[#, FCParallelize -> True] &;]
```

$$\{0.033204,\text{Null}\}$$

```mathematica
dir = FileNameJoin[{$TemporaryDirectory, "Reduction-GaToGa-2L"}];
Quiet[CreateDirectory[dir]];
```

```mathematica
FIREPrepareStartFile[mappings[[2]], dir];
```

```mathematica
FIRECreateLiteRedFiles[dir, mappings[[2]]];
```

```mathematica
FIRECreateStartFile[dir, mappings[[2]]];
```

```mathematica
FIRECreateIntegralFile[Cases2[ampPreFinal, GLI], mappings[[2]], dir];
```

$$\text{FIRECreateIntegralFile: }\;\text{fctopology1}\;\text{, number of loop integrals: }74$$

```mathematica
FIRECreateConfigFile[mappings[[2]], dir];
```

$$\text{FIRECreateConfigFile: Created .config file fctopology1.config for }\;\text{fctopology1}$$

```mathematica
FIRERunReduction[dir, mappings[[2]]];
```

```mathematica
reductionTable = FIREImportResults[mappings[[2]], dir] // Flatten;
```

```mathematica
resPreFinal = Collect2[Total[ampFinal /. reductionTable], GLI]
```

$$\frac{2 i (D-2) \left(D^2-7 D+16\right) e^4 G^{\text{fctopology1}}(1,1,0,1,1) \left(\text{pp} g^{\text{Lor1}\;\text{Lor2}}-p^{\text{Lor1}} p^{\text{Lor2}}\right)}{(D-4) (D-1)}-\frac{8 i (D-3) (D-2) \left(D^2-4 D+8\right) e^4 G^{\text{fctopology1}}(0,1,1,0,1) \left(\text{pp} g^{\text{Lor1}\;\text{Lor2}}-p^{\text{Lor1}} p^{\text{Lor2}}\right)}{(D-4)^2 (D-1) \;\text{pp}}$$

```mathematica
integralMappings = FCLoopFindIntegralMappings[Cases2[resPreFinal, GLI], mappings[[2]]]
```

$$\left\{\{\},\left\{G^{\text{fctopology1}}(0,1,1,0,1),G^{\text{fctopology1}}(1,1,0,1,1)\right\}\right\}$$

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

```mathematica

$$\text{$\backslash $tCompare to Grozin's Lectures on QED and QCD, hep-ph/0508242, Eq. 5.18:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }20.035\text{ s.}$$