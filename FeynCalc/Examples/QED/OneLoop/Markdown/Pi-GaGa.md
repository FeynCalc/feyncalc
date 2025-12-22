---
title: Adler-Bell-Jackiw anomaly in QED
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Pi -> Ga Ga, QED, axial current, 1-loop";
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

## Obtain the amplitude

Nicer typesetting

```mathematica
FCAttachTypesettingRule[mu, "\[Mu]"];
FCAttachTypesettingRule[nu, "\[Nu]"];
FCAttachTypesettingRule[la, "\[Lambda]"];
```

According to Peskin and Schroeder (Ch 19.2), the amplitude for the first triangle diagram reads

```mathematica
amp1[0] = ((-1) (-I SMP["e"])^2 DiracTrace[GAD[mu] . GA[5] . 
       	QuarkPropagator[l - k] . GAD[la] . QuarkPropagator[l] . 
       	GAD[nu] . QuarkPropagator[l + p]]) // ChangeDimension[#, D] & // Explicit
```

$$\text{e}^2 \;\text{tr}\left(\gamma ^{\mu }.\bar{\gamma }^5.\frac{i \gamma \cdot (l-k)}{(l-k)^2}.\gamma ^{\lambda }.\frac{i \gamma \cdot l}{l^2}.\gamma ^{\nu }.\frac{i \gamma \cdot (l+p)}{(l+p)^2}\right)$$

And the second one follows from the first by interchanging k with p and la with nu

```mathematica
amp2[0] = amp1[0] /. {k -> p, p -> k, la -> nu, nu -> la}
```

$$\text{e}^2 \;\text{tr}\left(\gamma ^{\mu }.\bar{\gamma }^5.\frac{i \gamma \cdot (l-p)}{(l-p)^2}.\gamma ^{\nu }.\frac{i \gamma \cdot l}{l^2}.\gamma ^{\lambda }.\frac{i \gamma \cdot (k+l)}{(k+l)^2}\right)$$

```mathematica
amps[0] = {amp1[0], amp2[0]};
```

## Evaluate the amplitudes

Contracting both amplitudes with I*(k+p)^mu we can check the non-conservation of the axial current.

```mathematica
amps[1] = Contract[I*FVD[k + p, mu] (amps[0]), FCParallelize -> True] // FCTraceFactor[#, FCParallelize -> True] &
```

$$\left\{\frac{\text{e}^2 \;\text{tr}\left((\gamma \cdot (k+p)).\bar{\gamma }^5.(\gamma \cdot (l-k)).\gamma ^{\lambda }.(\gamma \cdot l).\gamma ^{\nu }.(\gamma \cdot (l+p))\right)}{l^2 (l-k)^2 (l+p)^2},\frac{\text{e}^2 \;\text{tr}\left((\gamma \cdot (k+p)).\bar{\gamma }^5.(\gamma \cdot (l-p)).\gamma ^{\nu }.(\gamma \cdot l).\gamma ^{\lambda }.(\gamma \cdot (k+l))\right)}{l^2 (k+l)^2 (l-p)^2}\right\}$$

For this calculation it is crucial to use a correct scheme for gamma^5. As in the book, we use the 
Breitenlohner-Maison-t'Hooft-Veltman prescription.

```mathematica
FCClearScalarProducts[];
ScalarProduct[k, k] = 0;
ScalarProduct[p, p] = 0;
ScalarProduct[k, p] = kp;
FCSetDiracGammaScheme["BMHV"];
```

```mathematica
amps[2] = amps[1] // DiracSimplify[#, FCParallelize -> True] &;
```

## Identify and minimize the topologies

```mathematica
{amps[3], topos} = FCLoopFindTopologies[amps[2], {l}, FCParallelize -> True];
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

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }0\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }2$$

```mathematica
toposFinal = mappings[[2]];
```

## Rewrite the amplitude in terms of GLIs

```mathematica
AbsoluteTiming[ampReduced = FCLoopTensorReduce[amps[3], topos, FCParallelize -> True];]
```

$$\{0.243342,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampPreFinal = FCLoopApplyTopologyMappings[ampReduced, mappings, FCParallelize -> True];]
```

$$\{0.139798,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampFinal = ampPreFinal // DiracSimplify[#, FCParallelize -> True] & // 
      FeynAmpDenominatorExplicit // Collect2[#, DOT, FCParallelize -> True] &;]
```

$$\{0.037262,\text{Null}\}$$

```mathematica
ints = Cases2[ampFinal, GLI]
```

$$\left\{G^{\text{fctopology1}}(-1,1,1),G^{\text{fctopology1}}(0,0,1),G^{\text{fctopology1}}(0,1,0),G^{\text{fctopology1}}(0,1,1),G^{\text{fctopology1}}(1,0,0),G^{\text{fctopology2}}(-1,1,1),G^{\text{fctopology2}}(0,0,1),G^{\text{fctopology2}}(0,1,0),G^{\text{fctopology2}}(0,1,1),G^{\text{fctopology2}}(1,0,0)\right\}$$

```mathematica
dir = FileNameJoin[{$TemporaryDirectory, "Reduction-PiGaGa"}];
Quiet[CreateDirectory[dir]];
```

```mathematica
KiraCreateJobFile[toposFinal, ints, dir];
```

```mathematica
KiraCreateIntegralFile[ints, toposFinal, dir];
```

$$\text{KiraCreateIntegralFile: Number of loop integrals: }5$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }5$$

```mathematica
KiraCreateConfigFiles[toposFinal, ints, dir, KiraMassDimensions -> {kp -> 2}];
```

```mathematica
KiraRunReduction[dir, toposFinal, 
  KiraBinaryPath -> FileNameJoin[{$HomeDirectory, "bin", "kira"}], 
  KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]
```

$$\{\text{True},\text{True}\}$$

```mathematica
reductionTable = KiraImportResults[toposFinal, dir] // Flatten
```

$$\left\{G^{\text{fctopology1}}(0,0,1)\to 0,G^{\text{fctopology1}}(0,1,0)\to 0,G^{\text{fctopology1}}(1,0,0)\to 0,G^{\text{fctopology1}}(-1,1,1)\to -\text{kp} G^{\text{fctopology1}}(0,1,1),G^{\text{fctopology2}}(0,0,1)\to 0,G^{\text{fctopology2}}(0,1,0)\to 0,G^{\text{fctopology2}}(1,0,0)\to 0,G^{\text{fctopology2}}(-1,1,1)\to -\text{kp} G^{\text{fctopology2}}(0,1,1)\right\}$$

```mathematica
resPreFinal = Collect2[Total[ampFinal /. Dispatch[reductionTable]], GLI, FCParallelize -> True];
```

```mathematica
integralMappings = FCLoopFindIntegralMappings[Cases2[resPreFinal, GLI], mappings[[2]], FCParallelize -> True];
```

```mathematica
resFinal = Collect2[(resPreFinal /. Dispatch[integralMappings[[1]]]), GLI, FCParallelize -> True]
```

$$-\frac{8 i (D-4) \;\text{e}^2 G^{\text{fctopology1}}(0,1,1) \bar{\epsilon }^{\lambda \nu \overline{k}\overline{p}}}{D-2}$$

We only need the pole of the master integral, since the result is proportional to D-4. The result should be twice Eq. 19.59 in Peskin and Schroeder

```mathematica
res = resFinal // ReplaceAll[#, {GLI["fctopology1", {0, 1, 1}] -> I/(16 Pi^2 ep) + epHelp}] & // 
     FCReplaceD[#, {D -> 4 - 2 ep}] & // Series[#, {ep, 0, 0}] & // Normal
```

$$-\frac{\text{e}^2 \bar{\epsilon }^{\lambda \nu \overline{k}\overline{p}}}{2 \pi ^2}$$

## Check the final results

```mathematica
knownResult = 2 (SMP["e"]^2/(4 Pi^2) LC[al, la, be, nu] FV[k, al] FV[p, be]) // Contract;
FCCompareResults[res, knownResult, 
   Text -> {"\tCompare to Peskin and Schroeder, An Introduction to QFT, Eq 19.59:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```mathematica

$$\text{$\backslash $tCompare to Peskin and Schroeder, An Introduction to QFT, Eq 19.59:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }19.967\text{ s.}$$