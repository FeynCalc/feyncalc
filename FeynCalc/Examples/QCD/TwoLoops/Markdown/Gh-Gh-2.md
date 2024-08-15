---
title: QCD ghost self-energy
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Gh -> Gh, massless QCD, 2-loops";
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

$$\text{FeynCalc }\;\text{10.0.0 (dev version, 2024-08-07 16:59:34 +02:00, 2f62a22c). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{ The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.11 (3 Aug 2020) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

## Generate Feynman diagrams

Nicer typesetting

```mathematica
diags = InsertFields[CreateTopologies[2, 1 -> 1, ExcludeTopologies -> {Tadpoles}], {U[5]} -> {U[5]}, 
    InsertionLevel -> {Classes}, ExcludeParticles -> {V[1 | 2 | 3], S[_]}, Model -> SMQCD];
```

```mathematica
Paint[diags, ColumnsXRows -> {4, 1}, SheetHeader -> False,   
   Numbering -> True, ImageSize -> {1024, 256}];
```

![05sh7fbq1em3j](img/05sh7fbq1em3j.svg)

![08o391s12dph8](img/08o391s12dph8.svg)

![07bb7u7mnbgfv](img/07bb7u7mnbgfv.svg)

## Obtain the amplitude

```mathematica
ampRaw = FCFAConvert[CreateFeynAmp[DiagramExtract[diags, {2, 3, 4, 6, 7, 8, 9}], Truncated -> True, 
     	PreFactor -> 1, GaugeRules -> {}], IncomingMomenta -> {p}, OutgoingMomenta -> {p}, LoopMomenta -> {q1, q2}, 
    	UndoChiralSplittings -> True, ChangeDimension -> D, List -> True, SMP -> True, 
    	DropSumOver -> True, FinalSubstitutions -> {MQU[Index[Generation, 3]] -> 0, GaugeXi[_] -> 1 - GaugeXi}] // SMPToSymbol;
```

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
ScalarProduct[p, p] = pp;
```

## Calculate the amplitude

```mathematica
AbsoluteTiming[ampSimp = (ampRaw) // Contract // DiracSimplify // SUNSimplify;]
```

$$\{2.51085,\text{Null}\}$$

## Identify and minimize the topologies

```mathematica
{amp, topos} = FCLoopFindTopologies[ampSimp, {q1, q2}];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }3$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }3$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

```mathematica
subtopos = FCLoopFindSubtopologies[topos];
```

```mathematica
mappings = FCLoopFindTopologyMappings[topos, PreferredTopologies -> subtopos];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }2\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

## Rewrite the amplitude in terms of GLIs

```mathematica
AbsoluteTiming[ampReduced = FCLoopTensorReduce[amp, topos];]
```

$$\{1.10573,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampPreFinal = FCLoopApplyTopologyMappings[ampReduced, mappings];]
```

$$\{0.38536,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampFinal = ampPreFinal // DiracSimplify // SUNSimplify;]
```

$$\{1.55753,\text{Null}\}$$

```mathematica
(*FCReloadAddOns[{"FeynHelpers"}];
FIREPrepareStartFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateLiteRedFiles[FCGetNotebookDirectory[],mappings[[2]]]
FIRECreateStartFile[FCGetNotebookDirectory[],mappings[[2]]]
FIRECreateConfigFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateIntegralFile[Cases2[ampPreFinal,GLI],mappings[[2]],FCGetNotebookDirectory[]]
FIRERunReduction[FCGetNotebookDirectory[],mappings[[2]]]
tables=FIREImportResults[mappings[[2]],FCGetNotebookDirectory[]]//Flatten;
Put[tables,FileNameJoin[{FCGetNotebookDirectory[],"ReductionTable-Gh-Gh.m"}]];*)
```

```mathematica
reductionTable = Get[FileNameJoin[{FCGetNotebookDirectory[], "ReductionTable-Gh-Gh.m"}]];
```

```mathematica
resPreFinal = Collect2[Total[ampFinal /. reductionTable] // FeynAmpDenominatorExplicit, GLI]
```

$$\frac{1}{16 (D-6) (D-4)^2}i \;\text{gs}^4 C_A G^{\text{fctopology1}}(0,1,1,0,1) \delta ^{\text{Glu1}\;\text{Glu2}} \left(6 D^5 \xi ^2 C_A-115 D^4 \xi ^2 C_A+2 D^4 \xi  C_A+867 D^3 \xi ^2 C_A-130 D^3 \xi  C_A-16 D^3 C_A-3216 D^2 \xi ^2 C_A+1188 D^2 \xi  C_A+24 D^2 C_A+5884 D \xi ^2 C_A-3840 D \xi  C_A+432 D C_A-4256 \xi ^2 C_A+4160 \xi  C_A-1024 C_A+16 D^3-128 D^2+320 D-256\right)-\frac{i \;\text{gs}^4 \;\text{pp} C_A^2 \left(2 D^2 \xi ^2-8 D^2 \xi -15 D \xi ^2+58 D \xi -16 D+26 \xi ^2-104 \xi +56\right) G^{\text{fctopology1}}(1,1,0,1,1) \delta ^{\text{Glu1}\;\text{Glu2}}}{32 (D-4)}$$

```mathematica
integralMappings = FCLoopFindIntegralMappings[Cases2[resPreFinal, GLI], mappings[[2]]]
```

$$\left\{\{\},\left\{G^{\text{fctopology1}}(0,1,1,0,1),G^{\text{fctopology1}}(1,1,0,1,1)\right\}\right\}$$

```mathematica
resFinal = Collect2[resPreFinal /. integralMappings[[1]], GLI]
```

$$\frac{1}{16 (D-6) (D-4)^2}i \;\text{gs}^4 C_A G^{\text{fctopology1}}(0,1,1,0,1) \delta ^{\text{Glu1}\;\text{Glu2}} \left(6 D^5 \xi ^2 C_A-115 D^4 \xi ^2 C_A+2 D^4 \xi  C_A+867 D^3 \xi ^2 C_A-130 D^3 \xi  C_A-16 D^3 C_A-3216 D^2 \xi ^2 C_A+1188 D^2 \xi  C_A+24 D^2 C_A+5884 D \xi ^2 C_A-3840 D \xi  C_A+432 D C_A-4256 \xi ^2 C_A+4160 \xi  C_A-1024 C_A+16 D^3-128 D^2+320 D-256\right)-\frac{i \;\text{gs}^4 \;\text{pp} C_A^2 \left(2 D^2 \xi ^2-8 D^2 \xi -15 D \xi ^2+58 D \xi -16 D+26 \xi ^2-104 \xi +56\right) G^{\text{fctopology1}}(1,1,0,1,1) \delta ^{\text{Glu1}\;\text{Glu2}}}{32 (D-4)}$$

```mathematica
ruleMasters = {
   GLI["fctopology1", {0, 1, 1, 0, 1}] -> (-pp)^(1 - 2*ep)*(13/8 + 1/(4*ep) + (115*ep)/16 + (49*ep^2)/2 - (ep*Zeta2)/4 - (13*ep^2*Zeta2)/8 + (9*ep^2*(9/4 - 2*Zeta[3]))/8 - (5*ep^2*Zeta[3])/12), 
   GLI["fctopology1", {1, 1, 0, 1, 1}] -> (2 + ep^(-1) + 4*ep + (16*ep^2)/3 - (ep*Zeta2)/2 - ep^2*Zeta2 + (4*ep^2*(2 - 2*Zeta[3]))/3 + (ep^2*Zeta[3])/3)^2/(-pp)^(2*ep) 
  }
```

$$\left\{G^{\text{fctopology1}}(0,1,1,0,1)\to (-\text{pp})^{1-2 \;\text{ep}} \left(-\frac{13}{8} \;\text{ep}^2 \zeta (2)-\frac{5 \;\text{ep}^2 \zeta (3)}{12}+\frac{9}{8} \;\text{ep}^2 \left(\frac{9}{4}-2 \zeta (3)\right)+\frac{49 \;\text{ep}^2}{2}-\frac{1}{4} \;\text{ep} \zeta (2)+\frac{115 \;\text{ep}}{16}+\frac{1}{4 \;\text{ep}}+\frac{13}{8}\right),G^{\text{fctopology1}}(1,1,0,1,1)\to (-\text{pp})^{-2 \;\text{ep}} \left(\text{ep}^2 (-\zeta (2))+\frac{\text{ep}^2 \zeta (3)}{3}+\frac{4}{3} \;\text{ep}^2 (2-2 \zeta (3))+\frac{16 \;\text{ep}^2}{3}-\frac{1}{2} \;\text{ep} \zeta (2)+4 \;\text{ep}+\frac{1}{\text{ep}}+2\right)^2\right\}$$

```mathematica
resEpPre = FCReplaceD[resFinal /. ruleMasters, D -> 4 - 2 ep]
```

$$\frac{1}{64 (-2 \;\text{ep}-2) \;\text{ep}^2}i \;\text{gs}^4 C_A (-\text{pp})^{1-2 \;\text{ep}} \left(-\frac{13}{8} \;\text{ep}^2 \zeta (2)-\frac{5 \;\text{ep}^2 \zeta (3)}{12}+\frac{9}{8} \;\text{ep}^2 \left(\frac{9}{4}-2 \zeta (3)\right)+\frac{49 \;\text{ep}^2}{2}-\frac{1}{4} \;\text{ep} \zeta (2)+\frac{115 \;\text{ep}}{16}+\frac{1}{4 \;\text{ep}}+\frac{13}{8}\right) \delta ^{\text{Glu1}\;\text{Glu2}} \left(6 (4-2 \;\text{ep})^5 \xi ^2 C_A-115 (4-2 \;\text{ep})^4 \xi ^2 C_A+2 (4-2 \;\text{ep})^4 \xi  C_A+867 (4-2 \;\text{ep})^3 \xi ^2 C_A-130 (4-2 \;\text{ep})^3 \xi  C_A-3216 (4-2 \;\text{ep})^2 \xi ^2 C_A+1188 (4-2 \;\text{ep})^2 \xi  C_A+5884 (4-2 \;\text{ep}) \xi ^2 C_A-3840 (4-2 \;\text{ep}) \xi  C_A-16 (4-2 \;\text{ep})^3 C_A+24 (4-2 \;\text{ep})^2 C_A+432 (4-2 \;\text{ep}) C_A-4256 \xi ^2 C_A+4160 \xi  C_A-1024 C_A+16 (4-2 \;\text{ep})^3-128 (4-2 \;\text{ep})^2+320 (4-2 \;\text{ep})-256\right)+\frac{1}{64 \;\text{ep}}i \;\text{gs}^4 \;\text{pp} C_A^2 \left(2 (4-2 \;\text{ep})^2 \xi ^2-8 (4-2 \;\text{ep})^2 \xi -15 (4-2 \;\text{ep}) \xi ^2+58 (4-2 \;\text{ep}) \xi -16 (4-2 \;\text{ep})+26 \xi ^2-104 \xi +56\right) (-\text{pp})^{-2 \;\text{ep}} \left(\text{ep}^2 (-\zeta (2))+\frac{\text{ep}^2 \zeta (3)}{3}+\frac{4}{3} \;\text{ep}^2 (2-2 \zeta (3))+\frac{16 \;\text{ep}^2}{3}-\frac{1}{2} \;\text{ep} \zeta (2)+4 \;\text{ep}+\frac{1}{\text{ep}}+2\right)^2 \delta ^{\text{Glu1}\;\text{Glu2}}$$

To bring our result into the suitable form comparable with the literature, we  must divide it  (1- Zeta2/2 ep^2)^2  and again expand it in ep. This yields a prefactor called eta^2. 
We also factor out the prefactor (-pp)^(-2ep)

```mathematica
resEp = Collect2[Series[eta^2/(1 - Zeta2/2 ep^2)^2 FCReplaceD[Cancel[resEpPre/(-pp)^(-2 ep)], D -> 4 - 2 ep], {ep, 0, 0}] // Normal // SUNSimplify, ep, CA, ep]
```

$$-\frac{i \;\text{eta}^2 \;\text{gs}^4 \left(\xi ^2-14 \xi -40\right) \;\text{pp} C_A^2 \delta ^{\text{Glu1}\;\text{Glu2}}}{32 \;\text{ep}^2}-\frac{i \;\text{eta}^2 \;\text{gs}^4 \;\text{pp} C_A \delta ^{\text{Glu1}\;\text{Glu2}}}{4 \;\text{ep}^2}+\frac{i \;\text{eta}^2 \;\text{gs}^4 (7 \xi +166) \;\text{pp} C_A^2 \delta ^{\text{Glu1}\;\text{Glu2}}}{32 \;\text{ep}}-\frac{7 i \;\text{eta}^2 \;\text{gs}^4 \;\text{pp} C_A \delta ^{\text{Glu1}\;\text{Glu2}}}{8 \;\text{ep}}-\frac{53}{16} i \;\text{eta}^2 \;\text{gs}^4 \;\text{pp} C_A \delta ^{\text{Glu1}\;\text{Glu2}}-\frac{1}{64} i \;\text{eta}^2 \;\text{gs}^4 \;\text{pp} C_A^2 \left(-24 \xi ^2+9 \xi +12 \xi ^2 \zeta (3)+48 \zeta (3)-1198\right) \delta ^{\text{Glu1}\;\text{Glu2}}$$

## Check the final results

```mathematica
G2xPaper = (((CA^2*eta^2*gs^4)/(-pp)^(2*ep)) * ((83/16 + 7/32*GaugeXi)/ep + (5/4 + 7/16*GaugeXi - 1/32 GaugeXi^2)/ep^2 + 
        599/32 - 3/4 Zeta[3] - 9/64 GaugeXi + 3/8 GaugeXi^2 - 3/16 GaugeXi^2 Zeta[3] )/(4*Pi)^D);
```

```mathematica
G2qPaper = (((CA*eta^2*gs^4*Tf)/(-pp)^(2*ep))*(-53/8 - 1/(2*ep^2) - 7/(4*ep))/(4*Pi)^D);
```

```mathematica
G2xPaperFinal = pp FCI@SUNDelta[Glu1, Glu2] Collect2[Series[FCReplaceD[I (4 Pi)^D Cancel[(G2xPaper)/(-pp)^(-2 ep)], {D -> 4 - 2 ep}], {ep, 0, 0}] // Normal // PowerExpand, ep]
G2qPaperFinal = pp FCI@SUNDelta[Glu1, Glu2] Collect2[Series[FCReplaceD[I (4 Pi)^D Cancel[(G2qPaper)/(-pp)^(-2 ep)], {D -> 4 - 2 ep}], {ep, 0, 0}] // Normal // PowerExpand, ep] /. Tf -> 1/2
```

$$\text{pp} \delta ^{\text{Glu1}\;\text{Glu2}} \left(-\frac{i \;\text{eta}^2 \;\text{gs}^4 \left(\xi ^2-14 \xi -40\right) C_A^2}{32 \;\text{ep}^2}+\frac{i \;\text{eta}^2 \;\text{gs}^4 (7 \xi +166) C_A^2}{32 \;\text{ep}}-\frac{1}{64} i \;\text{eta}^2 \;\text{gs}^4 C_A^2 \left(-24 \xi ^2+9 \xi +12 \xi ^2 \zeta (3)+48 \zeta (3)-1198\right)\right)$$

$$\text{pp} \delta ^{\text{Glu1}\;\text{Glu2}} \left(-\frac{i \;\text{eta}^2 \;\text{gs}^4 C_A}{4 \;\text{ep}^2}-\frac{7 i \;\text{eta}^2 \;\text{gs}^4 C_A}{8 \;\text{ep}}-\frac{53}{16} i \;\text{eta}^2 \;\text{gs}^4 C_A\right)$$

```mathematica
resLit = G2xPaperFinal + G2qPaperFinal
```

$$\text{pp} \delta ^{\text{Glu1}\;\text{Glu2}} \left(-\frac{i \;\text{eta}^2 \;\text{gs}^4 C_A}{4 \;\text{ep}^2}-\frac{7 i \;\text{eta}^2 \;\text{gs}^4 C_A}{8 \;\text{ep}}-\frac{53}{16} i \;\text{eta}^2 \;\text{gs}^4 C_A\right)+\text{pp} \delta ^{\text{Glu1}\;\text{Glu2}} \left(-\frac{i \;\text{eta}^2 \;\text{gs}^4 \left(\xi ^2-14 \xi -40\right) C_A^2}{32 \;\text{ep}^2}+\frac{i \;\text{eta}^2 \;\text{gs}^4 (7 \xi +166) C_A^2}{32 \;\text{ep}}-\frac{1}{64} i \;\text{eta}^2 \;\text{gs}^4 C_A^2 \left(-24 \xi ^2+9 \xi +12 \xi ^2 \zeta (3)+48 \zeta (3)-1198\right)\right)$$

```mathematica
FCCompareResults[resLit, resEp, 
       Text -> {"\tCompare to Davydychev, Osland and Tarasov,     hep-ph/9801380, Eqs. 6.14-6.15:", "CORRECT.", "WRONG!"}, 
       Interrupt -> {Hold[Quit[1]], Automatic}, 
       Factoring -> Simplify]; 
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], 
       " s."];

```mathematica

$$\text{$\backslash $tCompare to Davydychev, Osland and Tarasov,     hep-ph/9801380, Eqs. 6.14-6.15:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }21.73\text{ s.}$$