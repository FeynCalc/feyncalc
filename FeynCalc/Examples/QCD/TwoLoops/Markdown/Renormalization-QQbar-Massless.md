---
title: 2-loop QCD renormalization in the minimal subtraction schemes
---


## Load FeynCalc and the necessary add-ons or other packages

This example uses a custom QCD model created with FeynRules.

```mathematica
description = "Renormalization, QCD, MSbar, 2-loop";
If[ $FrontEnd === Null, 
  	$FeynCalcStartupMessages = False; 
  	Print[description]; 
  ];
If[ $Notebooks === False, 
  	$FeynCalcStartupMessages = False 
  ];
LaunchKernels[8];
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

$$\text{FeynCalc }\;\text{10.2.0 (dev version, 2026-05-18 14:09:14 +02:00, 9ab9d838). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{ The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.12 (27 Mar 2025) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

$$\text{FeynHelpers }\;\text{2.0.0 (2026-02-05 17:03:01 +02:00, 5db84fbb). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{ The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{ If you use FeynHelpers in your research, please evaluate FeynHelpersHowToCite[] to learn how to cite this work.}$$

## Configure some options

```mathematica
modelDir = FileNameJoin[{$UserBaseDirectory, "Applications", "FeynCalc", "Examples", "Models", "QCD"}]
```

$$\text{/home/vs/.Wolfram/Applications/FeynCalc/Examples/Models/QCD}$$

```mathematica
FAPatch[PatchModelsOnly -> True, FAModelsDirectory -> modelDir];

(*Successfully patched FeynArts.*)
```

```mathematica
renConstants = Zm | Zpsi | ZA | ZAmxt | Zu | Zumxt | Zg | Zxi
```

$$\text{Zm}|\text{Zpsi}|\text{ZA}|\text{ZAmxt}|\text{Zu}|\text{Zumxt}|\text{Zg}|\text{Zxi}$$

## Generate Feynman diagrams

Nicer typesetting

```mathematica
FCAttachTypesettingRule[mu, "\[Mu]"];
FCAttachTypesettingRule[nu, "\[Nu]"];
FCAttachTypesettingRule[rho, "\[Rho]"];
FCAttachTypesettingRule[si, "\[Sigma]"];
```

```mathematica
diagQuarkSE = InsertFields[CreateTopologies[2, 1 -> 1, 
     ExcludeTopologies -> {Tadpoles, WFCorrections, WFCorrectionCTs}], {F[3, {1}]} -> {F[3, {1}]}, 
    InsertionLevel -> {Particles}, Model -> FileNameJoin[{modelDir, "QCD"}], 
    GenericModel -> FileNameJoin[{modelDir, "QCD"}], ExcludeParticles -> {F[3 | 4, {2 | 3}], F[4, {1}]}];
```

```mathematica
diagQuarkSECT = InsertFields[CreateCTTopologies[2, 1 -> 1, 
     ExcludeTopologies -> {Tadpoles, WFCorrections, WFCorrectionCTs}], {F[3, {1}]} -> {F[3, {1}]}, 
    InsertionLevel -> {Particles}, Model -> FileNameJoin[{modelDir, "QCD"}], 
    GenericModel -> FileNameJoin[{modelDir, "QCD"}]];
```

```mathematica
diagQuarkTreeSECT = InsertFields[CreateCTTopologies[1, 1 -> 1, 
     ExcludeTopologies -> {Tadpoles, WFCorrections, WFCorrectionCTs}], {F[3, {1}]} -> {F[3, {1}]}, 
    InsertionLevel -> {Particles}, Model -> FileNameJoin[{modelDir, "QCD"}], 
    GenericModel -> FileNameJoin[{modelDir, "QCD"}]];
```

Self-energy and vertex diagrams

```mathematica
Paint[diagQuarkSE, ColumnsXRows -> {4, 2}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> 128 {4, 2}];
```

![0iw9zunqieoks](img/0iw9zunqieoks.svg)

1-loop counter-term diagrams

```mathematica
Paint[diagQuarkSECT, ColumnsXRows -> {4, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> 128 {4, 1}];
```

![0mrvsztgpvris](img/0mrvsztgpvris.svg)

Tree-level counter-term diagrams

```mathematica
Paint[diagQuarkTreeSECT, ColumnsXRows -> {4, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> 128 {4, 1}];
```

![1dtsvhdtz2pc3](img/1dtsvhdtz2pc3.svg)

## Master integrals

The only required masters are 1- and 2-loop tadpoles

```mathematica
tadpoleMaster = Get[FileNameJoin[{$FeynCalcDirectory, "Examples", "MasterIntegrals","Tadpoles", "tad1LxFx1x1xxEp999x.m"}]];
```

```mathematica
tadpoleMaster1 = tadpoleMaster /. m1 -> mxt /. tad1L -> "tad1Lv1";
```

```mathematica
tadpoleMaster2 = Get[FileNameJoin[{$FeynCalcDirectory, "Examples", "MasterIntegrals","Tadpoles", 
        "tad2LxFx111x111xxEp1x.m"}]] /. m1 -> mxt /. tad2LxFx111x111xxEp1x -> "tad2Lv2";
```

## Obtain the amplitudes

```mathematica
{quarkSE$RawAmp, quarkSECT$RawAmp, diagQuarkTreeSECT$RawAmp} = 
   FCFAConvert[CreateFeynAmp[#, Truncated -> True, 
      	GaugeRules -> {}, PreFactor -> 1], 
     	IncomingMomenta -> {p}, OutgoingMomenta -> {p}, 
     	LorentzIndexNames -> {mu, nu}, DropSumOver -> True, 
     	LoopMomenta -> {k1, k2}, UndoChiralSplittings -> True, 
     	ChangeDimension -> D, SMP -> True, 
     	FinalSubstitutions -> {SMP["m_u"] -> 0, SMP["g_s"] -> 4 Pi Sqrt[as4]}] & /@ {
    	diagQuarkSE, diagQuarkSECT, diagQuarkTreeSECT};
```

## Calculate the amplitudes

### Quark self-energy at 2 loops

The 2-loop quark self-energy has superficial degree of divergence equal to 1

```mathematica
FCClearScalarProducts[];
divDegree = 1;
aux1 = FCLoopGetFeynAmpDenominators[Join[quarkSE$RawAmp[[1 ;; 3]], {Nf quarkSE$RawAmp[[4]]}, quarkSE$RawAmp[[5 ;;]]], 
    {k1, k2}, denHead, Momentum -> {p}, "Massless" -> True];
aux2 = FCLoopAddAuxiliaryMass[aux1[[2]], {k1, k2}, -mxt^2, 0, Head -> denHead]
```

$$\left\{\text{denHead}\left(\frac{1}{(\text{k1}^2+i \eta )}\right)\to \frac{1}{(\text{k1}^2-\text{mxt}^2+i \eta )},\text{denHead}\left(\frac{1}{(\text{k2}^2+i \eta )}\right)\to \frac{1}{(\text{k2}^2-\text{mxt}^2+i \eta )},\text{denHead}\left(\frac{1}{((\text{k1}+\text{k2})^2+i \eta )}\right)\to \frac{1}{((\text{k1}+\text{k2})^2-\text{mxt}^2+i \eta )},\text{denHead}\left(\frac{1}{((\text{k1}-p)^2+i \eta )}\right)\to \frac{1}{((\text{k1}-p)^2-\text{mxt}^2+i \eta )},\text{denHead}\left(\frac{1}{((\text{k1}-\text{k2}-p)^2+i \eta )}\right)\to \frac{1}{((\text{k1}-\text{k2}-p)^2-\text{mxt}^2+i \eta )},\text{denHead}\left(\frac{1}{((\text{k2}-p)^2+i \eta )}\right)\to \frac{1}{((\text{k2}-p)^2-\text{mxt}^2+i \eta )},\text{denHead}\left(\frac{1}{((\text{k1}+p)^2+i \eta )}\right)\to \frac{1}{((\text{k1}+p)^2-\text{mxt}^2+i \eta )},\text{denHead}\left(\frac{1}{((-\text{k1}+\text{k2}+p)^2+i \eta )}\right)\to \frac{1}{((-\text{k1}+\text{k2}+p)^2-\text{mxt}^2+i \eta )}\right\}$$

```mathematica
AbsoluteTiming[quarkSE$Amp = (aux1[[1]] /. aux2) // Contract[#, FCParallelize -> True] & // 
      SUNSimplify[#, FCI -> True, FCParallelize -> True] & // DiracSimplify[#, FCI -> True, FCParallelize -> True] &;]
```

$$\{14.9636,\text{Null}\}$$

```mathematica
isoSymbols = FCMakeSymbols[KK, Range[1, $KernelCount], List]
```

$$\{\text{KK1},\text{KK2},\text{KK3},\text{KK4},\text{KK5},\text{KK6},\text{KK7},\text{KK8}\}$$

```mathematica
AbsoluteTiming[quarkSE$Amp1 = Collect2[quarkSE$Amp, p, IsolateNames -> isoSymbols, FCParallelize -> True];]
```

$$\{3.85082,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSE$Amp2 = FourSeries[quarkSE$Amp1, {p, 0, 1}, FCParallelize -> True];]
```

$$\{2.17755,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSE$Amp3 = Collect2[FRH2[quarkSE$Amp2, isoSymbols], FeynAmpDenominator, FCParallelize -> True];]
```

$$\{0.892859,\text{Null}\}$$

The rest of the calculation follows the standard multiloop template

```mathematica
FCClearScalarProducts[]
SPD[p] = pp;
```

```mathematica
AbsoluteTiming[{quarkSE$Amp4, quarkSE$Topos} = FCLoopFindTopologies[quarkSE$Amp3, {k1, k2}, FCI -> True, FCParallelize -> True, 
     FCLoopBasisOverdeterminedQ -> True, FinalSubstitutions -> {Hold[SPD][p] -> pp}];]
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }3$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }2$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }1$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of found topologies: }2$$

$$\{3.79984,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSE$Amp5 = FCLoopTensorReduce[quarkSE$Amp4, quarkSE$Topos, FCParallelize -> True];]
```

$$\{8.23426,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSE$Amp6 = DiracSimplify[quarkSE$Amp5, FCParallelize -> True];]
```

$$\{1.82229,\text{Null}\}$$

```mathematica
AbsoluteTiming[{quarkSE$Amp7, quarkSE$Topos2} = FCLoopRewriteOverdeterminedTopologies[quarkSE$Amp6, quarkSE$Topos, FCParallelize -> True];]
```

$$\text{FCLoopRewriteIncompleteTopologies: }\;\text{No overdetermined topologies detected.}$$

$$\{0.064621,\text{Null}\}$$

```mathematica
AbsoluteTiming[{quarkSE$Amp8, quarkSE$Topos3} = FCLoopRewriteIncompleteTopologies[quarkSE$Amp7, quarkSE$Topos2, FCParallelize -> True];]
```

$$\text{FCLoopRewriteIncompleteTopologies: }\;\text{No incomplete topologies detected.}$$

$$\{0.082528,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSE$SubTopos = FCLoopFindSubtopologies[quarkSE$Topos3, Flatten -> True, Remove -> True, FCParallelize -> True];]
```

$$\{0.099527,\text{Null}\}$$

```mathematica
{quarkSE$TopoMappings, 
    quarkSE$FinalTopos} = FCLoopFindTopologyMappings[quarkSE$Topos3, PreferredTopologies -> quarkSE$SubTopos, FCParallelize -> True];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }1\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

```mathematica
AbsoluteTiming[quarkSE$AmpGLI = FCLoopApplyTopologyMappings[quarkSE$Amp8, {quarkSE$TopoMappings, 
      quarkSE$FinalTopos}, FCParallelize -> True];]
```

$$\{1.56531,\text{Null}\}$$

```mathematica
quarkSE$GLIs = Cases2[quarkSE$AmpGLI, GLI];
```

```mathematica
quarkSE$dir = FileNameJoin[{$TemporaryDirectory, "Reduction-quarkSE-2L-massive"}];
Quiet[CreateDirectory[quarkSE$dir]];
```

```mathematica
KiraCreateJobFile[quarkSE$FinalTopos, quarkSE$GLIs, quarkSE$dir]
```

$$\{\text{/tmp/Reduction-quarkSE-2L-massive/fctopology1/job.yaml}\}$$

```mathematica
KiraCreateIntegralFile[quarkSE$GLIs, quarkSE$FinalTopos, quarkSE$dir]
KiraCreateConfigFiles[quarkSE$FinalTopos, quarkSE$GLIs, quarkSE$dir, 
  KiraMassDimensions -> {pp -> 2, mxt -> 1}]
```

$$\text{KiraCreateIntegralFile: Number of loop integrals: }118$$

$$\{\text{/tmp/Reduction-quarkSE-2L-massive/fctopology1/KiraLoopIntegrals}\}$$

$$\left(
\begin{array}{cc}
 \;\text{/tmp/Reduction-quarkSE-2L-massive/fctopology1/config/integralfamilies.yaml} & \;\text{/tmp/Reduction-quarkSE-2L-massive/fctopology1/config/kinematics.yaml} \\
\end{array}
\right)$$

```mathematica
KiraRunReduction[quarkSE$dir, quarkSE$FinalTopos, 
  KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}], 
  KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]
```

$$\{\text{True}\}$$

```mathematica
quarkSE$ReductionTables = KiraImportResults[quarkSE$FinalTopos, quarkSE$dir] // Flatten;
```

```mathematica
AbsoluteTiming[quarkSE$resPreFinal1 = (quarkSE$AmpGLI /. Dispatch[quarkSE$ReductionTables]);]
```

$$\{0.014238,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSE$resPreFinal2 = Map[Collect2[#, GLI, DiracGamma, FCParallelize -> True] &, quarkSE$resPreFinal1];]
```

$$\{0.668195,\text{Null}\}$$

```mathematica
quarkSE$masters = Cases2[quarkSE$resPreFinal1, GLI];
```

```mathematica
quarkSE$MIMappings = FCLoopFindIntegralMappings[quarkSE$masters, Join[tadpoleMaster1[[2]], {tadpoleMaster2[[2]]}, 
    quarkSE$FinalTopos], PreferredIntegrals -> {tadpoleMaster1[[1]][[1]] tadpoleMaster1[[1]][[1]], tadpoleMaster2[[1]][[1]]}]
```

$$\left(
\begin{array}{cc}
 G^{\text{fctopology1}}(1,1,0)\to G^{\text{tad1LxFx1x1xxEp999x}}(1)^2 & G^{\text{fctopology1}}(1,1,1)\to G^{\text{tad2Lv2}}(1,1,1) \\
 G^{\text{tad2Lv2}}(1,1,1) & G^{\text{tad1LxFx1x1xxEp999x}}(1)^2 \\
\end{array}
\right)$$

```mathematica
isoSymbols1 = FCMakeSymbols[LL, Range[1, $KernelCount], List];
isoSymbols2 = FCMakeSymbols[LM, Range[1, $KernelCount], List];
```

```mathematica
AbsoluteTiming[quarkSE$resPreFinal2 = Collect2[quarkSE$resPreFinal1, D, GLI, IsolateNames -> isoSymbols1,FCParallelize -> True] // FCReplaceD[#, D -> 4 - 2 ep] & // ReplaceAll[#, quarkSE$MIMappings[[1]]] & // 
      ReplaceAll[#, {tadpoleMaster1[[1]], tadpoleMaster2[[1]]}] & // Collect2[#, ep, IsolateNames -> isoSymbols2, FCParallelize -> True] &;]
```

$$\{4.52247,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSE$resPreFinal3 = quarkSE$resPreFinal2 // Series[#, {ep, 0, -1}] & // Normal // Series[(I*(4*Pi)^(-2 + ep))^2 #, {ep, 0, -1}] & // Normal;]
```

$$\{1.70851,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSE$resPreFinal4 = Collect2[FRH2[FRH2[quarkSE$resPreFinal3, isoSymbols2], isoSymbols1],DiracGamma, pp, mxt, ep, FCParallelize -> True];]
```

$$\{0.285546,\text{Null}\}$$

```mathematica
isoSymbols3 = FCMakeSymbols[LJ, Range[1, $KernelCount], List];
```

```mathematica
AbsoluteTiming[quarkSE$resPreFinal5 = Series[Total[Collect2[quarkSE$resPreFinal4, mxt, IsolateNames -> isoSymbols3,FCParallelize -> True]], {mxt, 0, 0}] // Normal;]
```

$$\{2.13284,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSE$resPreFinal6 = Collect2[FRH2[quarkSE$resPreFinal5, isoSymbols3] // ReplaceAll[#, Log[m_Symbol^2] :> 2 Log[m]] &, DiracGamma, pp, mxt, ep, FCParallelize -> True];]
```

$$\{0.132363,\text{Null}\}$$

```mathematica
quarkSE$resFinal = Collect2[Collect2[quarkSE$resPreFinal6, ep, CA, CF, mq, Nf, SUNFDelta, as4, DiracGamma, GaugeXi, Factoring -> FullSimplify], ep, mq, mxt]
```

$$\frac{i \;\text{as4}^2 \xi _{\text{G}} \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p \left(-4 C_A C_F \xi _{\text{G}}+3 C_A^2 \xi _{\text{G}}+3 C_A^2+4 C_F^2 \xi _{\text{G}}-3 \xi _{\text{G}}-3\right)}{8 \;\text{ep}^2}-\frac{1}{960 \;\text{ep}}i \;\text{as4}^2 \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p \left(-520 C_A C_F \xi _{\text{G}}^2-320 C_A C_F \xi _{\text{G}}+960 \log (4 \pi ) C_A C_F \xi _{\text{G}}^2-360 C_A C_F+81 C_A^2 \xi _{\text{G}}^3+435 C_A^2 \xi _{\text{G}}^2+127 C_A^2 \xi _{\text{G}}-720 \log (4 \pi ) C_A^2 \xi _{\text{G}}^2-720 \log (4 \pi ) C_A^2 \xi _{\text{G}}-3 C_A^2+432 C_F N_f \xi _{\text{G}}^2+256 C_F N_f \xi _{\text{G}}+432 C_F N_f+240 C_F^2 \xi _{\text{G}}^2+400 C_F^2 \xi _{\text{G}}-960 \log (4 \pi ) C_F^2 \xi _{\text{G}}^2+720 C_F^2-81 \xi _{\text{G}}^3-435 \xi _{\text{G}}^2-127 \xi _{\text{G}}+720 \log (4 \pi ) \xi _{\text{G}}^2+720 \log (4 \pi ) \xi _{\text{G}}+3\right)-\frac{i \;\text{as4}^2 \xi _{\text{G}} \log (\text{mxt}) \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p \left(-4 C_A C_F \xi _{\text{G}}+3 C_A^2 \xi _{\text{G}}+3 C_A^2+4 C_F^2 \xi _{\text{G}}-3 \xi _{\text{G}}-3\right)}{2 \;\text{ep}}$$

### Quark self-energy 1-loop CT

```mathematica
FCClearScalarProducts[];
divDegree = 1;
aux1 = FCLoopGetFeynAmpDenominators[quarkSECT$RawAmp, {k1}, denHead, Momentum -> {p}, "Massless" -> True];
aux2 = FCLoopAddAuxiliaryMass[aux1[[2]], {k1}, -mxt^2, 0, Head -> denHead]
```

$$\left\{\text{denHead}\left(\frac{1}{(\text{k1}^2+i \eta )}\right)\to \frac{1}{(\text{k1}^2-\text{mxt}^2+i \eta )},\text{denHead}\left(\frac{1}{((\text{k1}-p)^2+i \eta )}\right)\to \frac{1}{((\text{k1}-p)^2-\text{mxt}^2+i \eta )},\text{denHead}\left(\frac{1}{((\text{k1}+p)^2+i \eta )}\right)\to \frac{1}{((\text{k1}+p)^2-\text{mxt}^2+i \eta )}\right\}$$

```mathematica
quarkSECT$StrName = StringReplace[ToString[Hold[quarkSECT$Amp]], {"Hold[" -> "", "]" -> ""}]
```

$$\text{quarkSECT\$Amp}$$

```mathematica
AbsoluteTiming[quarkSECT$Amp = (aux1[[1]] /. aux2) // Contract[#, FCParallelize -> True] & // 
      SUNSimplify[#, FCParallelize -> True] & // DiracSimplify[#, FCParallelize -> True] &;]
```

$$\{0.478243,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSECT$Amp1 = Collect2[quarkSECT$Amp, p, IsolateNames -> KK];]
AbsoluteTiming[quarkSECT$Amp2 = FourSeries[quarkSECT$Amp1, {p, 0, divDegree}, FCParallelize -> True];]
AbsoluteTiming[quarkSECT$Amp3 = Collect2[FRH[quarkSECT$Amp2], FeynAmpDenominator, FCParallelize -> True];]
```

$$\{0.131969,\text{Null}\}$$

$$\{0.071087,\text{Null}\}$$

$$\{0.135756,\text{Null}\}$$

The rest of the calculation follows the standard multiloop template

```mathematica
FCClearScalarProducts[];
SPD[p] = pp;
```

```mathematica
{quarkSECT$Amp4, quarkSECT$Topos} = FCLoopFindTopologies[quarkSECT$Amp3, {k1}, FCParallelize -> True, 
    FCLoopBasisOverdeterminedQ -> True, FinalSubstitutions -> {Hold[SPD][p] -> pp}, Names -> quarkSEtopo];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }1$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }1$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of found topologies: }1$$

```mathematica
AbsoluteTiming[quarkSECT$Amp5 = FCLoopTensorReduce[quarkSECT$Amp4, quarkSECT$Topos, FCParallelize -> True];]
```

$$\{0.630146,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSECT$Amp6 = DiracSimplify[quarkSECT$Amp5, FCParallelize -> True];]
```

$$\{0.154509,\text{Null}\}$$

```mathematica
{quarkSECT$Amp7, quarkSECT$Topos2} = FCLoopRewriteOverdeterminedTopologies[quarkSECT$Amp6, quarkSECT$Topos, FCParallelize -> True];
```

$$\text{FCLoopRewriteIncompleteTopologies: }\;\text{No overdetermined topologies detected.}$$

```mathematica
{quarkSECT$Amp8, quarkSECT$Topos3} = FCLoopRewriteIncompleteTopologies[quarkSECT$Amp7, quarkSECT$Topos2, FCParallelize -> True];
```

$$\text{FCLoopRewriteIncompleteTopologies: }\;\text{No incomplete topologies detected.}$$

```mathematica
AbsoluteTiming[quarkSECT$SubTopos = FCLoopFindSubtopologies[quarkSECT$Topos2, Flatten -> True, Remove -> True, FCParallelize -> True];]
```

$$\{0.047428,\text{Null}\}$$

```mathematica
AbsoluteTiming[{quarkSECT$TopoMappings, quarkSECT$FinalTopos} = FCLoopFindTopologyMappings[quarkSECT$Topos2, PreferredTopologies -> quarkSECT$SubTopos, FCParallelize -> True];]
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }0\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

$$\{0.054842,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSECT$AmpGLI = FCLoopApplyTopologyMappings[quarkSECT$Amp8, {quarkSECT$TopoMappings, quarkSECT$FinalTopos}, FCParallelize -> True];]
```

$$\{0.171788,\text{Null}\}$$

```mathematica
quarkSECT$GLIs = Cases2[quarkSECT$AmpGLI, GLI];
```

```mathematica
quarkSECT$dir = FileNameJoin[{$TemporaryDirectory, "Reduction-" <> quarkSECT$StrName <> "-1L-massive"}];
Quiet[CreateDirectory[quarkSECT$dir]];
```

```mathematica
KiraCreateJobFile[quarkSECT$FinalTopos, quarkSECT$GLIs, quarkSECT$dir]
```

$$\{\text{/tmp/Reduction-quarkSECT\$Amp-1L-massive/quarkSEtopo1/job.yaml}\}$$

```mathematica
KiraCreateIntegralFile[quarkSECT$GLIs, quarkSECT$FinalTopos, quarkSECT$dir]
KiraCreateConfigFiles[quarkSECT$FinalTopos, quarkSECT$GLIs, quarkSECT$dir, 
  KiraMassDimensions -> {pp -> 2, mq -> 1, mxt -> 1}]
```

$$\text{KiraCreateIntegralFile: Number of loop integrals: }5$$

$$\{\text{/tmp/Reduction-quarkSECT\$Amp-1L-massive/quarkSEtopo1/KiraLoopIntegrals}\}$$

$$\left(
\begin{array}{cc}
 \;\text{/tmp/Reduction-quarkSECT\$Amp-1L-massive/quarkSEtopo1/config/integralfamilies.yaml} & \;\text{/tmp/Reduction-quarkSECT\$Amp-1L-massive/quarkSEtopo1/config/kinematics.yaml} \\
\end{array}
\right)$$

```mathematica
KiraRunReduction[quarkSECT$dir, quarkSECT$FinalTopos, 
  KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}], 
  KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]
```

$$\{\text{True}\}$$

```mathematica
quarkSECT$ReductionTables = KiraImportResults[quarkSECT$FinalTopos, quarkSECT$dir] // Flatten;
```

```mathematica
quarkSECT$resPreFinal1 = Collect2[Total[quarkSECT$AmpGLI /. Dispatch[quarkSECT$ReductionTables]], GLI, 
    GaugeXi, D, DiracGamma, FCParallelize -> True];
```

```mathematica
quarkSECT$masters = Cases2[quarkSECT$resPreFinal1, GLI];
```

```mathematica
quarkSECT$MIMappings = FCLoopFindIntegralMappings[quarkSECT$masters, Join[tadpoleMaster1[[2]], 
    quarkSECT$FinalTopos], PreferredIntegrals -> {tadpoleMaster1[[1]][[1]]}]
```

$$\left(
\begin{array}{c}
 G^{\text{quarkSEtopo1}}(1)\to G^{\text{tad1LxFx1x1xxEp999x}}(1) \\
 G^{\text{tad1LxFx1x1xxEp999x}}(1) \\
\end{array}
\right)$$

Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization
we need to multiply by I*(4 Pi)^(ep-2)

At this point we need to insert the 1-loop renormalization constants

```mathematica
knownResults1L = {
    rc[delZA, 1] -> (13*CA - 4*Nf - 3*CA*GaugeXi["G"])/(6*ep), 
    rc[delZAmxt, 1] -> -1/8*(CA + 8*Nf + 3*CA*GaugeXi["G"])/ep, 
    rc[delZxi, 1] -> (13*CA - 4*Nf - 3*CA*GaugeXi["G"])/(6*ep), 
    rc[delZpsi, 1] -> -((CF*GaugeXi["G"])/ep), 
    rc[delZumxt, 1] -> 0, rc[delZu, 1] -> (CA*(3 - GaugeXi["G"]))/(4*ep), 
    rc[delZg, 1] -> -1/6*(11*CA - 2*Nf)/ep};
```

```mathematica
AbsoluteTiming[quarkSECT$resPreFinal2 = Collect2[quarkSECT$resPreFinal1, D, GLI, IsolateNames -> KK] // FCReplaceD[#, D -> 4 - 2 ep] & // 
            ReplaceAll[#, quarkSECT$MIMappings[[1]]] & // ReplaceAll[#, {tadpoleMaster1[[1]], tadpoleMaster2[[1]]}] & // 
          Collect2[#, ep, IsolateNames -> KK2] & // Series[(I*(4*Pi)^(-2 + ep)) #, {ep, 0, 1}] & // Normal // FCLoopAddMissingHigherOrdersWarning[#, ep, epHelp] & // FRH // 
     ReplaceAll[#, {Log[mxt^2] -> 2 Log[mxt]}] &;]
```

$$\{4.08231,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSECT$resPreFinal2 = Collect2[quarkSECT$resPreFinal1, Join[{as4}, List @@ renConstants],IsolateNames -> KK] // ReplaceAll[#, Zxi -> ZA] & // ReplaceAll[#, {
         	(h : renConstants) :> 1 + (as4 rc[ToExpression["del" <> ToString[h]], 1] + as4^2 rc[ToExpression["del" <> ToString[h]], 2])}] & // Series[#, {as4, 0, 2}] & // Normal;]
```

$$\{0.138242,\text{Null}\}$$

```mathematica
AbsoluteTiming[quarkSECT$resPreFinal3 = Collect2[quarkSECT$resPreFinal2 // FRH, {rc, D, GLI}, IsolateNames -> KK] // FCReplaceD[#, {D -> 4 - 2 ep}] & // ReplaceRepeated[#, knownResults1L] & // 
        ReplaceAll[#, quarkSECT$MIMappings[[1]]] & // ReplaceAll[#, {tadpoleMaster1[[1]], tadpoleMaster2[[1]]}] & // If[! FreeQ[#, GLI], Abort[], #] & // Collect2[#, ep, IsolateNames -> KK] &;]
```

$$\{0.123596,\text{Null}\}$$

```mathematica
quarkSECT$resFinal = quarkSECT$resPreFinal3 // Series[(I*(4*Pi)^(-2 + ep)) #, {ep, 0, -1}] & // Normal // FRH // 
        Collect2[#, mxt, IsolateNames -> KK] & // Series[#, {mxt, 0, 0}] & // Normal // FRH // ReplaceAll[#, Log[m_^2] :> 2 Log[m]] & // Collect2[#, ep, mxt] &
```

$$-\frac{i \;\text{as4}^2 C_F \xi _{\text{G}} \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p \left(C_A \xi _{\text{G}}+3 C_A+2 C_F \xi _{\text{G}}\right)}{2 \;\text{ep}^2}+\frac{1}{480 \;\text{ep}}i \;\text{as4}^2 C_F \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p \left(81 C_A \xi _{\text{G}}^3+235 C_A \xi _{\text{G}}^2+447 C_A \xi _{\text{G}}-240 \log (4 \pi ) C_A \xi _{\text{G}}^2-720 \log (4 \pi ) C_A \xi _{\text{G}}+1317 C_A+120 C_F \xi _{\text{G}}^2+200 C_F \xi _{\text{G}}-480 \log (4 \pi ) C_F \xi _{\text{G}}^2+216 N_f \xi _{\text{G}}^2+128 N_f \xi _{\text{G}}-24 N_f\right)+\frac{i \;\text{as4}^2 C_F \xi _{\text{G}} \log (\text{mxt}) \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p \left(C_A \xi _{\text{G}}+3 C_A+2 C_F \xi _{\text{G}}\right)}{\text{ep}}$$

### Determination of renormalization constants

```mathematica
diagQuarkTreeSECT$Amp = (Total[diagQuarkTreeSECT$RawAmp]) // ReplaceRepeated[#, {
        	(h : renConstants) :> 1 + (as4 rc[ToExpression["del" <> ToString[h]], 1] + as4^2 rc[ToExpression["del" <> ToString[h]], 2])}] & // 
    	Series[#, {as4, 0, 2}] & // Normal // ReplaceRepeated[#, knownResults1L] &
```

$$i \;\text{as4}^2 \;\text{rc}(\text{delZpsi},2) \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p-\frac{i \;\text{as4} C_F \xi _{\text{G}} \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p}{\text{ep}}$$

```mathematica
quarkSE$RenConstants2L = Collect2[Coefficient[SUNSimplify[quarkSE$resFinal + quarkSECT$resFinal + diagQuarkTreeSECT$Amp, SUNNToCACF -> False], as4, 2], as4, mxt, DiracGamma, Factoring -> FullSimplify] // 
   	FCMatchSolve[#, {ep, CF, DiracGamma, mq, mxt, SUNDelta, SUNTF, SUNFDelta, CA, GaugeXi, as4, Pair, pp, Nf, SUNN}] & // Collect2[#, ep] &
```

$$\text{FCMatchSolve: Solving for: }\{\text{rc}(\text{delZpsi},2)\}$$

$$\text{FCMatchSolve: A solution exists.}$$

$$\left\{\text{rc}(\text{delZpsi},2)\to \frac{(N-1) (N+1) \xi _{\text{G}} \left(-\xi _{\text{G}}+2 N^2 \xi _{\text{G}}+3 N^2\right)}{8 \;\text{ep}^2 N^2}-\frac{(N-1) (N+1) \left(-4 N N_f+N^2 \xi _{\text{G}}^2+8 N^2 \xi _{\text{G}}+22 N^2+3\right)}{16 \;\text{ep} N^2}\right\}$$

## Check the final results

Our final QCD 2-loop wave-function renormalization constants

```mathematica
finalResults = Thread[Rule[List @@ renConstants, 
      (List @@ renConstants /. (h : renConstants) :> 1 + as4 rc[ToExpression["del" <> ToString[h]], 1] + as4^2 rc[ToExpression["del" <> ToString[h]], 2]) // 
       ReplaceAll[#, Join[SUNSimplify[knownResults1L, SUNNToCACF -> False], quarkSE$RenConstants2L]] &]] // SelectNotFree[#, Zpsi] &;
```

```mathematica
knownResult = {
    rc[delZpsi, 2] -> ((-1 + SUNN)*(1 + SUNN)*GaugeXi["G"]*(3*SUNN^2 - GaugeXi["G"] + 2*SUNN^2*GaugeXi["G"]))/(8*ep^2*SUNN^2) - 
        ((-1 + SUNN)*(1 + SUNN)*(3 - 4*Nf*SUNN + 22*SUNN^2 + 8*SUNN^2*GaugeXi["G"] + SUNN^2*GaugeXi["G"]^2))/(16*ep*SUNN^2)};
```

```mathematica
FCCompareResults[quarkSE$RenConstants2L, knownResult, 
  Text -> {"\tCompare to Chetyrkin, Four-loop renormalization of QCD: full set of renormalization constants and anomalous dimensions, arXiv:hep-ph/0405193:", 
    "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```mathematica

$$\text{$\backslash $tCompare to Chetyrkin, Four-loop renormalization of QCD: full set of renormalization constants and anomalous dimensions, arXiv:hep-ph/0405193:} \;\text{CORRECT.}$$

$$\text{True}$$

$$\text{$\backslash $tCPU Time used: }95.845\text{ s.}$$