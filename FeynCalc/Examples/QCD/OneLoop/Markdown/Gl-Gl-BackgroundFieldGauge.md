---
title: Pure Yang-Mills 1-loop gluon self-energy in the background field formalism
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Gl -> Gl, YM+BGF, only UV divergences, 1-loop";
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

## Configure some options

We keep scaleless B0 functions, since otherwise the UV part would not come out right.

```mathematica
$KeepLogDivergentScalelessIntegrals = True;
```

```mathematica
FAPatch[PatchModelsOnly -> True];

(*Successfully patched FeynArts.*)
```

## Generate Feynman diagrams

Nicer typesetting

```mathematica
FCAttachTypesettingRule[mu, "\[Mu]"];
FCAttachTypesettingRule[nu, "\[Nu]"];
```

```mathematica
diags = InsertFields[CreateTopologies[1, 1 -> 1, ExcludeTopologies -> {Tadpoles}], 
    	{V[50, {a}]} -> {V[50, {b}]}, InsertionLevel -> {Classes}, 
    	Model -> FileNameJoin[{"QCDBGF", "QCDBGF"}], 
    	GenericModel -> FileNameJoin[{"QCDBGF", "QCDBGF"}], 
    	ExcludeParticles -> {F[_]}]; 
 
Paint[diags, ColumnsXRows -> {2, 2}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 512}];
```

![0921g3y0d2lml](img/0921g3y0d2lml.svg)

## Obtain corresponding amplitudes

The 1/(2Pi)^D prefactor is implicit.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, GaugeRules -> {}, 
    	PreFactor -> 1], IncomingMomenta -> {p}, OutgoingMomenta -> {p}, LoopMomenta -> {l}, 
   	LorentzIndexNames -> {mu, nu}, UndoChiralSplittings -> True, 
   	ChangeDimension -> D, List -> True, SMP -> True, DropSumOver -> True, 
   	FinalSubstitutions -> {SMP["m_u"] -> SMP["m_q"], 
     	GaugeXi[V[5, {_}]] :> GaugeXi[G]}];
```

```mathematica
amp[1] = DiracSimplify[#, FCParallelize -> True] & /@ amp[0];
```

```mathematica
amp[2] = amp[1] // SUNSimplify[#, FCParallelize -> True] & // 
    TID[#, l, ToPaVe -> True, FCParallelize -> True] &;
```

$$\text{TID}\;\text{::}\;\text{zerogram} :\text{  }"Warning! Following sets of external momenta are linearly dependent \!\(\*FormBox[\"\\\"{{-p, -p}}\\\"\", TraditionalForm]\). To avoid singularities due to zero Gram determinants, TID will automatically switch to the reduction in terms of Passarino-Veltman coefficient functions. If you want to have the reduction to scalar integrals, please find a set of linearly independent momenta using FCLoopFindTensorBasis and supply it to the function via the option TensorReductionBasisChange."$$

Discard all the finite pieces of the 1-loop amplitude.

```mathematica
ampDiv[0] = PaVeUVPart[#, Prefactor -> 1/(2 Pi)^D] & /@ amp[2]
```

$$\left\{0,0,\frac{i 2^{1-D} \pi ^{2-D} C_A g_s^2 \delta ^{ab} \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)}{(D-4) (D-1)},\frac{5 i (2 \pi )^{2-D} C_A g_s^2 \delta ^{ab} \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)}{(D-4) (D-1)}\right\}$$

```mathematica
ampDiv[1] = FCReplaceD[ampDiv[0], D -> 4 - 2 Epsilon] // 
    	Series[#, {Epsilon, 0, -1}] & // Normal // Simplify
```

$$\left\{0,0,-\frac{i C_A g_s^2 \delta ^{ab} \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)}{48 \pi ^2 \varepsilon },-\frac{5 i C_A g_s^2 \delta ^{ab} \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)}{24 \pi ^2 \varepsilon }\right\}$$

## Check the final results

```mathematica
knownResult = {
    	0, 
    	0, 
    	I CA SMP["g_s"]^2 SUNDelta[a, b]/(4 Pi)^2 (1/(3 Epsilon))*
     		(MTD[mu, nu] SPD[p] - FVD[p, mu] FVD[p, nu]), 
    	I CA SMP["g_s"]^2 SUNDelta[a, b]/(4 Pi)^2 (10/(3 Epsilon))*
     		(MTD[mu, nu] SPD[p] - FVD[p, mu] FVD[p, nu]) 
    } // FCI;
FCCompareResults[ampDiv[1] /. GaugeXi[G] -> 1, knownResult, 
   Text -> {"\tCompare to Abbott, Nucl. Phys. B 185 (1981) 189-203, Eqs 5.11-5.12:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```mathematica

$$\text{$\backslash $tCompare to Abbott, Nucl. Phys. B 185 (1981) 189-203, Eqs 5.11-5.12:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }27.546\text{ s.}$$