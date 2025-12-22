---
title: 5-photon interaction in QED
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Ga -> Ga Ga Ga Ga, QED, amplitude, 1-loop";
If[ $FrontEnd === Null, 
  	$FeynCalcStartupMessages = False; 
  	Print[description]; 
  ];
If[ $Notebooks === False, 
  	$FeynCalcStartupMessages = False 
  ];
LaunchKernels[4];
$LoadAddOns = {"FeynArts"};
<< FeynCalc`
$FAVerbose = 0;
$ParallelizeFeynCalc = True;
FCCheckVersion[10, 2, 0];
```

$$\text{FeynCalc }\;\text{10.2.0 (dev version, 2025-12-22 21:09:03 +01:00, fcd53f9b). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{ The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.12 (27 Mar 2025) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

## Generate Feynman diagrams

Nicer typesetting

```mathematica
FCAttachTypesettingRule[mu, "\[Mu]"];
FCAttachTypesettingRule[nu, "\[Nu]"];
FCAttachTypesettingRule[rho, "\[Rho]"];
FCAttachTypesettingRule[si, "\[Sigma]"];
FCAttachTypesettingRule[tau, "\[Tau]"];
FCAttachTypesettingRule[k1, {SubscriptBox, "k", "1"}];
FCAttachTypesettingRule[k2, {SubscriptBox, "k", "2"}];
FCAttachTypesettingRule[k3, {SubscriptBox, "k", "3"}];
FCAttachTypesettingRule[k4, {SubscriptBox, "k", "4"}];
FCAttachTypesettingRule[k5, {SubscriptBox, "k", "5"}];
```

```mathematica
diags = InsertFields[CreateTopologies[1, 1 -> 4], 
    		{V[1]} -> {V[1], V[1], V[1], V[1]}, InsertionLevel -> {Particles}, 
    		ExcludeParticles -> {S[_], V[_], U[_], F[3 | 4], F[2, {2 | 3}]}]; 
 
Paint[diags, ColumnsXRows -> {4, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![0s3ml8wqyh1s4](img/0s3ml8wqyh1s4.svg)

![1qenygmi8elzl](img/1qenygmi8elzl.svg)

![1rcnqj7zphhsu](img/1rcnqj7zphhsu.svg)

![0cbaxodayj44w](img/0cbaxodayj44w.svg)

![12judgvhx4vrp](img/12judgvhx4vrp.svg)

![03y9etuiae0hf](img/03y9etuiae0hf.svg)

## Obtain the amplitude

The 1/(2Pi)^D prefactor is implicit.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, PreFactor -> 1, 
    	Truncated -> True], IncomingMomenta -> {k1}, 
   	OutgoingMomenta -> {k2, k3, k4, k5}, LoopMomenta -> {q}, 
   	LorentzIndexNames -> {mu, nu, rho}, UndoChiralSplittings -> True, 
   	ChangeDimension -> D, SMP -> True];
```

## Evaluate the amplitudes

We obtain 24 diagrams. The sum vanishes because the contribution of each odd diagram is exactly cancelled by the contribution of the next even diagram, i.e. A1+A2=0, A3+A4=0 and so on

```mathematica
amp[1] = FCTraceFactor[amp[0], FCParallelize -> True];
```

```mathematica
res = amp[1] // Total
```

$$0$$

## Check the final results

```mathematica
FCCompareResults[res, 0, 
   Text -> {"\tVerify Furry's theorem for 5-photons at 1-loop:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```mathematica

$$\text{$\backslash $tVerify Furry's theorem for 5-photons at 1-loop:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }34.486\text{ s.}$$