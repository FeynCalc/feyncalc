---
title: Photon tadpole in QED
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Ga, QED, amplitude, 1-loop";
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
FCAttachTypesettingRule[mu, "\[Mu]"]
```

```mathematica
diags = InsertFields[CreateTopologies[1, 1 -> 0], 
    		{V[1]} -> {}, InsertionLevel -> {Particles}, 
    		ExcludeParticles -> {S[_], V[_], U[_], F[3 | 4], F[2, {2 | 3}]}]; 
 
Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![1ge1adt8fibyj](img/1ge1adt8fibyj.svg)

## Obtain the amplitude

The 1/(2Pi)^D prefactor is implicit.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, PreFactor -> 1, 
   	Truncated -> True], IncomingMomenta -> {k}, 
  	LorentzIndexNames -> {mu}, LoopMomenta -> {q}, 
  	UndoChiralSplittings -> True, ChangeDimension -> D, 
  	SMP -> True, FinalSubstitutions -> {SMP["m_e"] -> me}]
```

$$\left\{-\frac{i \;\text{tr}\left((\text{me}-\gamma \cdot q).\left(-i \;\text{e} \gamma ^{\mu }\right)\right)}{q^2-\text{me}^2}\right\}$$

## Evaluate the amplitudes

Having performed the Dirac algebra we clearly see that this diagram must 
vanish because the loop integral is antisymmetric under q^mu -> - q^mu.

```mathematica
amp[1] = DiracSimplify[amp[0], FCParallelize -> True]
```

$$\left\{\frac{4 \;\text{e} q^{\mu }}{q^2-\text{me}^2}\right\}$$

## Identify and minimize the topologies

```mathematica
{amp[2], topos} = FCLoopFindTopologies[amp[1], {q}, FCParallelize -> True];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }1$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }1$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

```mathematica
mappings = FCLoopFindTopologyMappings[topos, FCParallelize -> True];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }0\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

## Rewrite the amplitude in terms of GLIs

```mathematica
AbsoluteTiming[ampReduced = FCLoopTensorReduce[amp[2], topos, FCParallelize -> True];]
```

$$\{0.092708,\text{Null}\}$$

The amplitude vanishes after the tensor reduction

```mathematica
ampReduced
```

$$\{0\}$$

## Check the final results

```mathematica
FCCompareResults[Total[ampReduced], 0, 
   Text -> {"\tVerify Furry's theorem for 1-photon at 1-loop:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```mathematica

$$\text{$\backslash $tVerify Furry's theorem for 1-photon at 1-loop:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }23.119\text{ s.}$$