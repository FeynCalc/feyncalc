---
title: 3-photon interaction in QED
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Ga -> Ga Ga, QED, amplitude, 1-loop";
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
FCAttachTypesettingRule[k1, {SubscriptBox, "k", "1"}];
FCAttachTypesettingRule[k2, {SubscriptBox, "k", "2"}];
FCAttachTypesettingRule[k3, {SubscriptBox, "k", "3"}];
```

```mathematica
diags = InsertFields[CreateTopologies[1, 1 -> 2], 
    		{V[1]} -> {V[1], V[1]}, InsertionLevel -> {Particles}, 
    		ExcludeParticles -> {S[_], V[_], U[_], F[3 | 4], F[2, {2 | 3}]}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![09psr9ceodpz8](img/09psr9ceodpz8.svg)

## Obtain the amplitude

The 1/(2Pi)^D prefactor is implicit.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, PreFactor -> 1, 
   	Truncated -> True], IncomingMomenta -> {k1}, 
  	OutgoingMomenta -> {k2, k3}, LoopMomenta -> {q}, 
  	LorentzIndexNames -> {mu, nu, rho}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, SMP -> True]
```

$$\left\{\frac{i \;\text{tr}\left(\left(m_e+\gamma \cdot \left(-k_2-k_3+q\right)\right).\left(-i \;\text{e} \gamma ^{\rho }\right).\left(m_e+\gamma \cdot \left(q-k_2\right)\right).\left(-i \;\text{e} \gamma ^{\nu }\right).\left(m_e+\gamma \cdot q\right).\left(-i \;\text{e} \gamma ^{\mu }\right)\right)}{\left(q^2-m_e^2\right).\left((q-k_2){}^2-m_e^2\right).\left((-k_2-k_3+q){}^2-m_e^2\right)},\frac{i \;\text{tr}\left(\left(m_e+\gamma \cdot \left(-k_2-k_3+q\right)\right).\left(i \;\text{e} \gamma ^{\rho }\right).\left(m_e+\gamma \cdot \left(q-k_2\right)\right).\left(i \;\text{e} \gamma ^{\nu }\right).\left(m_e+\gamma \cdot q\right).\left(i \;\text{e} \gamma ^{\mu }\right)\right)}{\left(q^2-m_e^2\right).\left((q-k_2){}^2-m_e^2\right).\left((-k_2-k_3+q){}^2-m_e^2\right)}\right\}$$

## Evaluate the amplitudes

Having performed the Dirac algebra we clearly see that this diagram must 
vanish because the loop integral is antisymmetric under q^mu -> - q^mu.

```mathematica
amp[1] = DiracSimplify[amp[0], FCParallelize -> True];
```

## Identify and minimize the topologies

```mathematica
{amp[2], topos} = FCLoopFindTopologies[amp[1], {q}, FCParallelize -> True];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }1$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }1$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

$$\text{FCLoopFindTopologies: }\;\text{Your topologies depend on the follwing kinematic invariants that are not all entirely lowercase: }\{\text{Pair[Momentum[k2, D], Momentum[k2, D]]},\text{Pair[Momentum[k2, D], Momentum[k3, D]]},\text{Pair[Momentum[k3, D], Momentum[k3, D]]},\text{SMP[m$\_$e]}\}$$

$$\text{FCLoopFindTopologies: }\;\text{This may lead to issues if these topologies are meant to be processed using tools such as FIRE, KIRA or Fermat.}$$

```mathematica
mappings = FCLoopFindTopologyMappings[topos, FCParallelize -> True];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }0\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

## Rewrite the amplitude in terms of GLIs

```mathematica
AbsoluteTiming[ampReduced = FCLoopTensorReduce[amp[2], topos, FCParallelize -> True];]
```

$$\{0.512657,\text{Null}\}$$

The sum vanishes because the contribution of the first diagram cancels the contribution of the second diagram.

```mathematica
res = Collect2[ampReduced[[1]][[1]] + ampReduced[[2]][[1]], Pair]
```

$$0$$

## Check the final results

```mathematica
FCCompareResults[res, 0, 
   Text -> {"\tVerify Furry's theorem for 3-photons at 1-loop:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```mathematica

$$\text{$\backslash $tVerify Furry's theorem for 3-photons at 1-loop:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }19.259\text{ s.}$$