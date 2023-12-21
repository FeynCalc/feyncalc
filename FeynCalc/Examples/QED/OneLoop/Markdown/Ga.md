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
$LoadAddOns = {"FeynArts"};
<< FeynCalc`
$FAVerbose = 0; 
 
FCCheckVersion[9, 3, 1];
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
MakeBoxes[mu, TraditionalForm] := "\[Mu]";
```

```mathematica
diags = InsertFields[CreateTopologies[1, 1 -> 0], 
    		{V[1]} -> {}, InsertionLevel -> {Particles}, 
    		ExcludeParticles -> {S[_], V[_], U[_], F[3 | 4], F[2, {2 | 3}]}]; 
 
Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![0a1m2kqb65f42](img/0a1m2kqb65f42.svg)

## Obtain the amplitude

The 1/(2Pi)^D prefactor is implicit.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, PreFactor -> 1, 
   	Truncated -> True], IncomingMomenta -> {k}, 
  	LorentzIndexNames -> {mu}, LoopMomenta -> {q}, 
  	UndoChiralSplittings -> True, ChangeDimension -> D, 
  	List -> False, SMP -> True]
```

$$-\frac{i \;\text{tr}\left(\left(m_e-\gamma \cdot q\right).\left(-i \;\text{e} \gamma ^{\mu }\right)\right)}{q^2-m_e^2}$$

## Calculate the amplitude

Having performed the Dirac algebra we clearly see that this diagram must 
vanish because the loop integral is antisymmetric under q^mu -> - q^mu.

```mathematica
amp[1] = DiracSimplify[amp[0]]
```

$$\frac{4 \;\text{e} q^{\mu }}{q^2-m_e^2}$$

TID can recognize this and we obtain zero

```mathematica
amp[2] = TID[amp[1], q]
```

$$0$$

## Check the final results

```mathematica
FCCompareResults[amp[2], 0, 
   Text -> {"\tVerify Furry's theorem for 1-photon at 1-loop:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tVerify Furry's theorem for 1-photon at 1-loop:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }19.702\text{ s.}$$