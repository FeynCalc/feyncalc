---
title: QED vacuum polarization
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Ga -> Ga, QED, only UV divergences, 1-loop";
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

## Configure some options

We keep scaleless B0 functions, since otherwise the UV part would not come out right.

```mathematica
$KeepLogDivergentScalelessIntegrals = True;
```

## Generate Feynman diagrams

Nicer typesetting

```mathematica
MakeBoxes[mu, TraditionalForm] := "\[Mu]";
MakeBoxes[nu, TraditionalForm] := "\[Nu]";
```

```mathematica
diags = InsertFields[CreateTopologies[1, 1 -> 1], {V[1]} -> 
     		{V[1]}, InsertionLevel -> {Particles}, 
    		ExcludeParticles -> {S[_], V[2 | 3], (S | U)[_], F[3 | 4], F[2, {2 | 3}]}]; 
 
Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![0133bojpmpiil](img/0133bojpmpiil.svg)

## Obtain the amplitude

The 1/(2Pi)^D prefactor is implicit.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, PreFactor -> 1], 
  	IncomingMomenta -> {p}, OutgoingMomenta -> {p}, LoopMomenta -> {q},
  	LorentzIndexNames -> {mu, nu}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> False, SMP -> True, 
  	Contract -> True]

```mathematica

$$\frac{\text{tr}\left(\left(m_e-\gamma \cdot q\right).\left(i \;\text{e} \gamma ^{\nu }\right).\left(m_e+\gamma \cdot (p-q)\right).\left(i \;\text{e} \gamma ^{\mu }\right)\right)}{\left(q^2-m_e^2\right).\left((q-p)^2-m_e^2\right)}$$

## Calculate the amplitude

```mathematica
amp[1] = TID[amp[0], q, ToPaVe -> True]
```

$$\frac{2 i \pi ^2 \;\text{e}^2 \;\text{B}_0\left(p^2,m_e^2,m_e^2\right) \left(-\left((1-D) p^4 g^{\mu \nu }\right)+2 (1-D) p^2 p^{\mu } p^{\nu }+D p^2 p^{\mu } p^{\nu }+4 p^2 m_e^2 g^{\mu \nu }-4 m_e^2 p^{\mu } p^{\nu }-p^4 g^{\mu \nu }\right)}{(1-D) p^2}-\frac{4 i \pi ^2 \;\text{e}^2 \;\text{A}_0\left(m_e^2\right) \left(-(1-D) p^2 g^{\mu \nu }-D p^{\mu } p^{\nu }-p^2 g^{\mu \nu }+2 p^{\mu } p^{\nu }\right)}{(1-D) p^2}$$

Check the gauge invariance

```mathematica
tmp = Contract[FVD[p, mu] FVD[p, nu] amp[1]] // Factor
FCCompareResults[tmp, 0, 
   Text -> {"\tThe photon self-energy is gauge invariant:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
```

$$0$$

$$\text{$\backslash $tThe photon self-energy is gauge invariant:} \;\text{CORRECT.}$$

The UV divergence of the amplitude can be obtained via PaVeUVPart.
Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.
Hint: If you need the full result for the amplitude, use PaXEvaluate from FeynHelpers.

```mathematica
ampDiv[0] = PaVeUVPart[amp[1], Prefactor -> 1/(2 Pi)^D] // 
       FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
    SelectNotFree2[#, Epsilon] & // Simplify
```

$$\frac{i \;\text{e}^2 \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)}{12 \pi ^2 \varepsilon }$$

The self-energy amplitude is usually defined as  (p^2 g^{mu nu} - p^mu p^nu) i Pi(p^2)

```mathematica
pi[0] = FCI[ampDiv[0]/(I (SPD[p, p] MTD[mu, nu] - FVD[p, mu] FVD[p, nu]))] // Cancel
```

$$-\frac{\text{e}^2}{12 \pi ^2 \varepsilon }$$

## Check the final results

Keep in mind that Peskin and Schroeder use D = 4-Epsilon,
while we did the calculation with D = 4-2Epsilon.

```mathematica
knownResult = -SMP["e"]^2/(4 Pi)^(D/2) Gamma[2 - D/2]/
          	(SMP["m_e"]^2 - x (1 - x) SPD[p, p])^(2 - D/2)*(8 x (1 - x)) // 
        	FCReplaceD[#, D -> 4 - Epsilon] & // Series[#, {Epsilon, 0, 0}] & // 
      	Normal // SelectNotFree2[#, Epsilon] & // Integrate[#, {x, 0, 1}] & // 
   	ReplaceAll[#, 1/Epsilon -> 1/(2 Epsilon)] &;
FCCompareResults[pi[0], knownResult, 
   Text -> {"\tCompare to Peskin and Schroeder, An Introduction to QFT, Eq 10.44:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Peskin and Schroeder, An Introduction to QFT, Eq 10.44:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }19.064\text{ s.}$$