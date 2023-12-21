---
title: QCD ghost self-energy
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Gh -> Gh, QCD, only UV divergences, 1-loop";
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

```mathematica
diags = InsertFields[CreateTopologies[1, 1 -> 1, ExcludeTopologies -> {Tadpoles}], 
    		{U[5]} -> {U[5]}, InsertionLevel -> {Particles}, Model -> "SMQCD"]; 
 
Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![1123tdcz6igxy](img/1123tdcz6igxy.svg)

## Obtain the amplitude

The 1/(2Pi)^D prefactor is implicit.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, GaugeRules -> {}, 
   	PreFactor -> 1], IncomingMomenta -> {p}, OutgoingMomenta -> {p}, LoopMomenta -> {q}, 
  	UndoChiralSplittings -> True, ChangeDimension -> D, List -> False, SMP -> True, 
  	DropSumOver -> True, Contract -> True]
```

$$-\left(g_s^2 f^{\text{Glu1}\;\text{Glu3}\;\text{Glu4}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu4}} \left(\frac{\left(1-\xi _g\right) \left(p^2-p\cdot q\right) \left(q^2-p\cdot q\right)}{q^2.(q-p)^4}+\frac{p\cdot q}{q^2.(q-p)^2}\right)\right)$$

## Calculate the amplitude

```mathematica
amp[1] = amp[0] // SUNSimplify // TID[#, q, ToPaVe -> True] &
```

$$-\frac{1}{4} i \pi ^2 p^2 C_A \left(1-\xi _g\right) g_s^2 \;\text{B}_0(0,0,0) \delta ^{\text{Glu1}\;\text{Glu2}}-\frac{1}{2} i \pi ^2 p^2 C_A g_s^2 \delta ^{\text{Glu1}\;\text{Glu2}} \;\text{B}_0\left(p^2,0,0\right)+\frac{1}{4} i \pi ^2 p^4 C_A \left(1-\xi _g\right) g_s^2 \delta ^{\text{Glu1}\;\text{Glu2}} \;\text{C}_0\left(0,p^2,p^2,0,0,0\right)$$

The UV divergence of the amplitude can be obtained via PaVeUVPart.
Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.
Hint: If you need the full result for the amplitude, use PaXEvaluate from FeynHelpers.

```mathematica
ampDiv[0] = PaVeUVPart[amp[1], Prefactor -> 1/(2 Pi)^D] // 
       FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
    SelectNotFree2[#, Epsilon] & // Simplify
```

$$\frac{i p^2 C_A \left(\xi _g-3\right) g_s^2 \delta ^{\text{Glu1}\;\text{Glu2}}}{64 \pi ^2 \varepsilon }$$

The self-energy amplitude is usually defined as  (p^2 delta^ab  Pi(p^2)

```mathematica
pi[0] = FCI[ampDiv[0]/(I SUNDelta[SUNIndex[Glu1], SUNIndex[Glu2]]*SPD[p, p])] //Cancel
```

$$\frac{C_A \left(\xi _g-3\right) g_s^2}{64 \pi ^2 \varepsilon }$$

## Check the final results

```mathematica
knownResult = -SMP["g_s"]^2/(4 Pi)^2 CA (3 - GaugeXi[g])/4*1/Epsilon;
FCCompareResults[pi[0], knownResult, 
   Text -> {"\tCompare to Muta, Foundations of QCD, Eq. 2.5.136:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Muta, Foundations of QCD, Eq. 2.5.136:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }17.793\text{ s.}$$