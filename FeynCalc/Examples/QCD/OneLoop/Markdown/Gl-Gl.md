---
title: QCD vacuum polarization
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Gl -> Gl, QCD, only UV divergences, 1-loop";
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
```

$$\text{FeynCalc }\;\text{10.2.0 (dev version, 2025-12-22 21:09:03 +01:00, fcd53f9b). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{ The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.12 (27 Mar 2025) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

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
    		{V[5]} -> {V[5]}, InsertionLevel -> {Particles}, Model -> "SMQCD", 
    		ExcludeParticles -> {S[_], V[2 | 3], F[4], F[3, {2 | 3}]}]; 
 
Paint[diags, ColumnsXRows -> {2, 2}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 512}];
```

![1tcgqvpx23p0b](img/1tcgqvpx23p0b.svg)

## Obtain the amplitude

The 1/(2Pi)^D prefactor is implicit.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, GaugeRules -> {}, 
    	PreFactor -> 1], IncomingMomenta -> {p}, OutgoingMomenta -> {p}, LoopMomenta -> {q}, 
   	UndoChiralSplittings -> True, LorentzIndexNames -> {mu, nu}, 
   	ChangeDimension -> D, List -> True, DropSumOver -> True, 
   	FinalSubstitutions -> {FCGV["MU"] -> SMP["m_q"]}];
```

```mathematica
amp[1] = amp[0] // Contract[#, FCParallelize -> True] &;
```

## Calculate the amplitude

### The gluon tadpole 

This contribution is zero in dimensional regularization, because the loop integrals have no scale (and they are not log divergent)

```mathematica
amp1[0] = TID[amp[1][[1]], q, ToPaVe -> True, FCParallelize -> True]
```

$$0$$

```mathematica
FCCompareResults[amp1[0], 0, 
   Text -> {"\tThe gluon tadpole vanishes:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
```

$$\text{$\backslash $tThe gluon tadpole vanishes:} \;\text{CORRECT.}$$

### The quark loop

```mathematica
amp2[0] = amp[1][[2]] // SUNSimplify[#, FCParallelize -> True] & // 
   TID[#, q, ToPaVe -> True, FCParallelize -> True] &
```

$$\frac{i \pi ^2 g_s^2 \delta ^{\text{Glu1}\;\text{Glu2}} \;\text{B}_0\left(p^2,m_q^2,m_q^2\right) \left(-\left((1-D) p^4 g^{\mu \nu }\right)+2 (1-D) p^2 p^{\mu } p^{\nu }+D p^2 p^{\mu } p^{\nu }+4 p^2 m_q^2 g^{\mu \nu }-p^4 g^{\mu \nu }-4 m_q^2 p^{\mu } p^{\nu }\right)}{(1-D) p^2}-\frac{2 i \pi ^2 g_s^2 \delta ^{\text{Glu1}\;\text{Glu2}} \;\text{A}_0\left(m_q^2\right) \left(-(1-D) p^2 g^{\mu \nu }-D p^{\mu } p^{\nu }-p^2 g^{\mu \nu }+2 p^{\mu } p^{\nu }\right)}{(1-D) p^2}$$

The contribution of the quark loop alone is  gauge invariant.

```mathematica
tmp = Contract[FVD[p, mu] FVD[p, nu] amp2[0]] // Factor
FCCompareResults[tmp, 0, 
   Text -> {"\tThe quark loop contribution is gauge invariant:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
```

$$0$$

$$\text{$\backslash $tThe quark loop contribution is gauge invariant:} \;\text{CORRECT.}$$

### The ghost loop

```mathematica
amp3[0] = amp[0][[3]] // SUNSimplify[#, FCParallelize -> True] & // 
   TID[#, q, ToPaVe -> True, FCParallelize -> True] &
```

$$\frac{i \pi ^2 C_A g_s^2 \delta ^{\text{Glu1}\;\text{Glu2}} \;\text{B}_0\left(p^2,0,0\right) \left(2 (1-D) p^{\mu } p^{\nu }+D p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)}{4 (1-D)}$$

The contribution of the gluon loop alone is not gauge invariant.

```mathematica
tmp1 = Contract[FVD[p, mu] FVD[p, nu] amp3[0], FCParallelize -> True] // Factor
```

$$\frac{1}{4} i \pi ^2 p^4 C_A g_s^2 \delta ^{\text{Glu1}\;\text{Glu2}} \;\text{B}_0\left(p^2,0,0\right)$$

### The gluon loop

```mathematica
amp4[0] = amp[0][[4]] // SUNSimplify[#, FCParallelize -> True] & // 
   TID[#, q, ToPaVe -> True, FCParallelize -> True] &
```

![1cedw3mv5u88h](img/1cedw3mv5u88h.svg)

$$-\frac{i C_A \pi ^2 \;\text{D}_0\left(0,p^2,0,p^2,p^2,p^2,0,0,0,0\right) \left(1-\xi _g\right){}^2 p^4 \left((1-D) p^{\mu } p^{\nu }+D p^{\mu } p^{\nu }-g^{\mu \nu } p^2\right) \delta ^{\text{Glu1}\;\text{Glu2}} g_s^2}{8 (1-D)}-\frac{i C_A \pi ^2 \;\text{B}_0(0,0,0) \left(1-\xi _g\right) \left(7 (1-D) p^{\mu } p^{\nu }+3 D p^{\mu } p^{\nu }-(1-D) \xi _g p^{\mu } p^{\nu }-D \xi _g p^{\mu } p^{\nu }-4 (1-D) g^{\mu \nu } p^2+\xi _g g^{\mu \nu } p^2-3 g^{\mu \nu } p^2\right) \delta ^{\text{Glu1}\;\text{Glu2}} g_s^2}{4 (1-D)}+\frac{i C_A \pi ^2 \;\text{C}_0\left(0,p^2,p^2,0,0,0\right) \left(1-\xi _g\right) p^2 \left(3 (1-D) p^{\mu } p^{\nu }+D p^{\mu } p^{\nu }-\xi _g p^{\mu } p^{\nu }+p^{\mu } p^{\nu }-2 (1-D) g^{\mu \nu } p^2+\xi _g g^{\mu \nu } p^2-2 g^{\mu \nu } p^2\right) \delta ^{\text{Glu1}\;\text{Glu2}} g_s^2}{2 (1-D)}-\frac{1}{4 (1-D)}i C_A \pi ^2 \;\text{B}_0\left(p^2,0,0\right) \left(2 p^{\mu } p^{\nu } D^2-\xi _g^2 p^{\mu } p^{\nu } D+2 (1-D) p^{\mu } p^{\nu } D+6 \xi _g p^{\mu } p^{\nu } D-8 p^{\mu } p^{\nu } D-2 g^{\mu \nu } p^2 D-(1-D) \xi _g^2 p^{\mu } p^{\nu }+(1-D) p^{\mu } p^{\nu }+6 (1-D) \xi _g p^{\mu } p^{\nu }-8 \xi _g p^{\mu } p^{\nu }+8 p^{\mu } p^{\nu }+\xi _g^2 g^{\mu \nu } p^2-8 (1-D) g^{\mu \nu } p^2+2 \xi _g g^{\mu \nu } p^2\right) \delta ^{\text{Glu1}\;\text{Glu2}} g_s^2$$

The contribution of the gluon loop alone is not gauge invariant. Notice, however, that the sum
of the ghost and gluon contributions is gauge invariant!

```mathematica
tmp2 = Contract[FVD[p, mu] FVD[p, nu] amp4[0], FCParallelize -> True] // Factor
```

$$-\frac{1}{4} i \pi ^2 p^4 C_A g_s^2 \delta ^{\text{Glu1}\;\text{Glu2}} \;\text{B}_0\left(p^2,0,0\right)$$

```mathematica
FCCompareResults[tmp1 + tmp2, 0, 
   Text -> {"\tThe sum of the ghost and gluon loop contributions is gauge invariant:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
```

$$\text{$\backslash $tThe sum of the ghost and gluon loop contributions is gauge invariant:} \;\text{CORRECT.}$$

### Putting everything together

When adding all the contributions together, we multiply the quark contribution by N_f to account for the 6 quark flavors that actually run in that loop. We ignore the fact that different flavors have different masses, since the divergent piece of the gluon self-energy will not depend on the quark mass.

```mathematica
amp[1] = Nf amp2[0] + amp3[0] + amp4[0]
```

$$-\frac{i C_A \pi ^2 \;\text{D}_0\left(0,p^2,0,p^2,p^2,p^2,0,0,0,0\right) \left(1-\xi _g\right){}^2 p^4 \left((1-D) p^{\mu } p^{\nu }+D p^{\mu } p^{\nu }-g^{\mu \nu } p^2\right) \delta ^{\text{Glu1}\;\text{Glu2}} g_s^2}{8 (1-D)}+\frac{i C_A \pi ^2 \;\text{B}_0\left(p^2,0,0\right) \left(2 (1-D) p^{\mu } p^{\nu }+D p^{\mu } p^{\nu }-g^{\mu \nu } p^2\right) \delta ^{\text{Glu1}\;\text{Glu2}} g_s^2}{4 (1-D)}-\frac{i C_A \pi ^2 \;\text{B}_0(0,0,0) \left(1-\xi _g\right) \left(7 (1-D) p^{\mu } p^{\nu }+3 D p^{\mu } p^{\nu }-(1-D) \xi _g p^{\mu } p^{\nu }-D \xi _g p^{\mu } p^{\nu }-4 (1-D) g^{\mu \nu } p^2+\xi _g g^{\mu \nu } p^2-3 g^{\mu \nu } p^2\right) \delta ^{\text{Glu1}\;\text{Glu2}} g_s^2}{4 (1-D)}+\frac{i C_A \pi ^2 \;\text{C}_0\left(0,p^2,p^2,0,0,0\right) \left(1-\xi _g\right) p^2 \left(3 (1-D) p^{\mu } p^{\nu }+D p^{\mu } p^{\nu }-\xi _g p^{\mu } p^{\nu }+p^{\mu } p^{\nu }-2 (1-D) g^{\mu \nu } p^2+\xi _g g^{\mu \nu } p^2-2 g^{\mu \nu } p^2\right) \delta ^{\text{Glu1}\;\text{Glu2}} g_s^2}{2 (1-D)}-\frac{1}{4 (1-D)}i C_A \pi ^2 \;\text{B}_0\left(p^2,0,0\right) \left(2 p^{\mu } p^{\nu } D^2-\xi _g^2 p^{\mu } p^{\nu } D+2 (1-D) p^{\mu } p^{\nu } D+6 \xi _g p^{\mu } p^{\nu } D-8 p^{\mu } p^{\nu } D-2 g^{\mu \nu } p^2 D-(1-D) \xi _g^2 p^{\mu } p^{\nu }+(1-D) p^{\mu } p^{\nu }+6 (1-D) \xi _g p^{\mu } p^{\nu }-8 \xi _g p^{\mu } p^{\nu }+8 p^{\mu } p^{\nu }+\xi _g^2 g^{\mu \nu } p^2-8 (1-D) g^{\mu \nu } p^2+2 \xi _g g^{\mu \nu } p^2\right) \delta ^{\text{Glu1}\;\text{Glu2}} g_s^2+N_f \left(\frac{i \pi ^2 \;\text{B}_0\left(p^2,m_q^2,m_q^2\right) g_s^2 \left(-\left((1-D) g^{\mu \nu } p^4\right)-g^{\mu \nu } p^4+4 g^{\mu \nu } m_q^2 p^2+2 (1-D) p^{\mu } p^{\nu } p^2+D p^{\mu } p^{\nu } p^2-4 p^{\mu } p^{\nu } m_q^2\right) \delta ^{\text{Glu1}\;\text{Glu2}}}{(1-D) p^2}-\frac{2 i \pi ^2 \;\text{A}_0\left(m_q^2\right) \left(-D p^{\mu } p^{\nu }+2 p^{\mu } p^{\nu }-(1-D) g^{\mu \nu } p^2-g^{\mu \nu } p^2\right) g_s^2 \delta ^{\text{Glu1}\;\text{Glu2}}}{(1-D) p^2}\right)$$

The UV divergence of the amplitude can be obtained via PaVeUVPart.
Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.
Hint: If you need the full result for the amplitude, use PaXEvaluate from FeynHelpers.

```mathematica
ampDiv[0] = PaVeUVPart[amp[1], Prefactor -> 1/(2 Pi)^D] // 
       FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
    SelectNotFree2[#, Epsilon] & // Simplify
```

$$\frac{i g_s^2 \delta ^{\text{Glu1}\;\text{Glu2}} \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right) \left(3 C_A \xi _g-13 C_A+4 N_f\right)}{96 \pi ^2 \varepsilon }$$

The self-energy amplitude is usually defined as  (p^2 g^{mu nu} - p^mu p^nu) i Pi(p^2)

```mathematica
pi[0] = FCI[ampDiv[0]/(I SUNDelta[SUNIndex[Glu1], SUNIndex[Glu2]]*
      	(SPD[p, p] MTD[mu, nu] - FVD[p, mu] FVD[p, nu]))] // Cancel
```

$$-\frac{g_s^2 \left(3 C_A \xi _g-13 C_A+4 N_f\right)}{96 \pi ^2 \varepsilon }$$

## Check the final results

```mathematica
knownResult = -(SMP["g_s"]^2/(4 Pi)^2)*(4/3*(1/2)*Nf - 
      (1/2) CA (13/3 - GaugeXi[g]))*1/Epsilon;
FCCompareResults[pi[0], knownResult, 
   Text -> {"\tCompare to Muta, Foundations of QCD, Eqs 2.5.131-2.5.132:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```mathematica

$$\text{$\backslash $tCompare to Muta, Foundations of QCD, Eqs 2.5.131-2.5.132:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }39.496\text{ s.}$$