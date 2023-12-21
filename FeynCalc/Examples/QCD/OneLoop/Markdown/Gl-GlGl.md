---
title: QCD 3-gluon vertex at 1-loop
---


## Load FeynCalc and the necessary add-ons or other packages

This example uses a custom QCD model created with FeynRules. Please evaluate the file
FeynCalc/Examples/FeynRules/QCD/GenerateModelQCD.m before running it for the first time.

```mathematica
description = "Gl - Gl Gl, QCD, only UV divergences, 1-loop";
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

```mathematica
FAPatch[PatchModelsOnly -> True];

(*Successfully patched FeynArts.*)
```

## Generate Feynman diagrams

Nicer typesetting

```mathematica
MakeBoxes[mu, TraditionalForm] := "\[Mu]";
MakeBoxes[nu, TraditionalForm] := "\[Nu]";
MakeBoxes[rho, TraditionalForm] := "\[Rho]";
```

```mathematica
template = insertFields[createTopologies[1, 1 -> 2, 
    		ExcludeTopologies -> {Tadpoles, WFCorrections, 
      		WFCorrectionCTs, SelfEnergies}], {V[5]} -> 
    		{V[5], V[5]}, InsertionLevel -> {Particles}, 
   		Model -> FileNameJoin[{"QCD", "QCD"}], 
   		GenericModel -> FileNameJoin[{"QCD", "QCD"}], 
   		ExcludeParticles -> {F[3 | 4, {2 | 3}], F[4, {1}]}];
```

```mathematica
diags = template /. createTopologies -> CreateTopologies /. 
   	insertFields -> InsertFields;
diagsCT = template /. createTopologies -> CreateCTTopologies /. 
   	insertFields -> InsertFields;
```

```mathematica
Paint[diags, ColumnsXRows -> {3, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> {512, 256}];
```

![1upk049xa8r7h](img/1upk049xa8r7h.svg)

![0cty7gtqz5g4m](img/0cty7gtqz5g4m.svg)

![16fxlye0ek1v9](img/16fxlye0ek1v9.svg)

```mathematica
Paint[diagsCT, ColumnsXRows -> {3, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> {512, 256}];
```

![1pfdvzv05q15f](img/1pfdvzv05q15f.svg)

## Obtain the amplitudes

The 1/(2Pi)^D prefactor is implicit. We keep the full gauge dependence. To simplify comparisons
to the literature, we make all momenta incoming.

Quark contribution. Notice that we multiply the amplitude by Nf to account for the number
of quark flavours in the loop.

```mathematica
amp1[0] = FCFAConvert[CreateFeynAmp[DiagramExtract[diags, {1, 2}], 
   	Truncated -> True, GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p1}, OutgoingMomenta -> {p2, p3}, 
  	LorentzIndexNames -> {mu, nu, rho}, DropSumOver -> True, 
  	SUNIndexNames -> {a, b, c}, LoopMomenta -> {l}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> False, SMP -> True, Prefactor -> Nf, 
  	FinalSubstitutions -> {SMP["m_u"] -> SMP["m_q"], p2 -> -p2, p3 -> -p3}]
```

$$\frac{i N_f \;\text{tr}\left(\left(\gamma \cdot (l+\text{p2}+\text{p3})+m_q\right).\left(-i \gamma ^{\rho } g_s T_{\text{Col5}\;\text{Col6}}^c\right).\left(\gamma \cdot (l+\text{p2})+m_q\right).\left(-i \gamma ^{\nu } g_s T_{\text{Col6}\;\text{Col4}}^b\right).\left(\gamma \cdot l+m_q\right).\left(-i \gamma ^{\mu } g_s T_{\text{Col4}\;\text{Col5}}^a\right)\right)}{\left(l^2-m_q^2\right).\left((l+\text{p2})^2-m_q^2\right).\left((l+\text{p2}+\text{p3})^2-m_q^2\right)}+\frac{i N_f \;\text{tr}\left(\left(\gamma \cdot (l+\text{p2}+\text{p3})+m_q\right).\left(i \gamma ^{\rho } g_s T_{\text{Col6}\;\text{Col5}}^c\right).\left(\gamma \cdot (l+\text{p2})+m_q\right).\left(i \gamma ^{\nu } g_s T_{\text{Col4}\;\text{Col6}}^b\right).\left(\gamma \cdot l+m_q\right).\left(i \gamma ^{\mu } g_s T_{\text{Col5}\;\text{Col4}}^a\right)\right)}{\left(l^2-m_q^2\right).\left((l+\text{p2})^2-m_q^2\right).\left((l+\text{p2}+\text{p3})^2-m_q^2\right)}$$

Ghost contribution

```mathematica
amp2[0] = FCFAConvert[CreateFeynAmp[DiagramExtract[diags, {3, 4}], 
   	Truncated -> True, GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p1}, OutgoingMomenta -> {p2, p3}, 
  	LorentzIndexNames -> {mu, nu, rho}, SUNIndexNames -> {a, b, c}, 
  	DropSumOver -> True, LoopMomenta -> {l}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> False, SMP -> True, 
  	FinalSubstitutions -> {SMP["m_u"] -> SMP["m_q"], p2 -> -p2, p3 -> -p3}]
```

$$\frac{i g_s^3 l^{\nu } (l+\text{p2})^{\rho } f^{a\text{Glu4}\;\text{Glu5}} f^{b\text{Glu4}\;\text{Glu6}} f^{c\text{Glu5}\;\text{Glu6}} (l+\text{p2}+\text{p3})^{\mu }}{l^2.(l+\text{p2})^2.(l+\text{p2}+\text{p3})^2}+\frac{i g_s^3 l^{\mu } (-l-\text{p2})^{\nu } f^{a\text{Glu4}\;\text{Glu5}} f^{b\text{Glu4}\;\text{Glu6}} f^{c\text{Glu5}\;\text{Glu6}} (-l-\text{p2}-\text{p3})^{\rho }}{l^2.(l+\text{p2})^2.(l+\text{p2}+\text{p3})^2}$$

Gluon contribution

```mathematica
amp3[0] = FCFAConvert[CreateFeynAmp[DiagramExtract[diags, {5, 6, 7, 8}], 
    	Truncated -> True, GaugeRules -> {}, PreFactor -> 1], 
   	IncomingMomenta -> {p1}, OutgoingMomenta -> {p2, p3}, 
   	LorentzIndexNames -> {mu, nu, rho}, SUNIndexNames -> {a, b, c}, 
   	DropSumOver -> True, LoopMomenta -> {l}, UndoChiralSplittings -> True, 
   	ChangeDimension -> D, List -> False, SMP -> True, 
   	FinalSubstitutions -> {SMP["m_u"] -> SMP["m_q"], p2 -> -p2, p3 -> -p3}];
```

Counter-term

```mathematica
amp4[0] = FCFAConvert[CreateFeynAmp[diagsCT, 
   	Truncated -> True, GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p1}, OutgoingMomenta -> {p2, p3}, 
  	LorentzIndexNames -> {mu, nu, rho}, SUNIndexNames -> {a, b, c}, 
  	DropSumOver -> True, LoopMomenta -> {l}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> False, SMP -> True, 
  	FinalSubstitutions -> {SMP["m_u"] -> SMP["m_q"], p2 -> -p2, p3 -> -p3, 
    	ZA -> SMP["Z_A"], Zg -> SMP["Z_g"]}]
```

$$g_s \;\text{p1}^{\nu } \left(Z_A^{3/2} Z_g-1\right) g^{\mu \rho } f^{abc}-g_s \;\text{p1}^{\rho } \left(Z_A^{3/2} Z_g-1\right) g^{\mu \nu } f^{abc}+g_s \left(-\text{p2}^{\mu }\right) \left(Z_A^{3/2} Z_g-1\right) g^{\nu \rho } f^{abc}+g_s \;\text{p2}^{\rho } \left(Z_A^{3/2} Z_g-1\right) g^{\mu \nu } f^{abc}+g_s \;\text{p3}^{\mu } \left(Z_A^{3/2} Z_g-1\right) g^{\nu \rho } f^{abc}-g_s \;\text{p3}^{\nu } \left(Z_A^{3/2} Z_g-1\right) g^{\mu \rho } f^{abc}$$

## Calculate the amplitudes

### Quark contribution

```mathematica
AbsoluteTiming[amp1[1] = TID[FCE[amp1[0]] /. {p2 + p3 -> -p1, -p2 - p3 -> p1}, l, 
    	UsePaVeBasis -> True, ToPaVe -> True];]
```

$$\{1.59185,\text{Null}\}$$

```mathematica
amp1Div[0] = amp1[1] // PaVeUVPart[#, Prefactor -> 1/(2 Pi)^D, FCLoopExtract -> False] &;
```

```mathematica
amp1Div[1] = amp1Div[0] // SUNSimplify[#, Explicit -> True] & // ReplaceAll[#, 
           	SUNTrace[x__] :> SUNTrace[x, Explicit -> True]] & // 
        	FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // 
      	Normal // FCHideEpsilon // SelectNotFree2[#, SMP["Delta"]] & //FCE // 
  	Collect2[#, MTD, Factoring -> Function[x, MomentumCombine[Factor[x]]]] &
```

$$-\frac{\Delta  N_f g_s^3 g^{\nu \rho } (\text{p1}+2 \;\text{p2})^{\mu } f^{abc}}{24 \pi ^2}+\frac{\Delta  N_f g_s^3 g^{\mu \rho } (2 \;\text{p1}+\text{p2})^{\nu } f^{abc}}{24 \pi ^2}-\frac{\Delta  N_f g_s^3 g^{\mu \nu } (\text{p1}-\text{p2})^{\rho } f^{abc}}{24 \pi ^2}$$

In the calculation p3 was eliminated via the 4-momentum conservation p1+p2+p3=0. 
Now we need to reintroduce it

```mathematica
amp1Div[2] = amp1Div[1] /. {2 p1 + p2 -> p1 - p3, p1 + 2 p2 -> p2 - p3}
```

$$-\frac{\Delta  N_f g_s^3 g^{\mu \nu } (\text{p1}-\text{p2})^{\rho } f^{abc}}{24 \pi ^2}+\frac{\Delta  N_f g_s^3 g^{\mu \rho } (\text{p1}-\text{p3})^{\nu } f^{abc}}{24 \pi ^2}-\frac{\Delta  N_f g_s^3 g^{\nu \rho } (\text{p2}-\text{p3})^{\mu } f^{abc}}{24 \pi ^2}$$

### Ghost contribution

```mathematica
AbsoluteTiming[amp2[1] = TID[FCE[amp2[0]] /. {p2 + p3 -> -p1, -p2 - p3 -> p1}, l, 
    	UsePaVeBasis -> True, ToPaVe -> True];]
```

$$\{0.432995,\text{Null}\}$$

```mathematica
amp2Div[0] = amp2[1] // PaVeUVPart[#, Prefactor -> 1/(2 Pi)^D, FCLoopExtract -> False] &;
```

```mathematica
amp2Div[1] = amp2Div[0] // SUNSimplify[#, Explicit -> True] & // ReplaceAll[#, 
           	SUNTrace[x__] :> SUNTrace[x, Explicit -> True]] & // 
        	FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // 
      	Normal // FCHideEpsilon // SelectNotFree2[#, SMP["Delta"]] & //FCE // 
  	Collect2[#, MTD, Factoring -> Function[x, MomentumCombine[Factor[x]]]] &
```

$$\frac{\Delta  C_A g_s^3 g^{\nu \rho } (\text{p1}+2 \;\text{p2})^{\mu } f^{abc}}{384 \pi ^2}-\frac{\Delta  C_A g_s^3 g^{\mu \rho } (2 \;\text{p1}+\text{p2})^{\nu } f^{abc}}{384 \pi ^2}+\frac{\Delta  C_A g_s^3 g^{\mu \nu } (\text{p1}-\text{p2})^{\rho } f^{abc}}{384 \pi ^2}$$

In the calculation p3 was eliminated via the 4-momentum conservation p1+p2+p3=0. 
Now we need to reintroduce it

```mathematica
amp2Div[2] = amp2Div[1] /. {2 p1 + p2 -> p1 - p3, p1 + 2 p2 -> p2 - p3}
```

$$\frac{\Delta  C_A g_s^3 g^{\mu \nu } (\text{p1}-\text{p2})^{\rho } f^{abc}}{384 \pi ^2}-\frac{\Delta  C_A g_s^3 g^{\mu \rho } (\text{p1}-\text{p3})^{\nu } f^{abc}}{384 \pi ^2}+\frac{\Delta  C_A g_s^3 g^{\nu \rho } (\text{p2}-\text{p3})^{\mu } f^{abc}}{384 \pi ^2}$$

### Gluon contribution

This calculation requires about 70 seconds on a modern laptop

```mathematica
AbsoluteTiming[amp3[1] = TID[FCE[amp3[0]] /. {p2 + p3 -> -p1, -p2 - p3 -> p1}, l, 
    	UsePaVeBasis -> True, ExpandScalarProduct -> False, ToPaVe -> True];]
```

$$\{56.6716,\text{Null}\}$$

```mathematica
amp3Div[0] = amp3[1] // PaVeUVPart[#, Prefactor -> 1/(2 Pi)^D, FCLoopExtract -> False] &;
```

```mathematica
amp3Div[1] = amp3Div[0] // SUNSimplify // FCReplaceD[#, D -> 4 - 2 Epsilon] & // 
       	Series[#, {Epsilon, 0, 0}] & // Normal // FCHideEpsilon // 
    	SelectNotFree2[#, SMP["Delta"]] & // FCE // 
  	Collect2[#, MTD, Factoring -> Function[x, MomentumCombine[Factor[x]]]] &
```

$$-\frac{\Delta  C_A g_s^3 g^{\nu \rho } f^{abc} \left(6 \xi _{\text{G}} \;\text{p1}^{\mu }-3 \xi _{\text{G}} (\text{p1}+\text{p2}+\text{p3})^{\mu }+12 \xi _{\text{G}} \;\text{p2}^{\mu }+(4 \;\text{p1}-7 \;\text{p2}+15 \;\text{p3})^{\mu }\right)}{128 \pi ^2}+\frac{\Delta  C_A g_s^3 g^{\mu \rho } f^{abc} \left(13 \xi _{\text{G}} \;\text{p1}^{\nu }-4 \xi _{\text{G}} (\text{p1}+\text{p2})^{\nu }+7 \xi _{\text{G}} \;\text{p2}^{\nu }-3 \xi _{\text{G}} \;\text{p3}^{\nu }+(-7 \;\text{p1}+4 \;\text{p2}+15 \;\text{p3})^{\nu }\right)}{128 \pi ^2}-\frac{\Delta  C_A \left(6 \xi _{\text{G}}-11\right) g_s^3 g^{\mu \nu } (\text{p1}-\text{p2})^{\rho } f^{abc}}{128 \pi ^2}$$

```mathematica
amp3Div[2] = (((amp3Div[1] /. {p3 -> -p1 - p2}) // ExpandScalarProduct // FCE // 
           	Collect2[#, MTD, GaugeXi, Factoring -> Function[x, MomentumCombine[Factor[x]]]] &) /. 
         	2 p1 + p2 -> p1 - p3 /. p1 + 2 p2 -> p2 - p3 /. 22 p1 + 11 p2 -> 11 p1 - 11 p3 /. 
      	-22 p2 - 11 p1 -> -11 p2 + 11 p3) // ExpandScalarProduct // FCE // 
  	Collect2[#, MTD, Factoring -> Function[x, MomentumCombine[Factor[x]]]] &
```

$$-\frac{\Delta  C_A \left(6 \xi _{\text{G}}-11\right) g_s^3 g^{\mu \nu } (\text{p1}-\text{p2})^{\rho } f^{abc}}{128 \pi ^2}+\frac{\Delta  C_A \left(6 \xi _{\text{G}}-11\right) g_s^3 g^{\mu \rho } (\text{p1}-\text{p3})^{\nu } f^{abc}}{128 \pi ^2}-\frac{\Delta  C_A \left(6 \xi _{\text{G}}-11\right) g_s^3 g^{\nu \rho } (\text{p2}-\text{p3})^{\mu } f^{abc}}{128 \pi ^2}$$

### Counter-term

```mathematica
amp4[1] = amp4[0] // ReplaceAll[#, {SMP["Z_A"] -> 1 + alpha SMP["d_A"], 
           	SMP["Z_g"] -> 1 + alpha SMP["d_g"]}] & // Series[#, {alpha, 0, 1}] & // 
      	Normal // ReplaceAll[#, alpha -> 1] & // ExpandScalarProduct //FCE // 
  	Collect2[#, MTD, GaugeXi, Factoring -> Function[x, MomentumCombine[Factor[x]]]] &
```

$$-\frac{1}{2} g_s \left(3 \delta _A+2 \delta _g\right) g^{\mu \nu } (\text{p1}-\text{p2})^{\rho } f^{abc}-\frac{1}{2} g_s \left(3 \delta _A+2 \delta _g\right) g^{\mu \rho } (\text{p3}-\text{p1})^{\nu } f^{abc}-\frac{1}{2} g_s \left(3 \delta _A+2 \delta _g\right) g^{\nu \rho } (\text{p2}-\text{p3})^{\mu } f^{abc}$$

Check the cancellation of the UV divergences in the MSbar scheme. The renormalization constants
are obtained from another example calculation, "Renormalization.m"

```mathematica
renormalizationConstants = {
    	SMP["d_A"] -> SMP["alpha_s"]/(4 Pi) SMP["Delta"] (1/2 CA (13/3 - GaugeXi["G"]) - 2/3 Nf), 
    	SMP["d_g"] -> ((-11*CA*SMP["alpha_s"])/(24 Pi) SMP["Delta"] + (Nf*SMP["alpha_s"])/(12*Pi) SMP["Delta"]) 
    } /. SMP["alpha_s"] -> SMP["g_s"]^2/(4 Pi);
```

```mathematica
uvDiv[0] = ExpandScalarProduct[amp1Div[2] + amp2Div[2] + amp3Div[2] + amp4[1]] //FCE // 
   Collect2[#, MTD, Factoring -> Function[x, MomentumCombine[Factor[x]]]] &
```

$$-\frac{g_s g^{\mu \nu } (\text{p1}-\text{p2})^{\rho } f^{abc} \left(9 \Delta  C_A \xi _{\text{G}} g_s^2-17 \Delta  C_A g_s^2+288 \pi ^2 \delta _A+8 \Delta  N_f g_s^2+192 \pi ^2 \delta _g\right)}{192 \pi ^2}+\frac{g_s g^{\mu \rho } (\text{p1}-\text{p3})^{\nu } f^{abc} \left(9 \Delta  C_A \xi _{\text{G}} g_s^2-17 \Delta  C_A g_s^2+288 \pi ^2 \delta _A+8 \Delta  N_f g_s^2+192 \pi ^2 \delta _g\right)}{192 \pi ^2}-\frac{g_s g^{\nu \rho } (\text{p2}-\text{p3})^{\mu } f^{abc} \left(9 \Delta  C_A \xi _{\text{G}} g_s^2-17 \Delta  C_A g_s^2+288 \pi ^2 \delta _A+8 \Delta  N_f g_s^2+192 \pi ^2 \delta _g\right)}{192 \pi ^2}$$

```mathematica
uvDiv[1] = (uvDiv[0] /. renormalizationConstants) // Simplify
```

$$0$$

```mathematica
FCCompareResults[uvDiv[1], 0, 
   Text -> {"\tThe UV divergence of the 3-gluon vertex at 1-loop is cancelled by the counter-term :", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
```

$$\text{$\backslash $tThe UV divergence of the 3-gluon vertex at 1-loop is cancelled by the counter-term :} \;\text{CORRECT.}$$

## Check the final results

```mathematica
VertexLorentzStruct[{p_, q_, k_}, {mu_, nu_, si_}, {a_, b_, c_}] := 
   -I SUNF[a, b, c] (MTD[mu, nu] FVD[p - q, si] + MTD[nu, si] FVD[q - k, mu] + MTD[si, mu] FVD[k - p, nu]);
```

```mathematica
knownResult = {
    	(I SMP["g_s"]) Nf SMP["g_s"]^2/(4 Pi)^2*(-2/3) SMP["Delta"]*
     		VertexLorentzStruct[{p1, p2, p3}, {mu, nu, rho}, {a, b, c}], 
     
     
    	(I SMP["g_s"]) SMP["g_s"]^2/(4 Pi)^2  CA/8 (1/3) SMP["Delta"]*
     		VertexLorentzStruct[{p1, p2, p3}, {mu, nu, rho}, {a, b, c}], 
     
     
    	(I SMP["g_s"]) SMP["g_s"]^2/(4 Pi)^2  CA/8 SMP["Delta"]*(
      		-4 - 9 GaugeXi["G"] + 15 + 3 GaugeXi["G"])*
     	VertexLorentzStruct[{p1, p2, p3}, {mu, nu, rho}, {a, b, c}] 
    } // FCI;
FCCompareResults[{amp1Div[2], amp2Div[2], amp3Div[2]}, knownResult, 
  	Text -> {"\tCompare to Pascual and Tarrach, QCD: Renormalization for the Practitioner, Eq III.46:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Pascual and Tarrach, QCD: Renormalization for the Practitioner, Eq III.46:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }92.55\text{ s.}$$