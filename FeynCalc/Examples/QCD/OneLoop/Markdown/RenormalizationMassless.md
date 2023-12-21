---
title: 1-loop massless QCD renormalization in the minimal subtraction schemes
---


## Load FeynCalc and the necessary add-ons or other packages

This example uses a custom QCD model created with FeynRules. Please evaluate the file
FeynCalc/Examples/FeynRules/QCD/GenerateModelQCD.m before running it for the first time.

```mathematica
description = "Renormalization, massless QCD, MS and MSbar, 1-loop";
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
MakeBoxes[si, TraditionalForm] := "\[Sigma]";
```

```mathematica
params = {InsertionLevel -> {Particles}, Model -> FileNameJoin[{"QCD", "QCD"}], 
    GenericModel -> FileNameJoin[{"QCD", "QCD"}], ExcludeParticles -> {F[3 | 4, {2 | 3}], F[4, {1}]}};
top[i_, j_] := CreateTopologies[1, i -> j, 
   	ExcludeTopologies -> {Tadpoles, WFCorrections, WFCorrectionCTs}];
topTriangle[i_, j_] := CreateTopologies[1, i -> j, 
    	ExcludeTopologies -> {Tadpoles, WFCorrections, WFCorrectionCTs, SelfEnergies}]; 
 
topCT[i_, j_] := CreateCTTopologies[1, i -> j, 
   	ExcludeTopologies -> {Tadpoles, WFCorrections, WFCorrectionCTs}];
topTriangleCT[i_, j_] := CreateCTTopologies[1, i -> j, 
    	ExcludeTopologies -> {Tadpoles, WFCorrections, WFCorrectionCTs, SelfEnergyCTs}]; 
 
{diagQuarkSE, diagQuarkSECT} = InsertFields[#, {F[3, {1}]} -> {F[3, {1}]}, 
      	Sequence @@ params] & /@ {top[1, 1], topCT[1, 1]}; 
 
{diagGluonSE, diagGluonSECT} = InsertFields[#, {V[5]} -> {V[5]}, 
      	Sequence @@ params] & /@ {top[1, 1], topCT[1, 1]}; 
 
{diagGhostSE, diagGhostSECT} = InsertFields[#, {U[5]} -> {U[5]}, 
      	Sequence @@ params] & /@ {top[1, 1], topCT[1, 1]}; 
 
{diagQuarkGluonVertex, diagQuarkGluonVertexCT} = InsertFields[#, 
     	{F[3, {1}], V[5]} -> {F[3, {1}]}, Sequence @@ params] & /@ {topTriangle[2, 1], topTriangleCT[2, 1]};
```

```mathematica
diag1[0] = diagQuarkSE[[0]][Sequence @@ diagQuarkSE, 
   	Sequence @@ diagQuarkSECT];
diag2[0] = diagGluonSE[[0]][Sequence @@ diagGluonSE, 
   	Sequence @@ diagGluonSECT];
diag3[0] = diagGhostSE[[0]][Sequence @@ diagGhostSE, 
   	Sequence @@ diagGhostSECT];
diag4[0] = diagQuarkGluonVertex[[0]][Sequence @@ diagQuarkGluonVertex, 
   	Sequence @@ diagQuarkGluonVertexCT];
```

```mathematica
Paint[diag1[0], ColumnsXRows -> {2, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> {512, 256}];
```

![0qo7e2z805c6c](img/0qo7e2z805c6c.svg)

```mathematica
Paint[diag2[0], ColumnsXRows -> {4, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> {512, 128}];
```

![1m7b27sd00d0v](img/1m7b27sd00d0v.svg)

![0yw7gz095v7pi](img/0yw7gz095v7pi.svg)

```mathematica
Paint[diag3[0], ColumnsXRows -> {2, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> {512, 256}];
```

![1m1jhfclhy7cu](img/1m1jhfclhy7cu.svg)

```mathematica
Paint[diag4[0], ColumnsXRows -> {3, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> {512, 256}];
```

![1hioq5saix3mh](img/1hioq5saix3mh.svg)

## Obtain the amplitudes

The 1/(2Pi)^D prefactor is implicit.

Quark self-energy including the counter-term

```mathematica
ampQuarkSE[0] = FCFAConvert[CreateFeynAmp[diag1[0], Truncated -> True, 
   	GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p}, OutgoingMomenta -> {p}, 
  	LorentzIndexNames -> {mu, nu}, DropSumOver -> True, 
  	LoopMomenta -> {l}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> False, SMP -> True, 
  	FinalSubstitutions -> {Zm -> SMP["Z_m"], Zpsi -> SMP["Z_psi"], 
    	SMP["m_u"] -> 0}]
```

$$\left(\frac{\left(1-\xi _{\text{G}}\right) (l-p)^{\mu } (p-l)^{\nu }}{l^2.(l-p)^4}+\frac{g^{\mu \nu }}{l^2.(l-p)^2}\right) \left(-i \gamma ^{\nu } g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu3}}\right).(\gamma \cdot l).\left(-i \gamma ^{\mu } g_s T_{\text{Col3}\;\text{Col1}}^{\text{Glu3}}\right)+i \left(Z_{\psi }-1\right) \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p$$

Gluon self-energy including the counter-term

```mathematica
ampGluonSE[0] = FCFAConvert[CreateFeynAmp[diag2[0], Truncated -> True, 
   	GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p}, OutgoingMomenta -> {p}, 
  	LorentzIndexNames -> {mu, nu, rho, si}, DropSumOver -> True, 
  	LoopMomenta -> {l}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> True, SMP -> True, 
  	FinalSubstitutions -> {ZA -> SMP["Z_A"], Zxi -> SMP["Z_xi"], 
    	SMP["m_u"] -> 0}]
```

$$\left\{-\frac{1}{2} i \left(\frac{g^{\rho \sigma }}{l^2}-\frac{\left(1-\xi _{\text{G}}\right) l^{\rho } l^{\sigma }}{\left(l^2\right)^2}\right) \left(i g^{\mu \rho } g^{\nu \sigma } f^{\text{Glu1}\;\text{Glu3}\;\text{\$AL\$14442}} f^{\text{Glu2}\;\text{Glu3}\;\text{\$AL\$14442}} g_s^2-2 i g^{\mu \nu } g^{\rho \sigma } f^{\text{Glu1}\;\text{Glu3}\;\text{\$AL\$14443}} f^{\text{Glu2}\;\text{Glu3}\;\text{\$AL\$14443}} g_s^2+i g^{\mu \sigma } g^{\nu \rho } f^{\text{Glu1}\;\text{Glu3}\;\text{\$AL\$14445}} f^{\text{Glu2}\;\text{Glu3}\;\text{\$AL\$14445}} g_s^2\right),\frac{\text{tr}\left((-(\gamma \cdot l)).\left(-i \gamma ^{\nu } g_s T_{\text{Col3}\;\text{Col4}}^{\text{Glu2}}\right).(\gamma \cdot (p-l)).\left(-i \gamma ^{\mu } g_s T_{\text{Col4}\;\text{Col3}}^{\text{Glu1}}\right)\right)}{l^2.(l-p)^2},-\frac{(l-p)^{\mu } l^{\nu } g_s^2 f^{\text{Glu1}\;\text{Glu3}\;\text{Glu4}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu4}}}{l^2.(l-p)^2},-\frac{1}{2} \left(-\frac{\left(1-\xi _{\text{G}}\right){}^2 (l-p)^{\text{Lor5}} (p-l)^{\text{Lor6}} l^{\rho } l^{\sigma }}{\left(l^2\right)^2.(l-p)^4}+\frac{\left(1-\xi _{\text{G}}\right) (l-p)^{\text{Lor5}} (p-l)^{\text{Lor6}} g^{\rho \sigma }}{l^2.(l-p)^4}-\frac{\left(1-\xi _{\text{G}}\right) g^{\text{Lor5}\;\text{Lor6}} l^{\rho } l^{\sigma }}{\left(l^2\right)^2.(l-p)^2}+\frac{g^{\text{Lor5}\;\text{Lor6}} g^{\rho \sigma }}{l^2.(l-p)^2}\right) \left(-l^{\text{Lor5}} g^{\mu \rho } g_s f^{\text{Glu1}\;\text{Glu3}\;\text{Glu4}}-p^{\text{Lor5}} g^{\mu \rho } g_s f^{\text{Glu1}\;\text{Glu3}\;\text{Glu4}}+g^{\text{Lor5}\rho } l^{\mu } g_s f^{\text{Glu1}\;\text{Glu3}\;\text{Glu4}}+g^{\text{Lor5}\rho } (l-p)^{\mu } g_s f^{\text{Glu1}\;\text{Glu3}\;\text{Glu4}}+g^{\text{Lor5}\mu } p^{\rho } g_s f^{\text{Glu1}\;\text{Glu3}\;\text{Glu4}}+g^{\text{Lor5}\mu } (p-l)^{\rho } g_s f^{\text{Glu1}\;\text{Glu3}\;\text{Glu4}}\right) \left(l^{\text{Lor6}} g^{\nu \sigma } g_s f^{\text{Glu2}\;\text{Glu3}\;\text{Glu4}}+p^{\text{Lor6}} g^{\nu \sigma } g_s f^{\text{Glu2}\;\text{Glu3}\;\text{Glu4}}-g^{\text{Lor6}\sigma } l^{\nu } g_s f^{\text{Glu2}\;\text{Glu3}\;\text{Glu4}}+g^{\text{Lor6}\sigma } (p-l)^{\nu } g_s f^{\text{Glu2}\;\text{Glu3}\;\text{Glu4}}+g^{\text{Lor6}\nu } (l-p)^{\sigma } g_s f^{\text{Glu2}\;\text{Glu3}\;\text{Glu4}}-g^{\text{Lor6}\nu } p^{\sigma } g_s f^{\text{Glu2}\;\text{Glu3}\;\text{Glu4}}\right),i p^{\mu } p^{\nu } \left(Z_A-1\right) \delta ^{\text{Glu1}\;\text{Glu2}}-i g^{\mu \nu } p^2 \left(Z_A-1\right) \delta ^{\text{Glu1}\;\text{Glu2}}-\frac{i p^{\mu } p^{\nu } \left(Z_A-Z_{\xi }\right) \delta ^{\text{Glu1}\;\text{Glu2}}}{\xi _{\text{G}} Z_{\xi }}\right\}$$

Ghost self-energy including the counter-term

```mathematica
ampGhostSE[0] = FCFAConvert[CreateFeynAmp[diag3[0], Truncated -> True, 
   	GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p}, OutgoingMomenta -> {p}, 
  	LorentzIndexNames -> {mu, nu}, DropSumOver -> True, 
  	LoopMomenta -> {l}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> False, SMP -> True, 
  	FinalSubstitutions -> {Zu -> SMP["Z_u"]}]
```

$$-g_s^2 l^{\mu } p^{\nu } f^{\text{Glu1}\;\text{Glu3}\;\text{Glu4}} f^{\text{Glu2}\;\text{Glu3}\;\text{Glu4}} \left(\frac{\left(1-\xi _{\text{G}}\right) (l-p)^{\mu } (p-l)^{\nu }}{l^2.(l-p)^4}+\frac{g^{\mu \nu }}{l^2.(l-p)^2}\right)+i p^2 \left(Z_u-1\right) \delta ^{\text{Glu1}\;\text{Glu2}}$$

Quark-gluon vertex including the counter-term

```mathematica
ampQGlVertex[0] = FCFAConvert[CreateFeynAmp[diag4[0], Truncated -> True, 
   	GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p1, k}, OutgoingMomenta -> {p2}, 
  	LorentzIndexNames -> {mu, nu, rho}, DropSumOver -> True, LoopMomenta -> {l}, 
  	UndoChiralSplittings -> True, ChangeDimension -> D, 
  	List -> False, SMP -> True, FinalSubstitutions -> 
   	{ZA -> SMP["Z_A"], Zg -> SMP["Z_g"], Zpsi -> SMP["Z_psi"], 
    	SMP["m_u"] -> 0}]
```

$$i \left(\frac{\left(1-\xi _{\text{G}}\right) (k+l-\text{p2})^{\nu } (-k-l+\text{p2})^{\rho }}{l^2.(k+l)^2.(k+l-\text{p2})^4}+\frac{g^{\nu \rho }}{l^2.(k+l)^2.(k+l-\text{p2})^2}\right) \left(-i \gamma ^{\rho } g_s T_{\text{Col3}\;\text{Col5}}^{\text{Glu4}}\right).(\gamma \cdot (k+l)).\left(-i \gamma ^{\mu } g_s T_{\text{Col5}\;\text{Col4}}^{\text{Glu2}}\right).(\gamma \cdot l).\left(-i \gamma ^{\nu } g_s T_{\text{Col4}\;\text{Col1}}^{\text{Glu4}}\right)-i \left(g_s g^{\text{Lor4}\rho } (-k-l)^{\mu } f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}+g_s g^{\text{Lor4}\mu } (k+l)^{\rho } f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}+g_s \left(-k^{\text{Lor4}}\right) g^{\mu \rho } f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}+g_s k^{\rho } g^{\text{Lor4}\mu } f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}+g_s l^{\text{Lor4}} g^{\mu \rho } f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}-g_s l^{\mu } g^{\text{Lor4}\rho } f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}}\right) \left(\frac{\left(1-\xi _{\text{G}}\right) g^{\nu \rho } (-k-l)^{\text{Lor4}} (k+l)^{\text{Lor5}}}{l^2.(k+l)^4.(k+l-\text{p2})^2}-\frac{\left(1-\xi _{\text{G}}\right) l^{\nu } l^{\rho } g^{\text{Lor4}\;\text{Lor5}}}{\left(l^2\right)^2.(k+l)^2.(k+l-\text{p2})^2}+-\frac{\left(1-\xi _{\text{G}}\right){}^2 l^{\nu } l^{\rho } (-k-l)^{\text{Lor4}} (k+l)^{\text{Lor5}}}{\left(l^2\right)^2.(k+l)^4.(k+l-\text{p2})^2}+\frac{g^{\text{Lor4}\;\text{Lor5}} g^{\nu \rho }}{l^2.(k+l)^2.(k+l-\text{p2})^2}\right) \left(-i g_s \gamma ^{\text{Lor5}} T_{\text{Col3}\;\text{Col4}}^{\text{Glu5}}\right).(\gamma \cdot (-k-l+\text{p2})).\left(-i \gamma ^{\nu } g_s T_{\text{Col4}\;\text{Col1}}^{\text{Glu4}}\right)-i \gamma ^{\mu } g_s \left(\sqrt{Z_A} Z_g Z_{\psi }-1\right) T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}$$

## Calculate the amplitudes

### Quark self-energy

Tensor reduction allows us to express the quark self-energy in tems of the Passarino-Veltman coefficient functions.

```mathematica
ampQuarkSE[1] = ampQuarkSE[0] // SUNSimplify // DiracSimplify // 
   	TID[#, l, UsePaVeBasis -> True, ToPaVe -> True] &;
```

Discard all the finite pieces of the 1-loop amplitude.

```mathematica
ampQuarkSEDiv[0] = ampQuarkSE[1] // PaVeUVPart[#, Prefactor -> 1/(2 Pi)^D] &;
```

```mathematica
ampQuarkSEDiv[1] = FCReplaceD[ampQuarkSEDiv[0], D -> 4 - 2 Epsilon] // 
     	Series[#, {Epsilon, 0, 0}] & // Normal // FCHideEpsilon // Simplify
```

$$\frac{i \left(C_A-2 C_F\right) \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p \left(\left(C_A^2-1\right) (\Delta +\gamma +\log (\pi )) \xi _{\text{G}} g_s^2+32 \pi ^2 C_A \left(Z_{\psi }-1\right)\right)}{32 \pi ^2}$$

```mathematica
ampQuarkSEDiv[2] = ampQuarkSEDiv[1] // ReplaceRepeated[#, {
         	SMP["Z_m"] -> 1 + alpha SMP["d_m"], 
         	SMP["Z_psi"] -> 1 + alpha SMP["d_psi"]}] & // Series[#, {alpha, 0, 1}] & // 
    	Normal // ReplaceAll[#, alpha -> 1] & // SelectNotFree2[#, SMP["Delta"], SMP["d_m"], 
    	SMP["d_psi"]] &
```

$$\frac{i \Delta  \left(C_A^2-1\right) \xi _{\text{G}} g_s^2 \left(C_A-2 C_F\right) \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p}{32 \pi ^2}+i C_A \delta _{\psi } \left(C_A-2 C_F\right) \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p$$

```mathematica
ampQuarkSEDiv[3] = ampQuarkSEDiv[2] // SUNSimplify // 
  	Collect2[#, DiracGamma, Factoring -> Simplify] &
```

$$\frac{i \left(C_A-2 C_F\right) \delta _{\text{Col1}\;\text{Col2}} \gamma \cdot p \left(32 \pi ^2 C_A \delta _{\psi }+\Delta  \left(C_A^2-1\right) \xi _{\text{G}} g_s^2\right)}{32 \pi ^2}$$

```mathematica
sol[1] = Solve[ampQuarkSEDiv[3] == 0, SMP["d_psi"]] // 
      	Flatten // ReplaceAll[#, Rule[a_, b_] :> Rule[a, SUNSimplify[b]]] & // 
    	ReplaceAll[#, SMP["g_s"]^2 -> 4 Pi SMP["alpha_s"]] &; 
 
solMS1 = sol[1] /. {
   	SMP["d_psi"] -> SMP["d_psi^MS"], SMP["Delta"] -> 1/Epsilon 
   }
solMSbar1 = sol[1] /. {
   	SMP["d_psi"] -> SMP["d_psi^MSbar"] 
   }
```

$$\left\{\delta _{\psi }^{\text{MS}}\to -\frac{C_F \xi _{\text{G}} \alpha _s}{4 \pi  \varepsilon }\right\}$$

$$\left\{\delta _{\psi }^{\overset{---}{\text{MS}}}\to -\frac{\Delta  C_F \xi _{\text{G}} \alpha _s}{4 \pi }\right\}$$

### Gluon self-energy

Tensor reduction allows us to express the gluon self-energy in tems of the Passarino-Veltman coefficient functions.

```mathematica
ampGluonSE[1] = (ampGluonSE[0][[1]] + Nf ampGluonSE[0][[2]] + 
      	Total[ampGluonSE[0][[3 ;;]]]) // SUNSimplify // DiracSimplify;
```

```mathematica
ampGluonSE[2] = TID[ampGluonSE[1], l, UsePaVeBasis -> True, ToPaVe -> True];
```

Discard all the finite pieces of the 1-loop amplitude

```mathematica
ampGluonSEDiv[0] = ampGluonSE[2] // PaVeUVPart[#, Prefactor -> 1/(2 Pi)^D] &
```

$$\frac{1}{(D-4) (D-1) \xi _{\text{G}} Z_{\xi }}i 2^{-D-1} \pi ^{-D} \left(4 C_A \pi ^2 p^{\mu } p^{\nu } g_s^2 Z_{\xi } \xi _{\text{G}}^3-C_A D \pi ^2 p^{\mu } p^{\nu } g_s^2 Z_{\xi } \xi _{\text{G}}^3-14 C_A \pi ^2 p^{\mu } p^{\nu } g_s^2 Z_{\xi } \xi _{\text{G}}^2+2 C_A D \pi ^2 p^{\mu } p^{\nu } g_s^2 Z_{\xi } \xi _{\text{G}}^2-2 C_A \pi ^2 g^{\mu \nu } p^2 g_s^2 Z_{\xi } \xi _{\text{G}}^2+2 C_A D \pi ^2 g^{\mu \nu } p^2 g_s^2 Z_{\xi } \xi _{\text{G}}^2+30 C_A \pi ^2 p^{\mu } p^{\nu } g_s^2 Z_{\xi } \xi _{\text{G}}-C_A D \pi ^2 p^{\mu } p^{\nu } g_s^2 Z_{\xi } \xi _{\text{G}}-8 N_f \pi ^2 p^{\mu } p^{\nu } g_s^2 Z_{\xi } \xi _{\text{G}}-18 C_A \pi ^2 g^{\mu \nu } p^2 g_s^2 Z_{\xi } \xi _{\text{G}}-2 C_A D \pi ^2 g^{\mu \nu } p^2 g_s^2 Z_{\xi } \xi _{\text{G}}+8 N_f \pi ^2 g^{\mu \nu } p^2 g_s^2 Z_{\xi } \xi _{\text{G}}-2^{D+3} \pi ^D p^{\mu } p^{\nu } Z_{\xi } \xi _{\text{G}}-2^{D+1} D^2 \pi ^D p^{\mu } p^{\nu } Z_{\xi } \xi _{\text{G}}+5\ 2^{D+1} D \pi ^D p^{\mu } p^{\nu } Z_{\xi } \xi _{\text{G}}+2^{D+3} \pi ^D g^{\mu \nu } p^2 Z_{\xi } \xi _{\text{G}}+2^{D+1} D^2 \pi ^D g^{\mu \nu } p^2 Z_{\xi } \xi _{\text{G}}-5\ 2^{D+1} D \pi ^D g^{\mu \nu } p^2 Z_{\xi } \xi _{\text{G}}+2^{D+3} \pi ^D p^{\mu } p^{\nu } Z_A Z_{\xi } \xi _{\text{G}}+2^{D+1} D^2 \pi ^D p^{\mu } p^{\nu } Z_A Z_{\xi } \xi _{\text{G}}-5\ 2^{D+1} D \pi ^D p^{\mu } p^{\nu } Z_A Z_{\xi } \xi _{\text{G}}-2^{D+3} \pi ^D g^{\mu \nu } p^2 Z_A Z_{\xi } \xi _{\text{G}}-2^{D+1} D^2 \pi ^D g^{\mu \nu } p^2 Z_A Z_{\xi } \xi _{\text{G}}+5\ 2^{D+1} D \pi ^D g^{\mu \nu } p^2 Z_A Z_{\xi } \xi _{\text{G}}-2^{D+3} \pi ^D p^{\mu } p^{\nu } Z_A-2^{D+1} D^2 \pi ^D p^{\mu } p^{\nu } Z_A+5\ 2^{D+1} D \pi ^D p^{\mu } p^{\nu } Z_A+2^{D+3} \pi ^D p^{\mu } p^{\nu } Z_{\xi }+2^{D+1} D^2 \pi ^D p^{\mu } p^{\nu } Z_{\xi }-5\ 2^{D+1} D \pi ^D p^{\mu } p^{\nu } Z_{\xi }\right) \delta ^{\text{Glu1}\;\text{Glu2}}$$

```mathematica
ampGluonSEDiv[1] = FCReplaceD[ampGluonSEDiv[0], D -> 4 - 2 Epsilon] // 
      	Series[#, {Epsilon, 0, 0}] & // 
     	Normal // FCHideEpsilon // SUNSimplify;
```

```mathematica
ampGluonSEDiv[2] = ampGluonSEDiv[1] // ReplaceRepeated[#, {
         	SMP["Z_A"] -> 1 + alpha SMP["d_A"], 
         	SMP["Z_xi"] -> 1 + alpha SMP["d_A"]}] & // Series[#, {alpha, 0, 1}] & // 
    	Normal // ReplaceAll[#, alpha -> 1] & // SelectNotFree2[#, SMP["Delta"], SMP["d_A"], 
    	SMP["d_xi"]] &
```

$$\frac{i \Delta  C_A \xi _{\text{G}} g_s^2 p^{\mu } p^{\nu } \delta ^{\text{Glu1}\;\text{Glu2}}}{32 \pi ^2}-\frac{i \Delta  p^2 C_A \xi _{\text{G}} g_s^2 g^{\mu \nu } \delta ^{\text{Glu1}\;\text{Glu2}}}{32 \pi ^2}-\frac{13 i \Delta  C_A g_s^2 p^{\mu } p^{\nu } \delta ^{\text{Glu1}\;\text{Glu2}}}{96 \pi ^2}+\frac{13 i \Delta  p^2 C_A g_s^2 g^{\mu \nu } \delta ^{\text{Glu1}\;\text{Glu2}}}{96 \pi ^2}-i p^2 \delta _A g^{\mu \nu } \delta ^{\text{Glu1}\;\text{Glu2}}+i \delta _A p^{\mu } p^{\nu } \delta ^{\text{Glu1}\;\text{Glu2}}+\frac{i \Delta  N_f g_s^2 p^{\mu } p^{\nu } \delta ^{\text{Glu1}\;\text{Glu2}}}{24 \pi ^2}-\frac{i \Delta  p^2 N_f g_s^2 g^{\mu \nu } \delta ^{\text{Glu1}\;\text{Glu2}}}{24 \pi ^2}$$

```mathematica
sol[3] = Solve[ampGluonSEDiv[2] == 0, SMP["d_A"]] // Flatten // 
   	ReplaceAll[#, SMP["g_s"]^2 -> 4 Pi SMP["alpha_s"]] & // Simplify
solMS2 = sol[3] /. {SMP["d_A"] -> SMP["d_A^MS"], SMP["Delta"] -> 1/Epsilon}
solMSbar2 = sol[3] /. {SMP["d_A"] -> SMP["d_A^MSbar"]}
```

$$\left\{\delta _A\to -\frac{\Delta  \alpha _s \left(3 C_A \xi _{\text{G}}-13 C_A+4 N_f\right)}{24 \pi }\right\}$$

$$\left\{\delta _A^{\text{MS}}\to -\frac{\alpha _s \left(3 C_A \xi _{\text{G}}-13 C_A+4 N_f\right)}{24 \pi  \varepsilon }\right\}$$

$$\left\{\delta _A^{\overset{---}{\text{MS}}}\to -\frac{\Delta  \alpha _s \left(3 C_A \xi _{\text{G}}-13 C_A+4 N_f\right)}{24 \pi }\right\}$$

### Ghost self-energy

Tensor reduction allows us to express the ghost self-energy in tems of the Passarino-Veltman coefficient functions.

```mathematica
ampGhostSE[1] = ampGhostSE[0] // SUNSimplify // DiracSimplify;
```

```mathematica
ampGhostSE[2] = TID[ampGhostSE[1], l, UsePaVeBasis -> True, ToPaVe -> True];
```

Discard all the finite pieces of the 1-loop amplitude

```mathematica
ampGhostSEDiv[0] = ampGhostSE[2] // PaVeUVPart[#, Prefactor -> 1/(2 Pi)^D] &
```

$$\frac{i 2^{-D-1} \pi ^{-D} p^2 \delta ^{\text{Glu1}\;\text{Glu2}} \left(\pi ^2 \left(-C_A\right) \xi _{\text{G}} g_s^2+3 \pi ^2 C_A g_s^2-2^{D+3} \pi ^D Z_u+2^{D+1} D \pi ^D Z_u+2^{D+3} \pi ^D-2^{D+1} D \pi ^D\right)}{D-4}$$

```mathematica
ampGhostSEDiv[1] = FCReplaceD[ampGhostSEDiv[0], D -> 4 - 2 Epsilon] // 
     	Series[#, {Epsilon, 0, 0}] & // 
    	Normal // FCHideEpsilon // SUNSimplify
```

$$-\frac{1}{64 \pi ^2}i p^2 \delta ^{\text{Glu1}\;\text{Glu2}} \left(-\Delta  C_A \xi _{\text{G}} g_s^2+\gamma  \left(-C_A\right) \xi _{\text{G}} g_s^2+\log (4 \pi ) C_A \xi _{\text{G}} g_s^2-2 \log (\pi ) C_A \xi _{\text{G}} g_s^2-2 \log (2) C_A \xi _{\text{G}} g_s^2+3 \Delta  C_A g_s^2+3 \gamma  C_A g_s^2-3 \log (4 \pi ) C_A g_s^2+6 \log (\pi ) C_A g_s^2+6 \log (2) C_A g_s^2-64 \pi ^2 Z_u+64 \pi ^2\right)$$

```mathematica
ampGhostSEDiv[2] = ampGhostSEDiv[1] // ReplaceRepeated[#, {
          	SMP["Z_u"] -> 1 + alpha SMP["d_u"]}] & // Series[#, {alpha, 0, 1}] & // 
     	Normal // ReplaceAll[#, alpha -> 1] & // 
   	SelectNotFree2[#, SMP["Delta"], SMP["d_u"]] & // Simplify
```

$$\frac{i p^2 \delta ^{\text{Glu1}\;\text{Glu2}} \left(\Delta  C_A \left(\xi _{\text{G}}-3\right) g_s^2+64 \pi ^2 \delta _u\right)}{64 \pi ^2}$$

```mathematica
sol[4] = Solve[ampGhostSEDiv[2] == 0, SMP["d_u"]] // Flatten // 
   	ReplaceAll[#, SMP["g_s"]^2 -> 4 Pi SMP["alpha_s"]] & // Simplify
solMS3 = sol[4] /. {SMP["d_u"] -> SMP["d_u^MS"], SMP["Delta"] -> 1/Epsilon}
solMSbar3 = sol[4] /. {SMP["d_u"] -> SMP["d_u^MSbar"]}
```

$$\left\{\delta _u\to -\frac{\Delta  C_A \left(\xi _{\text{G}}-3\right) \alpha _s}{16 \pi }\right\}$$

$$\left\{\delta _u^{\text{MS}}\to -\frac{C_A \left(\xi _{\text{G}}-3\right) \alpha _s}{16 \pi  \varepsilon }\right\}$$

$$\left\{\delta _u^{\overset{---}{\text{MS}}}\to -\frac{\Delta  C_A \left(\xi _{\text{G}}-3\right) \alpha _s}{16 \pi }\right\}$$

### Quark-gluon vertex

Tensor reduction allows us to express the quark-gluon vertex in tems of the Passarino-Veltman coefficient functions.

```mathematica
ampQGlVertex[1] = ampQGlVertex[0] // SUNSimplify // DiracSimplify;
```

```mathematica
ampQGlVertex[2] = TID[ampQGlVertex[1], l, UsePaVeBasis -> True, ToPaVe -> True];
```

Discard all the finite pieces of the 1-loop amplitude

```mathematica
ampQGlVertexDiv[0] = ampQGlVertex[2] // PaVeUVPart[#, Prefactor -> 1/(2 Pi)^D] &
```

$$-\frac{1}{D-4}i (2 \pi )^{-D} \left(-2^{D+3} \pi ^D C_A \gamma ^{\mu } C_F g_s T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}+2^{D+1} D \pi ^D C_A \gamma ^{\mu } C_F g_s T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}+2^{D+3} \pi ^D C_A \sqrt{Z_A} \gamma ^{\mu } C_F g_s Z_g Z_{\psi } T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}-2^{D+1} D \pi ^D C_A \sqrt{Z_A} \gamma ^{\mu } C_F g_s Z_g Z_{\psi } T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}+2^{D+2} \pi ^D C_A^2 \gamma ^{\mu } g_s T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}-D (2 \pi )^D C_A^2 \gamma ^{\mu } g_s T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}-2^{D+2} \pi ^D C_A^2 \sqrt{Z_A} \gamma ^{\mu } g_s Z_g Z_{\psi } T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}+D (2 \pi )^D C_A^2 \sqrt{Z_A} \gamma ^{\mu } g_s Z_g Z_{\psi } T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}+\pi ^2 C_A \gamma ^{\mu } \xi _{\text{G}} g_s^3 T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}-2 \pi ^2 \gamma ^{\mu } C_F \xi _{\text{G}} g_s^3 T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}-3 i \pi ^2 \gamma ^{\mu } \xi _{\text{G}} g_s^3 f^{\text{Glu2}\;\text{FCGV}(\text{sun12181})\text{FCGV}(\text{sun12182})} \left(T^{\text{FCGV}(\text{sun12182})}T^{\text{FCGV}(\text{sun12181})}\right){}_{\text{Col3}\;\text{Col1}}-3 i \pi ^2 \gamma ^{\mu } g_s^3 f^{\text{Glu2}\;\text{FCGV}(\text{sun12181})\text{FCGV}(\text{sun12182})} \left(T^{\text{FCGV}(\text{sun12182})}T^{\text{FCGV}(\text{sun12181})}\right){}_{\text{Col3}\;\text{Col1}}\right)$$

```mathematica
ampQGlVertexDiv[1] = FCReplaceD[ampQGlVertexDiv[0], D -> 4 - 2 Epsilon] // 
      	Series[#, {Epsilon, 0, 0}] & // 
     	Normal // FCHideEpsilon // SUNSimplify;
```

```mathematica
ampQGlVertexDiv[2] = ampQGlVertexDiv[1] // ReplaceRepeated[#, {
          	SMP["Z_g"] -> 1 + alpha SMP["d_g"], 
          	SMP["Z_A"] -> 1 + alpha SMP["d_A"], 
          	SMP["Z_psi"] -> 1 + alpha SMP["d_psi"] 
          	}] & // Series[#, {alpha, 0, 1}] & // 
     	Normal // ReplaceAll[#, alpha -> 1] & // 
   	SelectNotFree2[#, SMP["Delta"], SMP["d_g"], SMP["d_A"], SMP["d_psi"]] & // Simplify
```

$$-\frac{1}{32 \pi ^2}i \gamma ^{\mu } g_s \left(32 \pi ^2 C_A \delta _g \left(C_A-2 C_F\right) T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}-64 \pi ^2 C_A C_F \delta _{\psi } T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}+16 \pi ^2 C_A \delta _A \left(C_A-2 C_F\right) T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}-\Delta  C_A \xi _{\text{G}} g_s^2 T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}+32 \pi ^2 C_A^2 \delta _{\psi } T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}+2 \Delta  C_F \xi _{\text{G}} g_s^2 T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}}+3 i \Delta  \xi _{\text{G}} g_s^2 f^{\text{Glu2}\;\text{FCGV}(\text{sun59231})\text{FCGV}(\text{sun59232})} \left(T^{\text{FCGV}(\text{sun59232})}T^{\text{FCGV}(\text{sun59231})}\right){}_{\text{Col3}\;\text{Col1}}+3 i \Delta  g_s^2 f^{\text{Glu2}\;\text{FCGV}(\text{sun59231})\text{FCGV}(\text{sun59232})} \left(T^{\text{FCGV}(\text{sun59232})}T^{\text{FCGV}(\text{sun59231})}\right){}_{\text{Col3}\;\text{Col1}}\right)$$

```mathematica
ampQGlVertexDiv[3] = ampQGlVertexDiv[2] // SUNSimplify[#, Explicit -> True] & // 
   	ReplaceAll[#, SUNTrace[x__] :> SUNTrace[x, Explicit -> True]] & //
  	Collect2[#, Epsilon, SUNIndex] &
```

$$\frac{3 \Delta  \gamma ^{\mu } \left(\xi _{\text{G}}+1\right) g_s^3 f^{\text{Glu2}\;\text{FCGV}(\text{sun59411})\text{FCGV}(\text{sun59412})} \left(T^{\text{FCGV}(\text{sun59412})}T^{\text{FCGV}(\text{sun59411})}\right){}_{\text{Col3}\;\text{Col1}}}{32 \pi ^2}-\frac{i \gamma ^{\mu } g_s \left(C_A-2 C_F\right) T_{\text{Col3}\;\text{Col1}}^{\text{Glu2}} \left(32 \pi ^2 C_A \delta _{\psi }+16 \pi ^2 C_A \delta _A+32 \pi ^2 C_A \delta _g+\Delta  \left(-\xi _{\text{G}}\right) g_s^2\right)}{32 \pi ^2}$$

```mathematica
ampQGlVertexDiv[4] = ampQGlVertexDiv[3] // 
      	ReplaceAll[#, suntf[xx_, _SUNFIndex, _SUNFIndex] :> SUNT @@ xx] & // 
     	ReplaceAll[#, SUNTF -> suntf] & // ReplaceAll[#, 
      	suntf[xx_, _SUNFIndex, _SUNFIndex] :> SUNT @@ xx] & // 
   	SUNSimplify // Collect2[#, SMP] &
```

$$-\frac{i \Delta  \gamma ^{\mu } g_s^3 T^{\text{Glu2}} \left(C_A-2 C_F\right) \left(3 C_A^2 \xi _{\text{G}}+3 C_A^2-2 \xi _{\text{G}}\right)}{64 \pi ^2}-i C_A \gamma ^{\mu } \delta _{\psi } g_s T^{\text{Glu2}} \left(C_A-2 C_F\right)-\frac{1}{2} i C_A \delta _A \gamma ^{\mu } g_s T^{\text{Glu2}} \left(C_A-2 C_F\right)-i C_A \gamma ^{\mu } \delta _g g_s T^{\text{Glu2}} \left(C_A-2 C_F\right)$$

```mathematica
ampQGlVertexDiv[5] = (ampQGlVertexDiv[4] /. {
         	SMP["d_A"] -> SMP["d_A^MS"], 
         	SMP["d_psi"] -> SMP["d_psi^MS"], 
         	SMP["d_g"] -> SMP["d_g"], 
         	SMP["Delta"] -> 1/Epsilon 
         } /. solMS1 /. solMS2) // ReplaceAll[#, SMP["g_s"]^3 -> 4 Pi SMP["alpha_s"] SMP["g_s"]] & //
    Collect2[#, Epsilon] & // SUNSimplify
```

$$\frac{i \gamma ^{\mu } g_s T^{\text{Glu2}} \left(-11 C_A \alpha _s+2 N_f \alpha _s-24 \pi  \varepsilon  \delta _g\right)}{24 \pi  \varepsilon }$$

```mathematica
sol[5] = Solve[ampQGlVertexDiv[5] == 0, SMP["d_g"]] // Flatten // Simplify
solMS4 = sol[5] /. {SMP["d_g"] -> SMP["d_g^MS"]}
solMSbar4 = sol[5] /. {SMP["d_g"] -> SMP["d_g^MSbar"], 1/Epsilon -> SMP["Delta"]}
```

$$\left\{\delta _g\to \frac{\alpha _s \left(2 N_f-11 C_A\right)}{24 \pi  \varepsilon }\right\}$$

$$\left\{\delta _g^{\text{MS}}\to \frac{\alpha _s \left(2 N_f-11 C_A\right)}{24 \pi  \varepsilon }\right\}$$

$$\left\{\delta _g^{\overset{---}{\text{MS}}}\to \frac{\Delta  \alpha _s \left(2 N_f-11 C_A\right)}{24 \pi }\right\}$$

## Check the final results

```mathematica
knownResult = {
    	SMP["d_psi^MS"] -> -SMP["alpha_s"]/(4 Pi) 1/Epsilon CF GaugeXi["G"],
    	SMP["d_psi^MSbar"] -> -SMP["alpha_s"]/(4 Pi) SMP["Delta"] CF GaugeXi["G"], 
    	SMP["d_A^MS"] -> SMP["alpha_s"]/(4 Pi) 1/Epsilon (1/2 CA (13/3 - GaugeXi["G"]) - 2/3 Nf), 
    	SMP["d_A^MSbar"] -> SMP["alpha_s"]/(4 Pi) SMP["Delta"] (1/2 CA (13/3 - GaugeXi["G"]) - 2/3 Nf), 
    	SMP["d_u^MS"] -> SMP["alpha_s"]/(4 Pi) CA/Epsilon (3 - GaugeXi["G"])/4, 
    	SMP["d_u^MSbar"] -> SMP["alpha_s"]/(4 Pi) CA SMP["Delta"] (3 - GaugeXi["G"])/4, 
    	SMP["d_g^MS"] -> ((-11*CA*SMP["alpha_s"])/(24*Epsilon*Pi) + (Nf*SMP["alpha_s"])/(12*Epsilon*Pi)), 
    	SMP["d_g^MSbar"] -> ((-11*CA*SMP["alpha_s"])/(24 Pi) SMP["Delta"] + (Nf*SMP["alpha_s"])/(12*Pi) SMP["Delta"]) 
    } // Factor2;
FCCompareResults[Join[solMS1, solMSbar1, solMS2, solMSbar2, solMS3, solMSbar3, solMS4,solMSbar4] // Factor2, knownResult, 
  Text -> {"\tCompare to Muta, Foundations of QCD, Eqs 2.5.131-2.5.147:", 
    "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Muta, Foundations of QCD, Eqs 2.5.131-2.5.147:} \;\text{CORRECT.}$$

$$\text{True}$$

$$\text{$\backslash $tCPU Time used: }41.781\text{ s.}$$