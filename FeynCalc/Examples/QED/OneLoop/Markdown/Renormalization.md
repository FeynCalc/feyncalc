---
title: 1-loop QED renormalization in the minimal subtraction schemes
---


## Load FeynCalc and the necessary add-ons or other packages

This example uses a custom QED model created with FeynRules. Please evaluate the file
FeynCalc/Examples/FeynRules/QED/GenerateModelQED.m before running it for the first time.

```mathematica
description = "Renormalization, QED, MS and MSbar, 1-loop";
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
```

```mathematica
params = {InsertionLevel -> {Particles}, Model -> FileNameJoin[{"QED", "QED"}], 
    GenericModel -> FileNameJoin[{"QED", "QED"}], ExcludeParticles -> {F[2, {2 | 3}]}};
top[i_, j_] := CreateTopologies[1, i -> j, 
    ExcludeTopologies -> {Tadpoles, WFCorrections, WFCorrectionCTs}];
topCT[i_, j_] := CreateCTTopologies[1, i -> j, 
     ExcludeTopologies -> {Tadpoles, WFCorrections, WFCorrectionCTs}]; 
 
{diagElectronSE, diagElectronSECT} = InsertFields[#, {F[2, {1}]} -> {F[2, {1}]}, 
      Sequence @@ params] & /@ {top[1, 1], topCT[1, 1]};
{diagPhotonSE, diagPhotonSECT} = InsertFields[#, {V[1]} -> {V[1]}, 
      Sequence @@ params] & /@ {top[1, 1], topCT[1, 1]};
{diagVertex, diagVertexCT} = InsertFields[#,  {F[2, {1}], V[1]} -> {F[2, {1}]}, 
      Sequence @@ params] & /@ {top[2, 1], topCT[2, 1]};
```

```mathematica
diag1[0] = diagElectronSE[[0]][diagElectronSE[[1]], diagElectronSECT[[1]]];
diag2[0] = diagPhotonSE[[0]][diagPhotonSE[[1]], diagPhotonSECT[[1]]];
diag3[0] = diagVertex[[0]][diagVertex[[1]], diagVertexCT[[1]]];
```

```mathematica
Paint[diag1[0], ColumnsXRows -> {2, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> {512, 256}];
```

![0fg397uh5r1xi](img/0fg397uh5r1xi.svg)

```mathematica
Paint[diag2[0], ColumnsXRows -> {2, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> {512, 256}];
```

![0ep70pqwe885a](img/0ep70pqwe885a.svg)

```mathematica
Paint[diag3[0], ColumnsXRows -> {2, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> {512, 256}];
```

![1azkfgrquzf2w](img/1azkfgrquzf2w.svg)

## Obtain the amplitudes

The 1/(2Pi)^D prefactor is implicit. We need to replace e with -e to be compatible
with the convention D^mu = d^mu + ie A^mu

Electron self-energy including the counter-term

```mathematica
amp1[0] = FCFAConvert[CreateFeynAmp[diag1[0], Truncated -> True, 
   	GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p}, OutgoingMomenta -> {p}, 
  	LorentzIndexNames -> {mu}, 
  	LoopMomenta -> {l}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> False, SMP -> True, 
  	FinalSubstitutions -> {Zm -> SMP["Z_m"], Zpsi -> SMP["Z_psi"], 
    	SMP["e"] -> Sqrt[4 Pi SMP["alpha_fs"]], GaugeXi[V[1]] -> GaugeXi}, 
  	Contract -> True]
```

$$-\frac{4 \pi  \alpha  \gamma ^{\mu }.\left(m_e+\gamma \cdot l\right).\gamma ^{\mu }}{\left(l^2-m_e^2\right).(l-p)^2}-\frac{4 \pi  \alpha  (1-\xi ) (\gamma \cdot (p-l)).\left(m_e+\gamma \cdot l\right).(\gamma \cdot (l-p))}{\left(l^2-m_e^2\right).(l-p)^4}-i m_e \left(Z_m Z_{\psi }-1\right)+i \left(Z_{\psi }-1\right) \gamma \cdot p$$

Photon self-energy including the counter-term

```mathematica
amp2[0] = FCFAConvert[CreateFeynAmp[diag2[0], Truncated -> True, 
    	GaugeRules -> {}, PreFactor -> 1], 
   	IncomingMomenta -> {p}, OutgoingMomenta -> {p}, 
   	LorentzIndexNames -> {mu, nu}, 
   	LoopMomenta -> {l}, UndoChiralSplittings -> True, 
   	ChangeDimension -> D, List -> False, SMP -> True, 
   	FinalSubstitutions -> {ZA -> SMP["Z_A"], Zxi -> SMP["Z_xi"], 
     	SMP["e"] -> Sqrt[4 Pi SMP["alpha_fs"]], GaugeXi[V[1]] -> GaugeXi}, 
   	Contract -> True] // FCTraceFactor
```

$$-\frac{4 \pi  \alpha  \;\text{tr}\left(\left(m_e-\gamma \cdot l\right).\gamma ^{\nu }.\left(m_e+\gamma \cdot (p-l)\right).\gamma ^{\mu }\right)}{\left(l^2-m_e^2\right).\left((l-p)^2-m_e^2\right)}-i p^2 \left(Z_A-1\right) g^{\mu \nu }-\frac{i p^{\mu } p^{\nu } \left(Z_A-Z_{\xi }\right)}{\xi  Z_{\xi }}+i \left(Z_A-1\right) p^{\mu } p^{\nu }$$

Electron-photon vertex including the counter-term

```mathematica
amp3[0] = FCFAConvert[CreateFeynAmp[diag3[0], Truncated -> True, 
    	GaugeRules -> {}, PreFactor -> 1], 
   	IncomingMomenta -> {p1, k}, OutgoingMomenta -> {p2}, 
   	LorentzIndexNames -> {mu}, LoopMomenta -> {l}, 
   	UndoChiralSplittings -> True, ChangeDimension -> D, 
   	List -> False, SMP -> True, FinalSubstitutions -> 
    	{ZA -> SMP["Z_A"], Ze -> SMP["Z_e"], Zpsi -> SMP["Z_psi"], 
     	SMP["e"]^3 -> 4 Pi SMP["alpha_fs"] SMP["e"], GaugeXi[V[1]] -> GaugeXi}, 
   	Contract -> True] /. SMP["e"] -> -SMP["e"]
```

$$-\frac{4 \pi  \alpha  \;\text{e} \gamma ^{\text{Lor3}}.\left(m_e+\gamma \cdot (k+l)\right).\gamma ^{\mu }.\left(m_e+\gamma \cdot l\right).\gamma ^{\text{Lor3}}}{\left(l^2-m_e^2\right).\left((k+l)^2-m_e^2\right).(k+l-\text{p2})^2}-\frac{4 \pi  \alpha  \;\text{e} (1-\xi ) (\gamma \cdot (-k-l+\text{p2})).\left(m_e+\gamma \cdot (k+l)\right).\gamma ^{\mu }.\left(m_e+\gamma \cdot l\right).(\gamma \cdot (k+l-\text{p2}))}{\left(l^2-m_e^2\right).\left((k+l)^2-m_e^2\right).(k+l-\text{p2})^4}-i \;\text{e} \gamma ^{\mu } \left(\sqrt{Z_A} Z_e Z_{\psi }-1\right)$$

## Calculate the amplitudes

### Electron self-energy

```mathematica
amp1[1] = amp1[0] // ReplaceAll[#, {SMP["Z_psi"] -> 1 + alpha SMP["d_psi"], 
         SMP["Z_m"] -> 1 + alpha SMP["d_m"]}] & // Series[#, {alpha, 0, 1}] & // 
    Normal // ReplaceAll[#, alpha -> 1] &
```

$$-\frac{4 \pi  \alpha  \gamma ^{\mu }.\left(m_e+\gamma \cdot l\right).\gamma ^{\mu }}{\left(l^2-m_e^2\right).(l-p)^2}-\frac{4 \pi  \alpha  (1-\xi ) (\gamma \cdot (p-l)).\left(m_e+\gamma \cdot l\right).(\gamma \cdot (l-p))}{\left(l^2-m_e^2\right).(l-p)^4}-i m_e \left(\delta _{\psi }+\delta _m\right)+i \delta _{\psi } \gamma \cdot p$$

Tensor reduction allows us to express the electron self-energy in tems of the Passarino-Veltman coefficient functions.

```mathematica
amp1[2] = TID[amp1[1], l, ToPaVe -> True]
```

$$\frac{1}{p^2}2 i \pi ^3 \alpha  \;\text{B}_0\left(p^2,0,m_e^2\right) \left(-D \left(p^2-m_e^2\right) \gamma \cdot p-2 D p^2 m_e+2 D p^2 \gamma \cdot p+\xi  m_e^2 \gamma \cdot p-2 \xi  m_e (\gamma \cdot p).(\gamma \cdot p)-m_e^2 \gamma \cdot p+2 m_e (\gamma \cdot p).(\gamma \cdot p)+2 \left(p^2-m_e^2\right) \gamma \cdot p+\xi  p^2 \gamma \cdot p-5 p^2 \gamma \cdot p\right)-\frac{2 i \pi ^3 \alpha  (1-\xi ) \;\text{B}_0(0,0,0) \left(m_e^2 (-(\gamma \cdot p))+2 m_e (\gamma \cdot p).(\gamma \cdot p)-2 p^2 m_e+p^2 \gamma \cdot p\right)}{p^2}+\frac{2 i \pi ^3 \alpha  (1-\xi ) \left(-m_e^2 \left(p^2-m_e^2\right) \gamma \cdot p-4 p^2 m_e (\gamma \cdot p).(\gamma \cdot p)+2 m_e \left(p^2-m_e^2\right) (\gamma \cdot p).(\gamma \cdot p)+p^2 \left(p^2-m_e^2\right) \gamma \cdot p+2 p^2 m_e^3+2 p^4 m_e\right) \;\text{C}_0\left(0,p^2,p^2,0,0,m_e^2\right)}{p^2}+\frac{2 i \pi ^3 \alpha  (2-D) \gamma \cdot p \;\text{A}_0\left(m_e^2\right)}{p^2}+i \left(-\delta _{\psi } m_e-m_e \delta _m+\delta _{\psi } \gamma \cdot p\right)$$

Discard all the finite pieces of the 1-loop amplitude

```mathematica
amp1Div[0] = PaVeUVPart[amp1[2], Prefactor -> 1/(2 Pi)^D] // 
        FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
     FCHideEpsilon // SelectNotFree2[#, {SMP["Delta"], SMP["d_m"], 
       SMP["d_psi"]}] & // Simplify
```

$$\frac{i \left(\gamma \cdot p \left(\alpha  \Delta  \xi +4 \pi  \delta _{\psi }\right)-m_e \left(\alpha  \Delta  (\xi +3)+4 \pi  \left(\delta _{\psi }+\delta _m\right)\right)\right)}{4 \pi }$$

Equating the result to zero and solving for d_psi and d_m we obtain  the renormalization constants in 
the minimal subtraction schemes.

```mathematica
sol[1] = Solve[SelectNotFree2[amp1Div[0], DiracGamma] == 0, 
     	SMP["d_psi"]] // Flatten // Simplify;
sol[2] = Solve[(SelectFree2[amp1Div[0], DiracGamma] == 0) /. sol[1], 
     	SMP["d_m"]] // Flatten // Simplify;
solMS1 = Join[sol[1], sol[2]] /. {
   	SMP["d_psi"] -> SMP["d_psi^MS"], 
   	SMP["d_m"] -> SMP["d_m^MS"], SMP["Delta"] -> 1/Epsilon 
   }
solMSbar1 = Join[sol[1], sol[2]] /. {
   	SMP["d_psi"] -> SMP["d_psi^MSbar"], 
   	SMP["d_m"] -> SMP["d_m^MSbar"] 
   }
```

$$\left\{\delta _{\psi }^{\text{MS}}\to -\frac{\alpha  \xi }{4 \pi  \varepsilon },\delta _m^{\text{MS}}\to -\frac{3 \alpha }{4 \pi  \varepsilon }\right\}$$

$$\left\{\delta _{\psi }^{\overset{---}{\text{MS}}}\to -\frac{\alpha  \Delta  \xi }{4 \pi },\delta _m^{\overset{---}{\text{MS}}}\to -\frac{3 \alpha  \Delta }{4 \pi }\right\}$$

### Photon self-energy

```mathematica
amp2[1] = amp2[0] // ReplaceRepeated[#, {SMP["Z_xi"] -> SMP["Z_A"], 
         SMP["Z_A"] -> 1 + alpha SMP["d_A"]}] & // Series[#, {alpha, 0, 1}] & // 
    Normal // ReplaceAll[#, alpha -> 1] &
```

$$-\frac{4 \pi  \alpha  \;\text{tr}\left(\left(m_e-\gamma \cdot l\right).\gamma ^{\nu }.\left(m_e+\gamma \cdot (p-l)\right).\gamma ^{\mu }\right)}{\left(l^2-m_e^2\right).\left((l-p)^2-m_e^2\right)}+i \left(\delta _A p^{\mu } p^{\nu }-p^2 \delta _A g^{\mu \nu }\right)$$

Tensor reduction allows us to express the electron self-energy in tems of the Passarino-Veltman coefficient functions.

```mathematica
amp2[2] = TID[amp2[1], l, ToPaVe -> True]
```

$$\frac{8 i \pi ^3 \alpha  \;\text{B}_0\left(p^2,m_e^2,m_e^2\right) \left(-\left((1-D) p^4 g^{\mu \nu }\right)+2 (1-D) p^2 p^{\mu } p^{\nu }+D p^2 p^{\mu } p^{\nu }+4 p^2 m_e^2 g^{\mu \nu }-4 m_e^2 p^{\mu } p^{\nu }-p^4 g^{\mu \nu }\right)}{(1-D) p^2}-\frac{16 i \pi ^3 \alpha  \;\text{A}_0\left(m_e^2\right) \left(-(1-D) p^2 g^{\mu \nu }-D p^{\mu } p^{\nu }-p^2 g^{\mu \nu }+2 p^{\mu } p^{\nu }\right)}{(1-D) p^2}+i \delta _A \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)$$

Discard all the finite pieces of the 1-loop amplitude

```mathematica
amp2Div[0] = PaVeUVPart[amp2[2], Prefactor -> 1/(2 Pi)^D] // 
        FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
     FCHideEpsilon // SelectNotFree2[#, {SMP["Delta"], SMP["d_A"]}] & // Simplify
```

$$\frac{i \left(\alpha  \Delta +3 \pi  \delta _A\right) \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)}{3 \pi }$$

Equating this to zero and solving for d_A we obtain the wave-function renormalization constant for the photon in the minimal subtraction schemes.

```mathematica
sol[3] = Solve[amp2Div[0] == 0, SMP["d_A"]] // Flatten;
solMS2 = sol[3] /. {SMP["d_A"] -> SMP["d_A^MS"], SMP["Delta"] -> 1/Epsilon}
solMSbar2 = sol[3] /. {SMP["d_A"] -> SMP["d_A^MSbar"]}
```

$$\left\{\delta _A^{\text{MS}}\to -\frac{\alpha }{3 \pi  \varepsilon }\right\}$$

$$\left\{\delta _A^{\overset{---}{\text{MS}}}\to -\frac{\alpha  \Delta }{3 \pi }\right\}$$

### Electron-photon vertex

```mathematica
amp3[1] = amp3[0] // ReplaceRepeated[#, {SMP["Z_psi"] -> 1 + alpha SMP["d_psi"], 
         SMP["Z_A"] -> 1 + alpha SMP["d_A"], SMP["Z_e"] -> 1 + alpha SMP["d_e"]}] & // 
     Series[#, {alpha, 0, 1}] & // Normal // ReplaceAll[#, alpha -> 1] &
```

$$-\frac{4 \pi  \alpha  \;\text{e} \gamma ^{\text{Lor3}}.\left(m_e+\gamma \cdot (k+l)\right).\gamma ^{\mu }.\left(m_e+\gamma \cdot l\right).\gamma ^{\text{Lor3}}}{\left(l^2-m_e^2\right).\left((k+l)^2-m_e^2\right).(k+l-\text{p2})^2}-\frac{4 \pi  \alpha  \;\text{e} (1-\xi ) (\gamma \cdot (-k-l+\text{p2})).\left(m_e+\gamma \cdot (k+l)\right).\gamma ^{\mu }.\left(m_e+\gamma \cdot l\right).(\gamma \cdot (k+l-\text{p2}))}{\left(l^2-m_e^2\right).\left((k+l)^2-m_e^2\right).(k+l-\text{p2})^4}-i \;\text{e} \gamma ^{\mu } \left(\frac{\delta _A}{2}+\delta _{\psi }+\delta _e\right)$$

The result of the tensor reduction is quite large, since we keep the full gauge dependence and do not specify the kinematics

```mathematica
amp3[2] = TID[amp3[1], l, ToPaVe -> True, UsePaVeBasis -> True]
```

$$8 i (1-\xi ) \pi ^3 \;\text{B}_0(0,0,0) \gamma ^{\mu } \alpha  \;\text{e}+4 i (2-D) \pi ^3 \;\text{B}_0\left(\text{p2}^2,0,m_e^2\right) \gamma ^{\mu } \alpha  \;\text{e}-4 i D (1-\xi ) \pi ^3 \gamma ^{\mu } \;\text{C}_{00}\left(0,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,0,0,m_e^2\right) \alpha  \;\text{e}-8 i (2-D) \pi ^3 \gamma ^{\mu } \;\text{C}_{00}\left(k^2,\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,m_e^2,m_e^2,0\right) \alpha  \;\text{e}-8 i (2-D) \pi ^3 \gamma \cdot k k^{\mu } \;\text{C}_{11}\left(k^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,\text{p2}^2,m_e^2,m_e^2,0\right) \alpha  \;\text{e}-8 i (2-D) \pi ^3 \gamma \cdot \;\text{p2} \;\text{p2}^{\mu } \;\text{C}_{11}\left(\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,k^2,m_e^2,0,m_e^2\right) \alpha  \;\text{e}-4 i (1-\xi ) \pi ^3 \gamma ^{\mu } \left(k^2-2 (k\cdot \;\text{p2})+\text{p2}^2\right) \;\text{C}_{11}\left(k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,0,0,m_e^2,0\right) \alpha  \;\text{e}-8 i (2-D) \pi ^3 \left(\gamma \cdot \;\text{p2} k^{\mu }+\gamma \cdot k \;\text{p2}^{\mu }\right) \;\text{C}_{12}\left(k^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,\text{p2}^2,m_e^2,m_e^2,0\right) \alpha  \;\text{e}-\frac{1}{2} i \gamma ^{\mu } \left(\delta _A+2 \delta _e+2 \delta _{\psi }\right) \;\text{e}+4 i (1-\xi ) \pi ^3 \;\text{C}_1\left(k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,0,0,m_e^2,0\right) \alpha  \left(\gamma ^{\mu }.(-(\gamma \cdot (k-\text{p2}))).(\gamma \cdot k)-\gamma ^{\mu }.(-(\gamma \cdot (k-\text{p2}))).(\gamma \cdot \;\text{p2})-2 \gamma \cdot \;\text{p2} k^{\mu }+2 \gamma \cdot \;\text{p2} \;\text{p2}^{\mu }-2 \gamma ^{\mu } k^2+4 \gamma ^{\mu } (k\cdot \;\text{p2})-2 \gamma ^{\mu } \;\text{p2}^2+\gamma ^{\mu }.(-(\gamma \cdot (k-\text{p2}))) m_e\right) \;\text{e}+4 i (1-\xi ) \pi ^3 \;\text{C}_1\left(\text{p2}^2,\text{p2}^2,0,0,m_e^2,0\right) \alpha  \left((\gamma \cdot k).(\gamma \cdot \;\text{p2}).\gamma ^{\mu }+(\gamma \cdot \;\text{p2}).(\gamma \cdot k).\gamma ^{\mu }-(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}).\gamma ^{\mu }-2 \gamma ^{\mu } (k\cdot \;\text{p2})+(\gamma \cdot \;\text{p2}).\gamma ^{\mu } m_e\right) \;\text{e}+4 i \pi ^3 \;\text{C}_0\left(k^2,\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,m_e^2,m_e^2,0\right) \alpha  \left(-D (\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k)+2 (\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k)+\xi  (\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2})-(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2})+2 D \gamma \cdot k k^{\mu }-4 \gamma \cdot k k^{\mu }-2 \xi  \gamma \cdot \;\text{p2} k^{\mu }+2 \gamma \cdot \;\text{p2} k^{\mu }+D \gamma ^{\mu }.(\gamma \cdot k) m_e-4 \gamma ^{\mu }.(\gamma \cdot k) m_e+4 k^{\mu } m_e\right) \;\text{e}+4 i \pi ^3 \;\text{C}_1\left(k^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,\text{p2}^2,m_e^2,m_e^2,0\right) \alpha  \left(-D (\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k)+2 (\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k)+4 D \gamma \cdot k k^{\mu }-8 \gamma \cdot k k^{\mu }-2 \xi  \gamma \cdot \;\text{p2} k^{\mu }+2 \gamma \cdot \;\text{p2} k^{\mu }+D \gamma ^{\mu }.(\gamma \cdot k) m_e-4 \gamma ^{\mu }.(\gamma \cdot k) m_e+D (\gamma \cdot k).\gamma ^{\mu } m_e-4 (\gamma \cdot k).\gamma ^{\mu } m_e+8 k^{\mu } m_e\right) \;\text{e}+4 i \pi ^3 \;\text{C}_1\left(\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,k^2,m_e^2,0,m_e^2\right) \alpha  \left(-D (\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2})+4 (\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2})-2 (\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot k)+2 D \gamma \cdot \;\text{p2} k^{\mu }-4 \gamma \cdot \;\text{p2} k^{\mu }+2 D \gamma \cdot k \;\text{p2}^{\mu }-4 \gamma \cdot k \;\text{p2}^{\mu }-2 \xi  \gamma \cdot \;\text{p2} \;\text{p2}^{\mu }+2 \gamma \cdot \;\text{p2} \;\text{p2}^{\mu }+D \gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e-4 \gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e+D (\gamma \cdot \;\text{p2}).\gamma ^{\mu } m_e-4 (\gamma \cdot \;\text{p2}).\gamma ^{\mu } m_e+8 \;\text{p2}^{\mu } m_e\right) \;\text{e}-4 i (1-\xi ) \pi ^3 \;\text{C}_0\left(0,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,0,0,m_e^2\right) \alpha  \left(-\gamma ^{\mu } m_e^2+\gamma ^{\mu }.(\gamma \cdot k) m_e-\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e+\gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot k)-\gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot \;\text{p2})-\gamma ^{\mu }.(\gamma \cdot \;\text{p2}).(\gamma \cdot k)+\gamma ^{\mu }.(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2})-(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2})-(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot k)+2 \gamma \cdot \;\text{p2} k^{\mu }-2 \gamma \cdot \;\text{p2} \;\text{p2}^{\mu }+\gamma ^{\mu } k^2-2 \gamma ^{\mu } (k\cdot \;\text{p2})+\gamma ^{\mu } \;\text{p2}^2\right) \;\text{e}-4 i (1-\xi ) \pi ^3 \;\text{C}_0\left(0,\text{p2}^2,\text{p2}^2,0,0,m_e^2\right) \alpha  \left(-\gamma ^{\mu } m_e^2-(\gamma \cdot \;\text{p2}).\gamma ^{\mu } m_e+2 (\gamma \cdot k).(\gamma \cdot k).\gamma ^{\mu }-(\gamma \cdot k).(\gamma \cdot \;\text{p2}).\gamma ^{\mu }-(\gamma \cdot \;\text{p2}).(\gamma \cdot k).\gamma ^{\mu }+(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}).\gamma ^{\mu }+2 \gamma \cdot \;\text{p2} \;\text{p2}^{\mu }-2 \gamma ^{\mu } k^2+2 \gamma ^{\mu } (k\cdot \;\text{p2})-\gamma ^{\mu } \;\text{p2}^2\right) \;\text{e}-8 i (1-\xi ) \pi ^3 \;\text{D}_{00}\left(0,\text{p2}^2,k^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,0,0,m_e^2,m_e^2\right) \alpha  \left(-\gamma ^{\mu } m_e^2-\gamma ^{\mu }.(\gamma \cdot k) m_e+\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e+(\gamma \cdot \;\text{p2}).\gamma ^{\mu } m_e+(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot k)+\gamma ^{\mu } \;\text{p2}^2\right) \;\text{e}-8 i (1-\xi ) \pi ^3 k^{\mu } \;\text{D}_{11}\left(k^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,0,\text{p2}^2,\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,m_e^2,m_e^2,0,0\right) \alpha  \left(-(\gamma \cdot k) m_e^2-(\gamma \cdot k).(\gamma \cdot k) m_e+(\gamma \cdot k).(\gamma \cdot \;\text{p2}) m_e+(\gamma \cdot \;\text{p2}).(\gamma \cdot k) m_e+(\gamma \cdot \;\text{p2}).(\gamma \cdot k).(\gamma \cdot k)+\gamma \cdot k \;\text{p2}^2\right) \;\text{e}-16 i (1-\xi ) \pi ^3 \;\text{p2}^{\mu } \;\text{D}_{11}\left(0,\text{p2}^2,k^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,0,0,m_e^2,m_e^2\right) \alpha  \left(-(\gamma \cdot \;\text{p2}) m_e^2-(\gamma \cdot \;\text{p2}).(\gamma \cdot k) m_e+2 (\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}) m_e+(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}).(\gamma \cdot k)+\gamma \cdot \;\text{p2} \;\text{p2}^2\right) \;\text{e}-16 i (1-\xi ) \pi ^3 \;\text{p2}^{\mu } \;\text{D}_{12}\left(\text{p2}^2,0,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,k^2,\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,m_e^2,0,0,m_e^2\right) \alpha  \left(-(\gamma \cdot \;\text{p2}) m_e^2-(\gamma \cdot \;\text{p2}).(\gamma \cdot k) m_e+2 (\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}) m_e+(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}).(\gamma \cdot k)+\gamma \cdot \;\text{p2} \;\text{p2}^2\right) \;\text{e}-16 i (1-\xi ) \pi ^3 \;\text{D}_{12}\left(0,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,k^2,\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,\text{p2}^2,0,0,m_e^2,m_e^2\right) \alpha  \left(-(\gamma \cdot \;\text{p2}) k^{\mu } m_e^2-\gamma \cdot k \;\text{p2}^{\mu } m_e^2-(\gamma \cdot \;\text{p2}).(\gamma \cdot k) k^{\mu } m_e+2 (\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}) k^{\mu } m_e-(\gamma \cdot k).(\gamma \cdot k) \;\text{p2}^{\mu } m_e+(\gamma \cdot k).(\gamma \cdot \;\text{p2}) \;\text{p2}^{\mu } m_e+(\gamma \cdot \;\text{p2}).(\gamma \cdot k) \;\text{p2}^{\mu } m_e+(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}).(\gamma \cdot k) k^{\mu }+(\gamma \cdot \;\text{p2}).(\gamma \cdot k).(\gamma \cdot k) \;\text{p2}^{\mu }+\gamma \cdot \;\text{p2} k^{\mu } \;\text{p2}^2+\gamma \cdot k \;\text{p2}^{\mu } \;\text{p2}^2\right) \;\text{e}+4 i (1-\xi ) \pi ^3 \;\text{D}_0\left(0,\text{p2}^2,k^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,0,0,m_e^2,m_e^2\right) \alpha  \left(-\gamma ^{\mu }.(\gamma \cdot k) m_e^3-\gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot k) m_e^2+\gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot \;\text{p2}) m_e^2-2 (\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k) m_e^2+(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e^2-2 (\gamma \cdot k).(\gamma \cdot k).\gamma ^{\mu } m_e^2+(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot k) m_e^2+4 \gamma \cdot k k^{\mu } m_e^2-2 \gamma \cdot \;\text{p2} k^{\mu } m_e^2+(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot \;\text{p2}) m_e+2 (\gamma \cdot k).(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e+(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot k) m_e+2 (\gamma \cdot \;\text{p2}).(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k) m_e-2 (\gamma \cdot k).(\gamma \cdot \;\text{p2}) k^{\mu } m_e-4 (\gamma \cdot \;\text{p2}).(\gamma \cdot k) k^{\mu } m_e-2 (\gamma \cdot k).(\gamma \cdot \;\text{p2}) \;\text{p2}^{\mu } m_e-\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) k^2 m_e+(\gamma \cdot \;\text{p2}).\gamma ^{\mu } k^2 m_e+2 \gamma ^{\mu }.(\gamma \cdot \;\text{p2}) (k\cdot \;\text{p2}) m_e-\gamma ^{\mu }.(\gamma \cdot k) \;\text{p2}^2 m_e+2 (\gamma \cdot k).(\gamma \cdot k).(\gamma \cdot \;\text{p2}) \;\text{p2}^{\mu }-2 \gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot \;\text{p2}) (k\cdot \;\text{p2})-2 (\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) (k\cdot \;\text{p2})+4 \gamma \cdot \;\text{p2} k^{\mu } (k\cdot \;\text{p2})+(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k) \;\text{p2}^2+(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) \;\text{p2}^2-2 \gamma \cdot k k^{\mu } \;\text{p2}^2-2 \gamma \cdot \;\text{p2} k^{\mu } \;\text{p2}^2\right) \;\text{e}-4 i (1-\xi ) \pi ^3 \;\text{D}_1\left(k^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,0,\text{p2}^2,\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,m_e^2,m_e^2,0,0\right) \alpha  \left(\gamma ^{\mu }.(\gamma \cdot k) m_e^3+(\gamma \cdot k).\gamma ^{\mu } m_e^3+\gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot k) m_e^2-\gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot \;\text{p2}) m_e^2+2 (\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k) m_e^2-(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e^2+2 (\gamma \cdot k).(\gamma \cdot k).\gamma ^{\mu } m_e^2-(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot k) m_e^2-(\gamma \cdot \;\text{p2}).(\gamma \cdot k).\gamma ^{\mu } m_e^2-4 \gamma \cdot k k^{\mu } m_e^2+2 \gamma \cdot \;\text{p2} k^{\mu } m_e^2-(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot \;\text{p2}) m_e-2 (\gamma \cdot k).(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e-(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot k) m_e-2 (\gamma \cdot \;\text{p2}).(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k) m_e-2 (\gamma \cdot k).(\gamma \cdot k) k^{\mu } m_e+4 (\gamma \cdot k).(\gamma \cdot \;\text{p2}) k^{\mu } m_e+4 (\gamma \cdot \;\text{p2}).(\gamma \cdot k) k^{\mu } m_e+\gamma ^{\mu }.(\gamma \cdot k) k^2 m_e+\gamma ^{\mu }.(\gamma \cdot k) \;\text{p2}^2 m_e+(\gamma \cdot k).\gamma ^{\mu } \;\text{p2}^2 m_e+2 (\gamma \cdot \;\text{p2}).(\gamma \cdot k).(\gamma \cdot k) k^{\mu }-2 (\gamma \cdot k).(\gamma \cdot k).(\gamma \cdot \;\text{p2}) \;\text{p2}^{\mu }-(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot k) k^2+2 \gamma ^{\mu }.(\gamma \cdot k).(\gamma \cdot \;\text{p2}) (k\cdot \;\text{p2})-4 \gamma \cdot \;\text{p2} k^{\mu } (k\cdot \;\text{p2})-(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot k) \;\text{p2}^2+4 \gamma \cdot k k^{\mu } \;\text{p2}^2+2 \gamma \cdot \;\text{p2} k^{\mu } \;\text{p2}^2\right) \;\text{e}+8 i (1-\xi ) \pi ^3 \;\text{D}_1\left(0,\text{p2}^2,k^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,\text{p2}^2,k^2-2 (k\cdot \;\text{p2})+\text{p2}^2,0,0,m_e^2,m_e^2\right) \alpha  \left(-\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e^3-(\gamma \cdot \;\text{p2}).\gamma ^{\mu } m_e^3-\gamma ^{\mu }.(\gamma \cdot \;\text{p2}).(\gamma \cdot k) m_e^2+\gamma ^{\mu }.(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}) m_e^2-(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e^2-(\gamma \cdot k).(\gamma \cdot \;\text{p2}).\gamma ^{\mu } m_e^2-(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot k) m_e^2+2 (\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e^2-(\gamma \cdot \;\text{p2}).(\gamma \cdot k).\gamma ^{\mu } m_e^2+(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}).\gamma ^{\mu } m_e^2+2 \gamma \cdot \;\text{p2} k^{\mu } m_e^2+2 \gamma \cdot k \;\text{p2}^{\mu } m_e^2-2 \gamma \cdot \;\text{p2} \;\text{p2}^{\mu } m_e^2+(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}) m_e+(\gamma \cdot k).(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e+(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}).(\gamma \cdot k) m_e+2 (\gamma \cdot \;\text{p2}).(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) m_e+(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot k) m_e-4 (\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}) k^{\mu } m_e+2 (\gamma \cdot k).(\gamma \cdot k) \;\text{p2}^{\mu } m_e-2 (\gamma \cdot k).(\gamma \cdot \;\text{p2}) \;\text{p2}^{\mu } m_e-2 (\gamma \cdot \;\text{p2}).(\gamma \cdot k) \;\text{p2}^{\mu } m_e-\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) k^2 m_e-\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) \;\text{p2}^2 m_e-(\gamma \cdot \;\text{p2}).\gamma ^{\mu } \;\text{p2}^2 m_e+2 (\gamma \cdot k).(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}) \;\text{p2}^{\mu }-2 (\gamma \cdot \;\text{p2}).(\gamma \cdot k).(\gamma \cdot k) \;\text{p2}^{\mu }+(\gamma \cdot \;\text{p2}).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) k^2-2 \gamma ^{\mu }.(\gamma \cdot \;\text{p2}).(\gamma \cdot \;\text{p2}) (k\cdot \;\text{p2})+4 \gamma \cdot \;\text{p2} \;\text{p2}^{\mu } (k\cdot \;\text{p2})+(\gamma \cdot k).\gamma ^{\mu }.(\gamma \cdot \;\text{p2}) \;\text{p2}^2-2 \gamma \cdot \;\text{p2} k^{\mu } \;\text{p2}^2-2 \gamma \cdot k \;\text{p2}^{\mu } \;\text{p2}^2-2 \gamma \cdot \;\text{p2} \;\text{p2}^{\mu } \;\text{p2}^2\right) \;\text{e}$$

Discard all the finite pieces of the 1-loop amplitude

```mathematica
amp3Div[0] = PaVeUVPart[amp3[2], Prefactor -> 1/(2 Pi)^D] // DiracSimplify // 
        FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
     FCHideEpsilon // SelectNotFree2[#, {SMP["Delta"], SMP["d_A"], 
       SMP["d_e"], SMP["d_psi"]}] & // Simplify
```

$$-\frac{i \;\text{e} \gamma ^{\mu } \left(\alpha  \Delta  \xi +2 \pi  \delta _A+4 \pi  \delta _{\psi }+4 \pi  \delta _e\right)}{4 \pi }$$

```mathematica
ward[0] = Simplify[amp3Div[0]/(-FCI [I SMP["e"] GAD[mu]]) == 0]
```

$$\alpha  \Delta  \xi +2 \pi  \delta _A+4 \pi  \delta _{\psi }+4 \pi  \delta _e=0$$

```mathematica
wardMS[0] = Simplify[ward[0] /. Epsilon -> 1/SMP["Delta"] /. 
    	{SMP["d_psi"] -> SMP["d_psi^MSbar"]} /. solMSbar1]
wardMSbar[0] = Simplify[ward[0] /. {SMP["d_psi"] -> SMP["d_psi^MSbar"]} /. 
   	solMSbar1]
```

$$\delta _A+2 \delta _e=0$$

$$\delta _A+2 \delta _e=0$$

```mathematica
knownResults = {SMP["d_A"] + 2*SMP["d_e"] == 0, 
   	SMP["d_A"] + 2*SMP["d_e"] == 0};
FCCompareResults[{wardMS[0], wardMSbar[0]}, knownResults, 
   Text -> {"\tVerify Ward's identity:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
```

$$\text{$\backslash $tVerify Ward's identity:} \;\text{CORRECT.}$$

## Check the final results

```mathematica
knownResult = {
   	SMP["d_psi^MS"] -> -(GaugeXi*SMP["alpha_fs"])/(4*Epsilon*Pi), 
   	SMP["d_m^MS"] -> (-3*SMP["alpha_fs"])/(4*Epsilon*Pi), 
   	SMP["d_A^MS"] -> -SMP["alpha_fs"]/(3*Epsilon*Pi), 
   	SMP["d_psi^MSbar"] -> -(GaugeXi*SMP["alpha_fs"]*SMP["Delta"])/(4*Pi), 
   	SMP["d_m^MSbar"] -> (-3*SMP["alpha_fs"]*SMP["Delta"])/(4*Pi), 
   	SMP["d_A^MSbar"] -> -(SMP["alpha_fs"]*SMP["Delta"])/(3*Pi) 
   	};
FCCompareResults[Join[solMS1, solMS2, solMSbar1, solMSbar2], knownResult, 
   Text -> {"\tCheck the final result:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```

$$\text{$\backslash $tCheck the final result:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }26.364\text{ s.}$$