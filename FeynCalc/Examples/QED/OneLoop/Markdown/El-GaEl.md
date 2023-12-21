---
title: Electron's g-2 in QED
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "El -> Ga El, QED, F2(0) form factor, 1-loop";
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
MakeBoxes[p1, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(2\)]\)";
```

```mathematica
diags = InsertFields[CreateTopologies[1, 1 -> 2, 
     		ExcludeTopologies -> {Tadpoles, WFCorrections}], {F[2, {1}]} ->
     		{V[1], F[2, {1}]}, InsertionLevel -> {Particles}, 
    		ExcludeParticles -> {S[_], V[2 | 3], (S | U)[_], F[3 | 4], F[2, {2 | 3}]}]; 
 
Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![1egyqolokmcnv](img/1egyqolokmcnv.svg)

## Obtain the amplitude

The 1/(2Pi)^D prefactor is implicit. We need to replace e with -e to be compatible
with the convention D^mu = d^mu + ie A^mu

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, PreFactor -> 1], 
    	IncomingMomenta -> {p1}, OutgoingMomenta -> {k, p2}, 
    	LorentzIndexNames -> {mu}, 
    	LoopMomenta -> {q}, UndoChiralSplittings -> True, 
    	ChangeDimension -> D, List -> False, SMP -> True, 
    	FinalSubstitutions -> {SMP["e"] -> -SMP["e"]}] /. 
   	k -> p1 - p2 /. q -> q + p1
```

$$\frac{i g^{\text{Lor2}\;\text{Lor3}} \varepsilon ^{*\mu }\left(p_1-p_2\right) \left(\varphi (p_2,m_e)\right).\left(-i \;\text{e} \gamma ^{\text{Lor3}}\right).\left(m_e+\gamma \cdot \left(q+p_2\right)\right).\left(-i \;\text{e} \gamma ^{\mu }\right).\left(m_e+\gamma \cdot \left(q+p_1\right)\right).\left(-i \;\text{e} \gamma ^{\text{Lor2}}\right).\left(\varphi (p_1,m_e)\right)}{\left((p_1+q){}^2-m_e^2\right).\left((p_2+q){}^2-m_e^2\right).q^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
ME = SMP["m_e"];
ScalarProduct[p1, p1] = ME^2;
ScalarProduct[p2, p2] = ME^2;
ScalarProduct[k, k] = 0;
ScalarProduct[p1, p2] = ME^2;
```

## Calculate the amplitude

Amputate the polarization vector.

```mathematica
amp[1] = amp[0] // ReplaceAll[#, 
      	Pair[Momentum[Polarization[___], ___], ___] :> 1] & // 
   	Contract // ReplaceAll[#, SMP["e"]^3 -> 4 Pi SMP["e"] SMP["alpha_fs"]] &
```

$$-\frac{4 \pi  \alpha  \;\text{e} \left(\varphi (p_2,m_e)\right).\gamma ^{\text{Lor3}}.\left(m_e+\gamma \cdot \left(q+p_2\right)\right).\gamma ^{\mu }.\left(m_e+\gamma \cdot \left(q+p_1\right)\right).\gamma ^{\text{Lor3}}.\left(\varphi (p_1,m_e)\right)}{\left((p_1+q){}^2-m_e^2\right).\left((p_2+q){}^2-m_e^2\right).q^2}$$

```mathematica
amp[2] = TID[amp[1], q, ToPaVe -> True] // DiracSimplify // 
  	Collect2[#, Spinor] &
```

$$8 i \pi ^3 \alpha  \;\text{e} m_e \left(p_1{}^{\mu }+p_2{}^{\mu }\right) \left(2 \;\text{C}_1\left(0,m_e^2,m_e^2,m_e^2,m_e^2,0\right)+D \;\text{C}_{11}\left(0,m_e^2,m_e^2,m_e^2,m_e^2,0\right)-2 \;\text{C}_{11}\left(0,m_e^2,m_e^2,m_e^2,m_e^2,0\right)+D \;\text{C}_{12}\left(m_e^2,0,m_e^2,0,m_e^2,m_e^2\right)-2 \;\text{C}_{12}\left(m_e^2,0,m_e^2,0,m_e^2,m_e^2\right)\right) \left(\varphi (p_2,m_e)\right).\left(\varphi (p_1,m_e)\right)-4 i \pi ^3 \alpha  \;\text{e} \left(D \;\text{B}_0\left(0,m_e^2,m_e^2\right)-6 \;\text{B}_0\left(0,m_e^2,m_e^2\right)+4 \;\text{B}_0\left(m_e^2,0,m_e^2\right)+4 m_e^2 \;\text{C}_0\left(0,m_e^2,m_e^2,m_e^2,m_e^2,0\right)-2 D \;\text{C}_{00}\left(0,m_e^2,m_e^2,m_e^2,m_e^2,0\right)+4 \;\text{C}_{00}\left(0,m_e^2,m_e^2,m_e^2,m_e^2,0\right)\right) \left(\varphi (p_2,m_e)\right).\gamma ^{\mu }.\left(\varphi (p_1,m_e)\right)$$

To extract F2 (0) we need to look only at the piece proportional to (p1+p2)^mu. Thus we can drop the g^mu -piece

```mathematica
amp[3] = amp[2] // ReplaceAll[#, FCI[GAD[mu]] :> 0] & // DotSimplify
```

$$8 i \pi ^3 \alpha  \;\text{e} m_e \left(p_1{}^{\mu }+p_2{}^{\mu }\right) \left(2 \;\text{C}_1\left(0,m_e^2,m_e^2,m_e^2,m_e^2,0\right)+D \;\text{C}_{11}\left(0,m_e^2,m_e^2,m_e^2,m_e^2,0\right)-2 \;\text{C}_{11}\left(0,m_e^2,m_e^2,m_e^2,m_e^2,0\right)+D \;\text{C}_{12}\left(m_e^2,0,m_e^2,0,m_e^2,m_e^2\right)-2 \;\text{C}_{12}\left(m_e^2,0,m_e^2,0,m_e^2,m_e^2\right)\right) \left(\varphi (p_2,m_e)\right).\left(\varphi (p_1,m_e)\right)$$

The explicit values for the PaVe functions C1, C11 and C12 can be obtained e.g. from H. Patel's Package-X. Here we just insert the known results.

```mathematica
amp[4] = amp[3] /. {
   	PaVe[1, {0, SMP["m_e"]^2, SMP["m_e"]^2}, {SMP["m_e"]^2, SMP["m_e"]^2, 0}, OptionsPattern[]] -> 
    		1/(32 Pi^4 ME^2), 
   	PaVe[1, 1, {0, SMP["m_e"]^2, SMP["m_e"]^2}, {SMP["m_e"]^2, SMP["m_e"]^2, 0}, OptionsPattern[]] -> 
    		-(1/(96 Pi^4 ME^2)), 
   	PaVe[1, 2, {SMP["m_e"]^2, 0, SMP["m_e"]^2}, {0, SMP["m_e"]^2, SMP["m_e"]^2}, OptionsPattern[]] -> 
    		-(1/(192 Pi^4 ME^2)) 
   }
```

$$8 i \pi ^3 \alpha  \;\text{e} m_e \left(\frac{3}{32 \pi ^4 m_e^2}-\frac{D}{64 \pi ^4 m_e^2}\right) \left(p_1{}^{\mu }+p_2{}^{\mu }\right) \left(\varphi (p_2,m_e)\right).\left(\varphi (p_1,m_e)\right)$$

As expected, F2 (0) is free of any divergences. So we can safely do the limit D ->4

```mathematica
amp[5] = amp[4] // ChangeDimension[#, 4] & // ReplaceAll[#, D -> 4] &
```

$$\frac{i \alpha  \;\text{e} \left(\overline{p_1}{}^{\mu }+\overline{p_2}{}^{\mu }\right) \left(\varphi (\overline{p_2},m_e)\right).\left(\varphi (\overline{p_1},m_e)\right)}{4 \pi  m_e}$$

We obtained $\frac{i e}{2 m_e} (p_1+p_2)^\mu F_2 (0) \bar{u}(p_2) u(p_1)$.
Dividing by the numerical prefactor and substituting $e^2 = 4\pi^2 \alpha$ yields F2(0)

```mathematica
f2[0] = (amp[5]/((I SMP["e"])/(2 ME))) // 
  	ReplaceAll [#, {Spinor[__] . Spinor[__] :> 1, 
     	FCI[FV[p1, _] + FV[p2, _]] :> 1}] &
```

$$\frac{\alpha }{2 \pi }$$

## Check the final results

```mathematica
knownResult = AlphaFS/(2 Pi);
FCCompareResults[f2[0], knownResult, 
   Text -> {"\tCompare to J. Schwinger, Phys. Rev. 73, 416-417, 1948:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```

$$\text{$\backslash $tCompare to J. Schwinger, Phys. Rev. 73, 416-417, 1948:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }25.367\text{ s.}$$