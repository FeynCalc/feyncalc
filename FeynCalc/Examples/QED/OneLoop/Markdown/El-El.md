---
title: QED electron self-energy
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "El -> El, QED, only UV divergences, 1-loop";
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
diags = InsertFields[CreateTopologies[1, 1 -> 1, 
     		ExcludeTopologies -> Tadpoles], {F[2, {1}]} -> 
     		{F[2, {1}]}, InsertionLevel -> {Particles}, 
    		ExcludeParticles -> {S[_], V[2 | 3], (S | U)[_], F[3 | 4], F[2, {2 | 3}]}]; 
 
Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![1v49gnh8hxl8s](img/1v49gnh8hxl8s.svg)

## Obtain the amplitude

The 1/(2Pi)^D prefactor is implicit. We keep the full gauge dependence.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, 
   	PreFactor -> 1, GaugeRules -> {}], IncomingMomenta -> {p}, 
  	OutgoingMomenta -> {p}, LoopMomenta -> {q}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> False, SMP -> True, Contract -> True]
```

$$-\frac{\text{e}^2 \gamma ^{\text{Lor2}}.\left(m_e+\gamma \cdot q\right).\gamma ^{\text{Lor2}}}{\left(q^2-m_e^2\right).(q-p)^2}-\frac{\text{e}^2 \left(1-\xi _A\right) (\gamma \cdot (p-q)).\left(m_e+\gamma \cdot q\right).(\gamma \cdot (q-p))}{\left(q^2-m_e^2\right).(q-p)^4}$$

## Calculate the amplitude

```mathematica
amp[1] = TID[amp[0], q, ToPaVe -> True]
```

$$\frac{1}{2 p^2}i \pi ^2 \;\text{e}^2 \;\text{B}_0\left(p^2,0,m_e^2\right) \left(\xi _A m_e^2 \gamma \cdot p-2 \xi _A m_e (\gamma \cdot p).(\gamma \cdot p)+p^2 \xi _A \gamma \cdot p-D \left(p^2-m_e^2\right) \gamma \cdot p-2 D p^2 m_e+2 D p^2 \gamma \cdot p+m_e^2 (-(\gamma \cdot p))+2 m_e (\gamma \cdot p).(\gamma \cdot p)+2 \left(p^2-m_e^2\right) \gamma \cdot p-5 p^2 \gamma \cdot p\right)-\frac{i \pi ^2 \;\text{e}^2 \left(1-\xi _A\right) \;\text{B}_0(0,0,0) \left(m_e^2 (-(\gamma \cdot p))+2 m_e (\gamma \cdot p).(\gamma \cdot p)-2 p^2 m_e+p^2 \gamma \cdot p\right)}{2 p^2}+\frac{i \pi ^2 \;\text{e}^2 \left(1-\xi _A\right) \left(-m_e^2 \left(p^2-m_e^2\right) \gamma \cdot p-4 p^2 m_e (\gamma \cdot p).(\gamma \cdot p)+2 m_e \left(p^2-m_e^2\right) (\gamma \cdot p).(\gamma \cdot p)+p^2 \left(p^2-m_e^2\right) \gamma \cdot p+2 p^2 m_e^3+2 p^4 m_e\right) \;\text{C}_0\left(0,p^2,p^2,0,0,m_e^2\right)}{2 p^2}+\frac{i \pi ^2 (2-D) \;\text{e}^2 \gamma \cdot p \;\text{A}_0\left(m_e^2\right)}{2 p^2}$$

The UV divergence of the amplitude can be obtained via PaVeUVPart.
Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.
Hint: If you need the full result for the amplitude, use PaXEvaluate from FeynHelpers.

```mathematica
ampDiv[0] = PaVeUVPart[amp[1], Prefactor -> 1/(2 Pi)^D] // 
       FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
    SelectNotFree2[#, Epsilon] & // Simplify
```

$$\frac{i \;\text{e}^2 \left(\xi _A \gamma \cdot p-\left(\xi _A+3\right) m_e\right)}{16 \pi ^2 \varepsilon }$$

The self-energy amplitude is usually defined as -i Sigma(p^2)

```mathematica
sigma[0] = I ampDiv[0]
```

$$-\frac{\text{e}^2 \left(\xi _A \gamma \cdot p-\left(\xi _A+3\right) m_e\right)}{16 \pi ^2 \varepsilon }$$

```mathematica
sigmaFeynmanGauge[0] = sigma[0] /. GaugeXi[A] -> 1
```

$$-\frac{\text{e}^2 \left(\gamma \cdot p-4 m_e\right)}{16 \pi ^2 \varepsilon }$$

## Check the final results

Keep in mind that Peskin and Schroeder use D = 4-Epsilon,
while we did the calculation with D = 4-2Epsilon.

```mathematica
knownResult = SMP["e"]^2/(4 Pi)^(D/2) Gamma[2 - D/2]/
          	((1 - x) SMP["m_e"]^2 + x ScaleMu^2 - x (1 - x) SPD[p, p])^(2 - D/2)*
         	((4 - Epsilon) SMP["m_e"] - (2 - Epsilon) x GSD[p]) // 
        	FCReplaceD[#, D -> 4 - Epsilon] & // Series[#, {Epsilon, 0, 0}] & // 
      	Normal // SelectNotFree2[#, Epsilon] & // Integrate[#, {x, 0, 1}] & // 
   	ReplaceAll[#, 1/Epsilon -> 1/(2 Epsilon)] &;
FCCompareResults[sigmaFeynmanGauge[0], knownResult, 
   Text -> {"\tCompare to Peskin and Schroeder, An Introduction to QFT, Eq 10.41:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Peskin and Schroeder, An Introduction to QFT, Eq 10.41:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }15.789\text{ s.}$$