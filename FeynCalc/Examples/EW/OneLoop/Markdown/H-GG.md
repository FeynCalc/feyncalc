---
title: Higgs decaying into two gluons
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "H -> Gl Gl, EW, total decay rate, 1-loop";
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

Here we consider only the dominant contribution from the top quark mass. However, it is trivial to include also loops from other quark flavors.

```mathematica
diags = InsertFields[CreateTopologies[1, 1 -> 2, ExcludeTopologies -> WFCorrections], 
    	{S[1]} -> {V[5], V[5]}, InsertionLevel -> {Particles}, Model -> "SMQCD", 
    	ExcludeParticles -> {F[3 | 4, {1 | 2}], F[4, {3}]}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![0zhg0w73lhlw0](img/0zhg0w73lhlw0.svg)

## Obtain the amplitudes

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, PreFactor -> -1], IncomingMomenta -> {pH}, 
   	OutgoingMomenta -> {k1, k2}, LoopMomenta -> {q}, List -> False, Contract -> True, 
   	TransversePolarizationVectors -> {k1, k2}, ChangeDimension -> D, 
   	DropSumOver -> True, SMP -> True, UndoChiralSplittings -> True];
```

```mathematica
amp[1] = amp[0] // FCTraceFactor // SUNSimplify
```

$$\frac{\text{e} g_s^2 m_t \delta ^{\text{Glu2}\;\text{Glu3}} \;\text{tr}\left(\left(m_t-\gamma \cdot (\text{k1}+\text{k2}-q)\right).\left(\gamma \cdot \varepsilon ^*(\text{k2})\right).\left(m_t-\gamma \cdot (\text{k1}-q)\right).\left(\gamma \cdot \varepsilon ^*(\text{k1})\right).\left(m_t+\gamma \cdot q\right)\right)}{2 m_W \left(\left.\sin (\theta _W\right)\right) \left(q^2-m_t^2\right).\left((\text{k1}-q)^2-m_t^2\right).\left((\text{k1}+\text{k2}-q)^2-m_t^2\right)}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
ScalarProduct[k1, k1] = 0;
ScalarProduct[k2, k2] = 0;
ScalarProduct[pH, pH] = SMP["m_H"]^2;
ScalarProduct[k1, k2] = (SMP["m_H"]^2)/2;
```

## Evaluate the amplitudes

Dirac trace and tensor decomposition

```mathematica
amp[2] = amp[1] // DiracSimplify // TID[#, q, ToPaVe -> True] &
```

$$\frac{2 i \pi ^2 \;\text{e} g_s^2 m_t^2 \delta ^{\text{Glu2}\;\text{Glu3}} \;\text{B}_0\left(m_H^2,m_t^2,m_t^2\right) \left(-\left((2-D) m_H^2 \left(\varepsilon ^*(\text{k1})\cdot \varepsilon ^*(\text{k2})\right)\right)-2 D \left(\text{k1}\cdot \varepsilon ^*(\text{k2})\right) \left(\text{k2}\cdot \varepsilon ^*(\text{k1})\right)-2 m_H^2 \left(\varepsilon ^*(\text{k1})\cdot \varepsilon ^*(\text{k2})\right)+8 \left(\text{k1}\cdot \varepsilon ^*(\text{k2})\right) \left(\text{k2}\cdot \varepsilon ^*(\text{k1})\right)\right)}{(2-D) m_H^2 m_W \left(\left.\sin (\theta _W\right)\right)}+\frac{i \pi ^2 \;\text{e} g_s^2 m_t^2 \delta ^{\text{Glu2}\;\text{Glu3}} \left((2-D) m_H^2+8 m_t^2\right) \;\text{C}_0\left(0,0,m_H^2,m_t^2,m_t^2,m_t^2\right) \left(2 \left(\text{k1}\cdot \varepsilon ^*(\text{k2})\right) \left(\text{k2}\cdot \varepsilon ^*(\text{k1})\right)-m_H^2 \left(\varepsilon ^*(\text{k1})\cdot \varepsilon ^*(\text{k2})\right)\right)}{(2-D) m_H^2 m_W \left(\left.\sin (\theta _W\right)\right)}$$

The explicit values for the PaVe functions B0 and  C0 can be obtained e.g. from H. Patel's Package-X. Here we just insert the known results.

```mathematica
loopInts = {
   	B0[SMP["m_H"]^2, SMP["m_t"]^2, SMP["m_t"]^2] -> 
    		1/(16*Epsilon*Pi^4) - (-2*SMP["m_H"] + 
        		EulerGamma*SMP["m_H"] - Log[4*Pi]*SMP["m_H"] - 
        		Log[ScaleMu^2/SMP["m_t"]^2]*SMP["m_H"] - 
        		Log[(-SMP["m_H"]^2 + 2*SMP["m_t"]^2 + 
             		SMP["m_H"]*Sqrt[SMP["m_H"]^2 - 4*SMP["m_t"]^2])/
           		(2*SMP["m_t"]^2)]*Sqrt[SMP["m_H"]^2 - 
           		4*SMP["m_t"]^2])/(16*Pi^4*SMP["m_H"]), 
   	C0[0, 0, SMP["m_H"]^2, SMP["m_t"]^2, SMP["m_t"]^2, SMP["m_t"]^2] ->
    		Log[(-SMP["m_H"]^2 + 2*SMP["m_t"]^2 + 
          		SMP["m_H"]*Sqrt[SMP["m_H"]^2 - 4*SMP["m_t"]^2])/
        		(2*SMP["m_t"]^2)]^2/(32*Pi^4*SMP["m_H"]^2) 
   };
```

```mathematica
$Assumptions = {SMP["m_H"] > 0, SMP["m_t"] > 0};
amp[3] = (amp[2] /. loopInts) // FCReplaceD[#, D -> 4 - 2 Epsilon] & //
   	Series[#, {Epsilon, 0, 0}] & // Normal
```

$$-\frac{i \;\text{e} g_s^2 m_t^2 \delta ^{\text{Glu2}\;\text{Glu3}} \left(m_H^2 \log ^2\left(\frac{m_H \sqrt{m_H^2-4 m_t^2}-m_H^2+2 m_t^2}{2 m_t^2}\right)-4 m_t^2 \log ^2\left(\frac{m_H \sqrt{m_H^2-4 m_t^2}-m_H^2+2 m_t^2}{2 m_t^2}\right)-4 m_H^2\right) \left(m_H^2 \left(\varepsilon ^*(\text{k1})\cdot \varepsilon ^*(\text{k2})\right)-2 \left(\text{k1}\cdot \varepsilon ^*(\text{k2})\right) \left(\text{k2}\cdot \varepsilon ^*(\text{k1})\right)\right)}{32 \pi ^2 m_H^4 m_W \left(\left.\sin (\theta _W\right)\right)}$$

As expected, the result is finite (i.e. contains no 1/Epsilon poles), so that it is safe to switch back to 4 dimensions

```mathematica
amp[4] = amp[3] // ChangeDimension[#, 4] &
```

$$-\frac{i \;\text{e} g_s^2 m_t^2 \delta ^{\text{Glu2}\;\text{Glu3}} \left(m_H^2 \log ^2\left(\frac{m_H \sqrt{m_H^2-4 m_t^2}-m_H^2+2 m_t^2}{2 m_t^2}\right)-4 m_t^2 \log ^2\left(\frac{m_H \sqrt{m_H^2-4 m_t^2}-m_H^2+2 m_t^2}{2 m_t^2}\right)-4 m_H^2\right) \left(m_H^2 \left(\bar{\varepsilon }^*(\text{k1})\cdot \bar{\varepsilon }^*(\text{k2})\right)-2 \left(\overline{\text{k1}}\cdot \bar{\varepsilon }^*(\text{k2})\right) \left(\overline{\text{k2}}\cdot \bar{\varepsilon }^*(\text{k1})\right)\right)}{32 \pi ^2 m_H^4 m_W \left(\left.\sin (\theta _W\right)\right)}$$

## Square the amplitudes

```mathematica
ampSquared[0] = 1/2 (amp[4] (ComplexConjugate[amp[4]])) // 
     	SUNSimplify[#, SUNNToCACF -> False] & // 
    	DoPolarizationSums[#, k1, k2] & // DoPolarizationSums[#, k2, k1] & // 
  	Simplify
```

$$\frac{\text{e}^2 \left(N^2-1\right) g_s^4 m_t^4 \left(m_H^2 \left(\log ^2\left(\frac{m_H \sqrt{m_H^2-4 m_t^2}-m_H^2+2 m_t^2}{2 m_t^2}\right)-4\right)-4 m_t^2 \log ^2\left(\frac{m_H \sqrt{m_H^2-4 m_t^2}-m_H^2+2 m_t^2}{2 m_t^2}\right)\right){}^2}{1024 \pi ^4 m_H^4 m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2}$$

## Total decay rate

```mathematica
phaseSpacePrefactor[m_] := 1/(16 Pi SMP["m_H"]) Sqrt[1 - 4 m^2 / SMP["m_H"]^2]
```

```mathematica
totalDecayRate = phaseSpacePrefactor[0] ampSquared[0] // 
    ReplaceAll[#, {SMP["e"]^2 -> 4 Pi SMP["alpha_fs"], 
       SMP["g_s"]^4 -> 16 Pi^2 SMP["alpha_s"]^2}] & // Simplify
```

$$\frac{\alpha  \left(N^2-1\right) m_t^4 \alpha _s^2 \left(m_H^2 \left(\log ^2\left(\frac{m_H \sqrt{m_H^2-4 m_t^2}-m_H^2+2 m_t^2}{2 m_t^2}\right)-4\right)-4 m_t^2 \log ^2\left(\frac{m_H \sqrt{m_H^2-4 m_t^2}-m_H^2+2 m_t^2}{2 m_t^2}\right)\right){}^2}{256 \pi ^2 m_H^5 m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2}$$

```mathematica
ISq = totalDecayRate/( SMP["alpha_s"]^2/(9 Pi^2) SMP["m_H"]^2/SMP["m_W"]^2 *
    	SMP["alpha_fs"] SMP["m_H"]/(8 SMP["sin_W"]^2))
```

$$\frac{9 \left(N^2-1\right) m_t^4 \left(m_H^2 \left(\log ^2\left(\frac{m_H \sqrt{m_H^2-4 m_t^2}-m_H^2+2 m_t^2}{2 m_t^2}\right)-4\right)-4 m_t^2 \log ^2\left(\frac{m_H \sqrt{m_H^2-4 m_t^2}-m_H^2+2 m_t^2}{2 m_t^2}\right)\right){}^2}{32 m_H^8}$$

ISq corresponds to I(m_H^2/m_q^2) from Peskin and Schroeder, Final Project 3, part (c). It should go to 1 for m_q -> Infinity and to 0 for m_q -> 0

```mathematica
limit1 = Limit[ISq, SMP["m_t"] -> Infinity] /. SUNN -> 3
limit2 = Limit[ISq, SMP["m_t"] -> 0]
```

![0sqx3a3ja9c2n](img/0sqx3a3ja9c2n.svg)

$$1$$

![1gx23ju0ivcsq](img/1gx23ju0ivcsq.svg)

$$0$$

## Check the final results

```mathematica
knownResults = {1, 0};
FCCompareResults[{limit1, limit2}, 
   knownResults, Factoring -> Simplify, 
   Text -> {"\tCompare to Peskin and Schroeder,An Introduction to QFT, Final Project III, part (c):", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];

```

$$\text{$\backslash $tCompare to Peskin and Schroeder,An Introduction to QFT, Final Project III, part (c):} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }22.532\text{ s.}$$