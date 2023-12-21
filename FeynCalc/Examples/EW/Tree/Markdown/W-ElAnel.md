---
title: W decaying into an electron and an electron-antineutrino
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "W -> El Anel, EW, total decay rate, tree";
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
MakeBoxes[k1, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(2\)]\)";
```

```mathematica
diags = InsertFields[CreateTopologies[0, 1 -> 2], 
    	{V[3]} -> {F[2, {1}], -F[1, {1}]}, InsertionLevel -> {Particles}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![1vtklee98p8jh](img/1vtklee98p8jh.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p}, 
  	OutgoingMomenta -> {k1, k2}, ChangeDimension -> 4, List -> False, SMP -> True, 
  	TransversePolarizationVectors -> {p}, 
  	Contract -> True, DropSumOver -> True, FinalSubstitutions -> 
   	{SMP["e"] -> Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_W"]^2 SMP["sin_W"]^2]}]
```

$$-\frac{2^{3/4} \sqrt{G_F m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2} \left(\varphi (\overline{k_1},m_e)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }(p)\right).\bar{\gamma }^7.\left(\varphi (-\overline{k_2})\right)}{\left.\sin (\theta _W\right)}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[]
SP[p] = SMP["m_W"]^2;
SP[k1] = SMP["m_e"]^2;
SP[k2] = 0;
SP[k1, k2] = (SMP["m_W"]^2 - SMP["m_e"]^2)/2;
SP[p, k1] = (SMP["m_W"]^2 + SMP["m_e"]^2)/2;
SP[p, k2] = (SMP["m_W"]^2 - SMP["m_e"]^2)/2;
```

## Square the amplitude

We average over the polarizations of the W-boson, hence the additional factor 1/3

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // SUNSimplify // 
     	FermionSpinSum // DiracSimplify // 
   	DoPolarizationSums[#, p, ExtraFactor -> 1/3] & // Simplify
```

$$-\frac{2}{3} \sqrt{2} G_F \left(m_e^2 m_W^2+m_e^4-2 m_W^4\right)$$

## Total decay rate

```mathematica
phaseSpacePrefactor[m1_, m2_, M_] := 1/(16 Pi M) Sqrt[1 - (m1 + m2)^2/M^2]*
   	Sqrt[1 - (m1 - m2)^2/M^2];
```

```mathematica
totalDecayRate = phaseSpacePrefactor[SMP["m_e"], 0, SMP["m_W"]]*
   	ampSquared[0] // Simplify
```

$$\frac{G_F \left(m_e^2-m_W^2\right){}^2 \left(m_e^2+2 m_W^2\right)}{12 \sqrt{2} \pi  m_W^3}$$

## Check the final results

```mathematica
knownResults = {
   	(SMP["G_F"] (SMP["m_e"] - SMP["m_W"])^2 (SMP["m_e"] + 
         	SMP["m_W"])^2 (SMP["m_e"]^2 + 2*SMP["m_W"]^2))/
    	(12*Sqrt[2]*Pi*SMP["m_W"]^3) 
   };
FCCompareResults[{totalDecayRate}, 
   knownResults, 
   Text -> {"\tCompare to Grozin, Using REDUCE in High Energy Physics, Chapter 5.2:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];

```

$$\text{$\backslash $tCompare to Grozin, Using REDUCE in High Energy Physics, Chapter 5.2:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }22.804\text{ s.}$$