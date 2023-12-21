---
title: Z boson decaying into a fermion-antifermion pair
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Z -> F Fbar, EW, total decay rate, tree";
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

```mathematica
MakeBoxes[k1, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(2\)]\)";
MakeBoxes[l1, TraditionalForm] := "\!\(\*SubscriptBox[\(l\), \(1\)]\)";
MakeBoxes[l2, TraditionalForm] := "\!\(\*SubscriptBox[\(l\), \(2\)]\)";
MakeBoxes[p1, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(2\)]\)";
```

```mathematica
diagsLeptonsMassless = InsertFields[CreateTopologies[0, 1 -> 2], 
    	{V[2]} -> {F[1, {l}], -F[1, {l}]}, InsertionLevel -> {Classes}]; 
 
Paint[diagsLeptonsMassless, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![1reb9cr7zutsj](img/1reb9cr7zutsj.svg)

```mathematica
diagsLeptonsMassive = InsertFields[CreateTopologies[0, 1 -> 2], 
    	{V[2]} -> {F[2, {l}], -F[2, {l}]}, InsertionLevel -> {Classes}]; 
 
Paint[diagsLeptonsMassive, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![1vh5029e11gma](img/1vh5029e11gma.svg)

```mathematica
diagsUpQuarks = InsertFields[CreateTopologies[0, 1 -> 2], 
    	{V[2]} -> {F[3, {l}], -F[3, {l}]}, InsertionLevel -> {Classes}]; 
 
Paint[diagsUpQuarks, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![1800qcsnze0re](img/1800qcsnze0re.svg)

```mathematica
diagsDownQuarks = InsertFields[CreateTopologies[0, 1 -> 2], 
    	{V[2]} -> {F[4, {l}], -F[4, {l}]}, InsertionLevel -> {Classes}]; 
 
Paint[diagsDownQuarks, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![181cwu4s4s2ij](img/181cwu4s4s2ij.svg)

## Obtain the amplitudes

```mathematica
ampLeptonsMassless[0] = FCFAConvert[CreateFeynAmp[diagsLeptonsMassless], IncomingMomenta -> {p}, 
  	OutgoingMomenta -> {l1, l2}, List -> False, ChangeDimension -> 4, 
  	DropSumOver -> True, SMP -> True, Contract -> True, 
  	FinalSubstitutions -> {MLE[l] -> SMP["m_l"], SMP["e"] -> Sqrt[8/Sqrt[2]*
       	SMP["G_F"]  SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]
```

$$-\frac{\sqrt[4]{2} \sqrt{G_F m_Z^2 \left(\left.\cos (\theta _W\right)\right){}^2 \left(\left.\sin (\theta _W\right)\right){}^2} \left(\varphi (\overline{l_1})\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }(p)\right).\bar{\gamma }^7.\left(\varphi (-\overline{l_2})\right)}{\left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}$$

```mathematica
ampLeptonsMassive[0] = FCFAConvert[CreateFeynAmp[diagsLeptonsMassive], IncomingMomenta -> {p}, 
  	OutgoingMomenta -> {p1, p2}, List -> False, ChangeDimension -> 4, 
  	DropSumOver -> True, SMP -> True, Contract -> True, 
  	FinalSubstitutions -> {MLE[l] -> SMP["m_l"], SMP["e"] -> Sqrt[8/Sqrt[2]*
       	SMP["G_F"]  SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]
```

$$i \left(\varphi (\overline{p_1},m_l)\right).\left(\frac{2 i \sqrt[4]{2} \left(\left.\sin (\theta _W\right)\right) \left(\bar{\gamma }\cdot \bar{\varepsilon }(p)\right).\bar{\gamma }^6 \sqrt{G_F m_Z^2 \left(\left.\cos (\theta _W\right)\right){}^2 \left(\left.\sin (\theta _W\right)\right){}^2}}{\left.\cos (\theta _W\right)}+\frac{2 i \sqrt[4]{2} \left(\left(\left.\sin (\theta _W\right)\right){}^2-\frac{1}{2}\right) \left(\bar{\gamma }\cdot \bar{\varepsilon }(p)\right).\bar{\gamma }^7 \sqrt{G_F m_Z^2 \left(\left.\cos (\theta _W\right)\right){}^2 \left(\left.\sin (\theta _W\right)\right){}^2}}{\left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}\right).\left(\varphi (-\overline{p_2},m_l)\right)$$

```mathematica
ampUpQuarks[0] = FCFAConvert[CreateFeynAmp[diagsUpQuarks], IncomingMomenta -> {p}, 
  	OutgoingMomenta -> {k1, k2}, List -> False, ChangeDimension -> 4, 
  	DropSumOver -> True, SMP -> True, Contract -> True, 
  	FinalSubstitutions -> {MQU[l] -> SMP["m_q"], SMP["e"] -> Sqrt[8/Sqrt[2]*
       	SMP["G_F"]  SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]
```

$$i \left(\varphi (\overline{k_1},m_q)\right).\left(\frac{2 i \sqrt[4]{2} \delta _{\text{Col2}\;\text{Col3}} \left(\frac{1}{2}-\frac{2}{3} \left(\left.\sin (\theta _W\right)\right){}^2\right) \left(\bar{\gamma }\cdot \bar{\varepsilon }(p)\right).\bar{\gamma }^7 \sqrt{G_F m_Z^2 \left(\left.\cos (\theta _W\right)\right){}^2 \left(\left.\sin (\theta _W\right)\right){}^2}}{\left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}-\frac{4 i \sqrt[4]{2} \delta _{\text{Col2}\;\text{Col3}} \left(\left.\sin (\theta _W\right)\right) \left(\bar{\gamma }\cdot \bar{\varepsilon }(p)\right).\bar{\gamma }^6 \sqrt{G_F m_Z^2 \left(\left.\cos (\theta _W\right)\right){}^2 \left(\left.\sin (\theta _W\right)\right){}^2}}{3 \left(\left.\cos (\theta _W\right)\right)}\right).\left(\varphi (-\overline{k_2},m_q)\right)$$

```mathematica
ampDownQuarks[0] = FCFAConvert[CreateFeynAmp[diagsDownQuarks], IncomingMomenta -> {p}, 
  	OutgoingMomenta -> {k1, k2}, List -> False, ChangeDimension -> 4, 
  	DropSumOver -> True, SMP -> True, Contract -> True, 
  	FinalSubstitutions -> {MQD[l] -> SMP["m_q"], SMP["e"] -> Sqrt[8/Sqrt[2]*
       	SMP["G_F"]  SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]
```

$$i \left(\varphi (\overline{k_1},m_q)\right).\left(\frac{2 i \sqrt[4]{2} \delta _{\text{Col2}\;\text{Col3}} \left(\frac{1}{3} \left(\left.\sin (\theta _W\right)\right){}^2-\frac{1}{2}\right) \left(\bar{\gamma }\cdot \bar{\varepsilon }(p)\right).\bar{\gamma }^7 \sqrt{G_F m_Z^2 \left(\left.\cos (\theta _W\right)\right){}^2 \left(\left.\sin (\theta _W\right)\right){}^2}}{\left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}+\frac{2 i \sqrt[4]{2} \delta _{\text{Col2}\;\text{Col3}} \left(\left.\sin (\theta _W\right)\right) \left(\bar{\gamma }\cdot \bar{\varepsilon }(p)\right).\bar{\gamma }^6 \sqrt{G_F m_Z^2 \left(\left.\cos (\theta _W\right)\right){}^2 \left(\left.\sin (\theta _W\right)\right){}^2}}{3 \left(\left.\cos (\theta _W\right)\right)}\right).\left(\varphi (-\overline{k_2},m_q)\right)$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[]
SP[p] = SMP["m_Z"]^2;
SP[k1] = SMP["m_q"]^2;
SP[k2] = SMP["m_q"]^2; 
 
SP[p1] = SMP["m_l"]^2;
SP[p2] = SMP["m_l"]^2; 
 
SP[l1] = 0;
SP[l2] = 0; 
 
SP[k1, k2] = Simplify[(SP[p] - SP[k1] - SP[k2])/2];
SP[p, k1] = Simplify[ExpandScalarProduct[SP[k1 + k2, k1]]];
SP[p, k2] = Simplify[ExpandScalarProduct[SP[k1 + k2, k2]]]; 
 
SP[p1, p2] = Simplify[(SP[p] - SP[p1] - SP[p2])/2];
SP[p, p1] = Simplify[ExpandScalarProduct[SP[p1 + p2, p1]]];
SP[p, p2] = Simplify[ExpandScalarProduct[SP[p1 + p2, p2]]]; 
 
SP[l1, l2] = Simplify[(SP[p] - SP[l1] - SP[l2])/2];
SP[p, l1] = Simplify[ExpandScalarProduct[SP[l1 + l2, l1]]];
SP[p, l2] = Simplify[ExpandScalarProduct[SP[l1 + l2, l2]]];
```

## Square the amplitudes

We average over the polarizations of the W-boson, hence the additional factor 1/3

```mathematica
ampSquaredLeptonsMassless[0] = 
 	(ampLeptonsMassless[0] (ComplexConjugate[ampLeptonsMassless[0]])) //
     	FermionSpinSum // DiracSimplify // 
   	DoPolarizationSums[#, p, ExtraFactor -> 1/3] & // Simplify
```

$$\frac{2}{3} \sqrt{2} G_F m_Z^4$$

```mathematica
ampSquaredLeptonsMassive[0] = 
 	(ampLeptonsMassive[0] (ComplexConjugate[ampLeptonsMassive[0]])) // 
     	FermionSpinSum // DiracSimplify // 
   	DoPolarizationSums[#, p, ExtraFactor -> 1/3] & // Simplify
```

$$\frac{2}{3} \sqrt{2} G_F m_Z^2 \left(m_l^2 \left(16 \left(\left.\sin (\theta _W\right)\right){}^4-8 \left(\left.\sin (\theta _W\right)\right){}^2-1\right)+m_Z^2 \left(8 \left(\left.\sin (\theta _W\right)\right){}^4-4 \left(\left.\sin (\theta _W\right)\right){}^2+1\right)\right)$$

```mathematica
ampSquaredUpQuarks[0] = 
 	(ampUpQuarks[0] (ComplexConjugate[ampUpQuarks[0]])) // 
      	FermionSpinSum // DiracSimplify // SUNSimplify // 
   	DoPolarizationSums[#, p, ExtraFactor -> 1/3] & // Simplify
```

$$\frac{2}{27} \sqrt{2} C_A G_F m_Z^2 \left(m_q^2 \left(64 \left(\left.\sin (\theta _W\right)\right){}^4-48 \left(\left.\sin (\theta _W\right)\right){}^2-9\right)+m_Z^2 \left(32 \left(\left.\sin (\theta _W\right)\right){}^4-24 \left(\left.\sin (\theta _W\right)\right){}^2+9\right)\right)$$

```mathematica
ampSquaredDownQuarks[0] = 
 	(ampDownQuarks[0] (ComplexConjugate[ampDownQuarks[0]])) // 
      	FermionSpinSum // DiracSimplify // SUNSimplify // 
   	DoPolarizationSums[#, p, ExtraFactor -> 1/3] & // Simplify
```

$$\frac{2}{27} \sqrt{2} C_A G_F m_Z^2 \left(m_q^2 \left(16 \left(\left.\sin (\theta _W\right)\right){}^4-24 \left(\left.\sin (\theta _W\right)\right){}^2-9\right)+m_Z^2 \left(8 \left(\left.\sin (\theta _W\right)\right){}^4-12 \left(\left.\sin (\theta _W\right)\right){}^2+9\right)\right)$$

## Total decay rates

```mathematica
phaseSpacePrefactor[m1_, m2_, M_] := 1/(16 Pi M) Sqrt[1 - (m1 + m2)^2/M^2]*
   	Sqrt[1 - (m1 - m2)^2/M^2];
```

```mathematica
totalDecayRateLeptonsMassless = phaseSpacePrefactor[0, 0, SMP["m_Z"]]*
   	ampSquaredLeptonsMassless[0] // Simplify
```

$$\frac{G_F m_Z^3}{12 \sqrt{2} \pi }$$

```mathematica
totalDecayRateLeptonsMassive = phaseSpacePrefactor[SMP["m_l"], SMP["m_l"], SMP["m_Z"]]*
   	ampSquaredLeptonsMassive[0] // Simplify
```

$$\frac{G_F m_Z \sqrt{\frac{1}{2}-\frac{2 m_l^2}{m_Z^2}} \left(m_l^2 \left(16 \left(\left.\sin (\theta _W\right)\right){}^4-8 \left(\left.\sin (\theta _W\right)\right){}^2-1\right)+m_Z^2 \left(8 \left(\left.\sin (\theta _W\right)\right){}^4-4 \left(\left.\sin (\theta _W\right)\right){}^2+1\right)\right)}{12 \pi }$$

```mathematica
totalDecayRateUpQuarks = phaseSpacePrefactor[SMP["m_q"], SMP["m_q"], SMP["m_Z"]]*
   	ampSquaredUpQuarks[0] // Simplify
```

$$\frac{C_A G_F m_Z \sqrt{\frac{1}{2}-\frac{2 m_q^2}{m_Z^2}} \left(m_q^2 \left(64 \left(\left.\sin (\theta _W\right)\right){}^4-48 \left(\left.\sin (\theta _W\right)\right){}^2-9\right)+m_Z^2 \left(32 \left(\left.\sin (\theta _W\right)\right){}^4-24 \left(\left.\sin (\theta _W\right)\right){}^2+9\right)\right)}{108 \pi }$$

```mathematica
totalDecayRateDownQuarks = phaseSpacePrefactor[SMP["m_q"], SMP["m_q"], SMP["m_Z"]]*
   	ampSquaredDownQuarks[0] // Simplify
```

$$\frac{C_A G_F m_Z \sqrt{\frac{1}{2}-\frac{2 m_q^2}{m_Z^2}} \left(m_q^2 \left(16 \left(\left.\sin (\theta _W\right)\right){}^4-24 \left(\left.\sin (\theta _W\right)\right){}^2-9\right)+m_Z^2 \left(8 \left(\left.\sin (\theta _W\right)\right){}^4-12 \left(\left.\sin (\theta _W\right)\right){}^2+9\right)\right)}{108 \pi }$$

## Check the final results

```mathematica
tmp = (SMP["G_F"]*Sqrt[1 - (4*mf^2)/SMP["m_Z"]^2]*SMP["m_Z"]^3*
     	(cv^2 (1 + 2 mf^2/SMP["m_Z"]^2) + ca^2 (1 - 4 mf^2/SMP["m_Z"]^2)))/(6*Pi Sqrt[2]);
knownResults = {
   	tmp /. {cv | ca -> 1/2, mf -> 0}, 
   	tmp /. {cv -> -1/2 + 2 SMP["sin_W"]^2, ca -> -1/2, mf -> SMP["m_l"]}, 
   	CA tmp /. {cv -> 1/2 - 4/3 SMP["sin_W"]^2, ca -> 1/2, mf -> SMP["m_q"]}, 
   	CA tmp /. {cv -> -1/2 + 2/3 SMP["sin_W"]^2, ca -> -1/2, mf -> SMP["m_q"]} 
   };
FCCompareResults[{
   	totalDecayRateLeptonsMassless, 
   	totalDecayRateLeptonsMassive, 
   	totalDecayRateUpQuarks, 
   	totalDecayRateDownQuarks}, 
   knownResults, 
   Text -> {"\tCompare to Grozin, Using REDUCE in High Energy Physics, Chapter 5.2:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Grozin, Using REDUCE in High Energy Physics, Chapter 5.2:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }18.707\text{ s.}$$