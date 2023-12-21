---
title: Higgs decaying into a fermion-antifermion pair
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "H -> F Fbar, EW, total decay rate, tree";
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
diagsLeptons = InsertFields[CreateTopologies[0, 1 -> 2], {S[1]} -> 
     	{F[2, {l}], -F[2, {l}]}, InsertionLevel -> {Classes}]; 
 
Paint[diagsLeptons, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![10dgl14gz48yi](img/10dgl14gz48yi.svg)

```mathematica
diagsQuarks = InsertFields[CreateTopologies[0, 1 -> 2], {S[1]} -> 
     	{F[3, {l}], -F[3, {l}]}, InsertionLevel -> {Classes}]; 
 
Paint[diagsQuarks, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![0je4j21leizag](img/0je4j21leizag.svg)

## Obtain the amplitudes

```mathematica
ampLeptons[0] = FCFAConvert[CreateFeynAmp[diagsLeptons], IncomingMomenta -> {pH}, 
  	OutgoingMomenta -> {p1, p2}, List -> False, ChangeDimension -> 4, 
  	DropSumOver -> True, SMP -> True, Contract -> True, UndoChiralSplittings -> True, 
  	FinalSubstitutions -> {MLE[l] -> SMP["m_l"]}]
```

$$i \left(\varphi (\overline{\text{p1}},m_l)\right).\left(-\frac{i \;\text{e} m_l}{2 m_W \left(\left.\sin (\theta _W\right)\right)}\right).\left(\varphi (-\overline{\text{p2}},m_l)\right)$$

```mathematica
ampQuarks[0] = FCFAConvert[CreateFeynAmp[diagsQuarks], IncomingMomenta -> {pH}, 
  	OutgoingMomenta -> {k1, k2}, List -> False, ChangeDimension -> 4, 
  	DropSumOver -> True, SMP -> True, Contract -> True, UndoChiralSplittings -> True, 
  	FinalSubstitutions -> {MQU[l] -> SMP["m_q"]}]
```

$$i \left(\varphi (\overline{\text{k1}},m_q)\right).\left(-\frac{i \;\text{e} m_q \delta _{\text{Col2}\;\text{Col3}}}{2 m_W \left(\left.\sin (\theta _W\right)\right)}\right).\left(\varphi (-\overline{\text{k2}},m_q)\right)$$

```mathematica
ampLeptons[1] = ampLeptons[0] // DiracSimplify
```

$$\frac{\text{e} m_l \left(\varphi (\overline{\text{p1}},m_l)\right).\left(\varphi (-\overline{\text{p2}},m_l)\right)}{2 m_W \left(\left.\sin (\theta _W\right)\right)}$$

```mathematica
ampQuarks[1] = ampQuarks[0] // DiracSimplify
```

$$\frac{\text{e} m_q \delta _{\text{Col2}\;\text{Col3}} \left(\varphi (\overline{\text{k1}},m_q)\right).\left(\varphi (-\overline{\text{k2}},m_q)\right)}{2 m_W \left(\left.\sin (\theta _W\right)\right)}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SP[p1, p1] = SMP["m_l"]^2;
SP[k1, k1] = SMP["m_q"]^2;
SP[p2, p2] = SMP["m_l"]^2;
SP[k2, k2] = SMP["m_q"]^2;
SP[pH, pH] = SMP["m_H"]^2;
SP[p1, p2] = (SMP["m_H"]^2 - 2 SMP["m_l"]^2)/2;
SP[k1, k2] = (SMP["m_H"]^2 - 2 SMP["m_q"]^2)/2;
```

## Square the amplitudes

```mathematica
ampLeptonsSquared[0] = ampLeptons[1]*ComplexConjugate[ampLeptons[1]] // 
    	FermionSpinSum // DiracSimplify // Simplify
```

$$\frac{\text{e}^2 m_l^2 \left(m_H^2-4 m_l^2\right)}{2 m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2}$$

```mathematica
ampQuarksSquared[0] = ampQuarks[1]*ComplexConjugate[ampQuarks[1]] // 
    	FermionSpinSum // DiracSimplify // SUNSimplify
```

$$\frac{\text{e}^2 C_A m_q^2 \left(m_H^2-4 m_q^2\right)}{2 m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2}$$

## Total decay rates

```mathematica
$Assumptions = {SMP["m_H"] > 0, SMP["m_l"] > 0};
phaseSpacePrefactor[m_] := 1/(16 Pi SMP["m_H"]) Sqrt[1 - 4 m^2 / SMP["m_H"]^2];
```

```mathematica
totalDecayRateLeptons = phaseSpacePrefactor[SMP["m_l"]] ampLeptonsSquared[0] // 
    ReplaceAll[#, SMP["e"]^2 -> 4 Pi SMP["alpha_fs"]] & // Simplify
```

$$\frac{\alpha  m_l^2 \left(m_H^2-4 m_l^2\right){}^{3/2}}{8 m_H^2 m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2}$$

```mathematica
totalDecayRateQuarks = phaseSpacePrefactor[SMP["m_q"]] ampQuarksSquared[0] // 
    ReplaceAll[#, SMP["e"]^2 -> 4 Pi SMP["alpha_fs"]] & // Simplify
```

$$\frac{\alpha  C_A m_q^2 \left(m_H^2-4 m_q^2\right){}^{3/2}}{8 m_H^2 m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2}$$

```mathematica
(AlphaFS SMP["m_H"])/(8 SMP["sin_W"]^2) ( SMP["m_l"]^2/SMP["m_W"]^2*
     	(1 - 4 SMP["m_l"]^2/SMP["m_H"]^2)^(3/2)) - totalDecayRateLeptons // Factor
```

$$-\frac{\alpha  m_l^2 \left(\sqrt{m_H^2-4 m_l^2}-m_H \sqrt{\frac{m_H^2-4 m_l^2}{m_H^2}}\right) \left(m_H \sqrt{m_H^2-4 m_l^2} \sqrt{\frac{m_H^2-4 m_l^2}{m_H^2}}+2 m_H^2-8 m_l^2\right)}{8 m_H^2 m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2}$$

## Check the final results

```mathematica
knownResults = {
   	(AlphaFS SMP["m_H"])/(8 SMP["sin_W"]^2) ( SMP["m_l"]^2/SMP["m_W"]^2*
      	(1 - 4 SMP["m_l"]^2/SMP["m_H"]^2)^(3/2)), 
   	(CA*SMP["alpha_fs"]*SMP["m_H"]*SMP["m_q"]^2*
       (1 - (4*SMP["m_q"]^2)/SMP["m_H"]^2)^(3/2))/(8*SMP["m_W"]^2*SMP["sin_W"]^2) 
   };
FCCompareResults[{totalDecayRateLeptons, totalDecayRateQuarks}, 
   knownResults, Factoring -> Simplify, 
   Text -> {"\tCompare to Peskin and Schroeder,An Introduction to QFT, Final Project III, part (a):", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Peskin and Schroeder,An Introduction to QFT, Final Project III, part (a):} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }25.599\text{ s.}$$