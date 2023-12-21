---
title: Higgs decaying into a Z boson pair
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "H -> Z Z, EW, total decay rate, tree";
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
diags = InsertFields[CreateTopologies[0, 1 -> 2], {S[1]} -> {V[2], V[2]}, 
    	InsertionLevel -> {Classes}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![1hxdkz0wsktq0](img/1hxdkz0wsktq0.svg)

## Obtain the amplitudes

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {pH}, 
  	OutgoingMomenta -> {k1, k2}, List -> False, ChangeDimension -> 4, 
  	DropSumOver -> True, SMP -> True, Contract -> True, UndoChiralSplittings -> True]
```

$$\frac{\text{e} m_W \left(\bar{\varepsilon }^*(\text{k1})\cdot \bar{\varepsilon }^*(\text{k2})\right)}{\left(\left.\cos (\theta _W\right)\right){}^2 \left(\left.\sin (\theta _W\right)\right)}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SP[k1, k1] = SMP["m_Z"]^2;
SP[k2, k2] = SMP["m_Z"]^2;
SP[pH, pH] = SMP["m_H"]^2;
SP[k1, k2] = (SMP["m_H"]^2 - 2 SMP["m_Z"]^2)/2;
```

## Square the amplitudes

```mathematica
ampSquared[0] = 1/2 (amp[0] (ComplexConjugate[amp[0]])) // 
     	FeynAmpDenominatorExplicit // DoPolarizationSums[#, k1] & // 
   	DoPolarizationSums[#, k2] & // Simplify
```

$$\frac{\text{e}^2 m_W^2 \left(-4 m_H^2 m_Z^2+m_H^4+12 m_Z^4\right)}{8 m_Z^4 \left(\left.\cos (\theta _W\right)\right){}^4 \left(\left.\sin (\theta _W\right)\right){}^2}$$

## Total decay rates

```mathematica
$Assumptions = {SMP["m_H"] > 0, SMP["m_Z"] > 0};
phaseSpacePrefactor[m_] := 1/(16 Pi SMP["m_H"]) Sqrt[1 - 4 m^2 / SMP["m_H"]^2];
```

```mathematica
totalDecayRate = phaseSpacePrefactor[SMP["m_Z"]] ampSquared[0] // 
    ReplaceRepeated[#, {SMP["e"]^2 -> 4 Pi SMP["alpha_fs"], 1/SMP["m_Z"]^4 -> 
       	SMP["cos_W"]^4/SMP["m_W"]^4}] & // Simplify
```

$$\frac{\alpha  \sqrt{m_H^2-4 m_Z^2} \left(-4 m_H^2 m_Z^2+m_H^4+12 m_Z^4\right)}{32 m_H^2 m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2}$$

Rewrite the result in a nicer way

```mathematica
(totalDecayRate /. SMP["m_Z"]^2 -> h[SMP["m_Z"]^2/SMP["m_H"]^2] SMP["m_H"]^2 /. 
     SMP["m_Z"]^4 -> h[SMP["m_Z"]^4/SMP["m_H"]^4] SMP["m_H"]^4) // FullSimplify // ReplaceAll[#, h -> Identity] &
```

$$\frac{\alpha  m_H^3 \sqrt{1-\frac{4 m_Z^2}{m_H^2}} \left(\frac{12 m_Z^4}{m_H^4}-\frac{4 m_Z^2}{m_H^2}+1\right)}{32 m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2}$$

## Check the final results

```mathematica
knownResults = {
   	(SMP["alpha_fs"]*SMP["m_H"]^3*Sqrt[1 - (4*SMP["m_Z"]^2)/SMP["m_H"]^2]*
       (1 - (4*SMP["m_Z"]^2)/SMP["m_H"]^2 + (12*SMP["m_Z"]^4)/SMP["m_H"]^4))/
     (32*SMP["m_W"]^2*SMP["sin_W"]^2)};
FCCompareResults[{totalDecayRate}, 
   knownResults, Factoring -> Simplify, 
   Text -> {"\tCompare to Gunion, Haber, Kane and Dawson, Higgs Hunter Guide, Eq 2.10:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Gunion, Haber, Kane and Dawson, Higgs Hunter Guide, Eq 2.10:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }20.691\text{ s.}$$