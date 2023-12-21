---
title: Pair annihilation
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "El Ael -> Ga Ga, QED, matrix element squared, tree";
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
MakeBoxes[p1, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(2\)]\)";
MakeBoxes[k1, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(2\)]\)";
```

```mathematica
diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[2, {1}], -F[2, {1}]} -> 
     		{V[1], V[1]}, InsertionLevel -> {Classes}, 
    		Restrictions -> QEDOnly]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![04qwebekrexaj](img/04qwebekrexaj.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p1, p2}, 
  	OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	TransversePolarizationVectors -> {k1, k2}, List -> False, SMP -> True, 
  	Contract -> True]
```

$$-\frac{\text{e}^2 \left(\varphi (-\overline{p_2},m_e)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_1\right)\right).\left(\bar{\gamma }\cdot \left(\overline{k_1}-\overline{p_2}\right)+m_e\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_2\right)\right).\left(\varphi (\overline{p_1},m_e)\right)}{(\overline{p_2}-\overline{k_1}){}^2-m_e^2}-\frac{\text{e}^2 \left(\varphi (-\overline{p_2},m_e)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_2\right)\right).\left(\bar{\gamma }\cdot \left(\overline{k_2}-\overline{p_2}\right)+m_e\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_1\right)\right).\left(\varphi (\overline{p_1},m_e)\right)}{(\overline{p_2}-\overline{k_2}){}^2-m_e^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_e"], SMP["m_e"], 0, 0];
```

## Square the amplitude

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // 
        	FeynAmpDenominatorExplicit // 
       	DoPolarizationSums[#, k1, 0] & // DoPolarizationSums[#, k2, 0] & // 
     	FermionSpinSum[#, ExtraFactor -> 1/2^2] & // 
    	DiracSimplify // 
   	TrickMandelstam[#, {s, t, u, 2 SMP["m_e"]^2}] & // Simplify
```

$$\frac{2 \;\text{e}^4 \left(m_e^4 \left(3 t^2+14 t u+3 u^2\right)-m_e^2 \left(t^3+7 t^2 u+7 t u^2+u^3\right)-6 m_e^8+t u \left(t^2+u^2\right)\right)}{\left(t-m_e^2\right){}^2 \left(u-m_e^2\right){}^2}$$

## Check the final results

```mathematica
knownResult = (2 SMP["e"]^4 (SP[p1, k2]/SP[p1, k1] + SP[p1, k1]/SP[p1, k2] + 
      	2 SMP["m_e"]^2 (1/SP[p1, k1] + 1/SP[p1, k2]) - 
      	SMP["m_e"]^4 (1/SP[p1, k1] + 1/SP[p1, k2])^2));
FCCompareResults[ampSquared[0], knownResult, 
   Text -> {"\tCompare to Peskin and Schroeder, An Introduction to QFT, Eq 5.105:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Peskin and Schroeder, An Introduction to QFT, Eq 5.105:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }24.084\text{ s.}$$