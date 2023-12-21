---
title: Electron-muon scattering
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "El Mu -> El Mu, QED, matrix element squared, tree";
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
diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[2, {1}], F[2, {2}]} -> 
     		{F[ 2, {1}], F[2, {2}]}, InsertionLevel -> {Classes}, 
    		Restrictions -> QEDOnly]; 
 
Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![08ja8c2z0syw7](img/08ja8c2z0syw7.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p1, p2}, 
  	OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	List -> False, SMP -> True, Contract -> True]
```

$$-\frac{\text{e}^2 \left(\varphi (\overline{k_1},m_e)\right).\bar{\gamma }^{\text{Lor2}}.\left(\varphi (\overline{p_1},m_e)\right) \left(\varphi (\overline{k_2},m_{\mu })\right).\bar{\gamma }^{\text{Lor2}}.\left(\varphi (\overline{p_2},m_{\mu })\right)}{(\overline{k_2}-\overline{p_2}){}^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, 
  	SMP["m_e"], SMP["m_mu"], SMP["m_e"], SMP["m_mu"]];
```

## Square the amplitude

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // 
     	FeynAmpDenominatorExplicit // FermionSpinSum[#, ExtraFactor -> 1/2^2] & // 
   	DiracSimplify // Simplify
```

$$\frac{2 \;\text{e}^4 \left(-2 m_e^2 \left(-2 m_{\mu }^2+s-t+u\right)+2 m_e^4+2 m_{\mu }^4-2 m_{\mu }^2 (s-t+u)+s^2+u^2\right)}{t^2}$$

```mathematica
ampSquaredMassless1[0] = ampSquared[0] // ReplaceAll[#, {SMP["m_e"] -> 0}] & // 
  	Simplify
```

$$\frac{2 \;\text{e}^4 \left(2 m_{\mu }^4-2 m_{\mu }^2 (s-t+u)+s^2+u^2\right)}{t^2}$$

```mathematica
ampSquaredMassless2[0] = ampSquared[0] // ReplaceAll[#, {
      	SMP["m_e"] -> 0, SMP["m_mu"] -> 0}] & // Simplify
```

$$\frac{2 \;\text{e}^4 \left(s^2+u^2\right)}{t^2}$$

## Check the final results

```mathematica
knownResults = {
   	(8 SMP["e"]^4 (SP[p1, k2] SP[p2, k1] + SP[p1, p2] SP[k1, k2] - 
          	SMP["m_mu"]^2 SP[p1, k1]))/(SP[k1 - p1])^2 // ExpandScalarProduct // 
    	ReplaceAll[#, SMP["m_e"] -> 0] &, 
   	((8 SMP["e"]^4/t^2) ((s/2)^2 + (u/2)^2)) 
   };
FCCompareResults[{ampSquaredMassless1[0], ampSquaredMassless2[0]}, knownResults, 
  Text -> {"\tCompare to Peskin and Schroeder, An Introduction to QFT, Eqs 5.61 and 5.71:", 
    "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Peskin and Schroeder, An Introduction to QFT, Eqs 5.61 and 5.71:} \;\text{CORRECT.}$$

$$\text{True}$$

$$\text{$\backslash $tCPU Time used: }20.121\text{ s.}$$