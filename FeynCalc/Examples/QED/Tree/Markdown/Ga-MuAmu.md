---
title: Muon production from the decay of a
virtual photon
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Ga^* -> Mu Amu, QED, total decay rate, tree";
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
MakeBoxes[p, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[k1, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(2\)]\)";
```

```mathematica
diags = InsertFields[CreateTopologies[0, 1 -> 2], {V[1]} -> 
     		{F[2, {2}], -F[2, {2}]}, InsertionLevel -> {Classes}, 
    		Restrictions -> QEDOnly]; 
 
Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![0aj3y7eoib1h7](img/0aj3y7eoib1h7.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p}, 
  	OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	List -> False, SMP -> True, Contract -> True]
```

$$\text{e} \left(-\left(\varphi (\overline{k_1},m_{\mu })\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }\left(p_1\right)\right).\left(\varphi (-\overline{k_2},m_{\mu })\right)\right)$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SP[k1] = SMP["m_mu"]^2;
SP[k2] = SMP["m_mu"]^2;
SP[k1, k2] = (QQ - SP[k1] - SP[k2])/2;
```

## Square the amplitude

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // 
      	FeynAmpDenominatorExplicit // FermionSpinSum // DiracSimplify //
   	DoPolarizationSums[#, p, 0, VirtualBoson -> True] & // Simplify
```

$$4 \;\text{e}^2 \left(2 m_{\mu }^2+\text{QQ}\right)$$

```mathematica
ampSquaredMassless[0] = ampSquared[0] // ReplaceAll[#, {
     	SMP["m_mu"] -> 0}] &
```

$$4 \;\text{e}^2 \;\text{QQ}$$

## Total decay rate

The differential decay rate  d Gamma/ d Omega is given by

```mathematica
prefac = ExpandScalarProduct[1/(64 Pi^2) 1/Sqrt[(SP[k1 + k2])]]
```

$$\frac{1}{64 \pi ^2 \sqrt{\text{QQ}}}$$

```mathematica
diffDecayRate = prefac ampSquaredMassless[0] /. 
   SMP["e"]^2 -> (4 Pi SMP["alpha_fs"])
```

$$\frac{\alpha  \sqrt{\text{QQ}}}{4 \pi }$$

The total decay-rate

```mathematica
decayRateTotal = 4 Pi diffDecayRate
```

$$\alpha  \sqrt{\text{QQ}}$$

## Check the final results

```mathematica
knownResults = {
   	SMP["alpha_fs"] Sqrt[QQ] 
   };
FCCompareResults[{decayRateTotal}, knownResults, 
   Text -> {"\tCompare to Field, Applications of Perturbative QCD, Eq. 2.1.29", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Field, Applications of Perturbative QCD, Eq. 2.1.29} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }17.633\text{ s.}$$