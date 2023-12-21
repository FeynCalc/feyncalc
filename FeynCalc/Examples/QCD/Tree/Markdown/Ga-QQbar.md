---
title: Quark-antiquark production from the decay of a virtual photon
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Ga^* -> Q Qbar, QCD, total decay rate, tree";
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
diags = InsertFields[CreateTopologies[0, 1 -> 2], {V[1]} -> 
    	{F[3, {1}], -F[3, {1}]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![0iknc4lr6azi5](img/0iknc4lr6azi5.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p}, 
  	OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	List -> False, SMP -> True, Contract -> True, DropSumOver -> True, 
  	Prefactor -> 3/2 SMP["e_Q"], FinalSubstitutions -> {SMP["m_u"] -> SMP["m_q"]}]
```

$$\text{e} e_Q \delta _{\text{Col2}\;\text{Col3}} \left(\varphi (\overline{k_1},m_q)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }(p)\right).\left(\varphi (-\overline{k_2},m_q)\right)$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SP[k1] = SMP["m_q"]^2;
SP[k2] = SMP["m_q"]^2;
SP[k1, k2] = (QQ - SP[k1] - SP[k2])/2;
```

## Square the amplitude

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // 
      	FeynAmpDenominatorExplicit // SUNSimplify[#, Explicit -> True, 
       	SUNNToCACF -> False] & // FermionSpinSum // DoPolarizationSums[#, p, 0, 
     	VirtualBoson -> True] & // DiracSimplify
```

$$8 \;\text{e}^2 N e_Q^2 m_q^2+4 \;\text{e}^2 N \;\text{QQ} e_Q^2$$

```mathematica
ampSquaredMassless[0] = ampSquared[0] // ReplaceAll[#, {SMP["m_q"] -> 0}] &
```

$$4 \;\text{e}^2 N \;\text{QQ} e_Q^2$$

```mathematica
ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0] /. SUNN -> 3
```

$$12 \;\text{e}^2 \;\text{QQ} e_Q^2$$

## Total decay rate

The differential decay rate  d Gamma/ d Omega is given by

```mathematica
prefac = ExpandScalarProduct[1/(64 Pi^2) 1/Sqrt[(SP[k1 + k2])]]
```

$$\frac{1}{64 \pi ^2 \sqrt{\text{QQ}}}$$

```mathematica
diffDecayRate = prefac ampSquaredMasslessSUNN3[0] /. 
   SMP["e"]^2 -> (4 Pi SMP["alpha_fs"])
```

$$\frac{3 \alpha  \sqrt{\text{QQ}} e_Q^2}{4 \pi }$$

The total decay-rate

```mathematica
decayRateTotal = 4 Pi diffDecayRate
```

$$3 \alpha  \sqrt{\text{QQ}} e_Q^2$$

Notice that up to the overall color factor 3 and the quark electric charge squared this result is identical to the total decay rate of a virtual photon into a muon-antimuon pair

```mathematica
decayRateTotalQED = SMP["alpha_fs"] Sqrt[QQ]
```

$$\alpha  \sqrt{\text{QQ}}$$

Taking the ration of the two gives us the famous R-ration prediction of the parton mode, where the summation over the quark flavors in front of the charge squared is understood

```mathematica
decayRateTotal/decayRateTotalQED
```

$$3 e_Q^2$$

## Check the final results

```mathematica
knownResults = {
   	3 SMP["alpha_fs"] SMP["e_Q"]^2 Sqrt[QQ] 
   };
FCCompareResults[{decayRateTotal}, knownResults, 
   Text -> {"\tCompare to Field, Applications of Perturbative QCD, Eq. 2.1.30", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Field, Applications of Perturbative QCD, Eq. 2.1.30} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }15.915\text{ s.}$$