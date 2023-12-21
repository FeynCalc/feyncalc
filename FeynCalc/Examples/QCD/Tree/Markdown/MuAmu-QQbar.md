---
title: Quark-antiquark production in muon-antimuon annihilation
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Mu Amu -> Q Qbar, QCD, total cross section, tree";
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
diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[2, {2}], -F[2, {2}]} -> 
     	{F[3, {1}], -F[3, {1}]}, InsertionLevel -> {Classes}, Model -> "SMQCD", 
    	ExcludeParticles -> {S[_], V[2]}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![1vy1iw6ip51gn](img/1vy1iw6ip51gn.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p1, p2}, 
  	OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	List -> False, SMP -> True, Contract -> True, DropSumOver -> True, 
  	Prefactor -> 3/2 SMP["e_Q"]]
```

$$\frac{\text{e}^2 e_Q \delta _{\text{Col3}\;\text{Col4}} \left(\varphi (\overline{k_1},m_u)\right).\bar{\gamma }^{\text{Lor1}}.\left(\varphi (-\overline{k_2},m_u)\right) \left(\varphi (-\overline{p_2},m_{\mu })\right).\bar{\gamma }^{\text{Lor1}}.\left(\varphi (\overline{p_1},m_{\mu })\right)}{(\overline{k_1}+\overline{k_2}){}^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_mu"], SMP["m_mu"], 
  	SMP["m_u"], SMP["m_u"]];
```

## Square the amplitude

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // 
       	FeynAmpDenominatorExplicit // SUNSimplify[#, Explicit -> True, 
        	SUNNToCACF -> False] & // FermionSpinSum[#, ExtraFactor -> 1/2^2] & // 
    	DiracSimplify // 
   	TrickMandelstam[#, {s, t, u, 2 SMP["m_u"]^2 + 2 SMP["m_mu"]^2}] & // Simplify
```

$$\frac{2 \;\text{e}^4 N e_Q^2 \left(2 m_{\mu }^4-4 m_{\mu }^2 \left(u-m_u^2\right)+2 m_u^4-4 u m_u^2+s^2+2 s u+2 u^2\right)}{s^2}$$

```mathematica
ampSquaredMassless[0] = ampSquared[0] // ReplaceAll[#, {SMP["m_u" | "m_mu"] -> 0}] & // 
  	TrickMandelstam[#, {s, t, u, 0}] &
```

$$\frac{2 \;\text{e}^4 N e_Q^2 \left(t^2+u^2\right)}{s^2}$$

```mathematica
ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0] /. SUNN -> 3
```

$$\frac{6 \;\text{e}^4 e_Q^2 \left(t^2+u^2\right)}{s^2}$$

## Total cross-section

```mathematica
integral = Integrate[Simplify[ampSquaredMasslessSUNN3[0]/(s/4) /. 
      u -> -s - t], {t, -s, 0}] /. SMP["e"]^4 -> (4 Pi SMP["alpha_fs"])^2
```

$$256 \pi ^2 \alpha ^2 e_Q^2$$

```mathematica
prefac = 2 Pi/(128 Pi^2 s)
```

$$\frac{1}{64 \pi  s}$$

The total cross-section 

```mathematica
crossSectionTotal = integral*prefac // PowerExpand // Factor2
```

$$\frac{4 \pi  \alpha ^2 e_Q^2}{s}$$

## Check the final results

```mathematica
knownResults = {
   	(6*(t^2 + u^2)*SMP["e"]^4*SMP["e_Q"]^2)/(s^2), 
   	(4*Pi*SMP["alpha_fs"]^2*SMP["e_Q"]^2)/s 
   };
FCCompareResults[{ampSquaredMasslessSUNN3[0], crossSectionTotal}, 
  knownResults, 
  Text -> {"\tCompare to CalcHEP and to Field, Applications of Perturbative QCD, Eq. 2.1.15", 
    "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];
```

$$\text{$\backslash $tCompare to CalcHEP and to Field, Applications of Perturbative QCD, Eq. 2.1.15} \;\text{CORRECT.}$$

$$\text{True}$$

$$\text{$\backslash $tCPU Time used: }21.975\text{ s.}$$