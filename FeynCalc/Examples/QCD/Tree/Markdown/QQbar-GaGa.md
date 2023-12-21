---
title: Quark-antiquark pair annihilation into photons
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Q Qbar -> Ga Ga, QCD, matrix element squared, tree";
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
diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}], -F[3, {1}]} -> 
     		{V[1], V[1]}, InsertionLevel -> {Classes}, Model -> "SMQCD"]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![00hnmheba4q54](img/00hnmheba4q54.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p1, p2}, 
  	OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	TransversePolarizationVectors -> {k1, k2}, List -> False, SMP -> True, 
  	Contract -> True, DropSumOver -> True, Prefactor -> 9/4 SMP["e_Q"]^2]
```

$$-\frac{\text{e}^2 e_Q^2 \delta _{\text{Col1}\;\text{Col2}} \left(\varphi (-\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_1\right)\right).\left(\bar{\gamma }\cdot \left(\overline{k_1}-\overline{p_2}\right)+m_u\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_2\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{p_2}-\overline{k_1}){}^2-m_u^2}-\frac{\text{e}^2 e_Q^2 \delta _{\text{Col1}\;\text{Col2}} \left(\varphi (-\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_2\right)\right).\left(\bar{\gamma }\cdot \left(\overline{k_2}-\overline{p_2}\right)+m_u\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_1\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{p_2}-\overline{k_2}){}^2-m_u^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_u"], SMP["m_u"], 0, 0];
```

## Square the amplitude

We average over the spins and the colors of the quarks, hence the additional factor 1/N^2*1/2^2. 
Since the final state particles are indistinguishable, we add an extra 1/2

```mathematica
ampSquared[0] = 1/2*1/(SUNN^2) (amp[0] (ComplexConjugate[amp[0]])) // 
         	FeynAmpDenominatorExplicit // SUNSimplify[#, Explicit -> True, 
          	SUNNToCACF -> False] & // FermionSpinSum[#, ExtraFactor -> 1/2^2] & // 
      	DiracSimplify // DoPolarizationSums[#, k1, k2] & // 
    	DoPolarizationSums[#, k2, k1] & // TrickMandelstam[#, {s, t, u, 2  SMP["m_u"]^2}] & // 
  	Simplify
```

$$\frac{\text{e}^4 e_Q^4 \left(m_u^4 \left(3 t^2+14 t u+3 u^2\right)-m_u^2 \left(t^3+7 t^2 u+7 t u^2+u^3\right)-6 m_u^8+t u \left(t^2+u^2\right)\right)}{N \left(u-m_u^2\right){}^2 \left(t-m_u^2\right){}^2}$$

```mathematica
ampSquaredMassless[0] = ampSquared[0] // ReplaceAll[#, {SMP["m_u"] -> 0}] & // 
  	TrickMandelstam[#, {s, t, u, 0}] &
```

$$\frac{\text{e}^4 e_Q^4 \left(t^2+u^2\right)}{N t u}$$

```mathematica
ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0] /. SUNN -> 3
```

$$\frac{\text{e}^4 e_Q^4 \left(t^2+u^2\right)}{3 t u}$$

## Check the final results

```mathematica
knownResults = {
   	((t^2 + u^2)*SMP["e"]^4*SMP["e_Q"]^4)/(3*t*u) 
   };
FCCompareResults[{ampSquaredMasslessSUNN3[0]}, {knownResults}, 
  Text -> {"\tCompare to CalcHEP:", 
    "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];
```

$$\text{$\backslash $tCompare to CalcHEP:} \;\text{CORRECT.}$$

$$\text{True}$$

$$\text{$\backslash $tCPU Time used: }25.286\text{ s.}$$