---
title: Quark-antiquark pair annihilation into gluons
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Q Qbar -> Gl Gl, QCD, matrix element squared, tree";
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
     		{V[5], V[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![1mxk9tunkc9hh](img/1mxk9tunkc9hh.svg)

![0t9kx7zt7rmjd](img/0t9kx7zt7rmjd.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p1, p2}, 
  	OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	TransversePolarizationVectors -> {k1, k2}, List -> False, SMP -> True, 
  	Contract -> True, DropSumOver -> True]
```

$$-\frac{g_s^2 T_{\text{Col5}\;\text{Col1}}^{\text{Glu3}} T_{\text{Col2}\;\text{Col5}}^{\text{Glu4}} \left(\varphi (-\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_2\right)\right).\left(\bar{\gamma }\cdot \left(\overline{k_2}-\overline{p_2}\right)+m_u\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_1\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{p_2}-\overline{k_2}){}^2-m_u^2}-\frac{g_s^2 T_{\text{Col5}\;\text{Col1}}^{\text{Glu4}} T_{\text{Col2}\;\text{Col5}}^{\text{Glu3}} \left(\varphi (-\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_1\right)\right).\left(\bar{\gamma }\cdot \left(\overline{k_1}-\overline{p_2}\right)+m_u\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_2\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{p_2}-\overline{k_1}){}^2-m_u^2}-\frac{2 i g_s^2 T_{\text{Col2}\;\text{Col1}}^{\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}} \left(\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_2\right)\right) \left(\varphi (-\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_1\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{k_1}+\overline{k_2}){}^2}+\frac{2 i g_s^2 T_{\text{Col2}\;\text{Col1}}^{\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}} \left(\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_1\right)\right) \left(\varphi (-\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_2\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{k_1}+\overline{k_2}){}^2}+\frac{i g_s^2 T_{\text{Col2}\;\text{Col1}}^{\text{Glu5}} f^{\text{Glu3}\;\text{Glu4}\;\text{Glu5}} \left(\bar{\varepsilon }^*\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_2\right)\right) \left(\varphi (-\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \left(\overline{k_1}-\overline{k_2}\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{k_1}+\overline{k_2}){}^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_u"], SMP["m_u"], 0, 0];
```

## Square the amplitude

```mathematica
ampSquared[0] = 1/(SUNN^2) (amp[0] (ComplexConjugate[amp[0]])) // 
         	FeynAmpDenominatorExplicit // SUNSimplify[#, Explicit -> True, 
          	SUNNToCACF -> False] & // FermionSpinSum[#, ExtraFactor -> 1/2^2] & // 
      	DiracSimplify // DoPolarizationSums[#, k1, k2] & // 
    	DoPolarizationSums[#, k2, k1] & // TrickMandelstam[#, {s, t, u, 2  SMP["m_u"]^2}] & // 
  	Simplify
```

$$\frac{\left(N^2-1\right) g_s^4 \left(m_u^4 \left(3 t^2+14 t u+3 u^2\right)-m_u^2 \left(t^3+7 t^2 u+7 t u^2+u^3\right)-6 m_u^8+t u \left(t^2+u^2\right)\right) \left(-2 N^2 m_u^2 (t+u)+2 N^2 m_u^4+N^2 \left(t^2+u^2\right)-s^2\right)}{2 N^3 s^2 \left(u-m_u^2\right){}^2 \left(t-m_u^2\right){}^2}$$

```mathematica
ampSquaredMassless[0] = ampSquared[0] // ReplaceAll[#, {SMP["m_u"] -> 0}] & // 
  	TrickMandelstam[#, {s, t, u, 0}] &
```

$$\frac{\left(1-N^2\right) g_s^4 \left(t^2+u^2\right) \left(-N^2 t^2-N^2 u^2+s^2\right)}{2 N^3 s^2 t u}$$

```mathematica
ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0] /. SUNN -> 3
```

$$-\frac{4 g_s^4 \left(t^2+u^2\right) \left(s^2-9 t^2-9 u^2\right)}{27 s^2 t u}$$

## Check the final results

```mathematica
knownResults = {
   	(32/27) SMP["g_s"]^4 (t^2 + u^2)/(t u) - (8/3) SMP["g_s"]^4 (t^2 + u^2)/(s^2) 
   };
FCCompareResults[{ampSquaredMasslessSUNN3[0]}, {knownResults}, 
  Text -> {"\tCompare to Ellis, Stirling and Weber, QCD and Collider Physics, Table 7.1:", "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}, Factoring -> 
   Function[x, Simplify[TrickMandelstam[x, {s, t, u, 0}]]]]
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Ellis, Stirling and Weber, QCD and Collider Physics, Table 7.1:} \;\text{CORRECT.}$$

$$\text{True}$$

$$\text{$\backslash $tCPU Time used: }36.75\text{ s.}$$