---
title: Quark-gluon scattering
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Q Gl -> Q Gl with ghosts, QCD, matrix element squared, tree";
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
diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}], V[5]} -> 
     		{F[3, {1}], V[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"]; 
 
Paint[diags, ColumnsXRows -> {2, 2}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 512}];
```

![1ugxu0yezpt32](img/1ugxu0yezpt32.svg)

```mathematica
diagsGh1 = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}], U[5]} -> 
     		{F[3, {1}], U[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"]; 
 
Paint[diagsGh1, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![0imattdskb2fc](img/0imattdskb2fc.svg)

```mathematica
diagsGh2 = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}], -U[5]} -> 
     		{F[3, {1}], -U[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"]; 
 
Paint[diagsGh2, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![1pwrygnu6erz8](img/1pwrygnu6erz8.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p1, k1}, 
  	OutgoingMomenta -> {p2, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	List -> False, SMP -> True, Contract -> True, DropSumOver -> True]
```

$$-\frac{g_s^2 T_{\text{Col5}\;\text{Col1}}^{\text{Glu2}} T_{\text{Col3}\;\text{Col5}}^{\text{Glu4}} \left(\varphi (\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_2\right)\right).\left(\bar{\gamma }\cdot \left(\overline{k_2}+\overline{p_2}\right)+m_u\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }\left(k_1\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(-\overline{k_2}-\overline{p_2}){}^2-m_u^2}-\frac{g_s^2 T_{\text{Col5}\;\text{Col1}}^{\text{Glu4}} T_{\text{Col3}\;\text{Col5}}^{\text{Glu2}} \left(\varphi (\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }\left(k_1\right)\right).\left(\bar{\gamma }\cdot \left(\overline{p_2}-\overline{k_1}\right)+m_u\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_2\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{k_1}-\overline{p_2}){}^2-m_u^2}+\frac{i g_s^2 T_{\text{Col3}\;\text{Col1}}^{\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}} \left(2 \left(\overline{k_2}\cdot \bar{\varepsilon }\left(k_1\right)\right)-\overline{k_1}\cdot \bar{\varepsilon }\left(k_1\right)\right) \left(\varphi (\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_2\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{k_2}-\overline{k_1}){}^2}+\frac{i g_s^2 T_{\text{Col3}\;\text{Col1}}^{\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}} \left(2 \left(\overline{k_1}\cdot \bar{\varepsilon }^*\left(k_2\right)\right)-\overline{k_2}\cdot \bar{\varepsilon }^*\left(k_2\right)\right) \left(\varphi (\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }\left(k_1\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{k_2}-\overline{k_1}){}^2}+\frac{i g_s^2 T_{\text{Col3}\;\text{Col1}}^{\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}} \left(\bar{\varepsilon }\left(k_1\right)\cdot \bar{\varepsilon }^*\left(k_2\right)\right) \left(\varphi (\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \left(-\overline{k_1}-\overline{k_2}\right)\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{k_2}-\overline{k_1}){}^2}$$

```mathematica
ampGh1[0] = FCFAConvert[CreateFeynAmp[diagsGh1], IncomingMomenta -> {p1, k1}, 
  	OutgoingMomenta -> {p2, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	List -> False, SMP -> True, Contract -> True, DropSumOver -> True]
```

$$-\frac{i g_s^2 T_{\text{Col3}\;\text{Col1}}^{\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}} \left(\varphi (\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \overline{k_2}\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{k_2}-\overline{k_1}){}^2}$$

```mathematica
ampGh2[0] = FCFAConvert[CreateFeynAmp[diagsGh2], IncomingMomenta -> {p1, k1}, 
  	OutgoingMomenta -> {p2, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	List -> False, SMP -> True, Contract -> True, DropSumOver -> True]
```

$$\frac{i g_s^2 T_{\text{Col3}\;\text{Col1}}^{\text{Glu5}} f^{\text{Glu2}\;\text{Glu4}\;\text{Glu5}} \left(\varphi (\overline{p_2},m_u)\right).\left(\bar{\gamma }\cdot \overline{k_1}\right).\left(\varphi (\overline{p_1},m_u)\right)}{(\overline{k_2}-\overline{k_1}){}^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, k1, -p2, -k2, SMP["m_u"], 0, SMP["m_u"], 0];
```

## Square the amplitude

```mathematica
ampSquaredUnphys[0] = 1/(SUNN (SUNN^2 - 1)) (amp[0] (ComplexConjugate[amp[0]])) // 
         	FeynAmpDenominatorExplicit // SUNSimplify[#, Explicit -> True, 
          	SUNNToCACF -> False] & // FermionSpinSum[#, ExtraFactor -> 1/2] & // 
      	DiracSimplify // DoPolarizationSums[#, k1, 0] & // DoPolarizationSums[#, k2, 0] & // 
   	TrickMandelstam[#, {s, t, u, 2 SMP["m_u"]^2}] & // Simplify
```

$$-\frac{1}{N^2 t^2 \left(u-m_u^2\right){}^2 \left(s-m_u^2\right){}^2}g_s^4 \left(m_u^8 \left(3 N^2 s^2+37 N^2 s u+3 N^2 u^2+6 t^2\right)-3 N^2 m_u^6 \left(3 s^3+19 s^2 u+19 s u^2+3 u^3\right)+m_u^4 \left(5 N^2 s^4+35 N^2 s^3 u+s^2 \left(43 N^2 u^2-3 t^2\right)-7 s \left(2 t^2 u-5 N^2 u^3\right)+5 N^2 u^4-3 t^2 u^2\right)-m_u^2 (s+u) \left(N^2 s^4+8 N^2 s^3 u-s^2 \left(t^2-5 N^2 u^2\right)+s \left(8 N^2 u^3-6 t^2 u\right)+N^2 u^4-t^2 u^2\right)+9 N^2 m_u^{10} (s+u)-11 N^2 m_u^{12}+s u \left(N^2 s^4-s^2 \left(t^2-3 N^2 u^2\right)+N^2 u^4-t^2 u^2\right)\right)$$

```mathematica
ampSquaredGh1[0] = 1/(SUNN (SUNN^2 - 1)) (ampGh1[0] (ComplexConjugate[ampGh1[0]])) // 
       	FeynAmpDenominatorExplicit // SUNSimplify[#, Explicit -> True, 
        	SUNNToCACF -> False] & // FermionSpinSum[#, ExtraFactor -> 1/2] & // 
    	DiracSimplify // TrickMandelstam[#, {s, t, u, 2 SMP["m_u"]^2}] & // 
  	Simplify
```

$$\frac{g_s^4 \left(m_u^2-u\right) \left(s-m_u^2\right)}{2 t^2}$$

```mathematica
ampSquaredGh2[0] = 1/(SUNN (SUNN^2 - 1)) (ampGh2[0] (ComplexConjugate[ampGh2[0]])) // 
       	FeynAmpDenominatorExplicit // SUNSimplify[#, Explicit -> True, 
        	SUNNToCACF -> False] & // FermionSpinSum[#, ExtraFactor -> 1/2] & // 
    	DiracSimplify // TrickMandelstam[#, {s, t, u, 2 SMP["m_u"]^2}] & // 
  	Simplify
```

$$\frac{g_s^4 \left(m_u^2-u\right) \left(s-m_u^2\right)}{2 t^2}$$

Subtract unphysical degrees of freedom using ghost contributions. Notice that here the averaging over the polarizations of
the initial gluons is done only after the subtraction of the unphysical contributions.

```mathematica
ampSquared[0] = 1/2 (ampSquaredUnphys[0] - ampSquaredGh1[0] - ampSquaredGh2[0]) // 
  	Together
```

$$-\frac{1}{2 N^2 t^2 \left(m_u^2-u\right){}^2 \left(s-m_u^2\right){}^2}g_s^4 \left(-N^2 s^5 m_u^2+5 N^2 s^4 m_u^4-9 N^2 s^4 u m_u^2-10 N^2 s^3 u^2 m_u^2-8 N^2 s^3 m_u^6+32 N^2 s^3 u m_u^4-10 N^2 s^2 u^3 m_u^2+34 N^2 s^2 u^2 m_u^4-48 N^2 s^2 u m_u^6-9 N^2 s u^4 m_u^2+32 N^2 s u^3 m_u^4-48 N^2 s u^2 m_u^6+12 N^2 s m_u^{10}+28 N^2 s u m_u^8-N^2 u^5 m_u^2+5 N^2 u^4 m_u^4-8 N^2 u^3 m_u^6-12 N^2 m_u^{12}+12 N^2 u m_u^{10}+s^3 t^2 m_u^2-3 s^2 t^2 m_u^4+7 s^2 t^2 u m_u^2+7 s t^2 u^2 m_u^2-14 s t^2 u m_u^4+t^2 u^3 m_u^2-3 t^2 u^2 m_u^4+6 t^2 m_u^8+N^2 s^5 u+2 N^2 s^3 u^3+N^2 s u^5-s^3 t^2 u-s t^2 u^3\right)$$

```mathematica
ampSquaredMassless[0] = ampSquared[0] // ReplaceAll[#, {SMP["m_u"] -> 0}] & // 
  	Simplify
```

$$-\frac{g_s^4 \left(s^2+u^2\right) \left(N^2 s^2+N^2 u^2-t^2\right)}{2 N^2 s t^2 u}$$

```mathematica
ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0] /. SUNN -> 3
```

$$-\frac{g_s^4 \left(s^2+u^2\right) \left(9 s^2-t^2+9 u^2\right)}{18 s t^2 u}$$

## Check the final results

```mathematica
knownResults = {
   	-(4/9) SMP["g_s"]^4 (s^2 + u^2)/(s u) + SMP["g_s"]^4 (u^2 + s^2)/(t^2) 
   };
FCCompareResults[{ampSquaredMasslessSUNN3[0]}, {knownResults}, 
  Text -> {"\tCompare to Ellis, Stirling and Weber, QCD and Collider Physics, Table 7.1:", 
    "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}, Factoring -> 
   Function[x, Simplify[TrickMandelstam[x, {s, t, u, 0}]]]]
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Ellis, Stirling and Weber, QCD and Collider Physics, Table 7.1:} \;\text{CORRECT.}$$

$$\text{True}$$

$$\text{$\backslash $tCPU Time used: }27.861\text{ s.}$$