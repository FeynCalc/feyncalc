---
title: Muon production
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "El Ael -> Mu Amu, QED, total cross section, tree";
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
     		{F[2, {2}], -F[2, {2}]}, InsertionLevel -> {Classes}, 
    		Restrictions -> QEDOnly]; 
 
Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![174iss4d43u6t](img/174iss4d43u6t.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p1, p2}, 
  	OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> 4, 
  	List -> False, SMP -> True, Contract -> True]
```

$$-\frac{\text{e}^2 \left(\varphi (-\overline{p_2},m_e)\right).\bar{\gamma }^{\text{Lor1}}.\left(\varphi (\overline{p_1},m_e)\right) \left(\varphi (\overline{k_1},m_{\mu })\right).\bar{\gamma }^{\text{Lor1}}.\left(\varphi (-\overline{k_2},m_{\mu })\right)}{(\overline{k_1}+\overline{k_2}){}^2}$$

Polarized production: the particles are right-handed, while the antiparticles are left-handed

```mathematica
ampPolarized[0] = amp[0] /. {
   	Spinor[-Momentum[k2], r__] :> 
    		GA[6] . Spinor[-Momentum[k2], r], 
   	Spinor[Momentum[k1], r__] :> 
    		Spinor[Momentum[k1], r] . GA[7], 
   	Spinor[Momentum[p1], r__] :> 
    		GA[6] . Spinor[Momentum[p1], r], 
   	Spinor[-Momentum[p2], r__] :> 
    		Spinor[Momentum[p2], r] . GA[7] 
   }
```

$$-\frac{\text{e}^2 \left(\varphi (\overline{p_2},m_e)\right).\bar{\gamma }^7.\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^6.\left(\varphi (\overline{p_1},m_e)\right) \left(\varphi (\overline{k_1},m_{\mu })\right).\bar{\gamma }^7.\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^6.\left(\varphi (-\overline{k_2},m_{\mu })\right)}{(\overline{k_1}+\overline{k_2}){}^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_e"], SMP["m_e"], 
  	SMP["m_mu"], SMP["m_mu"]];
```

## Square the amplitude

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // 
     	FeynAmpDenominatorExplicit // FermionSpinSum[#, ExtraFactor -> 1/2^2] & // 
   	DiracSimplify // Simplify
```

$$\frac{2 \;\text{e}^4 \left(2 m_e^2 \left(2 m_{\mu }^2+s-t-u\right)+2 m_e^4+2 m_{\mu }^4+2 m_{\mu }^2 (s-t-u)+t^2+u^2\right)}{s^2}$$

```mathematica
ampSquaredPolarized[0] = 
 	(ampPolarized[0] (ComplexConjugate[ampPolarized[0]])) // 
     	FeynAmpDenominatorExplicit // FermionSpinSum // DiracSimplify //Simplify
```

$$\frac{4 \;\text{e}^4 \left(m_e^2+m_{\mu }^2-u\right){}^2}{s^2}$$

```mathematica
ampSquaredMassless1[0] = ampSquared[0] // ReplaceAll[#, {
     	SMP["m_e"] -> 0}] &
```

$$\frac{2 \;\text{e}^4 \left(2 m_{\mu }^4+2 m_{\mu }^2 (s-t-u)+t^2+u^2\right)}{s^2}$$

```mathematica
ampSquaredMassless2[0] = ampSquared[0] // ReplaceAll[#, {
      	SMP["m_e"] -> 0, SMP["m_mu"] -> 0}] & // Simplify
```

$$\frac{2 \;\text{e}^4 \left(t^2+u^2\right)}{s^2}$$

```mathematica
ampSquaredPolarizedMassless[0] = ampSquaredPolarized[0] // ReplaceAll[#, {
      	SMP["m_e"] -> 0, SMP["m_mu"] -> 0}] & // Simplify
```

$$\frac{4 \;\text{e}^4 u^2}{s^2}$$

## Total cross-section

The differential cross-section d sigma/ d Omega is given by

```mathematica
prefac1 = 1/(64 Pi^2 s);
```

```mathematica
integral1 = (Factor[ampSquaredMassless2[0] /. {t -> -s/2 (1 - Cos[Th]), u -> -s/2 (1 + Cos[Th]), 
      SMP["e"]^4 -> (4 Pi SMP["alpha_fs"])^2}])
```

$$16 \pi ^2 \alpha ^2 \left(\cos ^2(\text{Th})+1\right)$$

```mathematica
diffXSection1 = prefac1 integral1
```

$$\frac{\alpha ^2 \left(\cos ^2(\text{Th})+1\right)}{4 s}$$

The differential cross-section d sigma/ d t d phi is given by

```mathematica
prefac2 = 1/(128 Pi^2 s)
```

$$\frac{1}{128 \pi ^2 s}$$

```mathematica
integral2 = Simplify[ampSquaredMassless2[0]/(s/4) /. 
   	{u -> -s - t, SMP["e"]^4 -> (4 Pi SMP["alpha_fs"])^2}]
```

$$\frac{128 \pi ^2 \alpha ^2 \left(s^2+2 s t+2 t^2\right)}{s^3}$$

```mathematica
diffXSection2 = prefac2 integral2
```

$$\frac{\alpha ^2 \left(s^2+2 s t+2 t^2\right)}{s^4}$$

The total cross-section. We see that integrating both expressions gives the same result

```mathematica
2 Pi Integrate[diffXSection1 Sin[Th], {Th, 0, Pi}]
```

$$\frac{4 \pi  \alpha ^2}{3 s}$$

```mathematica
crossSectionTotal = 2 Pi Integrate[diffXSection2, {t, -s, 0}]
```

$$\frac{4 \pi  \alpha ^2}{3 s}$$

## Check the final results

```mathematica
knownResults = {
   	(8 SMP["e"]^4 (SP[p1, k1] SP[p2, k2] + SP[p1, k2] SP[p2, k1] + 
          	SMP["m_mu"]^2 SP[p1, p2]))/(SP[p1 + p2])^2 // ExpandScalarProduct // 
    	ReplaceAll[#, SMP["m_e"] -> 0] &, 
   	(16 SMP["e"]^4 (SP[p1, k2] SP[p2, k1]))/(SP[p1 + p2])^2, 
   	((8 SMP["e"]^4/s^2) ((t/2)^2 + (u/2)^2)), (4*Pi*SMP["alpha_fs"]^2)/(3*s) 
   };
FCCompareResults[{ampSquaredMassless1[0], ampSquaredPolarized[0], 
    ampSquaredMassless2[0], crossSectionTotal}, knownResults, 
   Text -> {"\tCompare to Peskin and Schroeder, An Introduction to QFT, Eqs 5.10, 5.21, 5.70 and to Field, Applications of Perturbative QCD, Eq. 2.1.14", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Peskin and Schroeder, An Introduction to QFT, Eqs 5.10, 5.21, 5.70 and to Field, Applications of Perturbative QCD, Eq. 2.1.14} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }22.194\text{ s.}$$