---
title: Electron electron-antineutrino annihilation into a muon-antineutrino and a muon
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Anel El -> Anmu Mu, EW, total cross section, tree";
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
MakeBoxes[q1, TraditionalForm] := "\!\(\*SubscriptBox[\(q\), \(1\)]\)";
MakeBoxes[q2, TraditionalForm] := "\!\(\*SubscriptBox[\(q\), \(2\)]\)";
MakeBoxes[pe, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(e\)]\)";
MakeBoxes[pm, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(m\)]\)";
```

To avoid dealing with Goldstone bosons we do  the computation in the unitary gauge

```mathematica
InitializeModel[{SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}];
```

```mathematica
diags = InsertFields[CreateTopologies[0, 2 -> 2], {-F[1, {1}], 
      	F[2, {1}]} -> {-F[1, {2}], F[2, {2}]}, InsertionLevel -> {Classes}, 
    	Model -> {SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![084gon0zi1chz](img/084gon0zi1chz.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, GaugeRules -> {FAGaugeXi[W | Z] -> Infinity}], 
   IncomingMomenta -> {q1, pe}, OutgoingMomenta -> {q2, pm}, ChangeDimension -> 4, List -> False, 
   SMP -> True, Contract -> True, DropSumOver -> True,  
   FinalSubstitutions -> {SMP["e"] -> Sqrt[8/Sqrt[2]*SMP["G_F"]*
        SMP["m_W"]^2 SMP["sin_W"]^2]}]
```

$$-\frac{2 \sqrt{2} G_F m_W^2 \left(\varphi (-\overline{q_1})\right).\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^7.\left(\varphi (\overline{p_e},m_e)\right) \left(\varphi (\overline{p_m},m_{\mu })\right).\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^7.\left(\varphi (-\overline{q_2})\right)}{(\overline{p_m}+\overline{q_2}){}^2-m_W^2}-\frac{2 \sqrt{2} G_F \left(\varphi (\overline{p_m},m_{\mu })\right).\left(\bar{\gamma }\cdot \left(\overline{p_m}+\overline{q_2}\right)\right).\bar{\gamma }^7.\left(\varphi (-\overline{q_2})\right) \left(\varphi (-\overline{q_1})\right).\left(\bar{\gamma }\cdot \left(-\overline{p_m}-\overline{q_2}\right)\right).\bar{\gamma }^7.\left(\varphi (\overline{p_e},m_e)\right)}{(\overline{p_m}+\overline{q_2}){}^2-m_W^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, q1, pe, -q2, -pm, 0, SMP["m_e"], 0, SMP["m_mu"]];
```

## Square the amplitude

There is no polarization averaging for neutrinos here, as right handed neutrinos do not interact

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // 
    	FermionSpinSum[#, ExtraFactor -> 1/2] & // DiracSimplify // Factor
```

$$-4 G_F^2 \left(2 m_e^2 m_{\mu }^6-s^2 m_e^2 m_{\mu }^2-s m_e^2 m_{\mu }^4+2 s t m_e^2 m_{\mu }^2+2 s u m_e^2 m_{\mu }^2+2 s m_e^2 m_{\mu }^2 m_W^2-2 t m_e^2 m_{\mu }^4-6 t m_e^2 m_{\mu }^2 m_W^2-2 u m_e^2 m_{\mu }^4-2 u m_e^2 m_{\mu }^2 m_W^2+4 u m_e^2 m_W^4-4 m_e^2 m_{\mu }^2 m_W^4+m_{\mu }^8+s^3 m_{\mu }^2-s^2 m_{\mu }^4-2 s^2 m_{\mu }^2 m_W^2-s m_{\mu }^6-s t^2 m_{\mu }^2+2 s t m_{\mu }^4-2 s t u m_{\mu }^2-s u^2 m_{\mu }^2+2 s u m_{\mu }^4+2 s m_{\mu }^4 m_W^2+t^2 m_{\mu }^4+2 t^2 m_{\mu }^2 m_W^2-2 t m_{\mu }^6+2 t u m_{\mu }^4+4 t u m_{\mu }^2 m_W^2-2 t m_{\mu }^4 m_W^2+u^2 m_{\mu }^4+2 u^2 m_{\mu }^2 m_W^2-4 u^2 m_W^4-2 u m_{\mu }^6-2 u m_{\mu }^4 m_W^2+4 u m_{\mu }^2 m_W^4\right) \frac{1}{(\overline{p_m}+\overline{q_2}){}^2-m_W^2}{}^2$$

In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a very good approximation at low energies, as then (pm-q2)^2  <= m_mu^2 << m_W^2.

```mathematica
ampSquared[1] = ampSquared[0] // FCE // ReplaceAll[#, {pm + q2 -> 0}] & // 
    	FeynAmpDenominatorExplicit // Series[#, {SMP["m_W"], Infinity, 0}] & // Normal
```

$$16 G_F^2 \left(m_e^2 m_{\mu }^2-u m_e^2-u m_{\mu }^2+u^2\right)$$

## Total cross section

We need to carry out the angular integration, so let us specify the values of the temporal and spatial components of the 4-vectors

```mathematica
TC[q2] = (s - SMP["m_mu"]^2)/(2 Sqrt[s]);
TC[pe] = (s + SMP["m_e"]^2)/(2 Sqrt[s]);
CSP[pe] = (s - SMP["m_e"]^2)^2/(4 s);
CSP[pm] = (s - SMP["m_mu"]^2)^2/(4 s);
CSP[q2] = (s - SMP["m_mu"]^2)^2/(4 s);
```

```mathematica
prefac = 2 Pi/(64 Pi^2 s) Sqrt[(s - SMP["m_mu"]^2)^2]/Sqrt[(s - SMP["m_e"]^2)^2]
```

$$\frac{\sqrt{\left(s-m_{\mu }^2\right){}^2}}{32 \pi  s \sqrt{\left(s-m_e^2\right){}^2}}$$

```mathematica
integral = Integrate[Simplify[ampSquared[1] /. u -> SMP["m_e"]^2 - 
      	2 (TC[q2] TC[pe] - Sqrt[CSP[q2]] Sqrt[CSP[pe]] x)], {x, -1, 1}]
```

$$\frac{16 G_F^2 \left(s-m_e^2\right) \left(s-m_{\mu }^2\right) \left(m_e^2 \left(2 m_{\mu }^2+s\right)+s \left(m_{\mu }^2+2 s\right)\right)}{3 s^2}$$

The total cross-section 

```mathematica
crossSectionTotal = integral*prefac // PowerExpand // Factor2
```

$$\frac{G_F^2 \left(s-m_{\mu }^2\right){}^2 \left(2 m_e^2 m_{\mu }^2+s m_e^2+s m_{\mu }^2+2 s^2\right)}{6 \pi  s^3}$$

## Check the final results

```mathematica
knownResults = {
   	(SMP["G_F"]^2*(s - SMP["m_mu"]^2)^2) (s^2 + (SMP["m_mu"]^2 + SMP["m_e"]^2)/2 s +
       	SMP["m_mu"]^2 SMP["m_e"]^2)/(3 Pi s^3) 
   };
FCCompareResults[{crossSectionTotal}, 
   knownResults, 
   Text -> {"\tCompare to Grozin, Using REDUCE in High Energy Physics, Chapter 5.3:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];

```

$$\text{$\backslash $tCompare to Grozin, Using REDUCE in High Energy Physics, Chapter 5.3:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }21.376\text{ s.}$$