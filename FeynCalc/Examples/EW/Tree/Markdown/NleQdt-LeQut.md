---
title: Neutrino down-type quark annihilation into a lepton and an up-type quark
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Nle Qdt -> Le Qut, EW, total cross section, tree";
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
MakeBoxes[pu, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(u\)]\)";
MakeBoxes[pd, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(d\)]\)";
MakeBoxes[pn, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(n\)]\)";
MakeBoxes[pl, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(l\)]\)";
```

Enable CKM mixing

```mathematica
$CKM = True;
```

To avoid dealing with Goldstone bosons we do  the computation in the unitary gauge

```mathematica
InitializeModel[{SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}];
```

```mathematica
diags = InsertFields[CreateTopologies[0, 2 -> 2], 
     {F[4, {1, 1}], F[1, {2}]} -> {F[3, {1, 1}], F[2, {2}]}, InsertionLevel -> {Classes}, 
     Model -> {SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![1a21hok1czxcg](img/1a21hok1czxcg.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, GaugeRules -> {FAGaugeXi[W | Z] -> Infinity}], 
   IncomingMomenta -> {pd, pn}, OutgoingMomenta -> {pu, pl}, ChangeDimension -> 4, 
   List -> False, SMP -> True, Contract -> True, DropSumOver -> True,  
   FinalSubstitutions -> {SMP["e"] -> Sqrt[8/Sqrt[2]*SMP["G_F"]*
        SMP["m_W"]^2 SMP["sin_W"]^2]}]
```

$$-\frac{2 \sqrt{2} G_F m_W^2 V_{\text{ud}} \left(\varphi (\overline{p_l},m_{\mu })\right).\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^7.\left(\varphi (\overline{p_n})\right) \left(\varphi (\overline{p_u},m_u)\right).\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^7.\left(\varphi (\overline{p_d},m_d)\right)}{(\overline{p_l}-\overline{p_n}){}^2-m_W^2}-\frac{2 \sqrt{2} G_F V_{\text{ud}} \left(\varphi (\overline{p_l},m_{\mu })\right).\left(\bar{\gamma }\cdot \left(\overline{p_l}-\overline{p_n}\right)\right).\bar{\gamma }^7.\left(\varphi (\overline{p_n})\right) \left(\varphi (\overline{p_u},m_u)\right).\left(\bar{\gamma }\cdot \left(\overline{p_n}-\overline{p_l}\right)\right).\bar{\gamma }^7.\left(\varphi (\overline{p_d},m_d)\right)}{(\overline{p_l}-\overline{p_n}){}^2-m_W^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, pd, pn, -pu, -pl, SMP["m_d"], 0, SMP["m_u"], SMP["m_l"]];
```

## Square the amplitude

There is no polarization averaging for neutrinos here, as right handed neutrinos do not interact

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // 
    	FermionSpinSum[#, ExtraFactor -> 1/2] & // DiracSimplify // Factor
```

$$4 G_F^2 V_{\text{ud}}^* V_{\text{ud}} \left(-2 s t m_d^2 m_l^2+2 s m_d^2 m_l^2 m_W^2+2 s m_d^2 m_l^4+t^2 m_d^2 m_l^2-2 t u m_d^2 m_l^2+4 t m_d^2 m_l^2 m_u^2-2 t m_d^2 m_l^2 m_W^2+t m_d^2 m_l^4+6 u m_d^2 m_l^2 m_W^2-8 m_d^2 m_l^2 m_u^2 m_W^2+2 u m_d^2 m_l^4-4 m_d^2 m_l^4 m_u^2+4 m_d^2 m_l^2 m_W^4-2 m_d^2 m_l^6-4 s m_d^2 m_W^4+4 m_d^2 m_u^2 m_W^4+s^2 t m_l^2-2 s^2 m_l^2 m_W^2-s^2 m_l^4-2 s t m_l^2 m_u^2+2 s t u m_l^2-2 s t m_l^4+6 s m_l^2 m_u^2 m_W^2-4 s u m_l^2 m_W^2+2 s m_l^4 m_u^2-2 s u m_l^4+2 s m_l^4 m_W^2-4 s m_l^2 m_W^4+2 s m_l^6-t^3 m_l^2+t^2 m_l^2 m_u^2+2 t^2 m_l^2 m_W^2+t^2 m_l^4+t u^2 m_l^2-2 t m_l^2 m_u^2 m_W^2+t m_l^4 m_u^2-2 t u m_l^4-2 t u m_l^2 m_u^2-2 t m_l^4 m_W^2+t m_l^6-2 u^2 m_l^2 m_W^2-u^2 m_l^4+2 u m_l^4 m_W^2+2 u m_l^2 m_u^2 m_W^2-2 m_l^6 m_u^2+2 u m_l^6+2 u m_l^4 m_u^2-m_l^8+4 s^2 m_W^4-4 s m_u^2 m_W^4\right) \frac{1}{(\overline{p_l}-\overline{p_n}){}^2-m_W^2}{}^2$$

In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a very good approximation at low energies, as then (pl-pn)^2  <= m_mu^2 << m_W^2.

```mathematica
ampSquared[1] = ampSquared[0] // FCE // ReplaceAll[#, {pl - pn -> 0}] & // 
    	FeynAmpDenominatorExplicit // Series[#, {SMP["m_W"], Infinity, 0}] & // Normal
```

$$16 G_F^2 V_{\text{ud}}^* V_{\text{ud}} \left(m_d^2 m_l^2-s m_d^2+m_d^2 m_u^2-s m_l^2-s m_u^2+s^2\right)$$

## Total cross section

The total cross-section 

```mathematica
prefac = 4 Pi/(64 Pi^2 s) Sqrt[(s - SMP["m_l"]^2 - SMP["m_u"]^2)^2 - 4 SMP["m_l"]^2 *
      	SMP["m_u"]^2]/Sqrt[(s - SMP["m_d"]^2)^2]
```

$$\frac{\sqrt{\left(-m_l^2-m_u^2+s\right){}^2-4 m_l^2 m_u^2}}{16 \pi  s \sqrt{\left(s-m_d^2\right){}^2}}$$

```mathematica
crossSectionTotal = prefac*ampSquared[1] // PowerExpand
```

$$\frac{G_F^2 V_{\text{ud}}^* V_{\text{ud}} \sqrt{\left(-m_l^2-m_u^2+s\right){}^2-4 m_l^2 m_u^2} \left(m_d^2 m_l^2-s m_d^2+m_d^2 m_u^2-s m_l^2-s m_u^2+s^2\right)}{\pi  s \left(s-m_d^2\right)}$$

If the lepton is a muon, then the up quark mass can be neglected

```mathematica
crossSectionTotalMuon = PowerExpand[crossSectionTotal /. {SMP["m_u"] -> 0, 
    	SMP["m_l"] -> SMP["m_mu"]}]
```

$$\frac{G_F^2 V_{\text{ud}}^* V_{\text{ud}} \left(s-m_{\mu }^2\right) \left(m_d^2 m_{\mu }^2-s m_d^2-s m_{\mu }^2+s^2\right)}{\pi  s \left(s-m_d^2\right)}$$

## Check the final results

```mathematica
knownResults = {
   	(SMP["G_F"]^2*(s - SMP["m_mu"]^2)^2*SMP["V_ud", -I]*SMP["V_ud", I])/(Pi*s) 
   };
FCCompareResults[{crossSectionTotalMuon}, 
   knownResults, 
   Text -> {"\tCompare to Grozin, Using REDUCE in High Energy Physics, Chapter 5.3:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];

```

$$\text{$\backslash $tCompare to Grozin, Using REDUCE in High Energy Physics, Chapter 5.3:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }21.179\text{ s.}$$