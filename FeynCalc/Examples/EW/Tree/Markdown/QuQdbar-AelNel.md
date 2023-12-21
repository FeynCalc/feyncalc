---
title: Up quark antidown quark annihilation into a positron and an electron neutrino
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Qu Qdbar -> Ael Nel, EW, matrix element squared, tree";
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
    	{F[3, {1}], -F[4, {1}]} -> {-F[2, {1}], F[1, {1}]}, 
    	InsertionLevel -> {Particles}, Model -> {SM, UnitarySM}, 
    	GenericModel -> {Lorentz, UnitaryLorentz}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![0i1vdr9icjy26](img/0i1vdr9icjy26.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, GaugeRules -> {FAGaugeXi[W | Z] -> Infinity}], 
   IncomingMomenta -> {p1, p2}, OutgoingMomenta -> {k1, k2}, ChangeDimension -> 4, List -> False, 
   SMP -> True, Contract -> True, DropSumOver -> True]
```

$$\frac{\text{e}^2 V_{\text{ud}}^* \delta _{\text{Col1}\;\text{Col2}} \left(\varphi (\overline{k_2})\right).\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^7.\left(\varphi (-\overline{k_1},m_e)\right) \left(\varphi (-\overline{p_2},m_d)\right).\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^7.\left(\varphi (\overline{p_1},m_u)\right)}{2 \left(\left.\sin (\theta _W\right)\right){}^2 \left((\overline{k_1}+\overline{k_2}){}^2-m_W^2\right)}+\frac{\text{e}^2 V_{\text{ud}}^* \delta _{\text{Col1}\;\text{Col2}} \left(\varphi (\overline{k_2})\right).\left(\bar{\gamma }\cdot \left(\overline{k_1}+\overline{k_2}\right)\right).\bar{\gamma }^7.\left(\varphi (-\overline{k_1},m_e)\right) \left(\varphi (-\overline{p_2},m_d)\right).\left(\bar{\gamma }\cdot \left(-\overline{k_1}-\overline{k_2}\right)\right).\bar{\gamma }^7.\left(\varphi (\overline{p_1},m_u)\right)}{2 m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2 \left((\overline{k_1}+\overline{k_2}){}^2-m_W^2\right)}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[]
SetMandelstam[s, t, u, p1, p2, -k1, -k2 , 0, 0, 0, 0];
```

## Square the amplitude

We average over the spins and the colors of the quarks, hence the additional factor 1/3^2 1/2^2.

```mathematica
ampSquared[0] = 1/3^2*(amp[0] (ComplexConjugate[amp[0]])) // 
      	FermionSpinSum[#, ExtraFactor -> 1/2^2] & // DiracSimplify // 
    	FeynAmpDenominatorExplicit // SUNSimplify[#, SUNNToCACF -> False] & // 
  	ReplaceAll[#, SUNN -> 3] &
```

$$\frac{\text{e}^4 t^2 V_{\text{ud}}^* V_{\text{ud}}}{12 \left(s-m_W^2\right){}^2 \left(\left.\sin (\theta _W\right)\right){}^4}$$

## Check the final results

```mathematica
knownResults = {
   	(t^2*SMP["e"]^4*SMP["V_ud", -I]*SMP["V_ud", I])/
    	(12*(s - SMP["m_W"]^2)^2*SMP["sin_W"]^4) 
   };
FCCompareResults[{ampSquared[0]}, 
   knownResults, 
   Text -> {"\tCompare to CompHEP:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];

```

$$\text{$\backslash $tCompare to CompHEP:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }18.927\text{ s.}$$