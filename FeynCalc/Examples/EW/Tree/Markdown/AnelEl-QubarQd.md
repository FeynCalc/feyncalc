---
title: Electron electron-antineutrino annihilation into an antiup and a down
quark
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Anel El -> Qubar Qd, EW, total cross section, tree";
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
```

$$\text{FeynCalc }\;\text{10.0.0 (dev version, 2023-12-20 22:40:59 +01:00, dff3b835). For help, use the }\underline{\text{online} \;\text{documentation}}\;\text{, check out the }\underline{\text{wiki}}\;\text{ or visit the }\underline{\text{forum}.}$$

$$\text{Please check our }\underline{\text{FAQ}}\;\text{ for answers to some common FeynCalc questions and have a look at the supplied }\underline{\text{examples}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.11 (3 Aug 2020) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

## Generate Feynman diagrams

Enable CKM mixing

```mathematica
$CKM = True;
```

To avoid dealing with Goldstone bosons we do  the computation in the unitary gauge

```mathematica
InitializeModel[{SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}];
```

```mathematica
diags = InsertFields[CreateTopologies[0, 2 -> 2], {-F[1, 
        		{1}], F[2, {1}]} -> {-F[3, {1}], F[4, {1}]}, InsertionLevel -> {Classes}, 
    		Model -> {SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![1mixmjgiushbu](img/1mixmjgiushbu.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, GaugeRules -> {FAGaugeXi[W | Z] -> Infinity}], 
   DropSumOver -> True, IncomingMomenta -> {q, pe}, OutgoingMomenta -> {l1, l2}, 
   ChangeDimension -> 4, List -> False, SMP -> True, Contract -> True, 
   FinalSubstitutions -> {SMP["e"] -> Sqrt[8/Sqrt[2]*SMP["G_F"]*
        SMP["m_W"]^2 SMP["sin_W"]^2]}]
```

$$-\frac{2 \sqrt{2} G_F m_W^2 V_{\text{ud}}^* \delta _{\text{Col3}\;\text{Col4}} \left(\varphi (-\overline{q})\right).\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^7.\left(\varphi (\overline{\text{pe}},m_e)\right) \left(\varphi (\overline{\text{l2}},m_d)\right).\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^7.\left(\varphi (-\overline{\text{l1}},m_u)\right)}{(\overline{\text{l1}}+\overline{\text{l2}})^2-m_W^2}-\frac{2 \sqrt{2} G_F V_{\text{ud}}^* \delta _{\text{Col3}\;\text{Col4}} \left(\varphi (\overline{\text{l2}},m_d)\right).\left(\bar{\gamma }\cdot \left(\overline{\text{l1}}+\overline{\text{l2}}\right)\right).\bar{\gamma }^7.\left(\varphi (-\overline{\text{l1}},m_u)\right) \left(\varphi (-\overline{q})\right).\left(\bar{\gamma }\cdot \left(-\overline{\text{l1}}-\overline{\text{l2}}\right)\right).\bar{\gamma }^7.\left(\varphi (\overline{\text{pe}},m_e)\right)}{(\overline{\text{l1}}+\overline{\text{l2}})^2-m_W^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, q, pe, -l1, -l2, 0, SMP["m_e"], SMP["m_u"], SMP["m_d"]];
```

## Square the amplitude

There is no polarization averaging for neutrinos here, as right handed neutrinos do not interact

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // 
    	FermionSpinSum[#, ExtraFactor -> 1/2] & // DiracSimplify // Factor
```

$$-4 \frac{1}{(\overline{\text{l1}}+\overline{\text{l2}})^2-m_W^2}{}^2 G_F^2 \left(m_d^8+2 m_e^2 m_d^6-s m_d^6-2 t m_d^6-2 u m_d^6-2 m_u^4 m_d^4-s^2 m_d^4+t^2 m_d^4+u^2 m_d^4-s m_e^2 m_d^4-2 t m_e^2 m_d^4-2 u m_e^2 m_d^4-2 m_e^2 m_u^2 m_d^4-3 s m_u^2 m_d^4+2 t m_u^2 m_d^4+2 u m_u^2 m_d^4+8 m_u^2 m_W^2 m_d^4+2 s m_W^2 m_d^4-2 t m_W^2 m_d^4-2 u m_W^2 m_d^4+2 s t m_d^4+2 s u m_d^4+2 t u m_d^4-2 m_e^2 m_u^4 m_d^2-3 s m_u^4 m_d^2+2 t m_u^4 m_d^2+2 u m_u^4 m_d^2-4 m_e^2 m_W^4 m_d^2-4 m_u^2 m_W^4 m_d^2+4 u m_W^4 m_d^2+s^3 m_d^2-s t^2 m_d^2-s u^2 m_d^2-s^2 m_e^2 m_d^2+2 s t m_e^2 m_d^2+2 s u m_e^2 m_d^2+2 s^2 m_u^2 m_d^2-2 t^2 m_u^2 m_d^2-2 u^2 m_u^2 m_d^2-6 s m_e^2 m_u^2 m_d^2+4 t m_e^2 m_u^2 m_d^2+4 u m_e^2 m_u^2 m_d^2+4 s t m_u^2 m_d^2+4 s u m_u^2 m_d^2-4 t u m_u^2 m_d^2+8 m_u^4 m_W^2 m_d^2-2 s^2 m_W^2 m_d^2+2 t^2 m_W^2 m_d^2+2 u^2 m_W^2 m_d^2+2 s m_e^2 m_W^2 m_d^2-6 t m_e^2 m_W^2 m_d^2-2 u m_e^2 m_W^2 m_d^2+16 m_e^2 m_u^2 m_W^2 m_d^2-4 s m_u^2 m_W^2 m_d^2-12 t m_u^2 m_W^2 m_d^2-12 u m_u^2 m_W^2 m_d^2+4 t u m_W^2 m_d^2-2 s t u m_d^2+m_u^8+2 m_e^2 m_u^6-s m_u^6-2 t m_u^6-2 u m_u^6-s^2 m_u^4+t^2 m_u^4+u^2 m_u^4-s m_e^2 m_u^4-2 t m_e^2 m_u^4-2 u m_e^2 m_u^4+2 s t m_u^4+2 s u m_u^4+2 t u m_u^4-4 u^2 m_W^4+4 u m_e^2 m_W^4+4 u m_u^2 m_W^4+s^3 m_u^2-s t^2 m_u^2-s u^2 m_u^2-s^2 m_e^2 m_u^2+2 s t m_e^2 m_u^2+2 s u m_e^2 m_u^2-2 s t u m_u^2+2 s m_u^4 m_W^2-2 t m_u^4 m_W^2-2 u m_u^4 m_W^2-2 s^2 m_u^2 m_W^2+2 t^2 m_u^2 m_W^2+2 u^2 m_u^2 m_W^2+2 s m_e^2 m_u^2 m_W^2-2 t m_e^2 m_u^2 m_W^2-6 u m_e^2 m_u^2 m_W^2+4 t u m_u^2 m_W^2\right) V_{\text{ud}}^* V_{\text{ud}} \delta _{\text{Col3}\;\text{Col4}}^2$$

In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a very good approximation at low energies, as then (pm-q2)^2  <= m_mu^2 << m_W^2.

```mathematica
ampSquared[1] = ampSquared[0] // FCE // ReplaceAll[#, {l1 + l2 -> 0}] & // 
    	FeynAmpDenominatorExplicit // Series[#, {SMP["m_W"], Infinity, 0}] & // Normal
```

$$16 G_F^2 V_{\text{ud}}^* V_{\text{ud}} \delta _{\text{Col3}\;\text{Col4}}^2 \left(m_d^2 m_e^2-u m_d^2+m_d^2 m_u^2-u m_e^2-u m_u^2+u^2\right)$$

## Total cross-section

We need to carry out the angular integration, so let us specify the values of the temporal and spatial components of the 4-vectors

```mathematica
TC[pe] = (s + SMP["m_e"]^2)/(2 Sqrt[s]);
TC[l1] = (s + (SMP["m_u"]^2 - SMP["m_d"]^2))/(2 Sqrt[s]);
CSP[pe] = (s - SMP["m_e"]^2)^2/(4 s);
CSP[l1] = ((s - SMP["m_u"]^2 - SMP["m_d"]^2)^2 - 4 SMP["m_u"]^2 SMP["m_d"]^2)/(4 s);
```

```mathematica
prefac = 2 Pi/(64 Pi^2 s) Sqrt[CSP[l1]]/Sqrt[CSP[pe]];
```

```mathematica
integral = Collect2[ampSquared[1] /. u -> SMP["m_e"]^2 + SMP["m_u"]^2 - 2 (TC[l1] TC[pe] - Sqrt[CSP[l1]] Sqrt[CSP[pe]] x), x, IsolateNames -> KK] // 
      Integrate[#, {x, -1, 1}] & // FRH // SUNSimplify // Simplify
```

$$\frac{16 C_A G_F^2 V_{\text{ud}}^* V_{\text{ud}} \left(s-m_e^2\right) \left(m_d^2 \left(m_e^2 \left(4 m_u^2+s\right)-s \left(s-2 m_u^2\right)\right)-\left(m_d^4 \left(2 m_e^2+s\right)\right)+\left(s-m_u^2\right) \left(m_e^2 \left(2 m_u^2+s\right)+s \left(m_u^2+2 s\right)\right)\right)}{3 s^2}$$

The total cross-section 

```mathematica
crossSectionTotal = integral*prefac // PowerExpand // Factor2
```

$$\frac{1}{6 \pi  s^3}C_A G_F^2 V_{\text{ud}}^* V_{\text{ud}} \sqrt{\left(-2 m_d m_u-m_d^2-m_u^2+s\right) \left(2 m_d m_u-m_d^2-m_u^2+s\right)} \left(s m_d^2 m_e^2+4 m_d^2 m_e^2 m_u^2-2 m_d^4 m_e^2-s^2 m_d^2+2 s m_d^2 m_u^2-s m_d^4+s^2 m_e^2+s m_e^2 m_u^2-2 m_e^2 m_u^4-s^2 m_u^2-s m_u^4+2 s^3\right)$$

## Check the final results

```mathematica
knownResults = {
   	(CA*SMP["G_F"]^2*Sqrt[(s - SMP["m_d"]^2 - 2*SMP["m_d"]*SMP["m_u"] - 
          	SMP["m_u"]^2)*(s - SMP["m_d"]^2 + 2*SMP["m_d"]*SMP["m_u"] - SMP["m_u"]^2)]*
       (2*s^3 - s^2*SMP["m_d"]^2 - s*SMP["m_d"]^4 + s^2*SMP["m_e"]^2 +
         s*SMP["m_d"]^2*SMP["m_e"]^2 - 2*SMP["m_d"]^4*SMP["m_e"]^2 - 
         s^2*SMP["m_u"]^2 + 2*s*SMP["m_d"]^2*SMP["m_u"]^2 + 
         s*SMP["m_e"]^2*SMP["m_u"]^2 + 4*SMP["m_d"]^2*SMP["m_e"]^2*SMP["m_u"]^2 - 
         s*SMP["m_u"]^4 - 2*SMP["m_e"]^2*SMP["m_u"]^4)*SMP["V_ud", -I]*
       SMP["V_ud", I])/(6*Pi*s^3) 
   };
FCCompareResults[{crossSectionTotal}, 
   knownResults, 
   Text -> {"\tCompare to the known result:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];

```

$$\text{$\backslash $tCompare to the known result:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }28.854\text{ s.}$$