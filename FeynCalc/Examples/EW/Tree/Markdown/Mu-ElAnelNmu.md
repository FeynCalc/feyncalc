---
title: Muon decaying into an electron, electron-antineutrino and a muon-neutrino
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Mu -> El Anel Nmu, EW, total decay rate, tree";
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
```

To avoid dealing with Goldstone bosons we do  the computation in the unitary gauge

```mathematica
InitializeModel[{SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}];
```

```mathematica
diags = InsertFields[CreateTopologies[0, 1 -> 3], 
    	{F[2, {2}]} -> {F[2, {1}], -F[1, {1}], F[1, {2}]}, InsertionLevel -> {Classes}, 
    		Model -> {SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![16ieixv8knjj3](img/16ieixv8knjj3.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, GaugeRules -> {FAGaugeXi[W | Z] -> Infinity}], 
   IncomingMomenta -> {p}, OutgoingMomenta -> {k, q1, q2}, ChangeDimension -> 4, List -> False, 
   SMP -> True, Contract -> True, DropSumOver -> True,  
   FinalSubstitutions -> {SMP["e"] -> Sqrt[8/Sqrt[2]*SMP["G_F"]*
        SMP["m_W"]^2 SMP["sin_W"]^2]}]
```

$$-\frac{2 \sqrt{2} G_F m_W^2 \left(\varphi (\overline{k},m_e)\right).\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^7.\left(\varphi (-\overline{q_1})\right) \left(\varphi (\overline{q_2})\right).\bar{\gamma }^{\text{Lor1}}.\bar{\gamma }^7.\left(\varphi (\overline{p},m_{\mu })\right)}{(\overline{k}+\overline{q_1}){}^2-m_W^2}-\frac{2 \sqrt{2} G_F \left(\varphi (\overline{k},m_e)\right).\left(\bar{\gamma }\cdot \left(\overline{k}+\overline{q_1}\right)\right).\bar{\gamma }^7.\left(\varphi (-\overline{q_1})\right) \left(\varphi (\overline{q_2})\right).\left(\bar{\gamma }\cdot \left(-\overline{k}-\overline{q_1}\right)\right).\bar{\gamma }^7.\left(\varphi (\overline{p},m_{\mu })\right)}{(\overline{k}+\overline{q_1}){}^2-m_W^2}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[]
SP[k, k] = SMP["m_e"]^2;
SP[q1, q1] = 0;
SP[q2, q2] = 0;
```

## Square the amplitude

We average over the polarizations of the muon, hence the additional factor 1/2

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // 
    	FermionSpinSum[#, ExtraFactor -> 1/2] & // DiracSimplify // Factor
```

$$16 G_F^2 \frac{1}{(\overline{k}+\overline{q_1}){}^2-m_W^2}{}^2 \left(-2 m_e^2 \left(\overline{p}\cdot \overline{q_2}\right) (\overline{k}\cdot \overline{q_1}){}^2-2 m_e^2 m_W^2 \left(\overline{k}\cdot \overline{q_2}\right) \left(\overline{p}\cdot \overline{q_1}\right)+2 m_e^2 m_W^2 \left(\overline{k}\cdot \overline{q_1}\right) \left(\overline{p}\cdot \overline{q_2}\right)-2 m_e^2 m_W^2 \left(\overline{k}\cdot \overline{p}\right) \left(\overline{q_1}\cdot \overline{q_2}\right)+m_e^4 \left(-\left(\overline{k}\cdot \overline{q_1}\right)\right) \left(\overline{p}\cdot \overline{q_2}\right)+2 m_e^2 \left(\overline{k}\cdot \overline{p}\right) \left(\overline{k}\cdot \overline{q_1}\right) \left(\overline{k}\cdot \overline{q_2}\right)+2 m_e^2 \left(\overline{k}\cdot \overline{q_1}\right) \left(\overline{k}\cdot \overline{q_2}\right) \left(\overline{p}\cdot \overline{q_1}\right)+2 m_e^2 \left(\overline{k}\cdot \overline{p}\right) \left(\overline{k}\cdot \overline{q_1}\right) \left(\overline{q_1}\cdot \overline{q_2}\right)+2 m_e^2 \left(\overline{k}\cdot \overline{q_1}\right) \left(\overline{p}\cdot \overline{q_1}\right) \left(\overline{q_1}\cdot \overline{q_2}\right)-4 m_e^2 m_W^2 \left(\overline{p}\cdot \overline{q_1}\right) \left(\overline{q_1}\cdot \overline{q_2}\right)+4 m_W^4 \left(\overline{k}\cdot \overline{q_2}\right) \left(\overline{p}\cdot \overline{q_1}\right)\right)$$

In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a very good approximation at low energies, as then (k+q1)^2  <= m_mu^2 << m_W^2.

```mathematica
ampSquared[1] = ampSquared[0] // FCE // ReplaceAll[#, {k + q1 -> 0}] & // 
    	FeynAmpDenominatorExplicit // Series[#, {SMP["m_W"], Infinity, 0}] & // Normal
```

$$64 G_F^2 \left(\overline{k}\cdot \overline{q_2}\right) \left(\overline{p}\cdot \overline{q_1}\right)$$

## Total decay rate

To compute the total decay rate, we follow the calculation done in Okun, Leptons and Quarks, Chapter 3. The differential decay rate is given by

d Γ = 1/(2M) d^3 k / ((2π)^3 2 k^0) d^3 q1 / ((2π)^3 2 q1^0) d^3 q2 / ((2π)^3 2 q2^0) (2π)^4 δ^4 (q-q1-q2) sqAmpMuonDecayTree2 with q = p-k

```mathematica
prefac = (2 SMP["m_mu"] (2 Pi)^5 8)^-1;
diffDecayRate = prefac d3q1/En[q1] d3q2/En[q2] d3k/En[k] delta4[q - q1 - q2]*
  	ampSquared[1]
```

$$\frac{\text{d3k} \;\text{d3q1} \;\text{d3q2} G_F^2 \left(\overline{k}\cdot \overline{q_2}\right) \left(\overline{p}\cdot \overline{q_1}\right) \;\text{delta4}\left(q-q_1-q_2\right)}{8 \pi ^5 \;\text{En}(k) \;\text{En}\left(q_1\right) \;\text{En}\left(q_2\right) m_{\mu }}$$

First we reduce the tensor integrals in q1 to q2 to scalar ones by using tensor decomposition

```mathematica
q1q2[mu_, nu_] := ReplaceAll[Tdec[{{q1x, mu}, {q2x, nu}}, {q}, List -> False, Dimension -> 4], 
    {SP[q1x, q2x] -> SP[q, q]/2, SP[q, q1x | q2x] -> SP[q, q]/2}];
```

```mathematica
q1q2[mu, nu] // Factor2
```

$$\frac{1}{12} \left(\overline{q}^2 \bar{g}^{\text{mu}\;\text{nu}}+2 \overline{q}^{\text{mu}} \overline{q}^{\text{nu}}\right)$$

```mathematica
diffDecayRate1 = Uncontract[diffDecayRate, q1, q2, Pair -> All]
```

$$\frac{\text{d3k} \;\text{d3q1} \;\text{d3q2} G_F^2 \;\text{delta4}\left(q-q_1-q_2\right) \overline{k}^{\text{\$AL}(\text{\$77})} \overline{p}^{\text{\$AL}(\text{\$78})} \overline{q_1}{}^{\text{\$AL}(\text{\$78})} \overline{q_2}{}^{\text{\$AL}(\text{\$77})}}{8 \pi ^5 \;\text{En}(k) \;\text{En}\left(q_1\right) \;\text{En}\left(q_2\right) m_{\mu }}$$

```mathematica
diffDecayRate2 = ((diffDecayRate1 // FCE) /. FV[q1, mu_] FV[q2, nu_] :> q1q2[mu, nu]) // 
   	Contract // FCE
```

$$\frac{\text{d3k} \;\text{d3q1} \;\text{d3q2} G_F^2 \;\text{delta4}\left(q-q_1-q_2\right) \left(\frac{1}{6} \left(\overline{k}\cdot \overline{q}\right) \left(\overline{p}\cdot \overline{q}\right)+\frac{1}{12} \overline{q}^2 \left(\overline{k}\cdot \overline{p}\right)\right)}{8 \pi ^5 \;\text{En}(k) \;\text{En}\left(q_1\right) \;\text{En}\left(q_2\right) m_{\mu }}$$

Integrating over q1 and q2 (in the rest frame of the decaying muon) we get rid of the Dirac delta and simplify the integral

```mathematica
diffDecayRate3 = diffDecayRate2 /. {d3q2 delta4[q - q1 - q2] -> delta[En[q] - 2 En[q1]]} /. {En[q2] -> En[q1]} /. 
    {d3q1 -> 4 Pi dq10 En[q1]^2} /. {dq10 delta[En[q] - 2 En[q1]] -> 1/2}
```

$$\frac{\text{d3k} G_F^2 \left(\frac{1}{6} \left(\overline{k}\cdot \overline{q}\right) \left(\overline{p}\cdot \overline{q}\right)+\frac{1}{12} \overline{q}^2 \left(\overline{k}\cdot \overline{p}\right)\right)}{4 \pi ^4 \;\text{En}(k) m_{\mu }}$$

Then we use the kinematics of the process, to simplify things even further. Here we also use the fact that the mass of the electron is very small as compared to its energy

```mathematica
diffDecayRate4 = (diffDecayRate3 //. {SP[q, p] -> SMP["m_mu"]^2 - SMP["m_mu"] En[k], SP[k, q] | SP[p, k] -> SMP["m_mu"] En[k], 
      SP[q, q] -> SMP["m_mu"]^2 - 2 SMP["m_mu"] En[k]}) // Simplify
```

$$-\frac{\text{d3k} G_F^2 m_{\mu } \left(4 \;\text{En}(k)-3 m_{\mu }\right)}{48 \pi ^4}$$

Next we trade d3k for dOmega d k^0  (k^0)^2  and introduce Eps  that is defined as 2 k^0/ m_mu

```mathematica
diffDecayRate5 = (diffDecayRate4 /. d3k -> dk0 En[k]^2 4 Pi /. En[k] -> Eps SMP["m_mu"]/2 /. 
     dk0 -> dEps SMP["m_mu"]/2) // Factor2
```

$$\frac{\text{dEps} (3-2 \;\text{Eps}) \;\text{Eps}^2 G_F^2 m_{\mu }^5}{96 \pi ^3}$$

Integrating over Eps we arrive to the final result

```mathematica
decayRateTotal = Integrate[diffDecayRate5 /. dEps -> 1, {Eps, 0, 1}]
```

$$\frac{G_F^2 m_{\mu }^5}{192 \pi ^3}$$

## Check the final results

```mathematica
knownResults = {
   	(SMP["G_F"]^2*SMP["m_mu"]^5)/(192*Pi^3) 
   };
FCCompareResults[{decayRateTotal}, 
   knownResults, 
   Text -> {"\tCompare to Okun, Leptons and Quarks, Chapter 3.2:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];

```

$$\text{$\backslash $tCompare to Okun, Leptons and Quarks, Chapter 3.2:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }17.333\text{ s.}$$