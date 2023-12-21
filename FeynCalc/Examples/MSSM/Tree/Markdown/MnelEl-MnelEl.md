---
title: Neutralino-electron scattering
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Mnel El -> Mnel El, MSSM, matrix element squared, tree";
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
diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[11, {1}], 
      	F[2, {1}]} -> {F[11, {1}], F[2, {1}]}, InsertionLevel -> {Classes}, 
    	Model -> MSSM, ExcludeParticles -> {S[1], S[2], S[3], S[4], V[_]}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![1uwtptn19or7b](img/1uwtptn19or7b.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p1, p2}, 
   	OutgoingMomenta -> {k1, k2}, ChangeDimension -> 4, List -> False, SMP -> True, 
   	DropSumOver -> True] //. {
   	USf[args1__][args2__] :> 
    	USf[args2, args1], Index[Sfermion, 5] :> Sfe5, 
   	Conjugate[ZNeu[a__]] :> ZNeuC[a], 
   	Conjugate[USf[a_, b_, c_, d_]] :> USfC[a, b, c, d]}
```

$$\frac{1}{(\overline{k_1}-\overline{p_2}){}^2-\text{MSf}(\text{Sfe5},2,1)^2}\left(\varphi (\overline{k_1},\text{MNeu}(1))\right).\left(\frac{i \;\text{e} \bar{\gamma }^7 \left(\left(\left.\cos (\theta _W\right)\right) \left(\text{CB} \;\text{ZNeuC}(1,2) m_W \;\text{USf}(\text{Sfe5},1,2,1)-\text{ZNeuC}(1,3) m_e \;\text{USf}(\text{Sfe5},2,2,1)\right)+\text{CB} \;\text{ZNeuC}(1,1) m_W \;\text{USf}(\text{Sfe5},1,2,1) \left(\left.\sin (\theta _W\right)\right)\right)}{\sqrt{2} \;\text{CB} m_W \left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}-\frac{i \;\text{e} \bar{\gamma }^6 \left(2 \;\text{CB} \;\text{ZNeu}(1,1) m_W \;\text{USf}(\text{Sfe5},2,2,1) \left(\left.\sin (\theta _W\right)\right)+\text{ZNeu}(1,3) m_e \;\text{USf}(\text{Sfe5},1,2,1) \left(\left.\cos (\theta _W\right)\right)\right)}{\sqrt{2} \;\text{CB} m_W \left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}\right).\left(\varphi (\overline{p_2},m_e)\right) \left(\varphi (\overline{k_2},m_e)\right).\left(\frac{i \;\text{e} \bar{\gamma }^6 \left(\text{CB} m_W \;\text{USfC}(\text{Sfe5},1,2,1) \left(\text{ZNeu}(1,2) \left(\left.\cos (\theta _W\right)\right)+\text{ZNeu}(1,1) \left(\left.\sin (\theta _W\right)\right)\right)-\text{ZNeu}(1,3) m_e \;\text{USfC}(\text{Sfe5},2,2,1) \left(\left.\cos (\theta _W\right)\right)\right)}{\sqrt{2} \;\text{CB} m_W \left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}-\frac{i \;\text{e} \bar{\gamma }^7 \left(2 \;\text{CB} \;\text{ZNeuC}(1,1) m_W \;\text{USfC}(\text{Sfe5},2,2,1) \left(\left.\sin (\theta _W\right)\right)+\text{ZNeuC}(1,3) m_e \;\text{USfC}(\text{Sfe5},1,2,1) \left(\left.\cos (\theta _W\right)\right)\right)}{\sqrt{2} \;\text{CB} m_W \left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}\right).\left(\varphi (\overline{p_1},\text{MNeu}(1))\right)-\frac{1}{(\overline{k_1}+\overline{k_2}){}^2-\text{MSf}(\text{Sfe5},2,1)^2}\left(\varphi (\overline{k_2},m_e)\right).\left(\frac{i \;\text{e} \bar{\gamma }^6 \left(\text{CB} m_W \;\text{USfC}(\text{Sfe5},1,2,1) \left(\text{ZNeu}(1,2) \left(\left.\cos (\theta _W\right)\right)+\text{ZNeu}(1,1) \left(\left.\sin (\theta _W\right)\right)\right)-\text{ZNeu}(1,3) m_e \;\text{USfC}(\text{Sfe5},2,2,1) \left(\left.\cos (\theta _W\right)\right)\right)}{\sqrt{2} \;\text{CB} m_W \left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}-\frac{i \;\text{e} \bar{\gamma }^7 \left(2 \;\text{CB} \;\text{ZNeuC}(1,1) m_W \;\text{USfC}(\text{Sfe5},2,2,1) \left(\left.\sin (\theta _W\right)\right)+\text{ZNeuC}(1,3) m_e \;\text{USfC}(\text{Sfe5},1,2,1) \left(\left.\cos (\theta _W\right)\right)\right)}{\sqrt{2} \;\text{CB} m_W \left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}\right).\left(\varphi (-\overline{k_1},\text{MNeu}(1))\right) \left(\varphi (-\overline{p_1},\text{MNeu}(1))\right).\left(\frac{i \;\text{e} \bar{\gamma }^7 \left(\left(\left.\cos (\theta _W\right)\right) \left(\text{CB} \;\text{ZNeuC}(1,2) m_W \;\text{USf}(\text{Sfe5},1,2,1)-\text{ZNeuC}(1,3) m_e \;\text{USf}(\text{Sfe5},2,2,1)\right)+\text{CB} \;\text{ZNeuC}(1,1) m_W \;\text{USf}(\text{Sfe5},1,2,1) \left(\left.\sin (\theta _W\right)\right)\right)}{\sqrt{2} \;\text{CB} m_W \left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}-\frac{i \;\text{e} \bar{\gamma }^6 \left(2 \;\text{CB} \;\text{ZNeu}(1,1) m_W \;\text{USf}(\text{Sfe5},2,2,1) \left(\left.\sin (\theta _W\right)\right)+\text{ZNeu}(1,3) m_e \;\text{USf}(\text{Sfe5},1,2,1) \left(\left.\cos (\theta _W\right)\right)\right)}{\sqrt{2} \;\text{CB} m_W \left(\left.\cos (\theta _W\right)\right) \left(\left.\sin (\theta _W\right)\right)}\right).\left(\varphi (\overline{p_2},m_e)\right)$$

```mathematica
Cases2[amp[0], USf]
```

$$\{\text{USf}(\text{Sfe5},1,2,1),\text{USf}(\text{Sfe5},2,2,1)\}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, MNeu[1], SMP["m_e"], MNeu[1], SMP["m_e"]];
```

## Evaluate the amplitude

```mathematica
amp[1] = DiracSimplify[amp[0]];
```

```mathematica
ampCC[1] = ComplexConjugate[amp[1], Conjugate -> {ZNeuC, ZNeu, USf, USfC}] //. {
    	Conjugate[USf][a_, b_, c_, d_] :> USfC[a, b, c, d], 
    	Conjugate[ZNeu][a__] :> ZNeuC[a], 
    	Conjugate[ZNeuC][a__] :> ZNeu[a], 
    	Conjugate[USfC][a_, b_, c_, d_] :> USf[a, b, c, d], 
    	Sfe5 -> Sfe5c};
```

## Square the amplitude

To avoid having too many terms, we isolate everything that is not required to calculate the spin sums

```mathematica
amp[2] = Collect2[amp[1], Spinor, LorentzIndex, IsolateNames -> KK];
ampCC[2] = Collect2[ampCC[1], Spinor, LorentzIndex, IsolateNames -> KK];
```

```mathematica
ampSquared[0] = (amp[2] ampCC[2]) // FermionSpinSum // DiracSimplify;
```

For simplicity, we neglect the masses of the external particles.

```mathematica
ampSquared[1] = ampSquared[0] // FRH // PropagatorDenominatorExplicit // 
     	ReplaceAll[#, {MNeu[1] -> 0, SMP["m_e"] -> 0}] & // Factor2 // 
   	TrickMandelstam[#, {s, t, u, 0}] &;
```

As we will show below, the pieces that contain a Levi-Civita tensor can be discarded, so we ignore
them in the final result

```mathematica
ampSquared[2] = SelectFree2[ampSquared[1], Eps] // Simplify
```

$$-\left(\left(\text{e}^4 \left(\left(\left(\left(s^2+u^2\right) \;\text{MSf}(\text{Sfe5c},2,1)^2+s t u\right) \;\text{MSf}(\text{Sfe5},2,1)^2+s u \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right)\right) \;\text{USf}(\text{Sfe5},1,2,1) \;\text{USf}(\text{Sfe5c},1,2,1) \;\text{USfC}(\text{Sfe5},1,2,1) \;\text{USfC}(\text{Sfe5c},1,2,1) \;\text{ZNeu}(1,2)^2 \;\text{ZNeuC}(1,2)^2 \left(\left.\cos (\theta _W\right)\right){}^4+2 \left(\left(\left(s^2+u^2\right) \;\text{MSf}(\text{Sfe5c},2,1)^2+s t u\right) \;\text{MSf}(\text{Sfe5},2,1)^2+s u \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right)\right) \left(\left.\sin (\theta _W\right)\right) \;\text{USf}(\text{Sfe5},1,2,1) \;\text{USf}(\text{Sfe5c},1,2,1) \;\text{USfC}(\text{Sfe5},1,2,1) \;\text{USfC}(\text{Sfe5c},1,2,1) \;\text{ZNeu}(1,2) \;\text{ZNeuC}(1,2) (\text{ZNeu}(1,2) \;\text{ZNeuC}(1,1)+\text{ZNeu}(1,1) \;\text{ZNeuC}(1,2)) \left(\left.\cos (\theta _W\right)\right){}^3+\left(\left.\sin (\theta _W\right)\right){}^2 \left(4 \left(t \;\text{MSf}(\text{Sfe5},2,1)^2+2 s u\right) \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right) \;\text{USf}(\text{Sfe5},2,2,1) \;\text{USf}(\text{Sfe5c},1,2,1) \;\text{USfC}(\text{Sfe5},1,2,1) \;\text{USfC}(\text{Sfe5c},2,2,1) \;\text{ZNeu}(1,1) \;\text{ZNeu}(1,2) \;\text{ZNeuC}(1,1) \;\text{ZNeuC}(1,2)+\text{USf}(\text{Sfe5},1,2,1) \;\text{USfC}(\text{Sfe5c},1,2,1) \left(4 \left(t \;\text{MSf}(\text{Sfe5},2,1)^2+2 s u\right) \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right) \;\text{USf}(\text{Sfe5c},2,2,1) \;\text{USfC}(\text{Sfe5},2,2,1) \;\text{ZNeu}(1,1) \;\text{ZNeu}(1,2) \;\text{ZNeuC}(1,1) \;\text{ZNeuC}(1,2)+\left(\left(\left(s^2+u^2\right) \;\text{MSf}(\text{Sfe5c},2,1)^2+s t u\right) \;\text{MSf}(\text{Sfe5},2,1)^2+s u \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right)\right) \;\text{USf}(\text{Sfe5c},1,2,1) \;\text{USfC}(\text{Sfe5},1,2,1) \left(\text{ZNeu}(1,2)^2 \;\text{ZNeuC}(1,1)^2+4 \;\text{ZNeu}(1,1) \;\text{ZNeu}(1,2) \;\text{ZNeuC}(1,2) \;\text{ZNeuC}(1,1)+\text{ZNeu}(1,1)^2 \;\text{ZNeuC}(1,2)^2\right)\right)\right) \left(\left.\cos (\theta _W\right)\right){}^2+2 \left(\left.\sin (\theta _W\right)\right){}^3 \left(\text{USf}(\text{Sfe5},1,2,1) \left(\left(\left(\left(s^2+u^2\right) \;\text{MSf}(\text{Sfe5c},2,1)^2+s t u\right) \;\text{MSf}(\text{Sfe5},2,1)^2+s u \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right)\right) \;\text{USf}(\text{Sfe5c},1,2,1) \;\text{USfC}(\text{Sfe5},1,2,1)+2 \left(t \;\text{MSf}(\text{Sfe5},2,1)^2+2 s u\right) \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right) \;\text{USf}(\text{Sfe5c},2,2,1) \;\text{USfC}(\text{Sfe5},2,2,1)\right) \;\text{USfC}(\text{Sfe5c},1,2,1)+2 \left(t \;\text{MSf}(\text{Sfe5},2,1)^2+2 s u\right) \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right) \;\text{USf}(\text{Sfe5},2,2,1) \;\text{USf}(\text{Sfe5c},1,2,1) \;\text{USfC}(\text{Sfe5},1,2,1) \;\text{USfC}(\text{Sfe5c},2,2,1)\right) \;\text{ZNeu}(1,1) \;\text{ZNeuC}(1,1) (\text{ZNeu}(1,2) \;\text{ZNeuC}(1,1)+\text{ZNeu}(1,1) \;\text{ZNeuC}(1,2)) \left(\left.\cos (\theta _W\right)\right)+\left(\left.\sin (\theta _W\right)\right){}^4 \left(\text{USf}(\text{Sfe5},1,2,1) \left(\left(\left(\left(s^2+u^2\right) \;\text{MSf}(\text{Sfe5c},2,1)^2+s t u\right) \;\text{MSf}(\text{Sfe5},2,1)^2+s u \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right)\right) \;\text{USf}(\text{Sfe5c},1,2,1) \;\text{USfC}(\text{Sfe5},1,2,1)+4 \left(t \;\text{MSf}(\text{Sfe5},2,1)^2+2 s u\right) \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right) \;\text{USf}(\text{Sfe5c},2,2,1) \;\text{USfC}(\text{Sfe5},2,2,1)\right) \;\text{USfC}(\text{Sfe5c},1,2,1)+4 \;\text{USf}(\text{Sfe5},2,2,1) \left(\left(t \;\text{MSf}(\text{Sfe5},2,1)^2+2 s u\right) \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right) \;\text{USf}(\text{Sfe5c},1,2,1) \;\text{USfC}(\text{Sfe5},1,2,1)+4 \left(\left(\left(s^2+u^2\right) \;\text{MSf}(\text{Sfe5c},2,1)^2+s t u\right) \;\text{MSf}(\text{Sfe5},2,1)^2+s u \left(t \;\text{MSf}(\text{Sfe5c},2,1)^2+2 s u\right)\right) \;\text{USf}(\text{Sfe5c},2,2,1) \;\text{USfC}(\text{Sfe5},2,2,1)\right) \;\text{USfC}(\text{Sfe5c},2,2,1)\right) \;\text{ZNeu}(1,1)^2 \;\text{ZNeuC}(1,1)^2\right)\right)/\left(4 \left(s-\text{MSf}(\text{Sfe5},2,1)^2\right) \left(\text{MSf}(\text{Sfe5},2,1)^2-u\right) \left(s-\text{MSf}(\text{Sfe5c},2,1)^2\right) \left(u-\text{MSf}(\text{Sfe5c},2,1)^2\right) \left(\left.\cos (\theta _W\right)\right){}^4 \left(\left.\sin (\theta _W\right)\right){}^4\right)\right)$$

The explicit dependence on the Levi-Civita tensor vanishes once we exploit the unitarity of the
sfermion mixing matrix USf

```mathematica
discarded = SelectNotFree2[ampSquared[1], Eps] // Simplify
```

$$-\left(\left(2 i \;\text{e}^4 \;\text{ZNeu}(1,1) \;\text{ZNeuC}(1,1) (t+2 u) \left(\text{MSf}(\text{Sfe5},2,1)^2-\text{MSf}(\text{Sfe5c},2,1)^2\right) (\text{USf}(\text{Sfe5},1,2,1) \;\text{USfC}(\text{Sfe5},2,2,1) \;\text{USf}(\text{Sfe5c},2,2,1) \;\text{USfC}(\text{Sfe5c},1,2,1)-\text{USf}(\text{Sfe5},2,2,1) \;\text{USfC}(\text{Sfe5},1,2,1) \;\text{USf}(\text{Sfe5c},1,2,1) \;\text{USfC}(\text{Sfe5c},2,2,1)) \bar{\epsilon }^{\overline{k_1}\overline{k_2}\overline{p_1}\overline{p_2}} \left(\text{ZNeu}(1,2) \left(\left.\cos (\theta _W\right)\right)+\text{ZNeu}(1,1) \left(\left.\sin (\theta _W\right)\right)\right) \left(\text{ZNeuC}(1,2) \left(\left.\cos (\theta _W\right)\right)+\text{ZNeuC}(1,1) \left(\left.\sin (\theta _W\right)\right)\right)\right)/\left(\left(\left.\cos (\theta _W\right)\right){}^4 \left(\left.\sin (\theta _W\right)\right){}^2 \left(\text{MSf}(\text{Sfe5},2,1)^2-s\right) \left(s-\text{MSf}(\text{Sfe5c},2,1)^2\right) \left(u-\text{MSf}(\text{Sfe5},2,1)^2\right) \left(u-\text{MSf}(\text{Sfe5c},2,1)^2\right)\right)\right)$$

```mathematica
Sum[discarded, {Sfe5, 1, 2}, {Sfe5c, 1, 2}] // Simplify // ReplaceRepeated[#, 
     {USf[1, 1, re__] USfC[1, 2, re__] :> -USf[2, 1, re] USfC[2, 2, re], 
      USf[1, 2, re__] USfC[1, 1, re__] :> -USf[2, 2, re] USfC[2, 1, re] 
     }] & // Simplify
```

$$0$$

## Check the final results

```mathematica
knownResults = {
   	-(SMP["e"]^4*(SMP["sin_W"]^4*(USf[Sfe5, 1, 2, 1]*((s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1] + 
                       4*(2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5c, 2, 2, 1]*USfC[Sfe5, 2, 2, 1])*USfC[Sfe5c, 1, 2, 1] + 
                  4*USf[Sfe5, 2, 2, 1]*((2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1] + 
                       4*(s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*USf[Sfe5c, 2, 2, 1]*USfC[Sfe5, 2, 2, 1])*USfC[Sfe5c, 2, 2, 1])*ZNeu[1, 1]^2*
               ZNeuC[1, 1]^2 + (s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*SMP["cos_W"]^4*USf[Sfe5, 1, 2, 1]*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1]*
               USfC[Sfe5c, 1, 2, 1]*ZNeu[1, 2]^2*ZNeuC[1, 2]^2 + 2*SMP["cos_W"]*SMP["sin_W"]^3*
               (USf[Sfe5, 1, 2, 1]*((s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1] + 
                       2*(2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5c, 2, 2, 1]*USfC[Sfe5, 2, 2, 1])*USfC[Sfe5c, 1, 2, 1] + 
                  2*(2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5, 2, 2, 1]*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1]*USfC[Sfe5c, 2, 2, 1])*ZNeu[1, 1]*ZNeuC[1, 1]*
               (ZNeu[1, 2]*ZNeuC[1, 1] + ZNeu[1, 1]*ZNeuC[1, 2]) + 2*(s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*SMP["cos_W"]^3*SMP["sin_W"]*
               USf[Sfe5, 1, 2, 1]*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1]*USfC[Sfe5c, 1, 2, 1]*ZNeu[1, 2]*ZNeuC[1, 2]*(ZNeu[1, 2]*ZNeuC[1, 1] + ZNeu[1, 1]*ZNeuC[1, 2]) + 
             SMP["cos_W"]^2*SMP["sin_W"]^2*(4*(2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5, 2, 2, 1]*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1]*USfC[Sfe5c, 2, 2, 1]*ZNeu[1, 1]*
                    ZNeu[1, 2]*ZNeuC[1, 1]*ZNeuC[1, 2] + USf[Sfe5, 1, 2, 1]*USfC[Sfe5c, 1, 2, 1]*(4*(2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5c, 2, 2, 1]*USfC[Sfe5, 2, 2, 1]*
                         ZNeu[1, 1]*ZNeu[1, 2]*ZNeuC[1, 1]*ZNeuC[1, 2] + (s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1]*
                         (ZNeu[1, 2]^2*ZNeuC[1, 1]^2 + 4*ZNeu[1, 1]*ZNeu[1, 2]*ZNeuC[1, 1]*ZNeuC[1, 2] + ZNeu[1, 1]^2*ZNeuC[1, 2]^2)))))/
     (4*(s - MSf[Sfe5, 2, 1]^2)*(-u + MSf[Sfe5, 2, 1]^2)*(s - MSf[Sfe5c, 2, 1]^2)*(u - MSf[Sfe5c, 2, 1]^2)*SMP["cos_W"]^4*SMP["sin_W"]^4) 
   };
FCCompareResults[{ampSquared[2]}, 
   knownResults, 
   Text -> {"\tCompare to FormCalc:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];

```

$$\text{$\backslash $tCompare to FormCalc:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }19.299\text{ s.}$$