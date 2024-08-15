---
title: Muon production
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "El Ael -> Mu Amu, QED, Born-virtual, 1-loop";
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
diagsTree = InsertFields[CreateTopologies[0, 2 -> 2, 
    	ExcludeTopologies -> {Tadpoles, WFCorrections}], {F[2, {1}], -F[2, {1}]} -> 
    	{F[2, {2}], -F[2, {2}]}, InsertionLevel -> {Particles}, 
   	Restrictions -> QEDOnly, ExcludeParticles -> {F[1 | 3 | 4, _], F[2, {3}]}];
Paint[diagsTree, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {1024, 256}];
```

![0758qtt3uf52x](img/0758qtt3uf52x.svg)

```mathematica
diagsLoop = InsertFields[CreateTopologies[1, 2 -> 2, 
    	ExcludeTopologies -> {Tadpoles, WFCorrections}], {F[2, {1}], -F[2, {1}]} -> 
    	{F[2, {2}], -F[2, {2}]}, InsertionLevel -> {Particles}, 
   	Restrictions -> QEDOnly, ExcludeParticles -> {F[1 | 3 | 4, _], F[2, {3}]}];
Paint[DiagramExtract[diagsLoop, 1 .. 5], ColumnsXRows -> {5, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {1024, 196}];
```

![02p122cn86s4z](img/02p122cn86s4z.svg)

```mathematica
diagsLoopCT = InsertFields[CreateCTTopologies[1, 2 -> 2, ExcludeTopologies -> {Tadpoles, WFCorrectionCTs}], 
    		{F[2, {1}], -F[2, {1}]} -> {F[2, {2}], -F[2, {2}]}, InsertionLevel -> {Particles}, 
    		Restrictions -> QEDOnly, ExcludeParticles -> {F[1 | 3 | 4, _], F[2, {3}]}]; 
 
Paint[diagsLoopCT, ColumnsXRows -> {3, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {1024, 196}];
```

![017zis4550exy](img/017zis4550exy.svg)

## Obtain the amplitudes

```mathematica
ampLoopCT[0] = FCFAConvert[CreateFeynAmp[diagsLoopCT, Truncated -> False, PreFactor -> 1] //. 
     {(h : dZfL1 | dZfR1)[z__] :> dZf1[z], Conjugate[(h : dZfL1 | dZfR1)[z__]] :> dZf1[z], dZZA1 -> 0}, 
    IncomingMomenta -> {p1, p2}, OutgoingMomenta -> {k1, k2}, LoopMomenta -> {l}, ChangeDimension -> D, 
    DropSumOver -> True, UndoChiralSplittings -> True, SMP -> True, 
    FinalSubstitutions -> {SMP["m_e"] -> 0, SMP["m_mu"] -> 0}];
```

```mathematica
ampLoop[0] = FCFAConvert[CreateFeynAmp[DiagramExtract[diagsLoop, 1 .. 5], 
    	Truncated -> False, PreFactor -> 1], IncomingMomenta -> {p1, p2},OutgoingMomenta -> {k1, k2}, 
   	LoopMomenta -> {q}, ChangeDimension -> D, DropSumOver -> True, UndoChiralSplittings -> True, 
   	SMP -> True, FinalSubstitutions -> {SMP["m_e"] -> 0, SMP["m_mu"] -> 0}];
```

```mathematica
ampTree[0] = FCFAConvert[CreateFeynAmp[diagsTree, Truncated -> False, PreFactor -> 1], 
   	IncomingMomenta -> {p1, p2}, OutgoingMomenta -> {k1, k2}, 
   	ChangeDimension -> D, DropSumOver -> True, UndoChiralSplittings -> True, 
   	SMP -> True, FinalSubstitutions -> {SMP["m_e"] -> 0, SMP["m_mu"] -> 0}];
```

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, 0, 0, 0, 0];
```

## Evaluate the amplitudes

```mathematica
$KeepLogDivergentScalelessIntegrals = True;
```

```mathematica
ampLoop[1] = 
  	(FCTraceFactor /@ DotSimplify[#, Expanding -> False] & /@ Join[ampLoop[0][[1 ;; 4]], Nf ampLoop[0][[5 ;; 5]]]);
```

```mathematica
ampTree[1] = 
  	(FCTraceFactor /@ DotSimplify[#, Expanding -> False] & /@ ampTree[0]);
```

```mathematica
ampLoopCT[1] = 
  	(FCTraceFactor /@ DotSimplify[#, Expanding -> False] & /@ ampLoopCT[0]);
```

```mathematica
evalFuSimple[ex_] := ex // Contract // DiracSimplify // TID[#, q, ToPaVe -> True] & // 
       	DiracSimplify // Contract // ReplaceAll[#, (h : A0 | B0 | C0 | D0)[x__] :> 
        	TrickMandelstam[h[x], {s, t, u, 0}]] & // 
    	FeynAmpDenominatorExplicit // Collect2[#, {A0, B0, C0, D0}, 
     	Factoring -> Function[x, Factor2[TrickMandelstam[x, {s, t, u, 0}]]]] &;
```

```mathematica
(*about 50 seconds*)
  AbsoluteTiming[ampLoop[2] = evalFuSimple /@ ampLoop[1];]
```

$$\{34.7495,\text{Null}\}$$

```mathematica
ampTree[2] = (Total[ampTree[1]] // Contract // DiracSimplify) // FeynAmpDenominatorExplicit // 
  	FCCanonicalizeDummyIndices[#, LorentzIndexNames -> {mu}] &
```

$$-\frac{i \;\text{e}^2 \left(\varphi (k_1)\right).\gamma ^{\text{mu}}.\left(\varphi (-k_2)\right) \left(\varphi (-p_2)\right).\gamma ^{\text{mu}}.\left(\varphi (p_1)\right)}{s}$$

Obtain the Born-virtual interference term

```mathematica
(*about 3 seconds*)
  AbsoluteTiming[bornVirtualUnrenormalized[0] = 
    	Collect2[Total[ampLoop[2]], Spinor, LorentzIndex, IsolateNames -> KK] *
          	ComplexConjugate[ampTree[2]] // 
         	FermionSpinSum[#, ExtraFactor -> 1/2^2] & // DiracSimplify //
       	FRH // TrickMandelstam[#, {s, t, u, 0}] & // Collect2[#, B0, C0, D0] &;]
```

$$\{3.91418,\text{Null}\}$$

The explicit expressions for the PaVe functions can be obtained e.g. using Package-X / PaXEvaluate

```mathematica
PaVeEvalRules = {
    B0[0, 0, 0] -> -1/(16*EpsilonIR*Pi^4) + 1/(16*EpsilonUV*Pi^4), 
    B0[s_, 0, 0] :> 1/(16*EpsilonUV*Pi^4) - (-2 + EulerGamma - Log[4*Pi] - Log[-(ScaleMu^2/s)])/
         (16*Pi^4), 
    C0[0, s_, 0, 0, 0, 0] :> C0[0, 0, s, 0, 0, 0], 
    C0[0, 0, s_, 0, 0, 0] :> 1/(16*EpsilonIR^2*Pi^4*s) - 
       (EulerGamma - Log[4*Pi] - Log[-(ScaleMu^2/s)])/(16*EpsilonIR*Pi^4*s) - 
       (-6*EulerGamma^2 + Pi^2 + 12*EulerGamma*Log[4*Pi] - 6*Log[4*Pi]^2 + 
            12*EulerGamma*Log[-(ScaleMu^2/s)] - 12*Log[4*Pi]*Log[-(ScaleMu^2/s)] - 
            6*Log[-(ScaleMu^2/s)]^2)/(192*Pi^4*s), 
    D0[0, 0, 0, 0, s_, t_, 0, 0, 0, 0] :> 1/(4*EpsilonIR^2*Pi^4*s*t) - 
       (2*EulerGamma - 2*Log[4*Pi] - Log[-(ScaleMu^2/s)] - Log[-(ScaleMu^2/t)])/
         (8*EpsilonIR*Pi^4*s*t) - (-3*EulerGamma^2 + 2*Pi^2 + 6*EulerGamma*Log[4*Pi] - 
            3*Log[4*Pi]^2 + 3*EulerGamma*Log[-(ScaleMu^2/s)] - 3*Log[4*Pi]*Log[-(ScaleMu^2/s)] + 
            3*EulerGamma*Log[-(ScaleMu^2/t)] - 3*Log[4*Pi]*Log[-(ScaleMu^2/t)] - 
            3*Log[-(ScaleMu^2/s)]*Log[-(ScaleMu^2/t)])/(24*Pi^4*s*t)    
   };
```

```mathematica
bornVirtualUnrenormalized[1] = bornVirtualUnrenormalized[0] //. PaVeEvalRules;
```

Put together the counter-term contribution and the residue pole contribution

```mathematica
MSbarRC = {
   	SMP["dZ_psi"] -> - SMP["e"]^2/(16 Pi^2) 1/EpsilonUV, 
   	SMP["dZ_A"] -> - Nf SMP["e"]^2/(12 Pi^2) 1/EpsilonUV 
   };
```

```mathematica
RuleRS = {
   	dZe1 -> - 1/2 SMP["dZ_A"], 
   	dZAA1 -> SMP["dZ_A"], 
   	(dZf1 | dZf2)[__] -> SMP["dZ_psi"] 
   };
```

```mathematica
legResidueContrib = 1 + SMP["e"]^2/(4 Pi)*1/(4 Pi) 1/EpsilonIR;
```

```mathematica
aux0 = (Total[ampLoopCT[1]] /. RuleRS /. MSbarRC) // FeynAmpDenominatorExplicit // Contract // 
    	DiracSimplify // FCCanonicalizeDummyIndices[#, LorentzIndexNames -> {mu}] &;
```

```mathematica
ctContrib = (aux0/ampTree[2]) // Simplify;
```

```mathematica
fullCTAndResidue[0] = (ctContrib + (4*1/2) (legResidueContrib - 1)) ampTree[2]
```

$$-\frac{i \;\text{e}^2 \left(\frac{\text{e}^2 \left(2 N_f-3\right)}{24 \pi ^2 \varepsilon _{\text{UV}}}+\frac{\text{e}^2}{8 \pi ^2 \varepsilon _{\text{IR}}}\right) \left(\varphi (k_1)\right).\gamma ^{\text{mu}}.\left(\varphi (-k_2)\right) \left(\varphi (-p_2)\right).\gamma ^{\text{mu}}.\left(\varphi (p_1)\right)}{s}$$

Now get the interference of the counter term and residue contribution with the Born amplitude

```mathematica
bornCTAndResidue[0] = fullCTAndResidue[0] ComplexConjugate[ampTree[2]] // 
      FermionSpinSum[#, ExtraFactor -> 1/2^2] & // DiracSimplify // Simplify // 
  	TrickMandelstam[#, {s, t, u, 0}] &
```

$$-\frac{\text{e}^6 \left(D s^2-2 t^2-8 t u-2 u^2\right) \left(-2 N_f \varepsilon _{\text{IR}}+3 \varepsilon _{\text{IR}}-3 \varepsilon _{\text{UV}}\right)}{24 \pi ^2 s^2 \varepsilon _{\text{IR}} \varepsilon _{\text{UV}}}$$

For convenience, let us pull out an overall prefactor to get rid of ScaleMu, EulerGamma and some Pi's

```mathematica
aux1 = FCSplit[bornCTAndResidue[0], {EpsilonUV}] // 
   	ReplaceAll[#, {EpsilonIR -> 1/SMP["Delta_IR"], EpsilonUV -> 1/SMP["Delta_UV"]}] &;
bornCTAndResidue[1] = (FCReplaceD[1/Exp[EpsilonIR (Log[4 Pi] - EulerGamma)] aux1[[1]], D -> 4 - 2 EpsilonIR] + 
         	FCReplaceD[1/Exp[EpsilonUV (Log[4 Pi] - EulerGamma)] aux1[[2]], D -> 4 - 2 EpsilonUV]) // 
       	FCShowEpsilon // Series[#, {EpsilonUV, 0, 0}] & // 
     	Normal // Series[#, {EpsilonIR, 0, 0}] & // Normal // Collect2[#, EpsilonUV, EpsilonIR] &
```

$$\frac{\text{e}^6 \left(2 N_f-3\right) \left(2 s^2-t^2-4 t u-u^2\right)}{12 \pi ^2 s^2 \varepsilon _{\text{UV}}}-\frac{\text{e}^6 N_f}{6 \pi ^2}+\frac{\text{e}^6 \left(2 s^2-t^2-4 t u-u^2\right)}{4 \pi ^2 s^2 \varepsilon _{\text{IR}}}$$

```mathematica
aux2 = FCSplit[bornVirtualUnrenormalized[1], {EpsilonUV}];
bornVirtualUnrenormalized[2] = FCReplaceD[1/ScaleMu^(2 EpsilonIR)*
              	1/Exp[EpsilonIR (Log[4 Pi] - EulerGamma)] aux2[[1]], 
             	D -> 4 - 2 EpsilonIR] + FCReplaceD[1/ScaleMu^(2 EpsilonUV)*
              	1/Exp[EpsilonUV (Log[4 Pi] - EulerGamma)] aux2[[2]], D -> 4 - 2 EpsilonUV] // 
           	Collect2[#, EpsilonUV, EpsilonIR] & // Normal // Series[#, {EpsilonUV, 0, 0}] & // 
        	Normal // Series[#, {EpsilonIR, 0, 0}] & // Normal // 
     	ReplaceAll[#, Log[-ScaleMu^2/(h : s | t | u)] :> 2 Log[ScaleMu] - Log[-h]] & // 
    	TrickMandelstam[#, {s, t, u, 0}] & // Collect2[#, EpsilonUV, EpsilonIR] &;
```

Finally, we obtain the UV-finite but IR-divergent Born-virtual interference term

```mathematica
bornVirtualRenormalized[0] = (bornVirtualUnrenormalized[2] + bornCTAndResidue[1]) // 
   	TrickMandelstam[#, {s, t, u, 0}] & // Collect2[#, EpsilonUV, EpsilonIR] &
```

$$\frac{1}{72 \pi ^2 s^2}\;\text{e}^6 \left(12 t^2 N_f \log (-s)+12 u^2 N_f \log (-s)-20 t^2 N_f-20 u^2 N_f+18 t^2 \log (-s) \log (-u)+36 t^2 \log (-s)-54 t^2 \log (-s) \log (-t)-18 u^2 \log (-s) \log (-t)-72 t u \log (-s)-18 s t \log (-u)+18 s u \log (-t)+36 s t \log (-t)-36 u^2 \log ^2(-s)+54 u^2 \log (-s) \log (-u)-36 s u \log (-u)+9 t^2 \log ^2(-u)+21 \pi ^2 t^2-90 t^2+9 t^2 \log ^2(-t)-9 u^2 \log ^2(-t)+108 t u-15 \pi ^2 u^2-90 u^2-9 u^2 \log ^2(-u)\right)-\frac{\text{e}^6 \left(t^2+u^2\right)}{2 \pi ^2 s^2 \varepsilon _{\text{IR}}^2}+\frac{\text{e}^6 \left(2 t^2 \log (-s)+2 u^2 \log (-s)-2 t^2 \log (-u)-t^2+2 t^2 \log (-t)+2 u^2 \log (-t)+4 t u-u^2-2 u^2 \log (-u)\right)}{4 \pi ^2 s^2 \varepsilon _{\text{IR}}}$$

We can compare our O(eps^0) result to Eq. 2.22 in arXiv:hep-ph/0010075

```mathematica
ClearAll[LitA, LitATilde, auxBox6, Box6Eval, TriEval];
Li4 = PolyLog[4, #1] &;
ruleLit = {LitV -> Log[-s/u], LitW -> Log[-t/u], v -> s/u, w -> t/u};
```

```mathematica
LitA = (
    4*GaugeXi*(1 - 2 Epsilon)*u/s^2 ((2 - 3*Epsilon) u^2 - 6*Epsilon*t*u + 3 (2 - Epsilon) t^2)*Box6[s, t] 
     
     - 4 GaugeXi/(1 - 2 Epsilon)*t/s^2*((4 - 12*Epsilon + 7*Epsilon^2) t^2 - 
        6*Epsilon*(1 - 2*Epsilon)*t*u + (4 - 10*Epsilon + 5*Epsilon^2)*u^2)*Tri[t] 
     
     - 8/((1 - 2*Epsilon) (3 - 2*Epsilon))*1/s*(2 Epsilon (1 - Epsilon)*t*((1 - Epsilon)*t - Epsilon*u)*Nf - 
        Epsilon (3 - 2*Epsilon)*(2 - Epsilon + 2*Epsilon^2)*t*u + 
        (1 - Epsilon) (3 - 2*Epsilon) (2 - (1 - GaugeXi)*Epsilon + 2 Epsilon^2) t^2)*Tri[s]);
```

```mathematica
auxBox6 = (1/2 ((LitV - LitW)^2 + Pi^2) + 2*Epsilon*(Li3[-v] - LitV Li2[-v] - 1/3 LitV^3 - Pi^2/2 LitV) 
      - 2 Epsilon^2 (Li4[-v] + LitW Li3[-v] - 1/2 LitV^2 Li2[-v] - 1/8 LitV^4 - 
         1/6 LitV^3 LitW + 1/4*LitV^2*LitW^2 - Pi^2/4 LitV^2 - Pi^2/3 LitV LitW - 2 Zeta4)); 
 
Box6Eval[s, t] = u^(-1 - Epsilon)/(2 (1 - 2*Epsilon)) (1 - Pi^2/12 Epsilon^2) (
      auxBox6 + (auxBox6 /. {LitW -> LitV, LitV -> LitW, v -> w, w -> v})); 
 
Box6Eval[s, u] = Box6Eval[s, t] /. ruleLit /. {t -> u, u -> t}; 
 
TriEval[s_] := -(-s)^(-1 - Epsilon)/Epsilon^2 (1 - Pi^2/12 Epsilon^2 - 
    	7/3 Zeta[3] Epsilon^3 - 47/16 Zeta4 Epsilon^4)
```

```mathematica
knownResult = (( 2/3 Nf/Epsilon*8 ((t^2 + u^2)/s^2 - Epsilon) 
       
       + ((LitA /. {Tri -> TriEval, Box6 -> Box6Eval} /. ruleLit) + 
         (LitA /. {Tri -> TriEval, Box6 -> Box6Eval} /. 
             {GaugeXi -> -GaugeXi} /. ruleLit /. {t -> u, u -> t})) /. GaugeXi -> 1));
```

knownResult is the 1-loop result. Notice that is also an implicit overall prefactor prefLit from Eq. 2.8

```mathematica
prefLit = 32 Pi^2/SMP["e"]^6;
```

```mathematica
diff = Series[knownResult - 
         prefLit (bornVirtualRenormalized[0] /. EpsilonIR -> Epsilon), {Epsilon, 0, 0}] // Normal // 
      TrickMandelstam[#, {s, t, u, 0}] & // PowerExpand // SimplifyPolyLog // TrickMandelstam[#, {s, t, u, 0}] &
```

$$0$$

## Check the final results

```mathematica
FCCompareResults[0, diff, 
   Text -> {"\tCompare to arXiv:hep-ph/0010075:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```mathematica

$$\text{$\backslash $tCompare to arXiv:hep-ph/0010075:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }65.187\text{ s.}$$