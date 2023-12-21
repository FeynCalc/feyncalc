---
title: 1-loop phi^4 renormalization in the minimal subtraction schemes
---


## Load FeynCalc and the necessary add-ons or other packages

This example uses a custom phi^4 model created with FeynRules. Please evaluate the file
FeynCalc/Examples/FeynRules/Phi4/GenerateModelPhi4.m before running it for the first time.

```mathematica
description = "Renormalization, phi^4, MS and MSbar, 1-loop";
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

## Configure some options

We keep scaleless B0 functions, since otherwise the UV part would not come out right.

```mathematica
$KeepLogDivergentScalelessIntegrals = True;
```

```mathematica
FAPatch[PatchModelsOnly -> True];

(*Successfully patched FeynArts.*)
```

## Generate Feynman diagrams

```mathematica
params = {InsertionLevel -> {Particles}, Model -> FileNameJoin[{"Phi4", "Phi4"}], 
    GenericModel -> FileNameJoin[{"Phi4", "Phi4"}]};
top[i_, j_] := CreateTopologies[1, i -> j];
topCT[i_, j_] := CreateCTTopologies[1, i -> j];
topVertex[i_, j_] := CreateTopologies[1, i -> j, 
   	ExcludeTopologies -> {WFCorrections}];
topVertexCT[i_, j_] := CreateCTTopologies[1, i -> j, 
    	ExcludeTopologies -> {WFCorrectionCTs}]; 
 
{diagPhi4SE, diagPhi4SECT} = InsertFields[#, {S[1]} -> {S[1]}, 
      Sequence @@ params] & /@ {top[1, 1], topCT[1, 1]};
{diagVertex, diagVertexCT} = InsertFields[#,  {S[1], S[1]} -> {S[1], S[1]}, 
      Sequence @@ params] & /@ {topVertex[2, 2], topVertexCT[2, 2]};
```

```mathematica
diag1[0] = diagPhi4SE[[0]][Sequence @@ diagPhi4SE, 
   	Sequence @@ diagPhi4SECT];
diag2[0] = diagVertex[[0]][Sequence @@ diagVertex, 
   	Sequence @@ diagVertexCT];
```

```mathematica
Paint[diag1[0], ColumnsXRows -> {2, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> {512, 256}];
```

![1h4k6z31acjax](img/1h4k6z31acjax.svg)

```mathematica
Paint[diag2[0], ColumnsXRows -> {2, 1}, SheetHeader -> None, 
   Numbering -> Simple, ImageSize -> {512, 256}];
```

![0aev7upap3l3h](img/0aev7upap3l3h.svg)

![0dykryx55j2q1](img/0dykryx55j2q1.svg)

## Obtain the amplitudes

The 1/(2Pi)^D prefactor is implicit.

Self-energy including the counter-term

```mathematica
amp1[0] = FCFAConvert[CreateFeynAmp[diag1[0], Truncated -> True, 
   	GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p}, OutgoingMomenta -> {p}, 
  	LorentzIndexNames -> {mu}, 
  	LoopMomenta -> {l}, UndoChiralSplittings -> True, 
  	ChangeDimension -> D, List -> False, SMP -> True, 
  	FinalSubstitutions -> {Zm -> SMP["Z_m"], Zphi -> SMP["Z_phi"], 
    	GaugeXi[S[1]] -> 1, Mphi -> m}]
```

$$\frac{g}{2 \left(l^2-m^2\right)}-i m^2 \left(Z_m Z_{\phi }-1\right)+i p^2 \left(Z_{\phi }-1\right)$$

Quartic vertex including the counter-term

```mathematica
amp2[0] = FCFAConvert[CreateFeynAmp[diag2[0], Truncated -> True, 
   	GaugeRules -> {}, PreFactor -> 1], 
  	IncomingMomenta -> {p1, p2}, OutgoingMomenta -> {p3, p4}, 
  	LorentzIndexNames -> {mu}, LoopMomenta -> {l}, 
  	UndoChiralSplittings -> True, ChangeDimension -> D, 
  	List -> False, SMP -> True, FinalSubstitutions -> {Zg -> SMP["Z_g"], 
    	Zphi -> SMP["Z_phi"], GaugeXi[S[1]] -> 1, Mphi -> m}]
```

$$\frac{g^2}{2 \left(l^2-m^2\right).\left((l+\text{p2}-\text{p3})^2-m^2\right)}+\frac{g^2}{2 \left(l^2-m^2\right).\left((l+\text{p2}-\text{p4})^2-m^2\right)}+\frac{g^2}{2 \left(l^2-m^2\right).\left((l-\text{p3}-\text{p4})^2-m^2\right)}-i g \left(Z_g Z_{\phi }^2-1\right)$$

## Calculate the amplitudes

### Self-energy

```mathematica
amp1[1] = amp1[0] // ReplaceAll[#, {SMP["Z_phi"] -> 1 + alpha SMP["d_phi"], 
         SMP["Z_m"] -> 1 + alpha SMP["d_m"]}] & // Series[#, {alpha, 0, 1}] & // 
    Normal // ReplaceAll[#, alpha -> 1] &
```

$$\frac{g}{2 \left(l^2-m^2\right)}-i m^2 \left(\delta _{\phi }+\delta _m\right)+i p^2 \delta _{\phi }$$

Express the self-energy in tems of the Passarino-Veltman coefficient functions.

```mathematica
amp1[2] = ToPaVe[amp1[1], l]
```

$$\frac{1}{2} i \pi ^2 g \;\text{A}_0\left(m^2\right)-i \left(m^2 \delta _{\phi }+m^2 \delta _m-p^2 \delta _{\phi }\right)$$

Discard all the finite pieces of the 1-loop amplitude

```mathematica
amp1Div[0] = PaVeUVPart[amp1[2], Prefactor -> 1/(2 Pi)^D] // 
        FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
     FCHideEpsilon // SelectNotFree2[#, {SMP["Delta"], SMP["d_m"], 
       SMP["d_phi"]}] & // Simplify
```

$$\frac{i \Delta  g m^2}{32 \pi ^2}-i \left(m^2 \delta _m+\delta _{\phi } \left(m^2-p^2\right)\right)$$

Equating the result to zero and solving for d_phi and d_m we obtain  the renormalization constants in  the minimal subtraction schemes.

```mathematica
sol[1] = Solve[SelectNotFree2[amp1Div[0], p] == 0, 
     	SMP["d_phi"]] // Flatten // Simplify;
sol[2] = Solve[(SelectFree2[amp1Div[0], p] == 0) /. sol[1], 
     	SMP["d_m"]] // Flatten // Simplify;
solMS1 = Join[sol[1], sol[2]] /. {
   	SMP["d_phi"] -> SMP["d_phi^MS"], 
   	SMP["d_m"] -> SMP["d_m^MS"], SMP["Delta"] -> 1/Epsilon 
   }
solMSbar1 = Join[sol[1], sol[2]] /. {
   	SMP["d_phi"] -> SMP["d_phi^MSbar"], 
   	SMP["d_m"] -> SMP["d_m^MSbar"] 
   }
```

$$\left\{\delta _{\phi }^{\text{MS}}\to 0,\delta _m^{\text{MS}}\to \frac{g}{32 \pi ^2 \varepsilon }\right\}$$

$$\left\{\delta _{\phi }^{\overset{---}{\text{MS}}}\to 0,\delta _m^{\overset{---}{\text{MS}}}\to \frac{\Delta  g}{32 \pi ^2}\right\}$$

### Quartic vertex

```mathematica
amp2[1] = amp2[0] // ReplaceRepeated[#, {SMP["Z_g"] -> 1 + alpha SMP["d_g"], 
         SMP["Z_phi"] -> 1 + alpha SMP["d_phi"]}] & // 
     Series[#, {alpha, 0, 1}] & // Normal // ReplaceAll[#, alpha -> 1] &
```

$$\frac{1}{2} \left(\frac{g^2}{\left(l^2-m^2\right).\left((l+\text{p2}-\text{p3})^2-m^2\right)}+\frac{g^2}{\left(l^2-m^2\right).\left((l+\text{p2}-\text{p4})^2-m^2\right)}+\frac{g^2}{\left(l^2-m^2\right).\left((l-\text{p3}-\text{p4})^2-m^2\right)}\right)-i g \left(2 \delta _{\phi }+\delta _g\right)$$

Express the quartic vertex in tems of the Passarino-Veltman coefficient functions.

```mathematica
amp2[2] = ToPaVe[amp2[1], l]
```

$$\frac{1}{2} i \pi ^2 g^2 \;\text{B}_0\left(\text{p2}^2-2 (\text{p2}\cdot \;\text{p3})+\text{p3}^2,m^2,m^2\right)+\frac{1}{2} i \pi ^2 g^2 \;\text{B}_0\left(\text{p2}^2-2 (\text{p2}\cdot \;\text{p4})+\text{p4}^2,m^2,m^2\right)+\frac{1}{2} i \pi ^2 g^2 \;\text{B}_0\left(\text{p3}^2+2 (\text{p3}\cdot \;\text{p4})+\text{p4}^2,m^2,m^2\right)-i g \left(2 \delta _{\phi }+\delta _g\right)$$

Discard all the finite pieces of the 1-loop amplitude

```mathematica
amp2Div[0] = PaVeUVPart[amp2[2], Prefactor -> 1/(2 Pi)^D] // 
        FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal // 
     FCHideEpsilon // SelectNotFree2[#, {SMP["Delta"], SMP["d_g"], SMP["d_phi"]}] & // Simplify
```

$$\frac{1}{32} i g \left(\frac{3 \Delta  g}{\pi ^2}-32 \left(2 \delta _{\phi }+\delta _g\right)\right)$$

```mathematica
sol[3] = Solve[(amp2Div[0] == 0) /. sol[1], 
     	SMP["d_g"]] // Flatten // Simplify;
solMS2 = sol[3] /. {
   	SMP["d_g"] -> SMP["d_g^MS"], 
   	SMP["Delta"] -> 1/Epsilon 
   }
solMSbar2 = sol[3] /. {
   	SMP["d_g"] -> SMP["d_g^MSbar"] 
   }
```

$$\left\{\delta _g^{\text{MS}}\to \frac{3 g}{32 \pi ^2 \varepsilon }\right\}$$

$$\left\{\delta _g^{\overset{---}{\text{MS}}}\to \frac{3 \Delta  g}{32 \pi ^2}\right\}$$

## Check the final results

```mathematica
knownResult = {
   	SMP["d_phi^MS"] -> 0, 
   	SMP["d_m^MS"] -> (g*1/Epsilon)/(32*Pi^2), 
   	SMP["d_g^MS"] -> (3*g*1/Epsilon)/(32*Pi^2), 
    
   	SMP["d_phi^MSbar"] -> 0, 
   	SMP["d_m^MSbar"] -> (g*SMP["Delta"])/(32*Pi^2), 
   	SMP["d_g^MSbar"] -> (3*g*SMP["Delta"])/(32*Pi^2) 
   	};
FCCompareResults[Join[solMS1, solMS2, solMSbar1, solMSbar2], knownResult, 
   Text -> {"\tCompare to Bailin and Love, Introduction to Gauge Field Theory, Eqs. 7.73-7.74 and Eqs. 7.76-7.77:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Bailin and Love, Introduction to Gauge Field Theory, Eqs. 7.73-7.74 and Eqs. 7.76-7.77:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }25.751\text{ s.}$$