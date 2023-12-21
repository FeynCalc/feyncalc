---
title: Pure Yang-Mills 1-loop gluon self-energy in the background field formalism
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Gl -> Gl, YM+BGF, only UV divergences, 1-loop";
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
MakeBoxes[mu, TraditionalForm] := "\[Mu]";
MakeBoxes[nu, TraditionalForm] := "\[Nu]";
```

```mathematica
diags = InsertFields[CreateTopologies[1, 1 -> 1, ExcludeTopologies -> {Tadpoles}], 
    	{V[50, {a}]} -> {V[50, {b}]}, InsertionLevel -> {Classes}, 
    	Model -> FileNameJoin[{"QCDBGF", "QCDBGF"}], 
    	GenericModel -> FileNameJoin[{"QCDBGF", "QCDBGF"}], 
    	ExcludeParticles -> {F[_]}]; 
 
Paint[diags, ColumnsXRows -> {2, 2}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 512}];
```

![1bfytjw1lljyy](img/1bfytjw1lljyy.svg)

## Obtain corresponding amplitudes

The 1/(2Pi)^D prefactor is implicit.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, GaugeRules -> {}, 
    	PreFactor -> 1], IncomingMomenta -> {p}, OutgoingMomenta -> {p}, LoopMomenta -> {l}, 
   	LorentzIndexNames -> {mu, nu}, UndoChiralSplittings -> True, 
   	ChangeDimension -> D, List -> True, SMP -> True, DropSumOver -> True, 
   	FinalSubstitutions -> {SMP["m_u"] -> SMP["m_q"], 
     	GaugeXi[V[5, {_}]] :> GaugeXi[G]}];
```

```mathematica
amp[1] = DiracSimplify /@ amp[0];
```

```mathematica
amp[2] = SUNSimplify[TID[#, l, ToPaVe -> True]] & /@ amp[1];
```

Discard all the finite pieces of the 1-loop amplitude.

```mathematica
ampDiv[0] = PaVeUVPart[#, Prefactor -> 1/(2 Pi)^D] & /@ amp[2]
```

$$\left\{0,0,\frac{i 2^{1-D} \pi ^{2-D} C_A g_s^2 \delta ^{ab} \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)}{(D-4) (D-1)},\frac{i 2^{-D-1} \pi ^{2-D} C_A g_s^2 \left(\xi _G^2+6 \xi _G+33\right) \delta ^{ab} \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)}{(D-4) (D-1)}\right\}$$

```mathematica
ampDiv[1] = FCReplaceD[ampDiv[0], D -> 4 - 2 Epsilon] // 
    	Series[#, {Epsilon, 0, -1}] & // Normal // Simplify
```

$$\left\{0,0,-\frac{i C_A g_s^2 \delta ^{ab} \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)}{48 \pi ^2 \varepsilon },-\frac{i C_A g_s^2 \left(\xi _G^2+6 \xi _G+33\right) \delta ^{ab} \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)}{192 \pi ^2 \varepsilon }\right\}$$

## Check the final results

```mathematica
knownResult = {
    	0, 
    	0, 
    	I CA SMP["g_s"]^2 SUNDelta[a, b]/(4 Pi)^2 (1/(3 Epsilon))*
     		(MTD[mu, nu] SPD[p] - FVD[p, mu] FVD[p, nu]), 
    	I CA SMP["g_s"]^2 SUNDelta[a, b]/(4 Pi)^2 (10/(3 Epsilon))*
     		(MTD[mu, nu] SPD[p] - FVD[p, mu] FVD[p, nu]) 
    } // FCI;
FCCompareResults[ampDiv[1] /. GaugeXi[G] -> 1, knownResult, 
   Text -> {"\tCompare to Abbott, Nucl. Phys. B 185 (1981) 189-203, Eqs 5.11-5.12:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Abbott, Nucl. Phys. B 185 (1981) 189-203, Eqs 5.11-5.12:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }27.441\text{ s.}$$