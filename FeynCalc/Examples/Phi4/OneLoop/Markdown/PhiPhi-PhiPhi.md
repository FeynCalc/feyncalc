---
title: Phi Phi scattering at 1-loop
---


## Load FeynCalc and the necessary add-ons or other packages

This example uses a custom Phi^4 model created with FeynRules. Please evaluate the file
FeynCalc/Examples/FeynRules/Phi4/GenerateModelPhi4.m before running it for the first time.

```mathematica
description = "Phi Phi -> Phi Phi, Phi^4, asymptotic limit, 1-loop";
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

```mathematica
FAPatch[PatchModelsOnly -> True];

(*Successfully patched FeynArts.*)
```

## Generate Feynman diagrams

Nicer typesetting

```mathematica
MakeBoxes[p1, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2, TraditionalForm] := "\!\(\*SubscriptBox[\(p\), \(2\)]\)";
MakeBoxes[k1, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(2\)]\)";
```

```mathematica
diags = InsertFields[CreateTopologies[1, 2 -> 2, 
    		ExcludeTopologies -> {WFCorrections}], 
   		{S[1], S[1]} -> {S[1], S[1]}, InsertionLevel -> {Classes}, 
   		Model -> FileNameJoin[{"Phi4", "Phi4"}]];
Paint[diags, ColumnsXRows -> {3, 1}, Numbering -> None, SheetHeader -> None, 
   ImageSize -> {512, 256}];
```

![0q8rtof4phvi8](img/0q8rtof4phvi8.svg)

```mathematica
diagsCT = InsertFields[CreateCTTopologies[1, 2 -> 2, 
    	ExcludeTopologies -> {WFCorrectionCTs}],  {S[1], S[1]} -> {S[1], S[1]}, 
   	InsertionLevel -> {Classes},  Model -> FileNameJoin[{"Phi4", "Phi4"}]];
Paint[diagsCT, ColumnsXRows -> {1, 1}, Numbering -> None, SheetHeader -> None, 
   ImageSize -> {256, 256}];
```

![05b42tcocrt1l](img/05b42tcocrt1l.svg)

## Obtain the amplitude

The 1/(2Pi)^D prefactor is implicit.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, PreFactor -> 1], 
  	IncomingMomenta -> {p1, p2}, OutgoingMomenta -> {k1, k2}, 
  	LoopMomenta -> {q}, ChangeDimension -> D, List -> False, 
  	FinalSubstitutions -> {Mphi -> m}]
```

$$\frac{g^2}{2 \left(q^2-m^2\right).\left((-k_1-k_2+q){}^2-m^2\right)}+\frac{g^2}{2 \left(q^2-m^2\right).\left((-k_1+p_2+q){}^2-m^2\right)}+\frac{g^2}{2 \left(q^2-m^2\right).\left((-k_2+p_2+q){}^2-m^2\right)}$$

```mathematica
ampCT[0] = FCFAConvert[CreateFeynAmp[diagsCT, PreFactor -> 1], 
  	IncomingMomenta -> {p1, p2}, OutgoingMomenta -> {k1, k2}, 
  	LoopMomenta -> {q}, ChangeDimension -> D, List -> False, 
  	FinalSubstitutions -> {Mphi -> m, Zg -> 1 + SMP["d_g^MSbar"]}]
```

$$-i g \left(\text{Zphi}^2 \left(\delta _g^{\overset{---}{\text{MS}}}+1\right)-1\right)$$

## Fix the kinematics

For simplicity, let us consider the massless case

```mathematica
FCClearScalarProducts[]
SetMandelstam[s, t, u, p1, p2, -k1, -k2, 0, 0, 0, 0];
```

## Calculate the amplitude

```mathematica
amp[1] = amp[0] // ReplaceAll[#, m -> 0] & // ToPaVe[#, q] &
```

$$\frac{1}{2} i \pi ^2 g^2 \;\text{B}_0(s,0,0)+\frac{1}{2} i \pi ^2 g^2 \;\text{B}_0(t,0,0)+\frac{1}{2} i \pi ^2 g^2 \;\text{B}_0(u,0,0)$$

The explicit value of the integral can be obtained from Package-X via the FeynHelpers add-on.

```mathematica
loopInt = {
    B0[s_, 0, 0] :> -(-2 + Log[4*Pi] - 
         	Log[(-4*Pi*ScaleMu^2)/s])/(16*Pi^4) + SMP["Delta"]/(16*Pi^4) 
   };
```

```mathematica
amp[2] = (amp[1] /. loopInt) // Simplify
```

$$-\frac{i g^2 \left(-3 \Delta -\log \left(-\frac{4 \pi  \mu ^2}{s}\right)-\log \left(-\frac{4 \pi  \mu ^2}{t}\right)-\log \left(-\frac{4 \pi  \mu ^2}{u}\right)-6+3 \log (4 \pi )\right)}{32 \pi ^2}$$

```mathematica
ampFull[0] = Expand[(amp[2] + ampCT[0]) /. 
   	{SMP["d_g^MSbar"] -> (3*g*SMP["Delta"])/(32*Pi^2), Zphi -> 1}]
```

$$\frac{i g^2 \log \left(-\frac{4 \pi  \mu ^2}{s}\right)}{32 \pi ^2}+\frac{i g^2 \log \left(-\frac{4 \pi  \mu ^2}{t}\right)}{32 \pi ^2}+\frac{i g^2 \log \left(-\frac{4 \pi  \mu ^2}{u}\right)}{32 \pi ^2}+\frac{3 i g^2}{16 \pi ^2}-\frac{3 i g^2 \log (4 \pi )}{32 \pi ^2}$$

```mathematica
FCCompareResults[FreeQ[ampFull[0], SMP["Delta"]], True, 
   Text -> {"\tThe UV divergence is cancelled by the counter-term:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
```

$$\text{$\backslash $tThe UV divergence is cancelled by the counter-term:} \;\text{CORRECT.}$$

Now let us look at the asymptotic limit where  s goes to infinity and t is fixed

```mathematica
ampFullAsy[0] = Series[ampFull[0] /. u -> -s - t, {s, Infinity, 0}] // Normal
```

$$-\frac{i \left(g^2 \left(-\log \left(-\frac{4 \pi  \mu ^2}{s}\right)\right)-g^2 \log \left(\frac{4 \pi  \mu ^2}{s}\right)-g^2 \log \left(-\frac{4 \pi  \mu ^2}{t}\right)-6 g^2+3 g^2 \log (4 \pi )\right)}{32 \pi ^2}$$

The leading order behavior is governed by the log of s

```mathematica
ampFullAsy[1] = ampFullAsy[0] // PowerExpand // SelectNotFree2[#, s] &
```

$$-\frac{i g^2 \log (s)}{16 \pi ^2}$$

## Check the final results

```mathematica
knownResult = ((-I/16)*g^2*Log[s])/Pi^2;
FCCompareResults[ampFullAsy[1], knownResult, 
   Text -> {"\tCompare to Peskin and Schroeder, An Introduction to QFT, Ex 10.4:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```

$$\text{$\backslash $tCompare to Peskin and Schroeder, An Introduction to QFT, Ex 10.4:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }25.667\text{ s.}$$