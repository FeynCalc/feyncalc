---
title: Electron's g-2 in QED
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "El -> Ga El, QED, F2(0) form factor, 1-loop";
If[ $FrontEnd === Null, 
  	$FeynCalcStartupMessages = False; 
  	Print[description]; 
  ];
If[ $Notebooks === False, 
  	$FeynCalcStartupMessages = False 
  ];
LaunchKernels[4];
$LoadAddOns = {"FeynArts", "FeynHelpers"};
<< FeynCalc`
$FAVerbose = 0;
$ParallelizeFeynCalc = True; 
 
FCCheckVersion[10, 2, 0];
If[ToExpression[StringSplit[$FeynHelpersVersion, "."]][[1]] < 2, 
 	Print["You need at least FeynHelpers 2.0 to run this example."]; 
 	Abort[]; 
 ]
```

$$\text{FeynCalc }\;\text{10.2.0 (dev version, 2025-12-22 21:09:03 +01:00, fcd53f9b). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{ The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.12 (27 Mar 2025) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

$$\text{FeynHelpers }\;\text{2.0.0 (2025-12-22 19:07:44 +01:00, c92fb9f5). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{ If you use FeynHelpers in your research, please evaluate FeynHelpersHowToCite[] to learn how to cite this work.}$$

## Generate Feynman diagrams

Nicer typesetting

```mathematica
FCAttachTypesettingRule[mu, "\[Mu]"]
FCAttachTypesettingRule[p1, {SubscriptBox, "p", "1"}]
FCAttachTypesettingRule[p2, {SubscriptBox, "p", "2"}]
```

```mathematica
diags = InsertFields[CreateTopologies[1, 1 -> 2, 
     		ExcludeTopologies -> {Tadpoles, WFCorrections}], {F[2, {1}]} ->
     		{V[1], F[2, {1}]}, InsertionLevel -> {Particles}, 
    		ExcludeParticles -> {S[_], V[2 | 3], (S | U)[_], F[3 | 4], F[2, {2 | 3}]}]; 
 
Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {256, 256}];
```

![0tl9ycckvwwb8](img/0tl9ycckvwwb8.svg)

## Obtain the amplitude

The 1/(2Pi)^D prefactor is implicit. We need to replace e with -e to be compatible
with the convention D^mu = d^mu + ie A^mu

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags, PreFactor -> 1], 
    	IncomingMomenta -> {p1}, OutgoingMomenta -> {k, p2}, 
    	LorentzIndexNames -> {mu}, 
    	LoopMomenta -> {q}, UndoChiralSplittings -> True, 
    	ChangeDimension -> D, SMP -> True, 
    	FinalSubstitutions -> {SMP["e"] -> -SMP["e"], SMP["m_e"] -> me}] /. 
   	k -> p1 - p2 /. q -> q + p1
```

$$\left\{\frac{i g^{\text{Lor2}\;\text{Lor3}} \varepsilon ^{*\mu }\left(p_1-p_2\right) \left(\varphi (p_2,\text{me})\right).\left(-i \;\text{e} \gamma ^{\text{Lor3}}\right).\left(\text{me}+\gamma \cdot \left(p_2+q\right)\right).\left(-i \;\text{e} \gamma ^{\mu }\right).\left(\text{me}+\gamma \cdot \left(p_1+q\right)\right).\left(-i \;\text{e} \gamma ^{\text{Lor2}}\right).\left(\varphi (p_1,\text{me})\right)}{\left((p_1+q){}^2-\text{me}^2\right).\left((p_2+q){}^2-\text{me}^2\right).q^2}\right\}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
ScalarProduct[p1, p1] = me^2;
ScalarProduct[p2, p2] = me^2;
ScalarProduct[k, k] = 0;
ScalarProduct[p1, p2] = me^2;
```

## Evaluate the amplitudes

Amputate the polarization vector.

```mathematica
amp[1] = amp[0] // ReplaceAll[#, 
      	Pair[Momentum[Polarization[___], ___], ___] :> 1] & // 
   	Contract // ReplaceAll[#, SMP["e"]^3 -> 4 Pi SMP["e"] SMP["alpha_fs"]] &
```

$$\left\{-\frac{4 \pi  \alpha  \;\text{e} \left(\varphi (p_2,\text{me})\right).\gamma ^{\text{Lor3}}.\left(\text{me}+\gamma \cdot \left(p_2+q\right)\right).\gamma ^{\mu }.\left(\text{me}+\gamma \cdot \left(p_1+q\right)\right).\gamma ^{\text{Lor3}}.\left(\varphi (p_1,\text{me})\right)}{\left((p_1+q){}^2-\text{me}^2\right).\left((p_2+q){}^2-\text{me}^2\right).q^2}\right\}$$

```mathematica
amp[2] = DiracSimplify[amp[1], FCParallelize -> True];
```

## Identify and minimize the topologies

```mathematica
{amp[3], topos} = FCLoopFindTopologies[amp[2], {q}, FCParallelize -> True];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }1$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }1$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

```mathematica
mappings = FCLoopFindTopologyMappings[topos, FCParallelize -> True];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }0\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }1$$

## Rewrite the amplitude in terms of GLIs

```mathematica
FCLoopFindTensorBasis[{p1, p2}, {}, n, Prefactor -> Identity]
```

$$\left(
\begin{array}{c}
 p_1 \\
 p_2 \\
 p_2\to p_1 \\
\end{array}
\right)$$

```mathematica
AbsoluteTiming[ampReduced = FCLoopTensorReduce[amp[3], topos, FCParallelize -> True, 
     TensorReductionBasisChange -> {{p1, p2} -> {p1}}];]
```

$$\{0.317605,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampPreFinal = FCLoopApplyTopologyMappings[ampReduced, mappings, FCParallelize -> True];]
```

$$\{0.131764,\text{Null}\}$$

```mathematica
ampFinal = ampPreFinal // DiracSimplify[#, FCParallelize -> True] & // Collect2[#, Dirac, FCParallelize -> True] &;
```

```mathematica
ints = Cases2[ampFinal, GLI]
```

$$\left\{G^{\text{fctopology1}}(-1,1,1),G^{\text{fctopology1}}(0,1,0),G^{\text{fctopology1}}(0,1,1),G^{\text{fctopology1}}(1,0,1),G^{\text{fctopology1}}(1,1,-1),G^{\text{fctopology1}}(1,1,0),G^{\text{fctopology1}}(1,1,1)\right\}$$

```mathematica
dir = FileNameJoin[{$TemporaryDirectory, "Reduction-ElToGaEl"}];
Quiet[CreateDirectory[dir]];
```

```mathematica
KiraCreateJobFile[topos, ints, dir];
```

```mathematica
KiraCreateIntegralFile[ints, topos, dir];
```

$$\text{KiraCreateIntegralFile: Number of loop integrals: }7$$

```mathematica
KiraCreateConfigFiles[topos, ints, dir, KiraMassDimensions -> {me -> 1}];
```

```mathematica
KiraRunReduction[dir, topos, 
  KiraBinaryPath -> FileNameJoin[{$HomeDirectory, "bin", "kira"}], 
  KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]
```

$$\{\text{True}\}$$

```mathematica
reductionTable = KiraImportResults[topos, dir]
```

$$\left\{\left\{G^{\text{fctopology1}}(-1,1,1)\to (D-1) G^{\text{fctopology1}}(0,1,0),G^{\text{fctopology1}}(0,1,1)\to \frac{(D-2) G^{\text{fctopology1}}(0,1,0)}{2 \;\text{me}^2},G^{\text{fctopology1}}(1,0,1)\to \frac{(D-2) G^{\text{fctopology1}}(0,1,0)}{(2 D-6) \;\text{me}^2},G^{\text{fctopology1}}(1,1,-1)\to 0,G^{\text{fctopology1}}(1,1,0)\to \frac{(D-2) G^{\text{fctopology1}}(0,1,0)}{(2 D-6) \;\text{me}^2},G^{\text{fctopology1}}(1,1,1)\to \frac{(D-2) G^{\text{fctopology1}}(0,1,0)}{4 \;\text{me}^4}\right\}\right\}$$

```mathematica
resPreFinal = Collect2[Total[ampFinal /. Dispatch[reductionTable]], GLI, FCParallelize -> True];
```

```mathematica
integralMappings = FCLoopFindIntegralMappings[Cases2[resPreFinal, GLI], mappings[[2]], FCParallelize -> True]
```

$$\left\{\{\},\left\{G^{\text{fctopology1}}(0,1,0)\right\}\right\}$$

```mathematica
resFinal = Collect2[(resPreFinal /. Dispatch[integralMappings[[1]]]), GLI, FCParallelize -> True]
```

$$\left\{-\frac{1}{(D-3) \;\text{me}^3}2 \pi  \alpha  (D-2) \;\text{e} G^{\text{fctopology1}}(0,1,0) \left(D^2 \;\text{me} \left(\varphi (p_2,\text{me})\right).\gamma ^{\mu }.\left(\varphi (p_1,\text{me})\right)-D^2 p_1{}^{\mu } \left(\varphi (p_2,\text{me})\right).\left(\varphi (p_1,\text{me})\right)-8 D \;\text{me} \left(\varphi (p_2,\text{me})\right).\gamma ^{\mu }.\left(\varphi (p_1,\text{me})\right)+7 D p_1{}^{\mu } \left(\varphi (p_2,\text{me})\right).\left(\varphi (p_1,\text{me})\right)+2 D p_2{}^{\mu } \left(\varphi (p_2,\text{me})\right).\left(\varphi (p_1,\text{me})\right)+19 \;\text{me} \left(\varphi (p_2,\text{me})\right).\gamma ^{\mu }.\left(\varphi (p_1,\text{me})\right)-12 p_1{}^{\mu } \left(\varphi (p_2,\text{me})\right).\left(\varphi (p_1,\text{me})\right)-8 p_2{}^{\mu } \left(\varphi (p_2,\text{me})\right).\left(\varphi (p_1,\text{me})\right)\right)\right\}$$

To extract F2 (0) we need to look only at the piece proportional to (p1+p2)^mu. Thus we can drop the g^mu -piece

```mathematica
resGm2[0] = resFinal // Total // ReplaceAll[#, FCI[GAD[mu]] :> 0] & // FCReplaceMomenta[#, {p2 -> p1}] & // DotSimplify
```

$$-\frac{2 \pi  \alpha  (D-2) \;\text{e} G^{\text{fctopology1}}(0,1,0) \left(D^2 p_1{}^{\mu } \left(-\left(\varphi (p_2,\text{me})\right).\left(\varphi (p_1,\text{me})\right)\right)+9 D p_1{}^{\mu } \left(\varphi (p_2,\text{me})\right).\left(\varphi (p_1,\text{me})\right)-20 p_1{}^{\mu } \left(\varphi (p_2,\text{me})\right).\left(\varphi (p_1,\text{me})\right)\right)}{(D-3) \;\text{me}^3}$$

The master integral is just a tadpole

```mathematica
resGm2[1] = resGm2[0] // ReplaceAll[#, {GLI["fctopology1", {0, 1, 0}] -> ((-I)*(me^2)^(-1 + D/2)*Pi^(D/2)*
             Gamma[1 - D/2])/(2*Pi)^D}] & // FCReplaceD[#, D -> 4 - 2 ep] & // Series[#, {ep, 0, 0}] & // Normal
```

$$\frac{i \alpha  \;\text{e} p_1{}^{\mu } \left(\varphi (p_2,\text{me})\right).\left(\varphi (p_1,\text{me})\right)}{2 \pi  \;\text{me}}$$

As expected, F2(0) is free of any divergences. So we can safely do the limit D ->4

```mathematica
resGm2[2] = resGm2[1] // ChangeDimension[#, 4] & // ReplaceAll[#, D -> 4] &
```

$$\frac{i \alpha  \;\text{e} \overline{p_1}{}^{\mu } \left(\varphi (\overline{p_2},\text{me})\right).\left(\varphi (\overline{p_1},\text{me})\right)}{2 \pi  \;\text{me}}$$

We obtained $\frac{i e}{2 m_e} (p_1+p_2)^\mu F_2 (0) \bar{u}(p_2) u(p_1)$.
Dividing by the numerical prefactor and substituting $e^2 = 4\pi^2 \alpha$ yields F2(0)

```mathematica
f2[0] = (resGm2[2]/((I SMP["e"])/(2 me))) // 
  	ReplaceAll [#, {Spinor[__] . Spinor[__] :> 1, 
     	FCI[FV[p1, _]] :> 1/2}] &
```

$$\frac{\alpha }{2 \pi }$$

## Check the final results

```mathematica
knownResult = AlphaFS/(2 Pi);
FCCompareResults[f2[0], knownResult, 
   Text -> {"\tCompare to J. Schwinger, Phys. Rev. 73, 416-417, 1948:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```mathematica

$$\text{$\backslash $tCompare to J. Schwinger, Phys. Rev. 73, 416-417, 1948:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }34.061\text{ s.}$$