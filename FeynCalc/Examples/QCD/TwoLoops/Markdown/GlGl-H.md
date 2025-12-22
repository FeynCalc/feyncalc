---
title: Higgs production in gluon fusion
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Gl Gl -> H, QCD, 2-loops";
If[ $FrontEnd === Null, 
  	$FeynCalcStartupMessages = False; 
  	Print[description]; 
  ];
If[ $Notebooks === False, 
  	$FeynCalcStartupMessages = False 
  ];
LaunchKernels[8];
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
diags = InsertFields[CreateTopologies[2, 2 -> 1, ExcludeTopologies -> {Tadpoles, WFCorrections}], 
    	 {V[5], V[5]} -> {S[1]}, InsertionLevel -> {Particles}, Model -> "SMQCD", 
    	ExcludeParticles -> {F[3 | 4, {1 | 2}], F[4, {3}], V[1 | 2 | 3 | 4], S[_]}]; 
 
Paint[diags, ColumnsXRows -> {6, 4}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> 128 {6, 4}];
```

![106vd8ivt1oyg](img/106vd8ivt1oyg.svg)

## Obtain the amplitude

```mathematica
ampRaw = FCFAConvert[CreateFeynAmp[diags, PreFactor -> 1, GaugeRules -> {}], OutgoingMomenta -> {pH}, 
     	IncomingMomenta -> {q1, q2}, LoopMomenta -> {k1, k2}, List -> True, 
     	TransversePolarizationVectors -> {q1, q2}, ChangeDimension -> D,
     	DropSumOver -> True, SMP -> True, 
     	UndoChiralSplittings -> True] // SMPToSymbol // FCReplaceMomenta[#, {pH -> q1 + q2}] &;	
```

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
ScalarProduct[q1, q1] = 0;
ScalarProduct[q2, q2] = 0;
ScalarProduct[q1, q2] = (s)/2;
```

## Evaluate the amplitude

```mathematica
AbsoluteTiming[ampSimp = (ampRaw) // Contract[#, FCParallelize -> True] & // 
      DiracSimplify[#, FCParallelize -> True] & // SUNSimplify[#, FCParallelize -> True] &;]
```

$$\{21.1422,\text{Null}\}$$

## Rewrite the amplitude in terms of GLIs

```mathematica
{amp, topos} = FCLoopFindTopologies[ampSimp, {k1, k2}, FCParallelize -> True];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }12$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }12$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }0$$

```mathematica
mappings = FCLoopFindTopologyMappings[topos, FCParallelize -> True];
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }0\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }12$$

```mathematica
completedTopos = FCLoopBasisFindCompletion[topos];
```

```mathematica
basisCompletionRules = FCLoopCreateRuleGLIToGLI[completedTopos, List /@ topos] // Flatten;
```

```mathematica
mappings2 = {mappings[[1]] /. basisCompletionRules, completedTopos};
```

```mathematica
AbsoluteTiming[ampReduced = FCLoopTensorReduce[amp, topos, FCParallelize -> True];]
```

$$\{15.7984,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampPreFinal = FCLoopApplyTopologyMappings[ampReduced /. basisCompletionRules, mappings2, FCParallelize -> True];]
```

$$\{11.5527,\text{Null}\}$$

```mathematica
AbsoluteTiming[ampFinal = (ampPreFinal(*/.GaugeXi[g]->- gxi+ 1*)) // DiracSimplify[#, FCParallelize -> True] & // 
     SUNSimplify[#, FCParallelize -> True] &;]
```

$$\{2.62234,\text{Null}\}$$

```mathematica
dir = FileNameJoin[{$TemporaryDirectory, "Reduction-GlGlToH-2L"}];
Quiet[CreateDirectory[dir]];
```

```mathematica
KiraCreateConfigFiles[completedTopos, Cases2[ampFinal, GLI], dir, KiraMassDimensions -> {mt -> 1, s -> 2}];
```

```mathematica
KiraCreateIntegralFile[Cases2[ampFinal, GLI], completedTopos, dir];
```

$$\text{KiraCreateIntegralFile: Number of loop integrals: }88$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }104$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }61$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }60$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }103$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }100$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }206$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }165$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }219$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }214$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }139$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }61$$

```mathematica
KiraCreateJobFile[completedTopos, Cases2[ampFinal, GLI], dir];
```

Upon running the reduction we can load the tables

```mathematica
(*tables=KiraImportResults[completedTopos,dir]//Flatten;*)
```

```mathematica
SetDirectory[NotebookDirectory[]];
tables = Get["Reduction-GlGl2H.m"];
```

```mathematica
preMasters = Cases2[Last /@ tables, GLI];
```

```mathematica
integralMappings = FCLoopFindIntegralMappings[preMasters, completedTopos, FCParallelize -> True];
```

```mathematica
integralMappings // Last // Length
```

$$21$$

To find linear relations between master integrals we use the procedure described in arXiv:2407.08264

```mathematica
aux = FCLoopFindIntegralMappings[First /@ tables, completedTopos, FCParallelize -> True];
```

```mathematica
eqs = aux[[1]] /. tables /. integralMappings[[1]] /. Rule -> Equal;
linRelsRaw = Union[Simplify[eqs] /. True -> Nothing];
linRels = FCMatchSolve[Total[linRelsRaw /. Equal[a_, b_] :> (a - b) dummy[Unique[]]], {dummy, D, mt, s}];
```

$$\text{FCMatchSolve: Following coefficients trivially vanish: }\left\{G^{\text{fctopology10C}}(0,0,1,0,1,0,0)\to 0,G^{\text{fctopology9C}}(0,1,1,1,0,0,0)\to 0\right\}$$

$$\text{FCMatchSolve: Following variables will be treated as free parameters: }\{\}$$

$$\text{FCMatchSolve: Solving for: }\left\{G^{\text{fctopology10C}}(0,0,1,0,1,1,0),G^{\text{fctopology10C}}(0,1,0,1,1,0,0),G^{\text{fctopology10C}}(0,1,0,1,2,0,0),G^{\text{fctopology10C}}(0,1,1,0,1,1,0),G^{\text{fctopology10C}}(0,1,1,0,1,2,0),G^{\text{fctopology10C}}(0,1,1,1,1,0,0),G^{\text{fctopology10C}}(0,2,1,0,1,1,0),G^{\text{fctopology10C}}(1,0,1,1,1,1,0),G^{\text{fctopology10C}}(1,0,1,1,1,2,0),G^{\text{fctopology11C}}(0,1,1,2,0,1,0),G^{\text{fctopology12C}}(2,1,0,0,1,0,0),G^{\text{fctopology5C}}(1,1,1,1,0,2,0)\right\}$$

$$\text{FCMatchSolve: A solution exists.}$$

Check that gauge dependence cancels upon inserting linear relations between masters

```mathematica
resPreFinal = Collect2[ampFinal //. Dispatch[tables] //. Dispatch[integralMappings[[1]]] /. Dispatch[linRels], 
    GLI, GaugeXi, FCParallelize -> True];
```

```mathematica
gaugeDep = SelectNotFree2[#, GaugeXi] & /@ resPreFinal;
Collect2[Total[gaugeDep], GLI]
```

$$0$$

```mathematica
resFinal = Collect2[Total[resPreFinal], GLI, GaugeXi, FCParallelize -> True];
```

## Compare to the literature

The literature result for the 2-loop amplitude from arXiv:hep-ph/0611236 is given using a different basis on master integrals.
To compare both results we need to convert their basis to ours.

### Literature results 

```mathematica
propD[1, 1] = SFAD[k];
propD[1, 2] = SFAD[k + p1];
propD[1, 3] = SFAD[k + p1 + p2];
propD[1, 4] = SFAD[{l + p1 + p2, mt^2}];
propD[1, 5] = SFAD[{l + p1, mt^2}];
propD[1, 6] = SFAD[{l, mt^2}];
propD[1, 7] = SFAD[{k - l, mt^2}];
```

```mathematica
propD[2, 1] = SFAD[{k, mt^2}];
propD[2, 2] = SFAD[{k + p2, mt^2}];
propD[2, 3] = SFAD[{k + p1 + p2, mt^2}];
propD[2, 4] = SFAD[{l + p1 + p2, mt^2}];
propD[2, 5] = SFAD[{l + p2, mt^2}];
propD[2, 6] = SFAD[{l, mt^2}];
propD[2, 7] = SFAD[k - l];
```

```mathematica
propD[3, 1] = SFAD[{k, mt^2}];
propD[3, 2] = SFAD[k - l - p1];
propD[3, 3] = SFAD[{k + p1 + p2, mt^2}];
propD[3, 4] = SFAD[{l + p1 + p2, mt^2}];
propD[3, 5] = SFAD[{l + p1, mt^2}];
propD[3, 6] = SFAD[{k + p1, mt^2}];
propD[3, 7] = SFAD[k - l];
```

```mathematica
toposLit = {
    FCTopology[tp1, Table[propD[1, i], {i, 1, 7}], {k, l}, {p1, p2}, {Hold[SPD][p1, p1] -> 0, 
      Hold[SPD][p2, p2] -> 0, Hold[SPD][p1, p2] -> s/2}, {}], 
    
    FCTopology[tp2, Table[propD[2, i], {i, 1, 7}], {k, l}, {p1, p2}, {Hold[SPD][p1, p1] -> 0, 
      Hold[SPD][p2, p2] -> 0, Hold[SPD][p1, p2] -> s/2}, {}], 
    
    FCTopology[tp3, Table[propD[3, i], {i, 1, 7}], {k, l}, {p1, p2}, {Hold[SPD][p1, p1] -> 0, 
      Hold[SPD][p2, p2] -> 0, Hold[SPD][p1, p2] -> s/2}, {}] 
   };
```

```mathematica
gliRules$tp1 = FCLoopCreateRulesToGLI[toposLit[[1]]];
```

```mathematica
intWithNumerators = ExpandAll[ExpandScalarProduct[SPD[k + p1, l - k] GLI[tp1, Normal[SparseArray[{2 -> 1, 4 -> 1, 6 -> 1, 7 -> 1}]]]] /. gliRules$tp1 /. 
     GLI -> GLIMultiply] /. GLIMultiply -> GLI
```

$$-\frac{1}{2} G^{\text{tp1}}(0,0,0,1,0,1,1)+\frac{1}{2} G^{\text{tp1}}(0,1,0,1,-1,1,1)-\frac{1}{2} G^{\text{tp1}}(0,1,0,1,0,1,0)$$

```mathematica
mastersLit = {
    GLI[tp1, Normal[SparseArray[{5 -> 1, 7 -> 1}]]], 
    GLI[tp1, Normal[SparseArray[{1 -> 1, 3 -> 1, 4 -> 1, 6 -> 1, 7 -> 0}]]], 
    GLI[tp1, Normal[SparseArray[{1 -> 1, 3 -> 1, 6 -> 1, 7 -> 0}]]], 
    GLI[tp1, Normal[SparseArray[{4 -> 1, 6 -> 1, 7 -> 1}]]], 
    GLI[tp1, Normal[SparseArray[{4 -> 1, 5 -> 1, 6 -> 1, 7 -> 1}]]], 
    GLI[tp2, Normal[SparseArray[{1 -> 1, 3 -> 1, 4 -> 1, 6 -> 1, 7 -> 0}]]], 
    GLI[tp2, Normal[SparseArray[{1 -> 1, 3 -> 1, 4 -> 1, 5 -> 1, 6 -> 1, 7 -> 0}]]], 
   (*Three propagator ints *) 
    GLI[tp1, Normal[SparseArray[{1 -> 1, 4 -> 2, 7 -> 2}]]], 
    GLI[tp1, Normal[SparseArray[{1 -> 2, 4 -> 2, 7 -> 1}]]], 
   (*Four propagator ints *) 
    GLI[tp1, Normal[SparseArray[{1 -> 1, 4 -> 1, 5 -> 1, 7 -> 1}]]], 
    GLI[tp1, Normal[SparseArray[{2 -> 1, 4 -> 1, 6 -> 1, 7 -> 1}]]], 
   (*3rd int with sps added separately *) 
    GLI[tp1, Normal[SparseArray[{2 -> 1, 4 -> 1, 6 -> 1, 7 -> 1}]]], 
   (*the int above won't be used!*) 
    GLI[tp1, Normal[SparseArray[{2 -> 1, 4 -> 1, 6 -> 1, 7 -> 3}]]], 
    GLI[tp2, Normal[SparseArray[{2 -> 1, 3 -> 1, 4 -> 1, 6 -> 1, 7 -> 1}]]], 
    GLI[tp1(*!*), Normal[SparseArray[{1 -> 1, 3 -> 1, 4 -> 1, 6 -> 1, 7 -> 1}]]], 
    GLI[tp2, Normal[SparseArray[{1 -> 1, 3 -> 1, 4 -> 1, 5 -> 1, 6 -> 1, 7 -> 1}]]], 
    GLI[tp3, Normal[SparseArray[{1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1, 7 -> 1}]]], 
    Sequence @@ Cases2[intWithNumerators, GLI] 
   };
```

We run another reduction on the literature topologies to arrive at a minimal set of masters

```mathematica
KiraCreateConfigFiles[toposLit, mastersLit, dir, KiraMassDimensions -> {mt -> 1, s -> 2}];
```

```mathematica
KiraCreateIntegralFile[mastersLit, toposLit, dir];
```

$$\text{KiraCreateIntegralFile: Number of loop integrals: }13$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }4$$

$$\text{KiraCreateIntegralFile: Number of loop integrals: }1$$

```mathematica
KiraCreateJobFile[toposLit, mastersLit, dir];
```

Upon finishing the reduction we can load the results

```mathematica
(*kiraLitTables=(KiraImportResults[toposLit,dir])//Flatten;*)
```

```mathematica
SetDirectory[NotebookDirectory[]];
kiraLitTables = Get["Reduction-GlGl2H-aux.m"];
```

Final list of masters in the literature topologies

```mathematica
redLitM = Cases2[Dispatch[mastersLit /. kiraLitTables], GLI]
% // Length
```

$$\left\{G^{\text{tp1}}(0,0,0,0,0,1,1),G^{\text{tp1}}(0,0,0,1,0,1,1),G^{\text{tp1}}(0,0,0,1,1,1,1),G^{\text{tp1}}(0,0,1,0,0,1,2),G^{\text{tp1}}(0,0,2,0,0,1,1),G^{\text{tp1}}(0,1,0,1,0,1,1),G^{\text{tp1}}(0,1,0,1,0,1,2),G^{\text{tp1}}(0,2,0,1,0,1,1),G^{\text{tp1}}(1,0,0,1,1,0,1),G^{\text{tp1}}(1,0,1,0,0,1,0),G^{\text{tp1}}(1,0,1,1,0,1,0),G^{\text{tp1}}(1,0,1,1,0,1,1),G^{\text{tp2}}(0,1,1,1,0,1,1),G^{\text{tp2}}(1,0,0,1,1,1,0),G^{\text{tp2}}(1,0,1,1,0,1,0),G^{\text{tp2}}(1,0,1,1,1,1,0),G^{\text{tp2}}(1,0,2,0,1,1,1),G^{\text{tp3}}(1,1,1,1,1,0,1)\right\}$$

$$18$$

Final list of our masters

```mathematica
finMasters = Cases2[integralMappings[[2]] /. linRels, GLI];
```

```mathematica
toLitMappings = FCLoopFindIntegralMappings[redLitM, Join[toposLit, completedTopos], PreferredIntegrals -> integralMappings[[2]], 
    FCParallelize -> True];
```

Importing literature results from TeX

```mathematica
tmp = StringReplace[StringSplit[StringReplace[Import["ME2FM.tex", "Text"], 
       {"\\nonumber" | "\\qquad" | "\\quad" | "\\\\" | "\\Bigg" | "\\bigg" | "\\," -> " ", "&" -> "", 
        "{\\SetScale{0.8} \\dt{22}}" -> "{dt}[22]", "\n" -> "", "[" -> "(", "]" -> ")", "\{" -> "(", "\}" -> ")", 
        "C_F" -> "CF", "C_A" -> "CA", "+{\\cal O}(ep) ." -> "", 
        "{\\cal O}(\\epsilon)" -> ""}], "="][[2]], {"\\frac{" ~~ Shortest[x__] ~~ "}{" ~~ Shortest[y__] ~~ "}" :>"(" <> x <> ")/(" <> y <> ")", "+ ." -> ""}];
```

```mathematica
lhs = StringCases[StringReplace[tmp, "\\epsilon" -> " ep "], "\\" ~~ Shortest[x__] ~~ "{" ~~ Shortest[y__] ~~ "}", Infinity];
rhs = StringCases[StringReplace[tmp, "\\epsilon" -> " ep "], "\\" ~~ Shortest[x__] ~~ "{" ~~ Shortest[y__] ~~ "}" :> "{" <> x <> "}[" <> y <> "]", Infinity];
```

```mathematica
tmp2 = ToExpression[StringReplace[StringReplace[tmp, Thread[Rule[lhs, rhs]]], {"N" -> "{CA}", "CF" -> "{CF}"}], TeXForm];
```

$$\text{resLit}=\text{tmp2}\;\text{/.}\epsilon \;\text{-$>$}\;\text{ep}\;\text{/.}\{\text{dt}[\_]\text{-$>$}\;\text{mastersLit}[[1]]\text{mark}[1],\text{db}[\_]\text{-$>$}\;\text{mastersLit}[[2]]\text{mark}[2],\text{bttwo}[\_]\text{-$>$}\;\text{mastersLit}[[4]]\text{mark}[3],\text{tritad}[\_]\text{-$>$}\;\text{mastersLit}[[5]]\text{mark}[4],\text{tribub}[\_]\text{-$>$}\;\text{mastersLit}[[7]]\text{mark}[6],\text{glasses}[\_]\text{-$>$}\;\text{mastersLit}[[6]]\text{mark}[5],\text{ssonetwotwo}[\_]\text{-$>$}\;\text{mastersLit}[[8]]\text{mark}[7],\text{sstwoonetwo}[\_]\text{-$>$}\;\text{mastersLit}[[9]]\text{mark}[8],\text{mpfour}[\_]\text{-$>$}\;\text{mastersLit}[[10]]\text{mark}[9],\text{tria}[\_]\text{-$>$}\;\text{mastersLit}[[11]]\text{mark}[10],\text{triathree}[\_]\text{-$>$}\;\text{mastersLit}[[13]]\text{mark}[11],\text{dtria}[\_]\text{-$>$}\;\text{mastersLit}[[15]]\text{mark}[12],\text{mpsix}[\_]\text{-$>$}\;\text{mastersLit}[[16]]\text{mark}[13],\text{xtria}[\_]\text{-$>$}\;\text{mastersLit}[[17]]\text{mark}[14],\text{(*}\;\text{dtria}[\_]\text{-$>$}\;\text{mastersLit}[17],\text{*)}\;\text{triatwo}[\_]\text{-$>$}\;\text{intWithNumerators} \;\text{mark}[15]\};$$

```mathematica
resLit2 = (resLit //. {ep[x_] :> ep x, x[y_] :> x y, Power[x, n_][y_] :> x^n y, 
      Power[ep, n_][x_] :> ep^n x, s[y_] :> s y});
```

One of the masters from the literature has a different pole structure as compared to our masters it is related to.
Since the amplitude in the literature has already been expanded to O(ep^0), we need to keep that master to avoid
a precision loss

```mathematica
xtraRule = Solve[mastersLit[[16]] == (mastersLit[[16]] /. kiraLitTables /. toLitMappings[[1]] /. linRels), 
    GLI["fctopology10C", {1, 0, 1, 1, 1, 2, 0}]] // First
```

$$\left\{G^{\text{fctopology10C}}(1,0,1,1,1,2,0)\to \frac{1}{2 \;\text{mt}^2 \left(4 \;\text{mt}^2-s\right)}\left(-D G^{\text{fctopology11C}}(0,0,1,1,1,1,0)+2 D \;\text{mt}^2 G^{\text{fctopology6C}}(1,1,0,1,1,1,0)-4 D \;\text{mt}^4 G^{\text{tp2}}(1,0,1,1,1,1,1)+D \;\text{mt}^2 s G^{\text{tp2}}(1,0,1,1,1,1,1)+2 G^{\text{fctopology11C}}(0,0,1,1,1,1,0)-6 \;\text{mt}^2 G^{\text{fctopology6C}}(1,1,0,1,1,1,0)+16 \;\text{mt}^4 G^{\text{tp2}}(1,0,1,1,1,1,1)-4 \;\text{mt}^2 s G^{\text{tp2}}(1,0,1,1,1,1,1)\right)\right\}$$

Applying a projector to our result and fixing the normalization

```mathematica
pref = ((-4*I)*mW*sinW)/(e*gs^4);
```

```mathematica
kProjRule = {Pair[Momentum[Polarization[q1, I, Transversality -> True], D], Momentum[Polarization[q2, I, Transversality -> True], D]] -> 
     1/s + 1/s 2 Pair[Momentum[q1, D], 
        Momentum[Polarization[q2, I, Transversality -> True], D]]*Pair[Momentum[q2, D], Momentum[Polarization[q1, I, Transversality -> True], D]], 
    SUNDelta[_, _] -> 1};
resFinalProj = Collect2[pref resFinal /. kProjRule /. linRels /. xtraRule /. D -> 4 - 2 ep, GLI, 
      CA, CF, FCParallelize -> True] // Series[#, {ep, 0, 0}] & // Normal;
```

```mathematica
resLit3 = Collect2[resLit2, mark, GLI, FCParallelize -> True, 
        Factoring -> fun] /. fun[x_] :> FCLoopAddMissingHigherOrdersWarning[x, ep, help] /. SelectFree[kiraLitTables, mastersLit[[16]]] /. toLitMappings[[1]] /. linRels;
```

```mathematica
resLitFinal = Collect2[resLit3 /. mark[_] -> 1 /. D -> 4 - 2 ep, GLI, FCParallelize -> True] // Series[#, {ep, 0, 0}] & // Normal;
```

```mathematica
(*ClearAll[x]
tau= 4mtsq/s
Solve[x==(Sqrt[1-tau]-1)/(Sqrt[1-tau]+1),mtsq]//First*)
```

Checking the agreement with the literature

```mathematica
diff = Collect2[(resLitFinal - resFinalProj) /. mt -> Sqrt[mtsq] /. {mtsq -> -((s*x)/(-1 + x)^2)}, ep, GLI]
```

$$0$$

```mathematica
FCCompareResults[diff, 0, 
       Text -> {"\tCompare to Anastasiou, Beerli, Bucherer, Daleo, Kunszt,     arXiv:hep-ph/0611236, A.3:", "CORRECT.", "WRONG!"}, 
       Interrupt -> {Hold[Quit[1]], Automatic}, 
       Factoring -> Simplify]; 
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], 
       " s."];

```mathematica

$$\text{$\backslash $tCompare to Anastasiou, Beerli, Bucherer, Daleo, Kunszt,     arXiv:hep-ph/0611236, A.3:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }102.876\text{ s.}$$