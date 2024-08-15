---
title: B decaying to EtaC
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "B -> EtaC, QCD, topology minimization, 2-loops";
If[ $FrontEnd === Null, 
  	$FeynCalcStartupMessages = False; 
  	Print[description]; 
  ];
If[ $Notebooks === False, 
  	$FeynCalcStartupMessages = False 
  ];
<< FeynCalc` 
 
FCCheckVersion[10, 0, 0];
```

$$\text{FeynCalc }\;\text{10.0.0 (dev version, 2024-08-07 16:59:34 +02:00, 2f62a22c). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{ The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

## Load the topologies

```mathematica
SetDirectory[FCGetNotebookDirectory[]];
```

```mathematica
rawTopologies0 = Get["RawTopologies-B-Etac.m"];
```

## Run the naive topology identification

$$\text{fcVariables}=\{\text{gkin},\text{meta},\text{u0b}\};$$

```mathematica
(DataType[#, FCVariable] = True) & /@ fcVariables;
```

```mathematica
rawTopologies = loopHead /@ (rawTopologies0);
```

$$\text{kinematics}=\{\text{Hold}[\text{SPD}][n]\text{-$>$}0,\text{Hold}[\text{SPD}][\text{nb}]\text{-$>$}0,\text{Hold}[\text{SPD}][n,\text{nb}]\text{-$>$}2\};$$

```mathematica
aux1 = FCLoopFindTopologies[rawTopologies, {k1, k2}, FCLoopIsolate -> loopHead, 
    FCLoopBasisOverdeterminedQ -> True, FinalSubstitutions -> kinematics, 
    Names -> "preTopoDia", Head -> Identity, FCLoopGetKinematicInvariants -> False, FCLoopScalelessQ -> False];
```

$$\text{FCLoopFindTopologies: Number of the initial candidate topologies: }248$$

$$\text{FCLoopFindTopologies: Number of the identified unique topologies: }184$$

$$\text{FCLoopFindTopologies: Number of the preferred topologies among the unique topologies: }0$$

$$\text{FCLoopFindTopologies: Number of the identified subtopologies: }64$$

$$\text{FCLoopFindTopologies: Follwing identified topologies are scaleless and will be set to zero: }\{\text{preTopoDia31},\text{preTopoDia32},\text{preTopoDia33},\text{preTopoDia34},\text{preTopoDia60},\text{preTopoDia61},\text{preTopoDia77},\text{preTopoDia86},\text{preTopoDia88},\text{preTopoDia98},\text{preTopoDia110},\text{preTopoDia111},\text{preTopoDia118},\text{preTopoDia119},\text{preTopoDia120},\text{preTopoDia123},\text{preTopoDia139},\text{preTopoDia144},\text{preTopoDia152},\text{preTopoDia155},\text{preTopoDia156},\text{preTopoDia157},\text{preTopoDia158},\text{preTopoDia159},\text{preTopoDia160},\text{preTopoDia161},\text{preTopoDia162},\text{preTopoDia163},\text{preTopoDia164},\text{preTopoDia165},\text{preTopoDia166},\text{preTopoDia170},\text{preTopoDia174},\text{preTopoDia175},\text{preTopoDia176},\text{preTopoDia177},\text{preTopoDia178},\text{preTopoDia179},\text{preTopoDia180},\text{preTopoDia181},\text{preTopoDia182},\text{preTopoDia183},\text{preTopoDia184}\}$$

This particular set of topologies contains mixed quadratic-eikonal that will cause issues with the topology minimization if we leave them as is.

To handle this situation we employ the routine `FCLoopReplaceQuadraticEikonalPropagators`, telling it the loop momenta, kinematic constraints and the rules for completing the square for the pure loop parts of the propagators

```mathematica
topoPre = FCLoopReplaceQuadraticEikonalPropagators[aux1[[2]], LoopMomenta -> {k1, k2}, 
    InitialSubstitutions -> {ExpandScalarProduct[SPD[k1 - k2]] -> SPD[k1 - k2], 
      ExpandScalarProduct[SPD[k1 + k2]] -> SPD[k1 + k2]}, IntermediateSubstitutions -> kinematics];
```

## Handle overdetermined propagator bases

Single out topologies that have an overdetermined sets of propagators

```mathematica
overdeterminedToposPre = Select[topoPre, FCLoopBasisOverdeterminedQ];
```

```mathematica
overdeterminedToposPre // Length
```

$$105$$

Generate partial fractioning rules to be applied to the original sets of denominators

```mathematica
AbsoluteTiming[pfrRules = FCLoopCreatePartialFractioningRules[aux1[[1]], topoPre];]
```

$$\{76.2728,\text{Null}\}$$

Some examples of such rules are

```mathematica
pfrRules[[1]][[1 ;; 2]]
```

$$\left\{G^{\text{preTopoDia49}}(1,1,0,1,1,1,1)\to \frac{G^{\text{preTopoDia49PFR23}}(1,2,1,1,1)}{\text{meta} \;\text{u0b}}-\frac{G^{\text{preTopoDia49PFR35}}(1,1,2,1,1)}{\text{meta} (\text{u0b}-1)}+\frac{G^{\text{preTopoDia49PFR36}}(1,1,2,1,1)}{\text{meta} (\text{u0b}-1) \;\text{u0b}},G^{\text{preTopoDia126}}(1,1,1,1,1,1)\to \frac{G^{\text{preTopoDia126PFR2}}(1,1,2,1,1)}{\text{meta} \;\text{u0b}}-\frac{G^{\text{preTopoDia126PFR5}}(1,1,1,2,1)}{\text{meta} (\text{u0b}-1)}+\frac{G^{\text{preTopoDia126PFR6}}(1,1,1,2,1)}{\text{meta} (\text{u0b}-1) \;\text{u0b}}\right\}$$

These rules can be converted to FORM and used for simplifying the amplitude.

New topologies after partial fractioning

```mathematica
pfrToposPre = Union[First /@ pfrRules[[2]]];
aux2 = {aux1[[1]] /. Dispatch[pfrRules[[1]]], topoPre};
```

Some denominators from original topologies that do not require partial fractioning . Notice that the corresponding topologies themselves still might 
contain overdetermined sets of propagators 

```mathematica
remainderDens = SelectNotFree[aux2[[1]], First /@ overdeterminedToposPre] // Union
```

$$\left\{G^{\text{preTopoDia100}}(1,1,0,0,1,1,1),G^{\text{preTopoDia100}}(1,1,0,0,1,2,1),G^{\text{preTopoDia105}}(1,1,1,0,0,2,1),G^{\text{preTopoDia116}}(1,1,1,1,1,1,0),G^{\text{preTopoDia126}}(1,1,1,0,0,1),G^{\text{preTopoDia126}}(1,2,1,1,1,0),G^{\text{preTopoDia130}}(1,1,0,0,2,1),G^{\text{preTopoDia142}}(1,1,0,0,2,1),G^{\text{preTopoDia143}}(1,1,1,2,0,1),G^{\text{preTopoDia147}}(1,1,0,1,1,1),G^{\text{preTopoDia147}}(1,1,0,2,1,1),G^{\text{preTopoDia149}}(1,0,1,0,2,1),G^{\text{preTopoDia2}}(1,1,1,0,1,1,0,1),G^{\text{preTopoDia22}}(1,1,0,0,0,0,0,1),G^{\text{preTopoDia23}}(1,1,1,1,1,0,1,0),G^{\text{preTopoDia25}}(1,1,0,1,1,1,0,1),G^{\text{preTopoDia28}}(1,1,0,1,1,1,0,1),G^{\text{preTopoDia39}}(1,0,1,0,1,1,1,1),G^{\text{preTopoDia39}}(1,1,1,0,1,1,0,1),G^{\text{preTopoDia4}}(1,1,0,2,0,1,1,0),G^{\text{preTopoDia49}}(1,1,0,0,1,0,1),G^{\text{preTopoDia76}}(1,1,0,0,1,2,1),G^{\text{preTopoDia79}}(1,1,1,1,1,0,1),G^{\text{preTopoDia93}}(1,1,1,0,2,0,1),G^{\text{preTopoDia95}}(1,1,0,1,1,1,0),G^{\text{preTopoDia95}}(1,1,1,0,1,1,0),G^{\text{preTopoDia96}}(1,1,1,0,0,1,1)\right\}$$

Determine which topologies related to these denominators are overdetermined

```mathematica
overdeterminedTopos = FCLoopSelectTopology[remainderDens, overdeterminedToposPre];
```

Group  the  remaining  denominators  together  with  the  corresponding  topologies. Remove  the  now  irrelevant  propagators  from  the  leftover  topologies

```mathematica
toRemoveList = {#, First@SelectNotFree[overdeterminedTopos, #[[1]]], First /@ Position[#[[2]], 0]} & /@ remainderDens;
newNoPfrGLIs = (FCLoopRemovePropagator[#[[1]], #[[3]]] & /@ toRemoveList);
newNoPfrTopos = (FCLoopRemovePropagator[#[[2]], #[[3]]] & /@ toRemoveList);
```

List of all resulting topologies upon doing partial fractioning

```mathematica
pfrTopos = Union[pfrToposPre, First /@ newNoPfrTopos];
```

Replacement  rule  for  renaming  preTopo - topologies  (with  PFR - suffixes  from  partial  fractioning)  to  pfrTopo  topologies

```mathematica
pfrToposNew = Table["pfrTopo" <> ToString[i], {i, 1, Length[pfrTopos]}];
pfrRenRu = Thread[Rule[pfrTopos, pfrToposNew]];
```

An  extra  rule  for  mapping  the  remaining  denominators  to  the  corresponding  topologies  with  removed  propagators

```mathematica
gliRulePfr = Thread[Rule[remainderDens, newNoPfrGLIs]] /. pfrRenRu;
```

Final  list  of  topologies  upon  doing  partial  fractioning

```mathematica
relevantPFrTopos = Union[Cases[aux2[[1]] /. Dispatch[gliRulePfr], GLI[id_, ___] :> id, Infinity]];
finalPreToposPfrRaw = SelectNotFree[Join[topoPre, pfrRules[[2]], newNoPfrTopos /. Dispatch[pfrRenRu]],relevantPFrTopos] // Union;
```

Identify scaleless topologies among them

```mathematica
scalelessPfrTopos = Select[finalPreToposPfrRaw, FCLoopScalelessQ] /. Dispatch[pfrRenRu];
```

Remove scaleless topologies. This gives us the final list of topologies after partial fractioning

```mathematica
finalPreTopos = SelectFree[finalPreToposPfrRaw /. pfrRenRu, scalelessPfrTopos] // Union;
```

Check that there are no overdetermined topologies left

```mathematica
If[Union[FCLoopBasisOverdeterminedQ /@ finalPreTopos] =!= {False}, 
 	Print["ERROR! Not all overdetermined topologies were eliminated."]; 
 ]
```

Finally use `FCLoopFindTopologyMappings` to find mappings between topologies

```mathematica
AbsoluteTiming[mappedTopos = FCLoopFindTopologyMappings[finalPreTopos];]
```

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo378 and pfrTopo114. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo413 and pfrTopo114. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo76 and pfrTopo114. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo82 and pfrTopo114. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo97 and pfrTopo114. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo207 and pfrTopo154. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo320 and pfrTopo154. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo327 and pfrTopo154. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo6 and pfrTopo154. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo450 and pfrTopo19. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo80 and pfrTopo19. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo278 and pfrTopo203. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo349 and pfrTopo203. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies pfrTopo63 and pfrTopo23. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindMomentumShifts: }\;\text{Failed to derive the momentum shifts between topologies preTopoDia171 and pfrTopo23. This can be due to the presence of nonquadratic propagators or because shifts in external momenta are also necessary.}$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }190\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }241$$

$$\{22.4032,\text{Null}\}$$

Allowing for shifts of external momenta would give us even more mapping relations but this is not safe unless we explicitly know that the amplitude is symmetric under such shifts

```mathematica
AbsoluteTiming[mappedToposTest = FCLoopFindTopologyMappings[finalPreTopos, Momentum -> All];]
```

$$\text{FCLoopFindTopologyMappings: }\;\text{Found }205\text{ mapping relations }$$

$$\text{FCLoopFindTopologyMappings: }\;\text{Final number of independent topologies: }226$$

$$\{23.6277,\text{Null}\}$$

Introducing new names for the final topologies

```mathematica
finTopoNames = First /@ mappedTopos[[2]];
finTopoNamesNew = Table["finTopo" <> ToString[i], {i, 1, Length[finTopoNames]}];
finRenRu = Thread[Rule[finTopoNames, finTopoNamesNew]];
```

Some of the final topologies might be incomplete, so we need to account for that as well

```mathematica
finToposRenamed = mappedTopos[[2]] /. finRenRu;
incompleteTopos = Select[finToposRenamed, FCLoopBasisIncompleteQ];
```

For the basis completion we can use all available propagators

```mathematica
allProps = Union[Flatten[#[[2]] & /@ finToposRenamed]];
completedTopos = FCLoopBasisFindCompletion[incompleteTopos, Method -> allProps];
```

Generate basis completion rules

```mathematica
basisCompletionRules = FCLoopCreateRuleGLIToGLI[completedTopos, List /@ incompleteTopos] //Flatten;
```

Generating the ultimate list of topologies where all propagator sets now form a basis

```mathematica
ultimateTopos = finToposRenamed /. Thread[Rule[incompleteTopos, completedTopos]];
ultimateToposNewNames = Table["topology" <> ToString[i], {i, 1, Length[finToposRenamed]}];
ultimateToposRenamingRule = Thread[Rule[First /@ ultimateTopos, ultimateToposNewNames]];
ultimateToposRenamed = ultimateTopos /. ultimateToposRenamingRule;
fcTopologies = ultimateToposRenamed;
```

Finally, we  also  need  rules  to  eliminate  scalar  products

```mathematica
ruGLI = Map[{#[[1]], FCLoopCreateRulesToGLI[#]} &, fcTopologies // FCLoopTopologyNameToSymbol];
```

Names of the final topologies

```mathematica
sortedTopologyNames = First /@ fcTopologies;
```