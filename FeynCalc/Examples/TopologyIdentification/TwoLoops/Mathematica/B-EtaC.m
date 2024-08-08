(* ::Package:: *)

(* :Title: B-EtaC															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  B -> EtaC, QCD, topology minimization, 2-loops  			  *)

(* ------------------------------------------------------------------------ *)


(* ::Title:: *)
(*B decaying to EtaC*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="B -> EtaC, QCD, topology minimization, 2-loops";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
<<FeynCalc`

FCCheckVersion[10,0,0];


(* ::Section:: *)
(*Load the topologies*)


SetDirectory[FCGetNotebookDirectory[]];


rawTopologies0=Get["RawTopologies-B-Etac.m"];


(* ::Section:: *)
(*Run the naive topology identification*)


fcVariables=\!\(TraditionalForm\`{gkin, meta, u0b}\);


(DataType[#,FCVariable]=True)&/@fcVariables;


rawTopologies=loopHead/@(rawTopologies0);


kinematics=\!\(TraditionalForm\`{\(Hold[SPD]\)[n] -> 0, \(Hold[SPD]\)[nb] -> 0, \(Hold[SPD]\)[n, nb] -> 2}\);


aux1=FCLoopFindTopologies[rawTopologies,{k1,k2},FCLoopIsolate->loopHead,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->kinematics,
Names->"preTopoDia",Head->Identity,FCLoopGetKinematicInvariants->False,FCLoopScalelessQ->False];


(* ::Text:: *)
(*This particular set of topologies contains mixed quadratic-eikonal that will cause issues with the topology minimization if we leave them as is.*)


(* ::Text:: *)
(*To handle this situation we employ the routine `FCLoopReplaceQuadraticEikonalPropagators`, telling it the loop momenta, kinematic constraints and the rules for completing the square for the pure loop parts of the propagators*)


topoPre=FCLoopReplaceQuadraticEikonalPropagators[aux1[[2]],LoopMomenta->{k1,k2},
InitialSubstitutions->{ExpandScalarProduct[SPD[k1-k2]]->SPD[k1-k2],
ExpandScalarProduct[SPD[k1+k2]]->SPD[k1+k2]},IntermediateSubstitutions->kinematics];


(* ::Section:: *)
(*Handle overdetermined propagator bases*)


(* ::Text:: *)
(*Single out topologies that have an overdetermined sets of propagators*)


overdeterminedToposPre=Select[topoPre,FCLoopBasisOverdeterminedQ];


overdeterminedToposPre//Length


(* ::Text:: *)
(*Generate partial fractioning rules to be applied to the original sets of denominators*)


AbsoluteTiming[pfrRules=FCLoopCreatePartialFractioningRules[aux1[[1]],topoPre];]


(* ::Text:: *)
(*Some examples of such rules are*)


pfrRules[[1]][[1;;2]]


(* ::Text:: *)
(*These rules can be converted to FORM and used for simplifying the amplitude.*)


(* ::Text:: *)
(*New topologies after partial fractioning*)


pfrToposPre=Union[First/@pfrRules[[2]]];
aux2={aux1[[1]]/.Dispatch[pfrRules[[1]]],topoPre};


(* ::Text:: *)
(*Some denominators from original topologies that do not require partial fractioning . Notice that the corresponding topologies themselves still might *)
(*contain overdetermined sets of propagators *)


remainderDens=SelectNotFree[aux2[[1]],First/@overdeterminedToposPre]//Union


(* ::Text:: *)
(*Determine which topologies related to these denominators are overdetermined*)


overdeterminedTopos=FCLoopSelectTopology[remainderDens,overdeterminedToposPre];


(* ::Text:: *)
(*Group  the  remaining  denominators  together  with  the  corresponding  topologies. Remove  the  now  irrelevant  propagators  from  the  leftover  topologies*)


toRemoveList={#,First@SelectNotFree[overdeterminedTopos,#[[1]]],First/@Position[#[[2]],0]}&/@remainderDens;
newNoPfrGLIs=(FCLoopRemovePropagator[#[[1]],#[[3]]]&/@toRemoveList);
newNoPfrTopos=(FCLoopRemovePropagator[#[[2]],#[[3]]]&/@toRemoveList);


(* ::Text:: *)
(*List of all resulting topologies upon doing partial fractioning*)


pfrTopos=Union[pfrToposPre,First/@newNoPfrTopos];


(* ::Text:: *)
(*Replacement  rule  for  renaming  preTopo - topologies  (with  PFR - suffixes  from  partial  fractioning)  to  pfrTopo  topologies*)


pfrToposNew=Table["pfrTopo"<>ToString[i],{i,1,Length[pfrTopos]}];
pfrRenRu=Thread[Rule[pfrTopos,pfrToposNew]];


(* ::Text:: *)
(*An  extra  rule  for  mapping  the  remaining  denominators  to  the  corresponding  topologies  with  removed  propagators*)


gliRulePfr=Thread[Rule[remainderDens,newNoPfrGLIs]]/.pfrRenRu;


(* ::Text:: *)
(*Final  list  of  topologies  upon  doing  partial  fractioning*)


relevantPFrTopos=Union[Cases[aux2[[1]]/.Dispatch[gliRulePfr],GLI[id_,___]:>id,Infinity]];
finalPreToposPfrRaw=SelectNotFree[Join[topoPre,pfrRules[[2]],newNoPfrTopos/.Dispatch[pfrRenRu]],relevantPFrTopos]//Union;


(* ::Text:: *)
(*Identify scaleless topologies among them*)


scalelessPfrTopos=Select[finalPreToposPfrRaw,FCLoopScalelessQ]/.Dispatch[pfrRenRu];


(* ::Text:: *)
(*Remove scaleless topologies. This gives us the final list of topologies after partial fractioning*)


finalPreTopos=SelectFree[finalPreToposPfrRaw/.pfrRenRu,scalelessPfrTopos]//Union;


(* ::Text:: *)
(*Check that there are no overdetermined topologies left*)


If[Union[FCLoopBasisOverdeterminedQ/@finalPreTopos]=!={False},
	Print["ERROR! Not all overdetermined topologies were eliminated."];
]


(* ::Text:: *)
(*Finally use `FCLoopFindTopologyMappings` to find mappings between topologies*)


AbsoluteTiming[mappedTopos=FCLoopFindTopologyMappings[finalPreTopos];]


(* ::Text:: *)
(*Allowing for shifts of external momenta would give us even more mapping relations but this is not safe unless we explicitly know that the amplitude is symmetric under such shifts*)


AbsoluteTiming[mappedToposTest=FCLoopFindTopologyMappings[finalPreTopos,Momentum->All];]


(* ::Text:: *)
(*Introducing new names for the final topologies*)


finTopoNames=First/@mappedTopos[[2]];
finTopoNamesNew=Table["finTopo"<>ToString[i],{i,1,Length[finTopoNames]}];
finRenRu=Thread[Rule[finTopoNames,finTopoNamesNew]];


(* ::Text:: *)
(*Some of the final topologies might be incomplete, so we need to account for that as well*)


finToposRenamed=mappedTopos[[2]]/.finRenRu;
incompleteTopos=Select[finToposRenamed,FCLoopBasisIncompleteQ];


(* ::Text:: *)
(*For the basis completion we can use all available propagators*)


allProps=Union[Flatten[#[[2]]&/@finToposRenamed]];
completedTopos=FCLoopBasisFindCompletion[incompleteTopos,Method->allProps];


(* ::Text:: *)
(*Generate basis completion rules*)


basisCompletionRules=FCLoopCreateRuleGLIToGLI[completedTopos,List/@incompleteTopos]//Flatten;


(* ::Text:: *)
(*Generating the ultimate list of topologies where all propagator sets now form a basis*)


ultimateTopos=finToposRenamed/.Thread[Rule[incompleteTopos,completedTopos]];
ultimateToposNewNames=Table["topology"<>ToString[i],{i,1,Length[finToposRenamed]}];
ultimateToposRenamingRule=Thread[Rule[First/@ultimateTopos,ultimateToposNewNames]];
ultimateToposRenamed=ultimateTopos/.ultimateToposRenamingRule;
fcTopologies=ultimateToposRenamed;


(* ::Text:: *)
(*Finally, we  also  need  rules  to  eliminate  scalar  products*)


ruGLI=Map[{#[[1]],FCLoopCreateRulesToGLI[#]}&,fcTopologies//FCLoopTopologyNameToSymbol];


(* ::Text:: *)
(*Names of the final topologies*)


sortedTopologyNames=First/@fcTopologies;
