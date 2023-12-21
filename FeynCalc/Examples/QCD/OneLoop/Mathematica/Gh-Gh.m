(* ::Package:: *)

(* :Title: Gh-Gh															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Gh -> Gh, QCD, only UV divergences, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*QCD ghost self-energy*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Gh -> Gh, QCD, only UV divergences, 1-loop";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,1];


(* ::Section:: *)
(*Configure some options*)


(* ::Text:: *)
(*We keep scaleless B0 functions, since otherwise the UV part would not come out right.*)


$KeepLogDivergentScalelessIntegrals=True;


(* ::Section:: *)
(*Generate Feynman diagrams*)


diags = InsertFields[CreateTopologies[1, 1 -> 1, ExcludeTopologies->{Tadpoles}],
		{U[5]} -> {U[5]}, InsertionLevel -> {Particles}, Model -> "SMQCD"];

Paint[diags, ColumnsXRows -> {1,1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, GaugeRules->{},
	PreFactor->1], IncomingMomenta->{p}, OutgoingMomenta->{p},LoopMomenta->{q},
	UndoChiralSplittings->True, ChangeDimension->D, List->False, SMP->True,
	DropSumOver->True, Contract->True]


(* ::Section:: *)
(*Calculate the amplitude*)


amp[1]=amp[0]//SUNSimplify//TID[#,q,ToPaVe->True]&


(* ::Text:: *)
(*The UV divergence of the amplitude can be obtained via PaVeUVPart.*)
(*Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.*)
(*Hint: If you need the full result for the amplitude, use PaXEvaluate from FeynHelpers.*)


ampDiv[0]=PaVeUVPart[amp[1],Prefactor->1/(2Pi)^D]//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
SelectNotFree2[#,Epsilon]&//Simplify


(* ::Text:: *)
(*The self-energy amplitude is usually defined as  (p^2 delta^ab  Pi(p^2)*)


pi[0]= FCI[ampDiv[0]/(I SUNDelta[SUNIndex[Glu1], SUNIndex[Glu2]]*SPD[p,p])]//Cancel


(* ::Section:: *)
(*Check the final results*)


knownResult = -SMP["g_s"]^2/(4Pi)^2 CA (3-GaugeXi[g])/4*1/Epsilon;
FCCompareResults[pi[0],knownResult,
Text->{"\tCompare to Muta, Foundations of QCD, \
Eq. 2.5.136:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
