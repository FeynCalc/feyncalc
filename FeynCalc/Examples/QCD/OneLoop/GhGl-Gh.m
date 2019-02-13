(* ::Package:: *)

(* :Title: GhGl-Gh															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  GhGl - Gh, QCD, only UV divergences, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*QCD ghost-gluon vertex at 1-loop*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QCD model created with FeynRules. Please evaluate the file*)
(*FeynCalc/Examples/FeynRules/QCD/GenerateModelQCD.m before running it for the first time.*)


description="GhGl - Gh, QCD, only UV divergences, 1-loop";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;

If[!MatchQ[ToExpression[StringSplit[$FeynCalcVersion, "."]],{a_/;a>=9,b_/;b>=3,_}],
	If[ ($FrontEnd === Null||$Notebooks===False),
	Print["Your FeynCalc version is too old. \
This example requires at least FeynCalc 9.3!"];
	Quit[],
	CreateDialog[{TextCell["Your FeynCalc version is too old. \
This example requires at least FeynCalc 9.3!"],DefaultButton[]},
	Modal->True];
	]
];


(* ::Section:: *)
(*Configure some options*)


(* ::Text:: *)
(*We keep scaleless B0 functions, since otherwise the UV part would not come out right.*)


$KeepLogDivergentScalelessIntegrals=True;


FAPatch[PatchModelsOnly->True];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[mu,TraditionalForm]:="\[Mu]";
MakeBoxes[nu,TraditionalForm]:="\[Nu]";
MakeBoxes[rho,TraditionalForm]:="\[Rho]";


template = insertFields[createTopologies[1, 1 -> 2,
		ExcludeTopologies -> {Tadpoles,WFCorrections,
		WFCorrectionCTs,SelfEnergies}], {U[5]} ->
		{V[5],U[5]}, InsertionLevel -> {Particles},
		Model -> FileNameJoin[{"QCD","QCD"}],
		GenericModel -> FileNameJoin[{"QCD","QCD"}]];


diags=template/.createTopologies->CreateTopologies/.
	insertFields->InsertFields;
diagsCT=template/.createTopologies->CreateCTTopologies/.
	insertFields->InsertFields;


Paint[diags, ColumnsXRows -> {2,1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


Paint[diagsCT, ColumnsXRows -> {2,1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitudes*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit. We keep the full gauge dependence.*)


amp1[0] = FCFAConvert[CreateFeynAmp[DiagramExtract[diags,{1,2}],
	Truncated->True, GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p1}, OutgoingMomenta->{p2,p3},
	LorentzIndexNames->{mu,nu,rho}, DropSumOver->True,
	SUNIndexNames->{a,b,c}, LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions->{SMP["m_u"]->SMP["m_q"]}]


(* ::Text:: *)
(*Counter-term*)


amp2[0] = FCFAConvert[CreateFeynAmp[diagsCT,
	Truncated->True, GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p1}, OutgoingMomenta->{p2,p3},
	LorentzIndexNames->{mu,nu,rho}, SUNIndexNames->{a,b,c},
	DropSumOver->True, LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions->{SMP["m_u"]->SMP["m_q"],
	ZA->SMP["Z_A"],Zg->SMP["Z_g"],Zu->SMP["Z_u"]}]


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Ghost-gluon vertex*)


AbsoluteTiming[amp1[1]=TID[(FCE[amp1[0]]/.{-p2-p3->-p1}),l,
	UsePaVeBasis->True,ToPaVe->True];]


amp1Div[0]=amp1[1]//PaVeUVPart[#,Prefactor->1/(2Pi)^D]&;


amp1Div[1]=amp1Div[0]//SUNSimplify[#,Explicit->True]&//ReplaceAll[#,
	SUNTrace[x__]:>SUNTrace[x,Explicit->True]]&//
	FCReplaceD[#,D->4-2 Epsilon]&//Series[#,{Epsilon,0,0}]&//
	Normal//FCHideEpsilon//SelectNotFree2[#,SMP["Delta"]]&//FCE//
	Collect2[#,MTD,Factoring->Function[x,MomentumCombine[Factor[x]]]]&


(* ::Subsection:: *)
(*Counter-term*)


amp2[1]=amp2[0]//ReplaceAll[#,{SMP["Z_A"]->1+alpha SMP["d_A"],
	SMP["Z_u"]->1+alpha SMP["d_u"],
	SMP["Z_g"]->1+alpha SMP["d_g"]}]&//Series[#,{alpha,0,1}]&//
	Normal//ReplaceAll[#,alpha->1]&//ExpandScalarProduct//FCE//
	Collect2[#,MTD,GaugeXi,Factoring->Function[x,MomentumCombine[Factor[x]]]]&


(* ::Text:: *)
(*Check the cancellation of the UV divergences in the MSbar scheme. The renormalization constants*)
(*are obtained from another example calculation, "Renormalization.m"*)


renormalizationConstants = {
	SMP["d_A"]->SMP["alpha_s"]/(4Pi) SMP["Delta"] (1/2 CA(13/3-GaugeXi["G"])-2/3 Nf),
	SMP["d_g"]->((-11*CA*SMP["alpha_s"])/(24 Pi)SMP["Delta"] + (Nf*SMP["alpha_s"])/(12*Pi)SMP["Delta"]),
	SMP["d_u"]->SMP["alpha_s"]/(4Pi) CA SMP["Delta"] (3-GaugeXi["G"])/4
}/.SMP["alpha_s"]->SMP["g_s"]^2/(4Pi);


uvDiv[0]=ExpandScalarProduct[amp1Div[1]+ amp2[1]]//Simplify


uvDiv[1]=(uvDiv[0]/.renormalizationConstants)//Simplify


FCCompareResults[uvDiv[1],0,
Text->{"\tThe UV divergence of the ghost-gluon vertex at 1-loop is cancelled by \
the counter-term :",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];


(* ::Section:: *)
(*Check the final results*)


knownResult =
	-I(-I SMP["g_s"] FVD[p3,mu]SUNF[a,b,c]( SMP["g_s"]^2/(4Pi)^2 CA*
		GaugeXi["G"]/2 SMP["Delta"]));
FCCompareResults[amp1Div[1],knownResult,
Text->{"\tCompare to Muta, Foundations of QCD, \
Eq. 2.5.142:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
