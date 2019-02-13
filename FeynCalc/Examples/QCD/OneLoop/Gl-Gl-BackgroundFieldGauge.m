(* ::Package:: *)

(* :Title: Gl-Gl-BackgroundFieldGauge										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Gl -> Gl, YM+BGF, only UV divergences, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Pure Yang-Mills 1-loop gluon self-energy in the background field formalism*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Gl -> Gl, YM+BGF, only UV divergences, 1-loop";
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
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[mu,TraditionalForm]:="\[Mu]";
MakeBoxes[nu,TraditionalForm]:="\[Nu]";


diags=InsertFields[CreateTopologies[1, 1 -> 1,ExcludeTopologies -> {Tadpoles}],
	{V[50,{a}]} -> {V[50,{b}]},InsertionLevel -> {Classes},
	Model -> FileNameJoin[{"QCDBGF","QCDBGF"}],
	GenericModel->FileNameJoin[{"QCDBGF","QCDBGF"}],
	ExcludeParticles->{F[_]}];

Paint[diags, ColumnsXRows -> {2,2}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,512}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, GaugeRules->{},
	PreFactor->1], IncomingMomenta->{p}, OutgoingMomenta->{p},LoopMomenta->{l},
	LorentzIndexNames->{mu,nu}, UndoChiralSplittings->True,
	ChangeDimension->D, List->True, SMP->True, DropSumOver->True,
	FinalSubstitutions->{SMP["m_u"]->SMP["m_q"],
	GaugeXi[V[5, {_}]]:>GaugeXi[G]}];


amp[1]=DiracSimplify/@amp[0];


amp[2]=SUNSimplify[TID[#,l,ToPaVe->True]]&/@amp[1];


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude.*)


ampDiv[0]=PaVeUVPart[#,Prefactor->1/(2Pi)^D]&/@amp[2]


ampDiv[1]=FCReplaceD[ampDiv[0],D->4-2Epsilon]//
	Series[#,{Epsilon,0,-1}]&//Normal//Simplify


(* ::Section:: *)
(*Check the final results*)


knownResult = {
	0,
	0,
	I CA SMP["g_s"]^2 SUNDelta[a,b]/(4Pi)^2(1/(3 Epsilon))*
		(MTD[mu,nu]SPD[p]-FVD[p,mu]FVD[p,nu]),
	I CA SMP["g_s"]^2 SUNDelta[a,b]/(4Pi)^2(10/(3 Epsilon))*
		(MTD[mu,nu]SPD[p]-FVD[p,mu]FVD[p,nu])
}//FCI;
FCCompareResults[ampDiv[1]/.GaugeXi[G]->1,knownResult,
Text->{"\tCompare to Abbott, \
Nucl. Phys. B 185 (1981) 189-203, Eqs 5.11-5.12:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
