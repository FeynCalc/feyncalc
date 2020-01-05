(* ::Package:: *)

(* :Title: ElMu-ElMu														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  El Mu -> El Mu, QED, matrix element squared, tree				*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Electron-muon scattering*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El Mu -> El Mu, QED, matrix element squared, tree";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"FeynArtsLoader"};
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


MakeBoxes[p1,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(2\)]\)";
MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[2, {1}], F[2, {2}]} ->
		{F[ 2, {1}], F[2, {2}]}, InsertionLevel -> {Classes},
		Restrictions->QEDOnly];

Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,
	List->False, SMP->True, Contract->True]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2,
	SMP["m_e"], SMP["m_mu"], SMP["m_e"], SMP["m_mu"]];


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
	DiracSimplify//Simplify


ampSquaredMassless1[0] = ampSquared[0]//ReplaceAll[#,{SMP["m_e"] -> 0}]&//
	Simplify


ampSquaredMassless2[0] = ampSquared[0]//ReplaceAll[#,{
	SMP["m_e"] -> 0,SMP["m_mu"] -> 0}]&//Simplify


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(8SMP["e"]^4 (SP[p1,k2]SP[p2,k1]+SP[p1,p2]SP[k1,k2]-
	SMP["m_mu"]^2 SP[p1,k1]))/(SP[k1-p1])^2//ExpandScalarProduct//
	ReplaceAll[#,SMP["m_e"]->0]&,
	((8SMP["e"]^4/t^2)((s/2)^2+(u/2)^2))
};
FCCompareResults[{ampSquaredMassless1[0],ampSquaredMassless2[0]},knownResults,
Text->{"\tCompare to Peskin and Schroeder, An Introduction to QFT, \
Eqs 5.61 and 5.71:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
