(* ::Package:: *)

(* :Title: QutbarQdt-NelAnel                                             	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Qutbar Qdt -> Nel Anel, EW, matrix element squared, tree    	*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Antiup quark down quark annihilation into an electron and an antielectron-neutrino*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Qutbar Qdt -> Nel Anel, EW, matrix element squared, tree";
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


(* ::Text:: *)
(*Enable CKM mixing*)


$CKM=True;


(* ::Text:: *)
(*To avoid dealing with Goldstone bosons we do  the computation in the unitary gauge*)


InitializeModel[{SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}];


diags = InsertFields[CreateTopologies[0, 2 -> 2],
	{-F[3, {1}],F[4, {1}]} -> {F[2, {1}],-F[1,{1}]},
	InsertionLevel -> {Particles}, Model -> {SM, UnitarySM},
	GenericModel->{Lorentz, UnitaryLorentz}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags,GaugeRules->{FAGaugeXi[W|Z]->Infinity}], 
IncomingMomenta->{p2,p1}, OutgoingMomenta->{k1,k2},ChangeDimension->4,List->False, 
SMP->True, Contract->True, DropSumOver->True]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[]
SetMandelstam[s, t, u, p1, p2, -k1, -k2 , 0, 0, 0, 0];


(* ::Section:: *)
(*Square the amplitude*)


(* ::Text:: *)
(*We average over the spins and the colors of the quarks, hence the additional factor 1/3^2 1/2^2.*)


ampSquared[0] = 1/3^2*(amp[0] (ComplexConjugate[amp[0]]))//
	FermionSpinSum[#, ExtraFactor -> 1/2^2]&//DiracSimplify//
	FeynAmpDenominatorExplicit//SUNSimplify[#,SUNNToCACF->False]&//
	ReplaceAll[#,SUNN->3]&


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(u^2*SMP["e"]^4*SMP["V_ud", -I]*SMP["V_ud", I])/(12*(s - SMP["m_W"]^2)^2*SMP["sin_W"]^4)
};
FCCompareResults[{ampSquared[0]},
knownResults,
Text->{"\tCompare to CompHEP:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];



