(* ::Package:: *)

(* :Title: ElAel-GaGa														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  El Ael -> Ga Ga, QED, matrix element squared, tree			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Pair annihilation*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El Ael -> Ga Ga, QED, matrix element squared, tree";
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


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[p1,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(2\)]\)";
MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[2, {1}], -F[2, {1}]} ->
		{V[1], V[1]}, InsertionLevel -> {Classes},
		Restrictions->QEDOnly];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,
	TransversePolarizationVectors->{k1,k2}, List->False, SMP->True,
	Contract->True]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_e"], SMP["m_e"], 0, 0];


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//
	PropagatorDenominatorExplicit//
	DoPolarizationSums[#,k1,0]&//DoPolarizationSums[#,k2,0]&//
	FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
	DiracSimplify[#,DiracTraceEvaluate->True]&//
	TrickMandelstam[#,{s,t,u,2SMP["m_e"]^2}]&//Simplify


(* ::Section:: *)
(*Check the final results*)


knownResult = (2SMP["e"]^4(SP[p1,k2]/SP[p1,k1]+SP[p1,k1]/SP[p1,k2]+
	2SMP["m_e"]^2 (1/SP[p1,k1]+1/SP[p1,k2])-
	SMP["m_e"]^4 (1/SP[p1,k1]+1/SP[p1,k2])^2));
FCCompareResults[ampSquared[0],knownResult,
Text->{"\tCompare to Peskin and Schroeder, An Introduction to QFT, \
Eq 5.105:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



