(* ::Package:: *)

(* :Title: ElAel-ElAel														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  El Ael -> El Ael, QED, matrix element squared, tree			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Bhabha scattering*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El Ael -> El Ael, QED, matrix element squared, tree";
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
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[p1,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(2\)]\)";
MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[2, {1}], -F[2, {1}]} ->
		{F[ 2, {1}], -F[2, {1}]}, InsertionLevel -> {Classes},
		Restrictions->QEDOnly];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,
	List->False, SMP->True, Contract->True]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2,
	SMP["m_e"], SMP["m_e"], SMP["m_e"], SMP["m_e"]];


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
	DiracSimplify//Simplify


ampSquaredMassless[0] = ampSquared[0]//ReplaceAll[#,{SMP["m_e"] -> 0}]&//
	Simplify


(* ::Section:: *)
(*Check the final results*)


knownResult = (2 SMP["e"]^4 (s^2+u^2)/t^2 +
4 SMP["e"]^4 u^2/(s t) + 2 SMP["e"]^4 (t^2+u^2)/s^2);
FCCompareResults[ampSquaredMassless[0],knownResult,
Text->{"\tCheck the final result:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
