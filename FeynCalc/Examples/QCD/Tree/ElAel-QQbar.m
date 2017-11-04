(* ::Package:: *)

(* :Title: ElAel-QQbar                                                      *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)
	
(* :Summary:  El Ael -> Q Qbar, QCD, matrix element squared, tree             *)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Quark-antiquark production in electron-positron annihilation*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El Ael -> Q Qbar, QCD, matrix element squared, tree";
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
	{F[3, {1}], -F[3, {1}]}, InsertionLevel -> {Classes}, Model -> "SMQCD",
	ExcludeParticles -> {S[_],V[2]}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,
	List->False, SMP->True, Contract->True,DropSumOver->True, 
	Prefactor->3/2 SMP["e_Q"]]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_e"]^2, SMP["m_e"]^2, 
	SMP["m_u"]^2, SMP["m_u"]^2];


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//
	PropagatorDenominatorExplicit//SUNSimplify[#,Explicit->True,
	SUNNToCACF->False]&//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
	DiracSimplify[#,DiracTraceEvaluate->True]&//
	TrickMandelstam[#,{s,t,u,2SMP["m_u"]^2+2SMP["m_e"]^2}]&//Simplify


ampSquaredMassless[0] = ampSquared[0]//ReplaceAll[#,{SMP["m_u"|"m_e"] -> 0}]&//
	TrickMandelstam[#,{s,t,u,0}]&


ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0]/.SUNN->3


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(6*(t^2 + u^2)*SMP["e"]^4*SMP["e_Q"]^2)/(s^2)
};
FCCompareResults[{ampSquaredMasslessSUNN3[0]},{knownResults},
Text->{"\tCompare to CalcHEP:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];



