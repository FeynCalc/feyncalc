(* ::Package:: *)

(* :Title: QQbar-MuAmu                                              		*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Q Qbar -> Mu Amu, QCD, total cross section, tree    			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Quark-antiquark annihilation into a muon-antimuon pair*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Q Qbar -> Mu Amu, QCD, total cross section, tree";
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


diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}], -F[3, {1}]} ->
	{F[2, {2}], -F[2, {2}]}, InsertionLevel -> {Classes}, Model -> "SMQCD",
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
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_u"], SMP["m_u"],
	SMP["m_mu"], SMP["m_mu"]];


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0] = 1/(SUNN^2)(amp[0] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//SUNSimplify[#,Explicit->True,
	SUNNToCACF->False]&//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
	DiracSimplify//TrickMandelstam[#,{s,t,u,2SMP["m_u"]^2+2SMP["m_mu"]^2}]&//
	Simplify


ampSquaredMassless[0] = ampSquared[0]//ReplaceAll[#,{SMP["m_u"|"m_mu"] -> 0}]&//
	TrickMandelstam[#,{s,t,u,0}]&


ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0]/.SUNN->3


(* ::Section:: *)
(*Total cross-section*)


integral=Integrate[Simplify[ampSquaredMasslessSUNN3[0]/(s/4) /.
u-> -s-t],{t,-s,0}]/.SMP["e"]^4->(4 Pi SMP["alpha_fs"])^2


prefac=2Pi/(128 Pi^2 s)


(* ::Text:: *)
(*The total cross-section *)


crossSectionTotal=integral*prefac//PowerExpand//Factor2


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(2*(t^2 + u^2)*SMP["e"]^4*SMP["e_Q"]^2)/(3*s^2),
	(4*Pi*SMP["alpha_fs"]^2*SMP["e_Q"]^2)/(9*s)
};
FCCompareResults[{ampSquaredMasslessSUNN3[0],crossSectionTotal},
knownResults,
Text->{"\tCompare to CalcHEP  and to Field, \
Applications of Perturbative QCD, Eq. 5.1.17:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
