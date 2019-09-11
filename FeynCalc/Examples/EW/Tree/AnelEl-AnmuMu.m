(* ::Package:: *)

(* :Title: AnEl-AnmuMu                                                      *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary:  Anel El -> Anmu Mu, EW, total cross section, tree            	*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Electron electron-antineutrino annihilation into a muon-antineutrino and a muon*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Anel El -> Anmu Mu, EW, total cross section, tree";
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


MakeBoxes[q1,TraditionalForm]:="\!\(\*SubscriptBox[\(q\), \(1\)]\)";
MakeBoxes[q2,TraditionalForm]:="\!\(\*SubscriptBox[\(q\), \(2\)]\)";
MakeBoxes[pe,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(e\)]\)";
MakeBoxes[pm,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(m\)]\)";


(* ::Text:: *)
(*To avoid dealing with Goldstone bosons we do  the computation in the unitary gauge*)


InitializeModel[{SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}];


diags = InsertFields[CreateTopologies[0, 2 -> 2], {-F[1, {1}],
	F[2, {1}]} -> {-F[1,{2}],F[2,{2}]}, InsertionLevel -> {Classes},
	Model -> {SM, UnitarySM},GenericModel->{Lorentz, UnitaryLorentz}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{q1,pe},
	OutgoingMomenta->{q2,pm},ChangeDimension->4,List->False, SMP->True,
	Contract->True,DropSumOver->True,  FinalSubstitutions->{SMP["e"]->Sqrt[8/Sqrt[2]*
	SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2]}]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, q1, pe, -q2, -pm, 0, SMP["m_e"], 0, SMP["m_mu"]];


(* ::Section:: *)
(*Square the amplitude*)


(* ::Text:: *)
(*There is no polarization averaging for neutrinos here, as right handed neutrinos do not interact*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//
	FermionSpinSum[#, ExtraFactor -> 1/2]&//DiracSimplify


(* ::Text:: *)
(*In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a very good approximation at low energies, as then (pm-q2)^2  <= m_mu^2 << m_W^2.*)


ampSquared[1]=ampSquared[0]//FCE//ReplaceAll[#,{pm+q2->0}]&//
	FeynAmpDenominatorExplicit//Factor2


(* ::Section:: *)
(*Total cross section*)


(* ::Text:: *)
(*We need to carry out the angular integration, so let us specify the values of the temporal and spatial components of the 4-vectors*)


TC[q2]=(s-SMP["m_mu"]^2)/(2Sqrt[s]);
TC[pe]=(s+SMP["m_e"]^2)/(2Sqrt[s]);
CSP[pe]=(s-SMP["m_e"]^2)^2/(4s);
CSP[pm]=(s-SMP["m_mu"]^2)^2/(4s);
CSP[q2]=(s-SMP["m_mu"]^2)^2/(4s);


prefac=2Pi/(64 Pi^2 s) Sqrt[(s-SMP["m_mu"]^2)^2]/Sqrt[(s-SMP["m_e"]^2)^2]


integral=Integrate[Simplify[ampSquared[1]/.u-> SMP["m_e"]^2-
	2(TC[q2]TC[pe]-Sqrt[CSP[q2]]Sqrt[CSP[pe]]x)],{x,-1,1}]


(* ::Text:: *)
(*The total cross-section *)


crossSectionTotal=integral*prefac//PowerExpand//Factor2


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(SMP["G_F"]^2*(s - SMP["m_mu"]^2)^2)(s^2+(SMP["m_mu"]^2+SMP["m_e"]^2)/2 s+
	SMP["m_mu"]^2 SMP["m_e"]^2)/(3Pi s^3)
};
FCCompareResults[{crossSectionTotal},
knownResults,
Text->{"\tCompare to Grozin, \
Using REDUCE in High Energy Physics, Chapter 5.3:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
