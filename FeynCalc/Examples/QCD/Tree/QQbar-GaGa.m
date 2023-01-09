(* ::Package:: *)

(* :Title: QQbar-GaGa                                                    	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Q Qbar -> Ga Ga, QCD, matrix element squared, tree          	*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Quark-antiquark pair annihilation into photons*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Q Qbar -> Ga Ga, QCD, matrix element squared, tree";
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
		{V[1], V[1]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,
	TransversePolarizationVectors->{k1,k2}, List->False, SMP->True,
	Contract->True,DropSumOver->True, Prefactor->9/4 SMP["e_Q"]^2]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_u"], SMP["m_u"], 0, 0];


(* ::Section:: *)
(*Square the amplitude*)


(* ::Text:: *)
(*We average over the spins and the colors of the quarks, hence the additional factor 1/N^2*1/2^2. *)
(*Since the final state particles are indistinguishable, we add an extra 1/2*)


ampSquared[0] = 1/2*1/(SUNN^2)(amp[0] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//SUNSimplify[#,Explicit->True,
	SUNNToCACF->False]&//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
	DiracSimplify//DoPolarizationSums[#,k1,k2]&//
	DoPolarizationSums[#,k2,k1]&//TrickMandelstam[#,{s,t,u,2  SMP["m_u"]^2}]&//
	Simplify


ampSquaredMassless[0] = ampSquared[0]//ReplaceAll[#,{SMP["m_u"] -> 0}]&//
	TrickMandelstam[#,{s,t,u,0}]&


ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0]/.SUNN->3


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	((t^2 + u^2)*SMP["e"]^4*SMP["e_Q"]^4)/(3*t*u)
};
FCCompareResults[{ampSquaredMasslessSUNN3[0]},{knownResults},
Text->{"\tCompare to CalcHEP:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
