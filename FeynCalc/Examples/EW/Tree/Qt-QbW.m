(* ::Package:: *)

(* :Title: Qt-QbW                                                  			*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Qt -> Qb W, EW, total decay rate, tree              			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Top quark decaying into a quark and a W boson*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Qt -> Qb W, EW, total decay rate, tree";
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


MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


(* ::Text:: *)
(*Enable CKM mixing*)


$CKM=True;


diags = InsertFields[CreateTopologies[0, 1 -> 2],
{F[3,{3}]} -> {F[4,{3}],-V[3]}, InsertionLevel -> {Particles}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p},
	OutgoingMomenta->{k1,k2},ChangeDimension->4,List->False, SMP->True,
	Contract->True,DropSumOver->True, FinalSubstitutions->
	{SMP["e"]->Sqrt[8/Sqrt[2]SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2]}]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[]
SP[p]=SMP["m_t"]^2;
SP[k1]=SMP["m_b"]^2;
SP[k2]=SMP["m_W"]^2;
SP[k1,k2]=Simplify[(SP[p]-SP[k1]-SP[k2])/2];
SP[p,k1]=Simplify[ExpandScalarProduct[SP[k1+k2,k1]]];
SP[p,k2]=Simplify[ExpandScalarProduct[SP[k1+k2,k2]]];


(* ::Section:: *)
(*Square the amplitude*)


(* ::Text:: *)
(*We average over the polarizations of the top quark, hence the additional factor 1/2*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//SUNSimplify//
	FermionSpinSum[#, ExtraFactor -> 1/2]&//DiracSimplify//
	DoPolarizationSums[#,k2]&//Simplify


(* ::Section:: *)
(*Total decay rate*)


phaseSpacePrefactor[m1_,m2_,M_]:=1/(16 Pi M)Sqrt[1-(m1+m2)^2/M^2]*
	Sqrt[1-(m1-m2)^2/M^2];


totalDecayRate=phaseSpacePrefactor[SMP["m_b"],SMP["m_W"],SMP["m_t"]]*
	ampSquared[0]//Simplify//ReplaceAll[#,Sqrt[x_]Sqrt[y_]:>
		Sqrt[ExpandAll[x y]]]&


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	SMP["m_t"]^3(CA*SMP["G_F"]*Sqrt[((SMP["m_b"] - SMP["m_t"] -
	SMP["m_W"])*(SMP["m_b"] + SMP["m_t"] - SMP["m_W"])*(SMP["m_b"] -
	SMP["m_t"] + SMP["m_W"])*(SMP["m_b"] + SMP["m_t"] + SMP["m_W"]))/
	SMP["m_t"]^4]*((1-SMP["m_b"]^2/SMP["m_t"]^2)^2+ SMP["m_W"]^2/
	SMP["m_t"]^2(1+SMP["m_b"]^2/SMP["m_t"]^2)-2SMP["m_W"]^4/SMP["m_t"]^4
	)*SMP["V_tb", -I]*SMP["V_tb", I])/(8*Sqrt[2]*Pi)
};
FCCompareResults[{totalDecayRate},
knownResults,
Text->{"\tCompare to Grozin, \
Using REDUCE in High Energy Physics, Chapter 5.2:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
