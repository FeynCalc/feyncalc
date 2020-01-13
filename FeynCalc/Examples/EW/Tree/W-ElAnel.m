(* ::Package:: *)

(* :Title: W-ElAnel                                                  		*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  W -> El Anel, EW, total decay rate, tree              		*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*W decaying into an electron and an electron-antineutrino*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="W -> El Anel, EW, total decay rate, tree";
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

FCCheckVersion[9,3,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diags = InsertFields[CreateTopologies[0, 1 -> 2],
	{V[3]} -> {F[2,{1}],-F[1,{1}]}, InsertionLevel -> {Particles}];

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
SP[p]=SMP["m_W"]^2;
SP[k1]=SMP["m_e"]^2;
SP[k2]=0;
SP[k1,k2]=(SMP["m_W"]^2-SMP["m_e"]^2)/2;
SP[p,k1]=(SMP["m_W"]^2+SMP["m_e"]^2)/2;
SP[p,k2]=(SMP["m_W"]^2-SMP["m_e"]^2)/2;


(* ::Section:: *)
(*Square the amplitude*)


(* ::Text:: *)
(*We average over the polarizations of the W-boson, hence the additional factor 1/3*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//SUNSimplify//
	FermionSpinSum//DiracSimplify//
	DoPolarizationSums[#,p,ExtraFactor -> 1/3]&//Simplify


(* ::Section:: *)
(*Total decay rate*)


phaseSpacePrefactor[m1_,m2_,M_]:=1/(16 Pi M)Sqrt[1-(m1+m2)^2/M^2]*
	Sqrt[1-(m1-m2)^2/M^2];


totalDecayRate=phaseSpacePrefactor[SMP["m_e"],0,SMP["m_W"]]*
	ampSquared[0]//Simplify


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(SMP["G_F"](SMP["m_e"] - SMP["m_W"])^2(SMP["m_e"] +
	SMP["m_W"])^2(SMP["m_e"]^2 + 2*SMP["m_W"]^2))/
	(12*Sqrt[2]*Pi*SMP["m_W"]^3)
};
FCCompareResults[{totalDecayRate},
knownResults,
Text->{"\tCompare to Grozin, \
Using REDUCE in High Energy Physics, Chapter 5.2:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
