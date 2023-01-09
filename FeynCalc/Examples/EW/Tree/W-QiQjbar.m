(* ::Package:: *)

(* :Title: W-QiQjbar                                                  		*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  W -> Qi Qjbar, EW, total decay rate, tree              		*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*W decaying into a quark and an antiquark of different flavors*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="W -> Qi Qjbar, EW, total decay rate, tree";
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
(*Enable CKM mixing*)


$CKM=True;


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diags = InsertFields[CreateTopologies[0, 1 -> 2],
	{V[3]} -> {-F[3,{1}],F[4,{1}]}, InsertionLevel -> {Particles}];
	
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p},
	OutgoingMomenta->{k1,k2},ChangeDimension->4,List->False, SMP->True,
	TransversePolarizationVectors->{p},
	Contract->True,DropSumOver->True, FinalSubstitutions->
	{SMP["e"]->Sqrt[8/Sqrt[2]SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2]}]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[]
SP[p]=SMP["m_W"]^2;
SP[k1]=SMP["m_u"]^2;
SP[k2]=SMP["m_d"]^2;
SP[k1,k2]=(SMP["m_W"]^2-SMP["m_u"]^2-SMP["m_d"]^2)/2;
SP[p,k1]=Simplify[ExpandScalarProduct[SP[k1+k2,k1]]];
SP[p,k2]=Simplify[ExpandScalarProduct[SP[k1+k2,k2]]];


(* ::Section:: *)
(*Square the amplitude*)


(* ::Text:: *)
(*We average over the polarizations of the W-boson, hence the additional factor 1/3.*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//SUNSimplify//
	FermionSpinSum//DiracSimplify//
	DoPolarizationSums[#,p,ExtraFactor -> 1/3]&//Simplify


(* ::Section:: *)
(*Total decay rate*)


phaseSpacePrefactor[m1_,m2_,M_]:=1/(16 Pi M)Sqrt[1-(m1+m2)^2/M^2]*
	Sqrt[1-(m1-m2)^2/M^2];


totalDecayRate=phaseSpacePrefactor[SMP["m_u"], SMP["m_d"], SMP["m_W"]]*
	ampSquared[0]//Simplify//
	ReplaceAll[#,Sqrt[x_]Sqrt[y_]:>Sqrt[ExpandAll[x y]]]&//Factor2


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	SMP["m_W"]^3(CA*SMP["G_F"]*Sqrt[((SMP["m_d"] - SMP["m_u"] - SMP["m_W"])*(SMP["m_d"] + SMP["m_u"] - SMP["m_W"])*
	(SMP["m_d"] - SMP["m_u"] + SMP["m_W"])*(SMP["m_d"] + SMP["m_u"] + SMP["m_W"]))/SMP["m_W"]^4]*
(1-(SMP["m_u"]^2+SMP["m_d"]^2)/(2SMP["m_W"]^2)-(SMP["m_u"]^2-SMP["m_d"]^2)^2/(2 SMP["m_W"]^4))*SMP["V_ud", -I]*SMP["V_ud", I])/
(6*Sqrt[2]*Pi)
};
FCCompareResults[{totalDecayRate},
knownResults,
Text->{"\tCompare to Grozin, \
Using REDUCE in High Energy Physics, Chapter 5.2:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];






