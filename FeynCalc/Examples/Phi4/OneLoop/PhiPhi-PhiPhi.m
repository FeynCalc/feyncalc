(* ::Package:: *)

(* :Title: PhiPhi-PhiPhi													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  phi phi -> phi phi, phi^4, asymptotic limit, 1-loop			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*phi phi scattering at 1-loop*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom phi^4 model created with FeynRules. Please evaluate the file*)
(*FeynCalc/Examples/FeynRules/Phi4/GenerateModelPhi4.m before running it for the first time.*)


description="phi phi -> phi phi, phi^4, asymptotic limit, 1-loop";
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
(*Configure some options*)


FAPatch[PatchModelsOnly->True];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[p1,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(2\)]\)";
MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diags = InsertFields[CreateTopologies[1, 2 -> 2,
		ExcludeTopologies->{WFCorrections}],  
		{S[1],S[1]}-> {S[1],S[1]}, InsertionLevel -> {Classes}, 
		Model -> FileNameJoin[{"Phi4","Phi4"}]];
Paint[diags, ColumnsXRows -> {3, 1}, Numbering -> None,SheetHeader->None,
ImageSize->{512,256}];


diagsCT = InsertFields[CreateCTTopologies[1, 2 -> 2,
	ExcludeTopologies->{WFCorrectionCTs}],  {S[1],S[1]}-> {S[1],S[1]}, 
	InsertionLevel -> {Classes},  Model -> FileNameJoin[{"Phi4","Phi4"}]];
Paint[diagsCT, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->None,
ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


amp[0] = FCFAConvert[CreateFeynAmp[diags,PreFactor->1],
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{k1,k2},
	LoopMomenta->{q},ChangeDimension->D,List->False, 
	FinalSubstitutions->{Mphi->m}]


ampCT[0] = FCFAConvert[CreateFeynAmp[diagsCT,PreFactor->1],
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{k1,k2},
	LoopMomenta->{q},ChangeDimension->D,List->False, 
	FinalSubstitutions->{Mphi->m,Zg->1+SMP["d_g^MSbar"]}]


(* ::Section:: *)
(*Fix the kinematics*)


(* ::Text:: *)
(*For simplicity, let us consider the massless case*)


FCClearScalarProducts[]
SetMandelstam[s, t, u, p1, p2, -k1, -k2, 0, 0, 0, 0];


(* ::Section:: *)
(*Calculate the amplitude*)


amp[1]=amp[0]//ReplaceAll[#,m->0]&//ToPaVe[#,q]&


(* ::Text:: *)
(*The explicit value of the integral can be obtained from Package-X via the FeynHelpers add-on.*)


loopInt={
B0[s_,0,0]:>-(-2 + Log[4*Pi] - 
	Log[(-4*Pi*ScaleMu^2)/s])/(16*Pi^4) + SMP["Delta"]/(16*Pi^4)
};


amp[2]=(amp[1]/.loopInt)//Simplify


ampFull[0]=Expand[(amp[2]+ampCT[0])/.
	SMP["d_g^MSbar"] ->(3*g^2*SMP["Delta"])/(32*Pi^2)]


FCCompareResults[FreeQ[ampFull,SMP["Delta"]],True,
Text->{"\tThe UV divergence is cancelled by the counter-term:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];


(* ::Text:: *)
(*Now let us look at the asymptotic limit where  s goes to infinity and t is fixed*)


ampFullAsy[0]=Series[ampFull[0]/.u->-s-t,{s,Infinity,0}]//Normal


(* ::Text:: *)
(*The leading order behavior is governed by the log of s*)


ampFullAsy[1]=ampFullAsy[0]//PowerExpand//SelectNotFree2[#,s]&


(* ::Section:: *)
(*Check the final results*)


knownResult = ((-I/16)*g^2*Log[s])/Pi^2;
FCCompareResults[ampFullAsy[1],knownResult,
Text->{"\tCompare to Peskin and Schroeder, An Introduction to QFT, \
Ex 10.4:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



