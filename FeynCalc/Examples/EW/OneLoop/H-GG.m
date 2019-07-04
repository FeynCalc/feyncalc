(* ::Package:: *)

(* :Title: H-GG																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary:  H -> G G, EW, total decay rate, 1-loop              			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Higgs decaying into two gluons*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="H -> G G, EW, total decay rate, 1-loop";
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
(*Here we consider only the dominant contribution from the top quark mass. However, it is trivial to include also loops from other quark flavors.*)


diags = InsertFields[CreateTopologies[1,1 -> 2,ExcludeTopologies->WFCorrections], 
	{S[1]} -> {V[5],V[5]}, InsertionLevel ->{Particles},Model->"SMQCD",
	ExcludeParticles->{F[3|4,{1|2}],F[4,{3}]}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitudes*)


amp[0]=FCFAConvert[CreateFeynAmp[diags,PreFactor->-1],IncomingMomenta->{pH},
	OutgoingMomenta->{k1,k2},LoopMomenta->{q},List->False,Contract->True,
	TransversePolarizationVectors->{k1,k2}, ChangeDimension->D,
	DropSumOver->True,SMP->True,UndoChiralSplittings->True];


amp[1]=amp[0]//FCTraceFactor//SUNSimplify


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
ScalarProduct[k1,k1]=0;
ScalarProduct[k2,k2]=0;
ScalarProduct[pH,pH]=SMP["m_H"]^2;
ScalarProduct[k1,k2]=(SMP["m_H"]^2)/2;


(* ::Section:: *)
(*Evaluate the amplitudes*)


(* ::Text:: *)
(*Dirac trace and tensor decomposition*)


amp[2]=amp[1]//DiracSimplify//TID[#,q,ToPaVe->True]&


(* ::Text:: *)
(*The explicit values for the PaVe functions B0 and  C0 can be obtained e.g. from H. Patel's Package-X. Here we just insert the known results.*)


loopInts={
	B0[SMP["m_H"]^2,SMP["m_t"]^2,SMP["m_t"]^2]->
		1/(16*Epsilon*Pi^4) - (-2*SMP["m_H"] + 
		EulerGamma*SMP["m_H"] - Log[4*Pi]*SMP["m_H"] -
		Log[ScaleMu^2/SMP["m_t"]^2]*SMP["m_H"] - 
		Log[(-SMP["m_H"]^2 + 2*SMP["m_t"]^2 + 
		SMP["m_H"]*Sqrt[SMP["m_H"]^2 - 4*SMP["m_t"]^2])/
		(2*SMP["m_t"]^2)]*Sqrt[SMP["m_H"]^2 - 
		4*SMP["m_t"]^2])/(16*Pi^4*SMP["m_H"]),
	C0[0,0,SMP["m_H"]^2,SMP["m_t"]^2,SMP["m_t"]^2,SMP["m_t"]^2]->
		Log[(-SMP["m_H"]^2 + 2*SMP["m_t"]^2 +
		SMP["m_H"]*Sqrt[SMP["m_H"]^2 - 4*SMP["m_t"]^2])/
		(2*SMP["m_t"]^2)]^2/(32*Pi^4*SMP["m_H"]^2)
};


$Assumptions={SMP["m_H"]>0,SMP["m_t"]>0};
amp[3]=(amp[2]/.loopInts)//FCReplaceD[#,D->4-2Epsilon]&//
	Series[#,{Epsilon,0,0}]&//Normal


(* ::Text:: *)
(*As expected, the result is finite (i.e. contains no 1/Epsilon poles), so that it is safe to switch back to 4 dimensions*)


amp[4]=amp[3]//ChangeDimension[#,4]&


(* ::Section:: *)
(*Square the amplitudes*)


ampSquared[0]=1/2(amp[4] (ComplexConjugate[amp[4]]))//
	SUNSimplify[#,SUNNToCACF->False]&//
	DoPolarizationSums[#,k1,k2]&//DoPolarizationSums[#,k2,k1]&//
	Simplify


(* ::Section:: *)
(*Total decay rate*)


phaseSpacePrefactor[m_]:=1/(16 Pi SMP["m_H"])Sqrt[1-4 m^2 / SMP["m_H"]^2]


totalDecayRate=phaseSpacePrefactor[0] ampSquared[0]//
ReplaceAll[#,{SMP["e"]^2->4 Pi SMP["alpha_fs"],
SMP["g_s"]^4->16 Pi^2 SMP["alpha_s"]^2}]&//Simplify


ISq=totalDecayRate/( SMP["alpha_s"]^2/(9 Pi^2) SMP["m_H"]^2/SMP["m_W"]^2 *
	SMP["alpha_fs"]SMP["m_H"]/(8 SMP["sin_W"]^2))


(* ::Text:: *)
(*ISq corresponds to I(m_H^2/m_q^2) from Peskin and Schroeder, Final Project 3, part (c). It should go to 1 for m_q -> Infinity and to 0 for m_q -> 0*)


limit1=Limit[ISq,SMP["m_t"]->Infinity]/.SUNN->3
limit2=Limit[ISq,SMP["m_t"]->0]


(* ::Section:: *)
(*Check the final results*)


knownResults = {1,0};
FCCompareResults[{limit1,limit2},
knownResults,Factoring->Simplify,
Text->{"\tCompare to Peskin and Schroeder,An Introduction to QFT, \
Final Project III, part (c):",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];



