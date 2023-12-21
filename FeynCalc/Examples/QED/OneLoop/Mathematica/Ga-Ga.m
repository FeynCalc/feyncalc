(* ::Package:: *)

(* :Title: Ga-Ga															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Ga -> Ga, QED, only UV divergences, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*QED vacuum polarization*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Ga -> Ga, QED, only UV divergences, 1-loop";
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
(*Configure some options*)


(* ::Text:: *)
(*We keep scaleless B0 functions, since otherwise the UV part would not come out right.*)


$KeepLogDivergentScalelessIntegrals=True;


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[mu,TraditionalForm]:="\[Mu]";
MakeBoxes[nu,TraditionalForm]:="\[Nu]";


diags = InsertFields[CreateTopologies[1, 1 -> 1], {V[1]} ->
		{V[1]}, InsertionLevel -> {Particles},
		ExcludeParticles->{S[_],V[2|3],(S|U)[_],F[3|4],F[2,{2|3}]}];

Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},LoopMomenta->{q},
	LorentzIndexNames->{mu,nu}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	Contract->True]



(* ::Section:: *)
(*Calculate the amplitude*)


amp[1] = TID[amp[0], q, ToPaVe->True]


(* ::Text:: *)
(*Check the gauge invariance*)


tmp=Contract[FVD[p,mu]FVD[p,nu] amp[1]]//Factor
FCCompareResults[tmp,0,
Text->{"\tThe photon self-energy is gauge invariant:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];


(* ::Text:: *)
(*The UV divergence of the amplitude can be obtained via PaVeUVPart.*)
(*Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.*)
(*Hint: If you need the full result for the amplitude, use PaXEvaluate from FeynHelpers.*)


ampDiv[0]=PaVeUVPart[amp[1],Prefactor->1/(2Pi)^D]//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
SelectNotFree2[#,Epsilon]&//Simplify


(* ::Text:: *)
(*The self-energy amplitude is usually defined as  (p^2 g^{mu nu} - p^mu p^nu) i Pi(p^2)*)


pi[0]= FCI[ampDiv[0]/(I(SPD[p,p]MTD[mu,nu]-FVD[p,mu]FVD[p,nu]))]//Cancel


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Keep in mind that Peskin and Schroeder use D = 4-Epsilon,*)
(*while we did the calculation with D = 4-2Epsilon.*)


knownResult = -SMP["e"]^2/(4Pi)^(D/2) Gamma[2-D/2]/
	(SMP["m_e"]^2- x(1-x)SPD[p,p])^(2-D/2)*(8x(1-x))//
	FCReplaceD[#,D->4-Epsilon]&//Series[#,{Epsilon,0,0}]&//
	Normal//SelectNotFree2[#,Epsilon]&//Integrate[#,{x,0,1}]&//
	ReplaceAll[#,1/Epsilon->1/(2Epsilon)]&;
FCCompareResults[pi[0],knownResult,
Text->{"\tCompare to Peskin and Schroeder, An Introduction to QFT, \
Eq 10.44:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
