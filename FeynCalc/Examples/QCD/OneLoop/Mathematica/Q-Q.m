(* ::Package:: *)

(* :Title: Q-Q																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Q -> Q, QCD, only UV divergences, 1-loop						*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*QCD quark self-energy*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Q -> Q, QCD, only UV divergences, 1-loop";
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


diags = InsertFields[CreateTopologies[1, 1 -> 1,
		ExcludeTopologies->Tadpoles], {F[3,{1}]} ->
		{F[3,{1}]}, InsertionLevel -> {Particles}, Model -> "SMQCD",
		ExcludeParticles->{S[_],V[1|2|3]}];

Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit. We keep the full gauge dependence.*)


amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True,
	PreFactor->1, GaugeRules->{}], IncomingMomenta->{p},
	OutgoingMomenta->{p},LoopMomenta->{q}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True, DropSumOver->True,
	Contract->True,FinalSubstitutions->{SMP["m_u"]->SMP["m_q"]}]


(* ::Section:: *)
(*Calculate the amplitude*)


amp[1] = amp[0]//SUNSimplify//TID[#, q, ToPaVe->True]&


(* ::Text:: *)
(*The UV divergence of the amplitude can be obtained via PaVeUVPart.*)
(*Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.*)
(*Hint: If you need the full result for the amplitude, use PaXEvaluate from FeynHelpers.*)


ampDiv[0] = PaVeUVPart[amp[1],Prefactor->1/(2Pi)^D]//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
SelectNotFree2[#,Epsilon]&//Simplify


(* ::Text:: *)
(*The self-energy amplitude is usually defined as -i Sigma(p^2)*)


sigma[0] = I ampDiv[0]


sigmaFeynmanGauge[0] = sigma[0]/.GaugeXi[g]->1


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Notice that the result in the book must be multiplied by (-1) due to the way how self-energy is defined there (c.f. Eq. 2.4.4 and Eq. 2.4.6).*)


knownResult =-(-SMP["g_s"]^2/(4Pi)^2 CF*(3+GaugeXi[g])(1/Epsilon)*SMP["m_q"]+
		GSD[p]*SMP["g_s"]^2/(4Pi)^2*CF*GaugeXi[g]*(1/Epsilon))SDF[Col1,Col2];
FCCompareResults[sigma[0],knownResult,
Text->{"\tCompare to Muto, Foundations of QCD, \
Eq 10.41:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
