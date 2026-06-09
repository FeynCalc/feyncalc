(* ::Package:: *)

(* :Title: GlGl-GlGl                                                      	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Gl Gl -> Gl Gl, QCD, matrix element squared, tree           	*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Gluon-gluon to gluon-gluon scattering*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Gl Gl -> Gl Gl, QCD, matrix element squared, tree";
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
LaunchKernels[8];

$ParallelizeFeynCalc = True; 
FCCheckVersion[10,2,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[k1,{SubscriptBox,k,1}]
FCAttachTypesettingRule[k2,{SubscriptBox,k,2}]
FCAttachTypesettingRule[k3,{SubscriptBox,k,3}]
FCAttachTypesettingRule[k4,{SubscriptBox,k,4}]


diags = InsertFields[CreateTopologies[0, 2 -> 2], {V[5], V[5]} ->
		{V[5], V[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];

Paint[diags, ColumnsXRows -> {4, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->128 {4, 1}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{q1,q2},UndoChiralSplittings->True,ChangeDimension->D,
	TransversePolarizationVectors->{p1,p2,q1,q2}, List->True, SMP->True,
	Contract->True,DropSumOver->True];


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -q1, -q2, 0, 0, 0, 0];


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0]=SquareAmplitude[amp[0],ComplexConjugate[amp[0]],Real->True];


AbsoluteTiming[ampSquared[1]=FeynAmpDenominatorExplicit[ampSquared[0],FCParallelize->True]//SUNSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[ampSquared[2]=ampSquared[1]//DoPolarizationSums[#,p1,p2,FCParallelize->True,ExtraFactor->1/2]&;]


AbsoluteTiming[ampSquared[3]=ampSquared[2]//DoPolarizationSums[#,p2,p1,FCParallelize->True,ExtraFactor->1/2]&;]


AbsoluteTiming[ampSquared[4]=ampSquared[3]//DoPolarizationSums[#,q1,q2,FCParallelize->True]&;]


AbsoluteTiming[ampSquared[5]=ampSquared[4]//DoPolarizationSums[#,q2,q1,FCParallelize->True]&;]


ampSquared[6]=SUNSimplify[1/((SUNN^2-1)^2) ampSquared[5],FCParallelize->True,SUNNToCACF->False]//TrickMandelstam[#,{s,t,u,0},FCParallelize->True]&//Total//Collect2[#,D,FCParallelize->True]&//
TrickMandelstam[#,{s,t,u,0}]&


ampSquaredSUNN3[0]=ampSquared[6]/.D->4/.SUNN->3


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(9/2)SMP["g_s"]^4 (3 - t u/s^2 - s u/t^2 - s t/u^2)
};
FCCompareResults[{ampSquaredSUNN3[0]},{knownResults},
Text->{"\tCompare to Ellis, Stirling and Weber, QCD and Collider Physics, \
Table 7.1:","CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic},Factoring->
Function[x,Simplify[TrickMandelstam[x,{s,t,u,0}]]]]
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];

