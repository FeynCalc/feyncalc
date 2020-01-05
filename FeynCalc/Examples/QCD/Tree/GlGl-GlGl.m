(* ::Package:: *)

(* :Title: GlGl-GlGl                                                      	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
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


MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";
MakeBoxes[k3,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(3\)]\)";
MakeBoxes[k4,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(4\)]\)";


diags = InsertFields[CreateTopologies[0, 2 -> 2], {V[5], V[5]} ->
		{V[5], V[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{k1,k2},
	OutgoingMomenta->{k3,k4},UndoChiralSplittings->True,ChangeDimension->4,
	TransversePolarizationVectors->{k1,k2,k3,k4}, List->True, SMP->True,
	Contract->True,DropSumOver->True]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, k1, k2, -k3, -k4, 0, 0, 0, 0];


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0] = 1/((SUNN^2-1)^2)(amp[1] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//SUNSimplify[#,Explicit->True,
	SUNNToCACF->False]&;


polsums[x_,vec_,aux_,spinfac_]:=x//Collect2[#,Pair[_,
Momentum[Polarization[vec,__]]]]&//Isolate[#,{Polarization[vec,__]}]&//
DoPolarizationSums[#,vec,aux,ExtraFactor->spinfac]&//FixedPoint[ReleaseHold,#]&


ClearAll[re];
Table[Print["    calculating color factors in products of the amplitudes ", i,
" and ", j," (CC), time = ",
Timing[re[i,j]=(amp[0][[i]]ComplexConjugate[amp[0]][[j]]//
FeynAmpDenominatorExplicit//
SUNSimplify[#,Explicit->True,SUNNToCACF->False]&)][[1]]];re[i,j],{i,4},{j,i}];


ClearAll[pre];
Table[Print["    calculating product of the amplitudes ", i, " and ", j,
" (CC), time = ", Timing[pre[i,j]=re[i,j]//polsums[#,k1,k2,
1/2]&//polsums[#,k2,k1,1/2]&//polsums[#,k3,k4,1]&//
polsums[#,k4,k3,1]&//Simplify][[1]]];pre[i,j],{i,4},{j,i}];


fpre[i_,j_]:=pre[i,j]/;(i>=j);
fpre[i_,j_]:=ComplexConjugate[pre[j,i]]/;(i<j);
ampSquared[0]=1/((SUNN^2-1)^2)(Sum[fpre[i,j],{i,1,4},{j,1,4}])//
TrickMandelstam[#,{s,t,u,0}]&//Simplify


ampSquaredSUNN3[0]=ampSquared[0]/.SUNN->3


ampSquaredMassless[0] = ampSquared[0]//ReplaceAll[#,{SMP["m_u"] -> 0}]&//
	TrickMandelstam[#,{s,t,u,0}]&


ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0]/.SUNN->3


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(9/2)SMP["g_s"]^4 (3 - t u/s^2 - s u/t^2 - s t/u^2)
};
FCCompareResults[{ampSquaredMasslessSUNN3[0]},{knownResults},
Text->{"\tCompare to Ellis, Stirling and Weber, QCD and Collider Physics, \
Table 7.1:","CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic},Factoring->
Function[x,Simplify[TrickMandelstam[x,{s,t,u,0}]]]]
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
