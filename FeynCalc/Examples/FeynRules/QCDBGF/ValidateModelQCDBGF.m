(* ::Package:: *)

(* :Title: ValidateModelQCDBGF												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Validates FeynArts model for QCD in background field
				formalism                                                   *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Validation of the FeynArts model for QCD in background field formalism"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,0];


(* ::Section:: *)
(*Patch the generated models to work with FeynCalc*)


FAPatch[PatchModelsOnly->True];


(* ::Section:: *)
(*Compare vertices to FeynArts*)


top1To2 = CreateTopologies[0,1 -> 2];
top2To2 = CreateTopologies[0,2 -> 2];


compFu1To2[x_]:=FCFAConvert[CreateFeynAmp[x, Truncated -> True,PreFactor->1],
IncomingMomenta->{p1},OutgoingMomenta->{p2,p3},UndoChiralSplittings->True,
DropSumOver->True,List->False,SMP->True]//Contract;


compFu2To2[x_]:=FCFAConvert[CreateFeynAmp[x, Truncated -> True,PreFactor->1],
IncomingMomenta->{p1,p2},OutgoingMomenta->{p3,p4},UndoChiralSplittings->True,
DropSumOver->True,List->False,SMP->True]//Contract;


diagsFR[top_,in_,out_]:=InsertFields[top, in ->out,
InsertionLevel -> {Classes},Model -> FileNameJoin[{"QCDBGF","QCDBGF"}],
GenericModel->FileNameJoin[{"QCDBGF","QCDBGF"}]];


diagsFA[top_,in_,out_]:=InsertFields[top, in ->out,
InsertionLevel -> {Classes}, Model -> "SMQCD", GenericModel->Lorentz];


(* ::Subsection:: *)
(*QQG-coupling*)


particles={{F[3, {1}]},{F[3, {1}],V[5]}};
diff1=compFu1To2[diagsFR[top1To2,Sequence@@particles]]-compFu1To2[diagsFA[top1To2,Sequence@@particles]]


(* ::Subsection:: *)
(*GhGhG-coupling*)


particles={{U[5]},{U[5],V[5]}};
diff2=compFu1To2[diagsFR[top1To2,Sequence@@particles]]-compFu1To2[diagsFA[top1To2,Sequence@@particles]]


(* ::Subsection:: *)
(*GGG-coupling*)


particles={{V[5]},{V[5],V[5]}};
diff3=ExpandScalarProduct[compFu1To2[diagsFR[top1To2,Sequence@@particles]]-compFu1To2[diagsFA[top1To2,Sequence@@particles]]]//Simplify


(* ::Subsection:: *)
(*GGGG-coupling*)


particles={{V[5],V[5]},{V[5],V[5]}};
diff4=ExpandScalarProduct[compFu2To2[DiagramExtract[diagsFR[top2To2,Sequence@@particles],1]]-
compFu2To2[DiagramExtract[diagsFA[top2To2,Sequence@@particles],1]]]//FCCanonicalizeDummyIndices//Simplify


(* ::Subsection:: *)
(*GGGB-coupling*)


diff5=ExpandScalarProduct[compFu2To2[DiagramExtract[diagsFR[top2To2,{V[50],V[5]},{V[5],V[5]}],1]]-
compFu2To2[DiagramExtract[diagsFA[top2To2,{V[5],V[5]},{V[5],V[5]}],1]]]//FCCanonicalizeDummyIndices//Simplify


(* ::Subsection:: *)
(*GGB-Coupling*)


ggbDiag=InsertFields[top1To2, {V[50,{a}]} ->{V[5,{b}],V[5,{c}]},
InsertionLevel -> {Classes},Model -> FileNameJoin[{"QCDBGF","QCDBGF"}],
GenericModel->FileNameJoin[{"QCDBGF","QCDBGF"}]];
ggbAbbott=-(((FV[q, nu] + FV[-p + r, nu]*GaugeXi[G])*MT[la, mu]*SMP["g_s"]*SUNF[a, b, c])/GaugeXi[G]) + FV[-q + r, mu]*MT[la, nu]*SMP["g_s"]*SUNF[a, b, c] +
((FV[r, la] + FV[-p + q, la]*GaugeXi[G])*MT[mu, nu]*SMP["g_s"]*SUNF[a, b, c])/GaugeXi[G];
ggbVertexFR=FCFAConvert[CreateFeynAmp[ggbDiag, Truncated -> True,PreFactor->-1,GaugeRules->{}],
IncomingMomenta->{-p},OutgoingMomenta->{q,r},UndoChiralSplittings->True,
DropSumOver->True,List->False,SMP->True,LorentzIndexNames->{mu,nu,la}]//FCE//Collect2[#,MT,Factoring->FullSimplify]&//MomentumCombine
diff6=FCI[(ggbAbbott-ggbVertexFR)]


(* ::Subsection:: *)
(*GhGhB-coupling*)


ghghbDiag=InsertFields[top1To2, {U[5,{a}]} ->{U[5,{b}],V[50,{c}]},
InsertionLevel -> {Classes},Model -> FileNameJoin[{"QCDBGF","QCDBGF"}],
GenericModel->FileNameJoin[{"QCDBGF","QCDBGF"}]];
ghghbFR=FCFAConvert[CreateFeynAmp[ghghbDiag, Truncated -> True,PreFactor->-1,GaugeRules->{}],
IncomingMomenta->{-p},OutgoingMomenta->{-q,r},UndoChiralSplittings->True,
DropSumOver->True,List->False,SMP->True,LorentzIndexNames->{mu,nu,la}]//FCE//FCI//MomentumCombine
ghghbAbbott=FV[p + q, mu]*SMP["g_s"]*SUNF[a, b, c];
diff7=FCI[(ghghbAbbott-ghghbFR)]


(* ::Subsection:: *)
(*GGBB-Coupling*)


ggbbDiag=InsertFields[top2To2, {V[50,{a}],V[5,{d}]} ->{V[5,{b}],V[50,{c}]},
InsertionLevel -> {Classes},Model -> FileNameJoin[{"QCDBGF","QCDBGF"}],
GenericModel->FileNameJoin[{"QCDBGF","QCDBGF"}]];
ggbbAbbott=((-I)*(MT[la, rho]*MT[mu, nu] + GaugeXi[G]*(-(MT[la, nu]*MT[mu, rho]) + MT[la, mu]*MT[nu, rho]))*SMP["g_s"]^2*SUNF[a, d, x]*SUNF[b, c, x])/GaugeXi[G] +
I*(-(MT[la, rho]*MT[mu, nu]) + MT[la, nu]*MT[mu, rho])*SMP["g_s"]^2*SUNF[a, c, x]*SUNF[b, d, x] +
(I*(MT[la, nu]*MT[mu, rho] + GaugeXi[G]*(-(MT[la, rho]*MT[mu, nu]) + MT[la, mu]*MT[nu, rho]))*SMP["g_s"]^2*SUNF[a, b, x]*SUNF[c, d, x])/GaugeXi[G];
ggbbVertexFR=FCFAConvert[CreateFeynAmp[DiagramExtract[ggbbDiag,{1}], Truncated -> True,PreFactor->-1,GaugeRules->{}],
IncomingMomenta->{-p},OutgoingMomenta->{q,r},UndoChiralSplittings->True,
DropSumOver->True,List->False,SMP->True,LorentzIndexNames->{mu,nu,rho,la}]//FCE//FCCanonicalizeDummyIndices[#,SUNIndexNames->{x}]&//
Collect2[#,SUNF,Factoring->FullSimplify]&
diff8=FCI[(ggbbAbbott-ggbbVertexFR)]//FCCanonicalizeDummyIndices


(* ::Subsection:: *)
(*GhGhBG-Coupling*)


ghghbgDiag=InsertFields[top2To2, {U[5,{b}],V[50,{c}]} ->{U[5,{a}],V[5,{d}]},
InsertionLevel -> {Classes},Model -> FileNameJoin[{"QCDBGF","QCDBGF"}],
GenericModel->FileNameJoin[{"QCDBGF","QCDBGF"}]];
ghghbgAbbott=(-I)*MT[mu, nu]*SMP["g_s"]^2*SUNF[a, c, x]*SUNF[b, d, x];
ghghbgVertexFR=FCFAConvert[CreateFeynAmp[DiagramExtract[ghghbgDiag,{1}], Truncated -> True,PreFactor->-1,GaugeRules->{}],
IncomingMomenta->{-p},OutgoingMomenta->{q,r},UndoChiralSplittings->True,
DropSumOver->True,List->False,SMP->True,LorentzIndexNames->{mu,nu,rho,la}]//FCE//FCCanonicalizeDummyIndices[#,SUNIndexNames->{x}]&//
Collect2[#,SUNF,Factoring->FullSimplify]&
diff9=FCI[(ghghbgAbbott-ghghbgVertexFR)]


(* ::Subsection:: *)
(*GhGhBB-Coupling*)


ghghbbDiag=InsertFields[top2To2, {U[5,{b}],V[50,{c}]} ->{U[5,{a}],V[50,{d}]},
InsertionLevel -> {Classes},Model -> FileNameJoin[{"QCDBGF","QCDBGF"}],
GenericModel->FileNameJoin[{"QCDBGF","QCDBGF"}]];
ghghbbAbbott=(-I)*MT[mu, nu]*SMP["g_s"]^2*SUNF[a, d, x]*SUNF[b, c, x] - I*MT[mu, nu]*SMP["g_s"]^2*SUNF[a, c, x]*SUNF[b, d, x];
ghghbbVertexFR=FCFAConvert[CreateFeynAmp[DiagramExtract[ghghbbDiag,{1}], Truncated -> True,PreFactor->-1,GaugeRules->{}],
IncomingMomenta->{-p},OutgoingMomenta->{q,r},UndoChiralSplittings->True,
DropSumOver->True,List->False,SMP->True,LorentzIndexNames->{mu,nu,rho,la}]//FCE//FCCanonicalizeDummyIndices[#,SUNIndexNames->{x}]&//
Collect2[#,SUNF,Factoring->FullSimplify]&
diff10=FCI[(ghghbbAbbott-ghghbbVertexFR)]


(* ::Subsection:: *)
(*Check*)


Print["Check signs in the QCDBGF model: ",
			If[{diff1,diff2,diff3,diff4,diff5,diff6,diff7,diff8,diff9,diff10}===Table[0,10], "CORRECT.", "!!! WRONG !!!"]];
