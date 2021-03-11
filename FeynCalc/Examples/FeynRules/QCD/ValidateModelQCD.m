(* ::Package:: *)

(* :Title: ValidateModelQCD										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Validates FeynArts model for QCD                     *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Validation of the FeynArts model for QCD."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,1];


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
InsertionLevel -> {Classes},Model -> FileNameJoin[{"QCD","QCD"}],
GenericModel->FileNameJoin[{"QCD","QCD"}]];


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
(*Check*)


Print["Check signs in the QCD model: ",
			If[{diff1,diff2,diff3,diff4}===Table[0,4], "CORRECT.", "!!! WRONG !!!"]];
