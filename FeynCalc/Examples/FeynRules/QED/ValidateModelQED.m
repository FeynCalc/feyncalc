(* ::Package:: *)

(* :Title: ValidateModelQED												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Validates FeynArts model for QED                             *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Validation of the FeynArts model for QED"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Patch the generated models to work with FeynCalc*)


FAPatch[PatchModelsOnly->True];


(* ::Section:: *)
(*Compare vertices to FeynArts*)


top1To2 = CreateTopologies[0,1 -> 2];


compFu1To2[x_]:=FCFAConvert[FCPrepareFAAmp[CreateFeynAmp[x, Truncated -> True,PreFactor->1]],
IncomingMomenta->{p1},OutgoingMomenta->{p2,p3},UndoChiralSplittings->True,
DropSumOver->True,List->False,SMP->True]//Contract;


diagsFR[top_,in_,out_]:=InsertFields[top, in ->out,
InsertionLevel -> {Classes},Model -> FileNameJoin[{"QED","QED"}],
GenericModel->FileNameJoin[{"QED","QED"}]];


diagsFA[top_,in_,out_]:=InsertFields[top, in ->out,
InsertionLevel -> {Classes}, Model -> "SM", GenericModel->Lorentz];


(* ::Subsection:: *)
(*EEA-coupling*)


particles={{F[2, {1}]},{F[2, {1}],V[1]}};
diff1=compFu1To2[diagsFR[top1To2,Sequence@@particles]]-compFu1To2[diagsFA[top1To2,Sequence@@particles]]


(* ::Subsection:: *)
(*Check*)


Print["Check signs in the QED model: ",
			If[{diff1}===Table[0,1], "CORRECT.", "!!! WRONG !!!"]];
