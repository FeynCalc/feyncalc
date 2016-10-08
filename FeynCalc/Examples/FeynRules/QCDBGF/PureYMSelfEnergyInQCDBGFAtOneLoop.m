(* ::Package:: *)

(* :Title: PureYMSelfEnergyInQCDBGFAtOneLoop												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the self-energy diagrams in pure Yang Mills using
background field formalism at one loop.                                     *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the self-energy diagrams in pure Yang Mills using 
background field formalism at one loop."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;
FAPatch[PatchModelsOnly->True];


(* ::Section:: *)
(*Generate Feynman diagrams*)


topsYMSE=CreateTopologies[1, 1 -> 1,ExcludeTopologies -> {Tadpoles}];
diagsYMSE=InsertFields[topsYMSE, {V[50,{a}]} -> {V[50,{b}]},InsertionLevel -> {Classes},
Model -> FileNameJoin[{"QCDBGF","QCDBGF"}],GenericModel->FileNameJoin[{"QCDBGF","QCDBGF"}], 
ExcludeParticles->{F[_]}];
Paint[DiagramExtract[diagsYMSE,{3,4}], ColumnsXRows -> {2, 1}, Numbering -> None,SheetHeader->None,
ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampYMSE1=FCFAConvert[CreateFeynAmp[DiagramExtract[diagsYMSE,{3,4}], Truncated -> True,PreFactor->1],
IncomingMomenta->{k},OutgoingMomenta->{k},LoopMomenta->{l},UndoChiralSplittings->True,
DropSumOver->True,List->True,SMP->True,ChangeDimension->D]


ampYMSE2=TID[#,l,ToPaVe->True]&/@SUNSimplify/@Contract/@ampYMSE1


ampYMSE3=Factor2/@Normal/@(Series[#,{EpsilonUV,0,0}]&/@FCReplaceD[#,
D->4-2EpsilonUV]&/@(ampYMSE2/.B0[FCI[SPD[k]],0,0]->1/(16*EpsilonUV*Pi^4)))


(* ::Section:: *)
(*Check with the literature*)


resAbbott={
I CA SMP["g_s"]^2 SUNDelta[a,b]/(4Pi)^2(1/(3 EpsilonUV))(MTD[Lor1,Lor2]SPD[k]-FVD[k,Lor1]FVD[k,Lor2]),
I CA SMP["g_s"]^2 SUNDelta[a,b]/(4Pi)^2(10/(3 EpsilonUV))(MTD[Lor1,Lor2]SPD[k]-FVD[k,Lor1]FVD[k,Lor2])
};


Print["Check with Abbott, Nucl. Phys. B 185 (1981) 189-203, Eqs 5.11-5.12: ",
			If[Simplify[ampYMSE3-FCI[resAbbott]]==={0,0}, "CORRECT.", "!!! WRONG !!!"]];
