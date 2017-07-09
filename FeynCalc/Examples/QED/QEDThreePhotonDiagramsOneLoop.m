(* ::Package:: *)

(* :Title: QEDThreePhotonDiagramsOneLoop                                    *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the 3-photon diagrams in QED at 1-loop         *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*The 3-photon diagrams in QED at 1-loop*)


(* ::Subsection:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the 3-photon diagrams in QED at 1-loop"];
];
$LoadFeynArts=True;
<< FeynCalc`
$FAVerbose=0;


(* ::Subsection:: *)
(*Generate Feynman diagrams*)


Paint[diags = InsertFields[CreateTopologies[1, 1 -> 2 ], {V[1]} -> {V[1],V[1]},
		InsertionLevel -> {Particles},ExcludeParticles->{S[_],V[2|3],F[3|4],
		U[_],F[2,{2}],F[2,{3}]}], ColumnsXRows -> {2, 1},
		SheetHeader -> False,  Numbering -> None,ImageSize->{512,256}];


amps = FCFAConvert[CreateFeynAmp[diags, Truncated -> True,GaugeRules->{},PreFactor->1/((2^D)*(Pi)^(D/2))],
IncomingMomenta->{k1},OutgoingMomenta->{k2,k3},LoopMomenta->{q},UndoChiralSplittings->True,ChangeDimension->D,List->False,SMP->True];


(* ::Text:: *)
(*We obtain two triangle diagrams. The sum vanishes because the contribution of the first diagram cancels the contribution of*)
(*the second diagram.*)


threePhotonFinal=DiracSimplify[amps,DiracTraceEvaluate->True]


Print["The 3-photon diagrams in QED vanish: ",
			If[Simplify[threePhotonFinal]===0, "CORRECT.", "!!! WRONG !!!"]];
