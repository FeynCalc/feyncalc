(* ::Package:: *)

(* :Title: QEDOnePhotonTadpoleOneLoop                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the 1-photon tadpole diagram in QED at 1-loop  *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*The 1-photon tadpole in QED*)


(* ::Subsection:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the 1-photon tadpole diagram in QED at 1-loop"];
];
$LoadFeynArts=True;
<< FeynCalc`
$FAVerbose=0;


(* ::Subsection:: *)
(*Generate Feynman diagrams*)


Paint[diags = InsertFields[CreateTopologies[1, 1 -> 0 ], {V[1]} -> {},
		InsertionLevel -> {Particles},ExcludeParticles->{S[_],V[_],F[3|4],
		U[_],F[2,{2}],F[2,{3}]}], ColumnsXRows -> {1, 1}, SheetHeader -> False, 
		Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


amp = FCFAConvert[CreateFeynAmp[diags, Truncated -> True,GaugeRules->{},PreFactor->1],
IncomingMomenta->{k},LoopMomenta->{q},UndoChiralSplittings->True,ChangeDimension->D,List->False,SMP->True];


(* ::Text:: *)
(*We obtain one tadpole diagram. Having performed the Dirac algebra we clearly see that this diagram must vanish because the loop integral is antisymmetric under q^mu -> - q^mu.*)


ampEval=DiracSimplify[amp,DiracTraceEvaluate->True]


(* ::Text:: *)
(*FeynCalc's tensor integral decomposition function TID can recognize this.*)


onePhotonTadpoleFinal = TID[ampEval,q]


Print["The 1-photon tadpole diagrams in QED vanish: ",
			If[Simplify[onePhotonTadpoleFinal]===0, "CORRECT.", "!!! WRONG !!!"]];
