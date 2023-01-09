(* ::Package:: *)

(* :Title: Ga																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Ga, QED, amplitude, 1-loop									*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Photon tadpole in QED*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Ga, QED, amplitude, 1-loop";
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
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[mu,TraditionalForm]:="\[Mu]";


diags = InsertFields[CreateTopologies[1, 1 -> 0],
		{V[1]} -> {}, InsertionLevel -> {Particles},
		ExcludeParticles->{S[_],V[_],U[_],F[3|4],F[2,{2|3}]}];

Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


amp[0] = FCFAConvert[CreateFeynAmp[diags,PreFactor->1,
	Truncated->True], IncomingMomenta->{k},
	LorentzIndexNames->{mu}, LoopMomenta->{q},
	UndoChiralSplittings->True, ChangeDimension->D,
	List->False, SMP->True]


(* ::Section:: *)
(*Calculate the amplitude*)


(* ::Text:: *)
(*Having performed the Dirac algebra we clearly see that this diagram must *)
(*vanish because the loop integral is antisymmetric under q^mu -> - q^mu.*)


amp[1] = DiracSimplify[amp[0]]


(* ::Text:: *)
(*TID can recognize this and we obtain zero*)


amp[2] = TID[amp[1], q]


(* ::Section:: *)
(*Check the final results*)


FCCompareResults[amp[2],0,
Text->{"\tVerify Furry's theorem for 1-photon at 1-loop:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
