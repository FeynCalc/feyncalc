(* ::Package:: *)

(* :Title: Ga-GaGa															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Ga -> Ga Ga Ga Ga, QED, amplitude, 1-loop						*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*5-photon interaction in QED*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Ga -> Ga Ga Ga Ga, QED, amplitude, 1-loop";
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

FCCheckVersion[9,3,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[mu,TraditionalForm]:="\[Mu]";
MakeBoxes[nu,TraditionalForm]:="\[Nu]";
MakeBoxes[rho,TraditionalForm]:="\[Rho]";
MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";
MakeBoxes[k3,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(3\)]\)";
MakeBoxes[k4,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(4\)]\)";
MakeBoxes[k5,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(5\)]\)";


diags = InsertFields[CreateTopologies[1, 1 -> 4],
		{V[1]} -> {V[1],V[1],V[1],V[1]}, InsertionLevel -> {Particles},
		ExcludeParticles->{S[_],V[_],U[_],F[3|4],F[2,{2|3}]}];

Paint[diags, ColumnsXRows -> {4, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


amp[0] = FCFAConvert[CreateFeynAmp[diags,PreFactor->1,
	Truncated->True], IncomingMomenta->{k1},
	OutgoingMomenta->{k2, k3, k4, k5}, LoopMomenta->{q},
	LorentzIndexNames->{mu,nu,rho}, UndoChiralSplittings->True,
	ChangeDimension->D, List->True, SMP->True]


(* ::Section:: *)
(*Calculate the amplitude*)


(* ::Text:: *)
(*We obtain 24 diagrams. The sum vanishes because the contribution of each odd diagram is exactly cancelled by the contribution of the next even diagram, i.e. A1+A2=0, A3+A4=0 and so on*)


amp[1] = amp[0]//FCTraceFactor;


amp[2]=Total/@Partition[amp[1],2,2,1,{}]


amp[3]=Total[amp[2]]


(* ::Section:: *)
(*Check the final results*)


FCCompareResults[amp[3],0,
Text->{"\tVerify Furry's theorem for 5-photons at 1-loop:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
