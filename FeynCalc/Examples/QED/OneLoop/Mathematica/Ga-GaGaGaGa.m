(* ::Package:: *)

(* :Title: Ga-GaGa															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
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
LaunchKernels[4];
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;
$ParallelizeFeynCalc=True;
FCCheckVersion[10,2,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[mu,"\[Mu]"];
FCAttachTypesettingRule[nu,"\[Nu]"];
FCAttachTypesettingRule[rho,"\[Rho]"];
FCAttachTypesettingRule[si,"\[Sigma]"];
FCAttachTypesettingRule[tau,"\[Tau]"];
FCAttachTypesettingRule[k1,{SubscriptBox,"k","1"}];
FCAttachTypesettingRule[k2,{SubscriptBox,"k","2"}];
FCAttachTypesettingRule[k3,{SubscriptBox,"k","3"}];
FCAttachTypesettingRule[k4,{SubscriptBox,"k","4"}];
FCAttachTypesettingRule[k5,{SubscriptBox,"k","5"}];


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
	ChangeDimension->D, SMP->True];


(* ::Section:: *)
(*Evaluate the amplitudes*)


(* ::Text:: *)
(*We obtain 24 diagrams. The sum vanishes because the contribution of each odd diagram is exactly cancelled by the contribution of the next even diagram, i.e. A1+A2=0, A3+A4=0 and so on*)


amp[1] = FCTraceFactor[amp[0],FCParallelize->True];


res=amp[1]//Total


(* ::Section:: *)
(*Check the final results*)


FCCompareResults[res,0,
Text->{"\tVerify Furry's theorem for 5-photons at 1-loop:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



