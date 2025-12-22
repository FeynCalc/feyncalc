(* ::Package:: *)

(* :Title: Ga-GaGa															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Ga -> Ga Ga, QED, amplitude, 1-loop							*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*3-photon interaction in QED*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Ga -> Ga Ga, QED, amplitude, 1-loop";
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
FCAttachTypesettingRule[k1,{SubscriptBox,"k","1"}];
FCAttachTypesettingRule[k2,{SubscriptBox,"k","2"}];
FCAttachTypesettingRule[k3,{SubscriptBox,"k","3"}];


diags = InsertFields[CreateTopologies[1, 1 -> 2],
		{V[1]} -> {V[1],V[1]}, InsertionLevel -> {Particles},
		ExcludeParticles->{S[_],V[_],U[_],F[3|4],F[2,{2|3}]}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


amp[0] = FCFAConvert[CreateFeynAmp[diags,PreFactor->1,
	Truncated->True], IncomingMomenta->{k1},
	OutgoingMomenta->{k2, k3}, LoopMomenta->{q},
	LorentzIndexNames->{mu,nu,rho}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True]


(* ::Section:: *)
(*Evaluate the amplitudes*)


(* ::Text:: *)
(*Having performed the Dirac algebra we clearly see that this diagram must *)
(*vanish because the loop integral is antisymmetric under q^mu -> - q^mu.*)


amp[1] = DiracSimplify[amp[0],FCParallelize->True];


(* ::Section:: *)
(*Identify and minimize the topologies*)


{amp[2],topos}=FCLoopFindTopologies[amp[1],{q},FCParallelize->True];


mappings=FCLoopFindTopologyMappings[topos,FCParallelize->True];


(* ::Section:: *)
(*Rewrite the amplitude in terms of GLIs*)


AbsoluteTiming[ampReduced=FCLoopTensorReduce[amp[2],topos,FCParallelize->True];]


(* ::Text:: *)
(*The sum vanishes because the contribution of the first diagram cancels the contribution of the second diagram.*)


res=Collect2[ampReduced[[1]][[1]]+ampReduced[[2]][[1]],Pair]


(* ::Section:: *)
(*Check the final results*)


FCCompareResults[res,0,
Text->{"\tVerify Furry's theorem for 3-photons at 1-loop:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



