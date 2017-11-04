(* ::Package:: *)

(* :Title: Gl-Gl															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Gl -> Gl, QCD, only UV divergences, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*QCD vacuum polarization*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Gl -> Gl, QCD, only UV divergences, 1-loop";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Configure some options*)


(* ::Text:: *)
(*We keep scaleless B0 functions, since otherwise the UV part would not come out right.*)


$KeepLogDivergentScalelessIntegrals=True;


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[mu,TraditionalForm]:="\[Mu]";
MakeBoxes[nu,TraditionalForm]:="\[Nu]";


diags = InsertFields[CreateTopologies[1, 1 -> 1, ExcludeTopologies->{Tadpoles}],
		 {V[5]} -> {V[5]}, InsertionLevel -> {Particles}, Model -> "SMQCD",
		ExcludeParticles->{S[_],V[2|3],F[4],F[3,{2|3}]}];

Paint[diags, ColumnsXRows -> {2,2}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,512}];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


amp[0] = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, GaugeRules->{}, 
	PreFactor->1], IncomingMomenta->{p}, OutgoingMomenta->{p},LoopMomenta->{q},
	LorentzIndexNames->{mu,nu}, UndoChiralSplittings->True,
	ChangeDimension->D, List->True, SMP->True, DropSumOver->True,
	Contract->True,FinalSubstitutions->{SMP["m_u"]->SMP["m_q"]}]


(* ::Section:: *)
(*Calculate the amplitude*)


(* ::Subsection:: *)
(*The gluon tadpole *)


(* ::Text:: *)
(*This contribution is zero in dimensional regularization, because the loop integrals have no scale (and they are not log divergent)*)


amp1[0] = TID[amp[0][[1]], q, ToPaVe->True]


FCCompareResults[amp1[0],0,
Text->{"\tThe gluon tadpole vanishes:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];


(* ::Subsection:: *)
(*The quark loop*)


amp2[0] = amp[0][[2]]//SUNSimplify//TID[#, q, ToPaVe->True]&


(* ::Text:: *)
(*The contribution of the quark loop alone is  gauge invariant.*)


tmp=Contract[FVD[p,mu]FVD[p,nu] amp2[0]]//Factor
FCCompareResults[tmp,0,
Text->{"\tThe quark loop contribution is gauge invariant:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];


(* ::Subsection:: *)
(*The ghost loop*)


amp3[0] = amp[0][[3]]//SUNSimplify//TID[#, q, ToPaVe->True]&


(* ::Text:: *)
(*The contribution of the gluon loop alone is not gauge invariant.*)


tmp1=Contract[FVD[p,mu]FVD[p,nu] amp3[0]]//Factor


(* ::Subsection:: *)
(*The gluon loop*)


amp4[0] = amp[0][[4]]//SUNSimplify//TID[#, q, ToPaVe->True]&


(* ::Text:: *)
(*The contribution of the gluon loop alone is not gauge invariant. Notice, however, that the sum*)
(*of the ghost and gluon contributions is gauge invariant!*)


tmp2=Contract[FVD[p,mu]FVD[p,nu] amp4[0]]//Factor


FCCompareResults[tmp1+tmp2,0,
Text->{"\tThe sum of the ghost and gluon loop contributions is gauge invariant:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];


(* ::Subsection:: *)
(*Putting everything together*)


(* ::Text:: *)
(*When adding all the contributions together, we multiply the quark contribution by N_f to account for the 6 quark flavors that actually run in that loop. We ignore the fact that different flavors have different masses, since the divergent piece of the gluon self-energy will not depend on the quark mass.*)


amp[1]=Nf amp2[0]+amp3[0]+amp4[0]


(* ::Text:: *)
(*The UV divergence of the amplitude can be obtained via PaVeUVPart.*)
(*Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.*)
(*Hint: If you need the full result for the amplitude, use PaXEvaluate from FeynHelpers.*)


ampDiv[0]=PaVeUVPart[amp[1],Prefactor->1/(2Pi)^D]//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
SelectNotFree2[#,Epsilon]&//Simplify


(* ::Text:: *)
(*The self-energy amplitude is usually defined as  (p^2 g^{mu nu} - p^mu p^nu) i Pi(p^2)*)


pi[0]= FCI[ampDiv[0]/(I SUNDelta[SUNIndex[Glu1], SUNIndex[Glu2]]*
	(SPD[p,p]MTD[mu,nu]-FVD[p,mu]FVD[p,nu]))]//Cancel


(* ::Section:: *)
(*Check the final results*)


knownResult = -(SMP["g_s"]^2/(4Pi)^2)*(4/3*(1/2)*Nf-
(1/2)CA(13/3-GaugeXi[g]))*1/Epsilon;
FCCompareResults[pi[0],knownResult,
Text->{"\tCompare to Muta, Foundations of QCD, \
Eqs 2.5.131-2.5.132:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



