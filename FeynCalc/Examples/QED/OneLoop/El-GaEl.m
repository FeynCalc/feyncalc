(* ::Package:: *)

(* :Title: El-GaEl															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  El -> Ga El, QED, form factor, 1-loop							*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Electron's g-2 in QED*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El -> Ga El, QED, F2(0) form factor, 1-loop";
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
MakeBoxes[p1,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(2\)]\)";


diags = InsertFields[CreateTopologies[1, 1 -> 2,
		ExcludeTopologies->{Tadpoles, WFCorrections}], {F[2,{1}]} ->
		{V[1],F[2,{1}]}, InsertionLevel -> {Particles},
		ExcludeParticles->{S[_],V[2|3],(S|U)[_],F[3|4],F[2,{2|3}]}];

Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit. We need to replace e with -e to be compatible*)
(*with the convention D^mu = d^mu + ie A^mu*)


amp[0] = FCFAConvert[CreateFeynAmp[diags,PreFactor->1],
	IncomingMomenta->{p1}, OutgoingMomenta->{k,p2},
	LorentzIndexNames->{mu},
	LoopMomenta->{q}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions-> {SMP["e"]->-SMP["e"]}]/.
	k->p1-p2/.q->q+p1


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
ME=SMP["m_e"];
ScalarProduct[p1,p1]=ME^2;
ScalarProduct[p2,p2]=ME^2;
ScalarProduct[k,k]=0;
ScalarProduct[p1,p2]=ME^2;


(* ::Section:: *)
(*Calculate the amplitude*)


(* ::Text:: *)
(*Amputate the polarization vector.*)


amp[1] = amp[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//
	Contract//ReplaceAll[#, SMP["e"]^3-> 4 Pi SMP["e"] SMP["alpha_fs"]]&


amp[2] = TID[amp[1], q, ToPaVe->True]//DiracSimplify//
	Collect2[#,Spinor]&


(* ::Text:: *)
(*To extract F2 (0) we need to look only at the piece proportional to (p1+p2)^mu. Thus we can drop the g^mu -piece*)


amp[3] = amp[2]//ReplaceAll[#,FCI[GAD[mu]]:>0]&//DotSimplify


(* ::Text:: *)
(*The explicit values for the PaVe functions C1, C11 and C12 can be obtained e.g. from H. Patel's Package-X. Here we just insert the known results.*)


amp[4] = amp[3]/.{
	PaVe[1, {0, SMP["m_e"]^2, SMP["m_e"]^2}, {SMP["m_e"]^2, SMP["m_e"]^2, 0},OptionsPattern[]]->
		1/(32Pi^4 ME^2),
	PaVe[1, 1, {0, SMP["m_e"]^2, SMP["m_e"]^2}, {SMP["m_e"]^2, SMP["m_e"]^2, 0},OptionsPattern[]]->
		-(1/(96Pi^4 ME^2)),
	PaVe[1, 2, {SMP["m_e"]^2, 0, SMP["m_e"]^2}, {0, SMP["m_e"]^2, SMP["m_e"]^2},OptionsPattern[]]->
		-(1/(192Pi^4 ME^2))
}


(* ::Text:: *)
(*As expected, F2 (0) is free of any divergences. So we can safely do the limit D ->4*)


amp[5] = amp[4]//ChangeDimension[#,4]&//ReplaceAll[#,D->4]&


(* ::Text:: *)
(*We obtained $\frac{i e}{2 m_e} (p_1+p_2)^\mu F_2 (0) \bar{u}(p_2) u(p_1)$.*)
(*Dividing by the numerical prefactor and substituting $e^2 = 4\pi^2 \alpha$ yields F2(0)*)


f2[0]=(amp[5]/((I SMP["e"])/(2 ME)))//
	ReplaceAll [#,{Spinor[__] . Spinor[__]:>1,
	FCI[FV[p1,_]+FV[p2,_]]:>1}]&


(* ::Section:: *)
(*Check the final results*)


knownResult = AlphaFS/(2Pi);
FCCompareResults[f2[0],knownResult,
Text->{"\tCompare to J. Schwinger, Phys. Rev. 73, \
416-417, 1948:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



