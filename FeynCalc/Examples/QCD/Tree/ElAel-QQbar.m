(* ::Package:: *)

(* :Title: ElAel-QQbar                                              		*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:  El Ael -> Q Qbar, QCD, total cross section, tree    			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Quark-antiquark production in electron-positron annihilation*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El Ael -> Q Qbar, QCD, total cross section, tree";
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


MakeBoxes[p1,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(2\)]\)";
MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[2, {1}], -F[2, {1}]} ->
	{F[3, {1}], -F[3, {1}]}, InsertionLevel -> {Classes}, Model -> "SMQCD",
	ExcludeParticles -> {S[_],V[2]}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,
	List->False, SMP->True, Contract->True,DropSumOver->True,
	Prefactor->3/2 SMP["e_Q"],FinalSubstitutions->{SMP["m_u"]->SMP["m_q"]}]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_e"], SMP["m_e"],
	SMP["m_q"], SMP["m_q"]];


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//SUNSimplify[#,Explicit->True,
	SUNNToCACF->False]&//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
	DiracSimplify//
	TrickMandelstam[#,{s,t,u,2SMP["m_q"]^2+2SMP["m_e"]^2}]&//Simplify


ampSquaredMassless[0] = ampSquared[0]//ReplaceAll[#,{SMP["m_q"|"m_e"] -> 0}]&//
	TrickMandelstam[#,{s,t,u,0}]&


ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0]/.SUNN->3


(* ::Section:: *)
(*Total cross-section*)


(* ::Text:: *)
(*The differential cross-section d sigma/ d Omega is given by*)


prefac1=1/(64 Pi^2 s);


integral1=(Factor[ampSquaredMasslessSUNN3[0]/.{t->-s/2(1-Cos[Th]),u->-s/2(1+Cos[Th]),
SMP["e"]^4->(4 Pi SMP["alpha_fs"])^2}])


diffXSection1= prefac1 integral1


(* ::Text:: *)
(*The differential cross-section d sigma/ d t d phi is given by*)


prefac2=1/(128 Pi^2 s)


integral2=Simplify[ampSquaredMasslessSUNN3[0]/(s/4) /.{u-> -s-t,
SMP["e"]^4->(4 Pi SMP["alpha_fs"])^2}]


diffXSection2=prefac2 integral2


(* ::Text:: *)
(*The total cross-section. We see that integrating both expressions gives the same result*)


2 Pi Integrate[diffXSection1 Sin[Th],{Th,0,Pi}]


crossSectionTotal=2 Pi Integrate[diffXSection2,{t,-s,0}]


(* ::Text:: *)
(*Notice that up to the overall factor color factor 3 and the quark electric charge squared this result is identical to the total cross-section for the muon production in electron-positron annihilation.*)


crossSectionTotalQED=4*Pi*SMP["alpha_fs"]^2/3/s


(* ::Text:: *)
(*Taking the ratio of the two gives us the famous R-ration prediction of the parton mode, where the summation over the quark flavors in front of the charge squared is understood*)


crossSectionTotal/crossSectionTotalQED


quarkCharges={ eq[u|c|t]->2/3,eq[d|s|b]->-1/3};


(* ::Text:: *)
(*Depending on the available center of mass energy, we may not be able to produce all the existing*)
(*quark flavors. Below 3 GeV (roughly twice the mass of the charm quark) we have only up, down and strange quarks and the R-ratio is given by*)


Sum[3 eq[i]^2,{i,{u,d,s}}]/.quarkCharges


(* ::Text:: *)
(*At higher energies but below 9 GeV (roughly twice the mass of the bottom quark) we also have the *)
(*contribution from the charm quark*)


Sum[3 eq[i]^2,{i,{u,d,s,c}}]/.quarkCharges


(* ::Text:: *)
(*At even higher energies the bottom quark must also be taken into account*)


Sum[3 eq[i]^2,{i,{u,d,s,c,b}}]/.quarkCharges


(* ::Text:: *)
(*At some point we finally reach sufficiently high energies to produce the top quark*)


Sum[3 eq[i]^2,{i,{u,d,s,c,b,t}}]/.quarkCharges


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(6*(t^2 + u^2)*SMP["e"]^4*SMP["e_Q"]^2)/(s^2),
	(4*Pi*SMP["alpha_fs"]^2*SMP["e_Q"]^2)/s
};
FCCompareResults[{ampSquaredMasslessSUNN3[0],crossSectionTotal},
knownResults,
Text->{"\tCompare to CalcHEP and to Field, \
Applications of Perturbative QCD, Eq. 2.1.15:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
