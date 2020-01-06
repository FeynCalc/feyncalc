(* ::Package:: *)

(* :Title: ElAel-MuAmu														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  El Ael -> Mu Amu, QED, total cross section, tree				*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Muon production*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El Ael -> Mu Amu, QED, total cross section, tree";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"FeynArtsLoader"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[p1,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(2\)]\)";
MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[2, {1}], -F[2, {1}]} ->
		{F[2,{2}], -F[2, {2}]}, InsertionLevel -> {Classes},
		Restrictions->QEDOnly];

Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,
	List->False, SMP->True, Contract->True]


(* ::Text:: *)
(*Polarized production: the particles are right-handed, while the antiparticles are left-handed*)


ampPolarized[0] = amp[0]/.{
	Spinor[-Momentum[k2], r__]:>
		GA[6].Spinor[-Momentum[k2],r],
	Spinor[Momentum[k1], r__]:>
		Spinor[Momentum[k1], r].GA[7],
	Spinor[Momentum[p1],r__]:>
		GA[6].Spinor[Momentum[p1],r],
	Spinor[-Momentum[p2],r__]:>
		Spinor[Momentum[p2],r].GA[7]
}


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_e"], SMP["m_e"],
	SMP["m_mu"], SMP["m_mu"]];


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
	DiracSimplify//Simplify


ampSquaredPolarized[0] =
	(ampPolarized[0] (ComplexConjugate[ampPolarized[0]]))//
	FeynAmpDenominatorExplicit//FermionSpinSum//DiracSimplify//Simplify


ampSquaredMassless1[0] = ampSquared[0]//ReplaceAll[#,{
	SMP["m_e"] -> 0}]&


ampSquaredMassless2[0] = ampSquared[0]//ReplaceAll[#,{
	SMP["m_e"] -> 0,SMP["m_mu"] -> 0}]&//Simplify


ampSquaredPolarizedMassless[0] = ampSquaredPolarized[0]//ReplaceAll[#,{
	SMP["m_e"] -> 0,SMP["m_mu"] -> 0}]&//Simplify


(* ::Section:: *)
(*Total cross-section*)


(* ::Text:: *)
(*The differential cross-section d sigma/ d Omega is given by*)


prefac1=1/(64 Pi^2 s);


integral1=(Factor[ampSquaredMassless2[0]/.{t->-s/2(1-Cos[Th]),u->-s/2(1+Cos[Th]),
SMP["e"]^4->(4 Pi SMP["alpha_fs"])^2}])


diffXSection1= prefac1 integral1


(* ::Text:: *)
(*The differential cross-section d sigma/ d t d phi is given by*)


prefac2=1/(128 Pi^2 s)


integral2=Simplify[ampSquaredMassless2[0]/(s/4) /.
	{u-> -s-t,SMP["e"]^4->(4 Pi SMP["alpha_fs"])^2}]


diffXSection2=prefac2 integral2


(* ::Text:: *)
(*The total cross-section. We see that integrating both expressions gives the same result*)


2 Pi Integrate[diffXSection1 Sin[Th],{Th,0,Pi}]


crossSectionTotal=2 Pi Integrate[diffXSection2,{t,-s,0}]


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(8SMP["e"]^4 (SP[p1,k1]SP[p2,k2]+SP[p1,k2]SP[p2,k1]+
	SMP["m_mu"]^2 SP[p1,p2]))/(SP[p1+p2])^2//ExpandScalarProduct//
	ReplaceAll[#,SMP["m_e"]->0]&,
	(16SMP["e"]^4 (SP[p1,k2]SP[p2,k1]))/(SP[p1+p2])^2,
	((8SMP["e"]^4/s^2)((t/2)^2+(u/2)^2)),(4*Pi*SMP["alpha_fs"]^2)/(3*s)
};
FCCompareResults[{ampSquaredMassless1[0],ampSquaredPolarized[0],
ampSquaredMassless2[0],crossSectionTotal},knownResults,
Text->{"\tCompare to Peskin and Schroeder, An Introduction to QFT, \
Eqs 5.10, 5.21, 5.70 and to Field, \
Applications of Perturbative QCD, Eq. 2.1.14",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
