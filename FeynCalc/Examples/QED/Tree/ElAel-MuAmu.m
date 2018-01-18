(* ::Package:: *)

(* :Title: ElAel-MuAmu														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  El Ael -> Mu Amu, QED, matrix element squared, tree			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Muon production*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El Ael -> Mu Amu, QED, matrix element squared, tree";
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
	PropagatorDenominatorExplicit//
	FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
	DiracSimplify[#,DiracTraceEvaluate->True]&//Simplify


ampSquaredPolarized[0] =
	(ampPolarized[0] (ComplexConjugate[ampPolarized[0]]))//
	PropagatorDenominatorExplicit//FermionSpinSum//
	DiracSimplify[#,DiracTraceEvaluate->True]&//Simplify


ampSquaredMassless1[0] = ampSquared[0]//ReplaceAll[#,{
	SMP["m_e"] -> 0}]&


ampSquaredMassless2[0] = ampSquared[0]//ReplaceAll[#,{
	SMP["m_e"] -> 0,SMP["m_mu"] -> 0}]&//Simplify


ampSquaredPolarizedMassless[0] = ampSquaredPolarized[0]//ReplaceAll[#,{
	SMP["m_e"] -> 0,SMP["m_mu"] -> 0}]&//Simplify


(* ::Section:: *)
(*Total cross-section*)


integral=Integrate[Simplify[ampSquaredMassless2[0]/(s/4) /.
u-> -s-t],{t,-s,0}]/.SMP["e"]^4->(4 Pi SMP["alpha_fs"])^2


prefac=2Pi/(128 Pi^2 s)


(* ::Text:: *)
(*The total cross-section *)


crossSectionTotal=integral*prefac//PowerExpand//Factor2


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



