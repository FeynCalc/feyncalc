(* ::Package:: *)

(* :Title: Ga-MuAmu															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:  Ga^* -> Mu Amu, QED, total decay rate, tree					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Muon production from the decay of a*)
(*virtual photon*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Ga^* -> Mu Amu, QED, total decay rate, tree";
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


MakeBoxes[p,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diags = InsertFields[CreateTopologies[0, 1 -> 2], {V[1]} ->
		{F[2,{2}], -F[2, {2}]}, InsertionLevel -> {Classes},
		Restrictions->QEDOnly];

Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p},
	OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,
	List->False, SMP->True, Contract->True]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SP[k1]=SMP["m_mu"]^2;
SP[k2]=SMP["m_mu"]^2;
SP[k1,k2]= (QQ - SP[k1] - SP[k2])/2;


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//FermionSpinSum//DiracSimplify//
	DoPolarizationSums[#,p,0,VirtualBoson->True]&//Simplify


ampSquaredMassless[0] = ampSquared[0]//ReplaceAll[#,{
	SMP["m_mu"] -> 0}]&


(* ::Section:: *)
(*Total decay rate*)


(* ::Text:: *)
(*The differential decay rate  d Gamma/ d Omega is given by*)


prefac=ExpandScalarProduct[1/(64 Pi^2) 1/Sqrt[(SP[k1+k2])]]


diffDecayRate=prefac ampSquaredMassless[0]/.
SMP["e"]^2->(4 Pi SMP["alpha_fs"])


(* ::Text:: *)
(*The total decay-rate*)


decayRateTotal=4 Pi diffDecayRate


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	SMP["alpha_fs"] Sqrt[QQ]
};
FCCompareResults[{decayRateTotal},knownResults,
Text->{"\tCompare to Field, \
Applications of Perturbative QCD, Eq. 2.1.29",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
