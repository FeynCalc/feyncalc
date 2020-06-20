(* ::Package:: *)

(* :Title: Ga-QQbar                                              			*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Ga^* -> Q Qbar, QCD, total decay rate, tree    				*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Quark-antiquark production from the decay of a virtual photon*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Ga^* -> Q Qbar, QCD, total decay rate, tree";
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


MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diags = InsertFields[CreateTopologies[0, 1 -> 2], {V[1]} ->
	{F[3, {1}], -F[3, {1}]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p},
	OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,
	List->False, SMP->True, Contract->True,DropSumOver->True,
	Prefactor->3/2 SMP["e_Q"],FinalSubstitutions->{SMP["m_u"]->SMP["m_q"]}]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SP[k1]=SMP["m_q"]^2;
SP[k2]=SMP["m_q"]^2;
SP[k1,k2]= (QQ - SP[k1] - SP[k2])/2;


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//SUNSimplify[#,Explicit->True,
	SUNNToCACF->False]&//FermionSpinSum//DoPolarizationSums[#,p,0,
	VirtualBoson->True]&//DiracSimplify


ampSquaredMassless[0] = ampSquared[0]//ReplaceAll[#,{SMP["m_q"] -> 0}]&


ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0]/.SUNN->3


(* ::Section:: *)
(*Total decay rate*)


(* ::Text:: *)
(*The differential decay rate  d Gamma/ d Omega is given by*)


prefac=ExpandScalarProduct[1/(64 Pi^2) 1/Sqrt[(SP[k1+k2])]]


diffDecayRate=prefac ampSquaredMasslessSUNN3[0]/.
SMP["e"]^2->(4 Pi SMP["alpha_fs"])


(* ::Text:: *)
(*The total decay-rate*)


decayRateTotal=4 Pi diffDecayRate


(* ::Text:: *)
(*Notice that up to the overall color factor 3 and the quark electric charge squared this result is identical to the total decay rate of a virtual photon into a muon-antimuon pair*)


decayRateTotalQED= SMP["alpha_fs"] Sqrt[QQ]


(* ::Text:: *)
(*Taking the ration of the two gives us the famous R-ration prediction of the parton mode, where the summation over the quark flavors in front of the charge squared is understood*)


decayRateTotal/decayRateTotalQED


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	3 SMP["alpha_fs"] SMP["e_Q"]^2 Sqrt[QQ]
};
FCCompareResults[{decayRateTotal},knownResults,
Text->{"\tCompare to Field, \
Applications of Perturbative QCD, Eq. 2.1.30",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
