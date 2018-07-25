(* ::Package:: *)

(* :Title: EWZ-ffbar											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Z \[Rule] f fbar, EW theory, total decay rate, 
              tree level.		*)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Z \[Rule] f fbar, EW theory, total decay rate, tree level."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


diagsMasslessLeptonsTree = InsertFields[CreateTopologies[0,1 -> 2], {V[2]} -> {F[1,{l}],-F[1,{l}]},
InsertionLevel -> {Classes}];
Paint[diagsMasslessLeptonsTree,ColumnsXRows->{1,1},Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


diagsMassiveLeptonsTree = InsertFields[CreateTopologies[0,1 -> 2], {V[2]} -> {F[2,{l}],-F[2,{l}]},
InsertionLevel -> {Classes}];
Paint[diagsMassiveLeptonsTree,ColumnsXRows->{1,1},Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


diagsUpQuarksTree = InsertFields[CreateTopologies[0,1 -> 2], {V[2]} -> {F[3,{l}],-F[3,{l}]},
InsertionLevel -> {Classes}];
Paint[diagsUpQuarksTree,ColumnsXRows->{1,1},Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


diagsDownQuarksTree = InsertFields[CreateTopologies[0,1 -> 2], {V[2]} -> {F[4,{l}],-F[4,{l}]},
InsertionLevel -> {Classes}];
Paint[diagsDownQuarksTree,ColumnsXRows->{1,1},Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampsMasslessLeptonsTree=FCFAConvert[CreateFeynAmp[diagsMasslessLeptonsTree],IncomingMomenta->{p},OutgoingMomenta->{l1,l2},List->False,ChangeDimension->4,
DropSumOver->True,SMP->True,UndoChiralSplittings->True,FinalSubstitutions->{MLE[l]->SMP["m_l"],
SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"]  SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]//Contract


ampsMassiveLeptonsTree=FCFAConvert[CreateFeynAmp[diagsMassiveLeptonsTree],IncomingMomenta->{p},OutgoingMomenta->{p1,p2},List->False,ChangeDimension->4,
DropSumOver->True,SMP->True,UndoChiralSplittings->True,FinalSubstitutions->{MLE[l]->SMP["m_l"],
SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"]  SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]//Contract


ampsUpQuarksTree=FCFAConvert[CreateFeynAmp[diagsUpQuarksTree],IncomingMomenta->{p},OutgoingMomenta->{k1,k2},List->False,ChangeDimension->4,
DropSumOver->True,SMP->True,UndoChiralSplittings->True,FinalSubstitutions->{MQU[l]->SMP["m_q"],
SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]//Contract


ampsDownQuarksTree=FCFAConvert[CreateFeynAmp[diagsDownQuarksTree],IncomingMomenta->{p},OutgoingMomenta->{k1,k2},List->False,ChangeDimension->4,
DropSumOver->True,SMP->True,UndoChiralSplittings->True,FinalSubstitutions->{MQD[l]->SMP["m_q"],
SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]//Contract


(* ::Section:: *)
(*Setup the kinematics*)


FCClearScalarProducts[]
SP[p]=SMP["m_Z"]^2;
SP[k1]=SMP["m_q"]^2;
SP[k2]=SMP["m_q"]^2;

SP[p1]=SMP["m_l"]^2;
SP[p2]=SMP["m_l"]^2;

SP[l1]=0;
SP[l2]=0;

SP[k1,k2]=Simplify[(SP[p]-SP[k1]-SP[k2])/2];
SP[p,k1]=Simplify[ExpandScalarProduct[SP[k1+k2,k1]]];
SP[p,k2]=Simplify[ExpandScalarProduct[SP[k1+k2,k2]]];

SP[p1,p2]=Simplify[(SP[p]-SP[p1]-SP[p2])/2];
SP[p,p1]=Simplify[ExpandScalarProduct[SP[p1+p2,p1]]];
SP[p,p2]=Simplify[ExpandScalarProduct[SP[p1+p2,p2]]];

SP[l1,l2]=Simplify[(SP[p]-SP[l1]-SP[l2])/2];
SP[p,l1]=Simplify[ExpandScalarProduct[SP[l1+l2,l1]]];
SP[p,l2]=Simplify[ExpandScalarProduct[SP[l1+l2,l2]]];


(* ::Section:: *)
(*Obtain squared amplitude for the unpolarized process*)


(* ::Text:: *)
(*We average over the polarizations of the Z-boson*)


ampsMasslessLeptonsTreeSquared=ampsMasslessLeptonsTree ComplexConjugate[ampsMasslessLeptonsTree]//DoPolarizationSums[#,p, ExtraFactor->1/3]&//
FermionSpinSum//DiracSimplify//Simplify


ampsMassiveLeptonsTreeSquared=ampsMassiveLeptonsTree ComplexConjugate[ampsMassiveLeptonsTree]//DoPolarizationSums[#,p, ExtraFactor->1/3]&//
FermionSpinSum//DiracSimplify//Simplify


ampsUpQuarksTreeSquared=ampsUpQuarksTree ComplexConjugate[ampsUpQuarksTree]//DoPolarizationSums[#,p, ExtraFactor->1/3]&//
FermionSpinSum//DiracSimplify//SUNSimplify//Simplify


ampsDownQuarksTreeSquared=ampsDownQuarksTree ComplexConjugate[ampsDownQuarksTree]//DoPolarizationSums[#,p, ExtraFactor->1/3]&//
FermionSpinSum//DiracSimplify//SUNSimplify//Simplify


(* ::Section:: *)
(*Calculate the total decay rates*)


phaseSpacePrefactor[m1_,m2_,M_]:=1/(16 Pi M)Sqrt[1-(m1+m2)^2/M^2]Sqrt[1-(m1-m2)^2/M^2];


totalDecayRateMasslessLeptons=phaseSpacePrefactor[0,0,SMP["m_Z"]]ampsMasslessLeptonsTreeSquared//Simplify


totalDecayRateMassiveLeptons=phaseSpacePrefactor[SMP["m_l"],SMP["m_l"],SMP["m_Z"]]ampsMassiveLeptonsTreeSquared//Simplify


totalDecayRateUpQuarks=phaseSpacePrefactor[SMP["m_q"],SMP["m_q"],SMP["m_Z"]]ampsUpQuarksTreeSquared//Simplify


totalDecayRateDownQuarks=phaseSpacePrefactor[SMP["m_q"],SMP["m_q"],SMP["m_Z"]]ampsDownQuarksTreeSquared//Simplify


(* ::Section:: *)
(*Check with the literature*)


totalDecayRateKnown=(SMP["G_F"]*Sqrt[1 - (4*mf^2)/SMP["m_Z"]^2]*SMP["m_Z"]^3*(cv^2(1+2mf^2/SMP["m_Z"]^2)+ca^2(1-4mf^2/SMP["m_Z"]^2)))/(6*Pi Sqrt[2]);
Print["Check with Grozin, Using REDUCE in High Energy Physics, Chapter 5.2: ",
			If[{Simplify[(totalDecayRateKnown/.{cv|ca->1/2,mf->0})-totalDecayRateMasslessLeptons],
Simplify[(totalDecayRateKnown/.{cv->-1/2+2SMP["sin_W"]^2,ca->-1/2,mf->SMP["m_l"]})-totalDecayRateMassiveLeptons],
Simplify[CA(totalDecayRateKnown/.{cv->1/2-4/3SMP["sin_W"]^2,ca->1/2,mf->SMP["m_q"]})-totalDecayRateUpQuarks],
Simplify[CA(totalDecayRateKnown/.{cv->-1/2+2/3SMP["sin_W"]^2,ca->-1/2,mf->SMP["m_q"]})-totalDecayRateDownQuarks]}==={0,0,0,0}, 
"CORRECT.", "!!! WRONG !!!"]];
