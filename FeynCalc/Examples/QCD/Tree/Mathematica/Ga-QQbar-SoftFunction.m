(* ::Package:: *)

(* :Title: Ga-QQbar-SoftFunction                                     			*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Ga^* -> Q Qbar, QCD, SCET soft function, tree 			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*LO SCET Soft function*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Ga^* -> Q Qbar, QCD, SCET soft function, tree";
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

FCCheckVersion[10,0,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[k1,{SubscriptBox,k,1}]
FCAttachTypesettingRule[k2,{SubscriptBox,k,2}]


diagQQ = InsertFields[CreateTopologies[0, 1 -> 2], {V[1]} ->
	{F[3, {1}], -F[3, {1}]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];
Paint[diagQQ, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


diagsQQG = InsertFields[CreateTopologies[0, 1 -> 3], {V[1]} ->
	{F[3, {1}], -F[3, {1}], V[5]}, InsertionLevel -> {Classes},
	Model -> "SMQCD"];
Paint[diagsQQG, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitudes*)


ampQQ[0] = FCFAConvert[CreateFeynAmp[diagQQ], IncomingMomenta->{p},
	OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->D,
	List->False, SMP->True, Contract->True,DropSumOver->True,
	Prefactor->3/2 SMP["e_Q"],FinalSubstitutions->{SMP["m_u"]->0}]


ampQQG[0] = FCFAConvert[CreateFeynAmp[diagsQQG], IncomingMomenta->{p},
	OutgoingMomenta->{k1,k2,k},UndoChiralSplittings->True,ChangeDimension->D,
	List->True, SMP->True, Contract->True,DropSumOver->True,
	Prefactor->3/2 SMP["e_Q"],FinalSubstitutions->{SMP["m_u"]->0}]


(* ::Section:: *)
(*Fix the kinematics*)


(* ::Text:: *)
(*quark k1 is collinear so that k1 = n^mu (k1.nb) with k1 ~ (la^2,1,la)*)
(*antiquark k2 is anticollinear so that k2 = nb^mu (k2.n) with k2 ~ (1,la^2,la)*)
(*gluon k is ultrasoft with k ~ (la^2, la^2, la^2)*)


$FCDefaultLightconeVectorN=n;
$FCDefaultLightconeVectorNB=nb;
FCClearScalarProducts[]
ScalarProduct[nb]=0;
ScalarProduct[n,nb]=2;
ScalarProduct[n]=0;
ScalarProduct[k]=0;
ScalarProduct[k1,n]=0;
ScalarProduct[k2,nb]=0;


LightConePerpendicularComponent[Momentum[k1],Momentum[n],Momentum[nb]]=0;
LightConePerpendicularComponent[Momentum[k2],Momentum[n],Momentum[nb]]=0;
LightConePerpendicularComponent[Momentum[k1,D],Momentum[n,D],Momentum[nb,D]]=0;
LightConePerpendicularComponent[Momentum[k2,D],Momentum[n,D],Momentum[nb,D]]=0;


DataType[Q,FCVariable]=True;
DataType[la,FCVariable]=True;


(* ::Section:: *)
(*Auxiliary code *)


(* ::Text:: *)
(*This code handles the decomposition of spinors containing only collinear and anticollinear components*)


ClearAll[spinorDecomposeD];
spinorDecomposeD[ex_,cMoms_List,acMoms_List,n_,nb_]:=
	Block[{expr,holdDOT,res,Pmin,Pplus,hold},
		Pmin=GSD[nb,n]/4;
		Pplus=GSD[n,nb]/4;
		expr=ex/.DOT->holdDOT;
		expr=expr//.{
			(*ubar_xi_c n_slash = 0*)
			(*vbar_xi_c n_slash = 0*)
			holdDOT[Spinor[c_. Momentum[mom_,D],r___],rest___]/;MemberQ[cMoms,mom]:>
				holdDOT[hold[Spinor][c Momentum[mom,D],r],Pmin,rest],
			
			(*n_slash u_xi_c  = 0*)
			(*n_slash v_xi_c  = 0*)
			holdDOT[rest___,Spinor[c_. Momentum[mom_,D],r___]]/;MemberQ[cMoms,mom]:>
				holdDOT[rest,Pplus,hold[Spinor][c Momentum[mom,D],r]],

			(*ubar_xi_cbar nbar_slash = 0*)
			(*vbar_xi_cbar nbar_slash = 0*)
			holdDOT[Spinor[c_. Momentum[mom_,D],r___],rest___]/;MemberQ[acMoms,mom]:>
				holdDOT[hold[Spinor][c Momentum[mom,D],r],Pplus,rest],

			(*nbar_slash u_xi_cbar  = 0*)
			(*nbar_slash v_xi_cbar  = 0*)
			holdDOT[rest___,Spinor[c_. Momentum[mom_,D],r___]]/;MemberQ[acMoms,mom]:>
				holdDOT[rest,Pmin,hold[Spinor][c Momentum[mom,D],r]]
		};
		res=expr/.holdDOT->DOT/.hold->Identity;
		res
	];


(* ::Section:: *)
(*Expand and square the amplitudes*)


(* ::Text:: *)
(*Born amplitude rewritten in terms of large components of the collinear fields*)


ampQQ[1]=ampQQ[0]//ToLightConeComponents//spinorDecomposeD[#,{k1},{k2},n,nb]&//
DiracSimplify


ampQQSq[1]=SUNSimplify[ampQQ[1]ComplexConjugate[ampQQ[1]]]//Simplify


(* ::Text:: *)
(*Introduce the lightcone components, simplify Dirac algebra, add scaling of k for the expansion*)


ampQQG[1]=ampQQG[0]//FeynAmpDenominatorExplicit//ToLightConeComponents//
DiracSimplify//FCReplaceMomenta[#,{k->la^2 k}]&;


(* ::Text:: *)
(*Expand up to leading power, reorder Dirac matrices*)


ampQQG[2]=Series[ampQQG[1],{la,0,-2}]//Normal//DotSimplify//
DiracSimplify[#,DiracOrder->{n,nb,Polarization}]&//ReplaceAll[#,la->1]&


(* ::Text:: *)
(*Introduce large components of the collinear fields*)


ampQQG[3]=ampQQG[2]//spinorDecomposeD[#,{k1},{k2},n,nb]&//DiracSimplify


(* ::Text:: *)
(*Square the amplitudes, sum over the gluon polarizations*)


ampQQGSq[1]=Total[ampQQG[3]]ComplexConjugate[Total[ampQQG[3]]]//SUNSimplify//
DoPolarizationSums[#,k,aux]&


(* ::Section:: *)
(*Final LO soft function*)


(* ::Text:: *)
(*Divide out the born amplitude squared*)


aux=(ampQQGSq[1]/ampQQSq[1])/.SMP["g_s"]->Sqrt[4Pi SMP["alpha_s"]]


(* ::Text:: *)
(*Account for the extra prefactor*)


pref=1/(32Pi^2);


res = aux pref


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(CF*SMP["alpha_s"])/(2*Pi*Pair[Momentum[k, D], Momentum[n, D]]*Pair[Momentum[k, D], Momentum[nb, D]])
};
FCCompareResults[{res},knownResults,
Text->{"\tCompare to Automation, \
of Calculations in Soft-Collinear Effective Theory by R. Rahn, Eq. 5.3",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



