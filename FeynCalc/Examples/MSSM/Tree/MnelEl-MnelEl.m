(* ::Package:: *)

(* :Title: MnelEl-MnelEl                                                      *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Mnel El -> Mnel El, MSSM, matrix element squared, tree            	*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Neutralino-electron scattering*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Mnel El -> Mnel El, MSSM, matrix element squared, tree";
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


diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[11, {1}],
	F[2, {1}]} -> {F[11,{1}],F[2,{1}]}, InsertionLevel -> {Classes},
	Model -> MSSM, ExcludeParticles -> {S[1], S[2], S[3], S[4], V[_]}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{k1,k2},ChangeDimension->4,List->False, SMP->True,
	DropSumOver->True]//.{
	USf[args1__][args2__]:>
	USf[args2,args1],Index[Sfermion, 5]:>Sfe5,
	Conjugate[ZNeu[a__]]:>ZNeuC[a],
	Conjugate[USf[a_,b_,c_,d_]]:>USfC[a,b,c,d]}


Cases2[amp[0],USf]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, MNeu[1], SMP["m_e"], MNeu[1], SMP["m_e"]];


(* ::Section:: *)
(*Evaluate the amplitude*)


amp[1]=DiracSimplify[amp[0]];


ampCC[1]=ComplexConjugate[amp[1],Conjugate->{ZNeuC,ZNeu,USf,USfC}]//.{
	Conjugate[USf][a_,b_,c_,d_]:>USfC[a,b,c,d],
	Conjugate[ZNeu][a__]:>ZNeuC[a],
	Conjugate[ZNeuC][a__]:>ZNeu[a],
	Conjugate[USfC][a_,b_,c_,d_]:>USf[a,b,c,d],
	Sfe5->Sfe5c};


(* ::Section:: *)
(*Square the amplitude*)


(* ::Text:: *)
(*To avoid having too many terms, we isolate everything that is not required to calculate the spin sums*)


amp[2]=Collect2[amp[1],Spinor,LorentzIndex,IsolateNames->KK];
ampCC[2]=Collect2[ampCC[1],Spinor,LorentzIndex,IsolateNames->KK];


ampSquared[0] = (amp[2] ampCC[2])//FermionSpinSum//DiracSimplify;


(* ::Text:: *)
(*For simplicity, we neglect the masses of the external particles.*)


ampSquared[1]=ampSquared[0]//FRH//PropagatorDenominatorExplicit//
	ReplaceAll[#,{MNeu[1]->0,SMP["m_e"]->0}]&//Factor2//
	TrickMandelstam[#,{s,t,u,0}]&;


(* ::Text:: *)
(*As we will show below, the pieces that contain a Levi-Civita tensor can be discarded, so we ignore*)
(*them in the final result*)


ampSquared[2]=SelectFree2[ampSquared[1],Eps]//Simplify


(* ::Text:: *)
(*The explicit dependence on the Levi-Civita tensor vanishes once we exploit the unitarity of the*)
(*sfermion mixing matrix USf*)


discarded=SelectNotFree2[ampSquared[1],Eps]//Simplify


Sum[discarded,{Sfe5,1,2},{Sfe5c,1,2}]//Simplify//ReplaceRepeated[#,
{USf[1,1,re__]USfC[1,2,re__]:>-USf[2,1,re]USfC[2,2,re],
USf[1,2,re__]USfC[1,1,re__]:>-USf[2,2,re]USfC[2,1,re]
}]&//Simplify


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	-(SMP["e"]^4*(SMP["sin_W"]^4*(USf[Sfe5, 1, 2, 1]*((s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1] + 
        4*(2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5c, 2, 2, 1]*USfC[Sfe5, 2, 2, 1])*USfC[Sfe5c, 1, 2, 1] + 
      4*USf[Sfe5, 2, 2, 1]*((2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1] + 
        4*(s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*USf[Sfe5c, 2, 2, 1]*USfC[Sfe5, 2, 2, 1])*USfC[Sfe5c, 2, 2, 1])*ZNeu[1, 1]^2*
     ZNeuC[1, 1]^2 + (s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*SMP["cos_W"]^4*USf[Sfe5, 1, 2, 1]*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1]*
     USfC[Sfe5c, 1, 2, 1]*ZNeu[1, 2]^2*ZNeuC[1, 2]^2 + 2*SMP["cos_W"]*SMP["sin_W"]^3*
     (USf[Sfe5, 1, 2, 1]*((s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1] + 
        2*(2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5c, 2, 2, 1]*USfC[Sfe5, 2, 2, 1])*USfC[Sfe5c, 1, 2, 1] + 
      2*(2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5, 2, 2, 1]*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1]*USfC[Sfe5c, 2, 2, 1])*ZNeu[1, 1]*ZNeuC[1, 1]*
     (ZNeu[1, 2]*ZNeuC[1, 1] + ZNeu[1, 1]*ZNeuC[1, 2]) + 2*(s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*SMP["cos_W"]^3*SMP["sin_W"]*
     USf[Sfe5, 1, 2, 1]*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1]*USfC[Sfe5c, 1, 2, 1]*ZNeu[1, 2]*ZNeuC[1, 2]*(ZNeu[1, 2]*ZNeuC[1, 1] + ZNeu[1, 1]*ZNeuC[1, 2]) + 
    SMP["cos_W"]^2*SMP["sin_W"]^2*(4*(2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5, 2, 2, 1]*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1]*USfC[Sfe5c, 2, 2, 1]*ZNeu[1, 1]*
       ZNeu[1, 2]*ZNeuC[1, 1]*ZNeuC[1, 2] + USf[Sfe5, 1, 2, 1]*USfC[Sfe5c, 1, 2, 1]*(4*(2*s*u + t*MSf[Sfe5, 2, 1]^2)*(2*s*u + t*MSf[Sfe5c, 2, 1]^2)*USf[Sfe5c, 2, 2, 1]*USfC[Sfe5, 2, 2, 1]*
         ZNeu[1, 1]*ZNeu[1, 2]*ZNeuC[1, 1]*ZNeuC[1, 2] + (s*u*(2*s*u + t*MSf[Sfe5c, 2, 1]^2) + MSf[Sfe5, 2, 1]^2*(s*t*u + (s^2 + u^2)*MSf[Sfe5c, 2, 1]^2))*USf[Sfe5c, 1, 2, 1]*USfC[Sfe5, 1, 2, 1]*
         (ZNeu[1, 2]^2*ZNeuC[1, 1]^2 + 4*ZNeu[1, 1]*ZNeu[1, 2]*ZNeuC[1, 1]*ZNeuC[1, 2] + ZNeu[1, 1]^2*ZNeuC[1, 2]^2)))))/
 (4*(s - MSf[Sfe5, 2, 1]^2)*(-u + MSf[Sfe5, 2, 1]^2)*(s - MSf[Sfe5c, 2, 1]^2)*(u - MSf[Sfe5c, 2, 1]^2)*SMP["cos_W"]^4*SMP["sin_W"]^4)
};
FCCompareResults[{ampSquared[2]},
knownResults,
Text->{"\tCompare to FormCalc:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];



