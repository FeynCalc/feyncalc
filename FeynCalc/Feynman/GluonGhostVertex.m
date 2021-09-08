(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GluonGhostVertex													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Gluon ghost vertex												*)

(* ------------------------------------------------------------------------ *)


GGV::usage =
"GGV is equivalent to GluonGhostVertex.";

GluonGhostVertex::usage =
"GluonGhostVertex[{p, mu, a}, {q, nu, b}, {k, rho, c}] or GluonGhostVertex[ p,
mu, a , q, nu, b , k, rho, c] yields the Gluon-Ghost vertex. The first
argument represents the gluon and the third argument the outgoing ghost field
(but incoming 4-momentum).

GGV can be used as an abbreviation of GluonGhostVertex.The dimension and the
name of the coupling constant are determined by the options Dimension and
CouplingConstant.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`GluonGhostVertex`Private`"]

Options[GluonGhostVertex] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Explicit -> False
};

GluonGhostVertex[{_, ai_}, {bi_}, {ki_, ci_}, opt:OptionsPattern[]] :=
	GluonGhostVertex[{FCGV["x"], FCGV["y"], ai}, {FCGV["z"],FCGV["h"],bi},
	{ki,FCGV["l"],ci}, opt] /; OptionValue[Explicit];

GGV = GluonGhostVertex;

GluonGhostVertex[a_,b_,c_, d_,e_,f_, g_,h_,i_, opt:OptionsPattern[]] :=
	GluonGhostVertex[{a,b,c},{d,e,f},{g,h,i},opt] /;
	FreeQ[Map[Head,{a,b,c,d,e,f,g,h,i}], Integer|Rule|RuleDelayed|List, Heads->False];

GluonGhostVertex[{_, mui_, ai_}, {___, bi_}, {ki_, ___, ci_}, opt:OptionsPattern[]] :=
	SUNF[SUNIndex[ai], SUNIndex[bi], SUNIndex[ci]] GluonGhostVertex[ki,mui,opt];

GluonGhostVertex[ki_, mui_, OptionsPattern[]] :=
	Block[ {dim, k, mu, re},
		dim   = OptionValue[Dimension];
		k = Momentum[ki,dim];
		mu = LorentzIndex[mui, dim];
		re = - OptionValue[CouplingConstant] Pair[k, mu];
		(* that is a matter of taste; the sign can be swapped between
			GhostPropagator and GluonGhostVertex.
			For the moment let's be consistent with Abbott (Nucl. Phys. B185 (1981)).
		*)
		(* re = -re;*)
		re = QCDFeynmanRuleConvention[GluonGhostVertex] re;
		re
	] /; OptionValue[Explicit];

GluonGhostVertex /:
	MakeBoxes[GluonGhostVertex[p3_,mu3_], TraditionalForm] :=
		RowBox[{SuperscriptBox[OverscriptBox["\[CapitalLambda]","~"],
		TBox[mu3]], "(", TBox[p3], ")"}];

GluonGhostVertex /:
	MakeBoxes[GluonGhostVertex[{_,_},{_,_},{p3_,mu3_}], TraditionalForm] :=
		RowBox[{SuperscriptBox[OverscriptBox["\[CapitalLambda]","~"],
		TBox[mu3]], "(", TBox[p3], ")"}];

FCPrint[1,"GluonGhostVertex.m loaded"];
End[]
