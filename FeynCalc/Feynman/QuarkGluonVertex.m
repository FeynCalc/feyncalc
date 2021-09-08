(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QuarkGluonVertex													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Quark gluon vertex												*)

(* ------------------------------------------------------------------------ *)

QGV::usage =
"QGV is equivalent to QuarkGluonVertex.";

QuarkGluonVertex::usage =
"QuarkGluonVertex[\[Mu], a] gives the Feynman rule for the quark-gluon vertex.

QGV can be used as an abbreviation of QuarkGluonVertex.

The dimension and the name of the coupling constant are determined by the
options Dimension and CouplingConstant.";

Begin["`Package`"]
End[]

Begin["`QuarkGluonVertex`Private`"]

DeclareNonCommutative[QuarkGluonVertex];

Options[QuarkGluonVertex] = {
	CounterTerm -> False,
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Explicit -> False,
	OPE -> False,
	Polarization -> 0
};

QGV = QuarkGluonVertex;

QuarkGluonVertex[x1_,x2_,x3_,x4_,x5_,x6_,x7_,x8_, x9_, y:OptionsPattern[]] :=
	QuarkGluonVertex[{x1,x2,x3}, {x4,x5,x6}, {x7,x8,x9} , y] /;
	FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8,x9}]], Integer|Rule];

QuarkGluonVertex[mui_, ai_/;Head[ai]=!=Rule, opt:OptionsPattern[]] :=
	QuarkGluonVertex[{Null, mui, ai}, {Null, Null, Null},
	{Null, Null, Null}, opt] /;    FreeQ[Union[Map[Head, {mui,ai}]], Integer];

QuarkGluonVertex[{p_, mui_, ai_}, {q_,___}, {k_,___}, OptionsPattern[]] :=
	Block[ {gauge, dim, mu, a, gl3v, coun, coup, ope, pol},
		coup  = OptionValue[CouplingConstant];
		coun  = OptionValue[CounterTerm];
		dim   = OptionValue[Dimension];
		ope   = OptionValue[OPE];
		pol   = OptionValue[Polarization];
		mu = LorentzIndex[mui, dim];
		a  = SUNIndex[ai];
		If[ !ope,
			gl3v = 0,
			gl3v = OPE Twist2QuarkOperator[{q}, {k}, {p,mui,ai}, Polarization -> pol]
		];
		Which[
			coun === 1,
				gl3v = gl3v + (I) Sn coup^3 (CF-CA/2) DOT[SUNT[a], DiracGamma[mu,dim]] 2/Epsilon,
			coun === 2,
				gl3v = gl3v + (I) Sn coup^3 CA DOT[SUNT[a], DiracGamma[mu,dim]] 3/Epsilon,
			coun === 3,
				gl3v = gl3v + (I) Sn coup^3 (CF+CA) DOT[SUNT[a], DiracGamma[mu,dim]] 2/Epsilon
		];
		If[ !coun,
			gl3v = gl3v + I coup DOT[SUNT[a], DiracGamma[mu, dim]]
		];
		gl3v
	] /; OptionValue[Explicit] &&
					FreeQ[Union[Map[Head, {mui,ai}]], Integer];

QuarkGluonVertex[mui_, OptionsPattern[]] :=
	Block[ {gauge, dim, mu, a, gl3v, coun, coup, ope, pol},
		coup  = OptionValue[CouplingConstant];
		coun  = OptionValue[CounterTerm];
		dim   = OptionValue[Dimension];
		ope   = OptionValue[OPE];
		pol   = OptionValue[Polarization];
		mu = LorentzIndex[mui, dim];
		If[ !ope,
			gl3v = 0,
			gl3v = OPE Twist2QuarkOperator[{q}, {k}, {p,mui}, Polarization -> pol]
		];
		Which[
			coun === 1,
				gl3v = gl3v + (I) Sn coup^3 (CF-CA/2) DiracGamma[mu,dim] 2/Epsilon,
			coun === 2,
				gl3v = gl3v + (I) Sn coup^3 CA DiracGamma[mu,dim] 3/Epsilon,
			coun === 3,
				gl3v = gl3v + (I) Sn coup^3 (CF+CA) DiracGamma[mu,dim] 2/Epsilon
		];
		If[ !coun,
			gl3v = gl3v + I coup DiracGamma[mu, dim]
		];
		gl3v
	] /; OptionValue[Explicit] && FreeQ[Union[Map[Head, {mui}]], Integer];

QuarkGluonVertex /:
	MakeBoxes[QuarkGluonVertex[mu1_], TraditionalForm] :=
		SuperscriptBox["Q", TBox[mu1] ]

QuarkGluonVertex /:
	MakeBoxes[QuarkGluonVertex[{_,mu1_, a_},{__},{__}, OptionsPattern[]], TraditionalForm] :=
		SubsuperscriptBox["Q",TBox[a], TBox[mu1] ]

FCPrint[1,"QuarkGluonVertex.m loaded"];
End[]
