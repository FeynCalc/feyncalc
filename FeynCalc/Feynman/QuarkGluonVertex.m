(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QuarkGluonVertex													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: Quark gluon vertex												*)

(* ------------------------------------------------------------------------ *)

QGV::usage =
"QGV is equivalent to QuarkGluonVertex.";

QuarkGluonVertex::usage =
"QuarkGluonVertex[mu, a] gives the Feynman rule for the quark-gluon vertex.

QGV can be used as an abbreviation of QuarkGluonVertex.

The dimension and the name of the coupling constant are determined by the
options Dimension and CouplingConstant.";

Begin["`Package`"]
End[]

Begin["`QuarkGluonVertex`Private`"]

DeclareNonCommutative[QuarkGluonVertex];

Options[QuarkGluonVertex] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Explicit -> False,
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
		dim   = OptionValue[Dimension];

		pol   = OptionValue[Polarization];
		mu = LorentzIndex[mui, dim];
		a  = SUNIndex[ai];
		gl3v = I coup DOT[SUNT[a], DiracGamma[mu, dim]];
		gl3v
	] /; OptionValue[Explicit] &&
					FreeQ[Union[Map[Head, {mui,ai}]], Integer];

QuarkGluonVertex[mui_, OptionsPattern[]] :=
	Block[ {gauge, dim, mu, a, gl3v, coun, coup, ope, pol},
		coup  = OptionValue[CouplingConstant];

		dim   = OptionValue[Dimension];

		pol   = OptionValue[Polarization];
		mu = LorentzIndex[mui, dim];

		gl3v =  I coup DiracGamma[mu, dim];

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
