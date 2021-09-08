(* ::Package:: *)



(* :Title: TFIOrder                                                       	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Orders the arguments of some TFI functions in a
				standard way												*)

(* ------------------------------------------------------------------------ *)

TFIOrder::usage =
"TFIOrder[exp] orders the arguments of some TFI functions in exp in a standard
way.";


(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`TFIOrder`Private`"]

Options[TFIOrder] = {};

TFIOrder[expr_, OptionsPattern[]] :=
	Block[{res,repRule},

		If[	!FreeQ2[{expr}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		repRule= {
			(*Symmetry under the interchang q1 <-> q2*)
			Tarcer`TFI[d_, p_, {{1, m1_}, {1, m2_}, {1, m3_}, {1, m4_}, {1, m5_}}] /; ! OrderedQ[{m1, m2}] :>
				Tarcer`TFI[d, p, {{1, m2}, {1, m1}, {1, m4}, {1, m3}, {1, m5}}],
			(*Symmetry under the shifts q1 -> q1-p and q2 -> q2-p, *)
			Tarcer`TFI[d_, p_, {{1, m1_}, {1, m2_}, {1, m3_}, {1, m4_}, {1, m5_}}] /; ! OrderedQ[{{m1, m2}, {m3, m4}}] :>
				Tarcer`TFI[d, p, {{1, m3}, {1, m4}, {1, m1}, {1, m2}, {1, m5}}]
			};
		res = expr//.repRule;
		res
	];


FCPrint[1,"TFIOrder.m loaded."];
End[]
