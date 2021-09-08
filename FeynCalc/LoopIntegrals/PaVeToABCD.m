(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PaVeToABCD                                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Converts suitable PaVe functions to direct Passarino Veltman
				functions													*)

(* ------------------------------------------------------------------------ *)


PaVeToABCD::usage =
"PaVeToABCD[expr] converts suitable PaVe functions to direct Passarino-Veltman
functions (A0,  A00, B0, B1, B00, B11, C0, D0). PaVeToABCD is nearly the
inverse of ToPaVe2.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PaVeToABCD`Private`"]


Options[PaVeToABCD] = {
};

PaVeToABCD[expr_, OptionsPattern[]] :=
	Block[{res, repRule, paveInts, paveIntsEval},

		paveInts = Cases2[expr,PaVe];

		paveIntsEval = paveInts /. {
				PaVe[0,{},{m_}, OptionsPattern[]] :>
					A0[m],
				PaVe[0,0,{},{m_}, OptionsPattern[]] :>
					A00[m],
				PaVe[0,{pp_},{m1_,m2_}, OptionsPattern[]] :>
					B0[pp,m1,m2],
				PaVe[1,{pp_},{m1_,m2_}, OptionsPattern[]] :>
					B1[pp,m1,m2],
				PaVe[0,0,{pp_},{m1_,m2_}, OptionsPattern[]] :>
					B00[pp,m1,m2],
				PaVe[1,1,{pp_},{m1_,m2_}, OptionsPattern[]] :>
					B11[pp,m1,m2],
				PaVe[0,{p10_,p12_,p20_},{m1_,m2_,m3_}, OptionsPattern[]] :>
					C0[p10,p12,p20,m1,m2,m3],
				PaVe[0,{p10_,p12_,p23_,p30_,p20_,p13_},{m1_,m2_,m3_,m4_}, OptionsPattern[]] :>
					D0[p10,p12,p23,p30,p20,p13,m1,m2,m3,m4]
		};

		paveIntsEval = paveIntsEval/. Rule[a_,a_]:>Unevaluated[Sequence[]];

		repRule = Thread[Rule[paveInts,paveIntsEval]];

		res = expr/. Dispatch[repRule];

		res
	];

FCPrint[1,"PaVeToABCD.m loaded."];
End[]
