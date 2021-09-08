(* ::Package:: *)



(* :Title: ToPaVe2                                                       	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Converts scalar direct Passarino Veltman to PaVe functions	*)

(* ------------------------------------------------------------------------ *)


ToPaVe2::usage =
"ToPaVe2[expr] converts all the direct Passarino-Veltman functions (A0, A00,
B0, B1, B00, B11, C0, D0) to PaVe-functions.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToPaVe2`Private`"]


Options[ToPaVe2] = {
	PaVeAutoOrder -> True
};

ToPaVe2[expr_PaVe, OptionsPattern[]]:=
	expr;

ToPaVe2[expr_/;Head[expr]=!=PaVe, OptionsPattern[]] :=
	Block[{ex,pave},



		If[ !OptionValue[PaVe, PaVeAutoReduce],
			ex =expr /. {
				A0[m_, OptionsPattern[]] :> pave[0,{},{m}],
				A00[m_, OptionsPattern[]] :> pave[0,0,{},{m}],
				B0[pp_,m1_,m2_, OptionsPattern[]] :> pave[0,{pp},{m1,m2}],
				B1[pp_,m1_,m2_, OptionsPattern[]] :> pave[1,{pp},{m1,m2}],
				B00[pp_,m1_,m2_, OptionsPattern[]] :> pave[0,0,{pp},{m1,m2}],
				B11[pp_,m1_,m2_, OptionsPattern[]] :> pave[1,1,{pp},{m1,m2}],
				C0[p10_,p12_,p20_,m1_,m2_,m3_, OptionsPattern[]] :> pave[0,{p10,p12,p20},{m1,m2,m3}],
				D0[p10_,p12_,p23_,p30_,p20_,p13_,m1_,m2_,m3_,m4_, OptionsPattern[]] :> pave[0,{p10,p12,p23,p30,p20,p13},{m1,m2,m3,m4}]
			}
		];

		If[	OptionValue[PaVeAutoOrder]===False,
			ex = ex/. pave[x__] :> pave[x, PaVeAutoOrder->False]
		];

		ex = ex /. pave -> PaVe;


		ex
	];

FCPrint[1,"ToPaVe2.m loaded."];
End[]
