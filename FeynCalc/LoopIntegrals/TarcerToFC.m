(* ::Package:: *)



(* :Title: TarcerToFC                                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Conversion of TARCER integrals to FeynCalc ones				*)

(* ------------------------------------------------------------------------ *)

TarcerToFC::usage =
"TarcerToFC[expr, {q1, q2}] translates loop integrals in Tarcer-notation to the
FeynCalc notation.

See TFI for details on the convention.

As in the case of ToTFI, the \\frac{1}{\\pi^D} and \\frac{1}{\\pi^{D/2}}
prefactors are implicit, i.e. TarcerToFC doesn't add them.

To recover momenta from scalar products use the option ScalarProduct e.g. as
in TarcerToFC[TBI[D, pp^2, {{1, 0}, {1, 0}}], {q1, q2}, ScalarProduct ->
{{pp^2, p1}}]";

TarcerToFC::failmsg = "Error! TarcerToFC has encountered a fatal problem and \
must abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`TarcerToFC`Private`"]

Options[TarcerToFC]  = {
	FCE -> False,
	FCI -> False,
	ScalarProduct -> {}
};

TarcerToFC[expr_List, {q1_, q2_}, opts:OptionsPattern[]]:=
	TarcerToFC[#, {q1, q2}, opts]&/expr;

TarcerToFC[expr_/;Head[expr]=!=List, {q1_, q2_}, OptionsPattern[]] :=
	Block[{	ex, res, rules, tarcerHeads, tarcerInts, tarcerIntsConverted,
			ruConv, dimsPresent, optScalarProduct, momConvRules, momconv},

		tarcerHeads = {Tarcer`TFI,Tarcer`TVI,Tarcer`TJI,Tarcer`TBI,Tarcer`TAI};
		optScalarProduct = OptionValue[ScalarProduct];

		momConvRules = {
			momconv[0]->0,
			momconv[Pair[Momentum[p_,d___],Momentum[p_,d___]]] -> p
		};

		If[!MatchQ[optScalarProduct,{}|{{_,_}..}],
			Message[TarcerToFC::failmsg,"The option ScalarProduct must be of the form {{m2,p1},{pp,p2},...}"];
			Abort[]
		];

		If[	optScalarProduct=!={},
			momConvRules = Join[momConvRules,Map[Rule[momconv[#[[1]]], #[[2]]] &, optScalarProduct]]
		];

		If[	!FCDuplicateFreeQ[First/@momConvRules],
			Message[TarcerToFC::failmsg,"The rules given via the ScalarProduct option must be unambiguous."];
			Abort[]
		];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		(* Nothing to do *)
		If[	FreeQ2[ex,tarcerHeads],
			ex
		];

		If[	!FreeQ2[$ScalarProducts, {q1,q2}],
			Message[TarcerToFC::failmsg, "Some loop momenta have scalar product rules attached to them. Evaluation aborted!"];
			Abort[]
		];


		rules = {
			Tarcer`TFI[d_, pp_, {u_,v_,r_,s_,t_}, {{nu1_,m1_},{nu2_,m2_},{nu3_,m3_},{nu4_,m4_},{nu5_,m5_}}] :>
				Pair[Momentum[q1,d],Momentum[q1,d]]^u*
				Pair[Momentum[q2,d],Momentum[q2,d]]^v*
				Pair[Momentum[momconv[pp],d],Momentum[q1,d]]^r*
				Pair[Momentum[momconv[pp],d],Momentum[q2,d]]^s*
				Pair[Momentum[q1,d],Momentum[q2,d]]^t*
				FCI[FAD[{q1,m1,nu1},{q2,m2,nu2},{q1-momconv[pp],m3,nu3},{q2-momconv[pp],m4,nu4},{q1-q2,m5,nu5},Dimension->d]],


			Tarcer`TFI[d_, pp_, dp_, {a_,b_}, {{nu1_,m1_},{nu2_,m2_},{nu3_,m3_},{nu4_,m4_},{nu5_,m5_}}] :>
				Pair[Momentum[dp,d],Momentum[q1,d]]^a*
				Pair[Momentum[dp,d],Momentum[q2,d]]^b*
				Identity[FAD[{q1,m1,nu1},{q2,m2,nu2},{q1-momconv[pp],m3,nu3},{q2-momconv[pp],m4,nu4},{q1-q2,m5,nu5},Dimension->d]],

			Tarcer`TFI[d_, pp_, {{nu1_,m1_},{nu2_,m2_},{nu3_,m3_},{nu4_,m4_},{nu5_,m5_}}] :>
				FCI[FAD[{q1,m1,nu1},{q2,m2,nu2},{q1-momconv[pp],m3,nu3},{q2-momconv[pp],m4,nu4},{q1-q2,m5,nu5},Dimension->d]],

			Tarcer`TFI[d_, pp_, {nu1_,nu2_,nu3_,nu4_,nu5_}] :>
				FCI[FAD[{q1,0,nu1},{q2,0,nu2},{q1-momconv[pp],0,nu3},{q2-momconv[pp],0,nu4},{q1-q2,0,nu5},Dimension->d]],

			Tarcer`TVI[d_, pp_, {{nu1_,m1_},{nu2_,m2_},{nu3_,m3_},{nu4_,m4_}}] :>
				FCI[FAD[{q1-q2,m1,nu1},{q2,m2,nu2},{q1-momconv[pp],m3,nu3},{q2-momconv[pp],m4,nu4},Dimension->d]],

			Tarcer`TJI[d_, pp_, {{nu1_,m1_},{nu2_,m2_},{nu3_,m3_}}] :>
				FCI[FAD[{q1,m1,nu1},{q1-q2,m2,nu2},{q2-momconv[pp],m3,nu3},Dimension->d]],

			Tarcer`TBI[d_, pp_, {{nu1_,m1_},{nu2_,m2_}}] :>
				FCI[FAD[{q1,m1,nu1},{q1-momconv[pp],m2,nu2},Dimension->d]],

			Tarcer`TAI[d_, 0, {{nu1_,m1_}}] :>
				FCI[FAD[{q1,m1,nu1},Dimension->d]]


		};

		tarcerInts = Cases2[ex,tarcerHeads];

		dimsPresent = Union[First/@tarcerInts];

		tarcerIntsConverted = tarcerInts/. Dispatch[rules] /. momConvRules;
		ruConv = Thread[Rule[tarcerInts,tarcerIntsConverted]];

		(*
			If the p.p was replaced by a mass squared, we obviously cannot recover the original momentum.
			It is up to the user to do that. Th
		 *)

		ruConv = SelectFree[ruConv,momconv];

		res = ex /. Dispatch[ruConv];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint[1,"TarcerToFC.m loaded."];
End[]
