(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TIDL                               *)

(*
This software is covered by the GNU General Public License 3.
Copyright (C) 1990-2018 Rolf Mertig
Copyright (C) 1997-2018 Frederik Orellana
Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(*  :Summary:  Library of tensor integral decomposition formulas.
The formulas contained in tidlist can be always derived by using
Tdec. However, for performance reasons it is more convenient to
compute commonly used formulas only once and then keep them here, instead
of rederiving the decompositions over and over and thus loosing time.
*)

(* ------------------------------------------------------------------------ *)



TIDL::usage = "TIDL[{q,mu}, {p}]; TIDL[{{qi, mu}, {qj, nu}, ...}}, {p1, p2, ...}] \
or TIDL[exp, {{qi, mu}, {qj, nu}, ...}}, {p1, p2, ...}].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`TIDL`Private`"]

Options[TIDL] = {
Dimension -> D
};

TIDL[{a_/;Head[a]=!=List, b_/;Head[b]=!=List},c__] := TIDL[{{a,b}},c];

TIDL[a_List, b_List, OptionsPattern[]] :=
Block[{dim, rr},
	FCPrint[1,"Requesting integral of type",a,b,"in TIDL"];
	dim = OptionValue[Dimension];
	rr = tidl[Apply[Join, SortBy[GatherBy[a, #[[1]] &], {-Length[#] &}]], b, dim] /. tidlist;
	If[!FreeQ[rr, tidl],
	rr = rr /. tidl[aa_,_, dim] :>
	Apply[Times, Map[Pair[Momentum[#[[1]],dim],
	LorentzIndex[#[[2]],dim]]&, aa]]
	];
	rr
];

scpexp[x_,n_] :=
MemSet[scpexp[x,n],
	ChangeDimension[x[[1]]//.(x[[2]]//.ExpandScalarProduct[x[[3]]])//.
	ExpandScalarProduct[encli[[3]]], n]
];


(*To add new (long) formulas the following compression mechanism is quite effective:

exp = Tdec[{{q, i1}, {q, i2}, {q, i3}, {q, i4}, {q, i5}, {q, i6}, {q,
	i7}, {q, i8}}, {}, UseTIDL -> False, List -> False];
exp2 = Collect2[exp, {q, FVD, MTD}];
exp2 = Map[Isolate[#, HoldForm, IsolateNames -> ISO] &,
exp2 // ChangeDimension[#, 4] & // FCE //
	ReplaceAll[#, D -> n] &];
exp2 = Collect2[exp2, {q, HoldForm}] // FRH
z1 = Map[Isolate[#, HoldForm, IsolateNames -> ISO] &,
exp2 // ChangeDimension[#, 4] & // FCE //
	ReplaceAll[#, D -> n] &];
li = Cases[List@z1, HoldForm[ISO[__]], Infinity] // Union;
reru = MapIndexed[
	Rule[#1, ToExpression[("s" <> ToString[Identity @@ #2])]] &, li] //
	Flatten;
z2 = Map[ReplaceAll[#, HoldForm[a_] :> Rule[HoldForm[a], a]] &, li];
z1 = z1 /. reru;
z2 = z2 /. reru;
reru2 = MapIndexed[
	Rule[#1, ToExpression[("t" <> ToString[Identity @@ #2])]] &,
	Cases[z1, SP[_, _], Infinity] // Union] // Flatten;
reru3 = MapIndexed[
	Rule[#1, ToExpression[("v" <> ToString[Identity @@ #2])]] &,
	Cases[z1, FV[_, _], Infinity] // Union] // Flatten;
reru4 = MapIndexed[
	Rule[#1, ToExpression[("m" <> ToString[Identity @@ #2])]] &,
	Cases[z1, MT[_, _], Infinity] // Union] // Flatten;
reru21 = MapIndexed[
	Rule[#1, ToExpression[("t" <> ToString[Identity @@ #2])]] &,
	Cases[z2, SP[_, _], Infinity] // Union] // Flatten;
reru31 = MapIndexed[
	Rule[#1, ToExpression[("v" <> ToString[Identity @@ #2])]] &,
	Cases[z2, FV[_, _], Infinity] // Union] // Flatten;
reru41 = MapIndexed[
	Rule[#1, ToExpression[("m" <> ToString[Identity @@ #2])]] &,
	Cases[z2, MT[_, _], Infinity] // Union] // Flatten;
reru2 = Join[reru2, reru3, reru4];
reru21 = Join[reru21, reru31, reru41];
Put[{Collect2[z1 /. reru /. reru2, l], z2 /. reru21 /. reru2,
Map[Reverse, Join[reru21, reru2]]}, "formula.m"];
bli = Union@Flatten[{(Join[reru, reru2, reru3, reru4, reru21, reru31,
	reru41] /. {Rule[_, a_] :> a}), encli}]


Then edit formula.m and remove all spaces. Use following skeleton for Block

Block[{bli},
encli="formula.m";
scpexp[encli,n]
*)

(* 	Load precomputed tensor integral decompositions from FeynCalc/Tables/TIDL	*)
tidFiles = FileNames["*.tid",FileNameJoin[{$FeynCalcDirectory, "Tables", "TIDL"}]]
tidlist={}
Map[(tidlist=Join[tidlist,Get[#]]; 1)&,tidFiles];
tidlist = Dispatch[tidlist];


FCPrint[1,"TIDL.m loaded."];
End[]
