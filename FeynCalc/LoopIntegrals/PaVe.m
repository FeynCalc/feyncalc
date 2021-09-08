(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PaVe																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Passarino-Veltman functions and their symmetries				*)

(* ------------------------------------------------------------------------ *)

GenPaVe::usage =
"GenPaVe[i, j, ..., {{0, m0}, {Momentum[p1], m1}, {Momentum[p2], m2}, ...]
denotes the invariant (and scalar) Passarino-Veltman integrals, i.e. the
coefficient functions of the tensor integral decomposition. In contrast to
PaVe which uses the LoopTools convention,  masses and external momenta in
GenPaVe are written in the same order as they appear in the original tensor
integral, i.e. FAD[{q,m0},{q-p1,m1},{q-p2,m2},...].";

PaVe::usage =
"PaVe[i, j, ..., {p10, p12, ...}, {m1^2, mw^2, ...}] denotes the invariant (and
scalar) Passarino-Veltman integrals, i.e. the coefficient functions of the
tensor integral decomposition. Joining plist and mlist gives the same
conventions as for A0, B0, C0, D0. Automatic simplifications are performed for
the coefficient functions of two-point integrals and for the scalar integrals.";

PaVe::nonexistent=
"A Passarino-Veltman function `1` doesn't exist. Please check your input."

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PaVe`Private`"];


ClearAttributes[PaVe, ReadProtectecd];

Options[PaVe] = {
	PaVeAutoOrder -> True,
	PaVeAutoReduce -> False
};

(* Symmetry in the indices *)
PaVe[i_, j__,  pl_List, ml_List, opts:OptionsPattern[]] :=
	PaVe[Sequence@@Sort[{i,j}],pl,ml,opts]/; !OrderedQ[{i,j}];

(* Special cases of PaVe: *)
PaVe[0, {}, {x_}, OptionsPattern[]] :=
	A0[x]/; OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{x}];

PaVe[0,0, {}, {x_}, OptionsPattern[]] :=
	A00[x]/; OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{x}];

PaVe[0, {pp_}, {m1_, m2_}, OptionsPattern[]] :=
	B0[pp, m1, m2]/; OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{pp,m1,m2}];

PaVe[1,{pp_}, {m1_,m2_}, OptionsPattern[]] :=
	B1[pp, m1, m2]/; OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{pp,m1,m2}];

PaVe[0,0,{pp_}, {m1_,m2_}, OptionsPattern[]] :=
	B00[pp,m1,m2]/; OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{pp,m1,m2}];

PaVe[1,1,{pp_}, {m1_,m2_}, OptionsPattern[]] :=
	B11[pp,m1,m2]/; OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{pp,m1,m2}];

PaVe[0, invs1_List, invs2_List/;Length[invs2]===3, OptionsPattern[]]/; FCPatternFreeQ[{invs1,invs2}] :=
	C0[Sequence@@Flatten[List @@FeynCalc`Package`paveAutoOrder[FeynCalc`Package`paveHold[0, invs1, invs2]][[2 ;; 3]]]]/;
		OptionValue[PaVeAutoOrder] && OptionValue[PaVeAutoReduce];

PaVe[0, invs1_List, invs2_List/;Length[invs2]===4, OptionsPattern[]]/; FCPatternFreeQ[{invs1,invs2}] :=
	D0[Sequence@@Flatten[List @@FeynCalc`Package`paveAutoOrder[FeynCalc`Package`paveHold[0, invs1, invs2]][[2 ;; 3]]]]/;
		OptionValue[PaVeAutoOrder] && OptionValue[PaVeAutoReduce];

(*The number of 0's, i.e. indices of the metric tensors must be even *)
PaVe[0, x: 0..,{moms___},{masses___}, OptionsPattern[]]:=
	(
	Message[PaVe::nonexistent, StringReplace[ToString["pave"[0,x,{moms},{masses}],InputForm],"pave"->"PaVe"]];
	Abort[]
	)/; EvenQ[Length[{x}]] && FCPatternFreeQ[{x,moms,masses}];

(* There are no tensorial 1-point functions *)
PaVe[x: 1..,{},{m_}, OptionsPattern[]] :=
	(
	Message[PaVe::nonexistent, StringReplace[ToString["pave"[x,{},{m}],InputForm],"pave"->"PaVe"]];
	Abort[]
	)/; FCPatternFreeQ[{x,m}];

(* The number of the kinematic invariants depends on the number of the masses. *)
PaVe[i__, kinvs_List, ms_List, OptionsPattern[]] :=
	(
	Message[PaVe::nonexistent, StringReplace[ToString["pave"[i,kinvs,ms],InputForm],"pave"->"PaVe"]];
	Abort[]
	)/; (Length[kinvs]=!=Length[ms] (Length[ms]-1)/2) && FCPatternFreeQ[{i,kinvs,ms}];

(* 	If UV- and IR-divergences are regularized with the same regulator, then
	scaleless n-point functions vanish in DR	*)
PaVe[__,{0...},{0..}, OptionsPattern[]] :=
	0/; !$KeepLogDivergentScalelessIntegrals;

PaVe[inds__,{0},{0,0}, OptionsPattern[]]:=
	0/;MemberQ[bVanishList,{inds}] && FCPatternFreeQ[{inds}];

PaVe[inds__, {0, 0, 0}, {0, 0, 0}, OptionsPattern[]]:=
	0/;MemberQ[cVanishList,{inds}] && FCPatternFreeQ[{inds}];

PaVe[inds__, {0, 0, 0, 0, 0, 0}, {0, 0, 0,0}, OptionsPattern[]]:=
	0/;MemberQ[dVanishList,{inds}] && FCPatternFreeQ[{inds}];

(* Implemented symmetries of PaVe functions *)
PaVe[ids__, invs1_List, invs2_List, opts:OptionsPattern[]]/; FCPatternFreeQ[{invs1,invs2}] :=
	PaVe[Sequence@@(FeynCalc`Package`paveAutoOrder[FeynCalc`Package`paveHold[ids,invs1,invs2]]),opts]/;
		OptionValue[PaVeAutoOrder] &&
		FeynCalc`Package`paveAutoOrder[FeynCalc`Package`paveHold[ids,invs1,invs2]]=!=FeynCalc`Package`paveHold[ids,invs1,invs2];

(* Universal typesetting for PaVe functions *)
PaVe /:
	MakeBoxes[PaVe[ij__,{moms___},{masses__}, OptionsPattern[]], TraditionalForm]:=
	ToBoxes[Subscript[FromCharacterCode[64+Length[{masses}]], StringJoin[ToString /@ {ij}]
		][moms, masses],TraditionalForm
	]/; Length[{masses}]>=1;


(*
	If different UV and IR reguators are used, not every scaleless function vanishes.
	Only those vanish, whose UV-part is not dimensionless
*)

(* B functions that always vanish when scaleless (up to rank 10); C.f. appendix of arXiv:hep-ph/0609282

or use

liB = Join[{{0}}, (Table[Sequence @@ Union[Sort /@ Tuples[{0, 1}, {i}]], {i, 1, 10}] /.
List[a__ /; FreeQ[f[a], List]] /; OddQ[Count[{a}, 0]] :> Unevaluated[Sequence[]])];
{PaXEvaluateUVIRSplit[ PaVe[Sequence @@ #, {0}, {0, 0}, PaVeAutoReduce -> False]], #} & /@ liB;
resB = Cases[%, {0, _List}];
resB /. {0, a_List} :> a

 *)

bVanishList = {{0, 0}, {0, 0, 1}, {0, 0, 0, 0}, {0, 0, 1, 1}, {0, 0, 0, 0, 1}, {0,
0, 1, 1, 1}, {0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 1, 1}, {0, 0, 1, 1, 1,
1}, {0, 0, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 1, 1, 1}, {0, 0, 1, 1, 1,
1, 1}, {0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 1}, {0, 0, 0,
0, 1, 1, 1, 1}, {0, 0, 1, 1, 1, 1, 1, 1}, {0, 0, 0, 0, 0, 0, 0, 0,
1}, {0, 0, 0, 0, 0, 0, 1, 1, 1}, {0, 0, 0, 0, 1, 1, 1, 1, 1}, {0, 0,
1, 1, 1, 1, 1, 1, 1}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0,
0, 0, 0, 0, 1, 1}, {0, 0, 0, 0, 0, 0, 1, 1, 1, 1}, {0, 0, 0, 0, 1,
1, 1, 1, 1, 1}, {0, 0, 1, 1, 1, 1, 1, 1, 1, 1}};

(* C functions that always vanish when scaleless (up to rank 8); C.f. appendix of arXiv:hep-ph/0609282

or use

liC = Join[{{0}}, (Table[Sequence @@ Union[Sort /@ Tuples[{0, 1, 2}, {i}]], {i, 1, 8}] /.
List[a__ /; FreeQ[f[a], List]] /; OddQ[Count[{a}, 0]] :> Unevaluated[Sequence[]])];
{PaXEvaluateUVIRSplit[PaVe[Sequence @@ #, {0, 0, 0}, {0, 0, 0}, PaVeAutoReduce -> False],
PaXAnalytic -> True], #} & /@ liC;
resC = Cases[%, {0, _List}];
resC /. {0, a_List} :> a

 *)

cVanishList = {{0}, {1}, {2}, {1, 1}, {1, 2}, {2, 2}, {1, 1, 1}, {1, 1, 2}, {1, 2,
2}, {2, 2, 2}, {0, 0, 0, 0}, {1, 1, 1, 1}, {1, 1, 1, 2}, {1, 1, 2,
2}, {1, 2, 2, 2}, {2, 2, 2, 2}, {0, 0, 0, 0, 1}, {0, 0, 0, 0,
2}, {1, 1, 1, 1, 1}, {1, 1, 1, 1, 2}, {1, 1, 1, 2, 2}, {1, 1, 2, 2,
2}, {1, 2, 2, 2, 2}, {2, 2, 2, 2, 2}, {0, 0, 0, 0, 0, 0}, {0, 0, 0,
0, 1, 1}, {0, 0, 0, 0, 1, 2}, {0, 0, 0, 0, 2, 2}, {1, 1, 1, 1, 1,
1}, {1, 1, 1, 1, 1, 2}, {1, 1, 1, 1, 2, 2}, {1, 1, 1, 2, 2, 2}, {1,
1, 2, 2, 2, 2}, {1, 2, 2, 2, 2, 2}, {2, 2, 2, 2, 2, 2}, {0, 0, 0, 0,
0, 0, 1}, {0, 0, 0, 0, 0, 0, 2}, {0, 0, 0, 0, 1, 1, 1}, {0, 0, 0,
0, 1, 1, 2}, {0, 0, 0, 0, 1, 2, 2}, {0, 0, 0, 0, 2, 2, 2}, {1, 1, 1,
1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 2}, {1, 1, 1, 1, 1, 2, 2}, {1, 1,
1, 1, 2, 2, 2}, {1, 1, 1, 2, 2, 2, 2}, {1, 1, 2, 2, 2, 2, 2}, {1, 2,
2, 2, 2, 2, 2}, {2, 2, 2, 2, 2, 2, 2}, {0, 0, 0, 0, 0, 0, 0,
0}, {0, 0, 0, 0, 0, 0, 1, 1}, {0, 0, 0, 0, 0, 0, 1, 2}, {0, 0, 0, 0,
0, 0, 2, 2}, {0, 0, 0, 0, 1, 1, 1, 1}, {0, 0, 0, 0, 1, 1, 1,
2}, {0, 0, 0, 0, 1, 1, 2, 2}, {0, 0, 0, 0, 1, 2, 2, 2}, {0, 0, 0, 0,
2, 2, 2, 2}, {1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1,
2}, {1, 1, 1, 1, 1, 1, 2, 2}, {1, 1, 1, 1, 1, 2, 2, 2}, {1, 1, 1, 1,
2, 2, 2, 2}, {1, 1, 1, 2, 2, 2, 2, 2}, {1, 1, 2, 2, 2, 2, 2,
2}, {1, 2, 2, 2, 2, 2, 2, 2}, {2, 2, 2, 2, 2, 2, 2, 2}};

(* D functions that always vanish when scaleless (up to rank 8); C.f. appendix of arXiv:hep-ph/0609282

or use

liD = Join[{{0}}, (Table[Sequence @@ Union[Sort /@ Tuples[{0, 1, 2, 3}, {i}]], {i, 1, 8}] /.
List[a__ /; FreeQ[f[a], List]] /; OddQ[Count[{a}, 0]] :> Unevaluated[Sequence[]])];
{PaXEvaluateUVIRSplit[PaVe[Sequence @@ #, {0, 0, 0, 0, 0, 0}, {0, 0, 0, 0}, PaVeAutoReduce -> False],
PaXAnalytic -> True], #} & /@ liD;
resD = Cases[%, {0, _List}];
resD /. {0, a_List} :> a

 *)

dVanishList = {{0}, {1}, {2}, {3}, {0, 0}, {1, 1}, {1, 2}, {1, 3}, {2, 2}, {2,
3}, {3, 3}, {0, 0, 1}, {0, 0, 2}, {0, 0, 3}, {1, 1, 1}, {1, 1,
2}, {1, 1, 3}, {1, 2, 2}, {1, 2, 3}, {1, 3, 3}, {2, 2, 2}, {2, 2,
3}, {2, 3, 3}, {3, 3, 3}, {0, 0, 1, 1}, {0, 0, 1, 2}, {0, 0, 1,
3}, {0, 0, 2, 2}, {0, 0, 2, 3}, {0, 0, 3, 3}, {1, 1, 1, 1}, {1, 1,
1, 2}, {1, 1, 1, 3}, {1, 1, 2, 2}, {1, 1, 2, 3}, {1, 1, 3, 3}, {1,
2, 2, 2}, {1, 2, 2, 3}, {1, 2, 3, 3}, {1, 3, 3, 3}, {2, 2, 2,
2}, {2, 2, 2, 3}, {2, 2, 3, 3}, {2, 3, 3, 3}, {3, 3, 3, 3}, {0, 0,
1, 1, 1}, {0, 0, 1, 1, 2}, {0, 0, 1, 1, 3}, {0, 0, 1, 2, 2}, {0, 0,
1, 2, 3}, {0, 0, 1, 3, 3}, {0, 0, 2, 2, 2}, {0, 0, 2, 2, 3}, {0, 0,
2, 3, 3}, {0, 0, 3, 3, 3}, {1, 1, 1, 1, 1}, {1, 1, 1, 1, 2}, {1, 1,
1, 1, 3}, {1, 1, 1, 2, 2}, {1, 1, 1, 2, 3}, {1, 1, 1, 3, 3}, {1, 1,
2, 2, 2}, {1, 1, 2, 2, 3}, {1, 1, 2, 3, 3}, {1, 1, 3, 3, 3}, {1, 2,
2, 2, 2}, {1, 2, 2, 2, 3}, {1, 2, 2, 3, 3}, {1, 2, 3, 3, 3}, {1, 3,
3, 3, 3}, {2, 2, 2, 2, 2}, {2, 2, 2, 2, 3}, {2, 2, 2, 3, 3}, {2, 2,
3, 3, 3}, {2, 3, 3, 3, 3}, {3, 3, 3, 3, 3}, {0, 0, 0, 0, 0, 0}, {0,
0, 1, 1, 1, 1}, {0, 0, 1, 1, 1, 2}, {0, 0, 1, 1, 1, 3}, {0, 0, 1, 1,
2, 2}, {0, 0, 1, 1, 2, 3}, {0, 0, 1, 1, 3, 3}, {0, 0, 1, 2, 2,
2}, {0, 0, 1, 2, 2, 3}, {0, 0, 1, 2, 3, 3}, {0, 0, 1, 3, 3, 3}, {0,
0, 2, 2, 2, 2}, {0, 0, 2, 2, 2, 3}, {0, 0, 2, 2, 3, 3}, {0, 0, 2, 3,
3, 3}, {0, 0, 3, 3, 3, 3}, {1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1,
2}, {1, 1, 1, 1, 1, 3}, {1, 1, 1, 1, 2, 2}, {1, 1, 1, 1, 2, 3}, {1,
1, 1, 1, 3, 3}, {1, 1, 1, 2, 2, 2}, {1, 1, 1, 2, 2, 3}, {1, 1, 1, 2,
3, 3}, {1, 1, 1, 3, 3, 3}, {1, 1, 2, 2, 2, 2}, {1, 1, 2, 2, 2,
3}, {1, 1, 2, 2, 3, 3}, {1, 1, 2, 3, 3, 3}, {1, 1, 3, 3, 3, 3}, {1,
2, 2, 2, 2, 2}, {1, 2, 2, 2, 2, 3}, {1, 2, 2, 2, 3, 3}, {1, 2, 2, 3,
3, 3}, {1, 2, 3, 3, 3, 3}, {1, 3, 3, 3, 3, 3}, {2, 2, 2, 2, 2,
2}, {2, 2, 2, 2, 2, 3}, {2, 2, 2, 2, 3, 3}, {2, 2, 2, 3, 3, 3}, {2,
2, 3, 3, 3, 3}, {2, 3, 3, 3, 3, 3}, {3, 3, 3, 3, 3, 3}, {0, 0, 0, 0,
0, 0, 1}, {0, 0, 0, 0, 0, 0, 2}, {0, 0, 0, 0, 0, 0, 3}, {0, 0, 1,
1, 1, 1, 1}, {0, 0, 1, 1, 1, 1, 2}, {0, 0, 1, 1, 1, 1, 3}, {0, 0, 1,
1, 1, 2, 2}, {0, 0, 1, 1, 1, 2, 3}, {0, 0, 1, 1, 1, 3, 3}, {0, 0,
1, 1, 2, 2, 2}, {0, 0, 1, 1, 2, 2, 3}, {0, 0, 1, 1, 2, 3, 3}, {0, 0,
1, 1, 3, 3, 3}, {0, 0, 1, 2, 2, 2, 2}, {0, 0, 1, 2, 2, 2, 3}, {0,
0, 1, 2, 2, 3, 3}, {0, 0, 1, 2, 3, 3, 3}, {0, 0, 1, 3, 3, 3, 3}, {0,
0, 2, 2, 2, 2, 2}, {0, 0, 2, 2, 2, 2, 3}, {0, 0, 2, 2, 2, 3,
3}, {0, 0, 2, 2, 3, 3, 3}, {0, 0, 2, 3, 3, 3, 3}, {0, 0, 3, 3, 3, 3,
3}, {1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 2}, {1, 1, 1, 1, 1,
1, 3}, {1, 1, 1, 1, 1, 2, 2}, {1, 1, 1, 1, 1, 2, 3}, {1, 1, 1, 1, 1,
3, 3}, {1, 1, 1, 1, 2, 2, 2}, {1, 1, 1, 1, 2, 2, 3}, {1, 1, 1, 1,
2, 3, 3}, {1, 1, 1, 1, 3, 3, 3}, {1, 1, 1, 2, 2, 2, 2}, {1, 1, 1, 2,
2, 2, 3}, {1, 1, 1, 2, 2, 3, 3}, {1, 1, 1, 2, 3, 3, 3}, {1, 1, 1,
3, 3, 3, 3}, {1, 1, 2, 2, 2, 2, 2}, {1, 1, 2, 2, 2, 2, 3}, {1, 1, 2,
2, 2, 3, 3}, {1, 1, 2, 2, 3, 3, 3}, {1, 1, 2, 3, 3, 3, 3}, {1, 1,
3, 3, 3, 3, 3}, {1, 2, 2, 2, 2, 2, 2}, {1, 2, 2, 2, 2, 2, 3}, {1, 2,
2, 2, 2, 3, 3}, {1, 2, 2, 2, 3, 3, 3}, {1, 2, 2, 3, 3, 3, 3}, {1,
2, 3, 3, 3, 3, 3}, {1, 3, 3, 3, 3, 3, 3}, {2, 2, 2, 2, 2, 2, 2}, {2,
2, 2, 2, 2, 2, 3}, {2, 2, 2, 2, 2, 3, 3}, {2, 2, 2, 2, 3, 3,
3}, {2, 2, 2, 3, 3, 3, 3}, {2, 2, 3, 3, 3, 3, 3}, {2, 3, 3, 3, 3, 3,
3}, {3, 3, 3, 3, 3, 3, 3}, {0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0,
0, 0, 1, 1}, {0, 0, 0, 0, 0, 0, 1, 2}, {0, 0, 0, 0, 0, 0, 1, 3}, {0,
0, 0, 0, 0, 0, 2, 2}, {0, 0, 0, 0, 0, 0, 2, 3}, {0, 0, 0, 0, 0, 0,
3, 3}, {0, 0, 1, 1, 1, 1, 1, 1}, {0, 0, 1, 1, 1, 1, 1, 2}, {0, 0, 1,
1, 1, 1, 1, 3}, {0, 0, 1, 1, 1, 1, 2, 2}, {0, 0, 1, 1, 1, 1, 2,
3}, {0, 0, 1, 1, 1, 1, 3, 3}, {0, 0, 1, 1, 1, 2, 2, 2}, {0, 0, 1, 1,
1, 2, 2, 3}, {0, 0, 1, 1, 1, 2, 3, 3}, {0, 0, 1, 1, 1, 3, 3,
3}, {0, 0, 1, 1, 2, 2, 2, 2}, {0, 0, 1, 1, 2, 2, 2, 3}, {0, 0, 1, 1,
2, 2, 3, 3}, {0, 0, 1, 1, 2, 3, 3, 3}, {0, 0, 1, 1, 3, 3, 3,
3}, {0, 0, 1, 2, 2, 2, 2, 2}, {0, 0, 1, 2, 2, 2, 2, 3}, {0, 0, 1, 2,
2, 2, 3, 3}, {0, 0, 1, 2, 2, 3, 3, 3}, {0, 0, 1, 2, 3, 3, 3,
3}, {0, 0, 1, 3, 3, 3, 3, 3}, {0, 0, 2, 2, 2, 2, 2, 2}, {0, 0, 2, 2,
2, 2, 2, 3}, {0, 0, 2, 2, 2, 2, 3, 3}, {0, 0, 2, 2, 2, 3, 3,
3}, {0, 0, 2, 2, 3, 3, 3, 3}, {0, 0, 2, 3, 3, 3, 3, 3}, {0, 0, 3, 3,
3, 3, 3, 3}, {1, 1, 1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1, 1,
2}, {1, 1, 1, 1, 1, 1, 1, 3}, {1, 1, 1, 1, 1, 1, 2, 2}, {1, 1, 1, 1,
1, 1, 2, 3}, {1, 1, 1, 1, 1, 1, 3, 3}, {1, 1, 1, 1, 1, 2, 2,
2}, {1, 1, 1, 1, 1, 2, 2, 3}, {1, 1, 1, 1, 1, 2, 3, 3}, {1, 1, 1, 1,
1, 3, 3, 3}, {1, 1, 1, 1, 2, 2, 2, 2}, {1, 1, 1, 1, 2, 2, 2,
3}, {1, 1, 1, 1, 2, 2, 3, 3}, {1, 1, 1, 1, 2, 3, 3, 3}, {1, 1, 1, 1,
3, 3, 3, 3}, {1, 1, 1, 2, 2, 2, 2, 2}, {1, 1, 1, 2, 2, 2, 2,
3}, {1, 1, 1, 2, 2, 2, 3, 3}, {1, 1, 1, 2, 2, 3, 3, 3}, {1, 1, 1, 2,
3, 3, 3, 3}, {1, 1, 1, 3, 3, 3, 3, 3}, {1, 1, 2, 2, 2, 2, 2,
2}, {1, 1, 2, 2, 2, 2, 2, 3}, {1, 1, 2, 2, 2, 2, 3, 3}, {1, 1, 2, 2,
2, 3, 3, 3}, {1, 1, 2, 2, 3, 3, 3, 3}, {1, 1, 2, 3, 3, 3, 3,
3}, {1, 1, 3, 3, 3, 3, 3, 3}, {1, 2, 2, 2, 2, 2, 2, 2}, {1, 2, 2, 2,
2, 2, 2, 3}, {1, 2, 2, 2, 2, 2, 3, 3}, {1, 2, 2, 2, 2, 3, 3,
3}, {1, 2, 2, 2, 3, 3, 3, 3}, {1, 2, 2, 3, 3, 3, 3, 3}, {1, 2, 3, 3,
3, 3, 3, 3}, {1, 3, 3, 3, 3, 3, 3, 3}, {2, 2, 2, 2, 2, 2, 2,
2}, {2, 2, 2, 2, 2, 2, 2, 3}, {2, 2, 2, 2, 2, 2, 3, 3}, {2, 2, 2, 2,
2, 3, 3, 3}, {2, 2, 2, 2, 3, 3, 3, 3}, {2, 2, 2, 3, 3, 3, 3,
3}, {2, 2, 3, 3, 3, 3, 3, 3}, {2, 3, 3, 3, 3, 3, 3, 3}, {3, 3, 3, 3,
3, 3, 3, 3}};


FCPrint[1,"PaVe.m loaded."];
End[]
