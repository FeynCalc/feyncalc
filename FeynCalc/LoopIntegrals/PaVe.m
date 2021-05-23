(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PaVe *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: scalar integral *)

(* ------------------------------------------------------------------------ *)

GenPaVe::usage =
"GenPaVe[i,j,..., {{0,m0},{Momentum[p1],m1},{Momentum[p2],m2},...] denotes the invariant \
(and scalar) Passarino-Veltman integrals, i.e. the coefficient functions of \
the tensor integral decomposition. In contrast to PaVe which uses the LoopTools \
convention,  masses and external momenta in GenPaVe are written in the same order as \
they appear in the original tensor integral, i.e. FAD[{q,m0},{q-p1,m1},{q-p2,m2},...].";

PaVe::usage =
"PaVe[ i,j,... {p10,p12,...},{m1^2, mw^2, ...} ] denotes the invariant \
(and scalar) Passarino-Veltman integrals, i.e. the coefficient functions of \
the tensor integral decomposition.  Joining plist and mlist gives the same \
conventions as for A0, B0, C0, D0.  Automatic simlifications are \
performed for the coefficient functions of two-point integrals and \
for the scalar integrals.";

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

(*The number of 0's, i.e. indices of the metric tensors must be even *)
PaVe[0, x: 0..,{moms___},{masses___}, OptionsPattern[]]:=
	(
	Message[PaVe::nonexistent, StringReplace[ToString["pave"[0,x,{moms},{masses}],InputForm],"pave"->"PaVe"]];
	Abort[]
	)/; EvenQ[Length[{x}]] && FCPatternFreeQ[{x,moms,masses}];

(* there are no tensorial 1-point function *)
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
	0/; !$KeepLogDivergentScalelessIntegrals

PaVe[inds__,{0},{0,0}, OptionsPattern[]]:=
	0/;MemberQ[bVanishList,{inds}] && FCPatternFreeQ[{inds}];

PaVe[inds__, {0, 0, 0}, {0, 0, 0}, OptionsPattern[]]:=
	0/;MemberQ[cVanishList,{inds}] && FCPatternFreeQ[{inds}];

PaVe[inds__, {0, 0, 0, 0, 0, 0}, {0, 0, 0,0}, OptionsPattern[]]:=
	0/;MemberQ[dVanishList,{inds}] && FCPatternFreeQ[{inds}];

(* Argument ordering for B0 *)
PaVe[0,{pp_},{mm1_,mm2_}, opts:OptionsPattern[]] :=
	PaVe[0,{pp},{mm2,mm1},opts]/; !OrderedQ[{mm1,mm2}] && OptionValue[PaVeAutoOrder] && FCPatternFreeQ[{pp,mm1,mm2}];



(* ****************************************************************** *)
(* Notation :   p10 = p1^2;  p12 = (p1-p2)^2;  etc.                   *)
(* ****************************************************************** *)
(* C2 --> C1, C22 --> C11,  C002 --> C001, C222 --> C111,   *)
(* if p10=p20  and  m2=m3    *)
PaVe[2,{p10_,p12_,p10_},{m1_,m2_,m2_}, opts:OptionsPattern[]] :=
	PaVe[1,{p10,p12,p10},{m1,m2,m2},opts]/; OptionValue[PaVeAutoOrder] && FCPatternFreeQ[{p10, p12, m1, m2}];

PaVe[2,2,{p10_,p12_,p10_},{m1_,m2_,m2_}, opts:OptionsPattern[]] :=
	PaVe[1,1,{p10,p12,p10},{m1,m2,m2},opts]/; OptionValue[PaVeAutoOrder] && FCPatternFreeQ[{p10, p12, m1, m2}];

PaVe[0,0,2,{p10_,p12_,p10_},{m1_,m2_,m2_}, opts:OptionsPattern[]] :=
	PaVe[0,0,1,{p10,p12,p10},{m1,m2,m2},opts]/; OptionValue[PaVeAutoOrder] && FCPatternFreeQ[{p10, p12, m1, m2}];

PaVe[1,2,2,{p10_,p12_,p10_},{m1_,m2_,m2_}, opts:OptionsPattern[]] :=
	PaVe[1,1,2,{p10,p12,p10},{m1,m2,m2},opts]/; OptionValue[PaVeAutoOrder] && FCPatternFreeQ[{p10, p12, m1, m2}];

PaVe[2,2,2,{p10_,p12_,p10_},{m1_,m2_,m2_}, opts:OptionsPattern[]] :=
	PaVe[1,1,1,{p10,p12,p10},{m1,m2,m2},opts]/; OptionValue[PaVeAutoOrder] && FCPatternFreeQ[{p10, p12, m1, m2}];

(* a special case *)
PaVe[ 2,{p10_, pp_,pp_},{m_,m_,m2_}, opts:OptionsPattern[]] :=
	(- 2 PaVe[1,{p10,pp,pp},{m,m,m2},opts] - PaVe[0,{p10,pp,pp},{m,m,m2},opts])/;
	OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{p10, pp, m, m2}];

(* *********************************************************************** *)
(*  D's: The argument list is (in general) : p10, p12, p23, p30, p20, p13  *)
(* *********************************************************************** *)

(*  1 <---> 2;   p20=p10,  p23=p13 , m3 = m2  *)
PaVe[x__,{p10_,p12_,p13_,p30_,p10_,p13_},{m1_,m2_,m2_,m4_}, opts:OptionsPattern[]] :=
	PaVe[Sequence@@({x} /. {1:>2, 2:>1}), {p10,p12,p13,p30,p10,p13},{m1,m2,m2,m4}, opts]/;
	(Count[{x}, 2] > Count[{x}, 1]) && OptionValue[PaVeAutoOrder] && FCPatternFreeQ[{p10, p12, p13, p30, m1, m2, m4}];

(*  1 <---> 3;   p10=p30,  p12=p23 , m2 = m4  *)
PaVe[x__,{p10_,p12_,p12_,p10_,p20_,p13_},{m1_,m2_,m3_,m2_}, opts:OptionsPattern[]] :=
	PaVe[Sequence@@({x} /. {1:>3, 3:>1}), {p10,p12,p12,p10,p20,p13},{m1,m2,m3,m2}, opts]/;
	(Count[{x}, 3] > Count[{x}, 1]) && OptionValue[PaVeAutoOrder] && FCPatternFreeQ[{p10, p12, p13, p20, m1, m2, m3}];

(*  2 <---> 3;   p30=p20,  p13=p12 , m3 = m4  *)
PaVe[x__,{p10_,p12_,p23_,p20_,p20_,p12_},{m1_,m2_,m3_,m3_}, opts:OptionsPattern[]] :=
	PaVe[Sequence@@({x} /. {2:>3, 3:>2}), {p10,p12,p23,p20,p20,p12},{m1,m2,m3,m3},opts]/;
		(Count[{x}, 3] > Count[{x}, 2]) && OptionValue[PaVeAutoOrder] && FCPatternFreeQ[{p10, p12, p23, p20, m1, m2, m3}];

(* Argument ordering for 3-point functions *)

(*Scalar and 00.. coefficient functions*)
PaVe[id: 0.., {p10_, p12_, p20_}, {m1_, m2_, m3_}, opts:OptionsPattern[]] :=
	PaVe[id, Sequence@@FeynCalc`Package`argPermC[p10,p12,p20,m1,m2,m3],opts]/; OptionValue[PaVeAutoOrder] &&
		!OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{p10, p12, p20, m1, m2, m3}] && {{p10,p12,p20},{m1,m2,m3}}=!=FeynCalc`Package`argPermC[p10,p12,p20,m1,m2,m3];

PaVe[0, {p10_, p12_, p20_}, {m1_, m2_, m3_}, OptionsPattern[]] :=
	C0@@Flatten[FeynCalc`Package`argPermC[p10,p12,p20,m1,m2,m3]]/; OptionValue[PaVeAutoOrder] && OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{p10, p12, p20, m1, m2, m3}];

(* Argument ordering for 4-point functions *)

(*Scalar and 00.. coefficient functions*)
PaVe[id: 0.., {p10_, p12_, p23_, p30_, p13_, p20_}, {m1_, m2_, m3_, m4_}, opts:OptionsPattern[]] :=
	PaVe[id, Sequence@@FeynCalc`Package`argPermD[p10, p12, p23, p30, p13, p20, m1, m2, m3, m4],opts]/; OptionValue[PaVeAutoOrder] &&
		!OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{p10, p12, p23, p30, p13, p20, m1, m2, m3, m4}] &&
			{{p10, p12, p23, p30, p13, p20},{m1,m2,m3,m4}}=!=FeynCalc`Package`argPermD[p10, p12, p23, p30, p13, p20, m1, m2, m3, m4];

PaVe[0, {p10_, p12_, p23_, p30_, p13_, p20_}, {m1_, m2_, m3_, m4_}, OptionsPattern[]] :=
	D0@@Flatten[FeynCalc`Package`argPermD[p10, p12, p23, p30, p13, p20, m1, m2, m3, m4]]/; OptionValue[PaVeAutoOrder] && OptionValue[PaVeAutoReduce] &&
	FCPatternFreeQ[{p10, p12, p23, p30, p13, p20, m1, m2, m3, m4}];

(* Argument ordering for 5-point functions *)

(*Scalar and 00.. coefficient functions*)
PaVe[id: 0.., {p10_, p12_, p23_, p34_, p40_, p20_, p13_, p24_, p30_, p14_}, {m0_, m1_, m2_, m3_, m4_}, opts:OptionsPattern[]] :=
	PaVe[id, Sequence@@FeynCalc`Package`argPermE[p10, p12, p23, p34, p40, p20, p13, p24, p30, p14, m0, m1, m2, m3, m4],opts]/; OptionValue[PaVeAutoOrder] &&
		!OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{p10, p12, p23, p34, p40, p20, p13, p24, p30, p14, m0, m1, m2, m3, m4}] &&
			{{p10, p12, p23, p34, p40, p20, p13, p24, p30, p14},{m0,m1,m2,m3,m4}}=!=FeynCalc`Package`argPermE[p10, p12, p23, p34, p40, p20, p13, p24, p30, p14, m0, m1, m2, m3, m4];


(* Argument ordering for 6-point functions *)

(*Scalar and 00.. coefficient functions*)
PaVe[id: 0.., {p10_, p12_, p23_, p34_, p45_, p50_, p20_, p13_, p24_, p35_, p40_, p15_, p30_, p14_, p25_}, {m0_, m1_, m2_, m3_, m4_ ,m5_}, opts:OptionsPattern[]] :=
	PaVe[id, Sequence@@FeynCalc`Package`argPermF[p10, p12, p23, p34, p45, p50, p20, p13, p24, p35, p40, p15, p30, p14, p25, m0, m1, m2, m3, m4, m5],opts]/; OptionValue[PaVeAutoOrder] &&
		!OptionValue[PaVeAutoReduce] && FCPatternFreeQ[{p10, p12, p23, p34, p45, p50, p20, p13, p24, p35, p40, p15, p30, p14, p25, m0, m1, m2, m3, m4, m5}] &&
			{{p10, p12, p23, p34, p45, p50, p20, p13, p24, p35, p40, p15, p30, p14, p25},{m0, m1, m2, m3, m4, m5}}=!=FeynCalc`Package`argPermF[p10, p12, p23, p34, p45, p50, p20, p13, p24, p35, p40, p15, p30, p14, p25, m0, m1, m2, m3, m4, m5];

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

(* B functions that always vanish when scaleless; C.f. appendix of arXiv:hep-ph/0609282

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

(* C functions that always vanish when scaleless; C.f. appendix of arXiv:hep-ph/0609282

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

(* D functions that always vanish when scaleless; C.f. appendix of arXiv:hep-ph/0609282

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
