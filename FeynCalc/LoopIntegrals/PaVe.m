(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PaVe *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: scalar integral *)

(* ------------------------------------------------------------------------ *)

GenPaVe::usage =
"PaVe[ i,j,..., {{0,m0},{Momentum[p1],m1},{Momentum[p2],m2},...} denotes the invariant \
(and scalar) Passarino-Veltman integrals, i.e. the coefficient functions of \
the tensor integral decomposition. In contrast to PaVe which uses the LoopTools
convention,  masses and external momenta in GenPaVe are written in the same order as
they appear in the original tensor integral, i.e. FAD[{q,m0},{q-p1,m1},{q-p2,m2},...].";

PaVe::usage =
"PaVe[ i,j,... {p10,p12,...},{m1^2, mw^2, ...} ] denotes the invariant
(and scalar)
Passarino-Veltman integrals, i.e. the coefficient functions of
the tensor integral decomposition.  Joining plist and mlist gives the same
conventions as for A0, B0, C0, D0.  Automatic simlifications are
performed for the coefficient functions of two-point integrals and
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
	PaVeAutoReduce -> True
};

(* Symmetry in the indices *)
PaVe[i_,j__,  pl_List, ml_List, opts:OptionsPattern[]] :=
	PaVe @@Join[Sort[{i,j}],{pl,ml},opts]/;!OrderedQ[{i,j}];

(* Special cases of PaVe: *)
PaVe[0, {}, {x_}, OptionsPattern[]] :=
	A0[x]/; OptionValue[PaVeAutoReduce];

PaVe[0, {p2_}, {x_,y_}, OptionsPattern[]] :=
	B0[p2,x,y]/; OptionValue[PaVeAutoReduce];

PaVe[1,{pp_},{mm1_,mm2_}, OptionsPattern[]] :=
	B1[pp, mm1, mm2]/; OptionValue[PaVeAutoReduce];

(*The number of 0's, i.e. indices of the metric tensors must be even *)
PaVe[0, x: 0..,{moms___},{masses___}, OptionsPattern[]]:=
	(Message[PaVe::nonexistent, "PaVe[" <> (ToString[{0,x}]//StringReplace[#, {"{" | "}" -> ""}] &)
		<> ", " <> ToString[{moms}] <> ", " <> ToString[{masses}] <>"]"];
	Abort[];)/; EvenQ[Length[{x}]];

(* there are no tensorial 1-point function *)
PaVe[x: 1..,{},{m_}, OptionsPattern[]] :=
	(Message[PaVe::nonexistent, "PaVe[" <> (ToString[{x}]//StringReplace[#, {"{" | "}" -> ""}] &)
		<> ", {}, " <> ToString[{m}] <> "]"];
	Abort[];)

(* scaleless n-point functions vanish in DR	*)
PaVe[__,{0..},{0..}, OptionsPattern[]] :=
	0;

(* but a non-zero coefficient of g_munu *)
PaVe[0,0,{},{m2_}, OptionsPattern[]] :=
	(m2/4 A0[m2] + m2^2/8) /; $LimitTo4 && OptionValue[PaVeAutoReduce];

(* but a non-zero coefficient of g_munu *)
PaVe[0,0,{},{m2_}, OptionsPattern[]] :=
	(m2/D A0[m2]) /; $LimitTo4  && OptionValue[PaVeAutoReduce];

PaVe[0, {p_}, {m1_, m2_}, OptionsPattern[]] :=
	B0[p, m1, m2]/; OptionValue[PaVeAutoReduce];

PaVe[0,0,{p_},{m1_,m2_}, OptionsPattern[]] :=
	B00[p,m1,m2]/; $LimitTo4 && OptionValue[PaVeAutoReduce];

PaVe[1,1,{pp_},{mm1_,mm2_}, OptionsPattern[]] :=
	B11[pp,mm1,mm2]/; OptionValue[PaVeAutoReduce];

(* ****************************************************************** *)
(* Notation :   p10 = p1^2;  p12 = (p1-p2)^2;  etc.                   *)
(* ****************************************************************** *)
(* C2 --> C1, C22 --> C11,  C002 --> C001, C222 --> C111,   *)
(* if p10=p20  and  m2=m3    *)
PaVe[2,{p10_,p12_,p10_},{m1_,m2_,m2_}, opts:OptionsPattern[]] :=
	PaVe[1,{p10,p12,p10},{m1,m2,m2},opts]/; OptionValue[PaVeAutoOrder];

PaVe[2,2,{p10_,p12_,p10_},{m1_,m2_,m2_}, opts:OptionsPattern[]] :=
	PaVe[1,1,{p10,p12,p10},{m1,m2,m2},opts]/; OptionValue[PaVeAutoOrder];

PaVe[0,0,2,{p10_,p12_,p10_},{m1_,m2_,m2_}, opts:OptionsPattern[]] :=
	PaVe[0,0,1,{p10,p12,p10},{m1,m2,m2},opts]/; OptionValue[PaVeAutoOrder];

PaVe[1,2,2,{p10_,p12_,p10_},{m1_,m2_,m2_}, opts:OptionsPattern[]] :=
	PaVe[1,1,2,{p10,p12,p10},{m1,m2,m2},opts]/; OptionValue[PaVeAutoOrder];

PaVe[2,2,2,{p10_,p12_,p10_},{m1_,m2_,m2_}, opts:OptionsPattern[]] :=
	PaVe[1,1,1,{p10,p12,p10},{m1,m2,m2},opts]/; OptionValue[PaVeAutoOrder];
(* a special case *)
PaVe[ 2,{p10_, pp_,pp_},{m_,m_,m2_}, opts:OptionsPattern[]] :=
	(- 2 PaVe[1,{p10,pp,pp},{m,m,m2},opts] - PaVe[0,{p10,pp,pp},{m,m,m2},opts])/;
	OptionValue[PaVeAutoReduce];

(* *********************************************************************** *)
(*  D's: The argument list is (in general) : p10, p12, p23, p30, p20, p13  *)
(* *********************************************************************** *)
pav[{a__},pl_List,ml_List, opts:OptionsPattern[]] :=
	PaVe[a,pl,ml, opts];
(*  1 <---> 2;   p20=p10,  p23=p13 , m3 = m2  *)
PaVe[x__,{p10_,p12_,p13_,p30_,p10_,p13_},{m1_,m2_,m2_,m4_}, opts:OptionsPattern[]] :=
	pav[{x} /. {1:>2, 2:>1}, {p10,p12,p13,p30,p10,p13},{m1,m2,m2,m4}, opts]/;
	(Count[{x}, 2] > Count[{x}, 1]) && OptionValue[PaVeAutoOrder];

(*  1 <---> 3;   p10=p30,  p12=p23 , m2 = m4  *)
PaVe[x__,{p10_,p12_,p12_,p10_,p20_,p13_},{m1_,m2_,m3_,m2_}, opts:OptionsPattern[]] :=
	pav[{x} /. {1:>3, 3:>1}, {p10,p12,p12,p10,p20,p13},{m1,m2,m3,m2}, opts]/;
	(Count[{x}, 3] > Count[{x}, 1]) && OptionValue[PaVeAutoOrder];

(*  2 <---> 3;   p30=p20,  p13=p12 , m3 = m4  *)
PaVe[x__,{p10_,p12_,p23_,p20_,p20_,p12_},{m1_,m2_,m3_,m3_}, opts:OptionsPattern[]] :=
	pav[{x} /. {2:>3, 3:>2}, {p10,p12,p23,p20,p20,p12},{m1,m2,m3,m3},opts]/;
		(Count[{x}, 3] > Count[{x}, 2]) && OptionValue[PaVeAutoOrder];

(* in order to canonize the C0's  (args:   p1^2, (p2-p1)^2, p2^2)  *)
PaVe[0, {p10_, p12_, p20_}, {m1_, m2_, m3_}, OptionsPattern[]] :=
	cord[p10, p12, p20,m1,m2,m3]/;OptionValue[PaVeAutoOrder];

PaVe[0, {p10_, p12_, p23_, p30_, p13_, p20_}, {m1_, m2_, m3_, m4_}, OptionsPattern[]] :=
	D0[p10, p12, p23, p30, p13, p20, m1, m2, m3, m4]/;OptionValue[PaVeAutoOrder];


cord[a_,b_,c_, m1_,m2_,m3_] :=
	C0@@( Sort[{ {a,b,c, m1,m2,m3}, {c,b,a, m1,m3,m2},
								{a,c,b, m2,m1,m3}, {b,c,a, m2,m3,m1},
								{c,a,b, m3,m1,m2}, {b,a,c, m3,m2,m1} } ][[1]] );

	cord[C0[six__],{}] :=
		cord[six];
	cord[C0[te__], argu_List ] :=
		Block[ {int, puref, arg, smalist, six,
												varg, sma, pw},
			six =  {te}/. SmallVariable->sma;
			If[ FreeQ[six, sma],
				arg = argu,
				smalist = Select[Variables[six/.Power->pw],
												(!FreeQ[#, sma])&]/.pw->Power;
				If[ !FreeQ[smalist, Power],
					arg = (argu/.SmallVariable->Identity) /.
								Map[(#[[1,1]] -> (#[[1]]) )&, smalist ],
					arg = argu/.SmallVariable->sma
				];
			];
			varg = Variables[arg];
			For[iv = 1,iv<=Length[varg],iv++,
					If[ (!FreeQ[six, varg[[iv]]^2]) && FreeQ[arg,varg[[iv]]^2],
						arg = arg/.varg[[iv]]->(varg[[iv]]^2)
					];
					];
			puref = func[Apply[or,(stringmatchq[slot[1], #]& /@ tomatch[arg])
												]]/.slot->Slot/.func->Function/.or->Or/.
													stringmatchq->StringMatchQ;
			int = Select[ tostring /@ (oldper@@six),
										func[ stringmatchq[slot[1],tomatch[arg]]
												]/.slot->Slot/.func->Function/.
													stringmatchq->StringMatchQ
													];
			If[ Length[int] === 0,
				int = six,
				int = ToExpression[int[[1]]]
			];
			int/.sma->SmallVariable
		] /; Length[{te}]===6 && Length[argu]>0;


PaVe /:
	MakeBoxes[PaVe[ij___,{moms___},{masses__}, OptionsPattern[]], TraditionalForm]:=
	ToBoxes[Subscript[FromCharacterCode[64+Length[{masses}]], StringJoin[ToString /@ {ij}]
		][moms, masses],TraditionalForm
	]/; Length[{masses}]>=1;

FCPrint[1,"PaVe.m loaded."];
End[]
