(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SharedTools														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Small helper tools extensively used in FeynCalc			    *)

(* ------------------------------------------------------------------------ *)

Cases2::usage=
"Cases2[expr, f] is equivalent to \
Cases[{expr}, HoldPattern[f[___]], Infinity]//Union. \
Cases2[expr, f1, f2, ...] or \
Cases2[expr, {f1, f2, ...}] is equivalent to \
Cases[{expr}, f1[___] | f2[___] ..., Infinity]//Union.";

Combine::usage=
"Combine[expr] puts terms in a sum over a common denominator, and \
cancels factors in the result. Combine is similar to Together, \
but accepts the option Expanding and works usually \
better than Together on polynomials involving rationals with \
sums in the denominator.";

Complement1::usage=
"Complement1[l1, l2], where l1 and l2 are lists returns a list of \
elements from l1 not in l2. Multiple occurences of an element in l1 are \
kept and multiple occurences of an element in l2 are dropped multiply if \
present in l1";

Expand2::usage=
"Expand2[exp, x] expands all sums containing x. \
Expand2[exp, {x1, x2, ...}]  expands all sums containing x1, x2, ....";

ExpandAll2::usage=
"ExpandAll2[exp] is similar to ExpandAll, but much faster on simple structures.";

FCAntiSymmetrize::usage=
"FCAntiSymmetrize[expr, {a1, a2, ...}] antisymmetrizes expr with respect \
to the variables a1, a2, ... ";

FCFactorOut::usage=
"FCFactorOut[exp, pref] factors out pref out of exp. This is often need to \
bring exp into a particular form that Mathematica refuses to give";

FCMakeIndex::usage=
"FCMakeIndex[str1, str2, head] generates an index with the given head out \
of the string str1 and str2. For example, FCMakeIndex[\"Lor\",\"1\",LorentzIndex] \
yields LorentzIndex[Lor1]. The second argument can also be an integer. FCMakeIndex \
is useful for converting the output of different diagram generators such as \
FeynArts or QGAF into the FeynCalc notation. It uses memoization to improve the \
performance."

FCPatternFreeQ::usage =
"FCPatternFreeQ[{expr}] yields True if {expr} does not contain any \
pattern objects, e.g. Pattern, Blank, BlankSequence and BlankNullSequence. \n
FCPatternFreeQ[{expr},{h1,h2,...}] checks that in addition to the pattern \
objects, no heads h1, h2, ... are present.";

FCProgressBar::usage =
"FCProgressBar[text, i, total] is a simple auxiliary function that can \
be used to display the progress of a certain evaluation, e.g. mapping a list \
of integrals to some function. Here i is the number of the current step \
while total denotes the overall number of steps. A simple usage example
is Table[FCProgressBar[\"Calculating integral \", i, 10], {i, 1, 10}]";

FCSplit::usage = "FCSplit[expr,{v1, v2, ...}] splits expr into pieces \
that are free of any occurence of v1, v2, ... and pieces that contain \
those variables. This works both on sums and products. The output \
is provided in the form of a two element list. One can recover the \
original expression by applying Total to that list";

FCSymmetrize::usage=
"FCSymmetrize[expr, {a1, a2, ...}] symmetrizes expr with respect \
to the variables a1, a2, ... .";

FreeQ2::usage =
"FreeQ2[expr, {form1, form2, ...}] yields True if expr does not \
contain any occurence of form1, form2, ... and False otherwise. \
FreeQ2[expr, form] is the same as FreeQ[expr, form].";

FRH::usage =
"FRH[exp_] := FixedPoint[ReleaseHold, exp], i.e., FRH removes all \
HoldForm and Hold in exp.";

FunctionLimits::usage = "FunctionLimits is an option of ILimit, specifying which \
functions should be checked for finiteness.";

ILimit::usage = "ILimit[exp, a -> b] checks functions specified by the option \
FunctionLimits and takes the limit a->b of these functions only if it is finite.  \
For the rest of the expression exp, the limit is taken.";

Intersection1::usage=
"Intersection1[l1, l2], where l1 and l2 are lists returns a list of \
elements both in l1 and l2. Multiple occurences of an element are \
kept the minimum number of times it occures in l1 or l2";

Map2::usage=
"Map2[f, exp] is equivalent to Map if NTerms[exp] > 1, \
otherwise Map2[f, exp] gives f[exp].";

MemSet::usage =
"MemSet[f[x_], body] is like f[x_] := f[x] = body, \
but dependend on the value of the setting of MemoryAvailable -> \
memorycut (memorycut - MemoryInUse[]/10.^6) \
MemSet[f[x_], body] may evaluate as f[x_] := body."

MemoryAvailable::usage =
"MemoryAvailable is an option of MemSet. It can be set to an integer n, \
where n is the available amount of main memory in Mega Byte. \
The default setting is $MemoryAvailable.";

MLimit::usage=
"MLimit[expr, {lims}] takes multiple limits of expr using the limits lims.";

NTerms::usage=
"NTerms[x] is equivalent to Length if x is a sum; otherwise \
NTerms[x] returns 1, except NTerms[0] -> 0."

NumericalFactor::usage =
"NumericalFactor[expr] gives the overall numerical factor of expr.";

NumericQ1::usage=
"NumericQ1[x,{a,b,..}] is like NumericQ, but assumes that {a,b,..} are \
numeric quantities.";

PartitHead::usage=
"PartitHead[expr, h] returns a list {ex1, h[ex2]} with ex1 free of \
expressions with head h, and h[ex2] having head h.";

Power2::usage=
"Power2[x, y] represents x^y.  Sometimes Power2 is more useful than the \
Mathematica Power. Power2[-a,b] simplifies to (-1)^b Power2[a,b] \
(if no Epsilon is in b ...).";

PowerFactor::usage=
"PowerFactor[exp] replaces x^a y^a with (x y)^a.";

PowerSimplify::usage=
"PowerSimplify[exp]  simplifies (-x)^a to (-1)^a x^a and \
(y-x)^n to (-1)^n (x-y)^n; thus assuming that the exponent is \
an integer (even if it is symbolic). Furthermore \
(-1)^(a+n) and I^(a+n) are expanded and (I)^(2 m) -> (-1)^m and \
(-1)^(n_Integer?EvenQ m) -> 1 and \
(-1)^(n_Integer?OddQ m) -> (-1)^m and \
(-1)^(-n) -> (-1)^n and Exp[I m Pi] -> (-1)^m.";

SelectFree2::usage=
"SelectFree2[expr, a, b, ...] is like SelectFree but
it first expands the expression w.r.t to the arguments via
Expand2";

SelectFree::usage=
"SelectFree[expr, a, b, ...] is equivalent to \
Select[expr, FreeQ2[#, {a,b, ...}]&], except the \
special cases: SelectFree[a, b] returns a and \
SelectFree[a,a] returns 1 (where a is not a product or \
a sum).";

SelectNotFree::usage=
"SelectNotFree[expr, a, b, ...] is equivalent to \
Select[expr, !FreeQ2[#, {a,b, ...}]&], except the \
special cases: SelectNotFree[a, b] returns 1 and \
SelectNotFree[a,a] returns a (where a is not a product or \
a sum).";

SelectNotFree2::usage=
"SelectNotFree2[expr, a, b, ...] is like SelectNotFree but
it first expands the expression w.r.t to the arguments via
Expand2";

SelectSplit::usage=
"SelectSplit[l, p] Construct list of mutually exclusive subsets from l in \
which every element li satisfies a criterium pj[li] with pj from p and \
appends the subset of remaining unmatched elements.";

XYT::usage=
"XYT[exp, x,y] transforms  (x y)^m away ..."

FCSplit::fail = "Error! Splitting `1` w.r.t `2` failed!";

Begin["`Package`"]
End[]

(* ------------------------------------------------------------------------ *)

Begin["`SharedTools`Private`"]

SetAttributes[MemSet, HoldFirst];

Options[Cases2] = {
	Heads -> False
};

Options[Combine] = {
	Expanding -> False
};

Options[SelectSplit] = {
	Heads -> None
};

Options[FCFactorOut] = {
	Factoring -> Simplify,
	Head -> Identity
};

Options[ILimit] = {
	FunctionLimits -> {Log -> Log}
};

Options[MemSet] = {
	MemoryAvailable -> $MemoryAvailable
};

Options[MLimit] = {
	Limit -> Limit
};

Options[FCSplit] = {
	Expanding -> True
};

Options[FRH] = {
	IsolateNames->All
};

Options[PowerSimplify] = {
	Assumptions->True,
	PowerExpand->True
};

Options[Power2] = {
	Assumptions->True
};

Cases2[expr_, {f___}, opts:OptionsPattern[]] :=
	Cases2 @@ Prepend[{f,opts}, expr];

Cases2[expr_, f_, opts:OptionsPattern[]] :=
	Union[Cases[{expr}, HoldPattern[f[___]], Infinity,opts]];

Cases2[expr_, f_, g__, opts:OptionsPattern[]] :=
	Union[Cases[{expr}, Alternatives@@(#[___]&/@{f,g}),
	Infinity, FilterRules[{opts}, Options[Cases]]]];


Combine[x_, OptionsPattern[]] :=
	Block[{combinet1, combinet2, expanding, num, le},
		expanding = OptionValue[Expanding];
		combinet2 = Together[ x /. Plus ->
		(If[FreeQ[{##}, _^_?Negative] && FreeQ[{##}, Rational],
				combinet1[##],
				Plus[##]
		]&)] /. combinet1 -> Plus;
		Which[	expanding === All,
				combinet2 = ExpandNumerator[combinet2 // ExpandDenominator] ,
				expanding === True,
				num = Numerator[combinet2];
				If[Head[num] =!= Plus,
					combinet2 = Expand[num]/Denominator[combinet2],
					If[LeafCount[num]<1000,
							combinet2 = Expand[num]/Denominator[combinet2],
							le = Length[num];
							combinet2 = Sum[FCPrint[2,"expanding ", i," out of ",le];
							Expand[num[[i]]],{i,Length[num]}]/ Denominator[combinet2]
					]
				],
				True,
				combinet2
		];
		combinet2
	];

(*
If one does not need MatchQ, but just SameQ, then
Complement1[x_List, y__List] :=
Replace[x, Dispatch[(# :> Sequence[]) & /@ Union[y]], 1]
would be sufficient
*)

Complement1[a_List, b_List] :=
	Block[{len, len1, i, alt, p, drp, ii, go}, p = 0; i = 0; drp = {};
		len = Length[a]; len1 = Length[b]; alt = b;
		While[i < len && p < len1, ++i;
			If[ii = 0; go = True;
				While[ii < Length[alt] && go,
					++ii;
					If[MatchQ[a[[i]],
						alt[[ii]]],
						alt = Drop[alt, {ii}];
						go = False
					]
				];
				Not[go],
				p = p + 1;
				drp = Append[drp, i]
			]
		];
		Part[a, Complement[Range[len], drp]]
	];

Expand2[x_] :=
	Block[{pow},
		Expand[x/. y_ ^ n_ /;
		Head[n]=!=Integer :> pow[y,n]]/.pow->Power
	];

Expand2[x_, a_ /; Head[a] =!= List] :=
	Expand2[x, {a}];

Expand2[x_, l_List] :=
	If[FreeQ[x, Plus] || MatchQ[x, HoldPattern[Plus][a__] /; FreeQ[{a}, Plus]],
		x,
		Block[{pl, t, plus},
			pl[y__] := If[FreeQ2[{Hold[y]}, l], plus[y], Plus[y]];
			t = Expand[x /. Plus -> pl]//.plus->Plus /. pl -> Plus;
			t
		]
	];


ExpandAll2[expr_] :=
	FixedPoint[
		Switch[	Head[#],
				Times,
					Distribute[#],
				Plus,
					If[	!FreeQ[List@@#,Plus],
						(ExpandAll2 /@ #),
						#
					],
				_,
					expr
		] &, expr];

FCAntiSymmetrize[x_,v_List] :=
	Block[{su},
		su[y_, {a__}, {b__}] := y /. Thread[{a} -> {b}];
		1 / Factorial[Length[v]] Plus@@Map[(Signature[#] su[x,v,#])&, Permutations[v]]
	];


FCMakeIndex[x_String, y_List, head_: Identity] :=
	MemSet[	FCMakeIndex[x, y, head],
			FCMakeIndex[x,#,head]&/@y
	];

FCMakeIndex[x_String, y_String, head_: Identity] :=
	MemSet[	FCMakeIndex[x, y, head],
			head[ToExpression[x <> y]]
	];

FCMakeIndex[x_String, y_Integer, head_: Identity] :=
	MemSet[	FCMakeIndex[x, y, head],
			head[ToExpression[x <> ToString[y]]]
	]/; y >= 0;

FCMakeIndex[x_String, y_Integer, head_: Identity] :=
	MemSet[	FCMakeIndex[x, y, head],
			head[ToExpression[x <> "Minus" <> ToString[-y]]]
	]/; y < 0;

FCFactorOut[expr_,pref_,OptionsPattern[]]:=
	pref OptionValue[Head][OptionValue[Factoring][expr/pref]];


FCPatternFreeQ[expr_List]:=
	FreeQ2[expr, {Pattern, Blank,BlankSequence,BlankNullSequence, Alternatives}];

FCPatternFreeQ[expr_List ,objs_List]:=
	FreeQ2[expr, Join[ {Pattern, Blank,BlankSequence,BlankNullSequence, Alternatives}, objs]];

FCProgressBar[text_String, i_Integer, tot_Integer] :=
	FCPrint[0, text <> ToString[i] <> " / " <> ToString[tot] <> "\n", UseWriteString -> True];

FCSplit[expr_, vars_List /; vars =!= {}, OptionsPattern[]] :=
	Block[ {free, notfree, tmp, time},
		If[ OptionValue[Expanding],
			tmp = Expand2[expr, vars],
			tmp = expr
		];

		time=AbsoluteTime[];

		If[Head[tmp]===Plus,
			free = SelectFree[tmp, vars];
			notfree = SelectNotFree[tmp, vars],

			If[	FreeQ2[tmp,vars],
				free = tmp;
				notfree = 0,

				notfree = tmp;
				free = 0
			]
		];
		FCPrint[1,"FCSplit: Splitting, timing: ", N[AbsoluteTime[] - time, 4]];

		If[ free + notfree =!= tmp || ! FreeQ2[free, vars],
			Message[FCSplit::fail, expr, vars];
			Abort[]
		];
		{free, notfree}
	]/; Head[expr]=!=List;

FCSplit[expr_List, vars_List /; vars =!= {}, opts:OptionsPattern[]]:=
	Map[FCSplit[#, vars, opts]&, expr];


FCSymmetrize[x_,v_List] :=
	Block[{su},
		su[y_, {a__}, {b__}] := y /. Thread[{a} -> {b}];
		1 / Factorial[Length[v]] Plus@@Map[su[x, v, #]&, Permutations[v]]
	];

FreeQ2[_,{}] :=
	True;

FreeQ2[x_, y_]	:=
	FreeQ[x, y] /; Head[y] =!= List;

FreeQ2[x_, {y_}] :=
	FreeQ[x, y];

FreeQ2[x_, {y_, z__}] :=
	If[FreeQ[x, y],
		FreeQ2[x, {z}],
		False
	];

(* this is eventually slower ...
FreeQ2[x_, {y_, z__}] := FreeQ[x, Alternatives@@{y,z}];*)

FRH[x_, OptionsPattern[]] :=
	FixedPoint[ReleaseHold, x]/; OptionValue[IsolateNames]===All;

FRH[x_, OptionsPattern[]] :=
	FixedPoint[ReplaceRepeated[x, HoldForm[y_[z___]] /; ! FreeQ2[HoldForm[y], Flatten[{OptionValue[IsolateNames]}]] :> y[z]] &, x]/; OptionValue[IsolateNames]=!=All;

ILimit[exp_, lim_Rule, OptionsPattern[]] :=
	Block[{limruls, m, ff, fff, out,res},
		limruls = MapAt[(((If[FreeQ[ff[##], lim[[1]]] ||
		!FreeQ[out = Limit[Limit[ff[##] /. SmallVariable[_?((!MatchQ[#, lim[[1]]])&)] -> 0,
		SmallVariable[lim[[1]]] -> lim[[2]]], lim], DirectedInfinity[___] | Indeterminate | _Limit],
		fff[##], out]&))&) /. {ff -> #[[2]], fff -> #[[1]]}, #, 2]& /@ (OptionValue[FunctionLimits]);
		FCPrint[1, "limruls: ", limruls];
		res = exp /. limruls;
		FCPrint[1, "res: ", res];
		Limit[Limit[res, SmallVariable[lim[[1]]] -> lim[[2]]], lim]
	];

(*

Comment: Rolf Mertig, 17th March 2004

Should the implementation of Intersection1 not be the same as
MultiIntersection (from the "Further Examples" section of Intersection) ?

The only difference is that MultiIntersection gives a sorted output.

MultiIntersection[l1_List, l2_List] :=
	Module[{nl, f}, f[x_] := {First[#], Length[#]} & /@ Split[Sort[x]];
		nl = Sort[Join[Flatten[Map[f, {l1, l2}], 1]]];
		nl = Split[nl, #[[1]] === #2[[1]] &];
		Flatten[Cases[nl, {{x_, m_}, {x_, n_}} :> Table[x, {m}]], 1]]
*)


Intersection1[a_List, b_List] :=
		Block[{len, len1, i, alt, p, drp, ii, go}, p = 0; i = 0; drp = {};
			len = Length[a]; len1 = Length[b]; alt = b;
			While[i < len && p < len1, ++i;
				If[ii = 0; go = True;
					While[ii < Length[alt] && go,
						++ii;
						If[MatchQ[a[[i]],
								alt[[ii]]],
								alt = Drop[alt, {ii}];
								go = False
						]
					]; Not[go],
					p = p + 1;
					drp = Append[drp, i]
				]
			];
			Part[a, drp]
		];


Map2[f_, exp_] :=
	If[
		NTerms[exp] > 1,
		Map[f,exp],
		f[exp]
	];

MemSet[x_,y_, OptionsPattern[]] :=
	If[(OptionValue[MemoryAvailable] - MemoryInUse[]/1000000.) <1. || $DisableMemSet,
		y,
		Set[x, y]
	];

MLimit[x_, l_List, OptionsPattern[]] :=
	Fold[OptionValue[Limit][#1, Flatten[{##2}][[1]]]&, x, l];


NTerms[x_Plus] :=
	Length[x];

NTerms[x_] :=
	Block[{ntermslex = Expand[x]},
		If[ Head[ntermslex]===Plus,
			ntermslex = Length[ntermslex],
			If[x===0,
				ntermslex = 0,
				ntermslex = 1
			]
		];
	ntermslex
	];


NumericalFactor[a___ /; Length[{a}] =!=1] :=
	(Message[NumericalFactor::argrx, NumericalFactor, Length[{a}], 1];
	Abort[]);

NumericalFactor[x_]:=
	If[NumberQ[x],
		x,
		If[Head[x] === Times,
			If[NumberQ[First[x]], First[x], 1],
			1
		]
	];


NumericQ1[x_, nums_List] :=
	Block[{r, syms, res, ii=0, tag},
		SetAttributes[tag, {NumericFunction,NHoldAll}];
		Off[$MaxExtraPrecision::"meprec"];
		syms = (++ii; tag[ii])& /@ nums;
		Off[$MaxExtraPrecision::"meprec"];
		res = NumericQ[x /. ((Rule @@ #) & /@ Transpose[{nums, syms}])];
		On[$MaxExtraPrecision::"meprec"];
		res
	];

PartitHead[x_, y_] :=
	{1, x} /; Head[x] === y;

PartitHead[x_Times, y_] :=
	{x, 1} /; FreeQ[x, y];

PartitHead[x_, y_] :=
	{x, 0} /; FreeQ[x, y];

PartitHead[x_Plus, y_] :=
	{#, x - #}& @ Select[x, FreeQ[#, y[___]]&];

PartitHead[x_Times,y_] :=
	{x/#, #}& @ Select[x,If[Head[#]===y,True]&];

Power2 /:
	Power2[-1,OPEm, OptionsPattern[]]^2 :=
		1;
Power2[n_Integer?Positive, em_, OptionsPattern[]] :=
	n^em;
Power2[n_, em_Integer, OptionsPattern[]] :=
	n^em;
Power2[-1,OPEm-2, opts:OptionsPattern[]] =
	Power2[-1,OPEm, opts];
Power2[-a_,b_/;FreeQ2[b, {Epsilon,Epsilon2}], opts:OptionsPattern[]] :=
	PowerSimplify[(-1)^b,Assumptions->OptionValue[Assumptions]] Power2[a,b,opts];

Format[Power2[a_, b_ /; b, OptionsPattern[]]] :=
	a^b;

Power2 /:
	MakeBoxes[Power2[a_, b_, OptionsPattern[]] , TraditionalForm] :=
		ToBoxes[a^b, TraditionalForm];

PowerFactor[exp_Plus] :=
	PowerFactor /@ exp;

PowerFactor[exp_] :=
	If[Head[exp] =!= Times,
		exp //. {x_^a_ y_^a_ :> (x y)^a},
		SelectFree[exp, Power] (SelectNotFree[exp,
		Power] //. {x_^a_ y_^a_ :> (x y)^a})
	];

PowerSimplify[x_, OptionsPattern[]] :=
	Block[{nx, qcdsub = False, power3, assumpts,usePowerExpand},
		assumpts = OptionValue[Assumptions];
		usePowerExpand = OptionValue[PowerExpand];
		If[!FreeQ[x, ScaleMu],
			qcdsub = True;
			nx = x /. pow_[any_ /ScaleMu^2,exp_]:> power3[pow][any/ScaleMu^2,exp],
			nx = x
		];
		nx = nx /.
			{	(a_/;Head[a]===Plus || Head[a] === Times)^(w_)/;usePowerExpand :>
					(PowerExpand[Factor2[one*a]^w, Assumptions->assumpts] /. one -> 1),
				Power2[(a_/;Head[a]===Plus || Head[a] === Times),(w_)]/;usePowerExpand :>
					(PowerExpand[Factor2[one*a]^w, Assumptions->assumpts] /.
						(ab_Plus)^v_ :> Power2[ab, v] /. one -> 1)/.(-1)^vv_ :> Power2[-1,vv]	} /.
			{	(-1)^(a_Plus) :> Expand[(-1)^a]	}/.
			{(n_Integer?Negative)^m_ :> (-1)^m (-n)^m}/.
			{	((-1)^OPEm (1+(-1)^OPEm)) :> (1+(-1)^OPEm),
				((1-(-1)^OPEm)(1+(-1)^OPEm)) :> 0,
				((1+(-1)^OPEm)(1+(-1)^OPEm)) :> (2(1+(-1)^OPEm)),
				(-1)^OPEm (1-(-1)^OPEm) :> (-1+(-1)^OPEm),
				bbb_^(c_/;!FreeQ[c,Plus]) :> bbb^Expand[c]	}//.
			{	(-1)^(_Integer?EvenQ _) :> 1,
				(-1)^(_Integer?OddQ m_) :> (-1)^m,
				(-1)^(_Integer?EvenQ _. + i_) :> (-1)^i,
				(-1)^(n_Integer?OddQ m_. + i_) :> (-1)^(m+i) /; n=!=(-1),
				(-1)^(-n_) :> (-1)^n,
				I^(2 m_+i_.) :> (I)^i (-1)^m,
				(I/2)^(m_) I^m_ :> (-1)^m/2^m,
				I^(a_Plus) :> Expand[I^a],
				Exp[I Pi OPEi] :> (-1)^OPEi,
				Exp[I Pi OPEj] :> (-1)^OPEj,
				Exp[I Pi OPEm] :> (-1)^OPEm,
				HoldPattern[E^(em_ + Complex[0,n_] Pi)]  :> (-1)^n Exp[em],
				Power2[I, 2 m_ + i_.] :> I^i (-1)^m,
				Power2[I,(a_Plus)] :> Expand[I^a],
				Power2[(-1),(a_Plus)] :> Expand[(-1)^a]	};
			If[	qcdsub === True,
				nx = nx /. power3[poww_] :> poww
			];
		nx
	];

SelectFree[0,_] :=
	0;

SelectFree[a_, b__] :=
	Block[{dum1,dum2, select},
		select[x_, y_ /; Head[y] =!= List] :=
			Select[x, FreeQ[#, y]&];
		select[x_, y_List ] :=
			Select[x, FreeQ2[#, y]&];
		select[x_, y_, z__]  :=
			Select[x, FreeQ2[#, Flatten[{y, z}]]&];
		If[(Head[a] === Plus) || (Head[a] === Times),
			select[a,b],
			(* need two dummy-vars in case "a" is an integer *)
			select[a dum1 dum2, b] /. {dum1 :> 1, dum2 :> 1}
		]
	];


SelectFree2[x_,args__] :=
	SelectFree[Expand2[x,Flatten[{args}]],args];

SelectNotFree[0,_] :=
	0;

SelectNotFree[a_, b__] :=
	Block[{dum1,dum2, select},
		select[x_, y_ /; Head[y] =!= List]  :=
			Select[x, !FreeQ[#, y]&];
		select[x_, y_List ]  :=
			Select[x, !FreeQ2[#, y]&];
		select[x_, y_, z__]  :=
			Select[x, !FreeQ2[#, Flatten[{y, z}]]&];
		If[(Head[a] === Plus) || (Head[a] === Times) ||	(Head[a] === List),
			select[a,b],
			select[a dum1 dum2, b] /.	{dum1 :> 1, dum2 :> 1}
		]
	];

SelectNotFree2[x_,args__] :=
	SelectNotFree[Expand2[x,Flatten[{args}]],args];

SelectSplit[ex_, p_List, opts___Rule] :=
	Block[{ii, jj, aa, res, exp = List @@ ex, h = Head[ex],
		hh = Heads /. Flatten[{opts}] /. Options[SelectSplit]},
		ii = 0;
		res = (++ii; Select[#, (aa = #;	And @@
		((#[aa]=!=True)& /@ Drop[p, {ii}])) &])& /@ (Select[exp, #]& /@ p);
		If[hh =!= None && hh =!= False && hh =!= {},
			If[Length[hh] < Length[#] - 1,
				hh = Join[hh, Table[hh[[-1]], {Length[#] - 1 - Length[hh]}]]
			];
		Append[Table[hh[[jj]][#[[jj]]], {jj, Length[#] - 1}],
		If[Length[hh] === Length[#],
			hh[[-1]][#[[-1]]], #[[-1]]
		]], #
		] &[(h @@ #) & /@ Append[res, Complement[exp, Join @@ res]]]
	];

(* integral transformation only valid if nonsingular in x, y = 0,1 *)
XYT[exp_, x_, y_] :=
	Block[{z, t, u},
		t = 1/x Factor2[PowerSimplify[Factor2[exp] /. y -> (z/x)]];
		Factor2[PowerSimplify[(1-z) (t /. x :> (1-z) u + z)]/.{u:>y,z:>x}]
	];

FCPrint[1, "SharedTools loaded."];
End[]

