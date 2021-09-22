(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SharedTools														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Small helper tools extensively used in FeynCalc			    *)

(* ------------------------------------------------------------------------ *)

Cases2::usage=
"Cases2[expr, f] returns a list of all objects in expr with head f.

Cases2[expr,f] is equivalent to Cases2[{expr},f[___],Infinity]//Union.

Cases2[expr, f, g, ...] or Cases2[expr, {f,g, ...}] is equivalent to
Cases[{expr},f[___] | g[___] ...].";


Coefficient2::usage=
"Coefficient2[exp, form1, form2, ...] is like Coefficient, but it also allows
to extracts coefficients  of form1, form2, ... sequentially. To specify the
power in formi, write it as {var,pow}.";

Combine::usage=
"Combine[expr] puts terms in a sum over a common denominator and cancels
factors in the result. Combine is similar to Together, but accepts the option
Expanding and works usually better than Together for polynomials involving
rationals with sums in the denominator.";

Complement1::usage=
"Complement1[l1, l2] where l1 and l2 are lists returns a list of elements from
l1 not inl2. Multiple occurrences of an element in l1 are kept and multiple
occurrences of an element in l2 are dropped if present in l1.";

Expand2::usage=
"Expand2[exp, x] expands all sums containing x.

Expand2[exp, {x1, x2, ...}]  expands all sums containing x1, x2, ....";

ExpandAll2::usage=
"ExpandAll2[exp] is similar to ExpandAll, but much faster on simple structures.";

FCAntiSymmetrize::usage=
"FCAntiSymmetrize[expr, {a1, a2, ...}] antisymmetrizes expr with respect to the
variables a1, a2, ....";

FCAttachTypesettingRule::usage=
"FCAttachTypesettingRule[expr, ...] attaches a specific TraditionalForm
typesetting rule to expr. It doesn't change any properties of expr apart from
adding a FormatValue with a MakeBoxes rule.

Following choices are possible:

- FCAttachTypesettingRule[expr_, str]

- FCAttachTypesettingRules[expr, {SubscriptBox, var, sub}]

- FCAttachTypesettingRules[expr, {SuperscriptBox, var, sup}]

- FCAttachTypesettingRules[expr, {SubsuperscriptBox, var, sub, sup}]

Use FCRemoveTypesettingRules to remove all typesetting rules attached to expr.";

FCCheckVersion::usage=
"FCCheckVersion[major, minor, build] checks if the current version of FeynCalc
is larger or equal than marjor.minor.build. For example, FCCheckVersion[9,3,0]
will generate a warning (when running with the frontend) or quit kernel (when
running without the frontend) if the loaded FeynCalc version is older than
9.3.0.

Notice that this function is available only since FeynCalc 9.3.";

FCDuplicateFreeQ::usage=
"FCDuplicateFreeQ[list] yields True if list contains no duplicates and False
otherwise.

FCDuplicateFreeQ[list,test] uses test to determine whether two objects should
be considered duplicates.

FCDuplicateFreeQ returns the same results as the standard DuplicateFreeQ. The
only reason for introducing FCDuplicateFreeQ is that DuplicateFreeQ is not
available in Mathematica 8 and 9, which are still supported by FeynCalc.";

FCGetNotebookDirectory::usage=
"FCGetNotebookDirectory[] is a convenience function that returns the directory
in which the current notebook or .m file is located. It also works when the
FrontEnd is not available.";

FCFactorOut::usage=
"FCFactorOut[exp, pref] factors out pref out of exp. This is often needed to
bring exp into a particular form that Mathematica refuses to give.";

FCHighlight::usage=
"FCHighlight[exp, {{symbol1, color1}, {symbol2, color2}, ...}] highlights the
given set of symbols in the output using Style and the provided colors. This
works only in the frontend and alters the input expression in such a way, that
it cannot be processed further (because of the introduced Style heads).";

FCMakeIndex::usage=
"FCMakeIndex[str1, str2, head] generates an index with the given head out of
the string str1 and str2. For example, FCMakeIndex[\"Lor\",\"1\",LorentzIndex]
yields LorentzIndex[Lor1]. The second argument can also be an integer.
FCMakeIndex is useful for converting the output of different diagram
generators such as FeynArts or QGAF into the FeynCalc notation. It uses
memoization to improve the performance.";

FCMakeSymbols::usage=
"FCMakeSymbols[name, range, type] generates a list or a sequence of symbols
(depending on the value of type) by attaching elements of the list range to
name.

For example, FCMakeSymbols[mu, Range[1, 3], List] returns {mu1,mu2,mu3}.";

FCPatternFreeQ::usage =
"FCPatternFreeQ[{exp}] yields True if {exp} does not contain any pattern
objects, e.g. Pattern, Blank, BlankSequence and BlankNullSequence.

FCPatternFreeQ[{exp},{h1,h2,...}] checks that in addition to the pattern
objects, no heads h1, h2, ... are present.";

FCProgressBar::usage =
"FCProgressBar[text, i, total]  is a simple auxiliary function that can be used
to display the progress of a certain evaluation, e.g. mapping a list of
integrals to some function. Here i is the number of the current step while
total denotes the overall number of steps.";

FCReloadFunctionFromFile::usage =
"FCReloadFunctionFromFile[function, path] is an auxiliary function that
attempts to remove all the definitions of the given FeynCalc function and then
reload them from the specified file.

It is intended to be a helper tool for FeynCalc developers, which allows one
to debug/improve internal functions and test the results without restarting
the kernel. Depending on the complexity of the given function, there might
also be unknown side effects.

The function is not meant to be invoked by the normal users.";

FCReloadAddOns::usage =
"FCReloadAddOns[{addons}] is an auxiliary function that attempts to reload
FeynCalc addons without restarting the kernel.

It is intended to be a helper tool for FeynCalc developers, which allows one
to debug/improve add-ons and test the results on the fly. Depending on the
complexity of the given add-on, there might also be severe side effects.

The function is not meant to be invoked by the normal users.";

FCRemoveTypesettingRules::usage =
"FCRemoveTypesettingRules[expr] removes all typesetting rules attached to expr.
Effectively it sets the FormatValues of expr to an empty list.";

FCReplaceAll::usage=
"FCReplaceAll[exp, ru1, ...] is like ReplaceAll, but it also allows to apply
multiple replacement rules sequentially. Instead of doing exp /. ru1 /. ru2 /.
ru3 one can just write FCReplaceAll[exp, ru1, ru2, ru3].";

FCReplaceRepeated::usage=
"FCReplaceRepeated[exp, ru1, ...]  is like ReplaceRepeated, but it also allows
to apply multiple replacement rules sequentially.

Instead of doing exp //. ru1 //. ru2 //. ru3 one can just write
FCReplaceRepeated[exp, ru1, ru2, ru3].";

FCSplit::usage =
"FCSplit[exp, {v1, v2, ...}] splits expr into pieces that are free of any
occurrence of v1, v2, ... and pieces that contain those variables. This works
both on sums and products. The output is provided in the form of a two element
list. One can recover the original expression by applying Total to that list.";

FCProductSplit::usage =
"FCProductSplit[exp, {v1, v2, ...}] splits expr into pieces that are free of
any occurrence of v1, v2, ... and pieces that contain those variables. This
works both on sums and products. The output is provided in the form of a two
element list. One can recover the original expression by applying Total to
that list.";

FCSubsetQ::usage=
"FCSubsetQ[list1, list2]  yields True if list2 is a subset of list1 and False
otherwise. It returns the same results as the standard SubsetQ. The only
reason for introducing FCSubsetQ is that SubsetQ is not available in
Mathematica 8 and 9, which are still supported by FeynCalc.";

FCSymmetrize::usage=
"FCSymmetrize[expr, {a1, a2, ...}] symmetrizes expr with respect to the
variables a1,a2, ....";

FreeQ2::usage =
"FreeQ2[expr, {form1, form2, ...}] yields True if expr does not contain any
occurrence of form1, form2, ... and False otherwise.

FreeQ2[expr, form] is the same as FreeQ[expr, form].";

FRH::usage =
"FRH[exp_] corresponds to FixedPoint[ReleaseHold, exp],  i.e. FRH removes all
HoldForm and Hold in exp.";

FunctionLimits::usage =
"FunctionLimits is an option of ILimit, specifying which functions should be
checked for finiteness.";

ILimit::usage =
"ILimit[exp, a -> b] checks functions specified by the option FunctionLimits
and takes the limit a->b of these functions only if it is finite.  For the
rest of the expression exp, the limit is taken.";

Map2::usage=
"Map2[f, exp] is equivalent to Map if Nterms[exp] > 0, otherwise Map2[f, exp]
gives f[exp].";

MemSet::usage =
"MemSet[f[x_], body] is like f[x_] := f[x] = body, but depending on the value
of the setting of FCMemoryAvailable -> memorycut (memorycut -
MemoryInUse[]/10^6)

MemSet[f[x_], body] may evaluate as f[x_] := body.";

FCMemoryAvailable::usage =
"FCMemoryAvailable is an option of MemSet. It can be set to an integer n, where
n is the available amount of main memory in MiB. The default setting is
$FCMemoryAvailable.";

MLimit::usage=
"MLimit[expr, lims] takes multiple limits of expr using the limits lims.";

NTerms::usage=
"NTerms[x] is equivalent to Length if x is a sum; otherwise NTerms[x] returns
1, except NTerms[0] -> 0.";

NumericalFactor::usage =
"NumericalFactor[expr] gives the overall numerical factor of expr.";

NumericQ1::usage=
"NumericQ1[x, {a, b, ..}] is like NumericQ, but assumes that {a,b,..} are
numeric quantities.";

PartitHead::usage=
"PartitHead[expr, h] returns a list {ex1, h[ex2]} with ex1 free of expressions
with head h, and h[ex2] having head h.";

Power2::usage=
"Power2[x, y] represents x^y.  Sometimes Power2 is more useful than the
Mathematica Power. Power2[-a,b] simplifies to (-1)^b Power2[a,b] (if no
Epsilon is in b ...).";

PowerFactor::usage=
"PowerFactor[exp] replaces x^a y^a with (x y)^a.";

PowerSimplify::usage=
"PowerSimplify[exp] simplifies (-x)^a to (-1)^a x^a and (y-x)^n to (-1)^n
(x-y)^n thus assuming that the exponent is an integer (even if it is
symbolic).

Furthermore, (-1)^(a+n)  and I^(a+n) are expanded and (I)^(2 m) -> (-1)^m and
(-1)^(n_Integer?EvenQ m) -> 1 and (-1)^(n_Integer?OddQ m) -> (-1)^m for n even
and odd respectively and (-1)^(-n) -> (-1)^n and Exp[I m Pi] -> (-1)^m.";

SelectFree2::usage=
"SelectFree2[expr, a, b, ...] is similar to SelectFree but it also differs from
the latter in several respects.

If expr is  a list, SelectFree2 behaves exactly the same way as SelectFree.

If expr is not a list, SelectFree2 first expands the expression w.r.t. the
arguments via Expand2.

Furthermore, SelectFree2[a,b] returns a and SelectFree2[a,a] returns 0. This
differs from the behavior of SelectFree but is consistent with the naive
expectations when applying the function to a sum of terms.";

SelectFree::usage=
"SelectFree[expr, a, b, ...] is equivalent to Select[expr, FreeQ2[#, {a,b,
...}]&], except the special cases: SelectFree[a, b] returns a and
SelectFree[a,a] returns 1 (where a is not a product or a sum).";

SelectNotFree::usage=
"SelectNotFree[expr, x] returns that part of expr which is not free of any
occurrence of x.

SelectNotFree[expr, a, b, ...] is equivalent to Select[expr, !FreeQ2[#, {a, b,
...}]&], except the special cases:
SelectNotFree[a, b] returns 1 and SelectNotFree[a, a] returns a (where a is
not a product or a sum).";

SelectNotFree2::usage=
"SelectNotFree2[expr, a, b, ...] is similar to SelectNotFree but it also
differs from the latter in several respects.

If expr is  a list, SelectNotFree2 behaves exactly the same way as
SelectNotFree.

If expr is not a list, SelectNotFree2 first expands the expression w.r.t. the
arguments via Expand2.

Furthermore, SelectNotFree2[a,b] returns 0. This differs from the behavior of
SelectFree but is consistent with the naive expectations when applying the
function to a sum of terms.";

SelectSplit::usage=
"SelectSplit[l, p] constructs list of mutually exclusive subsets from l in
which every element li satisfies a criterion pj[li] with pj from p and appends
the subset of remaining unmatched elements.";

Variables2::usage=
"Variables2[expr] is like Variables, but it also works on rules and equalities
as well as lists thereof.

Variables2 always applies Union to the output.";

XYT::usage=
"XYT[exp, x, y] transforms  (x y)^m away.";

FCProductSplit::failmsg =
"Error! FCProductSplit has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

FCSplit::failmsg =
"Error! FCSplit has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

FCReloadFunctionFromFile::failmsg =
"Error! FCReloadFunctionFromFile has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

FCDuplicateFreeQ::failmsg =
"Error! FCDuplicateFreeQ has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

FCMakeSymbols::failmsg =
"Error! FCMakeSymbols has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

Begin["`Package`"];
End[]

(* ------------------------------------------------------------------------ *)

Begin["`SharedTools`Private`"];

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
	Factoring	-> Simplify,
	Head 		-> Identity
};

Options[ILimit] = {
	FunctionLimits -> {Log -> Log}
};

Options[MemSet] = {
	FCMemoryAvailable :> $FCMemoryAvailable
};

Options[MLimit] = {
	Limit -> Limit
};

Options[NTerms] = {
	Expand -> True
};

Options[FCHighlight] = {
	Style -> Function[x,Rule[x[[1]], Style[x[[1]], 14, x[[2]], Bold]]]
};

Options[FCSplit] = {
	Expanding -> True
};

Options[FCProductSplit] = {
	Abort -> False
};

Options[FRH] = {
	IsolateNames -> All
};

Options[PowerSimplify] = {
	Assumptions	-> True,
	PowerExpand	-> True
};

Options[Power2] = {
	Assumptions	-> True
};

Options[Variables2] = {
};

Cases2[expr_, {f___}, opts:OptionsPattern[]] :=
	Cases2 @@ Prepend[{f,opts}, expr];

Cases2[expr_, f_, opts:OptionsPattern[]] :=
	Union[Cases[{expr}, HoldPattern[f[___]], Infinity,opts]];

Cases2[expr_, f_, g__, opts:OptionsPattern[]] :=
	Union[Cases[{expr}, Alternatives@@(HoldPattern[#[___]]&/@{f,g}),
	Infinity, FilterRules[{opts}, Options[Cases]]]];


Coefficient2[ex_, {var_, pow_}] :=
	Coefficient[ex, var, pow];

Coefficient2[ex_, var_, pow_Integer] :=
	Coefficient[ex, var, pow];

Coefficient2[ex_, form_] :=
	Coefficient[ex, form]/; Head[form]=!=List;

Coefficient2[ex_, form1_, form2__] :=
	Fold[Coefficient2, ex, {form1, form2}]/; !MatchQ[{form2},{_Integer}];

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
					#
		] &, expr];

FCAttachTypesettingRule[expr_, {SubscriptBox, var_, sub_}] :=
	expr /: MakeBoxes[expr, TraditionalForm] :=
		SubscriptBox[ToString[var], ToString[sub]];

FCAttachTypesettingRule[expr_, {SuperscriptBox, var_, sup_}] :=
	expr /: MakeBoxes[expr, TraditionalForm] :=
		SuperscriptBox[ToString[var], ToString[sup]];

FCAttachTypesettingRule[expr_, {OverscriptBox, var_, ovr_}] :=
	expr /: MakeBoxes[expr, TraditionalForm] :=
		OverscriptBox[ToString[var], ToString[ovr]];

FCAttachTypesettingRule[expr_, {SubsuperscriptBox, var_, sub_, sup_}] :=
	expr /: MakeBoxes[expr, TraditionalForm] :=
		SubsuperscriptBox[ToString[var], ToString[sub], ToString[sup]];

FCAttachTypesettingRule[expr_, obs_String] :=
	expr /: MakeBoxes[expr, TraditionalForm] := obs;

FCAttachTypesettingRule[expr_, fbox_FormBox] :=
	expr /: MakeBoxes[expr, TraditionalForm] := fbox;

FCAntiSymmetrize[x_,v_List] :=
	Block[{su},
		su[y_, {a__}, {b__}] := y /. Thread[{a} -> {b}];
		1 / Factorial[Length[v]] Plus@@Map[(Signature[#] su[x,v,#])&, Permutations[v]]
	];

FCCheckVersion[major_Integer?NonNegative, minor_Integer?NonNegative, build_Integer?NonNegative, OptionsPattern[]]:=
	Block[{str, a, b, c},
		str = "Your FeynCalc version is too old. The following code requires at least FeynCalc "<>
		ToString[major]<>"."<> ToString[minor]<>"."<> ToString[build]<>".";
		{a, b, c} = ToExpression[StringSplit[$FeynCalcVersion, "."]];

		If[	FromDigits[{a,b,c}] < FromDigits[{major,minor,build}],
			If[	($FrontEnd === Null || $Notebooks===False),
				Print[str];
				Quit[],
				CreateDialog[{TextCell[str], DefaultButton[]}, Modal->True];
			]
		]
	];


FCDuplicateFreeQ[ex_/; Head[ex] =!= List, ___] :=
	(
	Message[FCDuplicateFreeQ::failmsg, "The input expression is not a list."];
	Abort[]
	);

FCDuplicateFreeQ[{}, ___] :=
	True;

FCDuplicateFreeQ[ex_List, test_ : FCGV[""]] :=
	Block[{tmp, num},

		If[	test === FCGV[""],
			tmp = Last[SortBy[Tally[ex], Last]],
			tmp = Check[Last[SortBy[Tally[ex, test], Last]], $Failed, Tally::smtst]
		];

		If[	Head[tmp] =!= List || tmp === {},
			Message[FCDuplicateFreeQ::failmsg, "Failed to count the elements in the list."];
			Abort[]
		];

		num = Last[tmp];

		If[	!MatchQ[num, _Integer?NonNegative],
			Message[FCDuplicateFreeQ::failmsg, "Failed to count the elements in the list."];
			Abort[]
		];

		num < 2
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

FCMakeSymbols[name_, range_List, type_, OptionsPattern[]] :=
	Block[{res, body},
		body = ToString[name];
		res = Map[ToExpression[(body <> ToString[#])] &, range];
		Switch[type,
			List,
				True,
			Sequence,
				res = Sequence @@ res,
			_,
				Message[FCMakeSymbols::failmsg, "Unknown type!"];
				Abort[]
		];
	res
	];



FCFactorOut[expr_,pref_,OptionsPattern[]]:=
	pref OptionValue[Head][OptionValue[Factoring][expr/pref]];

FCGetNotebookDirectory[]:=
	Block[{dir},
		If[$FrontEnd===Null,
			dir=DirectoryName[$InputFileName],
			dir=NotebookDirectory[]
		];
		dir
	];


FCHighlight[expr_, hlist_List/; !(OptionQ[hlist] || hlist==={}), OptionsPattern[]] :=
	(expr /. Map[OptionValue[Style][#]&, hlist])/; MatchQ[hlist,{__List}];


FCPatternFreeQ[expr_List]:=
	FreeQ2[expr, {Pattern, Blank,BlankSequence,BlankNullSequence, Alternatives}];

FCPatternFreeQ[expr_List ,objs_List]:=
	FreeQ2[expr, Join[ {Pattern, Blank,BlankSequence,BlankNullSequence, Alternatives}, objs]];

FCProgressBar[text_String, i_Integer, tot_Integer] :=
	FCPrint[0, text <> ToString[i] <> " / " <> ToString[tot] <> "\n", UseWriteString -> True];

FCReloadFunctionFromFile[fun_] :=
	FCReloadFunctionFromFile[fun,"AUTOMATIC"];


FCReloadAddOns[addons: {__String}]:=
(
BeginPackage["FeynCalc`"];
FCDeclareHeader /@ Map[ToFileName[{$FeynCalcDirectory, "AddOns", #}, # <> ".m"] &, addons];
Get /@ Map[ToFileName[{$FeynCalcDirectory, "AddOns", #}, # <> ".m"] &, addons];
EndPackage[];
);

FCReloadFunctionFromFile[fun_, fileRaw_String] :=
	Block[{names, names1, names2, str1, str2, file},

	If[	fileRaw==="AUTOMATIC",
		file = FileNames[ToString[fun]<>".m", FileNameJoin[{$FeynCalcDirectory, #}] & /@ {
			"Dirac", "ExportImport", "Feynman", "LoopIntegrals", "Lorentz",
			"NonCommAlgebra", "Pauli", "QCD", "Shared", "SUN", "Tables"}, Infinity];
		If[	Length[file]=!=1,
			Message[FCReloadFunctionFromFile::failmsg, "Failed to determine the file containing the code of this function."];
			Abort[],
			file = file[[1]]
		],
		file = fileRaw
	];

	If[! TrueQ[FileExistsQ[file]],
			Message[FCReloadFunctionFromFile::failmsg, "The file " <> file <> " does not exist."];
			Abort[]
	];

	str1 = "FeynCalc`" <> ToString[fun] <> "`*";
	str2 = "FeynCalc`" <> ToString[fun] <> "`Private`*";
	names = Join[Names[str1], Names[str2]];

	With[{x = str1}, ClearAll[x]];
	With[{x = str2}, ClearAll[x]];

	If[	names =!= {},
		Quiet[Remove /@ names, Remove::rmnsm]
	];

	ClearAll[fun];
	Quiet[Remove[fun], Remove::rmnsm];

	BeginPackage["FeynCalc`"];
		FCDeclareHeader[file];
		Get[file];
	EndPackage[];
	Null
];

FCProductSplit[expr_, {}, OptionsPattern[]]:=
	{expr,1};

FCProductSplit[expr_, vars_List /; vars =!= {}, OptionsPattern[]] :=
	Block[{dummy1,dummy2,exprAsList,pow,free,notfree,list},
		If[	NTerms[expr,Expand->False] > 1,
			Message[FCProductSplit::failmsg,"The input expression is not a product"];
			Abort[]
		];

		(*
			Using List instead of list may mess up powers inside functions.
			We don't want to touch those at all
		*)
		exprAsList = list@@(expr*dummy1*dummy2);
		If[	!FreeQ[exprAsList,Power],
			exprAsList /. Power -> pow //. {
				list[a___,pow[b_,n_Integer?Positive],c___] :> list[a,Sequence@@ConstantArray[b,n],c],
				list[a___,pow[b_,n_Integer?Negative],c___] :> list[a,Sequence@@ConstantArray[1/b,-n],c]
			} /. pow -> Power
		];
		exprAsList = exprAsList/.list->List;

		free = SelectFree[exprAsList,vars] /. dummy1|dummy2 :> Unevaluated[Sequence[]];
		notfree = SelectNotFree[exprAsList,vars];

		free = Times@@free;
		notfree = Times@@notfree;

		If[ free*notfree =!= expr || ! FreeQ2[free, vars],
			Message[FCProductSplit::failmsg, "Error! Splitting" <>ToString[expr,InputForm]<> " w.r.t " <>ToString[vars,InputForm]<> " failed!"];
			Abort[]
		];

		{free,notfree}
	];

FCRemoveTypesettingRules[expr_List] :=
	FCRemoveTypesettingRules/@expr;

FCRemoveTypesettingRules[expr_Symbol] :=
	(FormatValues[expr] = {};);

FCRemoveTypesettingRules[fun_[args__]] :=
	(FormatValues[fun] = SelectFree2[FormatValues[fun],fun[args]];);

FCReplaceAll[ex_, ru_] :=
	ReplaceAll[ex, ru];

FCReplaceAll[ex_, ru1_, ru2__] :=
	Fold[ReplaceAll, ex, {ru1, ru2}];

FCReplaceRepeated[ex_, ru_] :=
	ReplaceRepeated[ex, ru];

FCReplaceRepeated[ex_, ru1_, ru2__] :=
	Fold[ReplaceRepeated, ex, {ru1, ru2}];

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
			Message[FCSplit::failmsg, "Error! Splitting" <>ToString[expr,InputForm]<> " w.r.t " <>ToString[vars,InputForm]<> " failed!"];
			Abort[]
		];
		{free, notfree}
	]/; Head[expr]=!=List;

FCSplit[expr_List, vars_List /; vars =!= {}, opts:OptionsPattern[]]:=
	Map[FCSplit[#, vars, opts]&, expr];

FCSplit[_, vars_, OptionsPattern[]]:=
	(
	Message[FCSplit::failmsg, "The second argument must be a list."];
	Abort[]
	)/; Head[vars]=!=List;

FCSymmetrize[x_,v_List] :=
	Block[{su},
		su[y_, {a__}, {b__}] := y /. Thread[{a} -> {b}];
		1 / Factorial[Length[v]] Plus@@Map[su[x, v, #]&, Permutations[v]]
	];


FCSubsetQ[l1_List, l2_List] :=
(DeleteDuplicates[Sort[MemberQ[l1, #] & /@ l2]] === {True}) /; l2 =!= {};

FCSubsetQ[_List, {}] :=
	True;

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

Map2[f_, exp_] :=
	If[
		NTerms[exp] > 1,
		Map[f,exp],
		f[exp]
	];

MemSet[x_,y_, OptionsPattern[]] :=
	If[(OptionValue[FCMemoryAvailable] - MemoryInUse[]/1000000.) <1. || $DisableMemSet,
		y,
		Set[x, y]
	]/; MatchQ[OptionValue[FCMemoryAvailable],_Integer?Positive];

MemSet[_,_, OptionsPattern[]]:=
	(
	Message[FeynCalc::failmsg,"The value of FCMemoryAvailable is not a positive integer."];
	Abort[]
	)/; !MatchQ[OptionValue[FCMemoryAvailable],_Integer?Positive];

MLimit[x_, l_List, OptionsPattern[]] :=
	Fold[OptionValue[Limit][#1, Flatten[{##2}][[1]]]&, x, l];


NTerms[x_Plus, OptionsPattern[]] :=
	Length[x];

NTerms[x_, OptionsPattern[]] :=
	Block[{ntermslex},

		If[	OptionValue[Expand],
			ntermslex = Expand[x],
			ntermslex = x
		];

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

SelectFree[0,__] :=
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

SelectFree2[0,__]:=
	0;

SelectFree2[x_List,args__] :=
	SelectFree[x,args];

SelectFree2[x_,args__] :=
	Block[{tmp, res, null1, null2},
		tmp = Expand2[x,Flatten[{args}]];

		If[	Head[tmp]===Plus,
			res = SelectFree[tmp,args],
			res = SelectFree[tmp+null1+null2,args] /. null1|null2->0
		];
		res
	]/; Head[x]=!=List && x=!=0;

SelectNotFree[0,__] :=
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

SelectNotFree2[0,__]:=
	0;

SelectNotFree2[x_List,args__] :=
	SelectNotFree[x,args];

SelectNotFree2[x_,args__] :=
	Block[{tmp, res, null1, null2},
		tmp = Expand2[x,Flatten[{args}]];

		If[	Head[tmp]===Plus,
			res = SelectNotFree[tmp,args],
			res = SelectNotFree[tmp+null1+null2,args] /. null1|null2->0
		];
		res
	]/; Head[x]=!=List && x=!=0;

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


Variables2[expr_List, opts : OptionsPattern[]] :=
	Union[Flatten[Variables2[#, opts] & /@ expr]];

Variables2[expr_, OptionsPattern[]] :=
	Union[Variables[expr]] /; !MemberQ[{List, Equal, Rule, RuleDelayed}, Head[expr]];

Variables2[(Equal | Rule | RuleDelayed)[a_, b_], OptionsPattern[]] :=
	Union[Variables[a], Variables[b]];

(* integral transformation only valid if nonsingular in x, y = 0,1 *)
XYT[exp_, x_, y_] :=
	Block[{z, t, u},
		t = 1/x Factor2[PowerSimplify[Factor2[exp] /. y -> (z/x)]];
		Factor2[PowerSimplify[(1-z) (t /. x :> (1-z) u + z)]/.{u:>y,z:>x}]
	];

FCPrint[1, "SharedTools loaded."];
End[]

