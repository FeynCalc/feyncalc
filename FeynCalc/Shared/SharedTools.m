(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SharedTools														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
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
power in formi, write it as {var,pow}.

To keep the prefactor whose coefficient you extracted you need to set the
option Prefactor to True.";

Expand2::usage=
"Expand2[exp, x] expands all sums containing x.

Expand2[exp, {x1, x2, ...}]  expands all sums containing x1, x2, ....";

ExpandAll2::usage=
"ExpandAll2[exp] is similar to ExpandAll, but much faster on simple structures.";

Factor2::usage =
"Factor2[poly] factors a polynomial in a standard way.

Factor2 works sometimes better than Factor on polynomials involving rationals
with sums in the denominator.

Factor2 uses Factor internally and is in general slower than Factor.";

Factor3::usage=
"Factor3[exp] factors a rational function exp over the field of complex
numbers.

Factor3 is primarily meant to be used on matrices from differential equations
and Feynman parametric
representations of loop integrals. Its main goal is to rewrite all
denominators such, that they can be integrated in terms of HPLs or GPLs (when
possible).

To avoid performance bottlenecks, in the case of rational functions only the
denominator will be factored by default. This can be changed by setting the
option Numerator to True.";

FactorList2::usage=
"FactorList2[exp] is similar to FactorList except that it correctly handles
symbolic exponents.";

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
works both only for products. The output is provided in the form of a two
element list. One can recover the original expression by applying Times to
that list.";

FCSymmetrize::usage=
"FCSymmetrize[expr, {a1, a2, ...}] symmetrizes expr with respect to the
variables a1,a2, ....";

FreeQ2::usage =
"FreeQ2[expr, {form1, form2, ...}] yields True if expr does not contain any
occurrence of form1, form2, ... and False otherwise.

FreeQ2[expr, form] is the same as FreeQ[expr, form].";

FRH::usage =
"FRH[exp_] corresponds to FixedPoint[ReleaseHold, exp],  i.e. FRH removes all
HoldForm and Hold in exp.

Notice that FRH will not be able to reinsert abbreviations if they were
introduced by Collect2 running in parallel mode. For that you need to use FRH2";

FRH2::usage =
"FRH2[exp_, isoNames_] is similar FRH but is specifically designed to reinsert
abbreviations introduced by Collect2 running in parallel mode.

In such cases the user needs to set the IsolateNames option to a list
containing  as many elements as there are parallel kernels. Then, each
parallel kernel introduces its own set of abbreviations that are not known to
other kernels. FRH2 takes the value of the IsolateNames option as its second
arguments, fetches abbreviation definitions from each parallel kernel and
finally substitutes them back into exp.";

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

NTerms::usage=
"NTerms[x] is equivalent to Length if x is a sum; otherwise NTerms[x] returns
1, except NTerms[0] -> 0.";

NumericalFactor::usage =
"NumericalFactor[expr] gives the overall numerical factor of expr.";

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

Variables2::usage=
"Variables2[expr] is like Variables, but it also works on rules and equalities
as well as lists thereof.

Variables2 always applies Union to the output.";

FactorList2::failmsg =
"Error! FactorList2 has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

FCProductSplit::failmsg =
"Error! FCProductSplit has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

FCSplit::failmsg =
"Error! FCSplit has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

FCReloadFunctionFromFile::failmsg =
"Error! FCReloadFunctionFromFile has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

FCMakeSymbols::failmsg =
"Error! FCMakeSymbols has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

FRH2::failmsg =
"Error! FRH2 has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

Factor3::failmsg = "Error! Factor3 has encountered a fatal problem and must \
abort the computation. The problem reads: `1`";

Factor3::nonfact = "Factor3 failed to factor `1`";

Begin["`Package`"];
End[]

(* ------------------------------------------------------------------------ *)

Begin["`SharedTools`Private`"];

SetAttributes[MemSet, HoldFirst];

Options[Cases2] = {
	Heads -> False
};

Options[Coefficient2] = {
	Prefactor -> False
};

Options[Factor2] = {
};


Options[Factor3] = {
	Check 		-> True,
	FCVerbose 	-> False,
	Numerator	-> False,
	RandomPrime	-> 10^8,
	Variables 	-> Automatic
};

Options[FactorList2] = {
	Check		-> True,
	RandomPrime	-> 10^8
};

Options[FCFactorOut] = {
	Factoring	-> Simplify,
	Head 		-> Identity
};

Options[MemSet] = {
	FCMemoryAvailable :> $FCMemoryAvailable
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

Options[FRH2] = {
	IsolateNames -> All
};

Options[FRH] = {
	IsolateNames -> All
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


Coefficient2[ex_, {var_, pow_Integer}, OptionsPattern[]] :=
	Block[{res},
		res = Coefficient[ex, var, pow];

		If[ OptionValue[Prefactor],
			res = var^pow res
		];

		res
	];

Coefficient2[ex_, var_, pow_Integer, opts:OptionsPattern[]] :=
	Coefficient2[ex, {var, pow}, opts];

Coefficient2[ex_, form_, opts:OptionsPattern[]] :=
	Coefficient2[ex, {form, 1}, opts]/; Head[form]=!=List;

Coefficient2[ex_, form1_List, rest__/;!OptionQ[{rest}], opts:OptionsPattern[]] :=
	Coefficient2[Coefficient2[ex,form1,opts],rest, opts];


Coefficient2[ex_, form1_, form2_Integer, rest__/;!OptionQ[{rest}], opts:OptionsPattern[]] :=
	Coefficient2[Coefficient2[ex,{form1,form2},opts],rest, opts];

Coefficient2[ex_, form1_, form2_, rest___, opts:OptionsPattern[]] :=
	Coefficient2[Coefficient2[ex,{form1,1},opts],form2,rest,opts]/;
		Head[form2]=!=Integer && Head[form1]=!=List;


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

FactorList2[poly_, OptionsPattern[]]:=
	Block[{res,allVars,allVarsNum,holdPower,optRandomPrime,nRule,chk},

		optRandomPrime= OptionValue[RandomPrime];

		allVars = Variables2[Cases[poly /. Power -> holdPower, holdPower[x_, _] :> x, Infinity] /. holdPower -> Power];
		allVars = SelectFree[allVars, Variables2[Cases[poly, Power[_, x_] :> x, Infinity]]];
		allVarsNum 	= Table[RandomPrime[optRandomPrime],{i,1,Length[allVars]}];

		nRule = Thread[Rule[allVars, allVarsNum]];

		res = ReplaceRepeated[(FactorList[poly]), {
			List[r1___, {Power[x_, y_], pow_}, r2___] :> List[r1, {x, pow y}, r2],
			List[r1___, {a_, pow1_}, r2___, {a_, pow2_}, r3___] :> List[r1, {a, pow1 + pow2}, r2, r3],
			List[r1___, {a_, pow1_}, r2___]/;Denominator[a]=!=1 :> List[r1, {Numerator[a], pow1}, {Denominator[a], -pow1}, r2]
		}];

		res = ReplaceRepeated[(Factor[res]), {
			List[r1___, {Power[x_, y_], pow_}, r2___] :> List[r1, {x, pow y}, r2],
			List[r1___, {a_, pow1_}, r2___, {a_, pow2_}, r3___] :> List[r1, {a, pow1 + pow2}, r2, r3],
			List[r1___, {a_, pow1_}, r2___]/;Denominator[a]=!=1 :> List[r1, {Numerator[a], pow1}, {Denominator[a], -pow1}, r2]
		}];

		If[	OptionValue[Check],
			chk = (poly/.Dispatch[nRule]) - ((Times @@ (Power @@@ res))/.Dispatch[nRule]);
			If[	Simplify[PowerExpand[chk]]=!=0,
				Print[Simplify[PowerExpand[chk]]];
				Message[FactorList2::failmsg, "Something went wrong when factorizing the polynomial."];
				Abort[]
			];
		];

		res
	];



Factor2[ex_, OptionsPattern[]] :=
	Block[{mi,m1,mp1,num,den,iI,holdPlus,tmp,factorPre},


		factorPre[x_Times]:=
			Map[factorPre, x];

		factorPre[Power[x_,n_]]:=
			factorPre[x]^n;

		factorPre[x_]:=
			Factor[Expand[x]]/; !MemberQ[{Times,Power},Head[x]];

		mi[y_, z__] :=
			(m1 mp1[y,z] )/; (	If[Head[#] === Complex,
									False,
									If[ # < 0,
										True,
										False
									]
								]& @ NumericalFactor[y]);

		If[FreeQ[ex,Complex],
			tmp = ex,
			tmp = ex /. Complex[0,in_] :> iI in
		];

		tmp = tmp /. Plus -> (If[FreeQ[{##}, _^_?Negative] && FreeQ[{##}, Rational],
				holdPlus[##],
				Plus[##]
		]&);

		tmp = Together[tmp] /. holdPlus -> Plus;

		tmp = factorPre[Numerator[tmp]]/factorPre[Denominator[tmp]];

		{num,den} = {Numerator[tmp],Denominator[tmp]};

		{num,den} = {num,den} /. Plus -> holdPlus //. {
			fa_. holdPlus[a_, b_]^n_. holdPlus[a_, c_]^n_. :>
				(fa holdPlus[a^2, -b^2]^n) /; (((b + c) === 0) && IntegerQ[n]),
			fa_. holdPlus[a_, b_]^n_. holdPlus[c_, b_]^n_. :>
				(fa holdPlus[b^2, -a^2]^n) /; (((a + c) === 0) && IntegerQ[n])
		} /. holdPlus -> Plus;

		tmp = num/den;

		tmp = tmp /. Plus -> mi /. mi -> Plus /. m1 -> (-1) /. mp1 -> (-Plus[##]&);

		tmp/.iI->I
	];


Factor3[poly_/;Head[poly] =!= List, opts:OptionsPattern[]] :=
	Factor3[Numerator[poly], opts]/Factor3[Denominator[poly], opts]/; Denominator[poly] =!= 1 && poly=!=0 && OptionValue[Numerator];

Factor3[poly_/;Head[poly] =!= List, opts:OptionsPattern[]] :=
	Numerator[poly]/Factor3[Denominator[poly], opts]/; Denominator[poly] =!= 1 && poly=!=0 && !OptionValue[Numerator];

Factor3[ex_List, opts:OptionsPattern[]] :=
	Factor3[#,opts]& /@ ex;

Factor3[0, OptionsPattern[]]:=
	0;

Factor3[poly_/;Head[poly] =!= List, OptionsPattern[]] :=
	Block[{	factors, vars, polyNew, termsList, pref, res, dummy,
			optVariables, optRandomPrime, varsNum, repRule, allVars,
			f3Verbose, test},

		If [OptionValue[FCVerbose]===False,
			f3Verbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				f3Verbose=OptionValue[FCVerbose]
			];
		];

		optVariables = OptionValue[Variables];
		optRandomPrime = OptionValue[RandomPrime];

		If[	TrueQ[optVariables===Automatic],
			vars = Variables[poly],
			If[	Head[optVariables]===List,
				vars = optVariables,
				Message[Factor3::failmsg, "Incorrection value of the Variables option."];
				Abort[]
			]
		];

		FCPrint[1, "Factor3: Variables: ", vars, FCDoControl->f3Verbose];

		If[	vars==={} || FreeQ2[poly,vars] || !PolynomialQ[poly,vars],
			(*Nothing to do*)
			FCPrint[1, "Factor3: Leaving.", FCDoControl->f3Verbose];
			Return[poly]
		];

		allVars = Variables2[poly];

		FCPrint[1, "Factor3: All variables: ", allVars, FCDoControl->f3Verbose];

	(*
		Using the idea from
		https://mathematica.stackexchange.com/questions/256129/how-to-factor-real-polynomials-over-complex-field/
	*)


		Quiet[factors = Solve[poly == 0, #, Complexes]&/@vars, Solve::svars];

		If[!FreeQ[factors,Root],
			factors = ToRadicals[factors]
		];

		FCPrint[1, "Factor3: Factors: ", factors, FCDoControl->f3Verbose];

		If[	factors==={},
			Message[Factor3::nonfact,ToString[poly,InputForm]];
			Return[poly](*,
			factors = First[factors]*)
		];

		varsNum	= Table[RandomPrime[optRandomPrime],{i,1,Length[vars]}];
		repRule = Thread[Rule[vars, varsNum]];

		FCPrint[2, "Factor3: First numerical replacement rule: ", repRule, FCDoControl->f3Verbose];

		(*For cases such as (4*(5-2*(4-2*eps))*x-2*eps+2) *)
		If[	!FreeQ2[Last/@Flatten[factors],vars],
			Return[poly]
		];

		res = (Times @@ Flatten[factors /. Rule -> Subtract]);

		FCPrint[2, "Factor3: Raw result: ", res, FCDoControl->f3Verbose];

		pref = (poly/res)/.repRule;

		FCPrint[1, "Factor3: Intermediate prefactor: ", pref, FCDoControl->f3Verbose];

		pref = Simplify[pref];

		res = pref res;

		FCPrint[1, "Factor3: Overall prefactor: ", pref, FCDoControl->f3Verbose];
		FCPrint[3, "Factor3: Preliminary result: ", res, FCDoControl->f3Verbose];

		varsNum	= Table[RandomPrime[optRandomPrime],{i,1,Length[allVars]}];
		repRule = Thread[Rule[allVars, varsNum]];

		FCPrint[2, "Factor3: Second numerical replacement rule: ", repRule, FCDoControl->f3Verbose];


		If[	OptionValue[Check],
			test = Simplify[(poly - res) /. repRule];
			FCPrint[3, "Factor3: Check: ", test, FCDoControl->f3Verbose];
			If[	test=!=0,
				Message[Factor3::failmsg, "Something went wrong when factoring the input expression."];
				Abort[]
			];
		];

		res

	]/; Denominator[poly] === 1 && poly=!=0;




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

FCFactorOut[a_ == b_, y__] :=
	FCFactorOut[a,y] == FCFactorOut[b,y];

FCFactorOut[(h:Rule|RuleDelayed)[a_,b_], y__] :=
	With[{zz=FCFactorOut[b,y]}, h[a,zz]];

FCFactorOut[x_List, y__] :=
	FCFactorOut[#, y]& /@ x;

FCFactorOut[expr_,pref_,OptionsPattern[]]:=
	pref OptionValue[Head][OptionValue[Factoring][expr/pref]];/; !MemberQ[{Equal,Rule,RuleDelayed,List}, Head[expr]];

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

		If[	!MatchQ[expr,_Times],
			If[	NTerms[expr,Expand->False] > 1,
				Message[FCProductSplit::failmsg,"The input expression is not a product"];
				Abort[]
			];
		];

		If[	!FreeQ[exprAsList,Power],

			(*
			Using List instead of list may mess up powers inside functions.
			We don't want to touch those at all
			*)

			exprAsList = list@@(expr*dummy1*dummy2);
			exprAsList /. Power -> pow //. {
				list[a___,pow[b_,n_Integer?Positive],c___] :> list[a,Sequence@@ConstantArray[b,n],c],
				list[a___,pow[b_,n_Integer?Negative],c___] :> list[a,Sequence@@ConstantArray[1/b,-n],c]
			} /. pow -> Power;
			exprAsList = exprAsList/.list->List,

			exprAsList = List@@(expr*dummy1*dummy2)
		];


		free = SelectFree[exprAsList,vars] /. dummy1|dummy2 :> Unevaluated[Sequence[]];
		notfree = SelectNotFree[exprAsList,vars];

		free 	= Times@@free;
		notfree = Times@@notfree;

		If[ Factor[free*notfree - expr] || ! FreeQ2[free, vars],
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

FreeQ2[_,{}] :=
	True;

FreeQ2[x_, y_]	:=
	FreeQ[x, y] /; Head[y] =!= List;

FreeQ2[x_, {y_}] :=
	FreeQ[x, y];
(*
This recursive option is not compatible with parallelized calculations involving very long (>5K elements) lists
FreeQ2[x_, {y_, z__}] :=
	Block[{$IterationLimit=Infinity, $RecursionLimit=Infinity},
		If[FreeQ[x, y],
			FreeQ2[x, {z}],
			False
		]
	];
*)
FreeQ2[x_, {y_, z__}] :=
	FreeQ[x, Alternatives@@{y,z}];


FRH[x_, OptionsPattern[]] :=
	FixedPoint[ReleaseHold, x]/; OptionValue[IsolateNames]===All;

FRH[x_, OptionsPattern[]] :=
	FixedPoint[ReplaceRepeated[x, HoldForm[y_[z___]] /; ! FreeQ2[HoldForm[y], Flatten[{OptionValue[IsolateNames]}]] :> y[z]] &, x]/; OptionValue[IsolateNames]=!=All;

FRH2[ex_, isoSymbols_List/;!OptionQ[isoSymbols], OptionsPattern[]]:=
Block[{allHolds,holdsSorted,repRules,res},

	If[!$ParallelizeFeynCalc,
		Message[FRH2::failmsg,"FRH2 only works in parallel mode."];
		Abort[]
	];

	If[	Length[isoSymbols]=!=$KernelCount,
		Message[FRH2::failmsg,"The number of abbreviation symbols must match the number of parallel kernels."];
		Abort[]

	];

	allHolds=SelectNotFree[Cases2[ex,HoldForm],isoSymbols];
	holdsSorted=Map[SelectNotFree[allHolds,#]&,isoSymbols];
	repRules=Table[With[{exp=holdsSorted[[i]],isoName=isoSymbols[[i]]},ParallelEvaluate[Thread[Rule[exp,FRH[exp,IsolateNames->isoName]]],i,DistributedContexts->None]],{i,1,$KernelCount}];
	res = ex/.Dispatch[Flatten[repRules]];
	res
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
	]/; MatchQ[OptionValue[FCMemoryAvailable],_Integer?NonNegative];

MemSet[_,_, OptionsPattern[]]:=
	(
	Message[FeynCalc::failmsg,"The value of $FCMemoryAvailable must be a nonnegative integer."];
	Abort[]
	)/; !MatchQ[OptionValue[FCMemoryAvailable],_Integer?NonNegative];

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

SelectFree[0,__] :=
	0;

SelectFree[a_, b__] :=
	Block[{dum1,dum2, select(*, $IterationLimit=Infinity, $RecursionLimit=Infinity*)},
		select[x_, y_ /; Head[y] =!= List] :=
			Select[x, FreeQ[#, y]&];
		select[x_, y_List ] :=
			Select[x, FreeQ2[#, y]&];
		select[x_, y_, z__]  :=
			Select[x, FreeQ2[#, Flatten[{y, z}]]&];
		If[(Head[a] === Plus) || (Head[a] === Times) || (Head[a] === List),
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
	Block[{tmp, res, null1, null2(*, $IterationLimit=Infinity, $RecursionLimit=Infinity*)},
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
	Block[{dum1,dum2, select(*, $IterationLimit=Infinity, $RecursionLimit=Infinity*)},
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
	Block[{tmp, res, null1, null2(*, $IterationLimit=Infinity, $RecursionLimit=Infinity*)},
		tmp = Expand2[x,Flatten[{args}]];

		If[	Head[tmp]===Plus,
			res = SelectNotFree[tmp,args],
			res = SelectNotFree[tmp+null1+null2,args] /. null1|null2->0
		];
		res
	]/; Head[x]=!=List && x=!=0;

Variables2[expr_Plus, opts : OptionsPattern[]] :=
	Union[Flatten[Variables2[#, opts] & /@ List@@expr]];

Variables2[expr_List, opts : OptionsPattern[]] :=
	Union[Flatten[Variables2[#, opts] & /@ expr]];

Variables2[expr_, OptionsPattern[]] :=
	Union[Variables[expr]] /; !MemberQ[{List, Equal, Rule, RuleDelayed, Re, Im}, Head[expr]];

Variables2[(Equal | Rule | RuleDelayed)[a_, b_], OptionsPattern[]] :=
	Union[Variables[a], Variables[b]];

Variables2[(Re | Im)[a_], OptionsPattern[]] :=
	Variables[a];

FCPrint[1, "SharedTools loaded."];
End[]

