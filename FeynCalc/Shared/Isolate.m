(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Isolate															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: 	Isolate introduces abbreviations for common
				subexpressions												*)

(* ------------------------------------------------------------------------ *)

Isolate::usage=
"Isolate[expr] substitutes abbreviations KK[i] for all Plus[...] (sub-sums) in
expr. The inserted KK[i] have head HoldForm. Isolate[expr, varlist]
substitutes KK[i] for all subsums in expr which are free of any occurrence of
a member of the list varlist. Instead of KK any other head or a list of names
of the abbreviations may be specified with the option IsolateNames.";

IsolateFast::usage =
"IsolateFast is an option of Isolate and other functions using Isolate. When
set to True and when varlist is empty, Isolate will not attempt to recognize
existing abbreviations, but will immediately abbreviate the whole expression
instead. This is useful for very large expressions or prefactors, where
Isolate would otherwise require a lot of time to finish.";

IsolatePrint::usage =
"IsolatePrint is an option of Isolate. If it is set to OutputForm (or any other
*Form) the definitions of the abbreviations are printed during the operation
of Isolate.

The setting IsolatePrint -> False suppresses printing.";

IsolateSplit::usage =
"IsolateSplit is an option for Isolate. Its setting determines the maximum
number of characters of FortranForm[expr] which are abbreviated by Isolate. If
the expression is larger than the indicated number, it is split into smaller
pieces and onto each subsum Isolate is applied.

With the default setting IsolateSplit -> Infinity no splitting is done.";

IsolateTimes::usage =
"IsolateTimes is an option for Isolate and other functions using Isolate. If it
is set to True, Isolate will be applied also to pure products.";

IsolatePlus::usage =
"IsolatePlus is an option for Isolate and other functions using Isolate. If it
is set to True, Isolate will split sums that contain elements from vlist, to
be able to abbreviate the vlist-free part.";

Begin["`Package`"];
End[]
(* ------------------------------------------------------------------------ *)

Begin["`Isolate`Private`"];

Options[Isolate] = {
	IsolateFast		-> False,
	IsolateNames	-> KK,
	IsolatePlus 	-> False,
	IsolatePrint	-> False,
	IsolateSplit 	-> Infinity,
	IsolateTimes 	-> False
};

Isolate[OptionsPattern[]] :=
	(Message[Isolate::argt, Isolate, 0, 1, 2]; Abort[]);

Isolate[_,_,a___,z:Except[_?OptionQ], OptionsPattern[]] :=
	(Message[Isolate::argt, Isolate, Length[{a}]+3, 1, 2]; Abort[])/; !OptionQ[z]

Isolate[y_HoldForm^n_., _, OptionsPattern[]] :=
	y^n;

Isolate[x_?NumberQ, _, OptionsPattern[]] :=
	x;

Isolate[x_Symbol, _, OptionsPattern[]] :=
	x;

Isolate[x_ /; NumericalFactor[x] =!=1, z_, opts:OptionsPattern[]] :=
	(NumericalFactor[x] Isolate[x/NumericalFactor[x], z, opts]) /; x=!=0 && (!OptionQ[z] || z==={})

Isolate[ex_, (opts:OptionsPattern[])/;opts=!={}] :=
	Isolate[ex, {}, opts];

Isolate[ex_, var:Except[_?OptionQ], opts:OptionsPattern[]] :=
	Isolate[ex, {var}, opts]/; Head[var] =!= List;

Isolate[ exp_ /; Apply[Or[#===1, #===0]&, {NumericalFactor[exp]}], vars_List, opts:OptionsPattern[]] :=
	Block[{	plush,vlist,res,split,kk, di=1, defhead, abbprint, holdformlist = {},
			hres, nhres, remche, timesSplit, plusSplit, tmpIso, tmpKeep,
			isoIgnore, repRule},

		kk = OptionValue[IsolateNames];
		abbprint = OptionValue[IsolatePrint];
		split = OptionValue[IsolateSplit];

		vlist = Flatten[{vars}];

		If[	Head[kk] === List,
			kk = Flatten[kk];
			If[Length[Union[kk]] =!= Length[kk],
				kk = Union[kk]
			];
		];

		If[	abbprint === True,
			abbprint = OutputForm
		];

		(* This split-off is useful for various reasons ... *)
		plush[x__] :=
			If[	!FreeQ2[{x}, vlist],
				If[	OptionValue[IsolatePlus],
					(* special sum splitting for better isolation *)
					plusSplit[x],
					Plus[x]
				],
				If[	(checkIsolate[x, split] === True ) && (Length[{x}] > 4) && (split =!= Infinity),
					Isolate[Drop[Plus[x], Round[Length[Plus[x]]/2]] + Isolate[Take[Plus[x],
					Round[Length[Plus[x]]/2]], vars, IsolatePrint->False, opts], vars, IsolatePrint->False, opts],
					If[	Head[kk] === List,
						If[	Union[Head/@kk]==={Plus},
							kk = {}
						];
						remIsolatesave[Plus[x], kk] /. remIsolatesave -> remIsolate,
						remIsolate[Plus[x], kk]
					]
				]
			];

		timesSplit[x__] :=
		If[	!FreeQ2[{x}, Join[vlist, {isoIgnore}]],
			tmpIso = SelectFree[{x}, isoIgnore, Sequence @@ vlist];
			tmpKeep = SelectNotFree[{x}, Sequence @@ vlist];
			If[	tmpIso === {} || MatchQ[tmpIso, {_?NumericQ}],
				Times[Sequence @@ tmpIso],
				remIsolate[Times[Sequence @@SelectFree[{x}, isoIgnore, Sequence @@ vlist]],kk]
			]*Times[ Sequence @@ SelectNotFree[{x}, isoIgnore, Sequence @@ vlist]],
			remIsolate[Times[x], kk]
		];

		plusSplit[x__] :=
			(	tmpIso = SelectFree[{x}, Sequence @@ vlist,kk];
				tmpKeep = SelectNotFree[{x}, Sequence @@ vlist,kk];
				Plus[
					If[tmpIso==={},0,remIsolate[Plus[Sequence@@tmpIso],kk]],
					Plus[Sequence@@tmpKeep]
				]
			);


		res = exp;

		If[	!OptionValue[IsolateFast] || vlist =!={},
			res = exp /. Plus -> plush /. plush -> Plus
		];

		If [OptionValue[IsolateTimes],
			(* 	we don't want to touch products in existing abbreviations, this is why we need to hide
				those while we apply timesSplit and then put them back *)
			repRule = Map[Rule[#1, isoIgnore[Unique["isoIgnore"]]] &,Union[Cases[res, HoldForm[__], Infinity]]];
			res = (res /. repRule /. Times -> timesSplit) /. (Reverse /@ repRule)
		];

		If[Head[res] =!= HoldForm && vlist === {},
			res = remIsolate[res, kk]
		];

		(* do only sums here ... *)
		If[	abbprint=!=False,
			holdformlist = Cases2[res, HoldForm];
			hres = ReleaseHold[res];
			holdformlist = Join[holdformlist, Cases2[hres, HoldForm]];
			While[(nhres = ReleaseHold[hres]) =!= hres,
				hres = nhres;
				holdformlist = Join[holdformlist, Cases2[hres, HoldForm]];
			];
			holdformlist = Union[holdformlist];

			FCPrint[0, "\n", UseWriteString -> True];
			For[i = 1, i <= Length[holdformlist], i++,
				FCPrint[0, "       ", holdformlist[[i]], " = ", abbprint[ReleaseHold[holdformlist[[i]]]]];
				FCPrint[0, "\n", UseWriteString -> True];
			];
		];
		res
	];

checkIsolate[x__, i_] :=
	If[Head[i] === Integer,
		If[	Length[Characters[ToString[FortranForm[Plus[x]]]]]>i,
			True,
			False
		],
		If[	Head[i] === Complex,
			If[	Length[{x}] > Im[i],
				True,
				False,
				False
			],
			False
		]
	];

tokIsolate[y_, ab_, uh_] :=
	ab[ToExpression[StringJoin@@Drop[Characters[ToString[y]],Length[Characters[ToString[uh]]]]]];

remIsolate[x_,{}] :=
	remIsolate[x, If[Head[IsolateNames /. Options[Isolate]]===List, KK, IsolateNames /. Options[Isolate]]];

Clear[remIsolatesave];

(* Isolate for Times *)
remIsolate[x_, {___Times, abb_ /; Head[abb] =!= Times, ___}] :=
	Block[{re},
		re = HoldForm @@ {abb};
		Set@@{abb, x};
		Set@@{remIsolatesave[x, _], re};
		re
	];

(* Isolate for Plus *)
remIsolate[x_, {___Plus, abb_ /; Head[abb] =!= Plus, ___}] :=
	Block[{re},
		re = HoldForm @@ {abb};
		Set@@{abb, x};
		Set @@ {remIsolatesave[x, _], re};
		re
	];

remIsolate[x_, abb_ /; Head[abb] =!= List] :=
	Block[{re, h, temp},
		If[Length[(re = Select[DownValues @@ {abb}, (#[[2]]===x) &])] > 0,
			re = re[[1,1]] /. {Literal :> HoldForm, HoldPattern :> HoldForm},
			If[ Head[abb]===Symbol,
				temp = tokIsolate[ uni[ToString[abb]] /. uni->Unique,abb, abb],
				temp = tokIsolate[Unique["dude"], abb, "dude"]
			];
			re = HoldForm @@ {temp};
			Set@@{temp, x}
		];
		re
	];

FCPrint[1,"Isolate.m loaded"];
End[]
