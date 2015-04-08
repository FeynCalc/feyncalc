(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Isolate *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Isolate introduces abbreviations for common
						subexpressions *)

(* ------------------------------------------------------------------------ *)

Isolate::usage=
"Isolate[expr] substitutes abbreviations KK[i] for all
Plus[...] (sub-sums) in expr. The inserted KK[i] have head HoldForm.
Isolate[expr, varlist] substitutes
KK[i] for all subsums in expr which are free of any occurence
of a member of the list varlist. Instead of KK any other head or
a list of names
of the abbreviations may be specified
with the option IsolateNames.";

IsolatePrint::usage =
"IsolatePrint is an option of Isolate.
If it is set to OutputForm (or any other *Form) the definitions
of the abbreviations are printed during the operation of Isolate.
The setting IsolatePrint -> False suppresses printing.";

IsolateSplit::usage =
"IsolateSplit is an option for Isolate. Its setting determines the
maximum number of characters of FortranForm[expr] which are
abbreviated by Isolate. If the expression is larger than the
indicated number, it is split into smaller pieces and onto
each subsum Isolate is applied.
With the default setting IsolateSplit -> Infinity no splitting
is done.";

Begin["`Package`"]
End[]
(* ------------------------------------------------------------------------ *)

Begin["`Isolate`Private`"]

Isolate[___Rule] :=
	soso /; Message[Isolate::argt, Isolate, 0, 1, 2];

Isolate[_,_,a___,z_/;Head[z] =!= Rule, ___Rule] :=
	soso /; Message[Isolate::argt, Isolate, Length[{a}]+3, 1, 2];

Options[Isolate] = {
	IsolateNames -> KK,
	IsolatePrint -> False,
	IsolateSplit -> Infinity
};

Isolate[y_HoldForm^n_., ___] :=
	y^n;
(*
(* for the moment *)
Isolate[a_, ru___Rule] := Isolate[a, dummdumm, ru];
*)
(* this gives Problems if x has large HoldForm's ...
	Isolate[n_?NumberQ x_, y__]  := n Isolate[x, y];
*)
Isolate[x_?NumberQ, __] :=
	x;
Isolate[x_Symbol, __] :=
	x;
Isolate[x_ /; NumericalFactor[x] =!=1, y__ ] :=
	(NumericalFactor[x] Isolate[x/NumericalFactor[x], y]) /; x=!=0;

Isolate[ex_, r___Rule ] :=
	Isolate[ex, {}, r];
Isolate[ex_, var_, r___Rule] :=
	Isolate[ex, {var}, r]/;	(Head[var] =!= Rule) && Head[var] =!= List;

Isolate[ exp_ /; Apply[Or[#===1, #===0]&, {NumericalFactor[exp]}],
vars_List, ops___Rule] :=
	Block[{plush,vlist,res,split,kk, di=1, defhead, abbprint,
		holdformlist = {}, hres, nhres, remche},
		kk = IsolateNames/.{ops}/.Options[Isolate];
		abbprint = IsolatePrint /. {ops} /. Options[Isolate];
		split = IsolateSplit/.{ops}/.Options[Isolate];
		If[Head[kk] === List,
			kk = Flatten[kk];
			If[Length[Union[kk]] =!= Length[kk],
				kk = Union[kk]
			];
		];
		If[abbprint === True,
			abbprint = OutputForm
		];
		vlist = Flatten[{vars}];
		(* This split-off is useful for various reasons ... *)
		plush[x__] :=
			If[!FreeQ2[{x}, vlist],
				Plus[x],
				If[(checkIsolate[x, split] === True ) && (Length[{x}] > 4) && (split =!= Infinity),
					Isolate[Drop[Plus[x], Round[Length[Plus[x]]/2]] + Isolate[Take[Plus[x],
					Round[Length[Plus[x]]/2]], vars, IsolatePrint->False, ops],	vars,IsolatePrint->False, ops],
					If[Head[kk] === List,
						If[Union[Head/@kk]==={Plus},
								kk = {}
						];
						remIsolatesave[Plus[x], kk] /. remIsolatesave -> remIsolate,
						remIsolate[Plus[x], kk]
					]
				]
			];
		(* If[vars === {}, res = exp, *)
		res = exp /. Plus -> plush /. plush -> Plus;
		(*	];*)
		If[Head[res] =!= HoldForm && vlist === {},
			res = remIsolate[res, kk]
		];
		(* do only sums here ... *)
		If[abbprint =!= False,
			holdformlist = Cases2[res, HoldForm];
			hres = ReleaseHold[res];
			holdformlist = Join[holdformlist, Cases2[hres, HoldForm]];
			While[(nhres = ReleaseHold[hres]) =!= hres,
				hres = nhres;
				holdformlist = Join[holdformlist, Cases2[hres, HoldForm]];
			];
			holdformlist = Union[holdformlist];
			WriteString["stdout", "\n"];
			For[i = 1, i <= Length[holdformlist], i++,
				Print["       ", holdformlist[[i]], " = ",
				abbprint[ReleaseHold[holdformlist[[i]]]]];
				WriteString["stdout", "\n"];
			];
		];
		res
	];

(* three extra "global" functions *)
checkIsolate[x__, i_] :=
	If[Head[i] === Integer,  (* LGF *)
		If[Length[Characters[ToString[FortranForm[Plus[x]]]]]>i,
			True,
			False
		],
		If[Head[i] === Complex,
			If[Length[{x}] > Im[i],
				True,
				False,
				False
			],
			False
		]
	];

tokIsolate[y_, ab_, uh_] :=
	ab[ToExpression[ StringJoin@@Drop[Characters[ToString[y]],Length[Characters[ToString[uh]]]]]];

remIsolate[x_,{}] :=
	remIsolate[x, If[Head[IsolateNames /. Options[Isolate]]===List, KK, IsolateNames /. Options[Isolate]]];

Clear[remIsolatesave];

remIsolate[x_, {___Plus, abb_ /; Head[abb] =!= Plus, ___}] :=
	Block[{re},
		re = HoldForm @@ {abb};
		Set@@{abb, x};
		Set @@ {remIsolatesave[x, _], re};
		re
	];

remIsolate[x_, abb_ /; Head[abb] =!= List] :=
	Block[{re, h},(*LGF*)
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
