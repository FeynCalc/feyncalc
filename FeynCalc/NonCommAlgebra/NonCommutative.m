(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: NonCommutative													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Non-commutative quantities									*)

(* ------------------------------------------------------------------------ *)

AntiCommutator::usage =
"AntiCommutator[x, y] = c defines the anti-commutator of the non commuting
objects x and y.";

Commutator::usage =
"Commutator[x, y] = c defines the commutator between the (non-commuting)
objects x and y.";

CommutatorExplicit::usage=
"CommutatorExplicit[exp] substitutes any Commutator and AntiCommutator in exp
by their definitions.";

CommutatorOrder::usage=
"CommutatorOrder[exp] orders any Commutator and AntiCommutator
lexicographically.";

DeclareNonCommutative::usage =
"DeclareNonCommutative[a, b, ...] declares a,b, ... to be non-commutative,
i.e., DataType[a,b, ..., NonCommutative] is set to True.";

FCMatrixProduct::usage=
"FCMatrixProduct[mat1, mat2, ...] can be used to obtain products of matrices
with entries containing noncommutative symbols. Using the usual Dot on such
matrices would otherwise destroy the original ordering.

The resulting expression can be then further simplified using DotSimplify.";

NonCommFreeQ::usage =
"NonCommFreeQ[exp] yields True if exp contains no non-commutative objects (i.e.
those objects which are listed in $NonComm) or only non-commutative objects
inside DiracTraces or SUNTraces.";

NonCommHeadQ::usage =
"NonCommHeadQ[exp] yields True if the head of exp is a non-commutative object
or Dot.";

NonCommQ::usage =
"NonCommQ[exp] yields True if exp contains non-commutative objects (i.e. those
objects which are listed in $NonComm) not inside DiracTraces or SUNTraces.";

NonCommutative::usage=
"NonCommutative is a data type which may be used, e.g.,  as DataType[x,
NonCommutative] = True.";

UnDeclareNonCommutative::usage =
"UnDeclareNonCommutative[a, b, ...] undeclares a,b, ... to be noncommutative,
i.e., DataType[a,b, ..., NonCommutative] is set to False.";

UnDeclareAntiCommutator::usage =
"UnDeclareAntiCommutator[a, b] undeclares the value assigned to the
anticommutator of a and b.";

UnDeclareCommutator::usage =
"UnDeclareCommutator[a, b] undeclares the value assigned to the commutator of a
and b.";

UnDeclareAllCommutators::usage =
"UnDeclareAllCommutators[] undeclares all user-defined commutators.";

UnDeclareAllAntiCommutators::usage =
"UnDeclareAllAntiCommutators[] undeclares all user-defined anticommutators.";

(* ------------------------------------------------------------------------ *)
Begin["`Package`"];
End[]

Begin["`NonCommutative`Private`"];

AntiCommutator /:
	Set[AntiCommutator[a_, b_] , c_] :=
	Block[ {nd, acom},
		nd = (RuleDelayed @@ {HoldPattern @@ {acom[a, b]},c}) /. acom -> AntiCommutator;
		If[FreeQ2[DownValues[AntiCommutator], {nd,Verbatim[nd]}],
				PrependTo[DownValues[AntiCommutator], nd]
		];
		c
	];

AntiCommutator /:
	MakeBoxes[ AntiCommutator[a_, b_], TraditionalForm] :=
		TBox["{", a, ",", "\[MediumSpace]", b, "}"];

Commutator /:
	Set[Commutator[a_, b_] , c_] :=
		Block[ {nd, com},
			nd = (RuleDelayed @@ {HoldPattern @@ {com[a, b]}, c}) /. com -> Commutator;
			If[ FreeQ2[DownValues[Commutator], {nd,Verbatim[nd]}],
				PrependTo[DownValues[Commutator], nd]
			];
			c
		];

Commutator/: MakeBoxes[Commutator[a_, b_], TraditionalForm] :=
	RowBox[ {"[","\[NoBreak]", TBox[a] ,"\[NoBreak]", ",", TBox[b], "\[NoBreak]", "]"}];

CommutatorExplicit[exp_] :=
	exp /. {
		Commutator :> ((DOT[#1, #2] - DOT[#2, #1])&),
		AntiCommutator :> ((DOT[#1, #2] + DOT[#2, #1])&)
	};

CommutatorOrder[exp_] :=
	exp /. {
		Commutator[a_,b_]/; !OrderedQ[{a,b}] :> - Commutator[b,a],
		AntiCommutator[a_,b_]/; !OrderedQ[{a,b}] :> AntiCommutator[b,a]
	};


DeclareNonCommutative[] :=
	(Message[DeclareNonCommutative::argrx, DeclareNonCommutative, 0, "1 or more"];
	Abort[]);

DeclareNonCommutative[b__] :=
	(Map[Set[DataType[#, NonCommutative], True]&, Flatten[{b}]]; Null);

UnDeclareNonCommutative[] :=
	(Message[UnDeclareNonCommutative::argrx, UnDeclareNonCommutative, 0, "1 or more"];
	Abort[]);

UnDeclareNonCommutative[b__] :=
	(Map[Set[DataType[#, NonCommutative], False]&, Flatten[{b}]]; Null);

UnDeclareCommutator[a_, b_] :=
	Block[{exp, comm, dw},
		exp = HoldPattern[comm[a, b]] /. comm -> Commutator;
		dw = SelectFree[DownValues[Commutator], exp];
		DownValues[Commutator] = dw;
	];

UnDeclareAntiCommutator[a_, b_] :=
	Block[{exp, acomm, dw},
		exp = HoldPattern[acomm[a, b]] /. acomm -> AntiCommutator;
		dw = SelectFree[DownValues[AntiCommutator], exp];
		DownValues[AntiCommutator] = dw;
	];

excludeTraces = {_DiracTrace :> Unique["DiracTrace"],
		_PauliTrace :> Unique["PauliTrace"], _SUNTrace :> Unique["SUNTrace"]};

NonCommFreeQ[_?NumberQ] :=
	True;

NonCommFreeQ[x_] :=
	MemSet[NonCommFreeQ[x],
		If[ !FreeQ2[x,{DiracTrace,PauliTrace,SUNTrace}],
				FreeQ2[x /. excludeTraces, $NonComm],
				FreeQ2[x, $NonComm]
		]
	];

NonCommQ[_?NumberQ]   :=
	False;

NonCommQ[x_] :=
	MemSet[NonCommQ[x],
		If[ !FreeQ2[x,{DiracTrace,PauliTrace,SUNTrace}],
				!FreeQ2[x /. excludeTraces, $NonComm],
				!FreeQ2[x, $NonComm]
		]
	];

NonCommHeadQ[x_] :=
	MemberQ[Join[$NonComm,{DOT,Dot}],Head[x]];

UnDeclareAllAntiCommutators[OptionsPattern[]] :=
	(
		DownValues[AntiCommutator] = FeynCalc`Package`initialAntiCommutatorDownValues;
	);

UnDeclareAllCommutators[OptionsPattern[]] :=
	(
		DownValues[Commutator] = FeynCalc`Package`initialCommutatorDownValues;
	);

FCMatrixProduct[x_] :=
	x;
FCMatrixProduct[x_, y_] :=
	Inner[Dot, x, y];
FCMatrixProduct[x_, y_, z__] :=
	FCMatrixProduct[x, FCMatrixProduct[y, z]];

FCPrint[1,"NonCommutative loaded"];
End[]
