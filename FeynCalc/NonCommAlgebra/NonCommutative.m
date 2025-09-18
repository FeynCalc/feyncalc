(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: NonCommutative													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Non-commutative quantities									*)

(* ------------------------------------------------------------------------ *)

FCAntiCommutator::usage =
"FCAntiCommutator[x, y] = c defines the anti-commutator of the non commuting
objects x and y.";

FCCommutator::usage =
"FCCommutator[x, y] = c defines the commutator between the (non-commuting)
objects x and y.";

CommutatorExplicit::usage=
"CommutatorExplicit[exp] substitutes any FCCommutator and FCAntiCommutator in
exp by their definitions.";

CommutatorOrder::usage=
"CommutatorOrder[exp] orders any FCCommutator and FCAntiCommutator
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
anti-commutator of a and b.";

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

FCAntiCommutator /:
	Set[FCAntiCommutator[a_, b_] , c_] :=
	Block[ {nd, acom},
		nd = (RuleDelayed @@ {HoldPattern @@ {acom[a, b]},c}) /. acom -> FCAntiCommutator;
		If[FreeQ2[DownValues[FCAntiCommutator], {nd,Verbatim[nd]}],
				PrependTo[DownValues[FCAntiCommutator], nd]
		];
		c
	];

FCAntiCommutator /:
	MakeBoxes[ FCAntiCommutator[a_, b_], TraditionalForm] :=
		TBox["{", a, ",", "\[MediumSpace]", b, "}"];

FCCommutator /:
	Set[FCCommutator[a_, b_] , c_] :=
		Block[ {nd, com},
			nd = (RuleDelayed @@ {HoldPattern @@ {com[a, b]}, c}) /. com -> FCCommutator;
			If[ FreeQ2[DownValues[FCCommutator], {nd,Verbatim[nd]}],
				PrependTo[DownValues[FCCommutator], nd]
			];
			c
		];

FCCommutator/: MakeBoxes[FCCommutator[a_, b_], TraditionalForm] :=
	RowBox[ {"[","\[NoBreak]", TBox[a] ,"\[NoBreak]", ",", TBox[b], "\[NoBreak]", "]"}];

CommutatorExplicit[exp_] :=
	exp /. {
		FCCommutator :> ((DOT[#1, #2] - DOT[#2, #1])&),
		FCAntiCommutator :> ((DOT[#1, #2] + DOT[#2, #1])&)
	};

CommutatorOrder[exp_] :=
	exp /. {
		FCCommutator[a_,b_]/; !OrderedQ[{a,b}] :> - FCCommutator[b,a],
		FCAntiCommutator[a_,b_]/; !OrderedQ[{a,b}] :> FCAntiCommutator[b,a]
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
		exp = HoldPattern[comm[a, b]] /. comm -> FCCommutator;
		dw = SelectFree[DownValues[FCCommutator], exp];
		DownValues[FCCommutator] = dw;
	];

UnDeclareAntiCommutator[a_, b_] :=
	Block[{exp, acomm, dw},
		exp = HoldPattern[acomm[a, b]] /. acomm -> FCAntiCommutator;
		dw = SelectFree[DownValues[FCAntiCommutator], exp];
		DownValues[FCAntiCommutator] = dw;
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
		DownValues[FCAntiCommutator] = FeynCalc`Package`initialAntiCommutatorDownValues;
	);

UnDeclareAllCommutators[OptionsPattern[]] :=
	(
		DownValues[FCCommutator] = FeynCalc`Package`initialCommutatorDownValues;
	);

FCMatrixProduct[x_] :=
	x;
FCMatrixProduct[x_, y_] :=
	Inner[Dot, x, y];
FCMatrixProduct[x_, y_, z__] :=
	FCMatrixProduct[x, FCMatrixProduct[y, z]];

FCPrint[1,"NonCommutative loaded"];
End[]
