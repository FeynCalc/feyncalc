(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: test for non-commutativity *)

(* ------------------------------------------------------------------------ *)

AntiCommutator::usage =
"AntiCommutator[x, y] = c  defines the anti-commutator of the \
non-commuting objects x and y. \
Settings of AntiCommutator (e.g.AntiCommutator[a,b]=c) \
are recognized by DotSimplify.";

Commutator::usage =
"Commutator[x, y] = c  defines the commutator between the non-commuting \
objects x and y.";

CommutatorExplicit::usage=
"CommutatorExplicit[exp] substitutes any Commutator and AntiCommutator \
in exp by their definitions.";

DeclareNonCommutative::usage =
"DeclareNonCommutative[a, b, ...] declares a,b, ... to be \
noncommutative, i.e., DataType[a,b, ...,  NonCommutative] is set to \
True.";

NonCommFreeQ::usage =
"NonCommFreeQ[exp] yields True if exp contains no non-commutative objects \
(i.e. those objects which are listed in $NonComm) or only non-commutative \
objects inside DiracTrace's or SUNTrace's.";

NonCommQ::usage =
"NonCommQ[exp] yields True if exp contains non-commutative objects \
(i.e. those objects which are listed in $NonComm) not inside \
DiracTrace's or SUNTrace's.";

NonCommutative::usage=
"NonCommutative is a data type which may be used, e.g.,  as: \
DataType[x, NonCommutative] = True.";

UnDeclareNonCommutative::usage =
"UnDeclareNonCommutative[a, b, ...] undeclares a,b, ... to be \
noncommutative, i.e., DataType[a,b, ..., NonCommutative] = False \
is performed.";


(* ------------------------------------------------------------------------ *)
Begin["`Package`"]
End[]

Begin["`NonCommutative`Private`"]

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

(* Have traces treated as commutating objects. F.Orellana, 11/9-2002. *)
excludeTraces = a : (DiracTrace |  SUNTrace)[__] :>
(a /. (Rule[#, ToString[#]] & /@{DiracTrace, SUNTrace, Sequence @@ $NonComm}));

NonCommFreeQ[_?NumberQ] :=
	True;
NonCommFreeQ[x_] :=
	MemSet[NonCommFreeQ[x], FreeQ2[x /. excludeTraces, $NonComm]];

(*Comment: Because of this MemSet, NonCommFreeQ is updated when setting a new
	DataType[x,NonCommutative]=True or DataType[x,NonCommutative]=False.
	Rolf implemented it this way to gain speed I suppose. F.Orellana*)

NonCommQ[_?NumberQ]   :=
	False;
NonCommQ[x_] :=
	MemSet[NonCommQ[x],	!FreeQ2[x /. excludeTraces, $NonComm]];

FCPrint[1,"NonCommutative loaded"];
End[]
