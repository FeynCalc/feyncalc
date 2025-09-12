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
"UnDeclareAllAntiCommutators[] undeclares all user-defined anti-commutators.";

FCCommutator::notsync =
"You are using FeynCalc in the parallel mode, but the FCCommutator or FCAntiCommutator values \
are not synchronized between the master kernel and subkernels. \
This usually happens if such definitions have been set before activating the \
parallel mode. Please clear the existing definitions and redefine your symbols.";


(* ------------------------------------------------------------------------ *)
Begin["`Package`"];
End[]

Begin["`NonCommutative`Private`"];

FCAntiCommutator /:
	Set[FCAntiCommutator[a_, b_] , c_] :=
		Block[ {},

				If[	$ParallelizeFeynCalc,
					With[{xxx=a,yyy=b,zzz=c},
								ParallelEvaluate[setCommAcomm[xxx, yyy, zzz,FCAntiCommutator];,DistributedContexts -> None]
							];
				];
				setCommAcomm[a, b, c, FCAntiCommutator];

				c
			];

FCAntiCommutator /:
	MakeBoxes[ FCAntiCommutator[a_, b_], TraditionalForm] :=
		TBox["{", a, ",", "\[MediumSpace]", b, "}"];

FCCommutator /:
	Set[FCCommutator[a_, b_] , c_] :=
		Block[ {},

			If[	$ParallelizeFeynCalc,
				With[{xxx=a,yyy=b,zzz=c},
							ParallelEvaluate[setCommAcomm[xxx, yyy, zzz, FCCommutator];,DistributedContexts -> None]
						];
			];
			setCommAcomm[a, b, c, FCCommutator];

			c
		];

FCCommutator/: MakeBoxes[FCCommutator[a_, b_], TraditionalForm] :=
	RowBox[ {"[","\[NoBreak]", TBox[a] ,"\[NoBreak]", ",", TBox[b], "\[NoBreak]", "]"}];

setCommAcomm[a_,b_,c_,(type:FCCommutator|FCAntiCommutator)]:=
	Block[{nd, hold},
		nd = (RuleDelayed @@ {HoldPattern @@ {hold[a, b]}, c}) /. hold -> type;
		If[ FreeQ2[DownValues[type], {nd,Verbatim[nd]}],
			PrependTo[DownValues[type], nd]
		];

		(* If we are in the parallel mode and on the master kernel, need to check whether these values have already been set on subkernels*)
		If[$ParallelizeFeynCalc && ($KernelID===0),
			If[!FCValuesSynchronizedQ[{type}, DownValues],
				Message[FCCommutator::notsync]
			]
		];




		c
	];


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
