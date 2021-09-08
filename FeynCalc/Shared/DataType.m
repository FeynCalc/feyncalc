(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DataType is just a data type *)

(* ------------------------------------------------------------------------ *)

FreeIndex::usage =
"FreeIndex is a datatype which is recognized by Contract. Possible use:
DataType[mu, FreeIndex] = True.";

GrassmannParity::usage =
"GrassmannParity is a data type.

E.g. DataType[F, GrassmannParity] = 1 declares F to be of bosonic type and
DataType[F, GrassmannParity] = -1 of fermionic one.";

NegativeInteger::usage =
"NegativeInteger is a data type. E.g. DataType[n, NegativeInteger] can be set
to True.";

PositiveInteger::usage =
"PositiveInteger is a data type. E.g. DataType[OPEm, PositiveInteger] gives
True.";

PositiveNumber::usage =
"PositiveNumber is a data type. E.g. DataType[Epsilon, PositiveNumber] = True
(by default).";

FCTensor::usage =
"FCTensor is a data type. E.g. DataType[R, FCTensor] = True.";

FCVariable::usage =
"FCVariable  is a data type. E.g. DataType[z, FCVariable] = True.";

DataType::usage =
"DataType[exp, type] = True defines the object exp to have data-type type.

DataType[exp1, exp2, ..., type] defines the objects exp1, exp2, ... to have
data-type type.

The default setting is DataType[__, _] := False.

To assign a certain data-type, do, e.g., DataType[x, PositiveInteger] = True.
Currently used DataTypes: 

- NonCommutative

- PositiveInteger

- NegativeInteger

- PositiveNumber

- FreeIndex

- GrassmannParity

If loaded, PHI adds the DataTypes: UMatrix, UScalar.";

Begin["`Package`"];
End[]

(* ------------------------------------------------------------------------ *)

Begin["`DataType`Private`"];

set::usage="";

DataType[] = {
	FreeIndex,
	GrassmannParity,
	NegativeInteger,
	PositiveInteger,
	PositiveNumber,
	FCTensor,
	FCVariable
};

DataType[_] :=
	(Message[DataType::argrx, DataType, 1, "2 or more"];
	Abort[]);
(*
DataType[] :=
	(Message[DataType::argrx, DataType, 0, "2 or more"];
	Abort[]);
*)
(* Listability of DataType[x,y,z,type]=bol *)

DataType /:
	HoldPattern[Set[DataType[a_, b__,type_], bool_]] :=
		Map[set[dt[#, type], bool]&, {a, b}] /. {set:>Set,dt:>DataType};

DataType[a_, b__, type_] :=
	Flatten[{DataType[a, type], DataType[b, type]}];

(* Special rules for NonCommutative *)
(* Setting DataType[x,NonCommutative]=True or DataType[x,NonCommutative]=False
	updates $NonComm and NonCommFreeQ *)

DataType /:
	HoldPattern[Set[DataType[exp_, NonCommutative], (bool:True|False)]] :=
		Block[{ndt, ndf, dt, ncq, nnt, nnf, downvalues, hp, dvals, tmp},

			If[	bool,
				(* True *)
				If[!MemberQ[$NonComm, exp],
					AppendTo[$NonComm, exp]
				],
				(* False *)
				If[MemberQ[$NonComm, exp],
					$NonComm = SelectFree[$NonComm, exp];
				];

			];

			dvals = DownValues[DataType]/.{DataType->dt,HoldPattern->hp};
			ndt = (RuleDelayed @@ {hp @@ {dt[exp, NonCommutative]}, True});
			ndf = (RuleDelayed @@ {hp @@ {dt[exp, NonCommutative]}, False});

			If[	bool,
				(* True *)
				If[	FreeQ[dvals, ndt],
					If[ !FreeQ[dvals, ndf],
						tmp = SelectFree[dvals, ndf],
						tmp = dvals
					];
					DownValues[DataType] = (Flatten[{tmp, ndt}] /. hp->HoldPattern /. dt -> DataType);

				],

				(* False *)
				If[	FreeQ[dvals, ndf],
					If[ !FreeQ[dvals, ndt],
						tmp = SelectFree[dvals, ndt],
						tmp = dvals
					];
					DownValues[DataType] = (Flatten[{tmp, ndf}] /. hp->HoldPattern /. dt -> DataType);
				]
			];

			nnt = (RuleDelayed @@ {hp[ncq[exp]], True}) /. ncq -> NonCommFreeQ;
			nnf = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, False}) /. ncq -> NonCommQ;

			(* 	Careful, the downvalues of NonCommFreeQ and NonCommQ swell up a lot due to
				the memoization of those function. So one should avoid modifying them unless
				absolutely necessary. *)

			(* Update NonCommFreeQ *)
			If[	!FreeQ[DownValues[NonCommFreeQ],exp],
				dvals = DownValues[NonCommFreeQ];
				If[	bool,
					(*True*)
					set[downvalues[NonCommFreeQ],Prepend[SelectFree[dvals, exp], nnf]] /. {set :> Set, downvalues :> DownValues},

					(*False*)
					set[downvalues[NonCommFreeQ],Prepend[SelectFree[dvals, exp], nnt]] /. {set :> Set, downvalues :> DownValues}
				]
			];

			(* Update NonCommQ *)
			If[	!FreeQ[DownValues[NonCommQ],exp],
				dvals = DownValues[NonCommQ];
				If[	bool,
					(*True*)
					set[downvalues[NonCommQ],Prepend[SelectFree[dvals, exp], nnt]] /. {set :> Set, downvalues :> DownValues},

					(*False*)
					set[downvalues[NonCommQ],Prepend[SelectFree[dvals, exp], nnf]] /. {set :> Set, downvalues :> DownValues}
				]
			];

			bool
		];

(* Special rules for FCTensor *)
(* Setting DataType[x,FCTensor]=True or DataType[x,FCTensor]=False
	updates $FCTensorList *)

DataType /:
	HoldPattern[Set[DataType[exp_, FCTensor], True]] :=
		Block[{ndt, ndf, dt, ncq, nnn, nnt, set, downvalues, hp},
			If[!MemberQ[$FCTensorList, exp],
				AppendTo[$FCTensorList, exp]
			];
			ndt = (RuleDelayed @@ {HoldPattern @@ {dt[exp, FCTensor]}, True}) /. dt -> DataType;
			ndf = (RuleDelayed @@ {HoldPattern @@ {dt[exp, FCTensor]}, False}) /. dt -> DataType;
			If[FreeQ[DownValues[DataType], ndt],
				DownValues[DataType] = Prepend[SelectFree[DownValues[DataType]/.{DataType->dt,HoldPattern->hp},
					ndf/.{DataType->dt,HoldPattern->hp}]/.{dt->DataType,hp->HoldPattern}, ndt]
			];
			True
		];


DataType /:
	HoldPattern[Set[DataType[exp_, FCTensor], False]] :=
		Block[{ndt, ndf, dt, ncq, nnn, nnt, set, downvalues,hp},
			If[MemberQ[$FCTensorList, exp],
				$FCTensorList = SelectFree[$FCTensorList, exp];
			];
			ndt = (RuleDelayed @@ {HoldPattern @@{dt[exp, FCTensor]}, True}) /. dt -> DataType;
			ndf = (RuleDelayed @@ {HoldPattern @@{dt[exp, FCTensor]}, False}) /. dt -> DataType;
			If[FreeQ[DownValues[DataType], ndf],
				DownValues[DataType] = Prepend[SelectFree[DownValues[DataType]/.{DataType->dt,HoldPattern->hp},
					ndt/.{DataType->dt,HoldPattern->hp}]/.{dt->DataType,hp->HoldPattern}, ndf]
			];
			False
		];


HoldPattern[DataType[__, _]] :=
	False;

FCPrint[1,"Datatype.m loaded"];
End[]
