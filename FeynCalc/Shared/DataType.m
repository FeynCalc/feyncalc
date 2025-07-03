(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DataType is just a data type *)

(* ------------------------------------------------------------------------ *)

FreeIndex::usage =
"FreeIndex is a datatype which is recognized by Contract.

Possible use: DataType[mu, FreeIndex] = True.";

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

ImplicitDiracIndex::usage =
"ImplicitDiracIndex is a data type. It mainly applies to names of quantum
fields specifying that the corresponding field carries an implicit Dirac
index.

This information can be supplied e.g. via DataType[QuarkField,
ImplicitDiracIndex] = True, where QuarkField is a possible name of the
relevant field.

The ImplicitDiracIndex property becomes relevant when simplifying 
noncommutative products involving QuantumFields via ExpandPartialD,
DotSimplify.";

ImplicitPauliIndex::usage =
"ImplicitPauliIndex is a data type. It mainly applies to names of quantum
fields specifying that the corresponding field carries an implicit Pauli
index.

This information can be supplied e.g. via DataType[QuarkFieldChi,
ImplicitPauliIndex] = True, where QuarkFieldChi is a possible name of the
relevant field.

The ImplicitDiracIndex property becomes relevant when simplifying 
noncommutative products involving QuantumFields via ExpandPartialD,
DotSimplify.";

ImplicitSUNFIndex::usage =
"ImplicitSUNFIndex is a data type. It mainly applies to names of quantum fields
specifying that the corresponding field carries an implicit $SU(N)$ index in
the fundamental representation.

This information can be supplied e.g. via DataType[QuarkField,
ImplicitSUNFIndex] = True, where QuarkField is a possible name of the relevant
field.

The ImplicitSUNFIndex property becomes relevant when simplifying 
noncommutative products involving QuantumFields via ExpandPartialD,
DotSimplify.";

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

- FCTensor

- ImplicitDiracIndex

- ImplicitPauliIndex

- ImplicitSUNFIndex

If loaded, PHI adds the DataTypes: UMatrix, UScalar.";

DataType::notsync =
"You are using FeynCalc in the parallel mode, but the DataType values \
are not synchronized between the master kernel and subkernels. \
This usually happens if such definitions have been set before activating the \
parallel mode. Please clear the existing definitions with FCClearDataTypes[] \
and redefine your DataType.";

Begin["`Package`"];
End[]

(* ------------------------------------------------------------------------ *)

Begin["`DataType`Private`"];

set::usage="";

Options[DataType] = {
	FCParallelize	-> True,
	FCVerbose		-> False
};


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
	HoldPattern[Set[DataType[exp_, type_, opts:OptionsPattern[]], (bool:True|False)]] :=
	Block[{dtVerbose},

		If [OptionValue[DataType,{opts},FCVerbose]===False,
				dtVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[DataType,{opts},FCVerbose], _Integer],
					dtVerbose=OptionValue[DataType,{opts},FCVerbose]
				];
		];

		If[	$ParallelizeFeynCalc && OptionValue[DataType,{opts},FCParallelize],
				FCPrint[1,"DataType: Setting DataType value on subkernels. ", FCDoControl->dtVerbose];

				Switch[	type,

						NonCommutative,
						With[{xxx=exp,zzz=bool},
							ParallelEvaluate[setDataTypeNonCommutative[xxx, zzz];,DistributedContexts -> None]
						],

						FCTensor,
						With[{xxx=exp,zzz=bool},
							ParallelEvaluate[setDataTypeFCTensor[xxx, zzz];,DistributedContexts -> None]
						],

						_,
						With[{xxx=exp,yyy=type,zzz=bool},
							ParallelEvaluate[setDataType[xxx, yyy, zzz];,DistributedContexts -> None]
						];
				];
				FCPrint[1,"DataType: Done setting DataType value on subkernels. ", FCDoControl->dtVerbose]
		];

		FCPrint[1,"DataType: Setting DataType value on the main kernel. ", FCDoControl->dtVerbose];

		Switch[	type,

				NonCommutative,
				setDataTypeNonCommutative[exp, bool],

				FCTensor,
				setDataTypeFCTensor[exp, bool],

				_,
				setDataType[exp, type, bool]
		];

		setDataType[exp, type, bool];
		FCPrint[1,"DataType: Setting DataType value on value on the main kernel. ", FCDoControl->dtVerbose];

		bool
	];

setDataType[exp_,type_,bool_]:=
	Block[{ndt,ndf,dt,hp},
		ndt = (RuleDelayed @@ {HoldPattern @@{dt[exp, type]}, True}) /. dt -> DataType;
		ndf = (RuleDelayed @@ {HoldPattern @@{dt[exp, type]}, False}) /. dt -> DataType;
		If[	bool,

			If[FreeQ[DownValues[DataType], ndt],
					DownValues[DataType] = Prepend[SelectFree[DownValues[DataType]/.{DataType->dt,HoldPattern->hp},
						ndf/.{DataType->dt,HoldPattern->hp}]/.{dt->DataType,hp->HoldPattern}, ndt]
			],
			If[	FreeQ[DownValues[DataType], ndf],
				DownValues[DataType] = Prepend[SelectFree[DownValues[DataType]/.{DataType->dt,HoldPattern->hp},
				ndt/.{DataType->dt,HoldPattern->hp}]/.{dt->DataType,hp->HoldPattern}, ndf]
			]
		];

		(* If we are in the parallel mode and on the master kernel, need to check whether these values have already been set on subkernels*)
		If[$ParallelizeFeynCalc && ($KernelID===0),
			If[!FCValuesSynchronizedQ[{DataType}, DownValues],
				Message[DataType::notsync]
			]
		];
	];

(* Special rules for NonCommutative *)
(* Setting DataType[x,NonCommutative]=True or DataType[x,NonCommutative]=False
	updates $NonComm and NonCommFreeQ *)
setDataTypeNonCommutative[exp_, bool_]:=
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

		(* If we are in the parallel mode and on the master kernel, need to check whether these values have already been set on subkernels*)
		If[$ParallelizeFeynCalc && ($KernelID===0),
			If[!FCValuesSynchronizedQ[{DataType}, DownValues],
				Message[ScalarProduct::notsync]
			]
		];

		bool
	];


(* Special rules for FCTensor *)
(* Setting DataType[x,FCTensor]=True or DataType[x,FCTensor]=False
	updates $FCTensorList *)
setDataTypeFCTensor[exp_, bool_]:=
	Block[{ndt, ndf, dt, ncq, nnn, nnt, set, downvalues, hp},

		If[	bool,
			(* True *)
			If[!MemberQ[$FCTensorList, exp],
				AppendTo[$FCTensorList, exp]
			],
			(* False *)
			If[MemberQ[$FCTensorList, exp],
				$FCTensorList = SelectFree[$FCTensorList, exp];
			]
		];

		ndt = (RuleDelayed @@ {HoldPattern @@{dt[exp, FCTensor]}, True}) /. dt -> DataType;
		ndf = (RuleDelayed @@ {HoldPattern @@{dt[exp, FCTensor]}, False}) /. dt -> DataType;

		If[	bool,
			(* True *)
			If[FreeQ[DownValues[DataType], ndt],
				DownValues[DataType] = Prepend[SelectFree[DownValues[DataType]/.{DataType->dt,HoldPattern->hp},
					ndf/.{DataType->dt,HoldPattern->hp}]/.{dt->DataType,hp->HoldPattern}, ndt]
			],
			(* False *)
			If[FreeQ[DownValues[DataType], ndf],
				DownValues[DataType] = Prepend[SelectFree[DownValues[DataType]/.{DataType->dt,HoldPattern->hp},
					ndt/.{DataType->dt,HoldPattern->hp}]/.{dt->DataType,hp->HoldPattern}, ndf]
			]
		];

		(* If we are in the parallel mode and on the master kernel, need to check whether these values have already been set on subkernels*)
		If[$ParallelizeFeynCalc && ($KernelID===0),
			If[!FCValuesSynchronizedQ[{DataType}, DownValues],
				Message[ScalarProduct::notsync]
			]
		];

		bool
	];

DataType /:
	HoldPattern[Set[DataType[a_, b__,type_, opts:OptionsPattern[]], bool_]] :=
		Map[set[dt[#, type, opts], bool]&, {a, b}] /. {set:>Set,dt:>DataType};

DataType[a_, b__, type_, opts:OptionsPattern[]] :=
	Flatten[{DataType[a, type,opts], DataType[b, type,opts]}];


HoldPattern[DataType[__, _]] :=
	False;

FCPrint[1,"Datatype.m loaded"];
End[]
