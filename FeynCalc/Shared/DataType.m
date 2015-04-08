(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DataType is just a data type *)

(* ------------------------------------------------------------------------ *)

FreeIndex::usage =
"FreeIndex is a datatype which is recognized by Contract.
Possible use: DataType[mu, FreeIndex] = True.";

GrassmannParity::usage =
"GrassmannParity is a datatype. E.g. DataType[F, GrassmannParity] = 1
declares F to be of bosonic type and DataType[F, GrassmannParity] = -1
of fermionic one.";

NegativeInteger::usage =
"NegativeInteger is a data type. E.g. DataType[n, NegativeInteger] can
be set to True.";

PositiveInteger::usage =
"PositiveInteger is a data type. E.g. DataType[OPEm, PositiveInteger]
gives True.";

PositiveNumber::usage =  "PositiveNumber is a data type. E.g.
DataType[Epsilon, PositiveNumber] = True (by default). ";

DataType::usage =
"DataType[exp, type] = True   defines the object exp to have datatype type. \
DataType[exp1, exp2, ..., type] defines the objects exp1, exp2, ... to \
have datatype type. The default setting is DataType[__, _]:=False. \
To assign a certain data type, do e.g.: DataType[x, PositiveInteger] = True.";

Begin["`Package`"]
End[]

(* ------------------------------------------------------------------------ *)

Begin["`DataType`Private`"]

DataType[_] :=
	soso /; Message[DataType::argrx, DataType, 1, "2 or more"];
DataType[] :=
	soso /; Message[DataType::argrx, DataType, 0, "2 or more"];

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
	HoldPattern[Set[DataType[exp_, NonCommutative], True]] :=
		Block[{ndt, ndf, dt, ncq, nnn, nnt, set, downvalues, hp},
			If[!MemberQ[$NonComm, exp],
				AppendTo[$NonComm, exp]
			];
			ndt = (RuleDelayed @@ {HoldPattern @@ {dt[exp, NonCommutative]}, True}) /. dt -> DataType;
			ndf = (RuleDelayed @@ {HoldPattern @@ {dt[exp, NonCommutative]}, False}) /. dt -> DataType;
			If[FreeQ[DownValues[DataType], ndt],
				DownValues[DataType] = Prepend[SelectFree[DownValues[DataType]/.{DataType->dt,HoldPattern->hp},
					ndf/.{DataType->dt,HoldPattern->hp}]/.{dt->DataType,hp->HoldPattern}, ndt]
			];
			(* Update NonCommFreeQ *)
			nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, False}) /. ncq -> NonCommFreeQ;
			set[downvalues[NonCommFreeQ],Prepend[SelectFree[DownValues@@{NonCommFreeQ}, exp],
			nnt]] /. {set :> Set, downvalues :> DownValues};
			(* Update NonCommQ *)
			nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, True}) /. ncq -> NonCommQ;
			set[downvalues[NonCommQ],Prepend[SelectFree[DownValues@@{NonCommQ}, exp],
			nnt]] /. {set :> Set, downvalues :> DownValues};
			True
		];


DataType /:
	HoldPattern[Set[DataType[exp_, NonCommutative], False]] :=
		Block[{ndt, ndf, dt, ncq, nnn, nnt, set, downvalues,hp},
			If[MemberQ[$NonComm, exp],
				$NonComm = SelectFree[$NonComm, exp];
			];
			ndt = (RuleDelayed @@ {HoldPattern @@{dt[exp, NonCommutative]}, True}) /. dt -> DataType;
			ndf = (RuleDelayed @@ {HoldPattern @@{dt[exp, NonCommutative]}, False}) /. dt -> DataType;
			If[FreeQ[DownValues[DataType], ndf],
				DownValues[DataType] = Prepend[SelectFree[DownValues[DataType]/.{DataType->dt,HoldPattern->hp},
					ndt/.{DataType->dt,HoldPattern->hp}]/.{dt->DataType,hp->HoldPattern}, ndf]
			];
			(* Update NonCommFreeQ *)
			nnn = (RuleDelayed @@ {HoldPattern @@{ncq[exp]}, _}) /. ncq -> NonCommFreeQ;
			If[!FreeQ[DownValues[NonCommFreeQ], nnn],
				DownValues[NonCommFreeQ] = SelectFree[DownValues[NonCommFreeQ], nnn]
			];
			nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, True}) /. ncq -> NonCommFreeQ;
			set[downvalues[NonCommFreeQ],Prepend[SelectFree[DownValues@@{NonCommFreeQ}, exp],
			nnt]] /. {set :> Set, downvalues :> DownValues};
			(* Update NonCommQ *)
			nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, False}) /. ncq -> NonCommQ;
			set[downvalues[NonCommQ],Prepend[SelectFree[DownValues@@{NonCommQ}, exp],
			nnt]] /. {set :> Set, downvalues :> DownValues};
		False
		];

HoldPattern[DataType[__, _]] :=
	False;

FCPrint[1,"Datatype.m loaded"];
End[]
