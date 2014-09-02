(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DataType is just a data type *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DataType`",{"HighEnergyPhysics`FeynCalc`"}];

DataType::"usage"=
"DataType[exp, type] = True   defines the object exp to have datatype type. \
DataType[exp1, exp2, ..., type] defines the objects exp1, exp2, ... to \
have datatype type. \
The default setting is DataType[__, _]:=False. \
To assign a certain data type, do e.g.: \
DataType[x, PositiveInteger] = True.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"]


(*
MakeContext[NonCommFreeQ, NonCommQ, SelectFree];
*)
NonCommFreeQ := NonCommFreeQ = MakeContext["NonCommFreeQ"];
NonCommQ := NonCommQ = MakeContext["NonCommQ"];
SelectFree := SelectFree = MakeContext["SelectFree"];
noncommutative := noncommutative = MakeContext["NonCommutative"];

DataType[_] := soso /; Message[DataType::argrx, DataType, 1, "2 or more"];
DataType[] := soso /; Message[DataType::argrx, DataType, 0, "2 or more"];

(* Listability of DataType[x,y,z,type]=bol *)

DataType /: HoldPattern[Set[DataType[a_, b__,type_], bool_]] :=
            Map[set[dt[#, type], bool]&, {a, b}] /. {set:>Set,dt:>DataType};

DataType[a_, b__, type_] := Flatten[{DataType[a, type], DataType[b, type]}];

(* Special rules for NonCommutative *)
(* Setting DataType[x,NonCommutative]=True or DataType[x,NonCommutative]=False
   updates $NonComm and NonCommFreeQ *)

DataType /: HoldPattern[Set[DataType[exp_,
            HighEnergyPhysics`FeynCalc`NonCommutative`NonCommutative],
            True]] :=
 Block[{ndt, ndf, dt, ncq, nnn, nnt, set, downvalues},
              If[!MemberQ[$NonComm, exp], AppendTo[$NonComm, exp]];
               ndt = (RuleDelayed @@ {HoldPattern @@
                       {dt[exp, noncommutative]}, True}
                     ) /. dt -> DataType;
               ndf = (RuleDelayed @@ {HoldPattern @@
                       {dt[exp, noncommutative]}, False}
                     ) /. dt -> DataType;
        If[FreeQ[DownValues[DataType], ndt],
           DownValues[DataType] =
           Prepend[SelectFree[DownValues[DataType], ndf], ndt]
          ];
            nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, False}
                  ) /. ncq -> NonCommFreeQ;
            set[downvalues[NonCommFreeQ],Prepend[
                SelectFree[DownValues@@{NonCommFreeQ}, exp], nnt
                                                ]
              ] /. {set :> Set, downvalues :> DownValues};
           (*Let's update NonCommQ also. F.Orellana, 11/9-2002*)
            nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, True}
                  ) /. ncq -> NonCommQ;
            set[downvalues[NonCommQ],Prepend[
                SelectFree[DownValues@@{NonCommQ}, exp], nnt
                                                ]
              ] /. {set :> Set, downvalues :> DownValues};
  True];


DataType /: HoldPattern[Set[DataType[exp_,
            HighEnergyPhysics`FeynCalc`NonCommutative`NonCommutative],
            False]] :=
 Block[{ndt, ndf, dt, ncq, nnn, nnt, set, downvalues},
              If[MemberQ[$NonComm, exp],
                  $NonComm = SelectFree[$NonComm, exp];
                 ];
               ndt = (RuleDelayed @@ {HoldPattern @@
                      {dt[exp, noncommutative]}, True}
                     ) /. dt -> DataType;
               ndf = (RuleDelayed @@ {HoldPattern @@
                      {dt[exp, noncommutative]}, False}
                     ) /. dt -> DataType;
        If[FreeQ[DownValues[DataType], ndf],
           DownValues[DataType] =
           Prepend[SelectFree[DownValues[DataType], ndt], ndf]
          ];
                       nnn = (RuleDelayed @@ {HoldPattern @@
                   {ncq[exp]}, _}
                  ) /. ncq -> NonCommFreeQ;
           If[!FreeQ[DownValues[NonCommFreeQ], nnn],
              DownValues[NonCommFreeQ] =
              SelectFree[DownValues[NonCommFreeQ], nnn]
             ];
            nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, True}
                  ) /. ncq -> NonCommFreeQ;
            set[downvalues[NonCommFreeQ],Prepend[
                SelectFree[DownValues@@{NonCommFreeQ}, exp], nnt
                                                ]
              ] /. {set :> Set, downvalues :> DownValues};
           (*Let's update NonCommQ also. F.Orellana, 11/9-2002*)
            nnt = (RuleDelayed @@ {HoldPattern @@ {ncq[exp]}, False}
                  ) /. ncq -> NonCommQ;
            set[downvalues[NonCommQ],Prepend[
                SelectFree[DownValues@@{NonCommQ}, exp], nnt
                                                ]
              ] /. {set :> Set, downvalues :> DownValues};
 False];

HoldPattern[DataType[__, _]] := False;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DataType | \n "]];
Null
