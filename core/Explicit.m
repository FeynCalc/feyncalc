(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Explicit *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Explicit`",
             "HighEnergyPhysics`FeynCalc`"];

Explicit::"usage" = 
"Explicit is an option for FieldStrength, GluonVertex,
SUNF, CovariantFieldDerivative, Twist2GluonOperator and others functions. 
If set to True the full form of the operator is inserted. 
Explicit[exp] inserts explicit expressions of FieldStrength, 
GluonVertex, etc. in exp. \n
The dimension of the objects is changed according to the setting of the
option Dimension.
SUNF's are replaced by SUNTrace objects.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
ChangeDimension,
CouplingConstant,
Dimension,
ExpandScalarProduct,
FieldStrength,
Gauge,
GluonPropagator,
GhostPropagator,
Gstrong,
QuarkPropagator,
GluonVertex,
GluonGhostVertex,
GluonGhostVertex,
(*Loop,*)
OPE,
QuarkGluonVertex
];

FieldStrength := FieldStrength = MakeContext["FieldStrength"];

Options[Explicit] = {
CouplingConstant -> Gstrong, 
Dimension -> D,
Gauge -> 1, 
OPE -> False};

Explicit[y_, opts___?OptionQ] := Block[{dim, gh, gp, gvv, gv, qp, qgv, t2g, fis, pr, r = y, op}, 
           op = Sequence@@Join[{opts}, Options[Explicit]];
	   dim = Dimension /. {op};
           gv[x__]  := gv[x]  = ExpandScalarProduct[ GluonVertex[x, Explicit -> True, op]];
           gp[x__]  := gp[x]  = ExpandScalarProduct[ GluonPropagator[x, Explicit -> True, op]];
           gh[x__]  := gh[x]  = ExpandScalarProduct[ GhostPropagator[x, Explicit -> True, op]];
           qp[x__]  := qp[x]  = ExpandScalarProduct[ QuarkPropagator[x, Explicit -> True, op]];
           gvv[x__] := ghv[x] = ExpandScalarProduct[ GluonGhostVertex[x, Explicit -> True, op]];
           qgv[x__] := qgv[x] = ExpandScalarProduct[ QuarkGluonVertex[x, Explicit -> True, op]];
           r = r /. {GluonVertex :> gv, GluonGhostVertex :> gvv, GhostPropagator :> gh, 
                     GluonPropagator :> gp, QuarkGluonVertex :> qgv, QuarkPropagator :> qp
                    };

           If[CheckContext["Twist2GluonOperator"],
              t2g[x__] := t2g[x] = MakeContext["Twist2GluonOperator"][x, Explicit->True]; 
              r = r /. MakeContext["Twist2GluonOperator"] -> t2g ];

           If[CheckContext["Twist2QuarkOperator"],
              t2q[x__] := t2q[x] = MakeContext["Twist2QuarkOperator"][x, Explicit->True]; 
              r = r /. MakeContext["Twist2QuarkOperator"] -> t2q ];

           If[CheckContext["FieldStrength"],
              fis[x__] := fis[x] = MakeContext["FieldStrength"][x, Explicit->True]; 
              r = r /. MakeContext["FieldStrength"] -> fis];
  
           (* use ChangeDimension only if there have been changes *)
           If[r =!= y, r = ChangeDimension[r, dim]];
r];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Explicit | \n "]];
Null
