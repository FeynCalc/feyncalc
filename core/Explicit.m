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
GluonVertex and Twist2GluonOperator in exp. SUNF's are replaced 
by SUNTrace objects.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
CouplingConstant,
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

Options[Explicit] = {Gauge -> 1, CouplingConstant -> Gstrong, OPE -> False};

Explicit[y_, opts___?OptionQ] := Block[{gh, gvv, gv, qp, qgv, t2g, fis, pr, r = y, op}, 
           op = Sequence@@Join[{opts}, Options[Explicit]];
           gv[x__]  := gv[x]  = Expand[ExpandScalarProduct[ GluonVertex[x, Explicit -> True, op]]];
           gp[x__]  := gp[x]  = Expand[ExpandScalarProduct[ GluonPropagator[x, Explicit -> True, op]]];
           gh[x__]  := gh[x]  = Expand[ExpandScalarProduct[ GhostPropagator[x, Explicit -> True, op]]];
           qp[x__]  := qp[x]  = Expand[ExpandScalarProduct[ QuarkPropagator[x, Explicit -> True, op]]];
           gvv[x__] := ghv[x] = Expand[ExpandScalarProduct[ GluonGhostVertex[x, Explicit -> True, op]]];
           qgv[x__] := qgv[x] = Expand[ExpandScalarProduct[ QuarkGluonVertex[x, Explicit -> True, op]]];
           r = r /. {GluonVertex :> gv, GluonGhostVertex :> gvv, GhostPropagator :> gh, 
                     GluonPropagator :> gp, QuarkGluonVertex :> qgv
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

                   r];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Explicit | \n "]];
Null
