(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Explicit *)

(* ------------------------------------------------------------------------ *)

Explicit::usage =
"Explicit is an option for FieldStrength, GluonVertex, \
SUNF, CovariantFieldDerivative, Twist2GluonOperator and others functions. \
If set to True the full form of the operator is inserted. \
Explicit[exp] inserts explicit expressions of FieldStrength, \
GluonVertex, etc. in exp. \n
The dimension of the objects is changed according to the setting of the \
option Dimension. SUNF's are replaced by SUNTrace objects.";

(* ------------------------------------------------------------------------ *)


Begin["`Package`"]
End[]

Begin["`Explicit`Private`"]

Options[Explicit] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Gauge -> 1,
	OPE -> False
};

Explicit[expr_, opts:OptionsPattern[]] :=
	Block[{gh, gp, gvv, ghv, gv, qp, qgv, t2g, t2q, fis, ex},


		ex = expr;

		gv[x__]  := ExpandScalarProduct[ GluonVertex[x, Explicit -> True,
			FilterRules[{opts}, Options[GluonVertex]]]];
		gp[x__]  := ExpandScalarProduct[GluonPropagator[x, Explicit -> True,
			FilterRules[{opts}, Options[GluonPropagator]]]];
		gh[x__]  := ExpandScalarProduct[ GhostPropagator[x, Explicit -> True,
			FilterRules[{opts}, Options[GhostPropagator]]]];
		qp[x__]  := ExpandScalarProduct[ QuarkPropagator[x, Explicit -> True,
			FilterRules[{opts}, Options[QuarkPropagator]]]];
		gvv[x__] := ExpandScalarProduct[ GluonGhostVertex[x, Explicit -> True,
			FilterRules[{opts}, Options[GluonGhostVertex]]]];
		qgv[x__] := ExpandScalarProduct[ QuarkGluonVertex[x, Explicit -> True,
			FilterRules[{opts}, Options[QuarkGluonVertex]]]];
		t2g[x__] := Twist2GluonOperator[x, Explicit->True,
			FilterRules[{opts}, Options[Twist2GluonOperator]]];
		t2q[x__] := Twist2QuarkOperator[x, Explicit->True,
			FilterRules[{opts}, Options[Twist2QuarkOperator]]];
		fis[x__] := FieldStrength[x, Explicit->True,
			FilterRules[{opts}, Options[FieldStrength]]];

		ex = ex /. {
			GluonVertex :> gv,
			GluonPropagator :> gp,
			GhostPropagator :> gh,
			QuarkPropagator :> qp,
			GluonGhostVertex :> gvv,
			QuarkGluonVertex :> qgv,
			Twist2GluonOperator :> t2g,
			Twist2QuarkOperator :> t2q,
			FieldStrength :> fis
		};

		ex
	];

FCPrint[1,"Explicit.m loaded"];
End[]
