(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TimedIntegrate *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 17 April 2001 at 14:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

TimedIntegrate::usage=
"TimedIntegrate[exp, vars] is like Integrate, but stops after the number of
seconds specified by the option Timing. Options of Integrate can be given and
are passed on.";


TimedIntegrate::"time" =
"Time constraint `1` exceeded.";

TimedIntegrate::"abort" =
"Manual abort";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];

End[]

Begin["`TimedIntegrate`Private`"];


Options[TimedIntegrate] = {
	Assumptions -> Epsilon > 0,
	Expand 		-> True,
	Integrate 	-> Integrate,
	Timing 		-> 10
};

TimedIntegrate[exp_,vars__List,opts___Rule] :=
	Block[{tim,int,ops,eps,ex},
		If[(Expand/.Flatten[{opts}]/.Options[TimedIntegrate]) === True,
			ex = Expand[exp],
			ex = exp
		];
		ex = If[Head[ex] === Plus, List @@ ex, {ex}] /. Epsilon:>eps;
		int=(Integrate/.Flatten[{opts}]/.Options[TimedIntegrate]);
		tim=(Timing/.Flatten[{opts}]/.Options[TimedIntegrate]);
		ops=Sequence@@(Select[FixedPoint[
					Replace[#, {a___, b_ -> c_, d___, b_ -> _, f___} :>
						{a, b -> c, d, f}, {0}]&,
					Join[{opts},Options[TimedIntegrate]]],
		FreeQ[#,Timing|Integrate|Expand]&]/.Epsilon:>eps);
		CheckAbort[TimeConstrained[
				Plus@@(int[#,vars,ops]&/@ex/.eps:>Epsilon),tim,
				If[$VeryVerbose > 0,Message[TimedIntegrate::"time",tim]];
				DOT[Sequence@@((Integratedx@@#)& /@ {vars}), exp]
			],
			If[$VeryVerbose > 0,
				Message[TimedIntegrate::"abort"];
				Message[TimedIntegrate::"time",tim]
			];
			DOT[Sequence@@((Integratedx@@#)& /@ {vars}),exp]
		]
	];

FCPrint[1,"TimedIntegrate.m loaded"]
End[]
