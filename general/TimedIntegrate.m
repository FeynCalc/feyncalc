(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TimedIntegrate *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 17 April 2001 at 14:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`TimedIntegrate`",{"HighEnergyPhysics`FeynCalc`"}];

TimedIntegrate::"usage"= "
TimedIntegrate[exp, vars] is like Integrate, but stops after the number of \
seconds specified by the option Timing. Options of Integrate can be given \
and are passed on.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Epsilon = MakeContext["Epsilon"];
Integratedx = MakeContext["Integratedx"];

TimedIntegrate::"time" =
    "Time constraint `1` exceeded.";
TimedIntegrate::"abort" =
    "Manual abort";

Options[TimedIntegrate] = {Timing -> 10, Assumptions -> Epsilon > 0,
                           Integrate -> Integrate, Expand -> True};

TimedIntegrate[exp_,vars__List,opts___Rule] :=

Block[{tim,int,ops,eps,ex,a,b,c,d,e,f},
If[(Expand/.Flatten[{opts}]/.Options[TimedIntegrate]) === True,
  ex = Expand[exp],
  ex = exp];
ex = If[Head[ex] === Plus, List @@ ex, {ex}] /. Epsilon:>eps;
int=(Integrate/.Flatten[{opts}]/.Options[TimedIntegrate]);
tim=(Timing/.Flatten[{opts}]/.Options[TimedIntegrate]);
ops=Sequence@@(Select[FixedPoint[
      Replace[#, {a___, b_ -> c_, d___, b_ -> e_, f___} :>
        {a, b -> c, d, f}, {0}]&,
      Join[{opts},Options[TimedIntegrate]]],
  FreeQ[#,Timing|Integrate|Expand]&]/.Epsilon:>eps);

CheckAbort[
  TimeConstrained[
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

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TimedIntegrate | \n "]];
Null
