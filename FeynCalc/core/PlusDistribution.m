(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  a head for (1/(1-x))_+  and
                         (Log[1-x]/(1-x))_+
*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`PlusDistribution`",{"HighEnergyPhysics`FeynCalc`"}];

PlusDistribution::"usage"=
"PlusDistribution[1/(1-x)] denotes the distribution (1/(1-x))_+.
PlusDistribution[Log[1-x]/(1-x)] denotes the distribution
(Log[1-x]/(1-x))_+.
PlusDistribution[Log[x (1-x)]/(1-x)] simplifies to
Log[x] /(1-x) + PlusDistribution[Log[1-x]/(1-x)].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

PlusDistribution[Log[x_ (1-x_)]/(1-x_)] :=
Log[x] /(1-x) + PlusDistribution[Log[1-x]/(1-x)];

PlusDistribution /:
   MakeBoxes[
             PlusDistribution[ a_ ], TraditionalForm
            ] :=
   SubscriptBox[ RowBox[{"(",
                 MakeBoxes[a, TraditionalForm],")"}
                       ],"+"
                ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PlusDistribution | \n "]];
Null
