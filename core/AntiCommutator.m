(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`AntiCommutator`",
             "HighEnergyPhysics`FeynCalc`"];

AntiCommutator::"usage"=
"AntiCommutator[x, y] = c  defines the anti-commutator of the \
non-commuting objects x and y. \
Settings of AntiCommutator (e.g.AntiCommutator[a,b]=c) \
are recognized by DotSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DataType, NonCommutative];

AntiCommutator /: Set[AntiCommutator[a_, b_] , c_] := Block[{nd, acom},
                  nd = (RuleDelayed @@ {HoldPattern @@ {acom[a, b]}, c}
                        ) /. acom -> AntiCommutator ;
                If[FreeQ[DownValues[AntiCommutator], nd],
                    PrependTo[DownValues[AntiCommutator], nd]
                  ];
                                                          c];
  AntiCommutator /:
   MakeBoxes[
    AntiCommutator[a_, b_], TraditionalForm
            ] := Tbox["{", a, ",", "\[MediumSpace]", b, "}"];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "AntiCommutator | \n "]];
Null
