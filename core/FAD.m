(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FAD`",
             "HighEnergyPhysics`FeynCalc`"];

FAD::"usage"= "FAD[q, q-p, ...] denotes 1/(q^2 (q-p)^2 ...).
FAD[{q1,m}, {q1-p,m}, q2, ...] is
1/( (q1^2 - m^2) ( (q1-p)^2 - m^2 ) q2^2 ... ).
(Translation into FeynCalc internal form is performed by
FeynCalcInternal.)";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[Dimension];

Options[FAD] = {Dimension -> D};

ff[{y_,z_}] := SequenceForm["[",y^2, "-", z^2,"]"];

ff[y_/;Head[y]=!=List] := SequenceForm["[",y^2,"]"];

FAD[-p_]:=FAD[p];

FAD/:
    MakeBoxes[FAD[a__], TraditionalForm
             ] := ToBoxes[1/ Apply[Times,Map[ff, {a}]
                                  ],
                          TraditionalForm];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FAD | \n "]];
Null
