(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MetricTensor`",{"HighEnergyPhysics`FeynCalc`"}];

MetricTensor::"usage"=
"MetricTensor[mu, nu] is the metric tensor in 4 dimensions.
The metric tensor in d dimensions is obtained by supplying the
option Dimension->d.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

fci:= fci = MakeContext["FeynCalcInternal"];
Dimension = MakeContext["CoreOptions","Dimension"];

MakeContext[LorentzIndex, Pair];

Options[MetricTensor] = {Dimension -> 4, fci -> True};

MetricTensor[a_, b_, opt___Rule] :=
  Pair[LorentzIndex[a, Dimension /. Dimension -> (Dimension /. {opt} /.
                                     Options[MetricTensor]
                                                 )
                   ] ,
       LorentzIndex[b, Dimension /. Dimension -> (Dimension /. {opt} /.
                                     Options[MetricTensor]
                                                 )
                   ]
      ] /; ( fci /. {opt} /. Options[MetricTensor] ) === True;

 MetricTensor /:
   MakeBoxes[ MetricTensor[x_, y_, opts___], TraditionalForm] :=
   If[$LorentzIndices===True,
      ReleaseHold[
      Hold[MakeBoxes][fci[MetricTensor[x, y, opts]], TraditionalForm]],
      SuperscriptBox["g", Tbox[x,y]]
     ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MetricTensor | \n "]];
Null
