(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`PartialD`",{"HighEnergyPhysics`FeynCalc`"}];

PartialD::"usage"=
"PartialD[mu] denotes partial_mu. PartialD[x, mu] denotes d/d x^mu.
The first one acts on QuantumField[f], the second on QuantumField[f][x],
where f is some field name and x is a space-time variable.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[FreeQ2, LorentzIndex, Momentum, OPEDelta,
DeclareNonCommutative];

DeclareNonCommutative[PartialD];

(* ******************************************************************** *)
If[!MemberQ[$NonComm, PartialD], AppendTo[$NonComm, PartialD]];

PartialD[xx__] := PartialD @@ (LorentzIndex /@ {xx}) /;
                 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
                               Pattern, Blank}] && (Union[{xx}]=!={1});

PartialD[(1)..] = 1;
PartialD[c:OPEDelta..] := PartialD @@ (Momentum /@ {c});
PartialD[x_LorentzIndex, y__LorentzIndex] := DOT @@ Map[PartialD, {x, y}];
PartialD[x_Momentum, y__Momentum] := DOT @@ Map[PartialD, {x, y}];

PartialD /:
   MakeBoxes[PartialD[x_ ^n_], TraditionalForm] :=
    SubsuperscriptBox["\[PartialD]", Tbox[x], Tbox[n]
                     ] /; Head[x] === Momentum;

PartialD /:
   MakeBoxes[ PartialD[x_], TraditionalForm] :=
    SubscriptBox["\[PartialD]", ToBoxes[x,TraditionalForm]];

PartialD /:
   MakeBoxes[ PartialD[x_, HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[mu__]], TraditionalForm] :=
    RowBox[{"\[PartialD]", "/", "\[PartialD]",
            SuperscriptBox[ToBoxes[x,TraditionalForm], ToBoxes[LorentzIndex[mu],TraditionalForm]]
            }];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PartialD | \n "]];
Null
