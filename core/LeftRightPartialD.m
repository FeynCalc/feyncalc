(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`LeftRightPartialD`",
             "HighEnergyPhysics`FeynCalc`"];

LeftRightPartialD::"usage"=
"LeftRightPartialD[mu] denotes partial_mu, acting to the left and
right. ExplicitPartialD[LeftRightPartialD[mu]] gives
1/2 (RightPartialD[mu] - LeftPartialD[mu]).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative,
            FreeQ2, Momentum, LorentzIndex, OPEDelta];

DeclareNonCommutative[LeftRightPartialD];

(* ******************************************************************** *)
LeftRightPartialD[xx__] := LeftRightPartialD@@ (LorentzIndex /@ {xx}) /;
                 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
                               Pattern, Blank}] && (Union[{xx}]=!={1});

LeftRightPartialD[(1)..] = 1;

LeftRightPartialD[c:OPEDelta..] := LeftRightPartialD @@ (Momentum /@ {c});
LeftRightPartialD[x_LorentzIndex, y__LorentzIndex] :=
 DOT @@ Map[LeftRightPartialD, {x, y}];
LeftRightPartialD[x_Momentum, y__Momentum] :=
 DOT @@ Map[LeftRightPartialD, {x, y}];

(* nonsense;  commented out 9/95
LeftRightPartialD[Momentum[OPEDelta]^n_Integer?Positive] :=
       DOT @@ Map[LeftRightPartialD, Table[Momentum[OPEDelta],{n}]];
*)

LeftRightPartialD /:
   MakeBoxes[ LeftRightPartialD[x_] , TraditionalForm
            ] :=
    SubscriptBox[OverscriptBox["\[PartialD]", "\[LeftRightArrow]"], Tbox[x]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeftRightPartialD | \n "]];
Null
