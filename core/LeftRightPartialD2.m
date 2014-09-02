(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`LeftRightPartialD2`",{"HighEnergyPhysics`FeynCalc`"}];

LeftRightPartialD2::"usage"=
"LeftRightPartialD2[mu] denotes partial_mu, acting to the left and
right. ExplicitPartialD[LeftRightPartialD2[mu]] gives
(RightPartialD[mu] + LeftPartialD[mu]).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative,
       FreeQ2, Momentum, LorentzIndex, OPEDelta];

DeclareNonCommutative[LeftRightPartialD2];

(* ******************************************************************** *)
LeftRightPartialD2[xx__] := LeftRightPartialD2@@ (LorentzIndex /@ {xx}) /;
                 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
                               Pattern, Blank}] && (Union[{xx}]=!={1});

LeftRightPartialD2[(1)..] = 1;

LeftRightPartialD2[c:OPEDelta..] := LeftRightPartialD2 @@ (Momentum /@ {c});
LeftRightPartialD2[x_LorentzIndex, y__LorentzIndex] :=
 DOT @@ Map[LeftRightPartialD2, {x, y}];
LeftRightPartialD2[x_Momentum, y__Momentum] :=
 DOT @@ Map[LeftRightPartialD2, {x, y}];

LeftRightPartialD2[Momentum[OPEDelta]^n_Integer?Positive] :=
       DOT @@ Map[LeftRightPartialD2, Table[Momentum[OPEDelta],{n}]];

LeftRightPartialD2 /:
   MakeBoxes[ LeftRightPartialD2[x_] , TraditionalForm
            ] :=
    SubscriptBox[OverscriptBox[
       "\[PartialD]", "\[LeftRightArrow]"], Tbox[x]];


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeftRightPartialD2 | \n "]];
Null
