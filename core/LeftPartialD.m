(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`LeftPartialD`",{"HighEnergyPhysics`FeynCalc`"}];

LeftPartialD::"usage"=
"LeftPartialD[mu] denotes partial_mu, acting to the left.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ Commutator, DeclareNonCommutative,
    FreeQ2, LorentzIndex, Momentum, OPEDelta, RightPartialD];
(*Bug fix, 30/1-2003. F.Orellana*)
DeclareNonCommutative[LeftPartialD];
(* ******************************************************************** *)

LeftPartialD[xx__] := LeftPartialD @@ (LorentzIndex /@ {xx}) /;
		 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
			       Pattern, Blank}] && (Union[{xx}]=!={1});

LeftPartialD[(1)..] = 1;
LeftPartialD[c:OPEDelta..] := LeftPartialD @@ (Momentum /@ {c});
LeftPartialD[x_LorentzIndex, y__LorentzIndex] :=
          DOT @@ Map[LeftPartialD, {x, y}];
LeftPartialD[x_Momentum, y__Momentum] := DOT @@ Map[LeftPartialD, {x, y}];

Commutator[RightPartialD[a_], LeftPartialD[b_]] = 0;

 LeftPartialD /:
   MakeBoxes[LeftPartialD[x_ ^n_],TraditionalForm] :=
    SubsuperscriptBox[RowBox[{
      OverscriptBox["\[PartialD]", "\[LeftArrow]"]}], Tbox[" ",x],
        Tbox[n]
                     ] /; Head[x] === Momentum;

   LeftPartialD /:
   MakeBoxes[LeftPartialD[x_], TraditionalForm] :=
   SubscriptBox[OverscriptBox["\[PartialD]", "\[LeftArrow]"], TBox[x]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeftPartialD | \n "]];
Null
