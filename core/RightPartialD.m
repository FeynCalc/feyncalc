(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: partial derivative *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`RightPartialD`",{"HighEnergyPhysics`FeynCalc`"}];

RightPartialD::"usage"=
"RightPartialD[mu] denotes partial_mu, acting to the right.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

FreeQ2       = MakeContext["FreeQ2"];
LorentzIndex = MakeContext["LorentzIndex"];
Momentum     = MakeContext["Momentum"];
OPEDelta     = MakeContext["OPEDelta"];

DeclareNonCommutative = MakeContext["DeclareNonCommutative"];
DeclareNonCommutative[RightPartialD];

(* ******************************************************************** *)

RightPartialD[xx__] := RightPartialD @@ (LorentzIndex /@ {xx}) /; 
		 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox, 
			       Pattern, Blank}] && (Union[{xx}]=!={1});

RightPartialD[(1)..] = 1;
RightPartialD[c:OPEDelta..] := RightPartialD @@ (Momentum /@ {c});
RightPartialD[x_LorentzIndex, y__LorentzIndex] := 
          DOT @@ Map[RightPartialD, {x, y}];
RightPartialD[x_Momentum, y__Momentum] := DOT @@ Map[RightPartialD, {x, y}];

(*
RightPartialD[Momentum[OPEDelta]^n_Integer?Positive] :=
       DOT @@ Map[RightPartialD, Table[Momentum[OPEDelta],{n}]];
*)
   RightPartialD /:
   MakeBoxes[RightPartialD[x_ ^n_],TraditionalForm] :=
    SubsuperscriptBox[RowBox[{
      OverscriptBox["\[PartialD]", "\[RightArrow]"]}], Tbox[" ",x],Tbox[n]
                     ] /; Head[x] === Momentum;

   RightPartialD /:
   MakeBoxes[RightPartialD[x_] ,TraditionalForm] :=
    SubscriptBox[RowBox[{
      OverscriptBox["\[PartialD]", "\[RightArrow]"]}], Tbox[x]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "RightPartialD | \n "]];
Null
