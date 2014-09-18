(* :Summary: FourVector *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`FourVector`",{"HighEnergyPhysics`FeynCalc`"}];

FourVector::"usage" =
"FourVector[p, mu] is the four Dimensional vector p with Lorentz index m.
A vector with space-time Dimension d is obtained by supplying the option
Dimension->d."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

fci := fci = MakeContext["FeynCalcInternal"];

MakeContext[ Dimension, LorentzIndex, Momentum, Pair];

Options[FourVector]  = {Dimension -> 4, fci -> True};

(* experimentally *)
FourVector[a_,b_, c___Rule] :=
 Pair[Momentum[a, Dimension /. {c} /. Options[FourVector]],
      LorentzIndex[b, Dimension /. {c} /. Options[FourVector]]
     ] /; FreeQ[{a, b}, Momentum] &&
          FreeQ[{a, b}, LorentzIndex] &&
          ((fci /. {c} /. Options[FourVector]) === True);

   FourVector /:
   MakeBoxes[FourVector[a_Plus,b_, ___], TraditionalForm] :=
    SubscriptBox[Tbox["(",HoldForm[a],
                      ")"],Tbox[b]];
   FourVector /:
   MakeBoxes[FourVector[a_,b_, ___], TraditionalForm] :=
    SubscriptBox[Tbox[a],Tbox[b]] /; Head[a] =!= Plus;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)



