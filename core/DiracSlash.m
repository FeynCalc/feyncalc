(* :Summary: DiracSlash  is a Feynman slash *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracSlash`",
             "HighEnergyPhysics`FeynCalc`"];

DiracSlash::"usage" =
"DiracSlash[p] is the contraction FourVector[p, mu]*DiracSlash[mu]. \
A product of those can be entered in the form DiracSlash[p1, p2, ..]."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ Dimension];

fci := fci = MakeContext["FeynCalcInternal"];

MakeContext[ DeclareNonCommutative, DiracGamma, Momentum];

DeclareNonCommutative[DiracSlash];

Options[DiracSlash] = {Dimension -> 4, fci -> True};


DiracSlash[DOT[a_,b__] opt___Rule] := Map[DiracGamma[LorentzIndex[#,
 Dimension /. {opt} /. Options[DiracSlash]],
 Dimension /. {opt} /. Options[DiracSlash]]&, DOT[a,b]];

DiracSlash[a_, opt___Rule] :=
DiracGamma[Momentum[a, Dimension /. {opt} /. Options[DiracSlash]],
           Dimension /. {opt} /. Options[DiracSlash]
          ] /; ( fci /. {opt} /. Options[DiracSlash] ) === True;

DiracSlash[a__, opt___Rule] :=
Apply[DOT,
 DiracGamma[Momentum[#, Dimension /. {opt} /. Options[DiracSlash]],
            Dimension /. {opt} /. Options[DiracSlash]
           ]& /@ {a}
     ] /; ( fci /. {opt} /. Options[DiracSlash] ) === True;

   DiracSlash /:
      MakeBoxes[
                DiracSlash[x__], TraditionalForm
               ] := MakeBoxes@@{fci[DiracSlash[x]], TraditionalForm};

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSlash | \n "]];
Null
