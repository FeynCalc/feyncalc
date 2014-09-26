(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: LeviCivita *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`LeviCivita`",{"HighEnergyPhysics`FeynCalc`"}];

LeviCivita::"usage" =
"LeviCivita[mu, nu, ro, si] is an input  function for the
totally antisymmetric Levi-Civita tensor.
It evaluates automatically
to the internal representation Eps[ LorentzIndex[mu],  LorentzIndex[nu],
LorentzIndex[ro], LorentzIndex[si] ]
(or with a second argument in LorentzIndex for the Dimension,
if the option Dimension of LeviCivita is changed).  \n
LeviCivita[mu, nu ...][ p, ...] evaluates to
Eps[LorentzIndex[mu], LorentzIndex[nu], ..., Momentum[p], ...].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Dimension = MakeContext["CoreOptions","Dimension"];

MakeContext[ Eps, EpsEvaluate, FreeQ2, LorentzIndex, Momentum];

LeviCivita[a__Integer] := Eps[a];

Options[LeviCivita] = {Dimension -> 4};

frlivc[x_?NumberQ] :=True;
frlivc[x_]         := True/;(Head[x]=!=Momentum) &&
                        (Head[x]=!=LorentzIndex);
frlivc[x_,y__] := True/;FreeQ2[FixedPoint[ReleaseHold,{x,y}]
                               ,{Momentum, LorentzIndex}];

HoldPattern[LeviCivita[a___, b_, c___, b_, k___, ops___Rule]] := 0;
LeviCivita[ a__, ops___Rule ]:= ( Eps @@ ( LorentzIndex /@ {a} )
                                ) /; frlivc[a] && FreeQ[{a}, Rule] &&
                (Length[{a}] === 4) &&
                ( (Dimension /. {ops} /. Options[LeviCivita]) === 4);

LeviCivita[ a__, ops___Rule ]:= ( Eps@@(
   LorentzIndex[#, Dimension/.{ops}/.Options[LeviCivita]]& /@{a}
         )                     ) /;
   frlivc[a] && FreeQ[{a},Rule] && (Length[{a}] === 4) &&
   ( (Dimension /. {ops} /. Options[LeviCivita]) =!= 4);

LeviCivita[x___, ops___Rule][y___, ru___Rule] :=
  Eps @@ Join[Map[
    LorentzIndex[#, Dimension /. {ops} /. Options[LeviCivita]]& ,{x}
                 ],
              Map[
      Momentum[#, Dimension /. {ru} /. Options[LeviCivita]]& ,{y}
                 ]
             ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "LeviCivita | \n "]];
Null
