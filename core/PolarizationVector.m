(* :Summary: PolarizationVector *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`PolarizationVector`",
               "HighEnergyPhysics`FeynCalc`"];


PolarizationVector::"usage" =
"PolarizationVector[p, mu] gives a polarization vector.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

dimension := dimension   = MakeContext["Dimension"];
fci := fci               = MakeContext["FeynCalcInternal"];
fourvector := fourvector = MakeContext["FourVector"];
polarization := polarization = MakeContext["Polarization"];
sunindex := sunindex = MakeContext["SUNIndex"];

PolarizationVector[x_,{y_,z_}]:= PolarizationVector[x, y, z];
PolarizationVector[x__]:=
(PolarizationVector[x]=polVec[x] )/; FreeQ[{x}, Pattern] &&
(*Hack to keep unevaluated when given "FeynArts arguments". F.Orellana, 29/3-2003*)
  (Length[{x}]===2 ||
  (*FA uses particle name (which is alway not AtomQ) as first argument*)
  AtomQ[{x}[[1]]] || 
  Head[{x}[[-1]]===sunindex]);

fourv[x__] := fci[fourvector[x]];
polVec[k_polarization,mu_]:=
     fourv[k, mu, dimension -> 4 ];
polVec[k_polarization,mu_,glu_]:=
     fourv[polarization[
          k, I, sunindex[glu/.sunindex->Identity]],
                mu, dimension->4 ];
polVec[k_,mu_]:=
     fourv[polarization[k, I], mu, dimension->4 ];

polVec[k_,mu_,glu_]:=
     If[FreeQ[glu, Blank],
        fourv[polarization[k, I,
                   sunindex[glu/.sunindex->Identity]],
                   mu, dimension->4 ],
        fourv[polarization[k, I, glu], mu, dimension -> 4]
       ];

End[]; EndPackage[];



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

