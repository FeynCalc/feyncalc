(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PolarizationVectorExplicit *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PolarizationVectorExplicit *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`PolarizationVectorExplicit`",
             "HighEnergyPhysics`FeynCalc`"];

PolarizationVectorExplicit::usage = 
"PolarizationVectorExplicit[p, m]  transforms into the internal
FeynCalc representation.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

dimension            = MakeContext["Dimension"];
fci                  = MakeContext["FeynCalcInternal"];
fourvector           = MakeContext["FourVector"];
polarization         = MakeContext["Polarization"];
sunindex := sunindex = MakeContext["SUNIndex"];

PolarizationVectorExplicit[x__]:=
PolarizationVectorExplicit[x]=polVec[x];

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
If[$VeryVerbose > 0,
WriteString["stdout", "PolarizationVectorExplicit | \n "]];
Null
