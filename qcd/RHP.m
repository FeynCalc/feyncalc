(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: RHP*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  projectors for eq. (3.2.17) - (3.2.20) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`RHP`",
             "HighEnergyPhysics`FeynCalc`"];

RHP::"usage"= "RHP[i, mu, nu, p] with i from 1 to 4 gives the projectors for
RHO[i]. RHP[mu, nu, p] gives Sum[RHP[i,mu, nu, p] RHO[i], {i, 4}] collected
with respect to mu and nu.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
Collect2,
Dimension,
LorentzIndex,
Momentum,
OPEDelta,
Pair,
RHO];


Options[RHP] = {Dimension -> D};

RHP[mu_, nu_, p_, opt___Rule] := Collect2[
    Sum[RHP[j, mu, nu, p, opt] RHO[j], {j, 4}], {mu, nu}
                                     ];

RHP[i_Integer, mu_, nu_, p_, opt___Rule] := Block[{n, rhp},

n = Dimension /. {opt} /. Options[RHP];

rhp[1] = Pair[LorentzIndex[mu, n], LorentzIndex[nu, n]]/(-2 + n) +
   (Pair[LorentzIndex[mu, n], Momentum[p, n]]*
       Pair[LorentzIndex[nu, n], Momentum[OPEDelta, n]] +
      Pair[LorentzIndex[mu, n], Momentum[OPEDelta, n]]*
       Pair[LorentzIndex[nu, n], Momentum[p, n]])/
    ((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]) +
   (Pair[LorentzIndex[mu, n], Momentum[OPEDelta, n]]*
      Pair[LorentzIndex[nu, n], Momentum[OPEDelta, n]]*
      Pair[Momentum[p, n], Momentum[p, n]])/
    ((-2 + n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2);

rhp[2] = (Pair[LorentzIndex[mu, n], Momentum[OPEDelta, n]]*
     Pair[LorentzIndex[nu, n], Momentum[OPEDelta, n]]*
     Pair[Momentum[p, n], Momentum[p, n]])/
     Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2;

rhp[3] = -((Pair[LorentzIndex[mu, n], Momentum[p, n]]*
         Pair[LorentzIndex[nu, n], Momentum[OPEDelta, n]] +
        Pair[LorentzIndex[mu, n], Momentum[OPEDelta, n]]*
         Pair[LorentzIndex[nu, n], Momentum[p, n]])/
      Pair[Momentum[OPEDelta, n], Momentum[p, n]]) +
   (Pair[LorentzIndex[mu, n], Momentum[p, n]]*
      Pair[LorentzIndex[nu, n], Momentum[p, n]])/
    Pair[Momentum[p, n], Momentum[p, n]];

rhp[4] = (Pair[LorentzIndex[mu, n], Momentum[p, n]]*
     Pair[LorentzIndex[nu, n], Momentum[p, n]])/
   ((*at 2-loop: 1 instead of 2 *) 2 Pair[Momentum[p, n], Momentum[p, n]]);

rhp[i]] /; 0 < i < 5;


End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "RHP | \n "]];
Null
