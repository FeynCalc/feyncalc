(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: RHI2FC *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary:  change
Roelof Hambergs notation  (eq. (3C.19) in his thesis) into
(FC) 2-loop OPE-integrals
*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`RHI2FC`",{"HighEnergyPhysics`FeynCalc`"}];

RHI2FC::"usage"=
"RHI2FC[exp] transforms all
RHI-integrals in FeynAmpDenominator form to the 2-loop OPE-integrals.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Dimension = MakeContext["CoreOptions","Dimension"];
FeynAmpDenominator = MakeContext["CoreObjects","FeynAmpDenominator"];
Momentum = MakeContext["CoreObjects","Momentum"];
Pair = MakeContext["CoreObjects","Pair"];
PropagatorDenominator = MakeContext["CoreObjects","PropagatorDenominator"];

MakeContext[
Factor2,
FeynAmpDenominatorCombine,
FeynCalcForm,
OPEDelta,
RHI
];

Options[RHI2FC] = {Dimension -> D, Momentum ->
                   {Global`q1, Global`q2, Global`p}};

RHI2FC[x_,opt___Rule] := x /. RHI -> rhf[opt];

rhf[opt___Rule][a_List,b_List] := rhf[opt] @@ Join[a, b];
rhf[opt___Rule][a_List,b_List,c_List] := rhf[opt] @@ Join[a, b, c];

rhf[opt___Rule][a1_,a2_,a3_,a4_,a5_,a6_,a7_,a8_,a9_,a10_] :=
     rhf[opt][0,0,0,0,0, a1,a2,a3,a4,a5, a6,a7,a8,a9,a10];

rhf[op___Rule][n1_,n2_,n3_,n4_,n5_, a1_,a2_,a3_,a4_,a5_,
               d1_,d2_,d3_,d4_,d5_] :=
Block[{dim,k1,k2,p, dp,dk1,dk2,dpk1,dpk2,dk1k2,nk12,nk22,k1k2,nk1p,
       nk1k2, nk2p, k12,k22,pk12,pk22,nx,fdp
      },
 dim = Dimension /. {op} /. Options[RHI2FC];
  k1 = (Momentum /. {op} /. Options[RHI2FC])[[1]];
  k2 = (Momentum /. {op} /. Options[RHI2FC])[[2]];
   p = (Momentum /. {op} /. Options[RHI2FC])[[3]];
dp    = Pair[Momentum[OPEDelta, dim], Momentum[p, dim]];
dk1   = Pair[Momentum[OPEDelta, dim], Momentum[k1, dim]];
dk2   = Pair[Momentum[OPEDelta, dim], Momentum[k2, dim]];
dpk1  = dp  - dk1;
dpk2  = dp  - dk2;
dk1k2 = dk1 - dk2;
   nk12  = Pair[Momentum[k1,dim], Momentum[k1,dim]];
   nk22  = Pair[Momentum[k2,dim], Momentum[k2,dim]];
   nk1k2 = Pair[Momentum[k1,dim], Momentum[k2,dim]];
   nk1p  = Pair[Momentum[k1,dim], Momentum[p,dim]];
   nk2p  = Pair[Momentum[k2,dim], Momentum[p,dim]];
   fdp[yy_] := FeynAmpDenominator[PropagatorDenominator[yy,0]];
   k12 = fdp[Momentum[k1, dim]];
   k22 = fdp[Momentum[k2, dim]];
   pk12 = fdp[-Momentum[p, dim] + Momentum[k1, dim]];
   pk22 = fdp[-Momentum[p, dim] + Momentum[k2, dim]];
   k1k2 = fdp[ Momentum[k1, dim] - Momentum[k2, dim]];
nx = nk12^n1 nk22^n2 nk1p^n3 nk2p^n4 nk1k2^n5 *
      dk1^a1 dk2^a2 dpk1^a3 dpk2^a4 dk1k2^a5 *
      (k12^d1 k22^d2 pk12^d3 pk22^d4 k1k2^d5);
nx = FeynAmpDenominatorCombine[nx];
nx];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "RHI2FC | \n "]];
Null
