(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TLI2FC *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  change
Roelofs notation  (eq. (3C.19)) into
(FC) 2-loop OPE-integrals
*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`TLI2FC`",{"HighEnergyPhysics`FeynCalc`"}];

TLI2FC::"usage"=
"TLI2FC[exp] transforms all
TLI-integrals in exp to the FAD form.";

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
FeynCalcExternal,
FeynCalcForm,
OPEDelta,
TLI
];

Options[TLI2FC] = {Dimension -> D,
                    FeynCalcExternal -> False,
                    Momentum ->
                   {Global`q1, Global`q2, Global`p}};

TLI2FC[x_,opt___Rule] := If[FreeQ[x,TLI], x, x /. TLI -> rhf[opt]];

rhf[opt___Rule][a_List,b_List] := rhf[opt] @@ Join[a, b];
rhf[opt___Rule][a_List,b_List,c_List] := rhf[opt] @@ Join[a, b, c];

rhf[opt___Rule][a1_,a2_,a3_,a4_,a5_,a6_,a7_,a8_,a9_,a10_] :=
     rhf[opt][0,0,0,0,0, a1,a2,a3,a4,a5, a6,a7,a8,a9,a10];

(*
pf[a_Integer] := a;
*)
pf[a_] := a;
pf[{a_,em_ /; em=!=0}] := a;

rhf[op___Rule][n1_,n2_,n3_,n4_,n5_, a1_,a2_,a3_,a4_,a5_,
               d1_,d2_,d3_,d4_,d5_] :=
Block[{dim,k1,k2,p, dp,dk1,dk2,dpk1,dpk2,dk1k2,nk12,nk22,k1k2,nk1p,
       nk1k2, nk2p, k12,k22,pk12,pk22,nx,fdp
      },
 dim = Dimension /. {op} /. Options[TLI2FC];
  k1 = (Momentum /. {op} /. Options[TLI2FC])[[1]];
  k2 = (Momentum /. {op} /. Options[TLI2FC])[[2]];
   p = (Momentum /. {op} /. Options[TLI2FC])[[3]];
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
   fdp[yy_,_] := FeynAmpDenominator[PropagatorDenominator[yy,0]];
   fdp[yy_,{_,em_}] :=
                 FeynAmpDenominator[PropagatorDenominator[yy, em]];
   k12 = fdp[Momentum[k1, dim], d1];
   k22 = fdp[Momentum[k2, dim], d2];
   pk12 = fdp[-Momentum[p, dim] + Momentum[k1, dim], d3];
   pk22 = fdp[-Momentum[p, dim] + Momentum[k2, dim], d4];
   k1k2 = fdp[ Momentum[k1, dim] - Momentum[k2, dim], d5];
nx = nk12^n1 nk22^n2 nk1p^n3 nk2p^n4 nk1k2^n5 *
      dk1^a1 dk2^a2 dpk1^a3 dpk2^a4 dk1k2^a5 *
      (k12^pf[d1] k22^pf[d2] pk12^pf[d3] pk22^pf[d4] k1k2^pf[d5]);
nx = FeynAmpDenominatorCombine[nx];
If[(FeynCalcExternal /. {op} /. Options[TLI2FC]) === True,
   nx = FeynCalcExternal[nx];
  ];
nx];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TLI2FC | \n "]];
Null
