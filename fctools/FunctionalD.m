(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FunctionalD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 26 February '99 at 16:36 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: functional differentiation *)

(* extension: March 1998 on request of Peter Cho *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FunctionalD`",
             "HighEnergyPhysics`FeynCalc`"];

FunctionalD::"usage"=
"FunctionalD[expr, 
{QuantumField[name, LorentzIndex[mu], ..., SUNIndex[a]][p], ...}] calculates
the functional derivative of expr with respect to the field list (with
incoming momenta p, etc.) and does the fourier transform.

FunctionalD[expr, {QuantumField[name, LorentzIndex[mu], ...
SUNIndex[a]], ...}] calculates the functional derivative and does 
partial integration but omits the x-space delta functions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

FeynCalcForm = MakeContext["FeynCalcForm"];

Collect2     = MakeContext["Collect2"];
DotSimplify  = MakeContext["DotSimplify"];
ExpandPartialD = MakeContext["ExpandPartialD"];
Explicit      = MakeContext["Explicit"];
ExplicitPartialD = MakeContext["ExplicitPartialD"];
fci          = MakeContext["FeynCalcInternal"];
FieldStrength = MakeContext["FieldStrength"];
Eps           = MakeContext["Eps"];
lorind       = MakeContext["LorentzIndex"];
mom          = MakeContext["Momentum"];
OPEDelta     = MakeContext["OPEDelta"];
Pair         = MakeContext["Pair"];
PairContract = MakeContext["PairContract"];
PairContract3 = MakeContext["PairContract3"];
PartialD = MakeContext["PartialD"];
LeftPartialD = MakeContext["LeftPartialD"];
LeftRightPartialD2 = MakeContext["LeftRightPartialD2"];
RightPartialD = MakeContext["RightPartialD"];
QuantumField = MakeContext["QuantumField"];
Select1      = MakeContext["Select1"];
Select2      = MakeContext["Select2"];
sund        := sund = MakeContext["SUNDelta"];
sundc       := sundc = MakeContext["SUNDeltaContract"];
SUNIndex     = MakeContext["SUNIndex"];
ExplicitSUNIndex     = MakeContext["ExplicitSUNIndex"];
SUNSimplify = MakeContext["SUNSimplify"];

(* ********************************************************************** *)
(* products of metric tensors  and SU(N) deltas *)
 g[{}, {}] = d[{}, {}] = 1;
 g[{x__} ,{y__}] := g[{x}, {y}] = 
  (PairContract3[{x}[[1]], {y}[[1]]] g[Rest[{x}], Rest[{y}]]);
 d[{x__} ,{y__}] := d[{x}, {y}] =
  (sundc[{x}[[1]], {y}[[1]]] d[Rest[{x}], Rest[{y}]]);
field = QuantumField;
(* ********************************************************************** *)

ddl[PartialD[mom[OPEDelta]^m_]][p_] := 
 (-1)^m I^m Pair[mom[p], mom[OPEDelta]]^m;

ddl[][_] :=1;
ddl[pa:PartialD[mom[OPEDelta]..]][p_] := (-1)^Length[{pa}] I^Length[{pa}] *
   Pair[mom[p], mom[OPEDelta]]^Length[{pa}];
ddl[pa:PartialD[mom[OPEDelta]]..][p_] :=  (-1)^Length[{pa}] I^Length[{pa}] *
   Pair[mom[p], mom[OPEDelta]]^Length[{pa}];

ddl[PartialD[lorind[m_]]][p_] := (-I) dDelta[mom[p], lorind[m]];
ddl[a___,PartialD[lorind[m_]], x___][p_] := 
 (-I) dDelta[mom[p], lorind[m]] ddl[a, x][p];

ddl[PartialD[mom[m_]]][p_] := (-I) dDelta[mom[p], mom[m]];
ddl[a___,PartialD[mom[m_]], x___][p_] := 
 (-I) dDelta[mom[p], mom[m]] ddl[a, x][p];
ddl[a___,PartialD[mom[OPEDelta]^m_], x___][p_] := 
 (-1)^m I^m Pair[mom[p], mom[OPEDelta]]^m  ddl[a, x][p];

dot2[___,0,___] := 0;
dot2[a___,1,b___] := DOT[a,b];


(* functional differentation function:  FunctionalD *)
FunctionalD[y_, fi_QuantumField, op___]:= FunctionalD[y,{fi}, op];
(* product rule, recursive *)


 FunctionalD[y_, {fi__, a_ }, op___] := 
    FunctionalD[FunctionalD[y, {a}, op], {fi}, op];

(*Added ExplicitSUNIndex. F.Orellana, 16/9-2002*)
 FunctionalD[y_/;!FreeQ[y,field[__][___]], 
            {field[nam_, lor___lorind, sun___SUNIndex|sun___ExplicitSUNIndex][pp___]}]:=
  Block[{xX},
   D[If[!FreeQ[y, FieldStrength], Explicit[y],y] /.
    field[a___, nam, b___][pe___] -> field[a,nam,b][pe][xX], xX] /.
    {field[py___PartialD, nam, li___lorind, col___SUNIndex|col___ExplicitSUNIndex][]'[xX] :>
     If[{py} === {}, 1, ddl[py][pp]] * g[{lor}, {li}] d[{sun}, {col}],
     field[py___PartialD, nam, li___mom, col___SUNIndex|col___ExplicitSUNIndex][]'[xX] :>
     If[{py} === {}, 1, ddl[py][pp]] * g[{lor}, {li}] d[{sun}, {col}]
    } /. 
      {field[aa___, nam, bb___][pee___][xX] :> field[aa, nam, bb][pee],
       sund  :> sundc, Pair :> PairContract3
      } /. DOT -> dot2 /. dot2 -> DOT /. dDelta -> PairContract3/.
           {sundc :> sund,  PairContract3 :> Pair} 
       ];



(*Definition below added by Frederik Orellana 25/2-1999*)

 FunctionalD[y_/;FreeQ[y,field[__][___]],
 {field[nam_, lor___lorind, sun___SUNIndex|sun___ExplicitSUNIndex][pp___]}]:=
  Block[{xX},
   D[y /.
    field[a___, nam, b___] -> field[a,nam,b][xX], xX] /.
    {field[py___PartialD, nam, li___lorind, col___SUNIndex|col___ExplicitSUNIndex]'[xX] :>
     If[{py} === {}, 1, ddl[py][pp]] * g[{lor}, {li}] d[{sun}, {col}],
     field[py___PartialD, nam, li___mom, col___SUNIndex|col___ExplicitSUNIndex]'[xX] :>
     If[{py} === {}, 1, ddl[py][pp]] * g[{lor}, {li}] d[{sun}, {col}]
    } /.
      {field[aa___, nam, bb___][xX] -> field[aa, nam, bb] /.
                 sund  :> sundc, Pair :> PairContract3
      } /. DOT -> dot2 /. dot2 -> DOT /. dDelta -> PairContract3/.
           {sundc :> sund,  PairContract3 :> Pair}
       ] ;

 (*End add*)

(* stay in x-space, but omit deltafunctions *)
        (* op: { first entry:
                   how to replace derivatives of deltafunctions, i.e.,
                   how to do integration by parts,
                 second entry:
                   final substitutions
               }
        *)
 FunctionalD[y_, {field[nam_, lor___lorind, sun___SUNIndex|sun___ExplicitSUNIndex]
                 }, opin_:{-RightPartialD[#]&, {}}
            ] :=
  Block[{xX,  pard, r, lastrep = {}, op = opin, ddelta, partiald2},
(* ddelta stands for the derivative of the delta funtion *)
  If[Head[op] =!= List, op = {op}];
  pard = (ExplicitPartialD[DOT[##] /. partiald2 -> First[op]])&;
  If[Length[op] > 1, lastrep = Flatten[Rest[op]]];
  r = 
(
   D[If[!FreeQ[y, FieldStrength], Explicit[y],y] /.
    field[a___, nam, b___] -> field[a,nam,b][][xX], xX]
) /.
    {field[py___PartialD, nam, li___lorind, col___SUNIndex|col___ExplicitSUNIndex][]'[xX] :>
     If[{py} === {}, 1, (ddelta[py] /. PartialD ->
                partiald2)] * g[{lor}, {li}] d[{sun}, {col}]
    } /. 
      {field[aa___, nam, bb___][][xX] :> field[aa, nam, bb] ,
       sund  :> sundc, PairContract3 :> PairContract
      } /. Times -> DOT /. DOT -> dot2 /. dot2 -> DOT /. 
           {sundc :> sund, Pair :> PairContract} /. 
           PairContract -> Pair ;
     r = Expand[DotSimplify[r]] //. 
             { sundc :> sund, Pair :> PairContract } /. 
              PairContract ->Pair;
(* operate from the left *)
  If[!FreeQ[r, ddelta], 
     If[Head[r] =!= Plus,
        r = ExpandPartialD[((Select2[r, ddelta] /. ddelta -> pard) .
                             Select1[r, ddelta])/.Times->DOT
                          ] 
        ,
        r = Sum[ExpandPartialD[((Select2[r[[i]], ddelta] /.
                                  ddelta -> pard) .
                                Select1[r[[i]], ddelta]
                                     ) /. Times -> DOT
                                    ],
                {i, Length[r]}]
       ]
    ];

  r = Expand[r, Pair] /. Pair->PairContract /. 
      {PairContract:>Pair, PairContract3 :> Pair};
  If[(!FreeQ2[r, {LeftPartialD, RightPartialD}]) && !FreeQ[r, Eps],
     If[Head[r] === Plus, 
        r = Map[ExpandPartialD, r],
        r = ExpandPartialD[r]
       ]
    ];

  r = r /. lastrep /. DOT -> dot2 /. dot2 -> DOT;

      r];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FunctionalD | \n "]];
Null
