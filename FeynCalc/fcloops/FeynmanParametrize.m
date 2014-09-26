(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: FeynmanParametrize*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`FeynmanParametrize`",
             {"HighEnergyPhysics`FeynCalc`"}];

FeynmanParametrize::"usage"=
"FeynmanParametrize[exp,k] introduces feynman parameters for
all one-loop integrals in exp (k = integration momentum).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

FeynmanParameterNames = MakeContext["CoreOptions","FeynmanParameterNames"];

MakeContext[
ExpandScalarProduct,
FeynAmpDenominatorCombine,
FeynAmpDenominator,
FeynCalcInternal,
Integratedx,
Momentum,
Pair,
PropagatorDenominator
];

Options[FeynmanParametrize] =
 {FeynmanParameterNames -> {Global`x, Global`y, Global`z}};

fpar[{par__}][k_][FeynAmpDenominator[
  PropagatorDenominator[Momentum[k_,di___] +
                        min1_. Momentum[p1_,di1___], 0],
  PropagatorDenominator[Momentum[k_,di___] +
                        min2_. Momentum[p2_,di2___],0
                       ]            ]
                 ] :=
  DOT[Integratedx[{par}[[1]],0,1] , (
  1/(Expand[ExpandScalarProduct[
     Pair[Momentum[k,di] + min1 Momentum[p1,di1],
          Momentum[k,di] + min1 Momentum[p1,di1]
         ] ({par}[[1]]) +
     (1-({par}[[1]])) *
     Pair[Momentum[k,di] + min2 Momentum[p2,di1],
          Momentum[k,di] + min2 Momentum[p2,di1]
         ]
           ]                   ]
    )^2                        )];


(* 2-point; alpha = 2*)
fpar[{par__}][k_][FeynAmpDenominator[
  PropagatorDenominator[Momentum[k_,di___], 0],
  PropagatorDenominator[Momentum[k_,di___] +
                        min_. Momentum[p_,___],0
                       ]            ]
                 ] :=
 DOT[Integratedx[{par}[[1]],0,1] , (
  1/(Pair[Momentum[k,di], Momentum[k,di]] -
     2 (-min) ({par}[[1]]) Pair[Momentum[k,di], Momentum[p,di]] +
     ({par}[[1]]) min^2 Pair[Momentum[p,di],Momentum[p,di]]
    )^2                        )];

pc[h_,m_] := Expand[ExpandScalarProduct[Pair[h,h]]-m^2];

fpar[{par__}][_][FeynAmpDenominator[
  n1:PropagatorDenominator[a_, m1_]..,
  n2:PropagatorDenominator[b_ ,m2_]..]
                 ] :=
 DOT[Integratedx[{par}[[1]],0,1] , (
   Gamma[Length[{n1}]+Length[{n2}]] /
  Gamma[Length[{n1}]] Gamma[Length[{n2}]] *
    ({par}[[1]])^(Length[{n1}]-1) (1-({par}[[1]]))^(Length[{n2}]-1)*
  1/(pc[a,m1] ({par}[[1]]) + (1-({par}[[1]])) pc[b,m2]
    )^(Length[{n1}]+Length[{n2}])
                                )];

(* 2-point; alpha = 3*)
fpar[{par__}][k_][FeynAmpDenominator[
  PropagatorDenominator[Momentum[k_,di___], 0],
  PropagatorDenominator[Momentum[k_,di___], 0],
  PropagatorDenominator[Momentum[k_,di___] +
                        min_. Momentum[p_,___],0
                       ]            ]
                 ] :=
  Gamma[3] DOT[Integratedx[{par}[[1]],0,1] , (
  (1-({par}[[1]]))/(Pair[Momentum[k,di], Momentum[k,di]] -
     2 (-min) ({par}[[1]]) Pair[Momentum[k,di], Momentum[p,di]] +
     ({par}[[1]]) min^2 Pair[Momentum[p,di],Momentum[p,di]]
    )^3                        )];

(* 3-point; alpha = 3  ++++++++++++++++++++++++++++++++++++++ *)
fpar[{par__}][k_][FeynAmpDenominator[
  PropagatorDenominator[Momentum[k_,di___], 0],
  PropagatorDenominator[Momentum[k_,di___] + p1_,0],
  PropagatorDenominator[Momentum[k_,di___] + p3_,0]
                                    ]
                 ] := Block[{x,y,k2,kp1,kp3,p32},
 x = {par}[[2]];
 y = {par}[[1]];
 k2 = Pair[Momentum[k,di], Momentum[k,di]];
 kp1 = - Pair[Momentum[k,di], p1]//ExpandScalarProduct;
 kp3 = - Pair[Momentum[k,di], p3]//ExpandScalarProduct;
 p12 = Pair[p1, p1] // ExpandScalarProduct;
 p32 = Pair[p3, p3] // ExpandScalarProduct;
 Gamma[3] DOT[Integratedx[y,0,1] , Integratedx[x,0,1] , (
  y /( k2-2y x kp1 - 2 y (1-x) kp3 + y x p12 + y (1-x) p32
     )^3                                    )] ];

(* 3-point; alpha = 4  ++++++++++++++++++++++++++++++++++++++ *)
fpar[{par__}][k_][FeynAmpDenominator[
  PropagatorDenominator[Momentum[k_,di___], 0],
  PropagatorDenominator[Momentum[k_,di___], 0],
  PropagatorDenominator[Momentum[k_,di___] + p1_,0],
  PropagatorDenominator[Momentum[k_,di___] + p3_,0]
                                    ]
                 ] := Block[{x,y,k2,kp1,kp3,p32},
 x = {par}[[1]]; y = {par}[[2]];
 k2 = Pair[Momentum[k,di], Momentum[k,di]];
 kp1 = - Pair[Momentum[k,di], p1]//ExpandScalarProduct;
 kp3 = - Pair[Momentum[k,di], p3]//ExpandScalarProduct;
 p12 = Pair[p1, p1] // ExpandScalarProduct;
 p32 = Pair[p3, p3] // ExpandScalarProduct;
 Gamma[4] DOT[Integratedx[y,0,1] , Integratedx[x,0,1] , (
  y (1-y) /( k2-2y x kp1 - 2 y (1-x) kp3 + y x p12 + y (1-x) p32
     )^4                                    )] ];

(*
(* general a^i b^j c^k *)
fpar[{par__}][_][FeynAmpDenominator[
  ni:PropagatorDenominator[na_, m1_]..,
  nj:PropagatorDenominator[nb_ ,m2_]..,
  nk:PropagatorDenominator[nc_ ,m3_]..  ]
                 ] :=
  Block[{i=Length[{ni}],j=Length[{nj}],k=Length[{nk}],
         x = {par}[[1]], y = {par}[[2]],
         a = pc[na, m1], b = pc[nb, m2], c = pc[nc, m3]
        },
If[$VeryVerbose > 0, Print["GENERAL fpar "]];
        DOT[Integratedx[x,0,1] , Integratedx[y,0,1] ,
   (Gamma[i+j+k]/Gamma[i]/Gamma[j]/Gamma[k] *
    x^(i-1) (1-x)^(j-1) y^(i+j-1) (1-y)^(k-1) /
    ((a x + (1-x) b) y + (1-y) c)^(i+j+k)
   )]   ];
*)

(* If the above fail. F.Orellana, 11/9-2002 *)
fpar[{par__}][k_][p_] := (Print["Unknown denominator ", p, ". Try FeynmanParametrize1."]; p);

(* ********************************************************** *)
FeynmanParametrize[exp_, Momentum[q_,___], opt___Rule] :=
 FeynmanParametrize[exp,q,opt];
FeynmanParametrize[exp_,q_,opt___Rule] := Block[{},
        FeynAmpDenominatorCombine[exp] /.
          FeynAmpDenominator[aa__] :>
           Apply[
           fpar[FeynmanParameterNames/.{opt}/.
                Options[FeynmanParametrize]
               ][q//nomom],{FeynCalcInternal[FeynAmpDenominator[aa]]}
                ]                              ];

nomom[y_] := y/.Momentum[aa_,___]:>aa;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynmanParametrize | \n "]];
Null
