(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: TLIHYP *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`TLIHYP`",
             "HighEnergyPhysics`FeynCalc`"];

TLIHYP::"usage"= "TLIHYP[exp] expresses TLI's in exp. in terms
of hypergeometric functions, where possible.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[Epsilon, FeynmanParameterNames, OPEDelta, 
            ScalarProduct, Smu, TLI, Momentum];

(* Fri Feb 16 04:09:29 MET 1996 *)
(* By R. Scharf *)

(* General parametrizations for graph #1, #2, #3, #4 *)

(**********************************  preliminaries ******************)
$BLABLA = True;  (* a global variable controling comments *)
MCH[w_ /; Head[w]=!=Integer]     := True;  (* the m of the spin *)
integ[y_ /; Head[y] === Integer] := True;  (* check for integer*)
pinteg[y_Integer?Positive] := True;  (* check for positive integer*)
pinteg[y_ /; DataType[y, PositiveInteger] === True] := True;

comment[st_String] := If[$BLABLA = True, Print[st]];

Options[TLIHYP] = {FeynmanParameterNames -> Global`x,
                   Momentum -> Global`p
                 };

TLIHYP[exp_, opt___Rule] := Block[{tlihyp, so, x, p},
x = FeynmanParameterNames /. {opt} /.  Options[TLIHYP];
p = Momentum /. {opt} /.  Options[TLIHYP];

so[r] = ScalarProduct[OPEDelta, p];

rule2F1 = {HypergeometricPFQ[{alpha_, beta_}, {gamma_}, z_] :>
Hypergeometric2F1[alpha, beta, gamma, z]};

(* ************************   tlihyp, RHYP5     ************************ *)

tlihyp[tLI[{m_, 1, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?integ, M_Symbol}, 0, a3_?integ,
           {a4_?integ, M_Symbol}, {a5_?integ, M_Symbol}}
         ]
     ] := so[r] *
      tlihyp[tLI[{m, 0, g3, g4, g5}, {{a1, M}, 0, a3, {a4, M}, {a5,M}}]
            ]  -
      tlihyp[tLI[{m, 0, g3, g4+1,g5}, {{a1, M}, 0, a3, {a4, M}, {a5,M}}]
            ];

tlihyp[tLI[{m_ , 0, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?integ, M_Symbol}, 0, a3_?integ, 
           {a4_?integ, M_Symbol}, {a5_?integ, M_Symbol}}
         ]
     ] := (
comment["Using RHYP5"];
(
(Smu^2) . ((M^2)^(4 - a1 - a3 - a4 - a5)*so[r]^(g3 + g4 + g5 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a3 + a4 + a5 + g5)) .
(
  Gamma[-4 + a1 + a3 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1])
) .
(
    (x^(2 - a1 + Epsilon/2)*y^(-1 + a1 + g3 + g4 + g5)*
     (x + y)^(-2 - Epsilon/2 - g3 - g4 - g5 - m)*
     Gamma[2 - a1 - a3 + Epsilon/2]*
     Gamma[-2 + a1 + a3 + a5 - Epsilon/2 + g4]*
     Gamma[-2 + a1 + a3 + a4 - Epsilon/2 + g5])/
   (Gamma[2 - a1 + Epsilon/2]*Gamma[-4 + 2*a1 + 2*a3 + a4 + a5 - Epsilon +
       g4 + g5]*Gamma[a4]*Gamma[a5])*
 HypergeometricPFQ[{-4 + a1 + a3 + a4 + a5 - Epsilon, -1 + a1 - Epsilon/2,
  -2 + a1 + a3 + a5 - Epsilon/2 + g4, -2 + a1 + a3 + a4 - Epsilon/2 + g5},
 {-1 + a1 + a3 - Epsilon/2, 
  (-4 + 2*a1 + 2*a3 + a4 + a5 - Epsilon + g4 + g5)/2,
  (-3 + 2*a1 + 2*a3 + a4 + a5 - Epsilon + g4 + g5)/2}, -y^2/(4 x (x + y))] +
     (x^a3*y^(3 - a1 - 2*a3 + Epsilon + g3 + g4 + g5)*
     (x + y)^(-4 + a1 + a3 - Epsilon - g3 - g4 - g5 - m)*
     Gamma[-2 + a1 + a3 - Epsilon/2]*Gamma[-2 + a4 + a5 - Epsilon/2]*
     Pochhammer[a5, g4]*Pochhammer[a4, g5])/
  (Gamma[a3]*Gamma[-4 + a1 + a3 + a4 + a5 - Epsilon]*
   Gamma[a4 + a5 + g4 + g5])*
   HypergeometricPFQ[{1 - a3, -2 + a4 + a5 - Epsilon/2, a5 + g4, a4 + g5},
   {3 - a1 - a3 + Epsilon/2, (a4 + a5 + g4 + g5)/2, 
      (1 + a4 + a5 + g4 + g5)/2},
                   - y^2/(4 x (x+y))]
)
) /. {Dot :> Times, EulerGamma :> 0, y :> 1 - x}
          )/.rule2F1;
(* ************************   tlihyp, RHYP5  ************************ *)


(* ************************   TLIHYP, RHYP1 ************************ *)

tlihyp[ tLI[{m_, 0, g3_?integ, g4_?integ, g5_?integ},
           {{a1_?integ, M_Symbol}, {a2_?integ, M_Symbol}, 
             0, a4_?integ, a5_?integ}
          ]
     ] := (
comment["Using RHYP1"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a4 - a5)*so[r]^(g3 + g4 + g5 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a4 + a5 + g5)) .
(
  Gamma[-4 + a1 + a2 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a4]*Gamma[a5])
) .
(
   (1 - x)^(7 - a1 - a2 - 2*a4 - 2*a5 + 2*Epsilon + g3 + g4 + g5)
) .
(
  (x^(-2 + a2 + a4 + a5 - Epsilon/2)*Gamma[a2]*
      Gamma[2 - a2 - a4 + Epsilon/2]*Gamma[2 - a4 + Epsilon/2 + g4]*
      Gamma[2 - a2 - a5 + Epsilon/2 + g5]*
      HypergeometricPFQ[{a2, 3 - a1 - a5 + Epsilon/2, 
        2 - a4 + Epsilon/2 + g4}, 
       {-1 + a2 + a4 - Epsilon/2, -1 + a2 + a5 - Epsilon/2 - g5}, x])/
(Gamma[2 - a4 + Epsilon/2]*Gamma[4 - a2 - a4 - a5 + Epsilon + g4 + g5]) + 
  (x^(a5)*Gamma[-2 + a2 + a4 - Epsilon/2]*
      Gamma[-2 + a1 + a5 - Epsilon/2]*Gamma[4 - a2 - 2*a4 + Epsilon + g4]*
      Gamma[a4 - a5 + g5]*HypergeometricPFQ[{2 - a4 + Epsilon/2, 
        5 - a1 - a2 - a4 - a5 + Epsilon, 4 - a2 - 2*a4 + Epsilon + g4}, 
       {3 - a2 - a4 + Epsilon/2, 1 - a4 + a5 - g5}, x])/
    (Gamma[-4 + a1 + a2 + a4 + a5 - Epsilon]*
      Gamma[4 - a2 - a4 - a5 + Epsilon + g4 + g5]) + 
   (x^(a4 + g5)*Gamma[-2 + a1 + a5 - Epsilon/2]*
      Gamma[-a4 + a5 - g5]*Gamma[-2 + a2 + a5 - Epsilon/2 - g5]*
      Gamma[2 - a5 + Epsilon/2 + g5]*
      HypergeometricPFQ[{2 - a5 + Epsilon/2 + g5, 
        5 - a1 - a2 - 2*a5 + Epsilon + g5, 
        4 - a2 - a4 - a5 + Epsilon + g4 + g5}, 
       {1 + a4 - a5 + g5, 3 - a2 - a5 + Epsilon/2 + g5}, x])/
    (Gamma[2 - a4 + Epsilon/2]*Gamma[-4 + a1 + a2 + 2*a5 - Epsilon - g5])
)
) /. {Dot :> Times, EulerGamma :> 0}
          )/.rule2F1;
(* ************************   tlihyp, RHYP1ende   ************************ *)

(* ************************   tlihyp, RHYP3     ************************ *)
tlihyp[tLI[{g1_?integ, m_, 1, g4_?integ, g5_?integ},
          {{a1_?integ, M_Symbol}, {a2_?integ, M_Symbol}, 0, 
              a4_?pinteg, a5_?pinteg}
         ]
      ] := so[r] tlihyp[tLI[{g1, m, 0, g4, g5}, {{a1, M}, {a2, M}, 0, a4, a5}]
                       ] - 
               tlihyp[tLI[{g1+1, m, 0, g4, g5}, {{a1, M}, {a2, M}, 0, a4, a5}]
                       ];

tlihyp[tLI[{g1_?integ, m_ , 0, g4_?integ, g5_?integ},
          {{a1_?integ, M_Symbol}, {a2_?integ, M_Symbol}, 0, 
              a4_?integ, a5_?integ}
         ]
     ] := (
comment["Using RHYP3"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a4 - a5)*so[r]^(g1 + g4 + g5 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a4 + a5 + g5)) .
(
  Gamma[-4 + a1 + a2 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a4]*Gamma[a5])
) .
(
  (1-y)^(a4 + g1 + g5)
) .
(
     (y^(3 - a2 - 2*a4 + Epsilon + g4)*Gamma[-2 + a2 + a4 - Epsilon/2]*
        Gamma[-2 + a1 + a5 - Epsilon/2]*Gamma[a5 + g1]*
        Gamma[4 - a1 - 2*a5 + Epsilon + g5]*
        HypergeometricPFQ[{1 - a2, -2 + a1 + a5 - Epsilon/2, a5 + g1}, 
         {3 - a2 - a4 + Epsilon/2, -3 + a1 + 2*a5 - Epsilon - g5}, y])/
      (Gamma[-4 + a1 + a2 + a4 + a5 - Epsilon]*
        Gamma[4 - a1 - a5 + Epsilon + g1 + g5]) + 
     (y^(1 - a4 + Epsilon/2 + g4)*Gamma[a2]*Gamma[2 - a2 - a4 + Epsilon/2]*
        Gamma[-2 + a2 + a4 + a5 - Epsilon/2 + g1]*
        Gamma[6 - a1 - a2 - a4 - 2*a5 + (3*Epsilon)/2 + g5]*
        HypergeometricPFQ[{-4 + a1 + a2 + a4 + a5 - Epsilon, 
          -1 + a4 - Epsilon/2, -2 + a2 + a4 + a5 - Epsilon/2 + g1}, 
         {-1 + a2 + a4 - Epsilon/2, 
          -5 + a1 + a2 + a4 + 2*a5 - (3*Epsilon)/2 - g5}, y])/
      (Gamma[2 - a4 + Epsilon/2]*Gamma[4 - a1 - a5 + Epsilon + g1 + g5]) + 
     (y^(7 - a1 - a2 - 2*a4 - 2*a5 + 2*Epsilon + g4 + g5)*Gamma[a2]*
        Gamma[-6 + a1 + a2 + a4 + 2*a5 - (3*Epsilon)/2 - g5]*
        Gamma[-4 + a1 + 2*a5 - Epsilon - g5]*Gamma[2 - a5 + Epsilon/2 + g5]*
        HypergeometricPFQ[{2 - a5 + Epsilon/2 + g5, 
          5 - a1 - a2 - 2*a5 + Epsilon + g5, 
          4 - a1 - a5 + Epsilon + g1 + g5}, 
         {5 - a1 - 2*a5 + Epsilon + g5, 
          7 - a1 - a2 - a4 - 2*a5 + (3*Epsilon)/2 + g5}, y])/
      (Gamma[-4 + a1 + a2 + a4 + a5 - Epsilon]*
        Gamma[-4 + a1 + a2 + 2*a5 - Epsilon - g5])
)
) /. {Dot :> Times, EulerGamma :> 0, y :> 1 - x}
          )/.rule2F1;
(* ************************   tlihyp, RHYP3  ************************ *)

exp /. TLI -> tli /. tli[ww__] :> tlihyp[tLI[ww]] /. 
        tlihyp -> Identity /. tLI -> TLI
];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TLIHYP | \n "]];
Null
