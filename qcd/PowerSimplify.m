(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PowerSimplify *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 10 July '98 at 23:55 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  (-x)^a --> (-1)^a x^a;  (y-x)^n --> (-1)^n (x-y)^n *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`PowerSimplify`",
             "HighEnergyPhysics`FeynCalc`"];

PowerSimplify::"usage"=
"PowerSimplify[exp]  simplifies (-x)^a to (-1)^a x^a and
(y-x)^n to (-1)^n (x-y)^n; thus assuming that the exponent is
an integer (even if it is symbolic). Furthermore
(-1)^(a+n) and I^(a+n) are expanded and (I)^(2 m) -> (-1)^m and
(-1)^(n_Integer?EvenQ m) -> 1 and
(-1)^(n_Integer?OddQ m) -> (-1)^m and
(-1)^(-n) -> (-1)^n and Exp[I m Pi] -> (-1)^m.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[Factor2, Power2, ScaleMu, OPEm];

PowerSimplify[x_] := 
  Block[{nx, qcdsub = False, power3},
If[!FreeQ[x, ScaleMu],
   qcdsub = True;
   nx = x /. pow_[any_ /ScaleMu^2,exp_]:>
             power3[pow][any/ScaleMu^2,exp],
   nx = x
  ];
  nx = nx /. {(a_/;Head[a]===Plus || Head[a] === Times)^(w_) :>
       (PowerExpand[Factor2[one*a]^w] /. one -> 1),
        Power2[(a_/;Head[a]===Plus || Head[a] === Times),(w_)] :>
       (PowerExpand[Factor2[one*a]^w] /. (ab_Plus)^v_ :> Power2[ab, v] /.
        one -> 1)/.(-1)^vv_ :> Power2[-1,vv]
      } /. {(-1)^(a_Plus) :> Expand[(-1)^a]
           } /. {(n_Integer?Negative)^m_ :> (-1)^m (-n)^m
                }/.{((-1)^OPEm (1+(-1)^OPEm)) :> (1+(-1)^OPEm),
                    ((1-(-1)^OPEm)(1+(-1)^OPEm)) :> 0,
                    ((1+(-1)^OPEm)(1+(-1)^OPEm)) :> (2(1+(-1)^OPEm)),
                    (-1)^OPEm (1-(-1)^OPEm) :> (-1+(-1)^OPEm),
                  bbb_^(c_/;!FreeQ[c,Plus]) :> bbb^Expand[c]
                   }//.
            {
             (-1)^(n_Integer?EvenQ m_) :> 1,
             (-1)^(n_Integer?OddQ m_) :> (-1)^m,
             (-1)^(n_Integer?EvenQ m_. + i_) :> (-1)^i,
             (-1)^(n_Integer?OddQ m_. + i_) :> (-1)^(m+i) /; n=!=(-1),
             (-1)^(-n_) :> (-1)^n,
             I^(2 m_+i_.) :> (I)^i (-1)^m,
             (I/2)^(m_) I^m_ :> (-1)^m/2^m,
             I^(a_Plus) :> Expand[I^a],
             Exp[I Pi OPEi] :> (-1)^OPEi,
             Exp[I Pi OPEj] :> (-1)^OPEj,
             Exp[I Pi OPEm] :> (-1)^OPEm,
             HoldPattern[E^(em_ + Complex[0,n_] Pi)]  :> (-1)^n Exp[em],
             Power2[I, 2 m_ + i_.] :> I^i (-1)^m,
             Power2[I,(a_Plus)] :> Expand[I^a],
             Power2[(-1),(a_Plus)] :> Expand[(-1)^a]
            };
    If[qcdsub === True, nx = nx /. power3[poww_] :> poww];
 nx];


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PowerSimplify | \n "]];
Null
