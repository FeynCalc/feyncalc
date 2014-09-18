(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TLIFP *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 28 May '99 at 14:26 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`TLIFP`",{"HighEnergyPhysics`FeynCalc`"}];

TLIFP::"usage"= "TLIFP[exp] does Feynman-Parametrizations of TLI's in exp.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[ChangeDimension,Collect2, DeltaFunction,
            Epsilon, FeynmanParameterNames, GammaExpand, Momentum,
            OPEDelta, OPEi,
            OPEm, PositiveInteger, ScalarProduct, Smu, TLI, TLI2];

(* 15. Feb 1996; *)

(* General parametrizations for graph #1, #2, #3, #4 *)

(**********************************  preliminaries ******************)

(* Map[Integrate[#, {t, 0, 1}, {u, 0, 1}], ...] is understood *)


Options[TLIFP] = { FeynmanParameterNames -> 
                       {Global`x, Global`t, Global`u, Global`s,
                        Global`y},
                   GammaExpand -> True,
                   Momentum -> Global`p,
                   Print -> True
                 };

TLIFP[exp_, opt___Rule] := Block[
  {tlifp, so,x,t,u,s,y,p, r1,r2,r3,gaex, simp, comment, MCH, integ, pinteg},
  {x,t,u,s,y } = FeynmanParameterNames /. {opt} /.  Options[TLIFP];
p = Momentum /. {opt} /. Options[TLIFP];
gaex = GammaExpand /. {opt} /. Options[TLIFP];
If[gaex === True, 
   simp[zz_] := Collect2[GammaExpand[zz],Hypergeometric2F1];
   ,
   simp[zz_] := zz
  ];
SetAttributes[comment, HoldAll];
comment[yy_] := If[Print/.{opt}/.Options[TLIFP], Print[yy]];
MCH[w_ /; Head[w]=!=Integer]     := True;  (*the m of the spin *)
integ[yy_ /; Head[yy] === Integer] := True;  (* check for integer*)
integ[yy_] := True /; DataType[yy, PositiveInteger] === True;  
(* check for integer*)
pinteg[yy_Integer?Positive] := True;  (* check for positive integer*)
pinteg[yy_ /; DataType[yy, PositiveInteger] === True] := True;


so[r] = ScalarProduct[OPEDelta, p];

(* ************************   TLIFP, RFP1 ************************ *)
tlifp[ tLI[{m_, g2_?integ, g3_?integ, g4_?integ, g5_?integ},
           {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol}, 
             0, a4_?pinteg, a5_?pinteg}
          ]
     ] := (
comment["Using RFP1"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a4 - a5)*so[r]^(g2 + g3 + g4 + g5 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a4 + a5 + g5)) .
(
  Gamma[-4 + a1 + a2 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a4]*Gamma[a5])) .
(
  x^(a4 + g2 + g5)* 
  (1 - x)^(7 - a1 - a2 - 2*a4 - 2*a5 + 2*Epsilon + g3 +g4 + g5)
) .
(
  u^(1 - a5 + Epsilon/2 + g5) (1 - u)^(1 - a4 + Epsilon/2 + g4) *
  (1 - u*y)^(-4 + a2 + a4 + a5 - Epsilon - g2 - g4 - g5)
) .
(
  t^(-1 + a2) (1 - t)^(-3 + a1 + a5 - Epsilon/2) *
  (1 - t*u)^(4 - a1 - a2 - a4 - a5 + Epsilon)
)
) /. {Dot :> Times, EulerGamma :> 0, y :> 1 - x}
          ) /; 2 - a2 - a4 > 0;


(* ************************   tlifp, RFP1ende   ************************ *)

(* ************************   tlifp, RFP2       ************************ *)
(* use the following param. for: 2 - a2 - a4 + Epsilon/2 < 0 *)
tlifp[tLI[{m_, g2_?integ, g3_?integ, g4_?integ, g5_?integ},
      {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol}, 
       0, a4_?pinteg, a5_?pinteg}]
     ] := (
comment["Using RFP2"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a4 - a5)*so[r]^(g2 + g3 + g4 + g5 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a4 + a5 + g5)) .
(
  Gamma[-4 + a1 + a2 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a4]*Gamma[a5])) .
(
  x^(a4 + g2 + g5)*
  (1 - x)^(7 - a1 - a2 - 2*a4 - 2*a5 + 2*Epsilon + g3 + g4 + g5)
) .
(
  u^(1 - a5 + Epsilon/2 + g5) (1 - u)^(3 - a2 - 2 a4 + Epsilon + g4) *
  (1 - u*y)^(-4 + a2 + a4 + a5 - Epsilon - g2 - g4 - g5)
) .
(
  t^(-3 + a1 + a5 - Epsilon/2) (1 - t)^(-1 + a2) *
  (1 - t*u)^(-2 + a4 - Epsilon/2)
)
) /. {Dot :> Times, EulerGamma :> 0, y :> 1 - x}
          ) /; 2 - a2 - a4 <= 0;
(*************************   tlifp, RFP2ende  ************************ *)



(**********************   tlifp, RFP3prime     ************************ *)
(* use the following param. for: 4 - a1 - 2 a5 + Epsilon + g5 > 0 *)

tlifp[tLI[{g1_?integ, m_, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol}, 0, 
              a4_?pinteg, a5_?pinteg}
         ]
     ] := (
comment["Using RFP3"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a4 - a5)*so[r]^(g1 + g3 + g4 + g5 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a4 + a5 + g5)) .
(Gamma[-4 + a1 + a2 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a4]*Gamma[a5])) .
(x^(a4 + g1 + g5)*
  (1 - x)^(7 - a1 - a2 - 2*a4 - 2*a5 + 2*Epsilon + g3 + g4 + g5)) .
(u^(1 - a5 + Epsilon/2 + g5)*(1 - u)^(-3 + a2 + a4 + a5 - Epsilon/2 + g1)*
  (1 - u*x)^(-4 + a1 + a5 - Epsilon - g1 - g3 - g5)) .
(t^(-1 + a2)*(1 - t)^(-3 + a1 + a5 - Epsilon/2)*
  (1 - t*u)^(4 - a1 - a2 - a4 - a5 + Epsilon))
) /. {Dot :> Times, EulerGamma :> 0}
          ) /; 2 - a2 - a4 + Epsilon/2 > 0;
(* ************************   tlifp, RFP3prime  ***********************  *)


(* ************************   tlifp, RFP4prime  ************************ *)
(* use the following param. for: 2 - a2 - a4 + Epsilon/2 < 0 *)

tlifp[tLI[{g1_?integ, m_, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol}, 0, 
            a4_?pinteg, a5_?pinteg}
         ] 
     ] := (
comment["Using RFP4"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a4 - a5)*so[r]^(g1 + g3 + g4 + g5 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a4 + a5 + g5)) .
(Gamma[-4 + a1 + a2 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a4]*Gamma[a5])) .
(x^(a4 + g1 + g5)*
  (1 - x)^(7 - a1 - a2 - 2*a4 - 2*a5 + 2*Epsilon + g3 + g4 + g5)) .
(u^(1 - a5 + Epsilon/2 + g5)*(1 - u)^(-1 + a5 + g1)*
  (1 - u*x)^(-4 + a1 + a5 - Epsilon - g1 - g3 - g5)) .
(t^(-3 + a1 + a5 - Epsilon/2)*(1 - t)^(-1 + a2)*
  (1 - t*u)^(-2 + a4 - Epsilon/2))
) /. {Dot :> Times, EulerGamma :> 0}
          ) /;  2 - a2 - a4 <= 0;
(* ************************   tlifp, RFP4 prime  ********************  *)

(* ************************   tlifp, RFP5     ************************ *)

tlifp[ tLI[{m_ /;MCH[m], g2_?integ, g3_?integ, g4_?integ, 0},
           {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol}, 
             a3_?pinteg, a4_?integ, 0} (* a3 and a4 can be 0 *)
          ]
     ] := (
comment["Using RFP5"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a3 - a4) so[r]^(m + g2 + g3 + g4)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a3 + a4)) .
((Gamma[-2 + a1 + a3 - Epsilon/2] Gamma[-2 + a2 + a4 - Epsilon/2]*
  Gamma[4 - a2 - 2 a4 + Epsilon + g4] Pochhammer[a4, g2])/
 (E^(Epsilon*EulerGamma) Gamma[a1] Gamma[a2] Gamma[a3] *
  Gamma[4 - a2 - a4 + Epsilon + g2 + g4])) .
(x^a3 (1 - x)^(3 - a1 - 2 a3 + Epsilon + g3))
) /. {Dot :> Times, EulerGamma :> 0}
          );

(* ************************   tlifp, RFP5     ************************ *)

(* ************************   tlifp, RFP5b     ************************ *)

tlifp[ tLI[{g1_?integ, g2_?integ, g3_?integ, g4_?integ, 0},
           {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol},
             a3_?integ, a4_?integ, 0}       (* a3 and a4 can be 0 *)
          ]
     ] := (
comment["Using RFP5b"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a3 - a4) so[r]^(g1 + g2 + g3 + g4)) .
(x^(-1 + OPEm) * DeltaFunction[1 - x]) . ((-1)^(1 + a1 + a2 + a3 + a4)) .
(
 (Gamma[-2 + a1 + a3 - Epsilon/2]*Gamma[4 - a1 - 2 a3 + Epsilon + g3]*
  Pochhammer[a3, g1])/(Gamma[4 - a1 - a3 + Epsilon + g1 + g3]*Gamma[a1])*
 (Gamma[-2 + a2 + a4 - Epsilon/2]*Gamma[4 - a2 - 2 a4 + Epsilon + g4]*
  Pochhammer[a4, g2])/(Gamma[4 - a2 - a4 + Epsilon + g2 + g4]*Gamma[a2])/
  E^(Epsilon*EulerGamma)
)
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP5b     ************************ *)



(* ************************   tlifp, RFP6     ************************ *)
tlifp[ tLI[{m_, g2_?integ, g3_?integ, g4_?integ, -1},
           {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol}, 
	     a3_?pinteg, a4_?pinteg, 0}
          ]
     ] := (
comment["Using RFP6"];
(
  (Smu^2) . ((M^2)^(4 - a1 - a2 - a3 - a4) so[r]^(m + g2 + g3 + g4 - 1)) .
  (x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a3 + a4)) .
  ((Gamma[-2 + a1 + a3 - Epsilon/2] Gamma[-2 + a2 + a4 - Epsilon/2])/
    (E^(Epsilon EulerGamma) Gamma[a1] Gamma[a2] Gamma[a3] Gamma[a4])) .
  (x^a3 (1 - x)^(3 - a1 - 2 a3 + Epsilon + g3)).
(*
via Hamberg (3C.31) the expression 
  (u^(-1 + a4 + g2) (1 - u)^(3 - a2 - 2 a4 + Epsilon + g4) (x - u)^(-1))
is analytically continued and the imaginary part dropped
*)
  (
   - Pi Cot[Pi Epsilon] x^(-1 + a4 + g2) *
      (1 - x)^(3 - a2 - 2 a4 + Epsilon + g4) +
   (-1)^(a4 + g2) u^(-3 + a2 + a4 - Epsilon - g2 - g4) * 
      (1 - u)^(-1 + a4 + g2) * (1 - (1 - x) u)^(-1)
  )
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP6     *********************** *)

(* ************************   tlifp, RFP7     ************************ *)
(* Caution:  Handle the x -> 1 behaviour with care ******************* *)

tlifp[tLI[{m_ , g2_?integ, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?pinteg, M_Symbol}, 0, a3_?pinteg,
           {a4_?pinteg, M_Symbol}, {a5_?pinteg, M_Symbol}}
         ]
     ] := (
comment["Using RFP7"];
(
(Smu^2) . ((M^2)^(4 - a1 - a3 - a4 - a5)*so[r]^(g2 + g3 + g4 + g5 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a3 + a4 + a5 + g5)) .
(
  Gamma[-4 + a1 + a3 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a3]*Gamma[a4]*Gamma[a5])
) .
(
   x^(-2 + a3 + a4 + a5 - Epsilon/2)*(1 - x)^(-1 + a1 + g3 + g4 + g5)
) .
(
   u^(-3 + a1 + a3 + a5 - Epsilon/2 + g4)*
   (1 - u)^(-3 + a1 + a3 + a4 - Epsilon/2 + g5)*
   (1 - u*(1 - x))^g2
) .
(
   t^(-3 + a4 + a5 - Epsilon/2)*(1 - t)^(-1 + a3)*
   ((1 - u)*u*(1 - x)^2 + t*x)^(4 - a1 - a3 - a4 - a5 + Epsilon)
)
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP7  ************************ *)


(* ************************   tlifp, RFP8     ************************ *)
(* Caution:  Handle the x -> 1 behaviour with care ******************* *)


tlifp[tLI[{m_, g2_?integ, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?integ, M_Symbol}, a2_?integ, 0, 
           {a4_?integ, M_Symbol}, {a5_?integ, M_Symbol}}
         ]
     ] := (
comment["Using RFP8"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a4 - a5)*so[r]^(g2 + g3 + g4 + g5 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a4 + a5 + g5)) .
(
  Gamma[-4 + a1 + a2 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a4]*Gamma[a5])
) .
(
  x^(-2 + a2 + a4 + a5 - Epsilon/2)*(1 - x)^(-1 + a1 + a2 + g3 + g4 + g5)
) .
(
  u^(-3 + a1 + a2 + a5 - Epsilon/2 + g4)*
  (1 - u)^(-3 + a1 + a2 + a4 - Epsilon/2 + g5)*
  (1 - (1 - x)*u)^(4 - a1 - 2*a2 - a4 - a5 + Epsilon + g2)
) .
(
  t^(-3 + a1 + a5 - Epsilon/2)*(1 - t)^(-1 + a2)*
  (t*u + (1 - u)*x)^(4 - a1 - a2 - a4 - a5 + Epsilon)
)
) /. {Dot :> Times, EulerGamma :> 0}
          ) /; a2 =!= 0;
(* ************************   tlifp, RFP8  ************************ *)

(* ************************   tlifp, RFP8b     ************************ *)

tlifp[tLI[{m_, g2_?integ, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?pinteg, M_Symbol}, 0, 0, 
           {a4_?pinteg, M_Symbol}, {a5_?pinteg, M_Symbol}}
         ]
     ] := (
comment["Using RFP8b"];
(
(Smu^2) . ((M^2)^(4 - a1 - a4 - a5)*so[r]^(g2 + g3 + g4 + g5 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a4 + a5 + g5)) .
(
  Gamma[-4 + a1 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a4]*Gamma[a5])
) .
(
  x^(2 - a1 + Epsilon/2)*(1 - x)^(-1 + a1 + g3 + g4 + g5)
) . 
(
  u^(-3 + a1 + a5 - Epsilon/2 + g4)*(1 - u)^(-3 + a1 + a4 - Epsilon/2 + g5)*
  (1 - u*(1 - x))^(4 - a1 - a4 - a5 + Epsilon + g2)*
  (1 - u*(1 - 1/x))^(4 - a1 - a4 - a5 + Epsilon)
)
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP8b  ************************ *)
(* ************************   tlifp, RFP8c     ************************ *)
(* Caution:  Handle the x -> 1 behaviour with care ******************* *)

tlifp[tLI[{g1_?integ, g2_?integ, g3_?integ, g4_?integ, m_},
           {{a1_?pinteg, M_Symbol}, 0, a3_?pinteg, 
            {a4_?pinteg, M_Symbol}, {a5_?pinteg, M_Symbol}
           }
         ]
     ] := (
comment["Using RFP8c"];
(
(Smu^2) . ((M^2)^(4 - a1 - a3 - a4 - a5)*so[r]^(g1 + g2 + g3 + g4 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a3 + a4 + a5) * (-1)^m) .
(
  Gamma[-4 + a1 + a3 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a3]*Gamma[a4]*Gamma[a5])
) .
(
  x^(-2 + a1 + a3 + a4 - Epsilon/2)*
  (1 - x)^(-1 + a3 + a5 + g1 + g4)
) .
(
  u^(-3 + a3 + a4 + a5 - Epsilon/2 + g1)*
  (1 - u)^(-3 + a1 + a3 + a5 - Epsilon/2 + g4)* 
  (u + x - u*x)^g2*
  (1 - u + u*x)^(4 - a1 - 2*a3 - a4 - a5 + Epsilon + g3)
).
(
  t^(-3 + a4 + a5 - Epsilon/2)*
  (1 - t)^(-1 + a3)*
  (t*u + x - u*x)^(4 - a1 - a3 - a4 - a5 + Epsilon)
)
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP8c  ************************ *)


(* ************************   tlifp, RFP9     ************************ *)

tlifp[tLI[{m_, g2_?integ, g3_?integ, g4_?integ, g5_?integ},
          {0, a2_?pinteg, a3_?pinteg, 
          {a4_?pinteg, M_Symbol}, {a5_?pinteg, M_Symbol}}
         ]
     ] := (
comment["Using RFP9"];
(
(Smu^2) . ((M^2)^(4 - a2 - a3 - a4 - a5)*so[r]^(g2 + g3 + g4 + g5 + m)) .
(x^(-1 + m)) . ((-1)^(1 + a2 + a3 + a4 + a5)) .
(
  Gamma[-4 + a2 + a3 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a2]*Gamma[a3]*Gamma[a4]*Gamma[a5])
) .
(
  x^(a3 + a4 + g2 + g5)*(1 - x)^(1 - a3 + Epsilon/2 + g3)
) .
(
  u^(1 - a2 + Epsilon/2 + g2)*(1 - u)^(-3 + a2 + a3 + a4 - Epsilon/2 + g5)*
  (1 - u*x)^(-4 + a2 + a3 + a5 - Epsilon + g4)
) .
(
  t^(-3 + a3 + a5 - Epsilon/2)*(1 - t)^(-1 + a4)*
  (t*(1 - x) + (1 - u)*u*x^2)^(4 - a2 - a3 - a4 - a5 + Epsilon)
)
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP9  ************************ *)


(* ************************   tlifp, RFP12     ************************ *)

tlifp[tLI[{m_, g2_?integ, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?pinteg, M_Symbol}, a2_?pinteg, a3_?pinteg, 
           {a4_?pinteg, M_Symbol}, {a5_?pinteg, M_Symbol}}
         ]
     ] := (
comment["Using RFP12"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a3 - a4 - a5)*
  so[r]^(m + g2 + g3 + g4 + g5)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a3 + a4 + a5)) .
(
  Gamma[-4 + a1 + a2 + a3 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*
                          Gamma[a3]*Gamma[a4]*Gamma[a5])
) .
(
(
  x^(2 - a2 + a3 + Epsilon/2 + g2 + g5)*y^(1 - a3 + Epsilon/2 + g3)
) .
(
  u^(1 - a2 + Epsilon/2 + g2)*(1 - u)^g5*(1 - u*x)^g4
) .
(
  t^(-3 + a2 + a4 + a5 - Epsilon/2)*(1 - t)^(-1 + a1)*(1 - t*u)^(-1 + a3)
) .
(
  s^(-3 + a1 + a3 + a5 - Epsilon/2)*
  (1 - s)^(-1 + a4)*(1 - u*x - s*y)^(-1 + a2)*
  (1 - t*u*x - s*y)^(-1 - Epsilon/2)*
  (t*u*x + s*y)^(4 - a1 - a2 - a3 - a4 - a5 + Epsilon)
) 
  + (-1)^g5*
(
  x^(-2 + a2 + a3 + a4 + a5 - Epsilon/2)*   
  y^(1 + a2 - a3 + Epsilon/2 + g3 + g4 + g5)
) .
(
  u^(-3 + a1 + a2 + a3 + a5 - Epsilon/2 + g4)*(1 - u)^g5*
  (1 - u*y)^(4 - a1 - 2*a2 - a4 - a5 + Epsilon + g2)
) .
(
  t^(-3 + a1 + a3 + a5 - Epsilon/2)*(1 - t)^(-1 + a2)*(1 - t*u)^(-1 + a4)
) .
(
  s^(-3 + a2 + a4 + a5 - Epsilon/2)*(1 - s)^(-1 + a3)* 
  (1 - s*x - u*y)^(-1 + a1)*(1 - s*x - t*u*y)^(-1 - Epsilon/2)*
  (s*x + t*u*y)^(4 - a1 - a2 - a3 - a4 - a5 + Epsilon)
)
)
) /. {Dot :> Times, EulerGamma :> 0, y :> 1 - x}
          );
(* ************************   tlifp, RFP12  ************************ *)
(* ************************   tlifp, RFP13     ************************ *)

tlifp[tLI[{g1_?integ, g2_?integ, g3_?integ, g4_?integ, m_},
          {{a1_?pinteg, M_Symbol}, a2_?pinteg, a3_?pinteg, 
           {a4_?pinteg, M_Symbol}, {a5_?pinteg, M_Symbol}}
         ]
     ] := (
comment["Using RFP13"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a3 - a4 - a5)*so[r]^(m + g1 + g2 + g3 + g4)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a3 + a4 + a5)) .
(
  Gamma[-4 + a1 + a2 + a3 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a3]*Gamma[a4]*Gamma[a5])
) .
(
(
  x*y^(7 - a1 - 2*a2 - 2*a3 - a4 - a5 + 2*Epsilon + g2 + g3)
) .
(
  u^(1 - a3 + Epsilon/2 + g3)*(1 - u)^(1 - a2 + Epsilon/2 + g2)*
  ((1 - u)*y + x)^g1*(u*y + x)^g4
) .
(
  (1 - t)^(-1 + a1)*t^(-3 + a2 + a4 + a5 - Epsilon/2)*
  ((1 - t)*(1 - u)*y + x)^(-1 + a3)
) .
(
  (1 - s)^(-1 + a4)*s^(-3 + a1 + a3 + a5 - Epsilon/2)*
  ((1 - s)*u*y + x)^(-1 + a2)*
  (t*(1 - u) + s*u)^(4 - a1 - a2 - a3 - a4 - a5 + Epsilon)*
  (1 - (t*(1 - u) + s*u)*y)^(-1 - Epsilon/2)
)
  + (-1)^m*
(
  x*y^(-1 + a2 + a3 + a5 + g1 + g4)
) .
(
  u^(-3 + a2 + a3 + a4 + a5 - Epsilon/2 + g1)*
  (1 - u)^(-3 + a1 + a2 + a3 + a5 - Epsilon/2 + g4)*
  (1 - u*y)^(4 - a1 - 2*a3 - a4 - a5 + Epsilon + g3)*
  (x + u*y)^(4 - a1 - 2*a2 - a4 - a5 + Epsilon + g2)
) .
(
  t^(-3 + a1 + a3 + a5 - Epsilon/2)*(1 - t)^(-1 + a2)*
  (x + (1 - t)*(1 - u)*y)^(-1 + a4)
) .
(
  s^(-3 + a2 + a4 + a5 - Epsilon/2)*(1 - s)^(-1 + a3)*
  (x + (1 - s)*u*y)^(-1 + a1)*
  (t*(1 - u) + s*u)^(4 - a1 - a2 - a3 - a4 - a5 + Epsilon)*
  (1 - (t*(1 - u) + s*u)*y)^(-1 - Epsilon/2)
)
)
) /. {Dot :> Times, EulerGamma :> 0, y :> 1 - x}
          );
(* ************************   tlifp, RFP13  ************************ *)
(* ************************   tlifp, RFP14     ************************ *)

tlifp[tLI[{g1_?integ, g2_?integ, g3_?integ, g4_?integ, m_},
          {{a1_?pinteg, M_Symbol}, a2_?pinteg, a3_?pinteg,
           {a4_?pinteg, M_Symbol}, 0}
         ]
     ] := (
comment["Using RFP14"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a3 - a4)*so[r]^(m + g1 + g2 + g3 + g4)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a3 + a4)) .
(
  Gamma[-2 + a1 + a3 - Epsilon/2]*Gamma[-2 + a2 + a4 - Epsilon/2]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a3]*Gamma[a4])
) .
(
(
  x^(a2 + g4)*(1 - x)^(7 - a1 - 2*a2 - 2*a3 - a4 + 2*Epsilon + g2 + g3)
) .
(
  u^(3 - a1 - 2*a3 + Epsilon + g3)*(1 - u)^(3 - 2*a2 - a4 + Epsilon + g2)*
  (1 - u*(1 - x))^(-1 + a3 + g1)*
  (1 - u*(1 - 1/x))^(-1 + a2 + g4)
)
  + (-1)^m*
(
  x^(4 - 2*a2 - a4 + Epsilon + g2)*
  (1 - x)^(-1 + a2 + a3 + g1 + g4)
) .
(
  u^(-1 + a3 + g1)*(1 - u)^(-1 + a2 + g4)*
  (1 - u*(1 - x))^(3 - a1 - 2*a3 + Epsilon + g3)*
  (1 - u*(1 - 1/x))^(3 - 2*a2 - a4 + Epsilon + g2)
)
)
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP14  ************************ *)
(* ************************   tlifp, RFP15     ************************ *)

tlifp[tLI[{g1_?integ, g2_?integ, g3_?integ, g4_?integ, m_},
          {0, a2_?pinteg, a3_?pinteg, 0, {a5_?pinteg, M_Symbol}}
         ]
     ] := (
comment["Using RFP15"];
(Smu^2) . ((M^2)^(4 - a2 - a3 - a5)*so[r]^(m + g1 + g2 + g3 + g4)) .
(x^(-1 + m)) . ((-1)^(1 + a2 + a3 + a5) .
(
  Gamma[-4 + a2 + a3 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a2]*Gamma[a3]*Gamma[a5])
) .
(
  x^(-2 + a2 + a3 - Epsilon/2)*
  (1 - x)^(7 - 2*a2 - 2*a3 - a5 + 2*Epsilon + g2 + g3)
) .
(
  u^(1 - a2 + Epsilon/2 + g2)*(1 - u)^(1 - a3 + Epsilon/2 + g3)*
  (u + x - u*x)^g1*(1 - u + u*x)^g4
)
) /. {Dot :> Times, EulerGamma :> 0}
          );

(* For g1 = g4 = 0 integrable in closed Form *********************** *)
(* ************************   tlifp, RFP15  ************************ *)


(* ************************   tlifp, RFP17     ************************ *)
tlifp[tLI[{g1_. + OPEm - OPEi, 0, g3_?integ, 
           g4_?integ, g5_. + OPEi},
          {{a1_?pinteg, M_Symbol}, a2_?pinteg, a3_?pinteg,
           {a4_?pinteg, M_Symbol}, {a5_?pinteg, M_Symbol}}
         ]
     ] := (
comment["Using RFP17"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a3 - a4 - a5)*
           so[r]^(OPEm + g1 + g3 + g4 + g5)) .
((-1)^(1 + a1 + a2 + a3 + a4 + a5)) .
(
  Gamma[-4 + a1 + a2 + a3 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a3]*Gamma[a4]*Gamma[a5])
) .
(
(
  (1 - x^(-1 + OPEm))/(1 - x)
) .
(
  y^(-1 + OPEm)
) .
(
  x^g5*(1 - x)^(1 - a2 + Epsilon/2)
) .
(
  y^(2 - a2 + a3 + Epsilon/2 + g1 + g5)*(1 - y)^(1 - a3 + Epsilon/2 + g3)*
  (1 - (1 - x)*y)^g4
) .
(
  t^(-3 + a2 + a4 + a5 - Epsilon/2)*(1 - t)^(-1 + a1)*
  (1 - t*(1 - x))^(-1 + a3)
) .
(
  s^(-3 + a1 + a3 + a5 - Epsilon/2)*(1 - s)^(-1 + a4)*
  (1 - s*(1 - y) - (1 - x)*y)^(-1 + a2)*
  (1 - s*(1 - y) - t*(1 - x)*y)^(-1 - Epsilon/2)*
  (s*(1 - y) + t*(1 - x)*y)^(4 - a1 - a2 - a3 - a4 - a5 + Epsilon)
)
  + 
(
  x^(-1 + OPEm)
) .
(
  (-1)^g5*
(* 
managable contribution from
TLI[{1 + g1 + OPEm, -1, g3, g4, g5}, {{a1, M}, a2, a3, {a4, M}, {a5, M}}] 
*)
(
  x^(-1 + a2 + a3 + a4 + a5 - Epsilon/2 + g1)*   
  (1 - x)^(1 + a2 - a3 + Epsilon/2 + g3 + g4 + g5)
) .
(
  u^(-3 + a1 + a2 + a3 + a5 - Epsilon/2 + g4)*(1 - u)^g5*
  (1 - u*(1 - x))^(3 - a1 - 2*a2 - a4 - a5 + Epsilon)
) .
(
  t^(-3 + a1 + a3 + a5 - Epsilon/2)*(1 - t)^(-1 + a2)*(1 - t*u)^(-1 + a4)
) .
(
  s^(-3 + a2 + a4 + a5 - Epsilon/2)*(1 - s)^(-1 + a3)* 
  (1 - s*x - u*(1 - x))^(-1 + a1)*(1 - s*x - t*u*(1 - x))^(-1 - Epsilon/2)*
  (s*x + t*u*(1 - x))^(4 - a1 - a2 - a3 - a4 - a5 + Epsilon)
)
  + (-1)^OPEm*
(* 
managable contribution from  
TLI[{2 + g1, -1, g3, g4, OPEm + g5 - 1}, {{a1, M}, a2, a3, {a4, M}, {a5, M}}] 
*)
(
  x^g5*(1 - x)^(1 + a2 + a3 + a5 + g1 + g4)
) .
(
  u^(-1 + a2 + a3 + a4 + a5 - Epsilon/2 + g1)*
  (1 - u)^(-3 + a1 + a2 + a3 + a5 - Epsilon/2 + g4)*
  (1 - u*(1 - x))^(4 - a1 - 2*a3 - a4 - a5 + Epsilon + g3)*
  (x + u*(1 - x))^(3 - a1 - 2*a2 - a4 - a5 + Epsilon)
) .
(
  t^(-3 + a1 + a3 + a5 - Epsilon/2)*(1 - t)^(-1 + a2)*
  (x + (1 - t)*(1 - u)*(1 - x))^(-1 + a4)
) .
(
  s^(-3 + a2 + a4 + a5 - Epsilon/2)*(1 - s)^(-1 + a3)*
  (x + (1 - s)*u*(1 - x))^(-1 + a1)*
  (t*(1 - u) + s*u)^(4 - a1 - a2 - a3 - a4 - a5 + Epsilon)*
  (1 - (t*(1 - u) + s*u)*(1 - x))^(-1 - Epsilon/2)
)
)
)
) /. {Dot :> Times, EulerGamma :> 0}
          ) /; integ[g5] && integ[g1];
(* ************************   tlifp, RFP17  ************************ *)
(* ************************   tlifp, RFP18     ************************ *)
tlifp[tLI[{g1_ /; MCH[g1], g2_ /; MCH[g2], g3_?integ, g4_?integ, g5_?integ},
          {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol},
           a3_?pinteg, 0, a5_?pinteg}
         ]
     ] := (
comment["Using RFP18"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a3 - a5)*so[r]^(g1 + g2 + g3 + g4 + g5)) .
((-1)^(1 + a1 + a2 + a3 + a5)) .
(
  (Gamma[-4 + a1 + a2 + a3 + a5 - Epsilon]*
   Gamma[-2 + a2 + a5 - Epsilon/2])/
  (E^(Epsilon*EulerGamma)*Gamma[a2]*Gamma[a3]*Gamma[a5]*
   Gamma[-2 + a1 + a2 + a5 - Epsilon/2])
) .
(
  x^(-1 + a3 + g1 + g2 + g5)*
  (1 - x)^(5 - a1 - a2 - 2*a3 - a5 + (3*Epsilon)/2 + g3)
) .
(
  y^(1 - a2 + Epsilon/2 + g2)*(1 - y)^(1 - a5 + Epsilon/2 + g5)*
  (1 - x*y)^g4
) .
(
  Hypergeometric2F1[-4 + a1 + a2 + a3 + a5 - Epsilon,
   -2 + a2 + a5 - Epsilon/2, -2 + a1 + a2 + a5 - Epsilon/2,
   -((1 - y)/((1 - x)*y))]
)
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP18  ************************ *)
(* ************************   tlifp, RFP19  ************************ *)
tlifp[tLI[{g1_ /; MCH[g1], g2_?integ, g3_?integ, g4_?integ, g5_ /;MCH[g5]},
           {0, a2_?pinteg, a3_?pinteg, 
            {a4_?pinteg, M_Symbol}, {a5_?pinteg, M_Symbol}
           }
         ]
     ] := (
comment["Using RFP19"];
(
(Smu^2) ((M^2)^(4 - a3 - a2 - a4 - a5)*
         so[r]^(g1 + g2 + + g3 + g4 + g5))*
     ((-1)^(1 + a3 + a2 + a4 + a5))*
      Gamma[-4 + a3 + a2 + a4 + a5 - Epsilon]/
    (E^(Epsilon*EulerGamma)*Gamma[a3]*Gamma[a2]*Gamma[a4]*Gamma[a5])*
 ((1 - x)^(7 - 2*a2 - 2*a3 - a4 - a5 + 2*Epsilon + g2 + g3)*
     x^(1 - a5 + Epsilon/2 + g5)*
     (1 - y)^(5 - 2*a2 - a3 - a4 - a5 + (3*Epsilon)/2 + g2)*
     y^(1 - a3 + Epsilon/2 + g3)*(1 - (1 - x)*y)^g1*
     (x + (1 - x)*y)^(-4 + a2 + a3 + a5 - Epsilon + g4)*Gamma[a4]*
     Gamma[-2 + a3 + a5 - Epsilon/2]*
     Hypergeometric2F1[-4 + a2 + a3 + a4 + a5 - Epsilon,
      -2 + a3 + a5 - Epsilon/2, -2 + a3 + a4 + a5 - Epsilon/2,
      -(y/(x*(1 - y)))])/Gamma[-2 + a3 + a4 + a5 - Epsilon/2]
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP19  ************************ *)

(* ************************   tlifp, RFP21     ************************ *)

tlifp[tLI[{m_ /; MCH[m], g2_?integ, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?integ, M_Symbol}, {a2_?integ, M_Symbol}, 
            a3_?integ, a4_?integ, a5_?integ}
         ]
     ] := (
comment["Using RFP21"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a3 - a4 - a5)*so[r]^(m + g2 + g3 + g4 + g5)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a3 + a4 + a5)) .
(
  Gamma[-4 + a1 + a2 + a3 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a3]*Gamma[a4]*Gamma[a5])
) .
(
(1 - s)^(-1 + a4)*s^(-3 + a1 + a3 + a5 - Epsilon/2)*
  (1 - t)^(-3 + a2 + a4 + a5 - Epsilon/2)*t^(-1 + a1)*
  (1 - u)^(-1 + a2 + a4 + g5)*u^(-3 + a1 + a3 + a4 + a5 - Epsilon/2 + g2)*
  (1 - u + t*u*(1 - x))^(-1 + a3)*(1 - u + s*t*u*(1 - x))^(-1 - Epsilon/2)*
  (1 - t*(1 - u) - u + s*t*u*(1 - x))^(4 - a1 - a2 - a3 - a4 - a5 + Epsilon)*
  (1 - x)^(1 - a3 + Epsilon/2 + g3)*x^(a3 + a4 + g2 + g5)*
  (1 - u*x)^(4 - a2 - a3 - 2*a4 - a5 + Epsilon + g4) + 
 (-1)^g5*(1 - s)^(-1 + a3)*s^(-3 + a2 + a4 + a5 - Epsilon/2)*
  (1 - t)^(-3 + a1 + a3 + a5 - Epsilon/2)*t^(-1 + a2)*
  (1 - u)^(-1 + a1 + a3 + g5)*u^(1 - a4 + Epsilon/2 + g4)*(1 - u*(1 - x))^g2*
  (1 - x)^(7 - a1 - a2 - 2*a3 - 2*a4 - 2*a5 + 2*Epsilon + g3 + g4 + g5)*
  x^(-2 + a2 + a3 + a4 + a5 - Epsilon/2)*(1 - u + t*u*x)^(-1 + a4)*
  (1 - u + s*t*u*x)^(-1 - Epsilon/2)*(1 - t*(1 - u) - u + s*t*u*x)^
   (4 - a1 - a2 - a3 - a4 - a5 + Epsilon)
)
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP21  ************************ *)

(* ************************   tlifp, RFP22     ************************ *)

tlifp[tLI[{m_ /; MCH[m], g2_?integ, g3_?integ, g4_?integ, g5_?integ},
          {0, {a2_?integ, M_Symbol}, 
            a3_?integ, a4_?integ, a5_?integ}
         ]
     ] := (
comment["Using RFP22"];
(
(Smu^2) . ((M^2)^(4 - a2 - a3 - a4 - a5)*so[r]^(m + g2 + g3 + g4 + g5)) .
(x^(-1 + m)) . ((-1)^(1 + a2 + a3 + a4 + a5)) .
(
  Gamma[-4 + a2 + a3 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a2]*Gamma[a3]*Gamma[a4]*Gamma[a5])
) .
(
 x^(a3 + a4 + g2 + g5)*
 (1 - x)^(7 - a2 - 2*a3 - 2*a4 - 2*a5 + 2*Epsilon + g3 + g4 + g5)*
 t^(1 - a5 + Epsilon/2 + g5)*(1 - t)^(-3 + a3 + a5 - Epsilon/2)*
 s^(-1 + a3 + g5)*(1 - s)^(-1 + a4)*
 (1 - s*t)^g2*
 (1 - s*t*x)^(-4 + a2 + a4 + a5 - Epsilon - g2 - g4 - g5)
)
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP22  ************************ *)


(* ************************   tlifp, RFP23     ************************ *)

tlifp[tLI[{m_ /; MCH[m], g2_?integ, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?integ, M_Symbol}, {a2_?integ, M_Symbol}, 
            a3_?integ, a4_?integ, 0}
         ]
     ] := (
comment["Using RFP23"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a3 - a4)*so[r]^(m + g2 + g3 + g4 + g5)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a2 + a3 + a4)) .
(
  Gamma[-4 + a1 + a2 + a3 + a4 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a3]*Gamma[a4])
) .
(
 (x^(a3 + g5)*(1 - x)^(3 - a1 - 2*a3 + Epsilon + g3)*
  Gamma[-2 + a1 + a3 - Epsilon/2]*Gamma[-2 + a2 + a4 - Epsilon/2]*
  Gamma[a4 + g2]*Gamma[4 - a2 - 2*a4 + Epsilon + g4]*
  Hypergeometric2F1[a4 + g2, -g5, 4 - a2 - a4 + Epsilon + g2 + g4, x^(-1)])/
 (Gamma[-4 + a1 + a2 + a3 + a4 - Epsilon]*
  Gamma[4 - a2 - a4 + Epsilon + g2 + g4])
)
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP23  ************************ *)

(* ************************   tlifp, RFP24     ************************ *)

tlifp[tLI[{m_ /; MCH[m], g2_?integ, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?integ, M_Symbol}, 0, a3_?integ, a4_?integ, a5_?integ}
         ]
     ] := (
comment["Using RFP24"];
(
(Smu^2) . ((M^2)^(4 - a1 - a3 - a4 - a5)*so[r]^(m + g2 + g3 + g4 + g5)) .
(x^(-1 + m)) . ((-1)^(1 + a1 + a3 + a4 + a5)) .
(
  Gamma[-4 + a1 + a3 + a4 + a5 - Epsilon]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a3]*Gamma[a4]*Gamma[a5])
) .
(
 (x^(-2 + a3 + a4 + a5 - Epsilon/2)*
  (1 - x)^(7 - a1 - 2*a3 - 2*a4 - 2*a5 + 2*Epsilon + g3 + g4 + g5)*
  Gamma[a3]*Gamma[-2 + a4 + a5 - Epsilon/2]*Gamma[2 - a4 + Epsilon/2 + g4]*
  Gamma[2 - a5 + Epsilon/2 + g5]*Hypergeometric2F1[-g2,
   2 - a4 + Epsilon/2 + g4, 4 - a4 - a5 + Epsilon + g4 + g5, 1 - x])/
 (Gamma[-2 + a3 + a4 + a5 - Epsilon/2]*Gamma[4 - a4 - a5 + Epsilon + g4 + g5])
)
) /. {Dot :> Times, EulerGamma :> 0}
          );
(* ************************   tlifp, RFP24  ************************ *)






(* GENERAL permutation RULES *)

(* PR1 *)
tlifp[ tLI[{m_, g2_?integ, g3_?integ, g4_?integ, 0},
           {{a1_?pinteg, M_Symbol}, a2_?pinteg,
             a3_?pinteg, {a4_?pinteg, M_Symbol}, 0}
          ]
     ] := (comment["using PR1"];
           tlifp[tLI[{m, g4, g3, g2, 0}, {{a1, M}, {a4, M}, a3, a2, 0}]]
          );

(* PR2 *)
(* 1 <--> 3, 2 <--> 4 ALLGEMEIN *)
tlifp[tLI[{g1_, g2_, g3_, g4_, g5_}, 
          {a1_?integ, {a2_?pinteg, M_Symbol}, {a3_?pinteg, M_Symbol},
           a4_?integ, {a5_?pinteg, M_Symbol}
          }
         ]
     ]  := (comment["using PR2"];
            (-1)^g5 tlifp[tLI[{g3, g4, g1, g2, g5},
                              {{a3, M}, a4, a1, {a2, M}, {a5, M}}
                             ]
                         ]
           );

(* PR3 *)
(* 1 <--> 4, 2 <--> 3 ALLGEMEIN *)
tlifp[tLI[{g1_, g2_, g3_, m_ /; MCH[m], g5_}, 
          {{a1_?pinteg, M_Symbol}, a2_?pinteg, a3_?pinteg,
           {a4_?pinteg, M_Symbol},
           {a5_?pinteg, M_Symbol}
          }
         ]
      ] := (comment["using PR3"];
            tlifp[tLI[{m, g3, g2, g1, g5},
                      {{a4, M}, a3, a2, {a1, M}, {a5,M}}]
                 ]
           );

(* PR4 *)
tlifp[tLI[{m_ /; MCH[m], g2_, g3_, g4_, g5_},
          {{a1_?integ, M_Symbol}, a2_?integ, a3_?integ, 0, {a5_, M_Symbol}}
         ]
     ] := (comment["using PR4"];
           tlifp[tLI[{m, g5, g3, g4, g2},
                     {{a1, M}, {a5, M}, a3, 0, a2}]
                ]
          );

(* PR5 *)
tlifp[tLI[{m_ /; MCH[m], g2_, g3_, g4_, g5_},
          {{a1_?integ, M_Symbol}, {a2_?integ, M_Symbol}, 
            a3_?integ, 0, a5_?integ}
         ] 
     ] := (comment["using PR5"];
          (-1)^g5 tlifp[ tLI[{g2, m, g4, g3, g5},
                             {{a2, M}, {a1, M}, 0, a3, a5}
                            ]
                       ]
          );

(* PR6 *)
tlifp[ tLI[{0, g2_?integ, g3_?integ, g4_?integ, m_ /; MCH[m]},
           {0, a2_?pinteg, a3_?pinteg,
            {a4_?pinteg, M_Symbol}, {a5_?pinteg, M_Symbol} 
           }
          ]
     ] := (comment["using PR6"];
          (-1)^g3 tlifp[tLI[{m, g4, 0, g2, g3}, 
                            {{a5, M}, {a4, M}, 0, a2, a3}
                           ]
                       ]
          );

(* PR7 *)
tlifp[ tLI[{m_, g2_?integ, g3_?integ, g4_?integ, 0},
           {{a1_?integ, M_Symbol}, 
             a2_?integ, a3_?integ,
             {a4_?integ, M_Symbol}, 0
           } 
          ]
     ] := (comment["using PR7"];
           tlifp[tLI[{m, g4, g3, g2, 0}, {{a1,M}, {a4,M}, a3, a2, 0}]]
          );

(* PR8 *)
tlifp[ tLI[{m_, g2_?integ, g3_?integ, g4_?integ, 0},
           {{a1_?integ, M_Symbol}, 
             a2_?integ, a3_?integ,
             {a4_?integ, M_Symbol}, 0
           } 
          ]
     ] := (comment["using PR8"];
           tlifp[tLI[{m, g4, g3, g2, 0}, {{a1,M}, {a4,M}, a3, a2, 0}]]
          );

(* PR9 *)
tlifp[ tLI[{g1_, g2_, g3_, g4_, g5_},
           {a1_?pinteg, a2_?pinteg, 
            {a3_?pinteg, M_Symbol}, {a4_?pinteg, M_Symbol}, 0
           }]
     ] := (comment["using PR9"];
        tlifp[tLI[{g4, g3, g2, g1, g5}, {{a4, M}, {a3, M}, a2, a1, 0}]]
          );

(* PR10 *)
tlifp[tLI[{g1_, 0, g3_, g4_, m_ /; MCH[m]},
          {{a1_?pinteg, M_Symbol}, 0, a3_?pinteg,
           {a4_?pinteg, M_Symbol}, {a5_?pinteg, M_Symbol}}
         ]
     ] := (comment["using PR10"];
            (-1)^(m) (-1)^g4 *
           tLI[{m, g3, 0, g1, g4}, {{a5, M}, a3, 0, {a1, M}, {a4, M}}]
          );

(* PR11 *)
tlifp[tLI[{g1_, g2_, 0, g4_, g5_},
          {{a1_?pinteg, M_Symbol}, 0, 0, a4_?pinteg, a5_?pinteg}
         ]
     ] := (comment["using PR11"];
           (-1)^g5 tlifp[tLI[{g2, g5, g4, 0, g1}, 
                             {0, a5, a4, 0, {a1, M}}]
                        ]
          );

(* PR12 *)
tlifp[tLI[{g1_, g2_, 0_, g4_, g5_},
          {a1_?pinteg, 0, 0, a4_?pinteg, {a5_?pinteg, M_Symbol}}
         ]
     ] :=  (comment["using PR12"];
   tlifp[(-1)^g5 * tLI[{g3, g4, g1, g2, g5}, {0, a4, a1, 0, {a5, M}}]]
           );

(* PR13 *)
tlifp[tLI[{g1_, m_ /; MCH[m], g3_, g4_, 0},
           {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol}, 
             a3_?integ, a4_?pinteg, 0 
           }
         ]
     ] :=  (comment["using PR13"];
   tlifp[ tLI[{m, g1, g4, g3, 0}, { {a2,M}, {a1,M}, a4, a3, 0}]]
           );

(* PR14 *)
tlifp[tLI[{g1_,  m_ /; MCH[m], g3_, g4_, g5_},
           {a1_?pinteg, 0, {a3_?pinteg, M_Symbol}, 
             a4_?integ,    {a5_?pinteg, M_Symbol}
           }
         ]
     ] := (comment["using PR14"];
   (-1)^g5 tlifp[ tLI[{m, g1, g4, g3, g5}, 
                      {0, a1, a4, {a3, M}, {a5, M}}
                     ]
                ]
           );

(* PR15 *)
tlifp[tLI[{g1_,  g2_, g3_, g4_, m_ /; MCH[m]},
           {{a1_?pinteg, M_Symbol}, a2_?pinteg, 0, 
            {a4_?pinteg, M_Symbol}, {a5_?pinteg, M_Symbol}
           }
         ]
     ] := (comment["using PR15"];
          tlifp[ tLI[{g4, g3, g2, g1, m},
                     {{a4, M}, 0, a2, {a1, M}, {a5, M}}
                     ]
                ]
           );

(* PR16 *) (* correction  04/15 *)
tlifp[tLI[{g1_?NonNegative, m_, 0, g4_?integ, g5_?integ}, 
           {0, {a2_?pinteg, M_Symbol}, 0, a4_?integ,
            {a5_?pinteg, M_Symbol}
           }
         ]
     ] := (comment["using PR16"];
           (-1)^g5 Sum[Binomial[g1, i] (-1)^(g1-i) *
                       tlifp[ tLI[{m+i, g5+g1-i, g4, 0, 0},
                                  {{a2, M}, {a5, M}, a4, 0, 0}
                                 ]
                            ],
                       {i, 0, g1}
                      ]
          );

(* PR17 *)
tlifp[tLI[{0, m_, 1, g4_?integ, g5_?integ}, 
           {0, {a2_?pinteg, M_Symbol}, 0, a4_?integ,
            {a5_?pinteg, M_Symbol}
           }
         ]
     ] := (comment["using PR17"];
           tlifp[ tLI[{0, m, 0, g4 + 1, g5},
                      {0, {a2, M}, 0, a4, {a5, M}}
                     ] -
                  tLI[{0, m, 0, g4  ,  g5 + 1},
                      {0, {a2, M}, 0, a4, {a5, M}}
                     ]
                ]
          );


(* ********************************************************************* *)
(* FORMER TLIHYP  begin *)

rule2F1 = {HypergeometricPFQ[{alpha_, beta_}, {gamma_}, z_] :>
           Hypergeometric2F1[alpha, beta, gamma, z]};


(* ************************   tlihyp, RHYP5     ************************ *)

tlihyp[tLI[{m_, 2, g3_?integ, g4_?integ, g5_?integ},
          {{a1_?integ, M_Symbol}, 0, a3_?integ,
           {a4_?integ, M_Symbol}, {a5_?integ, M_Symbol}}
         ]
     ] := so[r] *
      tlihyp[tLI[{m, 1, g3, g4, g5}, {{a1, M}, 0, a3, {a4, M}, {a5,M}}]
            ]  -
      tlihyp[tLI[{m, 1, g3, g4+1,g5}, {{a1, M}, 0, a3, {a4, M}, {a5,M}}]
            ];

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


(*
(* ************************   TLIHYP, RHYP1 ************************ *)
tlihyp[ tLI[{m_, 0, g3_?integ, g4_?integ, g5_?integ},
           {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol}, 
             0, a4_?pinteg, a5_?pinteg}
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
*)

(* ************************   tlihyp, RHYP3     ************************ *)
tlihyp[tLI[{g1_?integ, m_, 1, g4_?integ, g5_?integ},
          {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol}, 0, 
              a4_?pinteg, a5_?pinteg}
         ]
      ] := so[r] tlihyp[tLI[{g1, m, 0, g4, g5}, {{a1, M}, {a2, M}, 0, a4, a5}]
                       ] - 
               tlihyp[tLI[{g1+1, m, 0, g4, g5}, {{a1, M}, {a2, M}, 0, a4, a5}]
                       ];

tlihyp[tLI[{g1_?integ, m_ , 0, g4_?integ, g5_?integ},
          {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol}, 0, 
              a4_?pinteg, a5_?pinteg}
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
(* ************************   tlihyp, RHYP3ende  ************************ *)


(* ************************   tlihyp, RHYP6     ************************ *)
tlihyp[tLI[{g1_?integ, m_ , 0, g4_?integ, g5_?integ},
           {{a1_?pinteg, M_Symbol}, a2_?pinteg, 0, a4_?pinteg, a5_?pinteg}
          ]
     ] := (
comment["Using RHYP6"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a4 - a5)*so[r]^(g1 + g4 + g5 + m)) .
(x^(-1 + m)) . (-1)^(1+g5) .
(
  Gamma[2+Epsilon/2-a5+g5]/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a4]*Gamma[a5])
) .
(			
     x^(4-a1-a2-a5+g1+g5)*
   (1 - x)^(1+Epsilon/2-a4+g4)
) . 
(
  (x^(-2 + a1 + a5 + Epsilon/2)*Gamma[-2 + a2 + a4 - Epsilon/2]*
     Gamma[-2 + a1 + a5 - Epsilon/2]*Gamma[a5 + g1]*
     HypergeometricPFQ[{1 - a2, -2 + a1 + a5 - Epsilon/2, a5 + g1}, 
      {3 - a2 - a4 + Epsilon/2, 2 + Epsilon/2 + g1 + g5}, x])/
   (E^(I*(-2 + a1 + a5 + Epsilon/2)*Pi)*Gamma[a2]*
     Gamma[2 + Epsilon/2 + g1 + g5]) + 
  (x^(-4 + a1 + a2 + a4 + a5)*Gamma[-4 + a1 + a2 + a4 + a5 - Epsilon]*
     Gamma[2 - a2 - a4 + Epsilon/2]*
     Gamma[-2 + a2 + a4 + a5 - Epsilon/2 + g1]*
     HypergeometricPFQ[{-4 + a1 + a2 + a4 + a5 - Epsilon, 
       -1 + a4 - Epsilon/2, -2 + a2 + a4 + a5 - Epsilon/2 + g1}, 
      {-1 + a2 + a4 - Epsilon/2, a2 + a4 + g1 + g5}, x])/
   (E^(I*(-4 + a1 + a2 + a4 + a5)*Pi)*Gamma[2 - a4 + Epsilon/2]*
     Gamma[a2 + a4 + g1 + g5])
)
)/.{Dot :> Times, EulerGamma :> 0}
          )/.rule2F1;
(* ************************   tlihyp, RHYP6ende  ************************ *)


(* ************************   tlihyp, RHYP7     ************************ *)
tlihyp[tLI[{0, m_ , 0, g4_?integ, 0},
           {{a1_?pinteg, M_Symbol}, a2_?pinteg, 0, a4_?pinteg, 0}
          ]
     ] := (
comment["Using RHYP7"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a4)*so[r]^(g4 + m)) .
(x^(-1 + m)) . (-1)^(1+a1) .
(
  (Gamma[a1-2-Epsilon/2]*Gamma[a2+a4-2-Epsilon/2])/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a4])
) .
(			
   x^(2+Epsilon/2-a2)*
   (1 - x)^(1+Epsilon/2-a4+g4)
) .
(
  Exp[-I Pi Epsilon/2]
)
)/.{Dot :> Times, EulerGamma :> 0}
          )/.rule2F1;
(* ************************   tlihyp, RHYP7ende  ************************ *)


(* ************************   tlihyp, RHYP8     ************************ *)
tlihyp[tLI[{0, m_ , 0, g4_?integ, 0},
           {{a1_?pinteg, M_Symbol}, {a2_?pinteg, M_Symbol}, 0, a4_?pinteg, 0}
          ]
     ] := (
comment["Using RHYP8"];
(
(Smu^2) . ((M^2)^(4 - a1 - a2 - a4)*so[r]^(g4 + m)) .
(x^(-1 + m)) . (-1)^(1+a1+a2+a4) .
(
  (Gamma[a1-2-Epsilon/2]*Gamma[a2+a4-2-Epsilon/2])/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a2]*Gamma[a4])
) .
(			
   x^a4*
   (1 - x)^(4+Epsilon-a2-2*a4+g4-1)
) 
)/.{Dot :> Times, EulerGamma :> 0}
          )/.rule2F1;
(* ************************   tlihyp, RHYP8ende  ************************ *)


(* ************************   tlihyp, RHYP9     ************************ *)

tlihyp[tLI[{m_, 2, g3_, g4_, g5_},
           {{a1_?pinteg, M_Symbol}, 0, a3_?integ, a4_?pinteg, a5_?pinteg}
         ]
     ] := so[r] *
      tlihyp[tLI[{m, 1, g3, g4, g5}, {{a1, M}, 0, a3, a4, a5}]
            ]  -
      tlihyp[tLI[{m, 1, g3, g4+1,g5}, {{a1, M}, 0, a3, a4, a5}]
            ];

tlihyp[tLI[{m_, 1, g3_, g4_, g5_},
           {{a1_?pinteg, M_Symbol}, 0, a3_?integ, a4_?pinteg, a5_?pinteg}
         ]
     ] := so[r] *
      tlihyp[tLI[{m, 0, g3, g4, g5}, {{a1, M}, 0, a3, a4, a5}]
            ]  -
      tlihyp[tLI[{m, 0, g3, g4+1,g5}, {{a1, M}, 0, a3, a4, a5}]
            ];

tlihyp[tLI[{m_, 0, g3_, g4_, g5_},
           {{a1_?pinteg, M_Symbol}, 0, a3_?integ, a4_?pinteg, a5_?pinteg}
          ]
     ] := (
comment["Using RHYP9"];
(
(Smu^2) . ((M^2)^(4-a1-a3-a4-a5)*so[r]^(g3+g4+g5+m)) .
(x^(-1 + m)) . (-1)^(1+a1+a3+a4+a5) .
(
  (Gamma[a4+a5-2-Epsilon/2]*Gamma[a1+a3+a4+a5-4-Epsilon]*
   Gamma[2+Epsilon/2-a4+g4]*Gamma[2+Epsilon/2-a5+g5])/
  (E^(Epsilon*EulerGamma)*Gamma[a1]*Gamma[a4]*Gamma[a5]*
   Gamma[a3+a4+a5-2-Epsilon/2]*Gamma[4+Epsilon-a4-a5+g4+g5])
) .
(			
   x^(a3+a4+a5-2-Epsilon/2)*
   (1 - x)^(8+2*Epsilon-a1-2*a3-2*a4-2*a5+g3+g4+g5-1)
) 
)/.{Dot :> Times, EulerGamma :> 0}
          )/.rule2F1;
(* ************************   tlihyp, RHYP9ende  ************************ *)
(* FORMER TLIHYP  end*)


r1 = exp /. {TLI :> tli, TLI2 :> tli} ;
r2 = r1 /. tli[ww__] :> tlihyp[tLI[ww]];
r3 = r2 /. tlihyp -> tlifp /.  tlifp -> Identity /.  tLI -> TLI;

r1 = r3 /. TLI -> tli;
r2 = r1 /. tli[ww__] :> tlihyp[tLI[ww]];
r3 = r2 /. tlihyp -> tlifp /.  tlifp -> Identity /.  tLI -> TLI;

simp[ChangeDimension[r3,4]]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TLIFP | \n "]];
Null
