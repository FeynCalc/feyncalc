(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TrickIntegrate *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`TrickIntegrate`",
             "HighEnergyPhysics`FeynCalc`"];

TrickIntegrate::"usage"= "
TrickIntegrate[(1-t)^(a Epsilon -1) g[t], t] does an integration
trick for the definite integral of ((1-t)^(a Epsilon -1) g[t]) 
from 0 to 1.
TrickIntegrate[(1-t)^(a Epsilon -1) g[t], t] gives 
g[1]/a/Epsilon + Hold[Integrate][(1-t)^(a Epsilon-1) (g[t]-g[1]),{t,0,1}].
TrickIntegrate[t^(a Epsilon -1) f[t], t] gives
f[0]/a/Epsilon + Hold[Integrate][t^(a Epsilon-1) (f[t]-f[0]),{t,0,1}],
provided g[1] and f[0] do exist.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[Apart3, Collect2, Factor2, Epsilon, Map2, Select1, Select2];

Options[TrickIntegrate] = {Hold -> True};

hopt[vau_,op___Rule][z_] := If[
 !TrueQ[Hold /. {op} /. Options[TrickIntegrate]],
  Map2[Factor2,Collect2[z /. Hold[Integrate][ww_,_]:>ww, vau]]
                               , z
                               ];

TrickIntegrate[exp_Plus, v_,opt___] := TrickIntegrate[#, v, opt]& /@ exp;

(* if there is nothing to substract, then : *)
TrickIntegrate[w_/;Head[w]=!=Plus, v_, opt___] :=
 (hopt[v,opt][Select1[w, v](Hold[Integrate]@@ {Select2[w,v], {v,0,1}})]
 )/;
   (!MatchQ[w,(1-v)^(a_. Epsilon-1) ex_.] &&
    !MatchQ[w,    v^(a_. Epsilon-1) ex_.] 
   );

(* this is just linearity *)
TrickIntegrate[exp_. y^a_. (1-y)^b_ (1-x_ y_)^g_, y_, opt___Rule] :=
exp TrickIntegrate[y^a (1-y)^b (1-x y)^g, x, opt] /; FreeQ[exp, y];

beta[x_, y_] := Gamma[x] Gamma[y]/Gamma[x+y];

(*
TrickIntegrate[ y^a_. (1-y)^b_ (1-x_ y_)^g_, y_, opt___Rule] :=
 (beta[-b-g, b+g+1]/beta[-a-b-g-1,a+b+g+2] *
    Hold[Integrate][y^a (1-y)^(-a-b-g-2) (1- (1-x)y)^g,{y,0,1}] +
         x^(-a-b-1) (1-x)^(b+g+1) beta[b+1,-b-g-1] *
               Hypergeometric2F1[a,g+1, b+g+2, 1-x]
 ) /; MatchQ[b, -1 + _. Epsilon] && MatchQ[g,  -1 + _. Epsilon];
*)

(* important; partial fractioning !! *)
TrickIntegrate[(1 - v_)^(a_. Epsilon-1) v_^(b_. Epsilon-1) exp_., 
               v_, opt___Rule
              ]:= TrickIntegrate[Apart3[
   (1-v)^(a Epsilon-1) v^(b Epsilon-1) exp, v], v, opt
                                ];

TrickIntegrate[(1-v_)^(a_. Epsilon-1) exp_., v_,opt___Rule] := 
 TrickIntegrate[v^(a Epsilon-1) * Factor2[exp/.v->1-v],v,opt] /.
  Hold[Integrate][xy_, gr_]:>Hold[Integrate][xy/.v->(1-v), gr];

(* old
TrickIntegrate[v_^(a_. Epsilon-1) exp_., v_,opt___Rule] := 
 TrickIntegrate[(1-v)^(a Epsilon-1) * Factor2[exp/.v->1-v],v,opt];
*)

TrickIntegrate[v_^(a_. Epsilon-1) exp_., v_, opt___Rule
              ]:= Block[{tt, pre, rr, integ, integg, lim, re},
tt  = Select2[exp, v]/.Hypergeometric2F1[a1_,a2_,a3_,z_]:>
      Hypergeometric2F1[a1,a2,a3,Factor2[z]];
pre = Select1[exp, v];
lim = Limit[tt, v -> 0]//Factor2;
(* if limit does not exist or reveals a singularity in 
v = 0 then: return the input *)
If[(lim === Indeterminate) (*|| (!FreeQ[tt, Log[v]])*) ||
   (MatchQ[lim, _. (1-v)^(bb_. Epsilon-1)]),
   tt pre (v)^(a Epsilon-1)
  ,
(*special PowerExpand by hand ... *)
   lim = lim /. {((z_Symbol) (1-r_Symbol))^eps_ :> z^eps (1-r)^eps,
                  ((z_Symbol) r_Symbol)^eps_ :> z^eps r^eps
                };
   rr =  1/a/Epsilon lim  + 
           integ[v^(a Epsilon-1) (tt-lim)];
integg[0]=0;
integg[zz_] := Select1[zz, v] * 
               (Hold[Integrate]@@ {Select2[zz, v], {v,0,1}});
re= pre (rr /. integ -> integg) /. (Hold[Integrate][1,{v,0,1}]) -> 1;
Expand[ re = hopt[v,opt][re], v]
] /. Hold[Integrate][1,{_,0,1}] -> 1
                       ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TrickIntegrate | \n "]];
Null
