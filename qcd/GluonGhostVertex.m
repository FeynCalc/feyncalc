(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GluonGhostVertex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 6 May '98 at 0:37 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: GluonGhostVertex *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`GluonGhostVertex`",
             "HighEnergyPhysics`FeynCalc`"];

GGV::"usage" =
"GGV is equivalent to GluonGhostVertex.";

GluonGhostVertex::"usage" =
"
GluonGhostVertex[{mu,a}, {b}, {k,c}] or
GluonGhostVertex[{p,mu,a}, {q,b}, {k,c}] or
GluonGhostVertex[ p,mu,a ,  q,nu,b ,  k,rho,c ] 
gives the  Gluon-Ghost vertex. 
The first argument represents the gluon and the third
argument the outgoing ghost field (but incoming four-momentum).
The dimension  and the name of the coupling constant
are determined by the options Dimension and CouplingConstant.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
MakeContext[
CouplingConstant,
Dimension,
Explicit,
Gauge,
Gstrong,
LorentzIndex,
Momentum,
Pair,
QCDFeynmanRuleConvention,
SUNF,
SUNIndex   ];

Options[GluonGhostVertex] = {
CouplingConstant -> Gstrong,
Dimension -> D, 
Explicit -> False
};

{l, c} = MakeFeynCalcPrivateContext /@ {"l", "c"};


GluonGhostVertex[{mui_, ai_}, {bi_}, {ki_, ci_}, opt___?OptionQ] := 
   GluonGhostVertex[{x, y, ai}, {z,h,bi}, {ki,l,ci}, opt] /; 
	(Explicit /. {opts} /. Options[GluonGhostVertex]) === True;

GluonGhostVertex[x___, i_Integer, y___] := 
GluonGhostVertex[x, l[i], c[i], y];

GGV = GluonGhostVertex;

GluonGhostVertex[a_,b_,c_, d_,e_,f_, g_,h_,i_, opt___?OptionQ] :=
GluonGhostVertex[{a,b,c},{d,e,f},{g,h,i},opt] /;
   FreeQ[Map[Head,{a,b,c,d,e,f,g,h,i}], Integer|Rule|RuleDelayed|List, Heads->False];

GluonGhostVertex[{pi_, mui_, ai_}, {___, bi_},
                 {ki_, ___, ci_}, opt___?OptionQ] := 
   SUNF[SUNIndex[ai], SUNIndex[bi], SUNIndex[ci]] GluonGhostVertex[ki,mui,opt];

GluonGhostVertex[ki_, mui_, opt___?OptionQ] :=
Block[
 {dim, p, k, mu, re},
  coup  = CouplingConstant /. {opt} /. Options[GluonGhostVertex];
  dim   = Dimension /. {opt} /. Options[GluonGhostVertex];
      {p,k}   = Map[Momentum[#, dim]&, {pi,ki}];
       mu     = LorentzIndex[mui, dim];
           re = - coup Pair[k, mu]; 
(* that is a matter of taste; the sign can be swapped between 
   GhostPropagator and GluonGhostVertex. 
   For the moment let's be consistent with Abbott (Nucl. Phys. B185 (1981)).
*)
       (* re = -re;*)
re = QCDFeynmanRuleConvention[GluonGhostVertex] re;
   re] /; (Explicit /. {opt} /. Options[GluonGhostVertex]) === True  ;

GluonGhostVertex /:
   MakeBoxes[GluonGhostVertex[p3_,mu3_],
             TraditionalForm
            ] := RowBox[{SuperscriptBox[OverscriptBox["\[CapitalLambda]","~"],
                         Tbox[mu3]],
                        "(", Tbox[p3], ")"
                        }];
GluonGhostVertex /:
   MakeBoxes[GluonGhostVertex[{p1_,mu1_},{p2_,mu2_},{p3_,mu3_}],
             TraditionalForm
            ] := RowBox[{SuperscriptBox[OverscriptBox["\[CapitalLambda]","~"],
                         Tbox[mu3]],
                        "(", Tbox[p3], ")"
                        }];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GluonGhostVertex | \n "]];
Null
