(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EpsChisholm *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`EpsChisholm`",
             "HighEnergyPhysics`FeynCalc`"];

EpsChisholm::"usage"=
"EpsChisholm[expr] substitutes for a gamma matrix contracted with
a Levi Civita tensor (Eps) the Chisholm identity.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   


MakeContext[DiracGamma, DiracSimplify, Eps, 
            ExpandScalarProduct, Expand2, LorentzIndex, Spinor];

   (*EpsChisholmdef*)
 EpsChisholm[x_] := x /; FreeQ[x, Eps] || FreeQ[x, DiracGamma];
 EpsChisholm[x_] := Block[{new=0, xx,i},
                          xx = Expand2[x,Eps];
                          If[ Head[xx]===Plus,
                              For[i=1, i<=Length[xx], i++,
                                  new = new +
                                  ((xx[[i]]/.Eps->eeps)/.eepsrule/.
                                    eepsrule/.eepsrule/.eepsrule/.
                                    eepsrule/.
                                    epsspcrule0/.epsspcrule0/.
                                    epsspcrule/.epsspcrule)
                                 ],
                              new = (xx/.Eps->eeps)/.eepsrule/.
                                    eepsrule/.eepsrule/.eepsrule/.
                                    eepsrule;
                              new = new/.epsspcrule0/.epsspcrule0;
                              new = new/.epsspcrule/.epsspcrule
                            ];
                      new/.eeps->Eps] /; !FreeQ[x,Eps];


(* #################################################################### *)
(*                             Main42                                   *)
(* #################################################################### *)

(* change maybe later *)
SpinorChainEvaluate = DiracSimplify;
scev = ExpandScalarProduct;

  eepsrule={ m_. DOT[ x___,DiracGamma[LorentzIndex[in_]],y___] *
                    eeps[a___,LorentzIndex[in_],b_,c___] :>
              -m DOT[x,DiracGamma[LorentzIndex[in]],y] *
               eeps[a,b,LorentzIndex[in],c]
           };
 epsspcrule0={
            m_. DOT[ x___,DiracGamma[LorentzIndex[in_]],y___] *
               eeps[a_,b_,c_,LorentzIndex[in_]] :>
               (( - I m ( DOT[x,DiracGamma[a],DiracGamma[b],DiracGamma[c],
                           DiracGamma[5],y
                          ] -
                      scev[a,b] DOT[x,DiracGamma[c].DiracGamma[5],y] -
                      scev[b,c] DOT[x,DiracGamma[a].DiracGamma[5],y] +
                      scev[a,c] DOT[x,DiracGamma[b].DiracGamma[5],y]
                     )//SpinorChainEvaluate
                )//Expand
               ) /; FreeQ[{x,y}, Spinor[_, _Symbol,___]]
             };
 epsspcrule={
            m_. DOT[ x___,DiracGamma[LorentzIndex[in_]],y___] *
               eeps[a_,b_,c_,LorentzIndex[in_]] :>
               ( - I m ( DOT[x,DiracGamma[a],DiracGamma[b],DiracGamma[c],
                           DiracGamma[5],y
                          ] -
                      scev[a,b] DOT[x,DiracGamma[c].DiracGamma[5],y] -
                      scev[b,c] DOT[x,DiracGamma[a].DiracGamma[5],y] +
                      scev[a,c] DOT[x,DiracGamma[b].DiracGamma[5],y]
                     )//SpinorChainEvaluate
               )//Expand,
              DiracGamma[LorentzIndex[in_]] *
              eeps[a_,b_,c_,LorentzIndex[in_]] :>
               ( - I ( DOT[x,DiracGamma[a],DiracGamma[b],DiracGamma[c],
                           DiracGamma[5],y
                          ] -
                      scev[a,b] DOT[x,DiracGamma[c].DiracGamma[5],y] -
                      scev[b,c] DOT[x,DiracGamma[a].DiracGamma[5],y] +
                      scev[a,c] DOT[x,DiracGamma[b].DiracGamma[5],y]
                     )//SpinorChainEvaluate
               )//Expand
            };


 
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "EpsChisholm | \n "]];
Null
