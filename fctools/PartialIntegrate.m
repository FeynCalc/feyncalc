(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PartialIntegrate*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`PartialIntegrate`",
             "HighEnergyPhysics`FeynCalc`"];

PartialIntegrate::usage= "
PartialIntegrate[exp, ap, t] does a partial
integration of the definite integral Integrate[exp,{t,0,1}].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
(*Options[PartialIntegrate] = {Hold -> False};*)
Options[PartialIntegrate] = {DummyIndex -> Global`z, Begin -> 0};

MakeContext[Collect2, Factor2, DummyIndex
                              (*should be called DummyVariable, but lets save names*)];

PartialIntegrate[exp_Plus, aa_, z_, opts___Rule] := PartialIntegrate[#, aa, z, opts]&/@exp;

PartialIntegrate[w_Times, aap_, z_, opts___Rule] :=
If[Head[z]===List,

(*Added 17/10-2001, F.Orellana*)
  If[FreeQ[w, aap] || IntegerQ[aap], w,
     Block[{aa, bb, bbb, bbp,
            dum=DummyIndex/.Flatten[{opts}]/.Options[PartialIntegrate],
            beg = Begin/.Flatten[{opts}]/.Options[PartialIntegrate]},
           bb  = w/aap;
           aa  = Factor2[Integrate[aap /. z[[1]] :> dum, {dum, beg, z[[1]]}]];
           bbp = If[FreeQ[bbb=D[bb, z[[1]]], Hypergeometric2F1],
                   bbb, Collect2[bbb, Hypergeometric2F1]
                 ];
           ((((aa bb) /. z[[1]]  -> z[[-1]]) -
           ((aa bb) /. z[[1]]  -> z[[2]])) /. 0^_ :> 0) -
            Integrate[Collect2[aa bbp, Hypergeometric2F1], z]
          ]
    ],

  If[FreeQ[w, aap] && aap =!= 1, w,
     Block[{aa, bb, bbp},
           bb  = w/aap;
           aa  = Factor2[Integrate[aap, z]];
           If[FreeQ[bb, Hypergeometric2F1],
              bbp = D[bb, z],
              bbp = Collect2[D[bb, z], Hypergeometric2F1]
             ];
           ((((aa bb) /. z  -> 1) -
           ((aa bb) /. z  -> 0)) /. 0^_ :> 0) -
            Collect2[aa bbp, Hypergeometric2F1]
          ]
    ]

];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PartialIntegrate | \n "]];
Null
