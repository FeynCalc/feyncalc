(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PartialIntegrate*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`PartialIntegrate`",
             "HighEnergyPhysics`FeynCalc`"];

PartialIntegrate::"usage"= "
PartialIntegrate[exp, ap, t] does a partial
integration of the definite integral Integrate[exp,{t,0,1}],
with ap the factor that is to be integrated and exp/ap the
factor that is to be differentiated.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
(*Options[PartialIntegrate] = {Hold -> False};*)
Options[PartialIntegrate] = {Integrate -> Integrate};

MakeContext[Collect2, Factor2, DummyIndex
                              (*should be called DummyVariable, but lets save names*)];

PartialIntegrate[exp_Plus, aa_, z_, opts___Rule] := PartialIntegrate[#, aa, z, opts]&/@exp;

PartialIntegrate[w_Times, aap_, z_, opts___Rule] :=
(integrate = (Integrate/.{opts}/.Options[PartialIntegrate]);

If[Head[z]===List,

(*Added 17/10-2001, F.Orellana*)
  If[FreeQ[w, aap] || IntegerQ[aap], w,
     Block[{aa, bb, bbb, bbp, dum, dumz1, dumz2, dumz3},
           bb  = w/aap;
           aa  = Factor2[integrate[aap , z[[1]]]];
           bbp = If[FreeQ[bbb=D[bb, z[[1]]], Hypergeometric2F1],
                   bbb, Collect2[bbb, Hypergeometric2F1]
                 ];
           ((HoldForm[dum /. dumz1  -> dumz3] -
             HoldForm[dum /. dumz1  -> dumz2]) /.
            {dumz1 -> z[[1]], dumz2 ->z[[2]], dumz3 -> z[[-1]], dum -> (aa bb)} /. 0^_ :> 0) -
            integrate[Collect2[aa bbp, Hypergeometric2F1], z] //
          If[FreeQ[#,integrate], ReleaseHold[#], #]&
          ]
    ](**),

  If[FreeQ[w, aap] && aap =!= 1, w,
     Block[{aa, bb, bbp, dum},
           bb  = w/aap;
           aa  = Factor2[integrate[aap, z]];
           If[FreeQ[bb, Hypergeometric2F1],
              bbp = D[bb, z],
              bbp = Collect2[D[bb, z], Hypergeometric2F1]
             ];
           ((HoldForm[dum /. z  -> 1] -
           HoldForm[dum /. z  -> 0]) /. dum -> (aa bb) /. 0^_ :> 0) -
            (*Added integrate. Why was it not there?
              Did we assume that the z-dependence drops?
              20/9-2002. F.Orellana*)
            integrate[Collect2[aa bbp, Hypergeometric2F1], {z,0,1}] //
          If[FreeQ[#,integrate], ReleaseHold[#], #]&
          ]
    ]

]);

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PartialIntegrate | \n "]];
Null
