(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TensorFunction*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`TensorFunction`",
             "HighEnergyPhysics`FeynCalc`"];

TensorFunction::usage= 
"TensorFunction[t, mu, nu, ...] transform into 
t[LorentzIndex[mu], LorentzIndex[nu], ...], i.e., it can be used
as an unspecified tensoriell function t. 
A symmetric tensor can be obtained by 
TensorFunction[{t, \"S\"}, mu, nu, ...],
and an antisymmteric one by
TensorFunction[{t, \"A\"}, mu, nu, ...].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[TensorFunction, ReadProtected];

Dimension    = MakeContext["Dimension"];
LorentzIndex = MakeContext["LorentzIndex"];

Options[TensorFunction] = {Dimension -> 4};

TensorFunction[ef_, munu___,last_/;Head[last]=!=Rule,
               opt___Rule] := Block[{f, dim, at},
  dim = Dimension /. {opt} /. Options[TensorFunction];
  If[Head[ef] === List && Length[ef] === 2,
     f  = First[ef]; at = Last[ef]
     ,
     f = ef
    ];
  Which[
        at === "S",
        SetAttributes@@{f, Orderless};
        ,
        at === "A",
        Evaluate[f[a__LorentzIndex]] := 
                         Condition[
                          Signature[{a}] Apply[f, Sort[{a}]],
                           {a} =!= Sort[{a}]
                                  ]
       ];
  f@@(Map[LorentzIndex[#, dim]&, {munu,last}])
                                   ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TensorFunction | \n "]];
Null
