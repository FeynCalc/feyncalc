(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Tensorfunction*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Tensorfunction`",
             "HighEnergyPhysics`FeynCalc`"];

Tensorfunction::usage= 
"Tensorfunction[t, mu, nu, ...] transform into 
t[LorentzIndex[mu], LorentzIndex[nu], ...], i.e., it can be used
as an unspecified tensoriell function t. 
A symmetric tensor can be obtained by 
Tensorfunction[{t, \"S\"}, mu, nu, ...],
and an antisymmteric one by
Tensorfunction[{t, \"A\"}, mu, nu, ...].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[Tensorfunction, ReadProtected];

Dimension    = MakeContext["Dimension"];
LorentzIndex = MakeContext["LorentzIndex"];

Options[Tensorfunction] = {Dimension -> 4};

Tensorfunction[ef_, munu___,last_/;Head[last]=!=Rule,
               opt___Rule] := Block[{f, dim, at},
  dim = Dimension /. {opt} /. Options[Tensorfunction];
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
If[$VeryVerbose > 0,WriteString["stdout", "Tensorfunction | \n "]];
Null
