(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Calc*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Calc does a lot *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Calc`",
             "HighEnergyPhysics`FeynCalc`"];

Calc::usage = 
"Calc[exp] performs several simplifications.
Calc[exp] is the same as
DotSimplify[
 DiracSimplify[EpsEvaluate[Contract[DiracSimplify[
Contract[
  Explicit[ SUNSimplify[PowerSimplify[Trick[exp]],
 Explicit -> False] ]]
               ]]]]].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[Calc, ReadProtected];

(*
Contract3 = MakeContext["Contract", "Contract3"];
*)

MakeContext[
Contract,
DiracSimplify,
DotSimplify,
EpsContract,
EpsEvaluate,
Expand2,
Expanding,
ExpandScalarProduct,
Explicit,
FeynCalcInternal,
PowerSimplify,
SUNSimplify,
Trick
           ];

Calc[exp_] :=
Expand2[ExpandScalarProduct[
DotSimplify[
 DiracSimplify[EpsEvaluate[Contract[DiracSimplify[
  Contract[Explicit[ SUNSimplify[Trick[exp]//PowerSimplify, 
Explicit -> False] ]]
               ]]]]]]//PowerSimplify];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Calc | \n "]];
Null
