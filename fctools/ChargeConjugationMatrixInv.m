(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ChargeConjugationMatrixInv *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`ChargeConjugationMatrixInv`",
             "HighEnergyPhysics`FeynCalc`"];

ChargeConjugationMatrixInv::usage=
"ChargeConjugationMatrixInv is the inverse of 
ChargeConjugationMatrix. It is substituted immediately by
-ChargeConjugationMatrix.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

ChargeConjugationMatrix = MakeContext["ChargeConjugationMatrix"];

ChargeConjugationMatrixInv = -ChargeConjugationMatrix;

(*
If[FreeQ[$NonComm, ChargeConjugationMatrixInv] && Head[$NonComm]===List,
   AppendTo[$NonComm, ChargeConjugationMatrixInv]];
*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,
WriteString["stdout", "ChargeConjugationMatrixInv | \n "]];
Null
