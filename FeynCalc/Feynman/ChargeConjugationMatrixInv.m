(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ChargeConjugationMatrixInv *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

ChargeConjugationMatrixInv::usage =
"ChargeConjugationMatrixInv is the inverse of ChargeConjugationMatrix. \
It is substituted immediately by -ChargeConjugationMatrix.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ChargeConjugationMatrixInv`Private`"]


ChargeConjugationMatrixInv =
	-ChargeConjugationMatrix;

(*
If[FreeQ[$NonComm, ChargeConjugationMatrixInv] && Head[$NonComm]===List,
	AppendTo[$NonComm, ChargeConjugationMatrixInv]];
*)

FCPrint[1,"ChargeConjugationMatrixInv.m loaded."];
End[]
