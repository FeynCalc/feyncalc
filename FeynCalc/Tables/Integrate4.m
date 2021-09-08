(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Integrate4 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 13 April '98 at 11:12 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

Integrate4::usage=
"Integrate4 is like Integrate5, but can be interrupted if $VeryVerbose > 2.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Integrate4`Private`"]

Integrate4[a__] := If[$VeryVerbose > 2,
(*
										InputForm[
											hold[int[a]]/.hold->Hold/.int->Integrate5]
*)
														Integrate5[a],
										Integrate5[a]
										];

FCPrint[1,"Integrate4.m loaded."];
End[]
