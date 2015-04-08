(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SMP *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

SMP::usage= "SMP[par] substitutes a symbol for the
Standard Model parameter par.
SMP[] gives the list of substitutions.
par should be a string; e.g., MP[\"SW\"] gives
sw (in the Global` context).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`fctables`Private`"]


SMP[] := SMP[] =
{"EL" :> FCGV["e"],
"CW" :> FCGV["cw"],
"ME" :> FCGV["me"],
"MH" :> FCGV["mh"],
"MW" :> FCGV["mw"],
"SW" :> FCGV["sw"]
};

SMP[par_String] := par /. SMP[];

FCPrint[1,"SMP.m loaded."];
End[]
