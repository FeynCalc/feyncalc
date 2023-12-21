(* :Title: StringChomp *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: copied from MathXLS, similar to Perl's chomp function *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

StringChomp::usage =
"StringChomp[str] chops initial and final white space of the string str.";

Begin["`Package`"]
End[]

Begin["`StringChomp`Private`"]


SetAttributes[StringChomp, Listable];

chomp[""] = chomp[" "] = "";
StringChomp[s_] :=
	FixedPoint[chomp, s];

chomp[s_] :=
	StringJoin @@ (Flatten[
	Replace[Split[ Characters[s]],
		{{{(" " | "\t" | "\n" | "\r") ..}, b___,
		{(" " | "\n" | "\t" | "\r") ..}} :> {b},
		{{(" " | "\n" | "\t" | "\r") ..}, r___} :> {r},
		{a___, {(" " | "\n" | "\t" | "\r") ..}} :> {a}}]]);

FCPrint[1,"StringChomp.m loaded."];
End[]

