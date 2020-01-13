(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DotExpand *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created February 26th 2003 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Expand DOT products *)

(* ------------------------------------------------------------------------ *)


DotExpand::usage =
"DotExpand[exp] expands dot products in exp.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DotExpand`Private`"]

DotExpand[expr_] :=
	expr //. {DOT[a___, b_ + c_, d___] :> Distribute[DOT[a, b + c, d]],
	DOT[a___, b_*c_, d___] :> b*DOT[a, c, d] /; NonCommFreeQ[b],
	DOT[a___, b_, d___] :> b*DOT[a, d] /; NonCommFreeQ[b],
	DOT[a___, b_*c__, d___] :> DOT[a, b, c, d] /; NonCommQ[b]} /.
	DOT[] :> Sequence[];

FCPrint[1,"DotExpand.m loaded"];
End[]
