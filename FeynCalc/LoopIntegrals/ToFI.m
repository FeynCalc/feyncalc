(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: ToFI *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on November 10th 2003 , RM*)
(* ------------------------------------------------------------------------ *)

(* :Summary: introduce Feynman integrals as functions (see also ToTFI) *)

(* ------------------------------------------------------------------------ *)

ToFI::usage =
"ToFI[expr, {q1, q2}, {p}] translates all non-tensorial loop integrals in expr
into TFI notation from TARCER.

ToFI[expr, {q}, {p}] introduces TBI B0-like integrals.

ToFI can be extended to more external particles and more loops if needed.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToFI`Private`"]

(*TODO ToFI should be generalized *)

ToFI[x_,y_List, z_List] :=
	tofi[x,y,z] /. {
		FeynmanIntegral[expr_, {q_}, {p_}] :> ToTFI[expr, q, p],
		FeynmanIntegral[expr_, {q1_, q2_}, {p_}] :> ToTFI[expr, q1, q2, p]
	};

tofi[a_, {q__},__] :=
	a /; FreeQ[a, Alternatives@@{q}];

tofi[expr_Plus,m__] :=
	tofi[#, m]& /@ expr;

tofi[expr_Times, {q__}, {ps__}] :=
	( FCE[SelectFree[expr, {q}]] *
	FeynmanIntegral[FCE @ SelectNotFree[expr, {q}], {q}, {ps}]
	) /; SelectFree[expr, {q}] =!= 1;

ToFI[e_,{q_},{p_}] :=
	ToTFI[e, q, p];

ToFI[e_,{q1_, q2_},{p_}] :=
	ToTFI[e, q1, q2, p];


FCPrint[1,"ToFI.m loaded."];
End[]
