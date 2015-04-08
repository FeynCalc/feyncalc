(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Zeta2 *)

(* ------------------------------------------------------------------------ *)

Zeta2::usage =
"Zeta2 denotes Zeta[2]. For convenience every Pi^2 occuring in
OPEIntegrateDelta is replaced by (6 Zeta2).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Zeta2`Private`"]


Zeta2 /: N[Zeta2] = N[Zeta[2]];
Zeta2 /: N[Zeta2, prec_] := N[Zeta[2], prec];

	Zeta2 /:
	MakeBoxes[Zeta2, TraditionalForm] :=
		RowBox[{"\[Zeta]","(",2,")"}];
(*SubscriptBox["\[Zeta]", 2]*)

FCPrint[1,"Zeta2.m loaded."];
End[]
