(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The inverse of DiracGammaExpand *)

(* ------------------------------------------------------------------------ *)

DiracGammaCombine::usage =
"DiracGammaCombine[exp] is (nearly) the inverse operation to \
DiracGammaExpand.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracGammaCombine`Private`"]

DiracGammaCombine[y_] :=
	If[ !FreeQ2[y,{GS, GSD, GSE}],
		FeynCalcInternal[y]//dircg,
		y//dircg
	];

dircg[x_Plus] :=
	If[ Length[x] > 8,
		Map[DiracGammaCombine, x],
		x //. gasumrules
	];

dircg[exp_] :=
	exp //. gasumrules;

(* merge sums of DiracGamma's into one *)
gasumrules = {
		n1_. DiracGamma[Momentum[x_,di___],di___] +
		n2_. DiracGamma[Momentum[y_,di___],di___] :>
			DiracGamma[ Momentum[n1 x + n2 y,di], di ] /;
			(NumberQ[n1] && NumberQ[n2]),
		(n1_. DiracGamma[Momentum[x_, di___], di___] +
		n2_. DiracGamma[Momentum[x_, di___], di___] ):>
			(n1+n2) DiracGamma[Momentum[x, di], di],
		(n3_. Momentum[x_,di___] + n4_. Momentum[y_,di___]):>
		Momentum[ Expand[n3 x + n4 y],di]/;(NumberQ[n3]&&NumberQ[n4])
};

FCPrint[1,"DiracGammaCombine.m loaded"];
End[]
