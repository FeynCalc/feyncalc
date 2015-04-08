(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: expands DiracGamma[ exp_Plus ] *)

(* ------------------------------------------------------------------------ *)

DiracGammaExpand::usage =
"DiracGammaExpand[exp] expands all DiracGamma[Momentum[a+b+..]] in \
exp into (DiracGamma[Momentum[a]] + DiracGamma[Momentum[b]] + ...).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracGammaExpand`Private`"]

DiracGammaExpand[x_] :=
	If[ FreeQ[x, DiracGamma],
		FeynCalcInternal[x],
		x
	] /. DiracGamma -> gaev /. gaevlin -> DiracGamma;

gaev[x_,di___] :=
	gaevlin[Expand[x//MomentumExpand, Momentum], di];

gaevlin[n_Integer] :=
	DiracGamma[n];

gaevlin[x_Plus, di___] :=
	Map[gaevlin[#, di]&, x];

FCPrint[1,"DiracGammaExpand.m loaded."];
End[]
