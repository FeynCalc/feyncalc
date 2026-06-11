(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: IntegrateByParts*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

IntegrateByParts::usage =
"IntegrateByParts[(1 - t)^(a Epsilon -1)g[t], deriv, t] does an integration by
parts of the definite integral over t from 0 to 1.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`IntegrateByParts`Private`"]


Options[IntegrateByParts] = {Hold -> False};

IntegrateByParts[exp_Plus, aa_, z_] :=
	IntegrateByParts[#, aa, z]&/@exp;

IntegrateByParts[w_Times, aap_, z_] :=
	If[ FreeQ[w, aap] && aap =!= 1,
		w,
		Block[ {aa, bb, bbp},
			bb  = w/aap;
			aa  = Factor2[Integrate[aap, z]];
			If[ FreeQ[bb, Hypergeometric2F1],
				bbp = D[bb, z],
				bbp = Collect2[D[bb, z], Hypergeometric2F1]
			];
			((((aa bb) /. z  -> 1) -
			((aa bb) /. z  -> 0)) /. 0^_ :> 0) -
				Collect2[aa bbp, Hypergeometric2F1]
		]
	];
FCPrint[1,"IntegrateByParts.m loaded."];
End[]
