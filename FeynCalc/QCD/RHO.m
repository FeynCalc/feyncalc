(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: RHO*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  projectors for eq. (3.2.17) - (3.2.20) *)

(* ------------------------------------------------------------------------ *)

RHO::usage = "RHO[i, mu, nu, p] with i from 1 to 4 is an abbreviation for the
4 operators (eq. (3.2.17) -- (3.2.20)) defined in R.Hambergs thesis.
The Lorentz indices are suppressed. The explicit expressions may
be recovered by RHO[i, mu, nu, p].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`RHO`Private`"]

Options[RHO] = {Dimension -> D};

RHO[i_Integer, mu_, nu_, p_, opt___Rule] :=
	Block[ {gmn, pm, dm, pn, dn, dp, p2, re},
		gmn = MT[mu,nu];
		pm = FV[p, mu];
		pn = FV[p, nu];
		dm = FV[OPEDelta, mu];
		dn = FV[OPEDelta, nu];
		dp = ScalarProduct[OPEDelta, p];
		p2 = ScalarProduct[p];
		re =
		Which[i === 1,
					gmn - (pm dn + dm pn)/dp + dm dn p2/dp^2,
				i === 2,
					pm pn/p2 - (pm dn + dm pn)/dp + dm dn p2/dp^2,
				i === 3,
					-1/2 (pm dn + dm pn)/dp + dm dn p2/dp^2,
				i === 4,
					(* at 2-loop : 1/2*) (pm dn + dm pn)/dp (*+ dm dn p2/dp^2*)
				];
		ChangeDimension[FeynCalcInternal[re], Dimension /. {opt} /. Options[RHO]]
	];

FCPrint[1,"RHO.m loaded"];
End[]
