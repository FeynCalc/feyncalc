(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Twist2AlienOperator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Twist2AlienOperator *)

(* ------------------------------------------------------------------------ *)

Twist2AlienOperator::usage =
"Twist2AlienOperator[p, 0] : (7);   Twist2AlienOperator[p1,p2,{p3,mu,a}, 0]
(p1: incoming quark momentum, p3: incoming gluon (count1)).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Twist2AlienOperator`Private`"]

DeclareNonCommutative[Twist2AlienOperator];

Options[Twist2AlienOperator] = { CouplingConstant -> SMP["g_s"],
													Dimension -> D, Polarization -> 0};
Twist2AlienOperator[pi_, 0, opt___Rule] :=
	Block[ {dim, p, re, pol, del, coup, li1,li2,li3},
		coup    = CouplingConstant /. {opt} /. Options[Twist2AlienOperator];
		dim    = Dimension /. {opt} /. Options[Twist2AlienOperator];
		pol    = Polarization /. {opt} /. Options[Twist2AlienOperator];
		p = Momentum[pi, dim];
		del = Momentum[OPEDelta, dim];
		Which[pol === 0,
				re =   -( -2*CF*coup^2 *
					( -2/(-1 + OPEm) + 2/OPEm - (1 + OPEm)^(-1) ) *
					Sn*DiracGamma[del, dim]*
					Pair[del, p]^(-1 + OPEm)
							) / Epsilon
			,
					pol === 1,
			re =
		-(4*CF*coup^2*
				(2/OPEm - (1 + OPEm)^(-1))*
				Sn*
				DiracGamma[del, dim] . DiracGamma[5]*
				Pair[del, p]^(-1 + OPEm))/ Epsilon
			,
		(* no Chisholm *)
					pol === 2,
		li1 = LorentzIndex[Unique[$MU], dim];
		li2 = LorentzIndex[Unique[$MU], dim];
		li3 = LorentzIndex[Unique[$MU], dim];
		re =  -(2*I*CF*coup^2*Sn*Pair[del, p]^(-2 + OPEm)*
		(DOT[DiracGamma[li3] , DiracGamma[del] , DiracGamma[li2]]*
				Eps[li2, li3, del, p] -
			OPEm*DOT[DiracGamma[li3] , DiracGamma[del] , DiracGamma[li2]]*
				Eps[li2, li3, del, p] -
			DOT[DiracGamma[li3] , DiracGamma[li1] , DiracGamma[li2]]*
				Eps[del, li1, li2, li3]*Pair[del, p]))/(Epsilon*OPEm*(1 + OPEm))

			];
		re
	];
(* count1 *)

Twist2AlienOperator[p1_, _, {p3_, mu_, a_}, 0, opt___Rule] :=
	Block[ {dim, re, pol, del, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2AlienOperator];
		dim    = Dimension /. {opt} /. Options[Twist2AlienOperator];
		pol    = Polarization /. {opt} /. Options[Twist2AlienOperator];
		Which[pol === 0,
					re =   coup^3 (1+(-1)^OPEm)/2*
					DOT[SUNT[SUNIndex[a]], DiracGamma[Momentum[OPEDelta, dim], dim]]*
				(2*I*Sn*
				Pair[LorentzIndex[mu, dim], Momentum[OPEDelta, dim]]*
				(((-1 + OPEm)^(-1) - OPEm^(-1))*
						(-Pair[Momentum[OPEDelta, dim], Momentum[p3, dim]])^(-2 + OPEm) +
					(-2/(-1 + OPEm) + 2/OPEm - (1 + OPEm)^(-1))*
						(Pair[Momentum[OPEDelta, dim], Momentum[p1, dim]]^(-1 + OPEm)/
							(Pair[Momentum[OPEDelta, dim], Momentum[p1, dim]] +
								Pair[Momentum[OPEDelta, dim], Momentum[p3, dim]]) -
							(-Pair[Momentum[OPEDelta, dim], Momentum[p3, dim]])^(-1 + OPEm)/
							(Pair[Momentum[OPEDelta, dim], Momentum[p1, dim]] +
								Pair[Momentum[OPEDelta, dim], Momentum[p3, dim]]))))/Epsilon
						,
					pol === 1,
					re =
		((1 - (-1)^OPEm)*CA*coup^3*(2 + OPEm)*
				DOT[SUNT[SUNIndex[a]],DiracGamma[Momentum[OPEDelta, dim], dim],
							DiracGamma[5]
						]*
				Pair[LorentzIndex[mu, dim], Momentum[OPEDelta, dim]]*
				(-(Pair[Momentum[OPEDelta, dim], Momentum[p1, dim]]*
							(-Pair[Momentum[OPEDelta, dim], Momentum[p3, dim]])^OPEm) -
					Pair[Momentum[OPEDelta, dim], Momentum[p1, dim]]^OPEm*
					Pair[Momentum[OPEDelta, dim], Momentum[p3, dim]]))/
			(Epsilon*OPEm*(1 + OPEm)*Pair[Momentum[OPEDelta, dim], Momentum[p1, dim]]*
				Pair[Momentum[OPEDelta, dim], Momentum[p3, dim]]*
				(Pair[Momentum[OPEDelta, dim], Momentum[p1, dim]] +
					Pair[Momentum[OPEDelta, dim], Momentum[p3, dim]]))
				];
		re
	];

FCPrint[1,"Twist2AlienOperator.m loaded"];
End[]
