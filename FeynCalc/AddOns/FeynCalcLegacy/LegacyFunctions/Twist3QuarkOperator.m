(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Twist3QuarkOperator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Twist3QuarkOperator *)

(* ------------------------------------------------------------------------ *)

Twist3QuarkOperator::usage =
"Twist3QuarkOperator[p] or Twist3QuarkOperator[p,_,_]  yields the  2-quark
operator (p is momentum in the direction of the fermion number flow).

Twist3QuarkOperator[{p1,___}, {p2,___}, {p3, mu, a}] or
Twist3QuarkOperator[p1,_,_,  p2,_,_,  p3,mu,a] yields the
Quark-Quark-Gluon-operator, where p1 is the incoming quark, p2 the incoming
antiquark and p3 denotes the (incoming) gluon momentum.

Twist3QuarkOperator[{p1,___}, {p2,___}, {p3, mu, a}, {p4, nu, b}] or
Twist3QuarkOperator[p1,_,_,  p2,_,_,  p3,mu,a, p4, nu, b]  gives the
Quark-Quark-Gluon-Gluon-operator. The setting of the option Polarization
(unpolarized: 0; polarized: 1) determines whether the unpolarized or polarized
operator is returned.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Twist3QuarkOperator`Private`"]

Options[Twist3QuarkOperator] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Polarization -> 1
};

Twist3QuarkOperator[a1_/;Head[a1] =!= List, v_/;Head[v] =!=Integer, w_/;Head[w] =!= Integer, opt___Rule] :=
	Twist3QuarkOperator[a1, opt];

(* 2 - Quark operator; unpolarized :   RH (3A.8); (3A.9) *)
Twist3QuarkOperator[pi_, opt___Rule] :=
	Block[ {dim, p, re, pol, del},
		dim    = Dimension /. {opt} /. Options[Twist3QuarkOperator];
		pol    = Polarization /. {opt} /. Options[Twist3QuarkOperator];
		p = Momentum[pi, dim];
		del = Momentum[OPEDelta, dim];
		If[ pol === 0,
			re = (-1)^OPEm DiracGamma[del,dim] Pair[del,p]^(OPEm-1),
			re = (-1)^OPEm DOT[DiracGamma[del,dim],
												DiracGamma[5]] Pair[del,p]^(OPEm-1)
		];
		re
	];

Twist3QuarkOperator[p_, _Integer, _Integer] :=
	QuarkOperator[p];

(* Quark - Quark - Gluon *)

Twist3QuarkOperator[{p1_,__}  {p2_,__},  {p3_,mu_,a_ /; Head[a] =!= Rule}, opt___Rule] :=
	Twist3QuarkOperator[{p1}, {p2}, {p3, mu, a}, opt];

Twist3QuarkOperator[p1_,_,_,  p2_,_,_,  p3_,mu_,a_ /; Head[a] =!= Rule, opt___Rule] :=
	Twist3QuarkOperator[{p1}, {p2}, {p3, mu, a}, opt];

Twist3QuarkOperator[{p2_}, {p1_}, {_, mu_, a_}, opt___Rule] :=
	Block[ {dim, pe1, pe2, pol, del, muu, coup, re},
		coup  = CouplingConstant /. {opt} /. Options[Twist3QuarkOperator];
		dim   = Dimension /. {opt} /. Options[Twist3QuarkOperator];
		pol    = Polarization /. {opt} /. Options[Twist3QuarkOperator];
		pe1 = Momentum[p1, dim];
		pe2 = Momentum[p2, dim];
		del = Momentum[OPEDelta, dim];
		muu = LorentzIndex[mu, dim];
		If[ pol === 0,
			re = -coup  SUNT[SUNIndex[a]] Pair[del,muu] *
				OPESum[(-1)^OPEi Pair[del, pe1]^OPEi *
				Pair[del, pe2]^(OPEm-OPEi-2), {OPEi, 0, OPEm-2}],
			re = -coup (*((1-(-1)^OPEm)/2)*) *
				DOT[DiracGamma[del,dim], DiracGamma[5], SUNT[SUNIndex[a]]] Pair[del,muu] *
				OPESum[(-1)^OPEi Pair[del, pe1]^OPEi * Pair[del, pe2]^(OPEm-OPEi-2), {OPEi, 0, OPEm-2}]
		];
		re
	];

(* Quark - Quark - Gluon - Gluon *)

Twist3QuarkOperator[{p1_,__}  {p2_,__},  {p3_,mu_,a_ /; Head[a] =!= Rule}, {p4_,nu_,b_ /; Head[b] =!= Rule}, opt___Rule] :=
	Twist3QuarkOperator[{p1}, {p2}, {p3, mu, a}, {p4, nu, b}, opt];

Twist3QuarkOperator[p1_,_,_,  p2_,_,_,  p3_,mu_,a_ /; Head[a] =!= Rule, p4_,nu_,b_, opt___Rule] :=
	Twist3QuarkOperator[{p1}, {p2}, {p3, mu, a}, {p4, nu, b}, opt];

Twist3QuarkOperator[{p2_}, {p1_}, {p3_, mu_, c_},{p4_, nu_, d_}, opt___Rule] :=
	Block[{	dim, pe1, pe2, pe3, pe4, pol, del, muu, nuu, coup, re},
		coup= CouplingConstant /. {opt} /. Options[Twist3QuarkOperator];
		dim	= Dimension /. {opt} /. Options[Twist3QuarkOperator];
		pol	= Polarization /. {opt} /. Options[Twist3QuarkOperator];
		pe1 = Momentum[p1, dim];
		pe2 = Momentum[p2, dim];
		pe3 = Momentum[p3, dim];
		pe4 = Momentum[p4, dim];
		del = Momentum[OPEDelta, dim];
		muu = LorentzIndex[mu, dim];
		nuu = LorentzIndex[nu, dim];

		If[ pol === 0,
			re = "Not ready yet!",
			re = (-coup^2 (*((1-(-1)^OPEm)/2)*) *
					DOT[DiracGamma[del], DiracGamma[5],
			(DOT[SUNT[SUNIndex[d]], SUNT[SUNIndex[c]]]*
				OPESum[(-1)^OPEj*Pair[del, pe1]^OPEi*
					Pair[del, pe2]^(-3 - OPEj + OPEm)*
					(Pair[del, pe1] +
							Pair[del, pe4])^(-OPEi + OPEj),
					{OPEi, 0, OPEj}, {OPEj, 0, -3 + OPEm}] -
				(-1)^OPEm*DOT[SUNT[SUNIndex[c]] , SUNT[SUNIndex[d]]]*
				OPESum[(-1)^OPEj*Pair[del, pe1]^
						(-3 - OPEj + OPEm)*Pair[del, pe2]^OPEi*
					(Pair[del, pe2] +
							Pair[del, pe4])^(-OPEi + OPEj),
					{OPEi, 0, OPEj}, {OPEj, 0, -3 + OPEm}])
			]*
			Pair[muu, del]*
			Pair[nuu, del])
		];
		re
	];

FCPrint[1,"Twist3QuarkOperator.m loaded"];
End[]
