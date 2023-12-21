(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Twist2QuarkOperator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 March '98 at 23:38 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Twist2QuarkOperator *)

(* ------------------------------------------------------------------------ *)


QO::usage =
"QO is equivalent to Twist2QuarkOperator.";

Twist2QuarkOperator::usage =
"Twist2QuarkOperator[p] or Twist2QuarkOperator[p,_,_] yields the
quark-antiquark operator (p is momentum in the direction of the incoming
quark). Twist2QuarkOperator[{p,q}] yields the 2-quark operator for non-zero
momentum insertion (p is momentum in the direction of the incoming quark).
Twist2QuarkOperator[{p1,___}, {p2,___}, {p3, mu, a}] or
Twist2QuarkOperator[p1,_,_, p2,_,_, p3,mu,a] is the quark-antiquark-gluon
operator, where p1 is the incoming quark, p2 the incoming antiquark and p3
denotes the incoming gluon momentum. Twist2QuarkOperator[{p1}, {p2}, {p3, mu,
a}, {p4, nu, b}] or Twist2QuarkOperator[{p1,___}, {p2,___}, {p3, mu, a}, {p4,
nu, b}] or Twist2QuarkOperator[p1,_,_, p2,_,_, p3,mu,a, p4, nu, b] gives the
Quark-Quark-Gluon-Gluon-operator. The setting of the option Polarization
(unpolarized: 0; polarized: 1) determines whether the unpolarized or polarized
operator is returned.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Twist2QuarkOperator`Private`"]

DeclareNonCommutative[Twist2QuarkOperator];

Options[Twist2QuarkOperator] = {
	CouplingConstant -> SMP["g_s"],
	Dimension        -> D,
	Explicit         -> True,
	Polarization     -> 1 ,
	ZeroMomentumInsertion -> True
};

QO = Twist2QuarkOperator;

(* p is the incoming quark *)
Twist2QuarkOperator[{p_, q_}, OptionsPattern[]] :=
	Block[ {dim,pol},
		dim    = OptionValue[Dimension];
		pol    = OptionValue[Polarization];
		ChangeDimension[
			If[ pol === 0,
				(2*DiracGamma[Momentum[OPEDelta]]*
				(Pair[Momentum[OPEDelta], Momentum[p]] -
					Pair[Momentum[OPEDelta], Momentum[q]])^(-1 + OPEm))/2^OPEm,
				(2*DOT[GA[5] , GS[OPEDelta]]*(SO[p] - SO[q])^(-1 + OPEm))/2^OPEm
			], dim]
	]/; !OptionValue[ZeroMomentumInsertion];


Twist2QuarkOperator[a1_/;Head[a1] =!= List, v_/;Head[v] =!=Integer,    w_/;Head[w] =!= Integer,
	opt:OptionsPattern[]] :=
	Twist2QuarkOperator[a1, opt];

Twist2QuarkOperator[a1_/;Head[a1] =!= List, v_/;Head[v] =!=Integer, w_/;Head[w] =!= Integer,
_,_,opt:OptionsPattern[]] :=
	Twist2QuarkOperator[a1, opt];


(* 2 - Quark operator; unpolarized :   RH (3A.8); (3A.9) *)
Twist2QuarkOperator[pi_, OptionsPattern[]] :=
	Block[ {dim, p, re, pol, del},
		dim  = OptionValue[Dimension];
		pol  = OptionValue[Polarization];
		p = Momentum[pi, dim];
		del = Momentum[OPEDelta, dim];
		If[ pol === 0,
			re =  DiracGamma[del,dim] Pair[del,p]^(OPEm-1),
			re =  -DOT[DiracGamma[del,dim],DiracGamma[5]] Pair[del,p]^(OPEm-1)
		];
		re
	] /; OptionValue[ZeroMomentumInsertion];


Twist2QuarkOperator[p_, _Integer, _Integer] :=
	Twist2QuarkOperator[p];

(* Quark - Quark - Gluon *)

Twist2QuarkOperator[{p1_,__}  {p2_,__},  {p3_,mu_,a_ /; Head[a] =!= Rule}, opt:OptionsPattern[]] :=
	Twist2QuarkOperator[{p1}, {p2}, {p3, mu, a}, opt];

Twist2QuarkOperator[p1_,_,_,  p2_,_,_,  p3_,mu_,a_ /; Head[a] =!= Rule, opt:OptionsPattern[]] :=
	Twist2QuarkOperator[{p1}, {p2}, {p3, mu, a}, opt];

Twist2QuarkOperator[{p1_}, {p2_}, {p3_, mu_, a_}, OptionsPattern[]] :=
	Block[ {dim, coup, pol},
		coup = OptionValue[CouplingConstant];
		dim = OptionValue[Dimension];
		pol = OptionValue[Polarization];
		ChangeDimension[
			If[ pol === 0,
				(4*coup*FV[OPEDelta, mu]*GS[OPEDelta]*
				OPESum[(SO[p1] - SO[p2] - SO[p3])^(-2 - OPEi + OPEm)*
							(SO[p1] - SO[p2] + SO[p3])^OPEi,
							{OPEi, 0, -2 + OPEm}]*SUNT[a])/2^OPEm,
				(4*coup*DOT[SUNT[a] , GA[5] , GS[OPEDelta]]*FV[OPEDelta, mu]*
				OPESum[(SO[p1] - SO[p2] - SO[p3])^(-2 - OPEi + OPEm)*
				(SO[p1] - SO[p2] + SO[p3])^OPEi, {OPEi, 0, -2 + OPEm}])/2^OPEm
			], dim]
	]/; !OptionValue[ZeroMomentumInsertion];

Twist2QuarkOperator[{p2_}, {p1_}, {_, mu_, a_}, OptionsPattern[]] :=
	Block[ {dim, pe1, pe2, pol, del, muu, coup, exp, re},
		coup = OptionValue[CouplingConstant];
		dim = OptionValue[Dimension];
		pol = OptionValue[Polarization];
		exp = OptionValue[Explicit];
		pe1 = Momentum[p1, dim];
		pe2 = Momentum[p2, dim];
		del = Momentum[OPEDelta, dim];
		muu = LorentzIndex[mu, dim];
		If[ pol === 0,
			If[ exp =!= All,
				re =  coup  DOT[DiracGamma[del,dim],
				SUNT[SUNIndex[a]]] Pair[del,muu] OPESum[(-1)^OPEi Pair[del, pe1]^OPEi *
				Pair[del, pe2]^(OPEm-OPEi-2),{OPEi, 0, OPEm-2}],
				re = -coup  DOT[DiracGamma[del,dim],
				SUNT[SUNIndex[a]]] Pair[del,muu] (((-1)^OPEm*Pair[del, pe1]^(-1 + OPEm))/
				(Pair[del, pe1] + Pair[del, pe2]) +    Pair[del, pe2]^(-1 + OPEm)/(Pair[del, pe1] +
				Pair[del, pe2]))
			],
			If[ exp =!= All,
				re = -coup DOT[DiracGamma[del,dim], DiracGamma[5],
				SUNT[SUNIndex[a]]] Pair[del,muu] OPESum[(-1)^OPEi Pair[del, pe1]^OPEi *
				Pair[del, pe2]^(OPEm-OPEi-2),{OPEi, 0, OPEm-2}],
				re = -coup * DOT[DiracGamma[del,dim], DiracGamma[5],
				SUNT[SUNIndex[a]]] Pair[del,muu] (((-1)^OPEm*Pair[del, pe1]^(-1 + OPEm))/
				(Pair[del, pe1] + Pair[del, pe2]) +
				Pair[del, pe2]^(-1 + OPEm)/(Pair[del, pe1] + Pair[del, pe2]))
			]
		];
		re
	] /; OptionValue[ZeroMomentumInsertion];

(* Quark - Quark - Gluon - Gluon *)
Twist2QuarkOperator[{p1_,__}  {p2_,__},  {p3_,mu_,a_ /; Head[a] =!= Rule},
{p4_,nu_,b_ /; Head[b] =!= Rule}, opt:OptionsPattern[]] :=
	Twist2QuarkOperator[{p1}, {p2}, {p3, mu, a}, {p4, nu, b}, opt];

Twist2QuarkOperator[p1_,_,_,  p2_,_,_,  p3_,mu_,a_ /; Head[a] =!= Rule,
p4_,nu_,b_, opt:OptionsPattern[]] :=
	Twist2QuarkOperator[{p1}, {p2}, {p3, mu, a}, {p4, nu, b}, opt];

Twist2QuarkOperator[{p1_}, {p2_}, {p3_, mu_, c_},{p4_, nu_, d_}, OptionsPattern[]] :=
	Block[ {dim, pe1, pe2, pe3, pe4, pol, del, muu, coup,exp,re, nuu},
		coup = OptionValue[CouplingConstant];
		dim  = OptionValue[Dimension];
		pol = OptionValue[Polarization];
		pe1 = Momentum[p1, dim];
		pe2 = Momentum[p2, dim];
		pe3 = Momentum[p3, dim];
		pe4 = Momentum[p4, dim];
		del = Momentum[OPEDelta, dim];
		exp = OptionValue[Explicit];
		muu = LorentzIndex[mu, dim];
		nuu = LorentzIndex[nu, dim];
		If[ pol === 0,
			re = -(coup^2 (-1)^OPEm *
					DOT[DiracGamma[del],
			(DOT[SUNT[SUNIndex[d]], SUNT[SUNIndex[c]]]*
				OPESum[Pair[del, -pe1]^(OPEm-3-OPEi)*
					(Pair[del, pe2])^(OPEj)*
					(Pair[del, pe2] +
							Pair[del, pe4])^(OPEi - OPEj),
					{OPEi, 0, -3 + OPEm}, {OPEj, 0, OPEi}] +
				DOT[SUNT[SUNIndex[c]] , SUNT[SUNIndex[d]]]*
				OPESum[Pair[del, -pe1]^
						(-3 - OPEi + OPEm)*(Pair[del, pe2])^OPEj*
					(Pair[del, pe2] +
							Pair[del, pe3])^(OPEi - OPEj),
						{OPEi, 0, -3 + OPEm}, {OPEj, 0, OPEi}])]*
				Pair[muu, del] Pair[nuu, del]),
			re = (-coup^2 DOT[DiracGamma[del], DiracGamma[5],
			(DOT[SUNT[SUNIndex[d]], SUNT[SUNIndex[c]]]*
			OPESum[(-1)^OPEj*Pair[del, pe1]^OPEi*
			Pair[del, pe2]^(-3 - OPEj + OPEm)*
			(Pair[del, pe1] + Pair[del, pe4])^(-OPEi + OPEj),
			{OPEj, 0, -3 + OPEm}, {OPEi, 0, OPEj}] -
			(-1)^OPEm DOT[SUNT[SUNIndex[c]] , SUNT[SUNIndex[d]]]*
			OPESum[(-1)^OPEj*Pair[del, pe1]^(-3 - OPEj + OPEm)*Pair[del, pe2]^OPEi*
			(Pair[del, pe2] +Pair[del, pe4])^(-OPEi + OPEj),
			{OPEj, 0, -3 + OPEm}, {OPEi, 0, OPEj}])]*Pair[muu, del]*Pair[nuu, del])
		];
		If[ exp===All,
			re = OPESumExplicit[re]
		];
		re
	];

FCPrint[1,"Twist2QuarkOperator.m loaded"];
End[]
