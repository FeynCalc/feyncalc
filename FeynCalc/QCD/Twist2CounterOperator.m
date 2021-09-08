(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Twist2CounterOperator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 13 April '98 at 19:22 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Twist2CounterOperator *)

(* ------------------------------------------------------------------------ *)

Twist2CounterOperator::usage =
"Twist2CounterOperator[p, mu, nu, a, b, 5] is a special routine for particular
QCD calculations.

Also available: Twist2CounterOperator[p, 7],
Twist2CounterOperator[p1,p2,{p3,mu,a}, 1] (p1: incoming quark momentum, p3:
incoming gluon (count1)).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Twist2CounterOperator`Private`"]

DeclareNonCommutative[Twist2CounterOperator];

(* if Fermion fields are involved a sign has to be
		put in front of the counterterms in order to cancel
		overlapping divergencies
	*)
FUDGESIGN = (-1);

Options[Twist2CounterOperator] =
{ CouplingConstant -> SMP["g_s"],
	Dimension -> D, Polarization -> 0, OPESumExplicit -> False
};

(* C7 *)
Twist2CounterOperator[pi_, 7, opt___Rule] :=
	Block[ {dim, p, re, pol, del, coup, li1,li2,li3},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		p = Momentum[pi, dim];
		del = Momentum[OPEDelta, dim];
		Which[pol === 0,
				re =  FUDGESIGN (1+(-1)^OPEm)/2 ( -2*CF*coup^2 *
					( -2/(-1 + OPEm) + 2/OPEm - (1 + OPEm)^(-1) ) *
					Sn*DiracGamma[del, dim]*
					Pair[del, p]^(-1 + OPEm)
							) / Epsilon
			,
					pol === 1,
			re = FUDGESIGN *
		(4*CF*coup^2* (1-(-1)^OPEm)/2 *
				(2/OPEm - (1 + OPEm)^(-1))*
				Sn*
				DOT[DiracGamma[del, dim] , DiracGamma[5]]*
				Pair[del, p]^(-1 + OPEm))/ Epsilon
			,
		(* no Chisholm *)
					pol === 2,
		li1 = LorentzIndex[Unique[$MU], dim];
		li2 = LorentzIndex[Unique[$MU], dim];
		li3 = LorentzIndex[Unique[$MU], dim];
		re =  FUDGESIGN (1-(-1)^OPEm)/2 *
			(2*I*CF*coup^2*Sn*Pair[del, p]^(-2 + OPEm)*
		(DOT[DiracGamma[li3] , DiracGamma[del] , DiracGamma[li2]]*
				Eps[li2, li3, del, p] -
			OPEm*DOT[DiracGamma[li3] , DiracGamma[del] , DiracGamma[li2]]*
				Eps[li2, li3, del, p] -
			DOT[DiracGamma[li3] , DiracGamma[li1] , DiracGamma[li2]]*
				Eps[del, li1, li2, li3]*Pair[del, p]))/(Epsilon*OPEm*(1 + OPEm))

			];
		re
	];

(* C1 *) (* COUNT1 *)
Twist2CounterOperator[p1_, pe3_, {_, mu_, a_}, 1, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, p3 = -pe3, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		oex    = OPESumExplicit /. {opt} /. Options[Twist2CounterOperator];

		(* count1 *)
		Which[pol === 0,
		ChangeDimension[
		FUDGESIGN *
		( ((1 + (-1)^OPEm)*(CA - 2*CF)*coup^3*
				(-2/(-1 + OPEm) + 2/OPEm - (1 + OPEm)^(-1))*Sn*
				DiracGamma[Momentum[OPEDelta]]*
				Pair[LorentzIndex[mu], Momentum[OPEDelta]]*
				(Pair[Momentum[OPEDelta], Momentum[p1]]^(-1 + OPEm)/
					(Pair[Momentum[OPEDelta], Momentum[p1]] -
						Pair[Momentum[OPEDelta], Momentum[p3]]) -
					Pair[Momentum[OPEDelta], Momentum[p3]]^(-1 + OPEm)/
					(Pair[Momentum[OPEDelta], Momentum[p1]] -
						Pair[Momentum[OPEDelta], Momentum[p3]]))*SUNT[SUNIndex[a]])/
			(2*Epsilon)
					),dim]
									,
					pol === 2,
		ChangeDimension[ FUDGESIGN count1[p1,p3,mu,a,SMP["g_s"]],dim],
					pol === 1,
		ChangeDimension[
		FUDGESIGN*(
		( (1 - (-1)^OPEm)*(CA - 2*CF)*coup^3*Sn)/Epsilon*
					OPESum[Pair[Momentum[OPEDelta], Momentum[p1]]^(OPEm-2-OPEi)*
								Pair[Momentum[OPEDelta], Momentum[p3]]^OPEi,
								{OPEi,0,OPEm-2}
								] * (
			(-2/OPEm + (1 + OPEm)^(-1))*
			SUNT[SUNIndex[a]] DOT[DiracGamma[Momentum[OPEDelta]] , DiracGamma[5]]*
			Pair[LorentzIndex[mu], Momentum[OPEDelta]]
										)
							), dim]
			]
	];

(* C2 *)
(* Count2 *)
Twist2CounterOperator[p1_, _, {p3_, mu_, a_}, 2, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, dp1, dp3, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		oex    = OPESumExplicit /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					dp1 = Pair[Momentum[OPEDelta, dim], Momentum[p1, dim]];
					dp3 = Pair[Momentum[OPEDelta, dim], Momentum[p3, dim]];
					re =   FUDGESIGN coup^3 (1+(-1)^OPEm)/2*
					DOT[SUNT[SUNIndex[a]], DiracGamma[Momentum[OPEDelta, dim], dim]]*
					(Sn CA *
					Pair[LorentzIndex[mu, dim], Momentum[OPEDelta, dim]]*
					(((-1 + OPEm)^(-1) - OPEm^(-1))*
						(-dp3)^(-2 + OPEm) +
					(-2/(-1 + OPEm) + 2/OPEm - (1 + OPEm)^(-1))*
						(If[ oex === True,
							dp1^(-1+OPEm)/(dp1+dp3) - (-dp3)^(-1+OPEm)/(dp1+dp3),
							OPESum[dp1^OPEi (-dp3)^(OPEm-OPEi-2), {OPEi,0,OPEm-2}]
						]
						)))/Epsilon
						,
					pol === 1,
					re = FUDGESIGN *
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

count1[p1_,pe3_,mu_,a_,coup_] :=
	Block[ {p3 = -pe3, A,b,c,d,e,f,g,i,m,pa1,pa2,pa3},
		A = Pair[LorentzIndex[mu], Momentum[OPEDelta]];
		b = Pair[Momentum[OPEDelta], Momentum[p1]];
		c = Pair[Momentum[OPEDelta], Momentum[p3]];
		d = Eps[LorentzIndex[mu], LorentzIndex[$MU[1]], LorentzIndex[$MU[2]],
			Momentum[OPEDelta]];
		e = Eps[LorentzIndex[$MU[1]], LorentzIndex[$MU[2]], LorentzIndex[$MU[3]],
			Momentum[OPEDelta]];
		f = Eps[LorentzIndex[$MU[1]], LorentzIndex[$MU[2]], Momentum[OPEDelta],
			Momentum[p1]];
		g = Eps[LorentzIndex[$MU[1]], LorentzIndex[$MU[2]],
			Momentum[OPEDelta], Momentum[p3]];
		i = OPEi;
		m = OPEm;
		pa1 = OPESum[b^(m-2-i) c^i,{i,0,m-2}] *
					(d DOT[DiracGamma[Momentum[OPEDelta]], DiracGamma[LorentzIndex[$MU[1]]],
					DiracGamma[LorentzIndex[$MU[2]]]]) (OPEm^(-1) - (1 + OPEm)^(-1));
		pa2 = A OPESum[b^(m-2-i) c^i,{i,0,m-2}] *
					( e* (OPEm^(-1) - (1 + OPEm)^(-1))*
				DOT[DiracGamma[LorentzIndex[$MU[1]]] , DiracGamma[LorentzIndex[$MU[2]]] ,
					DiracGamma[LorentzIndex[$MU[3]]]]);
		pa3 = ( (OPEm^(-1) - (1 + OPEm)^(-1))*
				DOT[DiracGamma[Momentum[OPEDelta]] , DiracGamma[LorentzIndex[$MU[1]]] ,
					DiracGamma[LorentzIndex[$MU[2]]]])*
					(-A OPESum[( (m-2-i) f + (i+1) g) b^(m-3-i) c^i,{i,0,m-3}]);
		-I*DOT[((1 - (-1)^OPEm)/2) , (CA - 2*CF)*coup^3*Sn ,(1/Epsilon),
		SUNT[SUNIndex[a]],
		(pa1 + pa2 + pa3)]
	];

(* C3 *)
(* Count3 *)
Twist2CounterOperator[p1_, _, {p3_, mu_, a_}, 3, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					FUDGESIGN * ChangeDimension[uc3[p1,-p3,mu,a,coup],dim],
					pol === 1,
					FUDGESIGN * ChangeDimension[pc3[p1,-p3,mu,a,coup],dim]
				]
	];

uc3[p1_, p3_, mu_, a_,cou_] :=
	Block[ {dp1, dp2, dp3, i = OPEi, m = OPEm},
		dp1 = Pair[Momentum[p1], Momentum[OPEDelta]];
		dp3 = Pair[Momentum[p3], Momentum[OPEDelta]];
		dp2 = dp1 - dp3;
		(
		-((1 + (-1)^OPEm)*CA*cou^3*Sn*DiracGamma[Momentum[OPEDelta]]*
					Pair[LorentzIndex[mu], Momentum[OPEDelta]]*SUNT[SUNIndex[a]])/
			(2*Epsilon)*
		(
		(1/(-1+m) - 1/m) dp3^(m-2) +
		(-2/(-1+m)+2/m-1/(1+m))*
		OPESum[(dp2)^i (-dp3)^(m-i-2),{i,0,m-2}]
		))
	];

pc3[p1_, p3_, mu_, a_,cou_] :=
	Block[ {dp1, dp2, dp3, i = OPEi, m = OPEm},
		dp1 = Pair[Momentum[p1], Momentum[OPEDelta]];
		dp3 = Pair[Momentum[p3], Momentum[OPEDelta]];
		dp2 = dp1 - dp3;
		(
		((1 - (-1)^OPEm)*CA*cou^3*(2 + OPEm)*Sn*
				DOT[DiracGamma[Momentum[OPEDelta]] , DiracGamma[5]]*
				Pair[LorentzIndex[mu], Momentum[OPEDelta]]*SUNT[SUNIndex[a]])/
			(Epsilon*OPEm*(1 + OPEm))*
		OPESum[(dp2)^i (-dp3)^(m-i-2),{i,0,m-2}]
		)
	];

(* C13 *)
(* Count13 *)
Twist2CounterOperator[p_, mu_, nu_, a_, b_, 13, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
						ChangeDimension[
			(-8*(1 + (-1)^OPEm)*coup^2*(2 - OPEm + OPEm^2)*Sn*Tf*FV[OPEDelta, nu]*
					FV[p, mu]*SD[a, b]*SO[p]^(-1 + OPEm))/
				(Epsilon*OPEm*(1 + OPEm)*(2 + OPEm)) -
			(8*(1 + (-1)^OPEm)*coup^2*(2 - OPEm + OPEm^2)*Sn*Tf*FV[OPEDelta, mu]*
					FV[p, nu]*SD[a, b]*SO[p]^(-1 + OPEm))/
				(Epsilon*OPEm*(1 + OPEm)*(2 + OPEm)) +
			(8*(1 + (-1)^OPEm)*coup^2*(2 + OPEm + OPEm^2)*Sn*Tf*MT[mu, nu]*
					SD[a, b]*SO[p]^OPEm)/(Epsilon*OPEm*(1 + OPEm)*(2 + OPEm)) +
			(8*(1 + (-1)^OPEm)*coup^2*(1 - OPEm)*(2 - OPEm)*Sn*Tf*FV[OPEDelta, mu]*
					FV[OPEDelta, nu]*SD[a, b]*SO[p]^(-2 + OPEm)*SP[p, p])/
				(Epsilon*OPEm*(1 + OPEm)*(2 + OPEm)),dim]
					,
					pol === 1,
						ChangeDimension[
		(8*I*(1 - (-1)^OPEm)*coup^2*(OPEm^(-1) - 2/(1 + OPEm))*Sn*Tf*
				SD[a, b]*SO[p]^(-1 + OPEm)*LC[mu, nu][OPEDelta, p])/Epsilon
											,dim]
					]
	];

(* C4 *)
(* Count4 *)
Twist2CounterOperator[p1_, _, {p3_, mu_, a_}, 4, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					FUDGESIGN * ChangeDimension[uc4[p1,-p3,mu,a,coup],dim],
					pol === 1,
					FUDGESIGN * ChangeDimension[pc4[p1,-p3,mu,a,coup],dim]
				]
	];

uc4[p1_, p3_, mu_, a_,cou_] :=
	Block[ {dp1, dp2, dp3, i = OPEi, m = OPEm},
		dp1 = Pair[Momentum[p1], Momentum[OPEDelta]];
		dp3 = Pair[Momentum[p3], Momentum[OPEDelta]];
		dp2 = dp1 - dp3;
		cou^3 Sn/Epsilon (1+(-1)^m)/2 *
		CA*DiracGamma[Momentum[OPEDelta]]*
			Pair[LorentzIndex[mu], Momentum[OPEDelta]]*SUNT[SUNIndex[a]]*
		(- ( (2/m-2/(m-1)-1/(m+1)) (
		OPESum[dp3^(i-1) dp1^(m-1-i), {i,0,m-1}] +
		OPESum[dp3^(i-1) (-dp2)^(m-1-i),{i,0,m-1}]) +
			(4/(m-1) - 4/m) dp3^(m-2)))
	];

pc4[p1_, p3_, mu_, a_,cou_] :=
	Block[ {dp1,dp3},
		dp1 = Pair[Momentum[p1], Momentum[OPEDelta]];
		dp3 = Pair[Momentum[p3], Momentum[OPEDelta]];
		((1 - (-1)^OPEm)*CA*cou^3*Sn*
			DOT[DiracGamma[Momentum[OPEDelta]] , DiracGamma[5]]*
			Pair[LorentzIndex[mu], Momentum[OPEDelta]]*SUNT[SUNIndex[a]])/Epsilon*
			(2/OPEm - 1/(OPEm+1)) *
		(OPESum[dp1^OPEi dp3^(OPEm-2-OPEi), {OPEi,0,OPEm-2}] -
			OPESum[(dp3-dp1)^OPEi dp3^(OPEm-2-OPEi), {OPEi,0,OPEm-2}] +
			OPESum[(dp1-dp3)^OPEi dp1^(OPEm-2-OPEi), {OPEi,0,OPEm-2}]
		)
	];

(* FUDGESIGN is highly weird here ... *)
(* C5 *)
(* Count5 *)
Twist2CounterOperator[p_, mu_, nu_, a_, b_, 5, opt___Rule] :=
	Block[ {pol,coup,dim},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
						FUDGESIGN * ChangeDimension[uc5[p,mu,nu,a,b, coup],dim],
					pol === 1,
						ChangeDimension[pc5[p,mu,nu,a,b, coup],dim]
				]
	];

uc5[ p_, mu_, nu_, a_, b_, cou_] :=
	Block[ {m = OPEm, gmn = Pair[LorentzIndex[mu], LorentzIndex[nu]],
	dm = Pair[LorentzIndex[mu], Momentum[OPEDelta]],
	pm = Pair[LorentzIndex[mu], Momentum[p]],
	dn = Pair[LorentzIndex[nu], Momentum[OPEDelta]],
	pn = Pair[LorentzIndex[nu], Momentum[p]],
	dp = Pair[Momentum[OPEDelta], Momentum[p]],
	p2 = Pair[Momentum[p], Momentum[p]]},
		((1 + (-1)^m)*CA*cou^2*(dp^m*gmn*
						(-8/(-1 + m) + 8/m - 12/(1 + m) + 8/(2 + m)) +
					dp^(-1 + m)*(6/(-1 + m) - 6/m + 16/(1 + m) - 16/(2 + m))*
						(dn*pm + dm*pn) + dm*dn*dp^(-2 + m)*
						(-4/(-1 + m) + 8/m - 24/(1 + m) + 24/(2 + m))*p2)*Sn*
				SUNDelta[SUNIndex[a], SUNIndex[b]])/(2*Epsilon)
	];

pc5[ p_, mu_, nu_, a_, b_, cou_] :=
	(
	(-4*I*(1 - (-1)^OPEm)*CA*cou^2*(4/OPEm - 3/(1 + OPEm))*Sn*
	Eps[LorentzIndex[mu], LorentzIndex[nu], Momentum[OPEDelta],
	Momentum[p]]*Pair[Momentum[OPEDelta], Momentum[p]]^(-1 + OPEm)*
	SUNDelta[SUNIndex[a], SUNIndex[b]])/Epsilon);

(* C6 *)
(* Count6 *)
Twist2CounterOperator[p_, mu_, nu_, a_, b_, 6, opt___Rule] :=
	Block[ {pol,coup,dim},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
						FUDGESIGN * ChangeDimension[uc6[p,mu,nu,a,b, coup],dim],
					pol === 1,
												ChangeDimension[pc6[p,mu,nu,a,b, coup],dim]
				]
	];

pc6[ p_, mu_, nu_, a_, b_, _] :=
	(
	(2*I*(1 - (-1)^OPEm)*CA*SMP["g_s"]^2*Sn*
	Eps[LorentzIndex[mu], LorentzIndex[nu], Momentum[OPEDelta], Momentum[p]]*
	Pair[Momentum[OPEDelta], Momentum[p]]^(-1 + OPEm)*
	((1 + OPEm + 2*EulerGamma*OPEm - OPEm^2 + 2*EulerGamma*OPEm^2 +
	OPEm*PolyGamma[0, OPEm] + OPEm^2*PolyGamma[0, OPEm])/
	(OPEm*(1 + OPEm)) + PolyGamma[0, 1 + OPEm])*
	SUNDelta[SUNIndex[a], SUNIndex[b]])/Epsilon);

uc6[ p_, mu_, nu_, a_, b_, cou_] :=
	Block[ {m = OPEm, gmn = Pair[LorentzIndex[mu], LorentzIndex[nu]],
	dm = Pair[LorentzIndex[mu], Momentum[OPEDelta]],
	pm = Pair[LorentzIndex[mu], Momentum[p]],
	dn = Pair[LorentzIndex[nu], Momentum[OPEDelta]],
	pn = Pair[LorentzIndex[nu], Momentum[p]],
	dp = Pair[Momentum[OPEDelta], Momentum[p]],
	p2 = Pair[Momentum[p], Momentum[p]]},
		(-1*(1 + (-1)^OPEm)*CA*cou^2*Sn*
				(dn*dp^(-1 + OPEm)*pm*(2 + 2*OPEm + 2*EulerGamma*OPEm - OPEm^2 +
							2*EulerGamma*OPEm^2 + 2*OPEm*PolyGamma[0, OPEm] +
							2*OPEm^2*PolyGamma[0, OPEm]) -
					dp^OPEm*Pair[LorentzIndex[mu], LorentzIndex[nu]]*
						(2 + 2*OPEm + 2*EulerGamma*OPEm - OPEm^2 + 2*EulerGamma*OPEm^2 +
							2*OPEm*PolyGamma[0, OPEm] + 2*OPEm^2*PolyGamma[0, OPEm]) -
					(dm*dn*dp^(-2 + OPEm)*p2*
							(2 + 5*OPEm + 4*EulerGamma*OPEm - 2*OPEm^2 +
								6*EulerGamma*OPEm^2 - OPEm^3 + 2*EulerGamma*OPEm^3 +
								4*OPEm*PolyGamma[0, OPEm] + 6*OPEm^2*PolyGamma[0, OPEm] +
								2*OPEm^3*PolyGamma[0, OPEm]))/(2 + OPEm) +
					(dm*dp^(-1 + OPEm)*pn*(4 + 6*OPEm + 4*EulerGamma*OPEm - 2*OPEm^2 +
								6*EulerGamma*OPEm^2 - OPEm^3 + 2*EulerGamma*OPEm^3 +
								4*OPEm*PolyGamma[0, OPEm] + 6*OPEm^2*PolyGamma[0, OPEm] +
								2*OPEm^3*PolyGamma[0, OPEm]))/(2 + OPEm))*
				SUNDelta[SUNIndex[a], SUNIndex[b]])/(Epsilon*OPEm*(1 + OPEm))
	];


(*C11 *)
(* Count11 *)
Twist2CounterOperator[p1_, mu1_, a1_,  (*p2*)_, mu2_, a2_,  pe3_, mu3_, a3_, 11, x_,  opt___Rule] :=
	Block[ {dim, re, pol, p3, coup},
		(* p3 = p1+p2 *)
		p3 = -pe3;
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
		ChangeDimension[
		(
		(I*CA*coup^3*Sn*(1+Power2[-1,OPEm])*
		(FV[OPEDelta,mu3]*MT[mu1,mu2]*
		(((30+5*OPEm+31*OPEm^2+6*OPEm^3)*
		Power2[SO[p1]-SO[p3],-1+OPEm])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2))-
		((1+5*OPEm+2*OPEm^2)*Power2[SO[p1],OPEm])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3]))-
		((22+9*OPEm+15*OPEm^2+2*OPEm^3)*Power2[SO[p3],OPEm])/
		(2*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3]))+
		((14-3*OPEm+11*OPEm^2+2*OPEm^3)*
		Power2[SO[p3],1+OPEm])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3]))+
		(6*(1+OPEm)*Power2[SO[p3],-1+OPEm]*SO[p1])/
		(OPEm*(1-OPEm^2)*(SO[p1]-SO[p3]))-
		(4*(1+OPEm)*Power2[SO[p3],-2+OPEm]*SO[p1]^2)/
		(OPEm*(1-OPEm^2)*(SO[p1]-SO[p3]))-
		((4+2*OPEm+5*OPEm^2+OPEm^3)*
		Power2[SO[p1]-SO[p3],-1+OPEm]*SO[p1])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p3])+
		((4+2*OPEm+5*OPEm^2+OPEm^3)*Power2[SO[p1],1+OPEm])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])*SO[p3])-
		((14-3*OPEm+11*OPEm^2+2*OPEm^3)*
		Power2[SO[p1]-SO[p3],-1+OPEm]*SO[p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]))+
		FV[OPEDelta,mu1]*FV[OPEDelta,mu2]*FV[p3,mu3]*
		(-((2-OPEm)*(1+3*OPEm)*Power2[SO[p1]-SO[p3],-2+OPEm])/
		(4*OPEm*(1-OPEm^2))+
		((9-OPEm-4*OPEm^2)*Power2[SO[p1],OPEm])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((1+13*OPEm-12*OPEm^2)*Power2[SO[p3],OPEm])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)-
		((4+5*OPEm-5*OPEm^2)*Power2[SO[p3],1+OPEm])/
		(4*OPEm*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3])^2)-
		((5+10*OPEm-7*OPEm^2)*Power2[SO[p3],-1+OPEm]*SO[p1])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((4+5*OPEm-5*OPEm^2)*Power2[SO[p1]-SO[p3],-2+OPEm]*
		SO[p3])/(4*OPEm*(1-OPEm^2)*SO[p1])-
		((1-4*OPEm)*(1+OPEm)*Power2[SO[p1],-1+OPEm]*SO[p3])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2))+
		FV[OPEDelta,mu1]*FV[OPEDelta,mu3]*FV[p3,mu2]*
		(-((16+34*OPEm-13*OPEm^2-OPEm^3)*
		Power2[SO[p1]-SO[p3],-2+OPEm])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2))+
		((13-5*OPEm-2*OPEm^2)*Power2[SO[p1],OPEm])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((28+156*OPEm-49*OPEm^2-22*OPEm^3-5*OPEm^4)*
		Power2[SO[p3],OPEm])/
		(8*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2)-
		((8+22*OPEm-5*OPEm^2-OPEm^3)*Power2[SO[p3],1+OPEm])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3])^2)-
		((42+71*OPEm-21*OPEm^2-21*OPEm^3-5*OPEm^4)*
		Power2[SO[p3],-1+OPEm]*SO[p1])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((1+OPEm)*(20-7*OPEm-5*OPEm^2)*
		Power2[SO[p3],-2+OPEm]*SO[p1]^2)/
		(8*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((2-OPEm)*(1+2*OPEm)*Power2[SO[p1]-SO[p3],-2+OPEm]*
		SO[p1])/(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p3])-
		((2-OPEm)*(1+2*OPEm)*Power2[SO[p1],1+OPEm])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2*SO[p3])+
		((8+22*OPEm-5*OPEm^2-OPEm^3)*
		Power2[SO[p1]-SO[p3],-2+OPEm]*SO[p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1])-
		((1-2*OPEm)*(1+OPEm)*Power2[SO[p1],-1+OPEm]*SO[p3])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2))+
		FV[OPEDelta,mu2]*FV[OPEDelta,mu3]*FV[p3,mu1]*
		(((14+5*OPEm+15*OPEm^2+2*OPEm^3)*
		Power2[SO[p1]-SO[p3],-2+OPEm])/
		(2*OPEm*(2+OPEm)*(1-OPEm^2))-
		((11+OPEm)*Power2[SO[p1],OPEm])/
		(4*(1-OPEm^2)*(SO[p1]-SO[p3])^2)-
		((128+34*OPEm+73*OPEm^2-14*OPEm^3-5*OPEm^4)*
		Power2[SO[p3],OPEm])/
		(8*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((2-OPEm+4*OPEm^2+OPEm^3)*Power2[SO[p3],1+OPEm])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3])^2)+
		((78+83*OPEm+22*OPEm^2-22*OPEm^3-5*OPEm^4)*
		Power2[SO[p3],-1+OPEm]*SO[p1])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2)-
		((1+OPEm)*(44-7*OPEm-5*OPEm^2)*
		Power2[SO[p3],-2+OPEm]*SO[p1]^2)/
		(8*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)-
		((10+7*OPEm+7*OPEm^2)*Power2[SO[p1]-SO[p3],-2+OPEm]*
		SO[p1])/(2*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p3])+
		((10+7*OPEm+7*OPEm^2)*Power2[SO[p1],1+OPEm])/
		(2*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2*SO[p3])-
		((2-OPEm+4*OPEm^2+OPEm^3)*
		Power2[SO[p1]-SO[p3],-2+OPEm]*SO[p3])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1])+
		((1+OPEm)^2*Power2[SO[p1],-1+OPEm]*SO[p3])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2))+
		FV[OPEDelta,mu1]*MT[mu2,mu3]*
		(((1-OPEm)*(2-OPEm)*Power2[SO[p1]-SO[p3],-1+OPEm])/
		(4*OPEm*(1-OPEm^2))+
		((1-OPEm)*(5+2*OPEm)*Power2[SO[p1],OPEm])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3]))+
		((4+15*OPEm-18*OPEm^2-5*OPEm^3)*Power2[SO[p3],OPEm])/
		(8*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3]))-
		(3*(1-OPEm)*Power2[SO[p3],1+OPEm])/
		(4*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3]))-
		((12+5*OPEm)*Power2[SO[p3],-1+OPEm]*SO[p1])/
		(8*OPEm*(SO[p1]-SO[p3]))+
		(3*(1-OPEm)*Power2[SO[p1]-SO[p3],-1+OPEm]*SO[p3])/
		(4*(1-OPEm^2)*SO[p1])-
		((1-2*OPEm)*(1+OPEm)*Power2[SO[p1],-1+OPEm]*SO[p3])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])))+
		FV[OPEDelta,mu2]*MT[mu1,mu3]*
		(((1-OPEm)*(5+2*OPEm)*Power2[SO[p1]-SO[p3],-1+OPEm])/
		(4*OPEm*(1-OPEm^2))+
		((1-OPEm)*(2-OPEm)*Power2[SO[p1],OPEm])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3]))-
		((20-3*OPEm-4*OPEm^2-5*OPEm^3)*Power2[SO[p3],OPEm])/
		(8*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3]))+
		((1-2*OPEm)*(1-OPEm)*Power2[SO[p3],1+OPEm])/
		(4*OPEm*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3]))+
		((12+5*OPEm)*Power2[SO[p3],-1+OPEm]*SO[p1])/
		(8*OPEm*(SO[p1]-SO[p3]))-
		((1-2*OPEm)*(1-OPEm)*Power2[SO[p1]-SO[p3],-1+OPEm]*
		SO[p3])/(4*OPEm*(1-OPEm^2)*SO[p1])+
		((1+OPEm)^2*Power2[SO[p1],-1+OPEm]*SO[p3])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])))+
		FV[OPEDelta,mu2]*FV[OPEDelta,mu3]*FV[p1,mu1]*
		((-3*(6-3*OPEm+7*OPEm^2)*Power2[SO[p1]-SO[p3],-2+OPEm])/
		(2*OPEm*(2+OPEm)*(1-OPEm^2))-
		((14-13*OPEm-24*OPEm^2-OPEm^3)*Power2[SO[p1],OPEm])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((62+17*OPEm+25*OPEm^2+4*OPEm^3)*Power2[SO[p3],OPEm])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((4-2*OPEm+OPEm^2)*Power2[SO[p3],2+OPEm])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]^2*(SO[p1]-SO[p3])^2)-
		((5+OPEm)*(2-OPEm+OPEm^2)*Power2[SO[p3],1+OPEm])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3])^2)-
		(5*(1+OPEm)*Power2[SO[p3],-1+OPEm]*SO[p1])/
		(OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		(3*(1+OPEm)*Power2[SO[p3],-2+OPEm]*SO[p1]^2)/
		(OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((6+OPEm+11*OPEm^2)*Power2[SO[p1]-SO[p3],-2+OPEm]*
		SO[p1])/(2*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p3])-
		((6+OPEm+11*OPEm^2)*Power2[SO[p1],1+OPEm])/
		(2*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2*SO[p3])+
		((10-7*OPEm+6*OPEm^2)*Power2[SO[p1]-SO[p3],-2+OPEm]*
		SO[p3])/(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1])+
		((2-5*OPEm-OPEm^2)*Power2[SO[p1],-1+OPEm]*SO[p3])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)-
		((4-2*OPEm+OPEm^2)*Power2[SO[p1]-SO[p3],-2+OPEm]*
		SO[p3]^2)/(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]^2))+
		FV[OPEDelta,mu1]*FV[OPEDelta,mu3]*FV[p1,mu2]*
		(-((50-7*OPEm+42*OPEm^2-OPEm^3)*
		Power2[SO[p1]-SO[p3],-2+OPEm])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2))+
		(6*(1+OPEm)*Power2[SO[p1],OPEm])/
		((2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((86+53*OPEm+37*OPEm^2+4*OPEm^3)*Power2[SO[p3],OPEm])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((22-3*OPEm+5*OPEm^2)*Power2[SO[p3],2+OPEm])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]^2*(SO[p1]-SO[p3])^2)\
		-((30+5*OPEm+11*OPEm^2+2*OPEm^3)*Power2[SO[p3],1+OPEm])/
		(2*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3])^2)-
		(7*(1+OPEm)*Power2[SO[p3],-1+OPEm]*SO[p1])/
		(OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		(3*(1+OPEm)*Power2[SO[p3],-2+OPEm]*SO[p1]^2)/
		(OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((6+OPEm+11*OPEm^2)*Power2[SO[p1]-SO[p3],-2+OPEm]*
		SO[p1])/(2*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p3])-
		((6+OPEm+11*OPEm^2)*Power2[SO[p1],1+OPEm])/
		(2*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^2*SO[p3])+
		((60-12*OPEm+25*OPEm^2-OPEm^3)*
		Power2[SO[p1]-SO[p3],-2+OPEm]*SO[p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1])-
		((1+3*OPEm)*Power2[SO[p1],-1+OPEm]*SO[p3])/
		(2*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)-
		((22-3*OPEm+5*OPEm^2)*Power2[SO[p1]-SO[p3],-2+OPEm]*
		SO[p3]^2)/(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]^2))+
		FV[OPEDelta,mu1]*FV[OPEDelta,mu2]*FV[p1,mu3]*
		(-((1-OPEm)*(7+OPEm)*Power2[SO[p1]-SO[p3],-2+OPEm])/
		(4*OPEm*(1-OPEm^2))-
		((1-OPEm)*(7+OPEm)*Power2[SO[p1],OPEm])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((13+OPEm)*Power2[SO[p3],OPEm])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)+
		((5-OPEm)*Power2[SO[p3],2+OPEm])/
		(4*OPEm*(1-OPEm^2)*SO[p1]^2*(SO[p1]-SO[p3])^2)-
		((5-OPEm)*Power2[SO[p3],1+OPEm])/
		(2*OPEm*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3])^2)+
		((2-OPEm)*(5-OPEm)*Power2[SO[p1]-SO[p3],-2+OPEm]*
		SO[p3])/(4*OPEm*(1-OPEm^2)*SO[p1])-
		((1+4*OPEm)*Power2[SO[p1],-1+OPEm]*SO[p3])/
		(2*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^2)-
		((5-OPEm)*Power2[SO[p1]-SO[p3],-2+OPEm]*SO[p3]^2)/
		(4*OPEm*(1-OPEm^2)*SO[p1]^2)+
		(Power2[SO[p1],-2+OPEm]*SO[p3]^2)/
		(4*OPEm*(SO[p1]-SO[p3])^2))+
		FV[OPEDelta,mu1]*FV[OPEDelta,mu2]*FV[OPEDelta,mu3]*
		(((84-40*OPEm+45*OPEm^2-11*OPEm^3)*
		Power2[SO[p1]-SO[p3],-3+OPEm]*SP[p1,p1])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2))+
		((20-8*OPEm-35*OPEm^2+5*OPEm^3)*Power2[SO[p1],OPEm]*
		SP[p1,p1])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3)-
		((66+43*OPEm+25*OPEm^2+4*OPEm^3)*Power2[SO[p3],OPEm]*
		SP[p1,p1])/
		(2*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3)+
		((10+3*OPEm+2*OPEm^2)*Power2[SO[p3],3+OPEm]*SP[p1,p1])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]^3*(SO[p1]-SO[p3])^3)-
		((130+43*OPEm+33*OPEm^2+4*OPEm^3)*
		Power2[SO[p3],2+OPEm]*SP[p1,p1])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]^2*(SO[p1]-SO[p3])^3)+
		((158+69*OPEm+55*OPEm^2+12*OPEm^3)*
		Power2[SO[p3],1+OPEm]*SP[p1,p1])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3])^3)+
		(5*(1+OPEm)*Power2[SO[p3],-1+OPEm]*SO[p1]*SP[p1,p1])/
		(OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^3)-
		(2*(1+OPEm)*Power2[SO[p3],-2+OPEm]*SO[p1]^2*SP[p1,p1])/
		(OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^3)-
		((4-2*OPEm+5*OPEm^2-OPEm^3)*
		Power2[SO[p1]-SO[p3],-3+OPEm]*SO[p1]*SP[p1,p1])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p3])+
		((4-2*OPEm+5*OPEm^2-OPEm^3)*Power2[SO[p1],1+OPEm]*
		SP[p1,p1])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3*SO[p3])-
		((158-41*OPEm+38*OPEm^2-11*OPEm^3)*
		Power2[SO[p1]-SO[p3],-3+OPEm]*SO[p3]*SP[p1,p1])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1])+
		((2+31*OPEm+23*OPEm^2-2*OPEm^3)*
		Power2[SO[p1],-1+OPEm]*SO[p3]*SP[p1,p1])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3)+
		((130+3*OPEm+21*OPEm^2-4*OPEm^3)*
		Power2[SO[p1]-SO[p3],-3+OPEm]*SO[p3]^2*SP[p1,p1])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]^2)+
		((1-OPEm)^2*Power2[SO[p1],-2+OPEm]*SO[p3]^2*SP[p1,p1])/
		(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^3)-
		((10+3*OPEm+2*OPEm^2)*Power2[SO[p1]-SO[p3],-3+OPEm]*
		SO[p3]^3*SP[p1,p1])/(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]^3)\
		-((10-OPEm)*(1-OPEm+OPEm^2)*Power2[SO[p1]-SO[p3],-3+OPEm]*
		SP[p1,p3])/(OPEm*(2+OPEm)*(1-OPEm^2))-
		((52-16*OPEm-31*OPEm^2+OPEm^3)*Power2[SO[p1],OPEm]*
		SP[p1,p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3)+
		((130-13*OPEm+17*OPEm^2-8*OPEm^3)*Power2[SO[p3],OPEm]*
		SP[p1,p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3)+
		((2-5*OPEm)*Power2[SO[p3],2+OPEm]*SP[p1,p3])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]^2*(SO[p1]-SO[p3])^3)-
		((8-12*OPEm+OPEm^2)*Power2[SO[p3],1+OPEm]*SP[p1,p3])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3])^3)-
		((42+15*OPEm+5*OPEm^2-8*OPEm^3)*
		Power2[SO[p3],-1+OPEm]*SO[p1]*SP[p1,p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3)+
		(2*(1+OPEm)*Power2[SO[p3],-2+OPEm]*SO[p1]^2*SP[p1,p3])/
		(OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^3)+
		((4-2*OPEm+5*OPEm^2-OPEm^3)*
		Power2[SO[p1]-SO[p3],-3+OPEm]*SO[p1]*SP[p1,p3])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p3])-
		((4-2*OPEm+5*OPEm^2-OPEm^3)*Power2[SO[p1],1+OPEm]*
		SP[p1,p3])/
		(OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3*SO[p3])+
		(2*(4-3*OPEm)*(1-OPEm)*Power2[SO[p1]-SO[p3],-3+OPEm]*
		SO[p3]*SP[p1,p3])/(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1])-
		((7+21*OPEm+2*OPEm^2)*Power2[SO[p1],-1+OPEm]*SO[p3]*
		SP[p1,p3])/(4*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^3)-
		((2-5*OPEm)*Power2[SO[p1]-SO[p3],-3+OPEm]*SO[p3]^2*
		SP[p1,p3])/(OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]^2)+
		(Power2[SO[p1],-2+OPEm]*SO[p3]^2*SP[p1,p3])/
		(4*OPEm*(SO[p1]-SO[p3])^3)-
		((10+33*OPEm-16*OPEm^2-3*OPEm^3)*
		Power2[SO[p1]-SO[p3],-3+OPEm]*SP[p3,p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2))+
		((11-OPEm-2*OPEm^2)*Power2[SO[p1],OPEm]*SP[p3,p3])/
		(2*OPEm*(1-OPEm^2)*(SO[p1]-SO[p3])^3)-
		((26-79*OPEm+36*OPEm^2+5*OPEm^3)*Power2[SO[p3],OPEm]*
		SP[p3,p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3)-
		((6+19*OPEm-10*OPEm^2-3*OPEm^3)*Power2[SO[p3],1+OPEm]*
		SP[p3,p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1]*(SO[p1]-SO[p3])^3)-
		((14+107*OPEm-32*OPEm^2-5*OPEm^3)*
		Power2[SO[p3],-1+OPEm]*SO[p1]*SP[p3,p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3)+
		((6+35*OPEm-14*OPEm^2-3*OPEm^3)*
		Power2[SO[p3],-2+OPEm]*SO[p1]^2*SP[p3,p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3)+
		((2+7*OPEm-3*OPEm^2)*Power2[SO[p1]-SO[p3],-3+OPEm]*
		SO[p1]*SP[p3,p3])/(2*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p3])-
		((2+7*OPEm-3*OPEm^2)*Power2[SO[p1],1+OPEm]*SP[p3,p3])/
		(2*OPEm*(2+OPEm)*(1-OPEm^2)*(SO[p1]-SO[p3])^3*SO[p3])+
		((6+19*OPEm-10*OPEm^2-3*OPEm^3)*
		Power2[SO[p1]-SO[p3],-3+OPEm]*SO[p3]*SP[p3,p3])/
		(4*OPEm*(2+OPEm)*(1-OPEm^2)*SO[p1])+
		((1+OPEm)*Power2[SO[p1],-1+OPEm]*SO[p3]*SP[p3,p3])/
		((1-OPEm^2)*(SO[p1]-SO[p3])^3)))*
		SUNF[SUNIndex[a1],SUNIndex[a2],SUNIndex[a3]])/Epsilon+
		(I*FCGV["ALIEN"]*CA*SMP["g_s"]^3*Sn*(1+Power2[-1,OPEm])*
		(FV[OPEDelta,mu1]*FV[OPEDelta,mu2]*FV[p1,mu3]*
		OPESum[{OPEi,0,-3+OPEm}]*
		(-(Power2[-1,OPEi]*Power2[1-x,1+OPEi]*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/2+
		Power2[-1,OPEi]*Power2[1-x,1+OPEi]*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1]-
		(Power2[-1,OPEi]*Power2[1-x,1+OPEi]*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/2-
		(3*(2+OPEi-x-OPEi*x)*Power2[x,1+OPEi]*
		Power2[SO[p3],OPEm])/
		(4*(1+OPEi)*(2+OPEi)*(SO[p1]-SO[p3])^2)-
		((4+2*OPEi-3*x-3*OPEi*x)*Power2[x,1+OPEi]*
		Power2[SO[p1],2+OPEi]*Power2[SO[p3],-2-OPEi+OPEm])/
		(4*(2+OPEi)*(SO[p1]-SO[p3])^2)+
		((4+OPEi-3*x-3*OPEi*x)*Power2[x,1+OPEi]*
		Power2[SO[p1],1+OPEi]*Power2[SO[p3],-1-OPEi+OPEm])/
		(4*(1+OPEi)*(SO[p1]-SO[p3])^2)+
		(Power2[x,1+OPEi]*Power2[SO[p1],OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/(4*(SO[p1]-SO[p3])^2)-
		((1-x)*Power2[-1,OPEi]*
		Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/(2*(SO[p1]-SO[p3])^2)-
		((1-x)*Power2[SO[p3],-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi])/
		(2*(SO[p1]-SO[p3])^2)+
		((1-x)*Power2[-1,OPEi]*
		Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1])/(SO[p1]-SO[p3])^2\
+((1-x)*Power2[SO[p3],-1-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1])/
		(SO[p1]-SO[p3])^2-
		((1-x)*Power2[-1,OPEi]*
		Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])^2)-
		((1-x)*Power2[SO[p3],-2-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])^2))+
		FV[OPEDelta,mu1]*FV[OPEDelta,mu2]*FV[p3,mu3]*
		OPESum[{OPEi,0,-3+OPEm}]*
		((Power2[-1,OPEi]*(Power2[1-x,OPEi]-
		x*Power2[1-x,OPEi]-6*Power2[x,1+OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/2+
		(Power2[-1,OPEi]*Power2[x,1+OPEi]*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],1-OPEi+OPEm])/SO[p1]-
		Power2[-1,OPEi]*(Power2[1-x,OPEi]-
		x*Power2[1-x,OPEi]-3*Power2[x,1+OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1]+
		(Power2[-1,OPEi]*(Power2[1-x,OPEi]-
		x*Power2[1-x,OPEi]-2*Power2[x,1+OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/2+
		(3*(8+10*OPEi+3*OPEi^2+x+2*OPEi*x+OPEi^2*x)*
		Power2[x,1+OPEi]*Power2[SO[p3],OPEm])/
		(4*(1+OPEi)*(2+OPEi)*(SO[p1]-SO[p3])^2)+
		((8+4*OPEi+3*x)*Power2[x,1+OPEi]*
		Power2[SO[p1],2+OPEi]*Power2[SO[p3],-2-OPEi+OPEm])/
		(4*(2+OPEi)*(SO[p1]-SO[p3])^2)-
		((7+4*OPEi)*Power2[x,1+OPEi]*Power2[SO[p1],1+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm])/
		(4*(1+OPEi)*(SO[p1]-SO[p3])^2)+
		(Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/(2*(SO[p1]-SO[p3])^2)-
		(x*Power2[SO[p3],-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi])/
		(2*(SO[p1]-SO[p3])^2)-
		(Power2[x,1+OPEi]*Power2[SO[p3],1+OPEm])/
		(SO[p1]*(SO[p1]-SO[p3])^2)-
		((5+3*x)*Power2[x,1+OPEi]*Power2[SO[p3],-1+OPEm]*
		SO[p1])/(4*(SO[p1]-SO[p3])^2)-
		(Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1])/(SO[p1]-SO[p3])^2\
+(x*Power2[SO[p3],-1-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1])/
		(SO[p1]-SO[p3])^2+
		(Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])^2)-
		(x*Power2[SO[p3],-2-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])^2))+
		FV[OPEDelta,mu1]*FV[OPEDelta,mu3]*FV[p3,mu2]*
		OPESum[{OPEi,0,-3+OPEm}]*
		((Power2[-1,OPEi]*(3*Power2[1-x,OPEi]-
		2*x*Power2[1-x,OPEi]+3*Power2[x,OPEi]-
		16*Power2[x,1+OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/4-
		((1-4*x)*Power2[-1,OPEi]*Power2[x,OPEi]*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],1-OPEi+OPEm])/(4*SO[p1])-
		(3*Power2[-1,OPEi]*
		(3*Power2[1-x,OPEi]-2*x*Power2[1-x,OPEi]+
		Power2[x,OPEi]-8*Power2[x,1+OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1])/4+
		(Power2[-1,OPEi]*(9*Power2[1-x,OPEi]-
		6*x*Power2[1-x,OPEi]+Power2[x,OPEi]-
		16*Power2[x,1+OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/4-
		(Power2[-1,OPEi]*(3*Power2[1-x,OPEi]-
		2*x*Power2[1-x,OPEi]-4*Power2[x,1+OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*SO[p1]^3)/4-
		((6+9*OPEi+3*OPEi^2-22*x-31*OPEi*x-10*OPEi^2*x-
		5*x^2-8*OPEi*x^2-3*OPEi^2*x^2)*Power2[x,OPEi]*
		Power2[SO[p3],OPEm])/
		(4*(1+OPEi)*(2+OPEi)*(SO[p1]-SO[p3])^2)+
		((1-5*x+2*x^2)*Power2[x,OPEi]*Power2[SO[p1],3+OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm])/(4*(SO[p1]-SO[p3])^2)-
		((2+OPEi-14*x-7*OPEi*x+3*x^2+2*OPEi*x^2)*
		Power2[x,OPEi]*Power2[SO[p1],2+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm])/
		(4*(2+OPEi)*(SO[p1]-SO[p3])^2)-
		((3+2*OPEi)*Power2[x,1+OPEi]*Power2[SO[p1],1+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm])/
		(4*(1+OPEi)*(SO[p1]-SO[p3])^2)+
		(Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/(4*(SO[p1]-SO[p3])^2)+
		((2-x)*x*Power2[SO[p3],-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi])/
		(4*(SO[p1]-SO[p3])^2)+
		((1-4*x)*Power2[x,OPEi]*Power2[SO[p3],1+OPEm])/
		(4*SO[p1]*(SO[p1]-SO[p3])^2)+
		((4-12*x-3*x^2)*Power2[x,OPEi]*Power2[SO[p3],-1+OPEm]*
		SO[p1])/(4*(SO[p1]-SO[p3])^2)-
		((3-2*x)*Power2[-1,OPEi]*
		Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1])/
		(4*(SO[p1]-SO[p3])^2)-
		(3*(2-x)*x*Power2[SO[p3],-1-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1])/
		(4*(SO[p1]-SO[p3])^2)-
		((1-3*x)*Power2[x,OPEi]*Power2[SO[p3],-2+OPEm]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])^2)+
		((3-4*x)*Power2[-1,OPEi]*
		Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/
		(4*(SO[p1]-SO[p3])^2)+
		(3*(2-x)*x*Power2[SO[p3],-2-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^2)/
		(4*(SO[p1]-SO[p3])^2)-
		((1-2*x)*Power2[-1,OPEi]*
		Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*SO[p1]^3)/
		(4*(SO[p1]-SO[p3])^2)-
		((2-x)*x*Power2[SO[p3],-3-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^3)/
		(4*(SO[p1]-SO[p3])^2))+
		FV[OPEDelta,mu2]*FV[OPEDelta,mu3]*FV[p3,mu1]*
		OPESum[{OPEi,0,-3+OPEm}]*
		(-((1-x)*Power2[-1,OPEi]*Power2[x,OPEi]*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-OPEi+OPEm])+
		Power2[-1,OPEi]*(Power2[1-x,OPEi]+2*Power2[x,OPEi]-
		3*Power2[x,1+OPEi])*Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1]-
		Power2[-1,OPEi]*(2*Power2[1-x,OPEi]+Power2[x,OPEi]-
		3*Power2[x,1+OPEi])*Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2+
		Power2[-1,OPEi]*(Power2[1-x,OPEi]-Power2[x,1+OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*SO[p1]^3+
		((8+12*OPEi+4*OPEi^2-6*x-13*OPEi*x-5*OPEi^2*x+
		4*x^2+7*OPEi*x^2+3*OPEi^2*x^2)*Power2[x,OPEi]*
		Power2[SO[p3],OPEm])/
		(4*(1+OPEi)*(2+OPEi)*(SO[p1]-SO[p3])^2)-
		((4-2*x-x^2)*Power2[x,OPEi]*Power2[SO[p1],3+OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm])/(4*(SO[p1]-SO[p3])^2)+
		((8+4*OPEi-2*x-OPEi*x-OPEi*x^2)*Power2[x,OPEi]*
		Power2[SO[p1],2+OPEi]*Power2[SO[p3],-2-OPEi+OPEm])/
		(4*(2+OPEi)*(SO[p1]-SO[p3])^2)-
		((3+OPEi)*Power2[x,1+OPEi]*Power2[SO[p1],1+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm])/
		(4*(1+OPEi)*(SO[p1]-SO[p3])^2)-
		(x^2*Power2[SO[p3],-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi])/
		(2*(SO[p1]-SO[p3])^2)-
		((12-11*x+3*x^2)*Power2[x,OPEi]*Power2[SO[p3],-1+OPEm]*
		SO[p1])/(4*(SO[p1]-SO[p3])^2)-
		(Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1])/(SO[p1]-SO[p3])^2\
+((1+3*x^2)*Power2[SO[p3],-1-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1])/
		(2*(SO[p1]-SO[p3])^2)+
		((4-3*x)*Power2[x,OPEi]*Power2[SO[p3],-2+OPEm]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])^2)+
		(2*Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/
		(SO[p1]-SO[p3])^2-
		((2+3*x^2)*Power2[SO[p3],-2-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])^2)-
		(Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*SO[p1]^3)/
		(SO[p1]-SO[p3])^2+
		((1+x^2)*Power2[SO[p3],-3-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^3)/
		(2*(SO[p1]-SO[p3])^2))+
		FV[OPEDelta,mu1]*FV[OPEDelta,mu3]*FV[p1,mu2]*
		OPESum[{OPEi,0,-3+OPEm}]*
		((3*Power2[-1,OPEi]*(Power2[1-x,OPEi]+3*Power2[x,OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/4-
		(3*Power2[-1,OPEi]*Power2[x,OPEi]*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],1-OPEi+OPEm])/(4*SO[p1])-
		(9*Power2[-1,OPEi]*(Power2[1-x,OPEi]+Power2[x,OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1])/4+
		(3*Power2[-1,OPEi]*(3*Power2[1-x,OPEi]+Power2[x,OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/4-
		(3*Power2[-1,OPEi]*Power2[1-x,OPEi]*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*SO[p1]^3)/4-
		((20+30*OPEi+10*OPEi^2+6*x+7*OPEi*x+2*OPEi^2*x-
		x^2-OPEi*x^2)*Power2[x,OPEi]*Power2[SO[p3],OPEm])/
		(4*(1+OPEi)*(2+OPEi)*(SO[p1]-SO[p3])^2)+
		((3-2*x+x^2)*Power2[x,OPEi]*Power2[SO[p1],3+OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm])/(4*(SO[p1]-SO[p3])^2)-
		((8+4*OPEi-2*x-OPEi*x+3*x^2+OPEi*x^2)*
		Power2[x,OPEi]*Power2[SO[p1],2+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm])/
		(4*(2+OPEi)*(SO[p1]-SO[p3])^2)+
		((1+OPEi+2*x+OPEi*x)*Power2[x,OPEi]*
		Power2[SO[p1],1+OPEi]*Power2[SO[p3],-1-OPEi+OPEm])/
		(4*(1+OPEi)*(SO[p1]-SO[p3])^2)-
		(3*Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/(4*(SO[p1]-SO[p3])^2)+
		((2+x^2)*Power2[SO[p3],-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi])/
		(4*(SO[p1]-SO[p3])^2)+
		(3*Power2[x,OPEi]*Power2[SO[p3],1+OPEm])/
		(4*SO[p1]*(SO[p1]-SO[p3])^2)+
		((13+2*x)*Power2[x,OPEi]*Power2[SO[p3],-1+OPEm]*SO[p1])/
		(4*(SO[p1]-SO[p3])^2)+
		(9*Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1])/
		(4*(SO[p1]-SO[p3])^2)-
		(3*(2+x^2)*Power2[SO[p3],-1-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1])/
		(4*(SO[p1]-SO[p3])^2)-
		(3*Power2[x,OPEi]*Power2[SO[p3],-2+OPEm]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])^2)-
		(9*Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/
		(4*(SO[p1]-SO[p3])^2)+
		(3*(2+x^2)*Power2[SO[p3],-2-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^2)/
		(4*(SO[p1]-SO[p3])^2)+
		(3*Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*SO[p1]^3)/
		(4*(SO[p1]-SO[p3])^2)-
		((2+x^2)*Power2[SO[p3],-3-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^3)/
		(4*(SO[p1]-SO[p3])^2))+
		FV[OPEDelta,mu2]*FV[OPEDelta,mu3]*FV[p1,mu1]*
		OPESum[{OPEi,0,-3+OPEm}]*
		((3*Power2[-1,OPEi]*Power2[x,OPEi]*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/4-
		(3*Power2[-1,OPEi]*(Power2[1-x,OPEi]+2*Power2[x,OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1])/4+
		(3*Power2[-1,OPEi]*(2*Power2[1-x,OPEi]+Power2[x,OPEi])*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/4-
		(3*Power2[-1,OPEi]*Power2[1-x,OPEi]*
		Power2[SO[p1]-SO[p3],-2+OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*SO[p1]^3)/4-
		((4+6*OPEi+2*OPEi^2+6*x+5*OPEi*x+OPEi^2*x-2*x^2-
		2*OPEi*x^2)*Power2[x,OPEi]*Power2[SO[p3],OPEm])/
		(4*(1+OPEi)*(2+OPEi)*(SO[p1]-SO[p3])^2)+
		((3-x+2*x^2)*Power2[x,OPEi]*Power2[SO[p1],3+OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm])/(4*(SO[p1]-SO[p3])^2)-
		((4+2*OPEi+2*x+OPEi*x+6*x^2+2*OPEi*x^2)*
		Power2[x,OPEi]*Power2[SO[p1],2+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm])/
		(4*(2+OPEi)*(SO[p1]-SO[p3])^2)-
		((1+OPEi-4*x-2*OPEi*x)*Power2[x,OPEi]*
		Power2[SO[p1],1+OPEi]*Power2[SO[p3],-1-OPEi+OPEm])/
		(4*(1+OPEi)*(SO[p1]-SO[p3])^2)+
		((1-2*x)*(1-x)*Power2[SO[p3],-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi])/
		(4*(SO[p1]-SO[p3])^2)+
		((8+x)*Power2[x,OPEi]*Power2[SO[p3],-1+OPEm]*SO[p1])/
		(4*(SO[p1]-SO[p3])^2)+
		(3*Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1])/
		(4*(SO[p1]-SO[p3])^2)-
		(3*(2-3*x+2*x^2)*Power2[SO[p3],-1-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1])/
		(4*(SO[p1]-SO[p3])^2)-
		(3*Power2[x,OPEi]*Power2[SO[p3],-2+OPEm]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])^2)-
		(3*Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])^2)+
		(3*(3-3*x+2*x^2)*Power2[SO[p3],-2-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^2)/
		(4*(SO[p1]-SO[p3])^2)+
		(3*Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*SO[p1]^3)/
		(4*(SO[p1]-SO[p3])^2)-
		((4-3*x+2*x^2)*Power2[SO[p3],-3-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^3)/
		(4*(SO[p1]-SO[p3])^2))+
		FV[OPEDelta,mu1]*MT[mu2,mu3]*OPESum[{OPEi,0,-3+OPEm}]*
		((Power2[-1,OPEi]*(Power2[1-x,OPEi]-
		x*Power2[1-x,OPEi]-6*Power2[x,1+OPEi])*
		Power2[SO[p1]-SO[p3],-1+OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/2+
		(Power2[-1,OPEi]*Power2[x,1+OPEi]*
		Power2[SO[p1]-SO[p3],-1+OPEi]*
		Power2[SO[p3],1-OPEi+OPEm])/SO[p1]-
		Power2[-1,OPEi]*(Power2[1-x,OPEi]-
		x*Power2[1-x,OPEi]-3*Power2[x,1+OPEi])*
		Power2[SO[p1]-SO[p3],-1+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1]+
		(Power2[-1,OPEi]*(Power2[1-x,OPEi]-
		x*Power2[1-x,OPEi]-2*Power2[x,1+OPEi])*
		Power2[SO[p1]-SO[p3],-1+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/2+
		((18+25*OPEi+8*OPEi^2+3*x+5*OPEi*x+2*OPEi^2*x)*
		Power2[x,1+OPEi]*Power2[SO[p3],OPEm])/
		(4*(1+OPEi)*(2+OPEi)*(SO[p1]-SO[p3]))+
		((8+4*OPEi-3*x-2*OPEi*x)*Power2[x,1+OPEi]*
		Power2[SO[p1],2+OPEi]*Power2[SO[p3],-2-OPEi+OPEm])/
		(4*(2+OPEi)*(SO[p1]-SO[p3]))-
		((3+2*OPEi)*Power2[x,1+OPEi]*Power2[SO[p1],1+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm])/
		(4*(1+OPEi)*(SO[p1]-SO[p3]))+
		(Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-OPEi+OPEm])/(2*(SO[p1]-SO[p3]))-
		(Power2[x,1+OPEi]*Power2[SO[p3],1+OPEm])/
		(SO[p1]*(SO[p1]-SO[p3]))-
		(3*Power2[x,1+OPEi]*Power2[SO[p3],-1+OPEm]*SO[p1])/
		(2*(SO[p1]-SO[p3]))-
		((2-x)*Power2[-1,OPEi]*
		Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1])/
		(2*(SO[p1]-SO[p3]))+
		((1-x)*Power2[-1,OPEi]*
		Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])))+
		FV[OPEDelta,mu2]*MT[mu1,mu3]*OPESum[{OPEi,0,-3+OPEm}]*
		(Power2[-1,OPEi]*Power2[x,1+OPEi]*
		Power2[SO[p1]-SO[p3],-1+OPEi]*Power2[SO[p3],-OPEi+OPEm]\
		-2*Power2[-1,OPEi]*Power2[x,1+OPEi]*
		Power2[SO[p1]-SO[p3],-1+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1]+
		Power2[-1,OPEi]*Power2[x,1+OPEi]*
		Power2[SO[p1]-SO[p3],-1+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2-
		((2+7*OPEi+3*OPEi^2-OPEi*x-OPEi^2*x)*
		Power2[x,1+OPEi]*Power2[SO[p3],OPEm])/
		(4*(1+OPEi)*(2+OPEi)*(SO[p1]-SO[p3]))-
		((4+2*OPEi+OPEi*x)*Power2[x,1+OPEi]*
		Power2[SO[p1],2+OPEi]*Power2[SO[p3],-2-OPEi+OPEm])/
		(4*(2+OPEi)*(SO[p1]-SO[p3]))-
		((3+OPEi)*Power2[x,1+OPEi]*Power2[SO[p1],1+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm])/
		(4*(1+OPEi)*(SO[p1]-SO[p3]))-
		(x*Power2[SO[p3],-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi])/
		(2*(SO[p1]-SO[p3]))+
		(3*Power2[x,1+OPEi]*Power2[SO[p3],-1+OPEm]*SO[p1])/
		(2*(SO[p1]-SO[p3]))-
		((1-2*x)*Power2[SO[p3],-1-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1])/
		(2*(SO[p1]-SO[p3]))+
		((1-x)*Power2[SO[p3],-2-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^2)/
		(2*(SO[p1]-SO[p3])))+
		FV[OPEDelta,mu3]*MT[mu1,mu2]*OPESum[{OPEi,0,-3+OPEm}]*
		(-(Power2[-1,OPEi]*Power2[x,OPEi]*
		Power2[SO[p1]-SO[p3],-1+OPEi]*
		Power2[SO[p3],-OPEi+OPEm])+
		Power2[-1,OPEi]*(Power2[1-x,OPEi]+2*Power2[x,OPEi])*
		Power2[SO[p1]-SO[p3],-1+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1]-
		Power2[-1,OPEi]*(2*Power2[1-x,OPEi]+Power2[x,OPEi])*
		Power2[SO[p1]-SO[p3],-1+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2+
		Power2[-1,OPEi]*Power2[1-x,OPEi]*
		Power2[SO[p1]-SO[p3],-1+OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*SO[p1]^3+
		(Power2[x,OPEi]*Power2[SO[p3],OPEm])/(SO[p1]-SO[p3])-
		((2-x)*Power2[x,OPEi]*Power2[SO[p1],3+OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm])/(2*(SO[p1]-SO[p3]))+
		(Power2[x,OPEi]*Power2[SO[p1],2+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm])/(2*(SO[p1]-SO[p3]))-
		((5+x)*Power2[x,OPEi]*Power2[SO[p3],-1+OPEm]*SO[p1])/
		(2*(SO[p1]-SO[p3]))-
		(Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SO[p1])/(SO[p1]-SO[p3])+
		((1+x)*Power2[SO[p3],-1-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1])/
		(2*(SO[p1]-SO[p3]))+
		(2*Power2[x,OPEi]*Power2[SO[p3],-2+OPEm]*SO[p1]^2)/
		(SO[p1]-SO[p3])+
		(2*Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SO[p1]^2)/(SO[p1]-SO[p3])\
		-((1+x)*Power2[SO[p3],-2-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^2)/
		(SO[p1]-SO[p3])-
		(Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*SO[p1]^3)/(SO[p1]-SO[p3])\
		+((1+x)*Power2[SO[p3],-3-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*SO[p1]^3)/
		(2*(SO[p1]-SO[p3])))+
		FV[OPEDelta,mu1]*FV[OPEDelta,mu2]*FV[OPEDelta,mu3]*
		(((1-x)*(3-7*x-2*OPEm*x+2*x^2+2*OPEm*x^2)*
		Power2[SO[p1]-x*SO[p1]-SO[p3],-3+OPEm]*SO[p1]^2*
		SP[p1,p1])/(4*(SO[p1]-SO[p3])^2)-
		((1-x)^2*(1-x-OPEm*x)*
		Power2[SO[p1]-x*SO[p1]-SO[p3],-3+OPEm]*SO[p1]^3*
		SP[p1,p1])/(4*(SO[p1]-SO[p3])^2*SO[p3])-
		((1-x)*(3-8*x-OPEm*x+x^2+OPEm*x^2)*
		Power2[SO[p1]-x*SO[p1]-SO[p3],-3+OPEm]*SO[p1]*SO[p3]*
		SP[p1,p1])/(4*(SO[p1]-SO[p3])^2)+
		((1-3*x)*(1-x)*Power2[SO[p1]-x*SO[p1]-SO[p3],
		-3+OPEm]*SO[p3]^2*SP[p1,p1])/(4*(SO[p1]-SO[p3])^2)-
		(3*(1-x-OPEm*x)*Power2[1-x,1+OPEm]*
		Power2[SO[p1]-SO[p3],-2+OPEm]*SO[p1]^2*
		(SP[p1,p1]-2*SP[p1,p3]+SP[p3,p3]))/
		(4*(SO[p1]-x*SO[p1]-SO[p3])^3)+
		((1-x-OPEm*x)*Power2[1-x,2+OPEm]*
		Power2[SO[p1]-SO[p3],-2+OPEm]*SO[p1]^3*
		(SP[p1,p1]-2*SP[p1,p3]+SP[p3,p3]))/
		(4*(SO[p1]-x*SO[p1]-SO[p3])^3*SO[p3])+
		(3*(1-x-OPEm*x)*Power2[1-x,OPEm]*
		Power2[SO[p1]-SO[p3],-2+OPEm]*SO[p1]*SO[p3]*
		(SP[p1,p1]-2*SP[p1,p3]+SP[p3,p3]))/
		(4*(SO[p1]-x*SO[p1]-SO[p3])^3)-
		((1-x-OPEm*x)*Power2[1-x,-1+OPEm]*
		Power2[SO[p1]-SO[p3],-2+OPEm]*SO[p3]^2*
		(SP[p1,p1]-2*SP[p1,p3]+SP[p3,p3]))/
		(4*(SO[p1]-x*SO[p1]-SO[p3])^3))+
		FV[OPEDelta,mu1]*FV[OPEDelta,mu2]*FV[OPEDelta,mu3]*
		OPESum[{OPEi,0,-3+OPEm}]*
		((3*OPEi*(1-x)*Power2[x,OPEi]*Power2[SO[p1],-1+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SP[p1,p1])/8+
		(OPEi*(1-x)*x*Power2[-1,OPEi]*
		Power2[SO[p1]-x*SO[p1]-SO[p3],-1+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*SP[p1,p1])/4+
		(3*OPEi*(1-x)*Power2[x,OPEi]*Power2[SO[p1],-1+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*SP[p1,p1])/
		(8*(SO[p1]-SO[p3]))-
		(Power2[x,OPEi]*Power2[SO[p1],OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*
		(4*SP[p1,p1]+3*x*SP[p1,p1]+3*OPEi*x*SP[p1,p1]-
		3*x^2*SP[p1,p1]-3*OPEi*x^2*SP[p1,p1]-4*SP[p1,p3]-
		5*SP[p3,p3]))/(8*(SO[p1]-SO[p3]))-
		(Power2[x,OPEi]*Power2[SO[p1],OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*
		(4*SP[p1,p1]+3*x*SP[p1,p1]+3*OPEi*x*SP[p1,p1]-
		3*x^2*SP[p1,p1]-3*OPEi*x^2*SP[p1,p1]-4*SP[p1,p3]-
		4*x*SP[p1,p3]-2*SP[p3,p3]))/8+
		(3*Power2[x,1+OPEi]*Power2[SO[p1],1+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*
		(SP[p1,p1]-2*x*SP[p1,p1]+2*x*SP[p1,p3]-SP[p3,p3]))/
		(8*(SO[p1]-SO[p3])^2)-
		(3*Power2[x,OPEi]*Power2[SO[p1],OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*
		(SP[p1,p1]-2*x*SP[p1,p1]+2*x*SP[p1,p3]-SP[p3,p3]))/
		(8*(SO[p1]-SO[p3])^2)-
		(11*Power2[x,1+OPEi]*Power2[SO[p1],1+OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*SP[p3,p3])/
		(8*(SO[p1]-SO[p3]))-
		(OPEi*(1-x)*x*Power2[SO[p3],-2-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],-1+OPEi]*
		(SP[p1,p1]-2*SP[p1,p3]+SP[p3,p3]))/8+
		(3*Power2[x,2+OPEi]*Power2[SO[p1],2+OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*
		(SP[p1,p1]-2*SP[p1,p3]+SP[p3,p3]))/
		(4*(2+OPEi)*(SO[p1]-SO[p3])^3)-
		(3*Power2[x,1+OPEi]*Power2[SO[p1],1+OPEi]*
		Power2[SO[p3],-1-OPEi+OPEm]*
		(SP[p1,p1]-2*SP[p1,p3]+SP[p3,p3]))/
		(4*(1+OPEi)*(SO[p1]-SO[p3])^3)+
		(3*Power2[x,1+OPEi]*Power2[SO[p3],-1+OPEm]*SO[p1]*
		(SP[p1,p1]-2*SP[p1,p3]+SP[p3,p3]))/
		(4*(1+OPEi)*(SO[p1]-SO[p3])^3)-
		(3*Power2[x,2+OPEi]*Power2[SO[p3],-2+OPEm]*SO[p1]^2*
		(SP[p1,p1]-2*SP[p1,p3]+SP[p3,p3]))/
		(4*(2+OPEi)*(SO[p1]-SO[p3])^3)-
		(Power2[-1,OPEi]*Power2[SO[p1]-x*SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*
		(3*SP[p1,p1]-4*x*SP[p1,p1]-OPEi*x*SP[p1,p1]+
		3*x^2*SP[p1,p1]+OPEi*x^2*SP[p1,p1]-4*SP[p1,p3]+
		2*x*SP[p1,p3]+SP[p3,p3]))/4-
		(Power2[-1,OPEi]*Power2[x,OPEi]*Power2[SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-2-OPEi+OPEm]*
		(2*SP[p1,p1]-2*SP[p1,p3]-SP[p3,p3]+4*x*SP[p3,p3]))/
		(4*SO[p1])+(Power2[x,OPEi]*Power2[SO[p3],OPEm]*
		(2*SP[p1,p1]-2*SP[p1,p3]-SP[p3,p3]+4*x*SP[p3,p3]))/
		(4*SO[p1]*(SO[p1]-SO[p3])^2)+
		(Power2[SO[p3],-3-OPEi+OPEm]*
		Power2[SO[p1]-x*SO[p1]+x*SO[p3],OPEi]*
		(6*SP[p1,p1]-5*x*SP[p1,p1]+OPEi*x*SP[p1,p1]+
		3*x^2*SP[p1,p1]-OPEi*x^2*SP[p1,p1]-4*SP[p1,p3]+
		6*x*SP[p1,p3]-2*OPEi*x*SP[p1,p3]-6*x^2*SP[p1,p3]+
		2*OPEi*x^2*SP[p1,p3]-x*SP[p3,p3]+OPEi*x*SP[p3,p3]+
		3*x^2*SP[p3,p3]-OPEi*x^2*SP[p3,p3]))/8+
		(Power2[x,OPEi]*Power2[SO[p3],-2+OPEm]*SO[p1]*
		(16*SP[p1,p1]+24*OPEi*SP[p1,p1]+8*OPEi^2*SP[p1,p1]+
		6*OPEi*x*SP[p1,p1]+3*OPEi^2*x*SP[p1,p1]+
		6*x^2*SP[p1,p1]-6*OPEi^2*x^2*SP[p1,p1]-
		16*SP[p1,p3]-24*OPEi*SP[p1,p3]-8*OPEi^2*SP[p1,p3]-
		12*OPEi*x*SP[p1,p3]-6*OPEi^2*x*SP[p1,p3]-
		12*x^2*SP[p1,p3]+12*OPEi^2*x^2*SP[p1,p3]-
		14*SP[p3,p3]-21*OPEi*SP[p3,p3]-7*OPEi^2*SP[p3,p3]+
		44*x*SP[p3,p3]+72*OPEi*x*SP[p3,p3]+
		25*OPEi^2*x*SP[p3,p3]-9*OPEi*x^2*SP[p3,p3]-
		9*OPEi^2*x^2*SP[p3,p3]))/
		(8*(1+OPEi)*(2+OPEi)*(SO[p1]-SO[p3])^2)-
		(3*Power2[x,OPEi]*Power2[SO[p3],-1+OPEm]*
		(6*SP[p1,p1]+9*OPEi*SP[p1,p1]+3*OPEi^2*SP[p1,p1]+
		6*x*SP[p1,p1]+7*OPEi*x*SP[p1,p1]+
		2*OPEi^2*x*SP[p1,p1]-2*x^2*SP[p1,p1]-
		4*OPEi*x^2*SP[p1,p1]-2*OPEi^2*x^2*SP[p1,p1]-
		8*SP[p1,p3]-12*OPEi*SP[p1,p3]-4*OPEi^2*SP[p1,p3]-
		4*x*SP[p1,p3]-2*OPEi*x*SP[p1,p3]+
		2*OPEi*x^2*SP[p1,p3]+2*OPEi^2*x^2*SP[p1,p3]-
		4*SP[p3,p3]-6*OPEi*SP[p3,p3]-2*OPEi^2*SP[p3,p3]+
		18*x*SP[p3,p3]+25*OPEi*x*SP[p3,p3]+
		8*OPEi^2*x*SP[p3,p3]-OPEi*x^2*SP[p3,p3]-
		OPEi^2*x^2*SP[p3,p3]))/
		(8*(1+OPEi)*(2+OPEi)*(SO[p1]-SO[p3])^2)-
		(3*OPEi*Power2[x,OPEi]*Power2[SO[p3],-2+OPEm]*
		(2*x*SP[p1,p1]+OPEi*x*SP[p1,p1]-2*x^2*SP[p1,p1]-
		2*OPEi*x^2*SP[p1,p1]-4*x*SP[p1,p3]-
		2*OPEi*x*SP[p1,p3]+4*x^2*SP[p1,p3]+
		4*OPEi*x^2*SP[p1,p3]+2*SP[p3,p3]+3*OPEi*SP[p3,p3]+
		OPEi^2*SP[p3,p3]-2*x*SP[p3,p3]-5*OPEi*x*SP[p3,p3]-
		2*OPEi^2*x*SP[p3,p3]+OPEi*x^2*SP[p3,p3]+
		OPEi^2*x^2*SP[p3,p3]))/
		(8*(1+OPEi)*(2+OPEi)*(SO[p1]-SO[p3]))+
		(Power2[-1,OPEi]*Power2[SO[p1]-SO[p3],OPEi]*
		Power2[SO[p3],-3-OPEi+OPEm]*
		(2*Power2[1-x,OPEi]*SP[p1,p1]-
		2*x*Power2[1-x,OPEi]*SP[p1,p3]-
		3*Power2[1-x,OPEi]*SP[p3,p3]+
		2*x*Power2[1-x,OPEi]*SP[p3,p3]+
		4*Power2[x,1+OPEi]*SP[p3,p3]))/4))*
		SUNF[SUNIndex[a1],SUNIndex[a2],SUNIndex[a3]])/Epsilon
		), dim]
		]
	];

(* C12 *)
(* Count12 *)
Twist2CounterOperator[p1_, mu1_, a1_,  (*p2*)_, mu2_, a2_,  (*p3*)_, mu3_, a3_, _(*s12*), opt___Rule] :=
	Block[ {dim, re, pol, del, oex, dp1, dp3, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					re =   FUDGESIGN coup^3 *
									( ((3*I)/4*(1 + (-1)^OPEm)*CA*(-1 + OPEm*(4 + OPEm))*Sn*
										FVD[OPEDelta, mu1]*
					FVD[OPEDelta, mu3]*FVD[p1, mu2]*SOD[p1]^(-2 + OPEm)*SUNF[a1, a2, a3])/
				(Epsilon*OPEm*(-1 + OPEm^2)) -
			((3*I)/4*(1 + (-1)^OPEm)*CA*(-1 + OPEm*(4 + OPEm))*Sn*FVD[OPEDelta, mu1]*
					FVD[OPEDelta, mu2]*FVD[p1, mu3]*SOD[p1]^(-2 + OPEm)*SUNF[a1, a2, a3])/
				(Epsilon*OPEm*(-1 + OPEm^2)) -
			((3*I)/4*(1 + (-1)^OPEm)*CA*(3 + OPEm)*Sn*FVD[OPEDelta, mu3]*
					MTD[mu1, mu2]*SOD[p1]^(-1 + OPEm)*SUNF[a1, a2, a3])/
				(Epsilon*(-1 + OPEm)*OPEm) +
			((3*I)/4*(1 + (-1)^OPEm)*CA*(3 + OPEm)*Sn*FVD[OPEDelta, mu2]*
					MTD[mu1, mu3]*SOD[p1]^(-1 + OPEm)*SUNF[a1, a2, a3])/
				(Epsilon*(-1 + OPEm)*OPEm)
									)
						,
		"Not ready yet!"
				];
		ChangeDimension[FeynCalcInternal[re],dim]
	];

(* quark-quark-gluon vertex *)
uc30[mu_,a_, coup_] :=
(*W.L.G.A.M v. N. is right ...*)
	-  (I*(CA - 2*CF)*coup^3*Sn*(1 (*- XI*))*DiracGamma[LorentzIndex[mu]]*SUNT[a]
	)/Epsilon;

(* C30  *)
Twist2CounterOperator[mu_,a_, 30, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup  = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim   = Dimension /. {opt} /. Options[Twist2CounterOperator];
		ChangeDimension[uc30[mu, a, coup],dim]
	];

(* quark-quark-gluon vertex *)
uc31[mu_, a_, coup_] :=
	-(-3*I*CA*coup^3*Sn*DiracGamma[LorentzIndex[mu]]*SUNT[a])/Epsilon;

(* C31  *)
Twist2CounterOperator[mu_,a_, 31, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup  = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim   = Dimension /. {opt} /. Options[Twist2CounterOperator];
		ChangeDimension[uc31[mu, a, coup],dim]
	];

uc40[p_, coup_] :=
	(+4*CF*coup^2*(OPEm^(-1) - (1 + OPEm)^(-1))*Sn*
			DiracGamma[Momentum[OPEDelta]]*
			Pair[Momentum[OPEDelta], Momentum[p]]^(-1 + OPEm))/Epsilon;

(* C40 *)
(* Count40  *)
Twist2CounterOperator[p1_, 40, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					ChangeDimension[uc40[p1, coup],dim],
					pol === 1,
						"Not available!"
						(*ChangeDimension[pc40[p1,coup],dim]*)
				]
	];

uc41[p_, coup_] :=
	-(4*CF*coup^2*Sn*DiracGamma[Momentum[OPEDelta]]*
			Pair[Momentum[OPEDelta], Momentum[p]]^(-1 + OPEm)*(-1 + SumS[1, OPEm]))/
		Epsilon;

(* C41 *)
(* Count41  *)
Twist2CounterOperator[p1_, 41, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					ChangeDimension[uc41[p1, coup],dim],
					pol === 1,
						"Not available!"
						(*ChangeDimension[pc41[p1, coup],dim]*)

				]
	];

(* NEW *)
(* C42 *)
(* Count42; p1,  p2, p3 incoming; p1: fermion*)
uc42[p1_, p2_, (*p3*)_, mu_, a_, (*coup*)_] :=
	-( (-2*CA*SMP["g_s"]^3*Sn*SUNT[a]*FV[OPEDelta, mu]*GS[OPEDelta]*
			(SO[p1]^OPEm*SO[p2] + (-1)^OPEm*SO[p1]*SO[p2]^OPEm))/
		(Epsilon*OPEm*(1 + OPEm)*SO[p1]*SO[p2]*(SO[p1] + SO[p2]))
		);

Twist2CounterOperator[p1_, p2_, p3_, mu_, a_, 42, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					ChangeDimension[uc42[p1, p2, p3, mu, a, coup],dim],
					pol === 1,
					"Not available!"
					(*ChangeDimension[pc42[p1, -p2, p3,  coup],dim]*)
				]
	];

(* NEW *)
(* C43 *)
(* Count43  p2 outgoing *)
uc43[p1_, p2_, (*p3*)_, mu_, a_, (*coup*)_] :=
	-(-2*(CA - 2*CF)*SMP["g_s"]^3*Sn*FV[OPEDelta, mu]*GS[OPEDelta]*
			(SO[p1]^(1 + OPEm) - SO[p1]*(SO[p1] - SO[p2])^OPEm -
				SO[p1]^OPEm*SO[p2])*SUNT[a])/
		(Epsilon*OPEm*(1 + OPEm)*SO[p1]*(SO[p1] - SO[p2])*SO[p2]);

Twist2CounterOperator[p1_, p2_, p3_, mu_, a_, 43, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					ChangeDimension[uc43[p1, -p2, p3, mu, a, coup],dim],
					pol === 1,
					"Not available!"
					(*ChangeDimension[pc43[p1, -p2, p3, mu, a, coup],dim]*)
				]
	];


(* C44 *)
(* Count44  p2 outgoing *)
uc44[p1_, p2_, (*p3*)_, mu_, a_, (*coup*)_] :=
	-( -2*(CA - 2*CF)*SMP["g_s"]^3*(1 - OPEm)*Sn*FV[OPEDelta, mu]*
			GS[OPEDelta]*((-1)^OPEm*(SO[p1] - SO[p2])^OPEm*SO[p2] +
				SO[p1]*SO[p2]^OPEm - SO[p2]^(1 + OPEm))*SUNT[a])/
		(Epsilon*OPEm*(1 - OPEm^2)*SO[p1]*(SO[p1] - SO[p2])*SO[p2]);

(* Count44  p2 incoming*)
Twist2CounterOperator[p1_, p2_, p3_, mu_, a_, 44, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					ChangeDimension[uc44[p1, -p2, p3, mu, a, coup],dim],
					pol === 1,
					"Not available!"
					(*ChangeDimension[pc44[p1, -p2, p3, mu, a, coup],dim]*)
				]
	];

(* C45 *)
(* Count45  p2 outgoing *)
(*for some reson there always is a global sign wrong  ....*)
uc45[p1_, p2_, (*p3*)_, mu_, a_, (*coup*)_] :=
	-Block[ {j = OPEl},
		(CA*SMP["g_s"]^3*Sn*DOT[SUNT[a] , GS[OPEDelta]]*FV[OPEDelta, mu]*
				(2*SO[p1]^OPEm*SO[p2] - 2*SO[p1]*SO[p2]^OPEm -
					2*OPEm*OPESum[(SO[p1]^(-j + OPEm)*SO[p2]^(j - OPEm))/j,
						{j, 1, -1 + OPEm}]*SO[p1]*SO[p2]^OPEm +
					OPEm*OPESum[(SO[p1]^(-j + OPEm)*SO[p2]^(j - OPEm))/j,
						{j, 1, -1 + OPEm}]*SO[p2]^(1 + OPEm) +
					OPEm*SO[p1]^OPEm*SO[p2]*SumS[1, -1 + OPEm]))/
			(Epsilon*OPEm*SO[p1]*(SO[p1] - SO[p2])*SO[p2])
	];


Twist2CounterOperator[p1_, p2_, p3_, mu_, a_, 45, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					ChangeDimension[uc45[p1, -p2, p3, mu, a, coup],dim],
					pol === 1,
					"Not available!"
					(*ChangeDimension[pc45[p1, -p2, p3, mu, a, coup],dim]*)
				]
	];

(* C46 *)
(* Count46  p2 outgoing *)
uc46[p1_, p2_, (*p3*)_, mu_, a_, (*coup*)_] :=
	-Block[ {j = OPEl},
		-((CA*SMP["g_s"]^3*Sn*DOT[SUNT[a] , GS[OPEDelta]]*FV[OPEDelta, mu]*
					(OPEm*OPESum[(SO[p1]^(j - OPEm)*SO[p2]^(-j + OPEm))/j,
							{j, 1, -1 + OPEm}]*SO[p1]^(1 + OPEm) - 2*SO[p1]^OPEm*SO[p2] -
						2*OPEm*OPESum[(SO[p1]^(j - OPEm)*SO[p2]^(-j + OPEm))/j,
							{j, 1, -1 + OPEm}]*SO[p1]^OPEm*SO[p2] + 2*SO[p1]*SO[p2]^OPEm +
						OPEm*SO[p1]*SO[p2]^OPEm*SumS[1, -1 + OPEm]))/
				(Epsilon*OPEm*SO[p1]*(SO[p1] - SO[p2])*SO[p2]))
	];

Twist2CounterOperator[p1_, p2_, p3_, mu_, a_, 46, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					ChangeDimension[uc46[p1, -p2, p3, mu, a, coup],dim],
					pol === 1,
					"Not available!"
					(*ChangeDimension[pc46[p1, -p2, p3, mu, a, coup],dim]*)
				]
	];


(* C47 *)
(* (47)
p1, p3, incoming

								p3

									g
									g
									g
									g
									X
								/ \
						-k ^   \ p3-k
							/     \
							ogggggggo
						/  p1-k   \
						/           \
					^             >
					/               \
				p1                p2
*)

(* Count47  p2 outgoing *)
uc47[p1_, _, p3_, mu_, a_, (*coup*)_] :=
	-
	(2*(CA - 2*CF)*SMP["g_s"]^3*Sn*FV[OPEDelta, mu]*GS[OPEDelta]*
	((-1)^OPEm*SO[p1]^(1 + OPEm) - (-1)^OPEm*SO[p1]*(SO[p1] - SO[p3])^OPEm +
	(-1)^OPEm*(SO[p1] - SO[p3])^OPEm*SO[p3] + SO[p1]*SO[p3]^OPEm -
	(-1)^OPEm*SO[p1]*SO[p3]^OPEm - SO[p3]^(1 + OPEm))*SUNT[a])/
	(Epsilon*OPEm*(1 + OPEm)*SO[p1]*(SO[p1] - SO[p3])*SO[p3]);

Twist2CounterOperator[p1_, p2_, p3_, mu_, a_, 47, opt___Rule] :=
	Block[ {dim, re, pol, del, oex, coup},
		coup    = CouplingConstant /. {opt} /. Options[Twist2CounterOperator];
		dim    = Dimension /. {opt} /. Options[Twist2CounterOperator];
		pol    = Polarization /. {opt} /. Options[Twist2CounterOperator];
		Which[pol === 0,
					ChangeDimension[uc47[p1, p2, p3, mu, a, coup],dim],
					pol === 1,
					"Not available!"
					(*ChangeDimension[pc47[p1, p2, p3, mu, a, coup],dim]*)
				]
	];


FCPrint[1,"Twist2CounterOperator.m loaded"];
End[]
