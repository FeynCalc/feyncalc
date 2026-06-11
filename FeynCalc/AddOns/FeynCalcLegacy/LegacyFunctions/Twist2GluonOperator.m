(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Twist2GluonOperator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 9 January '98 at 19:20 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Twist2GluonOperator *)

(* ------------------------------------------------------------------------ *)

GO::usage =
"GO is equivalent to Twist2GluonOperator.";

Twist2GluonOperator::usage =
"Twist2GluonOperator[{p, mu, a}, {nu, b}] or Twist2GluonOperator[p, {mu, a},
{nu, b}] or Twist2GluonOperator[p, mu,a, nu,b] yields the 2-gluon operator (p
is ingoing momentum corresponding to Lorentz index mu).

Twist2GluonOperator[{p,mu,a}, {q,nu,b}, {k,la,c}] or Twist2GluonOperator[
p,mu,a , q,nu,b , k,la,c] gives the 3-gluon operator.

Twist2GluonOperator[{p,mu,a}, {q,nu,b}, {k,la,c}, {s,si,d}] or
Twist2GluonOperator[p,mu,a , q,nu,b , k,la,c , s,si,d] yields the 4-Gluon
operator.

The dimension is determined by the option Dimension. The setting of the option
Polarization (unpolarized: 0; polarized: 1) determines whether the unpolarized
or polarized Feynman rule is returned.

With the setting Explicit set to False the color-structure and the
(1+(-1)^OPEm) (for polarized: (1-(-1)^OPEm)) is extracted and the color
indices are omitted in the arguments of Twist2GluonOperator.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Twist2GluonOperator`Private`"]

del::usage="";

Options[Twist2GluonOperator] = {
	CouplingConstant -> SMP["g_s"],
	Dimension -> D,
	Polarization -> 0,
	Explicit -> False,
	ZeroMomentumInsertion -> True
};
(*Used to be $AchmedRoss=False *)
AchmedRoss=False;

GO = Twist2GluonOperator;

Twist2GluonOperator[a1_, a2_, a3_, a4_, a5_, opt:OptionsPattern[]] :=
	Twist2GluonOperator[a1, {a2, a3}, {a4,a5}, opt] /;
	FreeQ2[Map[Head, {a1, a2, a3, a4, a5}], {Integer, Rule}];

Twist2GluonOperator[{a1_, a2_, a3_}, {a4_, a5_}, opt:OptionsPattern[]] :=
	Twist2GluonOperator[a1, {a2, a3}, {a4,a5}, opt] /;
	FreeQ[Map[Head, {a1, a2, a3, a4, a5}], Integer];

Li[a_,b_] :=
	If[ !FreeQ2[a, {LorentzIndex,Momentum}],
		a,
		LorentzIndex[a, b]
	];

Mo[a_,b_] :=
	If[ !FreeQ2[a, {LorentzIndex,Momentum}],
		a,
		Expand[Momentum[a, b]// MomentumExpand]
	];

Twist2GluonOperator[pi_, {mui_,  ai_}, {nui_, bi_},    opt:OptionsPattern[]] :=
	Block[ {re,pol,a,b, zmi, so,p1,p2, dim},
		dim = OptionValue[Dimension];
		pol = OptionValue[Polarization];
		zmi = OptionValue[ZeroMomentumInsertion];
		a = SUNIndex[ai];
		b =
		SUNIndex[bi];
		If[ zmi===False,
			(* non-zero momentum insertion *)
			so = ExpandScalarProduct[Pair[Momentum[OPEDelta], Momentum[#]]]&;
			p1 = pi[[1]];
			p2 = pi[[2]];
			re = ChangeDimension[
				If[ pol === 0,
				(* deus ex machina (-(1+(-1)^OPEm)),
				tune the prefactor by hand in order to get the
				supersymmetric relation ... *)
					-(1+(-1)^OPEm) (SUNDelta[a, b]*(Power2[SO[p1], OPEm-2] +
					Power2[SO[p2], OPEm-2]) (FV[OPEDelta, nui]*FV[p2, mui]*SO[p1] -
					MT[mui, nui]*SO[p1]*SO[p2] + FV[OPEDelta, mui]*(FV[p1, nui]*SO[p2] -
					FV[OPEDelta, nui]*SP[p1, p2])))/2,
					(* deus ex machina  ... (-1) (1-(-1)^OPEm)/2 *)
					(1-(-1)^OPEm)/2 I*SD[a, b]*(Power2[SO[p2],OPEm-2]*
					(-(FV[OPEDelta, nui]*LC[mui][OPEDelta, p1, p2]) + SO[p2]*LC[mui, nui][OPEDelta, p1]) +
					Power2[SO[p1],OPEm-2]*(FV[OPEDelta, mui] LC[nui][OPEDelta, p1, p2] -
					SO[p1]*LC[mui, nui][OPEDelta, p2]))
				], dim],
			p = Momentum[pi, dim];
			If[ pol === 0,
				re  = ((1 + Power2[(-1),OPEm])/2) SUNDelta[a, b] *
				Twist2GluonOperator[Mo[p,dim], {Li[mui, dim]}, {Li[nui, dim]}, opt],
				re =  I (1-Power2[(-1),OPEm]) SUNDelta[a, b] *
				Twist2GluonOperator[Mo[p,dim], {Li[mui, dim]}, {Li[nui, dim]}, opt]
			]
		];
		re
	] /; FreeQ[{pi,mui,ai,nui,bi}, Pattern] && (Head[pi]=!=List || (Head[pi]===List &&
		!OptionValue[ZeroMomentumInsertion]));

(* maybe too time-costly to do here ... *)
(* linearity in the contracted legs *)

Twist2GluonOperator[a___,{pe_, c_Plus},b___] :=
	Map[Twist2GluonOperator[a, {pe,#},b]&, c];

Twist2GluonOperator[a__,{pe_Plus},b___] :=
	Map[Twist2GluonOperator[a, {#},b]&, pe];

Twist2GluonOperator[a___,{p1___,Momentum[pe_Plus,di___],p2___},b___] :=
	Twist2GluonOperator[a,{p1, Expand[MomentumExpand[Momentum[pe,di]]], p2}, b] /; Length[{p1,p2}]===1;

Twist2GluonOperator[a___, {pe_, m_Integer mu_Momentum}, b___] :=
	m Twist2GluonOperator[a, {pe, mu}, b];

(*
Twist2GluonOperator[p_ /; FreeQ[p, Momentum], {a_}, {b_}, ru___Rule] :=
Twist2GluonOperator[Mo[p,Dimension /. {ru} /. Options[Twist2GluonOperator]],
							{a}, {b}, ru];

Twist2GluonOperator[p_, {a_ /; FreeQ2[a, {LorentzIndex, Momentum}]},
									{b_ /; FreeQ2[b, {LorentzIndex, Momentum}]},
							ru___Rule] :=
Twist2GluonOperator[p, {Li[a, Dimension /. {ru} /.
												Options[Twist2GluonOperator]]},
								{Li[b, Dimension /. {ru} /. Options[Twist2GluonOperator]]},
							ru];

Twist2GluonOperator[a___ ,{p_ /; FreeQ[p, Momentum], mu_}, b___,
										ru___Rule] :=
Twist2GluonOperator[a, {Mo[p,Dimension /.{ru}/.
	Options[Twist2GluonOperator]],mu}, b];

Twist2GluonOperator[a___,{p_, mu_ /; FreeQ2[mu, {LorentzIndex, Momentum}]},
							b___, ru___Rule] :=
Twist2GluonOperator[a, {p, Li[mu,Dimension/.{ru}/.
	Options[Twist2GluonOperator]]}, b];
*)

(* 2 - Gluon operator; unpolarized: RH (3A.8); (3A.9) *)
Twist2GluonOperator[pe_, {m_Integer mu_Momentum}, {nu_}, opt:OptionsPattern[]] :=
	m Twist2GluonOperator[pe, {mu}, {nu}, opt];

Twist2GluonOperator[pe_, {mu_}, {m_Integer nu_Momentum}, opt:OptionsPattern[]] :=
	m Twist2GluonOperator[pe, {mu}, {nu}, opt];

(* eq. (3A.10) of R.Hamberg *)
(* Ward2 *)
Twist2GluonOperator[m_. Momentum[pi_,___], {Momentum[pi_,___]},    {_}, OptionsPattern[]] :=
	0 /; IntegerQ[m] && ($OPEWard =!= False);

Twist2GluonOperator[a_+ m_. Momentum[pi_,di___], {Momentum[pi_,___]}, {q_}, opt:OptionsPattern[] ] :=
	- 1/m Twist2GluonOperator[a + m Momentum[pi,di], {a}, {q}, opt] /;
	(First[a/m+Momentum[pi,di]] === Momentum[pi,di]) && ($OPEWard =!= False);

Twist2GluonOperator[a_+ m_. Momentum[pi_,di___], {q_}, {Momentum[pi_,dii___]}, opt:OptionsPattern[] ] :=
	- 1/m Twist2GluonOperator[a + m Momentum[pi,dii], {q}, {a}, opt] /;
	(First[a/m+Momentum[pi,di]] === Momentum[pi,di]) && $OPEWard =!= False;

Twist2GluonOperator[m_. Momentum[pi_,___], {_}, {Momentum[pi_,___]}, OptionsPattern[]] :=
	0 /; IntegerQ[m] && $OPEWard =!= False;

(* Symmetry in Lorentz indices *)
Twist2GluonOperator[pi_ /; Head[pi] =!= List, {mui_}, {nui_}, opt:OptionsPattern[]] :=
	psi[opt] Twist2GluonOperator[pi, {nui}, {mui},opt] /; !OrderedQ[{mui,nui}];

Twist2GluonOperator[pi_ /; Head[pi] =!= List, {mui_}, {nui_}, OptionsPattern[]] :=
	Block[ {dim, p, mu, nu,  re, pol},
		dim    = OptionValue[Dimension];
		pol    = OptionValue[Polarization];
		p = Mo[pi, dim];
		mu = Li[mui, dim];
		nu = Li[nui, dim];
		del = Momentum[OPEDelta, dim];
		If[ pol === 0,
			re  = Power2[Pair[del,p],(OPEm-2)] *
			(Pair[mu,nu] Pair[del,p]^2 - (Pair[p,mu] Pair[del,nu] + Pair[p,nu] Pair[del,mu]) Pair[del,p] +
			Pair[p,p] Pair[del,mu] Pair[del,nu]),
			re = Eps[mu,nu,del,p] Power2[Pair[del,p],(OPEm-1)]
		];
		re//simp[OptionValue[Explicit]]
	] /; FreeQ[{pi,mui,nui}, Pattern] && OptionValue[Explicit];

o3[mu_, nu_, la_, p_, q_, _, dlt_] :=
	(((Pair[dlt,nu] Pair[la,mu] - Pair[dlt,la] Pair[mu,nu]) Pair[dlt, p] +
	Pair[dlt,mu] (Pair[p,nu] Pair[dlt,la] - Pair[p,la] Pair[dlt,nu])) Power2[Pair[dlt,p],(OPEm-2)] +
	Pair[dlt,la] ( Pair[dlt,p] Pair[q,mu]   Pair[dlt,nu] + Pair[dlt,q] Pair[p,nu]   Pair[dlt,mu] -
	Pair[dlt,p] Pair[dlt,q]  Pair[mu,nu]  -    Pair[p,q]   Pair[dlt,mu] Pair[dlt,nu]) *
	OPESum[(-Pair[dlt,p])^OPEi Pair[dlt,q]^(OPEm-3-OPEi), {OPEi,0,OPEm-3}]);

(* 3-gluon operator *)

zerom[oo___Rule] :=
	ZeroMomentumInsertion/.{oo}/.Options[Twist2GluonOperator];

Twist2GluonOperator[a1_, a2_, a3_, a4_, a5_, a6_, a7_, a8_, a9_, opt:OptionsPattern[]] :=
	Twist2GluonOperator[{a1,a2,a3}, {a4,a5,a6}, {a7,a8,a9}, opt];

(* for non-zero-momentum insertion only *)
Twist2GluonOperator[{p1_,m1_,a1_},{p2_,m2_,a2_}, {p3_,m3_,a3_}, opt:OptionsPattern[]] :=
	Block[ {coup,id,L,R,dim},
		coup = OptionValue[CouplingConstant];
		dim  = OptionValue[Dimension];
		id = Identity;
		ChangeDimension[
		If[ OptionValue[Polarization] === 0,
		(* UNPOLBEGIN *)
			L[1] = -3-OPEi+OPEm;
			L[2] = SO[p1]+SO[p2];
			L[3] = -3+OPEm;
			L[4] = (OPESum[id[L[2]]^OPEi*SO[p1]^id[L[1]],{OPEi,0,id[L[3]]}]);
			L[5] = SO[p2]+SO[p3];
			L[6] = (OPESum[id[L[5]]^OPEi*SO[p3]^id[L[1]],{OPEi,0,id[L[3]]}]);
			L[7] = (OPESum[id[L[2]]^OPEi*SO[p2]^id[L[1]],{OPEi,0,id[L[3]]}]);
			L[8] = SO[p1]+SO[p3];
			L[9] = (OPESum[id[L[8]]^OPEi*SO[p3]^id[L[1]],{OPEi,0,id[L[3]]}]);
			L[10] = (OPESum[id[L[8]]^OPEi*SO[p1]^id[L[1]],{OPEi,0,id[L[3]]}]);
			L[11] = (OPESum[id[L[5]]^OPEi*SO[p2]^id[L[1]],{OPEi,0,id[L[3]]}]);
			L[12] = (id[L[2]]^OPEm*SO[p3]^2+id[L[4]]*SO[p1]^3*SO[p3]^2-
			id[L[6]]*SO[p1]^3*SO[p3]^2+
			2*id[L[4]]*SO[p1]^2*SO[p2]*SO[p3]^2-
			2*id[L[6]]*SO[p1]^2*SO[p2]*SO[p3]^2+
			id[L[4]]*SO[p1]*SO[p2]^2*SO[p3]^2-
			id[L[6]]*SO[p1]*SO[p2]^2*SO[p3]^2+SO[p1]^2*SO[p3]^OPEm+
			2*SO[p1]*SO[p2]*SO[p3]^OPEm+SO[p2]^2*SO[p3]^OPEm);
			L[13] = (id[L[2]]^OPEm*SO[p3]^2+id[L[7]]*SO[p1]^2*SO[p2]*SO[p3]^2-
			id[L[9]]*SO[p1]^2*SO[p2]*SO[p3]^2+
			2*id[L[7]]*SO[p1]*SO[p2]^2*SO[p3]^2-
			2*id[L[9]]*SO[p1]*SO[p2]^2*SO[p3]^2+
			id[L[7]]*SO[p2]^3*SO[p3]^2-id[L[9]]*SO[p2]^3*SO[p3]^2+
			SO[p1]^2*SO[p3]^OPEm+2*SO[p1]*SO[p2]*SO[p3]^OPEm+SO[p2]^2*SO[p3]^OPEm);
			L[14] = (id[L[8]]^OPEm*SO[p2]^2+id[L[10]]*SO[p1]^3*SO[p2]^2-
			id[L[11]]*SO[p1]^3*SO[p2]^2+SO[p1]^2*SO[p2]^OPEm+
			2*id[L[10]]*SO[p1]^2*SO[p2]^2*SO[p3]-
			2*id[L[11]]*SO[p1]^2*SO[p2]^2*SO[p3]+2*SO[p1]*SO[p2]^OPEm*SO[p3]+
			id[L[10]]*SO[p1]*SO[p2]^2*SO[p3]^2-
			id[L[11]]*SO[p1]*SO[p2]^2*SO[p3]^2+SO[p2]^OPEm*SO[p3]^2);
			L[15] = (id[L[8]]^OPEm*SO[p2]^2+SO[p1]^2*SO[p2]^OPEm-
			id[L[7]]*SO[p1]^2*SO[p2]^2*SO[p3]+
			id[L[9]]*SO[p1]^2*SO[p2]^2*SO[p3]+2*SO[p1]*SO[p2]^OPEm*SO[p3]-
			2*id[L[7]]*SO[p1]*SO[p2]^2*SO[p3]^2+
			2*id[L[9]]*SO[p1]*SO[p2]^2*SO[p3]^2+SO[p2]^OPEm*SO[p3]^2-
			id[L[7]]*SO[p2]^2*SO[p3]^3+id[L[9]]*SO[p2]^2*SO[p3]^3);
			L[16] = 1+OPEm;
			L[17] = 2+OPEm;
			L[18] = (id[L[8]]^OPEm*SO[p1]^2*SO[p2]^2*SO[p3]+
			2*id[L[8]]^OPEm*SO[p1]*SO[p2]^3*SO[p3]+
			id[L[8]]^OPEm*SO[p2]^4*SO[p3]+SO[p1]^4*SO[p2]^OPEm*SO[p3]+
			2*SO[p1]^3*SO[p2]^id[L[16]]*SO[p3]+
			SO[p1]^2*SO[p2]^id[L[17]]*SO[p3]-
			id[L[2]]^OPEm*SO[p1]^2*SO[p2]*SO[p3]^2-
			id[L[7]]*SO[p1]^4*SO[p2]^2*SO[p3]^2+
			id[L[9]]*SO[p1]^4*SO[p2]^2*SO[p3]^2-
			2*id[L[7]]*SO[p1]^3*SO[p2]^3*SO[p3]^2+
			2*id[L[9]]*SO[p1]^3*SO[p2]^3*SO[p3]^2-
			id[L[7]]*SO[p1]^2*SO[p2]^4*SO[p3]^2+
			id[L[9]]*SO[p1]^2*SO[p2]^4*SO[p3]^2+
			2*SO[p1]^3*SO[p2]^OPEm*SO[p3]^2+
			4*SO[p1]^2*SO[p2]^id[L[16]]*SO[p3]^2+
			2*SO[p1]*SO[p2]^id[L[17]]*SO[p3]^2-
			2*id[L[2]]^OPEm*SO[p1]*SO[p2]*SO[p3]^3-
			2*id[L[7]]*SO[p1]^3*SO[p2]^2*SO[p3]^3+
			2*id[L[9]]*SO[p1]^3*SO[p2]^2*SO[p3]^3-
			4*id[L[7]]*SO[p1]^2*SO[p2]^3*SO[p3]^3+
			4*id[L[9]]*SO[p1]^2*SO[p2]^3*SO[p3]^3-
			2*id[L[7]]*SO[p1]*SO[p2]^4*SO[p3]^3+
			2*id[L[9]]*SO[p1]*SO[p2]^4*SO[p3]^3+SO[p1]^2*SO[p2]^OPEm*SO[p3]^3+
			2*SO[p1]*SO[p2]^id[L[16]]*SO[p3]^3+
			SO[p2]^id[L[17]]*SO[p3]^3-id[L[2]]^OPEm*SO[p2]*SO[p3]^4-
			id[L[7]]*SO[p1]^2*SO[p2]^2*SO[p3]^4+
			id[L[9]]*SO[p1]^2*SO[p2]^2*SO[p3]^4-
			2*id[L[7]]*SO[p1]*SO[p2]^3*SO[p3]^4+
			2*id[L[9]]*SO[p1]*SO[p2]^3*SO[p3]^4-
			id[L[7]]*SO[p2]^4*SO[p3]^4+id[L[9]]*SO[p2]^4*SO[p3]^4-
			SO[p1]^4*SO[p2]*SO[p3]^OPEm-2*SO[p1]^3*SO[p2]^2*SO[p3]^OPEm-
			SO[p1]^2*SO[p2]^3*SO[p3]^OPEm-2*SO[p1]^3*SO[p2]*SO[p3]^id[L[16]]-
			4*SO[p1]^2*SO[p2]^2*SO[p3]^id[L[16]]-
			2*SO[p1]*SO[p2]^3*SO[p3]^id[L[16]]-
			SO[p1]^2*SO[p2]*SO[p3]^id[L[17]]-
			2*SO[p1]*SO[p2]^2*SO[p3]^id[L[17]]-SO[p2]^3*SO[p3]^id[L[17]]);
			L[19] = (id[L[5]]^OPEm*SO[p1]^2+SO[p1]^OPEm*SO[p2]^2-
			id[L[10]]*SO[p1]^2*SO[p2]^3+id[L[11]]*SO[p1]^2*SO[p2]^3+
			2*SO[p1]^OPEm*SO[p2]*SO[p3]-2*id[L[10]]*SO[p1]^2*SO[p2]^2*SO[p3]+
			2*id[L[11]]*SO[p1]^2*SO[p2]^2*SO[p3]+SO[p1]^OPEm*SO[p3]^2-
			id[L[10]]*SO[p1]^2*SO[p2]*SO[p3]^2+
			id[L[11]]*SO[p1]^2*SO[p2]*SO[p3]^2);
			L[20] = (id[L[5]]^OPEm*SO[p1]^2+SO[p1]^OPEm*SO[p2]^2+
			2*SO[p1]^OPEm*SO[p2]*SO[p3]-id[L[4]]*SO[p1]^2*SO[p2]^2*SO[p3]+
			id[L[6]]*SO[p1]^2*SO[p2]^2*SO[p3]+SO[p1]^OPEm*SO[p3]^2-
			2*id[L[4]]*SO[p1]^2*SO[p2]*SO[p3]^2+
			2*id[L[6]]*SO[p1]^2*SO[p2]*SO[p3]^2-
			id[L[4]]*SO[p1]^2*SO[p3]^3+id[L[6]]*SO[p1]^2*SO[p3]^3);
			L[21] = (id[L[5]]^OPEm*SO[p1]^4*SO[p3]+
			2*id[L[5]]^OPEm*SO[p1]^3*SO[p2]*SO[p3]+
			id[L[5]]^OPEm*SO[p1]^2*SO[p2]^2*SO[p3]+
			SO[p1]^id[L[17]]*SO[p2]^2*SO[p3]+
			2*SO[p1]^id[L[16]]*SO[p2]^3*SO[p3]+SO[p1]^OPEm*SO[p2]^4*SO[p3]+
			2*SO[p1]^id[L[17]]*SO[p2]*SO[p3]^2-
			id[L[2]]^OPEm*SO[p1]*SO[p2]^2*SO[p3]^2-
			id[L[4]]*SO[p1]^4*SO[p2]^2*SO[p3]^2+
			id[L[6]]*SO[p1]^4*SO[p2]^2*SO[p3]^2+
			4*SO[p1]^id[L[16]]*SO[p2]^2*SO[p3]^2-
			2*id[L[4]]*SO[p1]^3*SO[p2]^3*SO[p3]^2+
			2*id[L[6]]*SO[p1]^3*SO[p2]^3*SO[p3]^2+
			2*SO[p1]^OPEm*SO[p2]^3*SO[p3]^2-
			id[L[4]]*SO[p1]^2*SO[p2]^4*SO[p3]^2+
			id[L[6]]*SO[p1]^2*SO[p2]^4*SO[p3]^2+
			SO[p1]^id[L[17]]*SO[p3]^3-
			2*id[L[2]]^OPEm*SO[p1]*SO[p2]*SO[p3]^3-
			2*id[L[4]]*SO[p1]^4*SO[p2]*SO[p3]^3+
			2*id[L[6]]*SO[p1]^4*SO[p2]*SO[p3]^3+
			2*SO[p1]^id[L[16]]*SO[p2]*SO[p3]^3-
			4*id[L[4]]*SO[p1]^3*SO[p2]^2*SO[p3]^3+
			4*id[L[6]]*SO[p1]^3*SO[p2]^2*SO[p3]^3+
			SO[p1]^OPEm*SO[p2]^2*SO[p3]^3-
			2*id[L[4]]*SO[p1]^2*SO[p2]^3*SO[p3]^3+
			2*id[L[6]]*SO[p1]^2*SO[p2]^3*SO[p3]^3-
			id[L[2]]^OPEm*SO[p1]*SO[p3]^4-id[L[4]]*SO[p1]^4*SO[p3]^4+
			id[L[6]]*SO[p1]^4*SO[p3]^4-
			2*id[L[4]]*SO[p1]^3*SO[p2]*SO[p3]^4+
			2*id[L[6]]*SO[p1]^3*SO[p2]*SO[p3]^4-
			id[L[4]]*SO[p1]^2*SO[p2]^2*SO[p3]^4+
			id[L[6]]*SO[p1]^2*SO[p2]^2*SO[p3]^4-SO[p1]^3*SO[p2]^2*SO[p3]^OPEm-
			2*SO[p1]^2*SO[p2]^3*SO[p3]^OPEm-SO[p1]*SO[p2]^4*SO[p3]^OPEm-
			2*SO[p1]^3*SO[p2]*SO[p3]^id[L[16]]-
			4*SO[p1]^2*SO[p2]^2*SO[p3]^id[L[16]]-
			2*SO[p1]*SO[p2]^3*SO[p3]^id[L[16]]-
			SO[p1]^3*SO[p3]^id[L[17]]-
			2*SO[p1]^2*SO[p2]*SO[p3]^id[L[17]]-
			SO[p1]*SO[p2]^2*SO[p3]^id[L[17]]);
			L[22] = (id[L[5]]^OPEm*SO[p1]^4*SO[p2]+SO[p1]^id[L[17]]*SO[p2]^3-
			id[L[8]]^OPEm*SO[p1]*SO[p2]^4-id[L[10]]*SO[p1]^4*SO[p2]^4+
			id[L[11]]*SO[p1]^4*SO[p2]^4-SO[p1]^3*SO[p2]^id[L[17]]+
			2*id[L[5]]^OPEm*SO[p1]^3*SO[p2]*SO[p3]+
			2*SO[p1]^id[L[17]]*SO[p2]^2*SO[p3]-
			2*id[L[8]]^OPEm*SO[p1]*SO[p2]^3*SO[p3]-
			2*id[L[10]]*SO[p1]^4*SO[p2]^3*SO[p3]+
			2*id[L[11]]*SO[p1]^4*SO[p2]^3*SO[p3]+
			2*SO[p1]^id[L[16]]*SO[p2]^3*SO[p3]-
			2*id[L[10]]*SO[p1]^3*SO[p2]^4*SO[p3]+
			2*id[L[11]]*SO[p1]^3*SO[p2]^4*SO[p3]-
			2*SO[p1]^3*SO[p2]^id[L[16]]*SO[p3]-
			2*SO[p1]^2*SO[p2]^id[L[17]]*SO[p3]+
			id[L[5]]^OPEm*SO[p1]^2*SO[p2]*SO[p3]^2+
			SO[p1]^id[L[17]]*SO[p2]*SO[p3]^2-
			id[L[8]]^OPEm*SO[p1]*SO[p2]^2*SO[p3]^2-
			id[L[10]]*SO[p1]^4*SO[p2]^2*SO[p3]^2+
			id[L[11]]*SO[p1]^4*SO[p2]^2*SO[p3]^2+
			4*SO[p1]^id[L[16]]*SO[p2]^2*SO[p3]^2-
			4*id[L[10]]*SO[p1]^3*SO[p2]^3*SO[p3]^2+
			4*id[L[11]]*SO[p1]^3*SO[p2]^3*SO[p3]^2+
			SO[p1]^OPEm*SO[p2]^3*SO[p3]^2-
			id[L[10]]*SO[p1]^2*SO[p2]^4*SO[p3]^2+
			id[L[11]]*SO[p1]^2*SO[p2]^4*SO[p3]^2-
			SO[p1]^3*SO[p2]^OPEm*SO[p3]^2-
			4*SO[p1]^2*SO[p2]^id[L[16]]*SO[p3]^2-
			SO[p1]*SO[p2]^id[L[17]]*SO[p3]^2+
			2*SO[p1]^id[L[16]]*SO[p2]*SO[p3]^3-
			2*id[L[10]]*SO[p1]^3*SO[p2]^2*SO[p3]^3+
			2*id[L[11]]*SO[p1]^3*SO[p2]^2*SO[p3]^3+
			2*SO[p1]^OPEm*SO[p2]^2*SO[p3]^3-
			2*id[L[10]]*SO[p1]^2*SO[p2]^3*SO[p3]^3+
			2*id[L[11]]*SO[p1]^2*SO[p2]^3*SO[p3]^3-
			2*SO[p1]^2*SO[p2]^OPEm*SO[p3]^3-
			2*SO[p1]*SO[p2]^id[L[16]]*SO[p3]^3+SO[p1]^OPEm*SO[p2]*SO[p3]^4-
			id[L[10]]*SO[p1]^2*SO[p2]^2*SO[p3]^4+
			id[L[11]]*SO[p1]^2*SO[p2]^2*SO[p3]^4-SO[p1]*SO[p2]^OPEm*SO[p3]^4);
			L[23] = (id[L[10]]*SP[p1,p2]-id[L[11]]*SP[p1,p2]-
			id[L[4]]*SP[p1,p3]+id[L[6]]*SP[p1,p3]+
			id[L[7]]*SP[p2,p3]-id[L[9]]*SP[p2,p3]);
			L[24] = (-I/2*FV[OPEDelta,m1]*FV[OPEDelta,m2]*FV[OPEDelta,m3]*id[L[23]]-
			(I/2*FV[OPEDelta,m1]*FV[OPEDelta,m3]*FV[p1,m2]*id[L[19]])/
			(id[L[5]]^2*SO[p1]^2)+
			(I/2*FV[OPEDelta,m1]*FV[OPEDelta,m2]*FV[p1,m3]*id[L[20]])/
			(id[L[5]]^2*SO[p1]^2)+
			(I/2*FV[OPEDelta,m2]*FV[OPEDelta,m3]*FV[p2,m1]*id[L[14]])/
			(id[L[8]]^2*SO[p2]^2)-
			(I/2*FV[OPEDelta,m1]*FV[OPEDelta,m2]*FV[p2,m3]*id[L[15]])/
			(id[L[8]]^2*SO[p2]^2)+
			(I/2*FV[OPEDelta,m3]*id[L[22]]*MT[m1,m2])/
			(id[L[5]]^2*id[L[8]]^2*SO[p1]*SO[p2])-
			(I/2*FV[OPEDelta,m2]*FV[OPEDelta,m3]*FV[p3,m1]*id[L[12]])/
			(id[L[2]]^2*SO[p3]^2)+
			(I/2*FV[OPEDelta,m1]*FV[OPEDelta,m3]*FV[p3,m2]*id[L[13]])/
			(id[L[2]]^2*SO[p3]^2)-
			(I/2*FV[OPEDelta,m2]*id[L[21]]*MT[m1,m3])/
			(id[L[2]]^2*id[L[5]]^2*SO[p1]*SO[p3])+
			(I/2*FV[OPEDelta,m1]*id[L[18]]*MT[m2,m3])/
			(id[L[2]]^2*id[L[8]]^2*SO[p2]*SO[p3]));
			coup*id[L[24]]*SUNF[a1,a2,a3](* UNPOLEND *)
			,
			(* BEGINPOL *)
			R[1] = -3-OPEi+OPEm;
			R[2] = SO[p2]+SO[p3];
			R[3] = -3+OPEm;
			R[4] = (OPESum[id[R[2]]^OPEi*SO[p2]^id[R[1]],{OPEi,0,id[R[3]]}]);
			R[5] = (OPESum[id[R[2]]^OPEi*SO[p3]^id[R[1]],{OPEi,0,id[R[3]]}]);
			R[6] = SO[p1]+SO[p3];
			R[7] = (OPESum[id[R[6]]^OPEi*SO[p1]^id[R[1]],{OPEi,0,id[R[3]]}]);
			R[8] = (OPESum[id[R[6]]^OPEi*SO[p3]^id[R[1]],{OPEi,0,id[R[3]]}]);
			R[9] = SO[p1]+SO[p2];
			R[10] = (OPESum[id[R[9]]^OPEi*SO[p1]^id[R[1]],{OPEi,0,id[R[3]]}]);
			R[11] = (OPESum[id[R[9]]^OPEi*SO[p2]^id[R[1]],{OPEi,0,id[R[3]]}]);
			R[12] = (id[R[2]]^OPEm+id[R[4]]*SO[p2]^3+
			2*id[R[4]]*SO[p2]^2*SO[p3]+id[R[4]]*SO[p2]*SO[p3]^2);
			R[13] = (id[R[6]]^OPEm+id[R[7]]*SO[p1]^3+
			2*id[R[7]]*SO[p1]^2*SO[p3]+id[R[7]]*SO[p1]*SO[p3]^2);
			R[14] = -2+OPEm;
			R[15] = (id[R[2]]^OPEm+id[R[5]]*SO[p2]^2*SO[p3]+
			2*id[R[5]]*SO[p2]*SO[p3]^2+id[R[5]]*SO[p3]^3);
			R[16] = (id[R[9]]^OPEm+id[R[10]]*SO[p1]^3+
			2*id[R[10]]*SO[p1]^2*SO[p2]+id[R[10]]*SO[p1]*SO[p2]^2);
			R[17] = (id[R[6]]^OPEm+id[R[8]]*SO[p1]^2*SO[p3]+
			2*id[R[8]]*SO[p1]*SO[p3]^2+id[R[8]]*SO[p3]^3);
			R[18] = (id[R[9]]^OPEm+id[R[11]]*SO[p1]^2*SO[p2]+
			2*id[R[11]]*SO[p1]*SO[p2]^2+id[R[11]]*SO[p2]^3);
			R[19] = (SO[p1]^OPEm*SO[p2]*SO[p3]+SO[p1]*SO[p2]^OPEm*SO[p3]+
			SO[p1]*SO[p2]*SO[p3]^OPEm);
			R[20] = (FV[OPEDelta,m2]*FV[OPEDelta,m3]*id[R[4]]*LC[m1][OPEDelta,p1,p2]-
			FV[OPEDelta,m2]*FV[OPEDelta,m3]*id[R[5]]*LC[m1][OPEDelta,p1,p3]+
			FV[OPEDelta,m1]*FV[OPEDelta,m3]*id[R[7]]*LC[m2][OPEDelta,p1,p2]+
			FV[OPEDelta,m1]*FV[OPEDelta,m3]*id[R[8]]*LC[m2][OPEDelta,p2,p3]-
			FV[OPEDelta,m1]*FV[OPEDelta,m2]*id[R[10]]*
			LC[m3][OPEDelta,p1,p3]+FV[OPEDelta,m1]*FV[OPEDelta,m2]*
			id[R[11]]*LC[m3][OPEDelta,p2,p3]-
			(FV[OPEDelta,m3]*id[R[12]]*LC[m1,m2][OPEDelta,p1])/
			id[R[2]]^2-(FV[OPEDelta,m3]*id[R[13]]*
			LC[m1,m2][OPEDelta,p2])/id[R[6]]^2-
			FV[OPEDelta,m3]*SO[p3]^id[R[14]]*LC[m1,m2][OPEDelta,p3]+
			(FV[OPEDelta,m2]*id[R[15]]*LC[m1,m3][OPEDelta,p1])/
			id[R[2]]^2+FV[OPEDelta,m2]*SO[p2]^id[R[14]]*
			LC[m1,m3][OPEDelta,p2]+(FV[OPEDelta,m2]*id[R[16]]*
			LC[m1,m3][OPEDelta,p3])/id[R[9]]^2-
			FV[OPEDelta,m1]*SO[p1]^id[R[14]]*LC[m2,m3][OPEDelta,p1]-
			(FV[OPEDelta,m1]*id[R[17]]*LC[m2,m3][OPEDelta,p2])/
			id[R[6]]^2-(FV[OPEDelta,m1]*id[R[18]]*
			LC[m2,m3][OPEDelta,p3])/id[R[9]]^2-
			(id[R[19]]*LC[m1,m2,m3][OPEDelta])/(SO[p1]*SO[p2]*SO[p3])
			);
			coup*id[R[20]]*SUNF[a1,a2,a3]
		(* ENDPOL *)
				] , dim]
	] /; (zerom[opt]===False);


HoldPattern[Twist2GluonOperator[{p_,m_}, {q_,n_}, {k_,l_}, opt:OptionsPattern[]]] :=
	(fe[{p,m},{q,n},{k,l},opt] /. eq->Twist2GluonOperator) /;
	FreeQ[{p,m,q,n,k,l}, Pattern] && (fe[{p,m},{q,n},{k,l},opt] =!= eq[{p,m},{q,n},{k,l},opt]);

fe[{p_,m_}, {q_,n_}, {k_,l_}, opt:OptionsPattern[]] :=
	(fe[{p,m}, {q,n}, {k,l}, opt] =
	(Sort[{
		eq[{p,m}, {q,n}, {k,l}, opt], -eq[{p,m}, {k,l}, {q,n}, opt],
		eq[{q,n}, {k,l}, {p,m}, opt], -eq[{q,n}, {p,m}, {k,l}, opt],
		eq[{k,l}, {p,m}, {q,n}, opt], -eq[{k,l}, {q,n}, {p,m}, opt]}
			][[4]]
	));

(* "Ward" - identities *)
psi[ru___Rule] :=
	If[ (Polarization /. {ru} /. Options[Twist2GluonOperator])===0,
		1,
		-1
	];

(* eq. (3A.13) *)
Twist2GluonOperator[{m_. Momentum[p_,___], Momentum[p_,___]}, {q_,nu_}, {k_,la_},  opt:OptionsPattern[] ] :=
	(m (Twist2GluonOperator[q, {nu}, {la}, opt] - psi[opt] Twist2GluonOperator[k, {nu}, {la}, opt]) ) /; (m^2 === 1) &&
	$OPEWard =!= False && (zerom[opt]);

Twist2GluonOperator[{p_,mu_}, {m_. Momentum[q_,___], Momentum[q_,___]}, {k_,la_}, opt:OptionsPattern[]] :=
(* careful: in the polarized case there is now a global minus sign *)
	(m psi[opt] * (Twist2GluonOperator[k, {mu}, {la}, opt] -
	psi[opt] Twist2GluonOperator[p, {mu}, {la}, opt])) /; (m^2 === 1) &&
	$OPEWard =!= False;

Twist2GluonOperator[{p_,mu_}, {q_,nu_}, {m_. Momentum[k_,___], Momentum[k_,___]}, opt:OptionsPattern[] ] :=
	(m (Twist2GluonOperator[p, {mu}, {nu}, opt] -
	psi[opt] Twist2GluonOperator[q, {mu}, {nu}, opt]) ) /; (m^2 === 1) &&
	$OPEWard =!= False && (zerom[opt]);

(* eq. (3A.16) *)

Twist2GluonOperator[{m_. Momentum[p_,___], Momentum[p_,___]}, {q_,nu_}, {k_,la_},  {s_,si_}, opt:OptionsPattern[]] :=
	(1/m ( Twist2GluonOperator[{-k-s,nu}, {k,la}, {s,si}, opt] - Twist2GluonOperator[{q,nu}, {k,la}, {s,si}, opt]))/;
	IntegerQ[m] &&  $OPEWard =!= False;

Twist2GluonOperator[{p_,mu_}, {m_. Momentum[q_,___], Momentum[q_,___]}, {k_,la_},  {s_,si_}, opts:OptionsPattern[]] :=
	(1/m ( -Twist2GluonOperator[{-k-s,mu}, {k,la}, {s,si}, opts] +
	Twist2GluonOperator[{p,mu}, {k,la}, {s,si}, opts]))/;
	IntegerQ[m] &&  $OPEWard =!= False;

Twist2GluonOperator[{pi_, mui_, ai_}, {qi_, nui_, bi_}, {ki_, lai_, ci_}, opt:OptionsPattern[]] :=
	Block[ {coup, dim, pol, p, q, k, mu, nu, la, a, b, c, re, threegluon, m,l},
		coup  = OptionValue[CouplingConstant];
		dim   = OptionValue[Dimension];
		pol   = OptionValue[Polarization];
		m[y_] :=
			Mo[y,dim];
		l[y_] :=
			Li[y,dim];
		del = Momentum[OPEDelta, dim];
		{a,b,c}    = Map[SUNIndex[#]&, {ai,bi,ci}];
		If[ pol === 0,
			re = -I coup ((1 + Power2[(-1),OPEm])/2) SUNF[a,b,c] *
			Twist2GluonOperator[{pi//m, mui//l}, {qi//m, nui//l},
			{ki//m, lai//l}, opt],
			If[ !AchmedRoss,
				re = coup (1 - Power2[(-1),OPEm]) SUNF[a,b,c] *
				Twist2GluonOperator[{pi//m, mui//l}, {qi//m, nui//l},
				{ki//m, lai//l}, opt],
				re = coup SUNF[a,b,c] *
				Twist2GluonOperator[{pi//m, mui//l},
				{qi//m, nui//l}, {ki//m, lai//l}, opt]
			]
		];
		re
	] /; (zerom[opt]);


simp[True][x_] :=
	ExpandScalarProduct[x /. Pair -> PairContract /.PairContract -> Pair];

simp[All][x_] :=
	Collect2[OPESumExplicit[ExpandScalarProduct[x /. Pair -> PairContract /.
	PairContract -> Pair]]/.{Power[a_,h_/;Head[h]=!=Integer] :> Power2[a,h]
	},{LorentzIndex,Momentum}];

Twist2GluonOperator[{pi_, mui_}, {qi_, nui_}, {ki_, lai_}, OptionsPattern[]] :=
	Block[ {coup, dim, pol, p, q, k, mu, nu, la, re, mm,ll,     threegluon, threegluonar},
		coup  = OptionValue[CouplingConstant];
		dim   = OptionValue[Dimension];
		pol   = OptionValue[Polarization];
		del = Momentum[OPEDelta, dim];
		mm[y_] :=
			Mo[y,dim];
		ll[y_] :=
			Li[y,dim];
		{p,q,k}    = Map[mm, {pi,qi,ki}];
		{mu,nu,la} = Map[ll, {mui,nui,lai}];
		If[ pol === 0,
			re =   (o3[mu,nu,la, p,q,k, del] +
			o3[nu,la,mu, q,k,p, del] +
			o3[la,mu,nu, k,p,q, del]),

			(*threegluon stuff*)


			If[ AchmedRoss,
				re = ACHMED (-1) threegluonar[{mu,p},{nu,q},{la,k}],
				re = threegluon[OptionValue[Explicit]][{mu,p},{nu,q},{la,k}]
			];
		];
		re//simp[OptionValue[Explicit]]
	] /; OptionValue[Explicit];

(* 4-gluon operator *)

Twist2GluonOperator[a1_, a2_, a3_, a4_, a5_, a6_, a7_, a8_, a9_,
							a10_, a11_, a12_, opt___Rule] :=
	Twist2GluonOperator[{a1,a2,a3},  {a4,a5,a6}, {a7,a8,a9}, {a10,a11,a12}, opt];

Twist2GluonOperator[{p_,mu_}, {q_,nu_},
							{k_,la_}, {s_,si_}, opt___Rule
						] :=
	Block[ {dlt, dim, m, l, pol},
		dim    = Dimension    /. {opt} /. Options[Twist2GluonOperator];
		pol    = Polarization /. {opt} /. Options[Twist2GluonOperator];
		dlt    = Momentum[OPEDelta, dim];
		m[y_] :=
			Mo[y,dim];
		l[y_] :=
			Li[y,dim];
		dlt = Momentum[OPEDelta, Dimension /. {opt} /.
										Options[Twist2GluonOperator]];
		If[ pol === 0,
			o415[mu//l,nu//l,la//l,si//l, p//m,q//m,k//m,s//m,dlt],
			pol4p[mu//l,nu//l,la//l,si//l, p//m,q//m,k//m,s//m,dlt]
		]//simp[Explicit /. {opt} /. Options[Twist2GluonOperator]]
	]/;
	(Explicit /. {opt} /. Options[Twist2GluonOperator]) =!= False;



			(* with the sums *)
			threegluon[True][{l1_,p1_}, {l2_,p2_}, {l3_,p3_}] :=
				-(OPESum[(-1)^OPEi*Pair[p3, del]^OPEi*
								Pair[del, p2]^(-3 - OPEi + OPEm), {OPEi, 0, -3 + OPEm}]*
							(Eps[l3, l2, del, p2]*Pair[p3, del] +
								Eps[l2, p3, del, p2]*Pair[l3, del])*Pair[l1, del] -
						OPESum[(-1)^OPEi*Pair[p3, del]^OPEi*
								Pair[del, p1]^(-3 - OPEi + OPEm), {OPEi, 0, -3 + OPEm}]*
							(Eps[l3, l1, del, p1]*Pair[p3, del] +
								Eps[l1, p3, del, p1]*Pair[l3, del])*Pair[l2, del] +
						Pair[p3, del]^(-2 + OPEm)*
							(Eps[l3, l2, p3, del]*Pair[l1, del] -
								Eps[l3, l1, p3, del]*Pair[l2, del]) +
						(Eps[l1, l2, del, p1]*Pair[l3, del] +
								Eps[l3, l1, del, p1]*Pair[l2, del])*
							Pair[del, p1]^(-2 + OPEm) +
						OPESum[(-1)^OPEi*Pair[del, p1]^OPEi*
								Pair[del, p2]^(-3 - OPEi + OPEm), {OPEi, 0, -3 + OPEm}]*
							Pair[l3, del]*(Eps[l2, del, p1, p2]*Pair[l1, del] -
								Eps[l1, l2, del, p2]*Pair[del, p1]) +
						(Eps[l1, l2, del, p2]*Pair[l3, del] -
								Eps[l3, l2, del, p2]*Pair[l1, del])*
							Pair[del, p2]^(-2 + OPEm));

(* the sums done *)
			threegluon[All][{l1_,p1_}, {l2_,p2_}, {l3_,p3_}] :=
				EpsEvaluate[
					-(Eps[l1, l2, del, p1]*Pair[del, l3]*Power2[Pair[del, p1], -2 + OPEm]) +
					(Eps[l2, del, p1, p2]*Pair[del, l1]*Pair[del, l3]*
							(Power2[-1, OPEm]*Power2[Pair[del, p1], -2 + OPEm] -
								Power2[Pair[del, p2], -2 + OPEm]))/(Pair[del, p1] + Pair[del, p2]) -
					(Eps[l1, l2, del, p2]*Pair[del, l3]*
							(Pair[del, p1]*Power2[-1, OPEm]*Power2[Pair[del, p1], -2 + OPEm] +
								Pair[del, p2]*Power2[Pair[del, p2], -2 + OPEm]))/
						(Pair[del, p1] + Pair[del, p2]) -
					Eps[l3, l2, p3, del]*Pair[del, l1]*Power2[Pair[del, p3], -2 + OPEm] +
					Eps[l3, l1, p3, del]*Pair[del, l2]*Power2[Pair[del, p3], -2 + OPEm] -
					(Eps[l1, p3, del, p1]*Pair[del, l2]*Pair[del, l3]*
							(-Power2[Pair[del, p1], -2 + OPEm] +
								Power2[-1, OPEm]*Power2[Pair[del, p3], -2 + OPEm]))/
						(Pair[del, p1] + Pair[del, p3]) +
					(Eps[l2, p3, del, p2]*Pair[del, l1]*Pair[del, l3]*
							(-Power2[Pair[del, p2], -2 + OPEm] +
								Power2[-1, OPEm]*Power2[Pair[del, p3], -2 + OPEm]))/
						(Pair[del, p2] + Pair[del, p3]) -
					(Eps[l3, l1, del, p1]*Pair[del, l2]*
							(Pair[del, p1]*Power2[Pair[del, p1], -2 + OPEm] +
								Pair[del, p3]*Power2[-1, OPEm]*Power2[Pair[del, p3], -2 + OPEm]))/
						(Pair[del, p1] + Pair[del, p3]) +
					(Eps[l3, l2, del, p2]*Pair[del, l1]*
							(Pair[del, p2]*Power2[Pair[del, p2], -2 + OPEm] +
								Pair[del, p3]*Power2[-1, OPEm]*Power2[Pair[del, p3], -2 + OPEm]))/
						(Pair[del, p2] + Pair[del, p3])
									];

(*This is Achmed & Ross ... ; really ...*)
threegluonar[{m_, k1_}, {n_, k2_}, {l_, k3_}] :=
				(
				Eps[m, n, del, k1]*Pair[del, l]*Power2[Pair[del, k1], -2 + OPEm] +
					Eps[l, n, del, k1]*Pair[del, m]*Power2[Pair[del, k1], -2 + OPEm] +
					Eps[l, m, del, k1]*Pair[del, n]*Power2[Pair[del, k1], -2 + OPEm] +
					Eps[m, n, del, k2]*Pair[del, l]*Power2[Pair[del, k2], -2 + OPEm] -
					Eps[l, n, del, k2]*Pair[del, m]*Power2[Pair[del, k2], -2 + OPEm] -
					Eps[l, m, del, k2]*Pair[del, n]*Power2[Pair[del, k2], -2 + OPEm] +
					(Eps[k2, m, n, del]*Pair[del, l]*
						(Power2[Pair[del, k1], -1 + OPEm] -
							Pair[del, k1]*Power2[-1, OPEm]*Power2[Pair[del, k2], -2 + OPEm]))/
					(Pair[del, k1] + Pair[del, k2]) -
					(Eps[k1, m, n, del]*Pair[del, l]*
						(Pair[del, k2]*Power2[-1, OPEm]*Power2[Pair[del, k1], -2 + OPEm] -
							Power2[Pair[del, k2], -1 + OPEm]))/(Pair[del, k1] + Pair[del, k2]) -
					Eps[m, n, del, k3]*Pair[del, l]*Power2[Pair[del, k3], -2 + OPEm] -
					Eps[l, n, del, k3]*Pair[del, m]*Power2[Pair[del, k3], -2 + OPEm] +
					Eps[l, m, del, k3]*Pair[del, n]*Power2[Pair[del, k3], -2 + OPEm] -
					(Eps[m, del, k1, k2]*Pair[del, l]*Pair[del, n]*
						(2*Pair[del, k1]*Power2[-1, OPEm]*Power2[Pair[del, k1], -2 + OPEm] +
							Pair[del, k2]*Power2[-1, OPEm]*Power2[Pair[del, k1], -2 + OPEm] +
							Pair[del, k3]*Power2[-1, OPEm]*Power2[Pair[del, k1], -2 + OPEm] -
							Pair[del, k1]*Power2[Pair[del, k2], -2 + OPEm] -
							Pair[del, k3]*Power2[Pair[del, k2], -2 + OPEm] -
							Pair[del, k1]*Power2[Pair[del, k3], -2 + OPEm] -
							Pair[del, k2]*Power2[Pair[del, k3], -2 + OPEm]))/
					((Pair[del, k1] + Pair[del, k2])*(Pair[del, k1] + Pair[del, k3])) +
					(Eps[n, del, k1, k2]*Pair[del, l]*Pair[del, m]*
						(Pair[del, k2]*Power2[Pair[del, k1], -2 + OPEm] +
							Pair[del, k3]*Power2[Pair[del, k1], -2 + OPEm] -
							Pair[del, k1]*Power2[-1, OPEm]*Power2[Pair[del, k2], -2 + OPEm] -
							2*Pair[del, k2]*Power2[-1, OPEm]*Power2[Pair[del, k2], -2 + OPEm] -
							Pair[del, k3]*Power2[-1, OPEm]*Power2[Pair[del, k2], -2 + OPEm] +
							Pair[del, k1]*Power2[Pair[del, k3], -2 + OPEm] +
							Pair[del, k2]*Power2[Pair[del, k3], -2 + OPEm]))/
					((Pair[del, k1] + Pair[del, k2])*(Pair[del, k2] + Pair[del, k3])) +
					(Eps[k3, l, m, del]*Pair[del, n]*
						(Power2[Pair[del, k1], -1 + OPEm] -
							Pair[del, k1]*Power2[-1, OPEm]*Power2[Pair[del, k3], -2 + OPEm]))/
					(Pair[del, k1] + Pair[del, k3]) -
					(Eps[k3, l, n, del]*Pair[del, m]*
						(Power2[Pair[del, k2], -1 + OPEm] -
							Pair[del, k2]*Power2[-1, OPEm]*Power2[Pair[del, k3], -2 + OPEm]))/
					(Pair[del, k2] + Pair[del, k3]) +
					(Eps[l, del, k1, k2]*Pair[del, m]*Pair[del, n]*
						(Pair[del, k2]*Power2[Pair[del, k1], -2 + OPEm] +
							Pair[del, k3]*Power2[Pair[del, k1], -2 + OPEm] +
							Pair[del, k1]*Power2[Pair[del, k2], -2 + OPEm] +
							Pair[del, k3]*Power2[Pair[del, k2], -2 + OPEm] -
							Pair[del, k1]*Power2[-1, OPEm]*Power2[Pair[del, k3], -2 + OPEm] -
							Pair[del, k2]*Power2[-1, OPEm]*Power2[Pair[del, k3], -2 + OPEm] -
							2*Pair[del, k3]*Power2[-1, OPEm]*Power2[Pair[del, k3], -2 + OPEm]))/
					((Pair[del, k1] + Pair[del, k3])*(Pair[del, k2] + Pair[del, k3])) -
					(Eps[k1, l, m, del]*Pair[del, n]*
						(Pair[del, k3]*Power2[-1, OPEm]*Power2[Pair[del, k1], -2 + OPEm] -
							Power2[Pair[del, k3], -1 + OPEm]))/(Pair[del, k1] + Pair[del, k3]) +
					(Eps[k2, l, n, del]*Pair[del, m]*
						(Pair[del, k3]*Power2[-1, OPEm]*Power2[Pair[del, k2], -2 + OPEm] -
							Power2[Pair[del, k3], -1 + OPEm]))/(Pair[del, k2] + Pair[del, k3]) -
					Eps[l, m, n, del]*(Power2[Pair[del, k1], -1 + OPEm] +
						Power2[Pair[del, k2], -1 + OPEm] + Power2[Pair[del, k3], -1 + OPEm])
				);


(* calculated by FC *)

pol4p[l1_, l2_, l3_, l4_, p1_, p2_, p3_, p4_, del_] :=
	(-(OPESum[(-1)^OPEi*(Pair[del, p1] + Pair[del, p2])^OPEi*
			Pair[del, p4]^(-3 - OPEi + OPEm), {OPEi, 0, -3 + OPEm}]*
		(Eps[l2, l4, del, p4]*Pair[del, l1] -
			Eps[l1, l4, del, p4]*Pair[del, l2])*Pair[del, l3]) +
	OPESum[(-1)^OPEi*(Pair[del, p1] + Pair[del, p2])^(-3 - OPEi + OPEm)*
		Pair[del, p3]^OPEi, {OPEi, 0, -3 + OPEm}]*
	(Eps[l3, l2, p3, del]*Pair[del, l1] - Eps[l3, l1, p3, del]*Pair[del, l2])*
	Pair[del, l4] - OPESum[(-1)^OPEi*Pair[del, p1]^OPEi*
		(Pair[del, p3] + Pair[del, p4])^(-3 - OPEi + OPEm),
		{OPEi, 0, -3 + OPEm}]*Pair[del, l2]*
	(Eps[l1, l4, del, p1]*Pair[del, l3] -
		Eps[l1, l3, del, p1]*Pair[del, l4]) +
		OPESum[(-1)^OPEi*Pair[del, p2]^OPEi*
		(Pair[del, p3] + Pair[del, p4])^(-3 - OPEi + OPEm),
		{OPEi, 0, -3 + OPEm}]*Pair[del, l1]*
	(Eps[l2, l4, del, p2]*Pair[del, l3] -
		Eps[l2, l3, del, p2]*Pair[del, l4]) -
		OPESum[(-1)^OPEj*Pair[del, p1]^(-4 - OPEj + OPEm)*Pair[del, p4]^OPEi*
		(Pair[del, p3] + Pair[del, p4])^(-OPEi + OPEj), {OPEi, 0, OPEj},
		{OPEj, 0, -4 + OPEm}]*Pair[del, l2]*Pair[del, l3]*
	(-(Eps[del, l4, p1, p4]*Pair[del, l1]) -
		Eps[l1, l4, del, p4]*Pair[del, p1]) +
	OPESum[(-1)^OPEj*Pair[del, p2]^(-4 - OPEj + OPEm)*Pair[del, p4]^OPEi*
		(Pair[del, p3] + Pair[del, p4])^(-OPEi + OPEj), {OPEi, 0, OPEj},
		{OPEj, 0, -4 + OPEm}]*Pair[del, l1]*Pair[del, l3]*
	(-(Eps[del, l4, p2, p4]*Pair[del, l2]) -
		Eps[l2, l4, del, p4]*Pair[del, p2]) +
	OPESum[(-1)^OPEj*Pair[del, p1]^(-4 - OPEj + OPEm)*Pair[del, p3]^OPEi*
		(Pair[del, p3] + Pair[del, p4])^(-OPEi + OPEj), {OPEi, 0, OPEj},
		{OPEj, 0, -4 + OPEm}]*Pair[del, l2]*Pair[del, l4]*
	(-(Eps[del, l1, p1, p3]*Pair[del, l3]) -
		Eps[l1, l3, del, p1]*Pair[del, p3]) -
	OPESum[(-1)^OPEj*Pair[del, p2]^(-4 - OPEj + OPEm)*Pair[del, p3]^OPEi*
		(Pair[del, p3] + Pair[del, p4])^(-OPEi + OPEj), {OPEi, 0, OPEj},
		{OPEj, 0, -4 + OPEm}]*Pair[del, l1]*Pair[del, l4]*
	(-(Eps[del, l2, p2, p3]*Pair[del, l3]) -
		Eps[l2, l3, del, p2]*Pair[del, p3]) +
	(Eps[del, l2, l3, l4]*Pair[del, l1] - Eps[del, l1, l3, l4]*Pair[del, l2])*
	(Pair[del, p3] + Pair[del, p4])^(-2 + OPEm))/. { ((-1)^OPEi a_^OPEi) :> (-a)^OPEi,
			((-1)^OPEj a_^(OPEj-OPEi) b_^OPEi) :>
				((-a)^(OPEj-OPEi) (-b)^OPEi)
		};


(* calculated by FC *)
o415[l1_, l2_, l3_, l4_, p1_, p2_, p3_, p4_, del_
		] :=
	(
	(Pair[del, p3] + Pair[del, p4])^(-2 + OPEm)*
	(Pair[del, l2]*Pair[del, l4]*Pair[l1, l3] -
	Pair[del, l2]*Pair[del, l3]*Pair[l1, l4] -
	Pair[del, l1]*Pair[del, l4]*Pair[l2, l3] +
	Pair[del, l1]*Pair[del, l3]*Pair[l2, l4]) +
	OPESum[(-1)^OPEi*(Pair[del, p1] + Pair[del, p2])^(-3 - OPEi + OPEm)*
	Pair[del, p3]^OPEi, {OPEi, 0, -3 + OPEm}]*Pair[del, l4]*
	(-(Pair[del, l2]*Pair[del, p3]*Pair[l1, l3]) +
	Pair[del, l2]*Pair[del, l3]*Pair[l1, p3] +
	Pair[del, l1]*Pair[del, p3]*Pair[l2, l3] -
	Pair[del, l1]*Pair[del, l3]*Pair[l2, p3]) +
	OPESum[(-1)^OPEi*(Pair[del, p1] + Pair[del, p2])^OPEi*
	Pair[del, p4]^(-3 - OPEi + OPEm), {OPEi, 0, -3 + OPEm}]*Pair[del, l3]*
	(-(Pair[del, l2]*Pair[del, p4]*Pair[l1, l4]) +
	Pair[del, l2]*Pair[del, l4]*Pair[l1, p4] +
	Pair[del, l1]*Pair[del, p4]*Pair[l2, l4] -
	Pair[del, l1]*Pair[del, l4]*Pair[l2, p4]) +
	OPESum[(-1)^OPEi*Pair[del, p1]^OPEi*
	(Pair[del, p3] + Pair[del, p4])^(-3 - OPEi + OPEm),
	{OPEi, 0, -3 + OPEm}]*Pair[del, l2]*
	(-(Pair[del, l4]*Pair[del, p1]*Pair[l1, l3]) +
	Pair[del, l3]*Pair[del, p1]*Pair[l1, l4] +
	Pair[del, l1]*Pair[del, l4]*Pair[l3, p1] -
	Pair[del, l1]*Pair[del, l3]*Pair[l4, p1]) -
	OPESum[(-1)^OPEi*Pair[del, p2]^OPEi*
	(Pair[del, p3] + Pair[del, p4])^(-3 - OPEi + OPEm),
	{OPEi, 0, -3 + OPEm}]*Pair[del, l1]*
	(-(Pair[del, l4]*Pair[del, p2]*Pair[l2, l3]) +
	Pair[del, l3]*Pair[del, p2]*Pair[l2, l4] +
	Pair[del, l2]*Pair[del, l4]*Pair[l3, p2] -
	Pair[del, l2]*Pair[del, l3]*Pair[l4, p2]) -
	OPESum[(-1)^OPEj*Pair[del, p1]^(-4 - OPEj + OPEm)*Pair[del, p3]^OPEi*
	(Pair[del, p3] + Pair[del, p4])^(-OPEi + OPEj), {OPEi, 0, OPEj},
	{OPEj, 0, -4 + OPEm}]*Pair[del, l2]*Pair[del, l4]*
	(Pair[del, p1]*Pair[del, p3]*Pair[l1, l3] -
	Pair[del, l3]*Pair[del, p1]*Pair[l1, p3] -
	Pair[del, l1]*Pair[del, p3]*Pair[l3, p1] +
	Pair[del, l1]*Pair[del, l3]*Pair[p1, p3]) +
	OPESum[(-1)^OPEj*Pair[del, p1]^(-4 - OPEj + OPEm)*Pair[del, p4]^OPEi*
	(Pair[del, p3] + Pair[del, p4])^(-OPEi + OPEj), {OPEi, 0, OPEj},
	{OPEj, 0, -4 + OPEm}]*Pair[del, l2]*Pair[del, l3]*
	(Pair[del, p1]*Pair[del, p4]*Pair[l1, l4] -
	Pair[del, l4]*Pair[del, p1]*Pair[l1, p4] -
	Pair[del, l1]*Pair[del, p4]*Pair[l4, p1] +
	Pair[del, l1]*Pair[del, l4]*Pair[p1, p4]) +
	OPESum[(-1)^OPEj*Pair[del, p2]^(-4 - OPEj + OPEm)*Pair[del, p3]^OPEi*
	(Pair[del, p3] + Pair[del, p4])^(-OPEi + OPEj), {OPEi, 0, OPEj},
	{OPEj, 0, -4 + OPEm}]*Pair[del, l1]*Pair[del, l4]*
	(Pair[del, p2]*Pair[del, p3]*Pair[l2, l3] -
	Pair[del, l3]*Pair[del, p2]*Pair[l2, p3] -
	Pair[del, l2]*Pair[del, p3]*Pair[l3, p2] +
	Pair[del, l2]*Pair[del, l3]*Pair[p2, p3]) -
	OPESum[(-1)^OPEj*Pair[del, p2]^(-4 - OPEj + OPEm)*Pair[del, p4]^OPEi*
	(Pair[del, p3] + Pair[del, p4])^(-OPEi + OPEj), {OPEi, 0, OPEj},
	{OPEj, 0, -4 + OPEm}]*Pair[del, l1]*Pair[del, l3]*
	(Pair[del, p2]*Pair[del, p4]*Pair[l2, l4] -
	Pair[del, l4]*Pair[del, p2]*Pair[l2, p4] -
	Pair[del, l2]*Pair[del, p4]*Pair[l4, p2] +
	Pair[del, l2]*Pair[del, l4]*Pair[p2, p4])
	) /. { ((-1)^OPEi a_^OPEi) :> (-a)^OPEi,
	((-1)^OPEj a_^(OPEj-OPEi) b_^OPEi) :>
	((-a)^(OPEj-OPEi) (-b)^OPEi)
	};

(*4GLUON*)
Twist2GluonOperator[{pi_, mui_, ai_}, {qi_, nui_, bi_}, {ki_, lai_, ci_}, {sI_, sii_, di_}, opt:OptionsPattern[]] :=
	Block[ {coup,dim, pol, del, p, q, k, s, mu, nu, la, si, a, b, c, d, e, re,m,l},
		coup = OptionValue[CouplingConstant];
		dim  = OptionValue[Dimension];
		pol  = OptionValue[Polarization];
		del = Momentum[OPEDelta, dim];
		m[y_] :=
			Mo[y, dim];
		l[y_] :=
			Li[y, dim];
		{p,q,k,s}     = Map[m, {pi,qi,ki,sI}];
		{mu,nu,la,si} = Map[l, {mui,nui,lai,sii}];
		{a,b,c,d}     = Map[SUNIndex[#]&, {ai,bi,ci,di}];
		e = SUNIndex[FCGV[ToString[Unique["c"]]]];
		If[ pol === 0,
			re =  coup^2 ((1 + (-1)^OPEm)/2) *
			(SUNF[a,b,e] SUNF[c,d,e]  *
			Twist2GluonOperator[{p,mu},{q,nu},{k,la},{s,si}, opt] +
			SUNF[a,c,e] SUNF[b,d,e]  *
			Twist2GluonOperator[{p,mu},{k,la},{q,nu},{s,si}, opt] -
			SUNF[a,d,e] SUNF[b,c,e] *
			Twist2GluonOperator[{k,la}, {q,nu}, {p,mu}, {s,si}, opt]),
			re = I coup^2 ((1 - (-1)^OPEm)) *
			(SUNF[a,b,e] SUNF[c,d,e] *
			Twist2GluonOperator[{p,mu},{q,nu},{k,la},{s,si}, opt] +
			SUNF[a,c,e] SUNF[b,d,e] *
			Twist2GluonOperator[{p,mu},{k,la},{q,nu},{s,si}, opt] -
			SUNF[a,d,e] SUNF[b,c,e] *
			Twist2GluonOperator[{k,la}, {q,nu}, {p,mu}, {s,si}, opt])
		];
		re
	];

Twist2GluonOperator /:
	MakeBoxes[Twist2GluonOperator[{p1_, m1_}, {p2_, m2_}, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubsuperscriptBox["O", RowBox[{TBox[m1],"\[VeryThinSpace]",
		TBox[m2]}], "G2"],RowBox[{"("}],TBox[p1], "," ,TBox[p2],RowBox[{")"}]}];

Twist2GluonOperator /:
	MakeBoxes[Twist2GluonOperator[p_, {mu_}, {nu_}, OptionsPattern[]], TraditionalForm] :=
		RowBox[{SubsuperscriptBox["O", RowBox[{TBox[mu],"\[VeryThinSpace]", TBox[nu]}],
		"G2"], RowBox[{"("}], TBox[p],RowBox[{")"}]}];

Twist2GluonOperator /:
	MakeBoxes[Twist2GluonOperator[{p1_, m1_}, {p2_, m2_}, {p3_,m3_}, OptionsPattern[]],
	TraditionalForm] :=
		RowBox[{SubsuperscriptBox["O", RowBox[ {TBox[m1],"\[VeryThinSpace]", TBox[m2],
		"\[VeryThinSpace]", TBox[m3]}], "G3"],RowBox[{"("}], TBox[p1],",",TBox[p2],",",TBox[p3],
		RowBox[{")"}]}];

Twist2GluonOperator /:
	MakeBoxes[Twist2GluonOperator[{p1_, m1_}, {p2_, m2_}, {p3_,m3_}, {p4_,m4_},OptionsPattern[]],
	TraditionalForm] :=
		RowBox[{SubsuperscriptBox["O", RowBox[ {TBox[m1],"\[VeryThinSpace]", TBox[m2],
		"\[VeryThinSpace]", TBox[m3], "\[VeryThinSpace]", TBox[m4]}], "G4"],
		RowBox[{"("}], TBox[p1],",",TBox[p2],",",TBox[p3],",",TBox[p4],    RowBox[{")"}]}];

FCPrint[1,"Twist2GluonOperator.m loaded"];
End[]
