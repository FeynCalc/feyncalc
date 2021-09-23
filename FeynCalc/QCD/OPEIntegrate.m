(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: OPEIntegrate *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 18 April '98 at 11:25 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: several integrals *)

(* ------------------------------------------------------------------------ *)

OPEIntegrate::usage =
"OPEIntegrate[expr, q, x] calculates a one-loop OPE-type integral.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`OPEIntegrate`Private`"]

Options[OPEIntegrate] = {
	Dimension -> D,
	Divideout -> 1,
	EpsContract -> 4,
	EpsilonOrder -> False,
	Factoring -> True,
	Factorout -> 1,
	FinalSubstitutions -> {D -> (4+Epsilon)},
	OPEIntegrateDelta -> False
};

OPEIntegrate[exp_, kk_, x_, opt___Rule] :=
	Block[ {facout,nex, nex0, epsc, epscc,factorout, divout,lcol,power3,dim,
	epscdi, intfake, irules0, irules1, irules2, irulesmassive, nfacfix,
	n, k, ka, p2, fp, sumk, sumk0, sumk1,de, dufa, powsub, opgeom, qqq,
	null1, null2, qse, dUMMYM, mUuU, nUuU, finsu,flowerpower,delcol,
	locepsilon, floweps, epsorder, nfa, noflow, opm, apa, dE, xXx, qqqk,
	sumbinom, sumbinomback, ki, null, nex1, muUU, nfax
	},
		facout = 1;
		n  = Dimension /. {opt} /. Options[OPEIntegrate];
		k  = Momentum[kk, n];
		ka = Momentum[kk, ___];
		de = Momentum[OPEDelta, n];
		finsu     = FinalSubstitutions /. {opt} /. Options[OPEIntegrate];
		factorout = Factorout /. {opt} /. Options[OPEIntegrate];
		divout    = Divideout /. {opt} /. Options[OPEIntegrate];
		epscdi    = EpsContract /. {opt} /. Options[OPEIntegrate];
		If[ epscdi =!= 4,
			SetOptions[Eps, Dimension -> epscdi]
		];
		sumk0 /: (y_ /; !FreeQ[y, kk]) sumk0[aaa_, b_] := sumk0[aaa y, b];
		sumk1[a_Times, b_] :=
			sumk[qqq[SelectNotFree[a, kk]],b
										] (a/SelectNotFree[a,kk]);
		(*
		( *
		*)
		powsub[xx_] :=
			xx /. (a_/;(Head[a]===Plus) && (Length[a]>2)
									)^(w_/;Head[w]=!=Integer) :>
									(Isolate[Collect2[a,kk],kk, IsolateNames->loc
													]^w) /.
									{
									(((nok_/;FreeQ[nok,k]) - Pair[de,k])^
										a_Integer?Negative
									) :>
									(sumk0[Pair[de,k]^(-a OPEk) /
													nok^(-a (OPEk+1)), {OPEk,0,Infinity}
												]
									)^(-a),
									( (p_. Pair[de, k] + (nok_ /; FreeQ[nok,k])
										)^a_Integer?Negative
									) :>
									(-sumk0[p^(-a OPEk) Pair[de,k]^(-a OPEk) /
													(-nok)^(-a (OPEk+1)), {OPEk,0,Infinity}
													]
									),
			(* NEW 08/95 *)
										((p_. Pair[de,k] + m_ )^a_/;Head[a] =!= Integer
										) :>
										(ki = Unique[OPEi];
										p^a sumbinom[a,OPEk[ki]] Pair[de,k]^OPEk[ki] *
												PowerSimplify[(m/p)^(a-OPEk[ki])]
										)

			(* ueberfluessig (01/95)
			,
									(Pair[de,k]^m_ (Pair[de,p_] - Pair[de,k])^a_) :>
									(sumk0[Pair[de,k]^(OPEk+m/(-a)) /
													Pair[de,p]^(OPEk+1), {OPEk,0,Infinity}
												]
										)^(-a),
									(Pair[de,k]^m_ (Pair[de,k] - Pair[de,p_])^a_) :>
									(-sumk0[Pair[de,k]^(OPEk+m/(-a)) /
													Pair[de,p]^(OPEk+1), {OPEk,0,Infinity}
												]
										)^(-a)
			*)
									} /. sumk0 -> sumk;
		opgeom[xy_,aa_] :=
			sumk[powsub[xy], aa];
		(*
		* )
		*)
		nex = PowerSimplify[FeynCalcInternal[exp] /. Power2->Power];
		nex = Expand[ChangeDimension[nex, n] /. OPESum -> sumk0 /.
											sumk0 -> sumk1 /. sumk1 -> sumk,  kk];

		(* amputate *)
		mUuU = LorentzIndex[FCGV[ToString[Unique["li"]]], n];
		nUuU = LorentzIndex[FCGV[ToString[Unique["li"]]], n];
		nex = nex /. Eps[a___, Momentum[kk,(*dii*)___], b___]^2 :>
								(Eps[a, mUuU, b] * Pair[Momentum[kk, n], mUuU] *
									Eps[a, nUuU, b] * Pair[Momentum[kk, n], nUuU]
								);
		qqqk[yy_Times] :=
			SelectFree[yy, kk] qqq[SelectNotFree[yy, kk]];
		nex = nex /. qqq -> qqqk /. qqqk -> qqq;

		(* do the 1-part too *)
		nex = nex  + null[1] + null[2];
		nex0 = SelectNotFree[nex, Pair[de,k]^(em_ /; (Head[em]=!=Integer))];
		nex1 = (nex - nex0) /. {null[1] :> 0, null[2] :> 0};
		nex1 = Expand[nex1 Pair[de,k]^(dUMMYM), kk];
		nex = nex0 + nex1;
		qse[null1] = qse[null2] = 0;
		qse[a_ sumk[b_, c__]] :=
			a sumk[qse[b], c];
		qse[sumk[b_,c__]] :=
			sumk[qse[b], c];
		qse[a_] :=
			If[ !FreeQ[a, sumk],
				a,
				If[ Head[a] =!= Times,
					a,
					qqq[SelectNotFree[a, kk]] (a/SelectNotFree[a, kk])
				]
			];
		nex = powsub[nex /. sumk -> opgeom];
		nex = Map[qse, nex + null1 + null2];
		If[ Head[nex] === Plus,
			nex = (# dufa)& /@ nex,
			nex = dufa nex
		];
		p2[pe_] :=
			Pair[pe,pe];
		fp[pe_] :=
			fp[pe] = (-p2[pe]/ScaleMu^2)^Expand[(n/2 - 2)];
		irules0 = {
				_. FeynAmpDenominator[PropagatorDenominator[k, 0]..]:>0,
				_. FeynAmpDenominator[PropagatorDenominator[k+ _, 0]]:>0,
		(* i1munu *)
		qqq[ Pair[de, k]^m_ fun1_[a___, ka, b___Momentum] *
			fun2_[aa___,ka,bb___Momentum] *
										FeynAmpDenominator[
										PropagatorDenominator[k, 0], PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p_, 0]
																			]
			]:>
		(muUU = LorentzIndex[Unique[$MU], n];
		fp[p] ( Pair[de, p]^m  x^(m-1) (x (1-x))^(n/2-2) *
		(*g_munu*)
			( Contract[fun1[a, muUU, b] fun2[aa, muUU, bb]] (1/2) Gamma[2-n/2] x*
				(1-x) +
		(*p_mu p_nu*)
		EpsEvaluate[fun1[a,p,b] fun2[aa,p,bb]]  Gamma[3-n/2] x^2/p2[p] +
		EpsEvaluate[(fun1[a,p,b] fun2[aa,de,bb] + fun1[a,de,b] fun2[aa,p,bb]
								) / Pair[de, p]] (1/2) (Gamma[2-n/2]-Gamma[3-n/2]) (2x^2-x)+
		1/4 (Gamma[3-n/2] (1-4 x + 4 x^2) + Gamma[2-n/2] (4 x - 6 x^2)) *
		fun1[a,de,b] fun2[aa,de,bb] p2[p]/Pair[de,p]^2)
		)) /; ( ((fun1 === Eps) || (fun1 === Pair)) &&
						((fun2 === Eps) || (fun2 === Pair))
					),

		qqq[ Pair[de, ka]^m_ fun1_[a___, ka, b___Momentum] *
			fun2_[aa___,k,bb___Momentum] *
										FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p_, 0],
										PropagatorDenominator[k - p_, 0]
																			]
			] :>
		(muUU = LorentzIndex[Unique[$MU], n];

		(*i6munu*)
		fp[p] EpsEvaluate[
		( ((2 - n)*x^(-3 + m)*((1 - x)*x)^(n/2)*
				(4 - n - 12*x + 4*n*x + 12*x^2 - 4*n*x^2)*
		(*
		FV[OPEDelta, MU]* FV[OPEDelta, NU]*
		 *)
				fun1[a, de, b] fun2[aa, de, bb] *
				Gamma[2 - n/2]*
				Pair[de, p]^(-2 + m))/(16*(1 - x)^3) +
			((4 - n)*(6 - n)*x^(-1 + m)*((1 - x)*x)^(n/2)*
			Gamma[2 - n/2]*
		(*
		FV[p, MU]* FV[p, NU]*
		 *)
				fun1[a, p, b] fun2[aa, p, bb] *
				Pair[de, p]^m)/
			(4*(1 - x)^3*Pair[p, p]^2) +
			((4 - n)*x^(-2 + m)*((1 - x)*x)^(n/2)*(2 - n - 6*x + 2*n*x)*
		(*
		(FV[OPEDelta, NU]*FV[p, MU] +
		FV[OPEDelta, MU]*FV[p, NU])*
		 *)
				(fun1[a,p,b] fun2[aa,de,bb] + fun1[a,de,b] fun2[aa,p,bb]
				)*
			Gamma[2 - n/2]*
				Pair[de, p]^(-1 + m))/
			(8*(1 - x)^3*Pair[p, p]) +
			((4 - n)*x^(-2 + m)*((1 - x)*x)^(n/2)*Gamma[2 - n/2]*
		(*
		MT[MU, NU]*
		 *)
				Contract[fun1[a,muUU,b] fun2[aa, muUU, bb]]*
				Pair[de, p]^m)/
			(4*(1 - x)^2*Pair[p, p])
					)]
		)
							};
		irules1 = {
		(*i6murule*)
		qqq[
		Pair[de, ka]^m_ fun_[a___, ka, b___Momentum] FeynAmpDenominator[
										PropagatorDenominator[k, 0], PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p_, 0],
										PropagatorDenominator[k - p_, 0]              ]
			]:>
		fp[p] ( 1/p2[p]^2 Pair[de, p]^m  x^(m-1) Gamma[3 - n/2] (x (1-x))^(n/2-2) *
		( (1 - (n-4)/2) x/(1-x) EpsEvaluate[fun[a, p, b]] +
			p2[p]/Pair[de, p] EpsEvaluate[fun[a, de, b]] (n-4)/4 (2x-1)/(1-x)
			)
		),

		(*i2murule*)
		qqq[
		Pair[de, ka]^m_ fun_[a___, ka, b___Momentum] FeynAmpDenominator[
										PropagatorDenominator[k, 0], PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p_, 0]         ]
			] :> (fp[p] (  (Pair[de, p]^m  x^(m-1) (x (1-x))^(n/2-2) *
				( Gamma[3 - n/2] x EpsEvaluate[fun[a, p, b]]/p2[p]   +
					1/2 (Gamma[2 - n/2] x + Gamma[3 - n/2] (1 - 2 x)
							)  EpsEvaluate[fun[a, de, b]]/Pair[de, p]
				)  ) )) /; ( (fun === Eps) || (fun === Pair)),
		(*i5murule = *)
		qqq[
		Pair[de, ka]^m_ fun_[a___,ka, b___Momentum] FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p_, 0]         ]
			]:>
									(fp[p] (Pair[de, p]^m  x^(m-1) (x (1-x))^(n/2-2) *
				( Gamma[2 - n/2] x^2  EpsEvaluate[fun[a, p, b]]   +
					1/2 Gamma[2 - n/2] (x - 2 x^2)  *
							EpsEvaluate[fun[a, de, b]] p2[p] /Pair[de, p]
				)       )) /; ( (fun === Eps) || (fun === Pair)),
		qqq[
		Pair[de, ka]^m_ fun_[a___, ka, b___Momentum]  FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p_, 0],
										PropagatorDenominator[k - p_, 0]
																			]
			]:> (fp[p] (
							Pair[de, p]^m  x^(m-1) (x^2/(1-x)) (x (1-x))^(n/2-2) *
							( Gamma[3 - n/2] / p2[p] )  fun[a, p, b] +
		(* B *)
							Pair[de, p]^m  / Pair[de, p]/2 x^(m-1) x (1 - 2 x)/(1 - x) *
							(x (1-x))^(n/2-2) ( Gamma[3 - n/2] ) fun[a, de, b] -
							Pair[de, p]^m  / Pair[de, p]/2 x^(m-1) x *
							(x (1-x))^(n/2-2) ( Gamma[2 - n/2] ) fun[a, de, b]
																					)
						) /;        ( (fun === Eps) || (fun === Pair))
							};
		irules2 = {
		(*i3rule = *)
		qqq[
		Pair[de, ka]^m_  FeynAmpDenominator[
										PropagatorDenominator[k, 0], PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p_, 0]         ]
			]:> (fp[p] Pair[de, p]^m  x^(m-1) (x (1-x))^(n/2-2) *
						( Gamma[3 - n/2] / p2[p] )
					),
		(*i4rule = *)
		qqq[
		Pair[de, ka]^m_  FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p_, 0]]
			] :> ( fp[p] Pair[de, p]^m  x^(m-1) (x (1-x))^(n/2-2) *
							( Gamma[2 - n/2] x )
						),
		(* newrule1 *)
		qqq[
		Pair[de, ka]^m_  FeynAmpDenominator[
										PropagatorDenominator[k - p1_, 0],
										PropagatorDenominator[k - p3_, 0]]
			] :> (fp[p1-p3] (-2)/Epsilon Gamma[1-Epsilon/2] *
						(x (1-x))^(Epsilon/2) (x Pair[de, p1] +
																(1-x) Pair[de, p3]
																	)^m
						) /; p1 =!= p3,

		qqq[
		Pair[de, ka]^m_  FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p_, 0],
										PropagatorDenominator[k - p_, 0]
																			]
			]:> (fp[p] (
							Pair[de, p]^m  x^(m-1) (x/(1-x)) (x (1-x))^(n/2-2) *
							( Gamma[3 - n/2] / p2[p] )      )
					),
		(* i6rule *)
		qqq[
		Pair[de, ka]^m_  FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p_, 0],
										PropagatorDenominator[k - p_, 0]
																			]
			]:> (fp[p] (1/p2[p]^2 *
							Pair[de, p]^m Gamma[3-n/2] x^(m-1) (x (1-x))^(n/2-2)*
							1/(1-x)  (1 - (n-4)/2)       )
					)
			};
		nos[0] = False;
		nos[eem_] :=
			If[ !FreeQ[eem, SmallVariable],
				False,
				True
			];
		irulesmassive = {
		qqq[Pair[de, ka]^m_ FeynAmpDenominator[PropagatorDenominator[k, e_ /; nos[e]], PropagatorDenominator[k - p_, 0]]] :>
			(- I fp[p] e^(Epsilon/2) ( 2/Epsilon Pair[de, p]^m Gamma[1-Epsilon/2] x^m *
				(1-x)^Epsilon)) /. p2[p] -> e^2 /. ScaleMu -> (-I e),

		qqq[Pair[de, ka]^m_ FeynAmpDenominator[
										PropagatorDenominator[k, em_ /; nos[em]],
										PropagatorDenominator[k, em_ /; nos[em]],
										PropagatorDenominator[k - p_, 0]
																			]
			] :> (I  fp[p] ( - Pair[de, p]^m Gamma[1-Epsilon/2] x^m/(1-x) *
							(1-x)^Epsilon (em^2)^( - 1)
						)),
		(* page 5 *)
		qqq[
		Pair[de, ka]^m_ FeynAmpDenominator[
										PropagatorDenominator[k, em_ /; nos[em]],
										PropagatorDenominator[k, em_ /; nos[em]],
										PropagatorDenominator[k - p_, SmallVariable[lam_]]
																			]
			] :> (I fp[p] (
						- Gamma[1-Epsilon/2] Pair[de, p]^m x^m *
							(em^2)^(Epsilon/2 - 1) ((1-x)^(Epsilon-1) +
							DeltaFunction[1-x] (-1/2 Log[SmallVariable[lam]^2/em^2] +
																	Log[SmallDelta])
																			)
						)),
		(* page 8 *)

		qqq[
		Pair[de, ka]^m_ fun_[a___, ka, b___Momentum] *
		FeynAmpDenominator[
										PropagatorDenominator[k, em_ /; nos[em]],
										PropagatorDenominator[k - p_, 0]
											]
			] :> (I fp[p] (x^m Pair[de,ka]^m Gamma[2-n/2] (1-x)^(n-4) *
						(x fun[a, p, b] + (1-x) fun[a,de,b] em^2/Pair[de,ka]
						)
						)),

		(* page 9 *)
		qqq[
		Pair[de, ka]^m_ fun_[a___, ka, b___Momentum] *
		FeynAmpDenominator[
										PropagatorDenominator[k, em_ /; nos[em]],
										PropagatorDenominator[k, em_ /; nos[em]],
										PropagatorDenominator[k - p_, 0]
											]
			] :> (I fp[p] *
		( - Gamma[3-n/2] Pair[de,p]^m fun[a,p,b] x^m (1-x)^(n-4) *
				x/(1-x) (m^2)^(Epsilon/2-1) -
				Gamma[3-n/2] Pair[de,p]^(m-1) fun[a,de,b] x^m (1-x)^(n-4) +
				1/2 Gamma[2-n/2] Pair[de,p]^(m-1) fun[a,de,b]  x^m (1-x)^(n-4)
		)       ) ,
		qqq[ Pair[de, k]^m_ fun1_[a___, ka, b___Momentum] *
			fun2_[aa___,ka,bb___Momentum] *
										FeynAmpDenominator[
										PropagatorDenominator[k, em_ /; nos[em]],
										PropagatorDenominator[k, em_ /; nos[em]],
										PropagatorDenominator[k - p_, 0]
																			]
			]:>
		(muUU = LorentzIndex[Unique[$MU], n];
		(I fp[p] * (
		x^m Pair[de,p]^m  (1-x)^(n-4) *
		(
		-Gamma[3-n/2] x^2/(1-x) fun1[a,p,b] fun2[aa,p,bb]/em^2 +
		1/2 Gamma[2-n/2] *
		Contract[fun1[a,muUU,b] fun2[aa, muUU, bb]] (1-x) +
		+ Contract[ fun1[a,de,b] fun2[aa,p,bb] +
								fun1[a,p,b]  fun2[aa,de,bb]
							]/Pair[de,p] *
			(-Gamma[3-n/2] x + Gamma[2-n/2] (x-1/2) ) +
		Contract[fun1[a,de,b] fun2[aa,de,bb]]/Pair[de,p]^2 em^2 *
		(-Gamma[3-n/2] (1-x) + 3/2 (1-x) Gamma[2-n/2])
		)
						)
		))
										};
		nex =
		nex//.irules0 //. irules1 //. irules2 /. irulesmassive /.
		p -> (-p) //.irules0 //. irules1 //. irules2 /. irulesmassive /.
		p -> (-p) /.
		dufa -> 1 /.qqq->Identity /.
											{sumk :> OPESum, dUMMYM :> 0};
		If[ !FreeQ[nex,sumbinom],
			FCPrint[2,"doing binomial sums"];
			sumbinomback[a_Plus] :=
				Map[sumbinomback, a];
			sumbinomback[y_ sumbinom[i_,j_]] :=
				sumbinomback[y sumbinom[i,j]] = Sum[y Binomial[i,j],{j,0,i}];
			nex = sumbinomback[nex]/.sumbinomback->Identity;
			FCPrint[2,"binomial sums done "];
		];
		epsc[Eps[ww__]^2] :=
			epsc[Eps[ww]^2] =
			Contract[ChangeDimension[Eps[ww],epscdi]^2];
		If[ epscdi =!= 4,
			nex = nex / ((epscdi - 3) (epscdi - 2) /2)
		];
		nex = nex /. Eps[uu__]^2 :> epscc[Eps[uu]^2];
		nex = nex /. epscc -> epsc;
		If[ !FreeQ2[nex, {Eps,LorentzIndex}],
			nex = Contract[nex, EpsContract -> True]
		];
		If[ !FreeQ[nex, OPESum],
			If[ !FreeQ[nex, ScaleMu],
				nex = nex /. (a_ / ScaleMu^2)^b_ :>
				power4[a/ScaleMu^2,b]
			];
			nex = OPESumExplicit[nex//OPESumSimplify]//Factor2;
			nex =  nex /. power4 -> Power
		];
		nex = nex /.  Gamma[3-n/2] -> (Gamma[2-n/2] -
									(n/2-1) Gamma[2-n/2]
																	);
		nex = nex /. finsu /. Gamma -> gammaex /.
					((n/2)/.finsu) :> Expand[(n/2)/.finsu];
		If[ !FreeQ[nex,LorentzIndex],
			nex = Contract[nex]
		];
		nex = Collect2[nex, x, Factoring -> False] /.
									Momentum[aa_, _] :> Momentum[aa];
		nex = nex / factorout;

		(*
		nex = nex /. Gamma[1 - Epsilon/2] :>
						( -(Epsilon*Gamma[-Epsilon/2])/2);
		*)
		n = n /. finsu;
		If[ !FreeQ[nex, x],
			If[ Head[nex] === Plus,
				nex = Map[(# I Sn )&, nex],
				nex = nex I Sn
			];
		];
		If[ (OPEIntegrateDelta /. {opt} /.
			Options[OPEIntegrate]) === True,
			nex = OPEIntegrateDelta[nex, x, OPEm]
		];
		nex = Collect2[nex,{DeltaFunction}];
		nex = Expand2[nex, x] /. (1-x)^e1_ x^e2_ :> ( (x(1-x))^e1 x^(e2-e1) );
		noflow = SelectNotFree[nex + null[1] + null[2], DeltaFunction];
		nex = ( Factor2[(nex-noflow) /( (x (1-x))^(Epsilon/2) )] *
						flowerpower[ (x (1-x)), Epsilon/2]
					) + noflow;
		floweps[bb_, aa_] :=
			flowerpower[bb, aa /. Epsilon -> locepsilon];
		epsorder = EpsilonOrder /. {opt} /.  Options[OPEIntegrate];
		nex = nex /. flowerpower -> floweps;
		nex = nex /. Momentum -> momentum4;
	(*
	Dialog[nex];
	*)
		If[ NumberQ[epsorder],
			If[ epscdi =!= 4,
				nex = 2 nex/(2+Epsilon) / (1+Epsilon);
			];
			nex = Series2[nex, Epsilon, epsorder] /. Pi^2 -> (6 Zeta2) /.
										finsu,
			nex = nex /. finsu;
		];
		(*
			If[!FreeQ[nex, ScaleMu],
					nex = nex /. Log[-p2/ScaleMu^2] -> (-Log[ScaleMu^2/p2]);
				];
		*)
		nex = Expand[nex,flowerpower];
		noflow = nex /. flowerpower[__] :> 0;
		nex = Factor2[Factor1[nex - noflow] dummyfa];
		FCPrint[1,"factoring done "];
		noflow = Factor2[noflow];
		If[ Head[noflow] === Times,
			noflow = SelectFree[noflow, Epsilon] Collect2[SelectNotFree[noflow,
															Epsilon], Epsilon]
		];
	(*
	Dialog[nex];
	*)
		nfa = SelectNotFree[nex, {flowerpower, SMP["g_s"], CA, CF, OPEm,
												Pi, Sn, SUNN, SUNIndex}
									] /. flowerpower -> Power /. locepsilon -> Epsilon;
		nfa = nfa;
		nfax = SelectNotFree[SelectFree[nfa,Epsilon], x];
		nex  = (nfax nex /. dummyfa -> 1) / nfa / x^(OPEm-1);
		nex  = facout nex /. flowerpower -> Power /. locepsilon -> Epsilon;
		nfa  = Factor2[(nfa / nfax) x^(OPEm-1)];
		opm = SelectFree[SelectNotFree[nfa, OPEm], x];
		nfa = (opm/facout)  Factor2[(nfa/opm)];
	(*
	Dialog[nex];
	*)
		If[ !FreeQ[nex, LorentzIndex],
				nex = Collect2[nex, LorentzIndex, Factoring -> True]
		];


		ccol[w_] :=
			If[ Head[w] =!= Times,
				apa[Collect2[w,
								{DeltaFunction, ScaleMu},
								Factoring -> True
										]
					],
				SelectNotFree[w, Epsilon] *
				apa[Collect2[SelectFree[w, Epsilon],
								{DeltaFunction, ScaleMu},
								Factoring -> True
										]
					]
			];
		delcol[y_] :=
			If[ Head[y] === Plus,
				Map[ccol, y],
				ccol[y]
			];
		apart[y_,xx_] :=
			Apart[y,xx] /. (1/(-1+xx)) :> ((-1)/(1-xx));
		dE[y_] :=
			y /. DeltaFunction[1-x] -> DeltaFunction[1-xXx];
		apa[y_] :=
			If[ Head[y] === Plus,
				Map[apa, y],
				If[ Head[y] =!= Times,
					y,
					SelectFree[y//dE, x] apart[SelectNotFree[y//dE,x], x]
				]/.DeltaFunction[1-xXx] -> DeltaFunction[1-x]
			];
		rcol[y_] :=
			delcol[
			Collect2[y , Epsilon, Factoring -> True]
									];
		nex = rcol[nex];
		nex = nex/.flowerpower->Power;
		nex = nex //. finsu;
		(*
		nex = nfa . nex + noflow;
		*)
		nex = factorout (nfa/divout) nex + (factorout/divout) noflow;
		nex = nex //. finsu;
		nex = nex /. Momentum -> momentum4;
		If[ !FreeQ[nex, x],
			If[ (OPEIntegrateDelta /. {opt} /.
				Options[OPEIntegrate]) =!= True,
				If[ $Notebooks,
					If[ FreeQ[nex,LorentzIndex],
						nex = Factor2[nex];
					];
					nex = nex /.(aa_^(en_ /; Head[en]=!= Integer)) :> power3[aa,en];
					nex = Factor2[nex] /. power3 -> Power;
					nex = nfacfix[nex, x]
				];,
				If[ $Notebooks,
					nex = nex /. (aa_^(en_ /; Head[en] =!= Integer)
											) :> power3[aa,en];
				];
				nex = nfacfix[nex, x];
			];
			If[ $Notebooks,
				nex = nex /. power3 -> Power;
			];
		];
		If[ !FreeQ[nex, LorentzIndex],
			lcol[yy__] :=
				Collect2[Plus[yy], LorentzIndex];
			nex = nex /. Plus  -> lcol;
		];
		If[ Factoring /. {opt} /. Options[OPEIntegrate],
			nex = Factor2[nex]
		];
		PowerSimplify[
		nex/.Momentum -> momentum4/.DiracGamma[aa_,4+Epsilon]:>DiracGamma[aa]
								]/.loc -> reloc /. reloc->loc
	];

reloc /:  HoldForm[reloc[a_]] := reloc[a];

denomout[y_] :=
	If[ Denominator[y]===1,
		y,
		DOT[(1/Denominator[y]), Numerator[y]]
	];

intmul[xxx_, yy_] :=
	If[ FreeQ[yy, SmallDelta],
		Integratedx[xxx,0,1],
		Integratedx[xxx,0,1-SmallDelta]
	];

intfake[y_,xx_] :=
	If[ Head[y] =!= Times,
		DOT[intmul[xx,y], y],
		DOT[SelectFree[y, xx],
				intmul[xx,y],
				denomout[SelectNotFree[y, xx]]
			]
	];

nfacfix[y_Plus,xxx_] :=
	Map[nfacfix[#, xxx]&, y];
nfacfix[h_,xx_] :=
	Block[ {nf},
		If[ $Notebooks =!= True,
			h,
			nf = NumericalFactor[h];
			nf = nf/Denominator[SelectFree[h/nf, xx]];
			If[ nf^2 === 1,
				h,
				If[ Denominator[nf/NumericalFactor[nf]]===1,
					DOT[nf, intfake[(h/nf), xx]],
					DOT[(1/Denominator[nf/NumericalFactor[nf]]),
							NumericalFactor[nf]*
							Numerator[nf/NumericalFactor[nf]],
							intfake[h/nf,xx]
						]
				]
			]
		]
	];

momentum4[a_,___Symbol] :=
	Momentum[a];
gammaex[xx_] :=
	Gamma[Expand[xx]];

FCPrint[1,"OPEIntegrate.m loaded"];
End[]
