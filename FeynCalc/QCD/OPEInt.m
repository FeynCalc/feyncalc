(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEInt *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 September '97 at 9:17 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: several integrals *)

(* ------------------------------------------------------------------------ *)

OPEInt::usage =
"OPEInt[expr, q, p, x] calculates 1-loop OPE-type self energies.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`OPEInt`Private`"]

Options[OPEInt] = {
	Dimension -> D,
	Divideout -> 1,
	EpsContract -> 4,
	EpsilonOrder -> 1,
	Factorout -> 1/2,
	FinalSubstitutions -> {D -> (4+Epsilon), Log[ScaleMu^2 _]:>0},
	OPEIntegrateDelta -> False
};

OPEInt[exp_, kk_, pp_, x_, opt___Rule] :=
	Block[{	facout,nex, nex0, epsc, epscc,factorout, divout,
			epscdi, n, k, ka, p, p2, sumk, sumk0, sumk1,de, dufa, powsub, opgeom, qqq,
			null1, null2, qse, dUMMYM, mUuU, nUuU, finsu,flowerpower,delcol,
			locepsilon, floweps, epsorder, nfa, noflow, opm, apa, dE, xXx, qqqk, nex1, null,
			muUU, irules0, irules1, irules2, irules3, nfax
			},
		facout = 1;
		n  = Dimension /. {opt} /. Options[OPEInt];
		k  = Momentum[kk, n];
		ka = Momentum[kk, ___];
		p  = Momentum[pp, n];
		de = Momentum[OPEDelta, n];
		p2 = Pair[p, p];
		finsu     = FinalSubstitutions /. {opt} /. Options[OPEInt];
		factorout = Factorout /. {opt} /. Options[OPEInt];
		divout    = Divideout /. {opt} /. Options[OPEInt];
		epscdi    = EpsContract /. {opt} /. Options[OPEInt];
		(*
		Dialog[epscdi];
		*)
		If[ epscdi =!= 4,
			SetOptions[Eps, Dimension -> epscdi]
		];
		sumk0 /: (y_ /; !FreeQ[y, kk]) sumk0[aaa_, b_] := sumk0[aaa y, b];
		sumk1[a_Times, b_] :=
			sumk[qqq[SelectNotFree[a, kk]],b] (a/SelectNotFree[a,kk]);
		(*
		powsub[xx_] := xx /.{(Pair[de,k]^m_ (Pair[de,p] - Pair[de,k])^a_) :>
												(sumk0[Pair[de,k]^(OPEk+m/(-a)) /
																Pair[de,p]^(OPEk+1), {OPEk,0,Infinity}
															]
													)^(-a),
												(Pair[de,k]^m_ (Pair[de,k] - Pair[de,p])^a_) :>
												(-sumk0[Pair[de,k]^(OPEk+m/(-a)) /
																Pair[de,p]^(OPEk+1), {OPEk,0,Infinity}
															]
													)^(-a)
												} /. sumk0 -> sumk;

		opgeom[xy_,aa_] := sumk[powsub[xy], aa];
		*)
		nex = PowerSimplify[FeynCalcInternal[exp] /. Power2->Power];
		nex = Expand[ChangeDimension[nex, n] /. OPESum -> sumk0 /.
											sumk0 -> sumk1 /. sumk1 -> sumk,  kk];

		(* amputate *)
		mUuU = LorentzIndex[FCGV[ToString[Unique["li"]]], n];
		nUuU = LorentzIndex[FCGV[ToString[Unique["lI"]]], n];
		nex = nex /. Eps[a___, Momentum[kk,(*dii*)___], b___]^2 :>
								(Eps[a, mUuU, b] * Pair[Momentum[kk, n], mUuU] *
									Eps[a, nUuU, b] * Pair[Momentum[kk, n], nUuU]
								);
		qqqk[yy_Times] :=
			SelectFree[yy, kk] qqq[SelectNotFree[yy, kk]];
		nex = nex /. qqq -> qqqk /. qqqk -> qqq;

		(* do the 1-part too *)
		nex = nex  + null[1] + null[2];
		nex0 = SelectNotFree[nex, Pair[de,k]^(em_/;(Head[em]=!=Integer))];
		nex1 = (nex - nex0) /. {null[1] :> 0, null[2] :> 0};
		nex1 = Expand[nex1 Pair[de,k]^(dUMMYM), kk];
		nex = nex0 + nex1;
		qse[null1] = qse[null2] = 0;
		qse[a_] :=
			If[ !FreeQ[a, sumk],
				a,
				If[ Head[a] =!= Times,
					a,
					qqq[SelectNotFree[a, kk]] (a/SelectNotFree[a, kk])
				]
			];
		nex = Map[qse, nex + null1 + null2];

		(*
		nex = nex /. sumk -> opgeom;
		*)
		If[ Head[nex] === Plus,
			nex = (# dufa)& /@ nex,
			nex = dufa nex
		];
		irules0 = {
				_. FeynAmpDenominator[PropagatorDenominator[k, 0]..]:>0,
				_. FeynAmpDenominator[PropagatorDenominator[k-p, 0]..]:>0,
		(* i1munu *)
		qqq[ Pair[de, k]^m_ fun1_[a___, ka, b___Momentum] *
			fun2_[aa___,ka,bb___Momentum] *
										FeynAmpDenominator[
										PropagatorDenominator[k, 0], PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p, 0]
																			]
			]:>
		(muUU = LorentzIndex[Unique[$MU], n];
		( Pair[de, p]^m  x^(m-1) (x (1-x))^(n/2-2) *
		(*g_munu*)
			( Contract[fun1[a, muUU, b] fun2[aa, muUU, bb]] (1/2) Gamma[2-n/2] x*
				(1-x) +
		(*p_mu p_nu*)
		EpsEvaluate[fun1[a,p,b] fun2[aa,p,bb]]  Gamma[3-n/2] x^2/p2 +
		EpsEvaluate[(fun1[a,p,b] fun2[aa,de,bb] + fun1[a,de,b] fun2[aa,p,bb]
								) / Pair[de, p]] (1/2) (Gamma[2-n/2]-Gamma[3-n/2]) (2x^2-x)+
		1/4 (Gamma[3-n/2] (1-4 x + 4 x^2) + Gamma[2-n/2] (4 x - 6 x^2)) *
		fun1[a,de,b] fun2[aa,de,bb] p2/Pair[de,p]^2)
		)) /; ( ((fun1 === Eps) || (fun1 === Pair)) &&
						((fun2 === Eps) || (fun2 === Pair))
					),

		qqq[ Pair[de, ka]^m_ fun1_[a___, ka, b___Momentum] *
			fun2_[aa___,k,bb___Momentum] *
										FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p, 0],
										PropagatorDenominator[k - p, 0]
																			]
			] :>
		(muUU = LorentzIndex[Unique[$MU], n];

		(*i6munu*)
		EpsEvaluate[
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
										PropagatorDenominator[k - p, 0],
										PropagatorDenominator[k - p, 0]              ]
			]:>
		( 1/p2^2 Pair[de, p]^m  x^(m-1) Gamma[3 - n/2] (x (1-x))^(n/2-2) *
		( (1 - (n-4)/2) x/(1-x) EpsEvaluate[fun[a, p, b]] +
			p2/Pair[de, p] EpsEvaluate[fun[a, de, b]] (n-4)/4 (2x-1)/(1-x)
			)
		),

		(*i2murule*)
		qqq[
		Pair[de, ka]^m_ fun_[a___, ka, b___Momentum] FeynAmpDenominator[
										PropagatorDenominator[k, 0], PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p, 0]         ]
			] :>
						(Pair[de, p]^m  x^(m-1) (x (1-x))^(n/2-2) *
				( Gamma[3 - n/2] x EpsEvaluate[fun[a, p, b]]/p2   +
					1/2 (Gamma[2 - n/2] x + Gamma[3 - n/2] (1 - 2 x)
							)  EpsEvaluate[fun[a, de, b]]/Pair[de, p]
				)  ) /; ( (fun === Eps) || (fun === Pair)),
		(*i5murule = *)
		qqq[
		Pair[de, ka]^m_ fun_[a___,ka, b___Momentum] FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p, 0]         ]
			]:>
									(Pair[de, p]^m  x^(m-1) (x (1-x))^(n/2-2) *
				( Gamma[2 - n/2] x^2  EpsEvaluate[fun[a, p, b]]   +
					1/2 Gamma[2 - n/2] (x - 2 x^2)  *
							EpsEvaluate[fun[a, de, b]] p2 /Pair[de, p]
				)        ) /; ( (fun === Eps) || (fun === Pair)),
		qqq[
		Pair[de, ka]^m_ fun_[a___, ka, b___Momentum]  FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p, 0],
										PropagatorDenominator[k - p, 0]
																			]
			]:> (
							Pair[de, p]^m  x^(m-1) (x^2/(1-x)) (x (1-x))^(n/2-2) *
							( Gamma[3 - n/2] / p2 )  fun[a, p, b] +
		(* B *)
							Pair[de, p]^m  / Pair[de, p]/2 x^(m-1) x (1 - 2 x)/(1 - x) *
							(x (1-x))^(n/2-2) ( Gamma[3 - n/2] ) fun[a, de, b] -
							Pair[de, p]^m  / Pair[de, p]/2 x^(m-1) x *
							(x (1-x))^(n/2-2) ( Gamma[2 - n/2] ) fun[a, de, b]
																					) /;
														( (fun === Eps) || (fun === Pair))
							};
		irules2 = {
		(*i3rule = *)
		qqq[
		Pair[de, ka]^m_  FeynAmpDenominator[
										PropagatorDenominator[k, 0], PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p, 0]         ]
			]:>
							Pair[de, p]^m  x^(m-1) (x (1-x))^(n/2-2) *
				( Gamma[3 - n/2] / p2 ),
		(*i4rule = *)
		qqq[
		Pair[de, ka]^m_  FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p, 0]         ]
			] :>
							Pair[de, p]^m  x^(m-1) (x (1-x))^(n/2-2) *
				( Gamma[2 - n/2] x ),
		(* newrule1 *)
		qqq[
		Pair[de, ka]^m_  FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p, 0],
										PropagatorDenominator[k - p, 0]
																			]
			]:> (
							Pair[de, p]^m  x^(m-1) (x/(1-x)) (x (1-x))^(n/2-2) *
							( Gamma[3 - n/2] / p2 )      ),
		(* i6rule *)
		qqq[
		Pair[de, ka]^m_  FeynAmpDenominator[
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k, 0],
										PropagatorDenominator[k - p, 0],
										PropagatorDenominator[k - p, 0]
																			]
			]:> (1/p2^2 *
							Pair[de, p]^m Gamma[3-n/2] x^(m-1) (x (1-x))^(n/2-2)*
							1/(1-x)  (1 - (n-4)/2)       )
			};
		nex =
		nex//.irules0 //. irules1 //. irules2 /. dufa -> 1 /.qqq->Identity /.
											{sumk :> OPESum, dUMMYM :> 0};
		epsc[Eps[ww__]^2] :=
			epsc[Eps[ww]^2] =
			Contract[ChangeDimension[Eps[ww],epscdi]^2];
		nex = nex /. Eps[uu__]^2 :> epscc[Eps[uu]^2];
		nex = nex /. epscc -> epsc;
		If[ !FreeQ2[nex, {Eps,LorentzIndex}],
			nex = Contract[nex, EpsContract -> True]
		];

		(*
		*)
		If[ !FreeQ[nex, OPESum],
			nex = OPESumExplicit[nex//OPESumSimplify]//Factor2
		];
		(*
		Dialog[nex];
		*)
		nex = nex /.  Gamma[3-n/2] -> (Gamma[2-n/2] -
									(n/2-1) Gamma[2-n/2]
																	);
		nex = nex /. finsu /. Gamma -> gammaex /.
					((n/2)/.finsu) :> Expand[(n/2)/.finsu];
		nex = Collect2[nex//Contract, x, Factoring -> False] /.
									Momentum[aa_, _] :> Momentum[aa];
		nex = nex / factorout;

		(*
		nex = nex /. Gamma[1 - Epsilon/2] :>
						( -(Epsilon*Gamma[-Epsilon/2])/2);
		*)
		n = n /. finsu;
		If[ Head[nex] === Plus,
			nex = Map[(# I Sn (*flowerpower[(-p2)/ScaleMu^2,
																Expand[(n/2 -2)]]*)
								)&, nex
								],
			nex = nex I Sn
		(*flowerpower[-p2/ScaleMu^2,
														Expand[(n/2 -2)]]*)
			];
		If[ (OPEIntegrateDelta /. {opt} /. Options[OPEInt]) === True,
			nex = OPEIntegrateDelta[nex, x, OPEm]
		];
		nex = Collect2[nex,{DeltaFunction}];
		nex = Expand[nex, x] /. (1-x)^e1_ x^e2_ :> ( (x(1-x))^e1 x^(e2-e1) );
		p2  = p2 /. Momentum[pp,_] :> Momentum[pp];
		noflow = SelectNotFree[nex + null[1] + null[2], DeltaFunction];
		FCPrint[1,"noflow = ", noflow];
		nex = ( Factor2[(nex-noflow) /( (x (1-x))^(Epsilon/2) )] *
						flowerpower[ (x (1-x)), Epsilon/2]
					) + noflow;
		floweps[bb_, aa_] :=
			flowerpower[bb, aa /. Epsilon -> locepsilon];
		epsorder = EpsilonOrder /. {opt} /.  Options[OPEInt];
		nex = nex /. flowerpower -> floweps;
		nex = nex (-p2/ScaleMu^2)^Expand[(n/2 -2)];
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
		If[ !FreeQ[nex, ScaleMu],
			nex = nex /. Log[-p2/ScaleMu^2] -> (-Log[ScaleMu^2/p2]);
		];
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
		nex = nex /. finsu;
		(*
		nex = nfa . nex + noflow;
		*)
		nex = factorout (nfa/divout) nex + (factorout/divout) noflow;
		nex = nex /. Momentum -> momentum4;
		If[ (OPEIntegrateDelta /. {opt} /. Options[OPEInt]) =!= True,
			nex = Factor2[nex],
			nex = nfacfix[nex];
		];
		nex
	];

nfacfix[y_Plus] :=
	Map[nfacfix, y];
nfacfix[h_] :=
	Block[ {nf},
		nf = NumericalFactor[h];
		If[ nf^2 === 1,
			h,
			nf . (h/nf)
		]
	];

momentum4[a_,___] :=
	Momentum[a];
gammaex[xx_] :=
	Gamma[Expand[xx]];

FCPrint[1,"OPEInt.m loaded"];
End[]
