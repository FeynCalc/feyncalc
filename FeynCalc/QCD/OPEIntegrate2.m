(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEIntegrate2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 February '99 at 2:06 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

OPEIntegrate2::usage =
"OPEIntegrate2[exp, k] does special loop (tensorial) integrations. Only the
residue is calculated.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`OPEIntegrate2`Private`"]

kli::usage="";
dummy::usage="";
sumgeom::usage="";

Options[OPEIntegrate2] = {
	Collecting  -> False,
	Names -> {FCGV["x"], FCGV["y"], FCGV["z"]},
	FinalSubstitutions -> {},
	Integratedx -> True,
	OPEDelta    -> False
};

integrate[yy_Plus, zz__] := (*Factor2*)
(*Expand[*)
	Map[integrate[#, zz]&, yy](*]*);
Global`$INTC = {};
intsav[a_,{z_,zi_,zf_},ass___Rule] :=
	intsav[a,{z,zi,zf},ass] =
	Block[ {na,re},
		FCPrint[2,"intsav simplify"];
		na = a//PowerSimplify//Factor2//Simplify;
		FCPrint[2,"intsav simplify done"];
		na = na/.Plus:>(Isolate[Collect2[Plus[##],z, Factoring->True(*,
									IsolateNames->LL*)
													],z,IsolateNames->LL]&
									);
		FCPrint[3,"intsav collect2 done"];
		FCPrint[4,na//InputForm];
		re = Integrate2[na,{z,zi,zf},ass
									] /. (Hold@@{Integrate3}) -> Integrate;
		re = PowerSimplify[re];
		FCPrint[3,"int done"];
		If[ Head[re] === If,
			AppendTo[Global`$INTC, re[[1]]];
			re = re[[2]]
		];
		FixedPoint[ReleaseHold, re]
	(*//Factor2*)
					];

integrate[yy_Times, {xx_,0,1}, ass___Rule] :=
	SelectFree[yy,xx] intsav[SelectNotFree[yy,xx],{xx,0,1},ass];

idx[y_, opt___] :=
	Block[ {nr, ci, intvars},
		If[ (Integratedx/.{opt}/.Options[OPEIntegrate2]) =!= True,
			nr = (ChangeDimension[y,4] /. D -> 4+Epsilon) /.
					{Power[a_,b_]:> Power[a,Expand[b]],
						Gamma[a_] :> Gamma[Expand[a]]},
			nr = PowerSimplify[ EpsEvaluate[ExpandScalarProduct[
													ChangeDimension[y, 4]/.D->(4+Epsilon)]]
												];
			FCPrint[2,"series2"];
			nr = Series2[nr , Epsilon, -1
									] // PowerSimplify//ExpandScalarProduct;
			FCPrint[2,"series2 done"];
			ci =  Reverse[Cases2[nr, Integratedx]];
			intvars = Map[First, ci];
			FCPrint[2,"intvars ", intvars];
			nr = Collect2[nr, intvars, Factoring -> (*False*) True];
			While[Length[ci] > 0,
							FCPrint[1,"doing ",ci[[1]]//Last," integration"];
							nr = Isolate[Collect2[nr, intvars[[1]],
														Factoring->False],intvars[[1]],
													IsolateNames -> LL];
							nr = nr /. (aa_ /; !FreeQ[aa,intvars[[1]]])^po_  :>
												FRH[aa^po];
							nr = integrate[Cancel[nr/ci[[1]]],
														{ci[[1,1]], ci[[1,2]], ci[[1,3]]}(*,
														Assumptions -> {Re[OPEm]>1}*)
														]//FRH;
							ci = Rest[ci];
							intvars = Rest[intvars];
					]
		];
		nr
	];

eventualfp[y_,k_,opt___Rule] :=
	If[ FreeQ[y,Integratedx],
		FeynmanParametrize[Uncontract[y,k,Pair-> All],k,
											Names->(
											Names/.{opt} /.
											Options[OPEIntegrate2]  )
											],
		y
	];

topower2[y_] :=
	y /. Power2 -> Power /. (a_ /; !FreeQ[a,OPEDelta])^
					(p_ /;Head[p] =!= Integer) :> Power2[a, p];

isol[ka_][y_] :=
	Isolate[y,Append[SelectFree[Variables[Flatten[
									Cases2[y,Power2]/. Power2->List]],Pair],
										ka], IsolateNames->KK, IsolateSplit->5555I];
gpowsub[xx_,k_] :=
	xx /. {
						(((nok_ /; FreeQ[nok,k]) -
							Pair[de_, Momentum[k,D]])^a_Integer?Negative
						) :>
						(sumgeom[ {OPEl,0,Infinity} ]*
							Pair[de, Momentum[k,D]]^(-a OPEl)/
								nok^(-a (OPEl+1))
						)^(-a),
						((Pair[de_, Momentum[k,D]] +
							(nok_/;FreeQ[nok,k]))^ a_Integer?Negative
						) :>
						(-sumgeom[ {OPEl,0,Infinity} ] *
							Pair[de, Momentum[k,D]
									]^(-a OPEl)/(-nok)^(-a (OPEl+1))
						)   };

opsu[a_Plus,b__] :=
	Map[opsu[#,b]&,a];
opsu[c_Times,{ka_,0,Infinity}] :=
	SelectFree[c,ka] *
	OPESum[SelectNotFree[c,ka],{ka,0,Infinity}];

gpowsumit[exp_] :=
	If[ FreeQ[exp, OPEl],
		exp,
		Block[ {g1,g2,g3,g4,n1,n2,gr},
			g1 = Expand2[exp,OPEl]/.Power2->Power;
			g2 = SelectFree[g1+n1+n2, OPEl] /. {n1 :> 0, n2 :> 0};
			g3 = Collect2[SelectNotFree[Map[#/sumgeom[{OPEl,0,Infinity}]&,
																g1+n1+n2
															] ,OPEl
													] /.{n1 :> 0, n2 :> 0}
													,OPEl,Factoring->False
									];
			gr = g2 + opsu[g3, {OPEl, 0, Infinity}];
			gr
		]
	];

OPEIntegrate2[exp_Plus,k_, opt___Rule] :=
	Map[OPEIntegrate2[#,k,opt]&, exp];

OPEIntegrate2[exp_, Momentum[k_,___],opt___Rule] :=
	OPEIntegrate2[exp,k,opt];

OPEIntegrate2[ex_,k_,opt___Rule] :=
	Block[ {fadk, kkfix, tt,nt,exp = FeynCalcInternal[ex]//ExpandScalarProduct},
		fadk[a___,-k+pe_.,b___] :=
			fadk[a,k-pe,b];
		fado[a___,z_,k,b___] :=
			fado[k,a,z,b] /; z=!=k;
		kkfix[z_ (pe_ - SO[k])^em_] :=
			FeynCalcExternal[
			PowerSimplify[ ExpandScalarProduct[FeynCalcInternal[
			z (pe-SO[k])^em]/. k-> -k+(pe/.SO->Identity)
									]]   ] /. FAD -> fadk /.
			fadk -> fado /. fado -> FAD;
		If[ Head[exp] === Times,
			exp = FeynCalcExternal[exp] /.SO->SOD/.SP->SPD;
			tt = SelectFree[exp, k] kli[k, SelectNotFree[exp,k]];
			nt = tt /. opeinttable /. 0^_ :> 0;
			If[ tt =!= nt,
				FeynCalcInternal[nt],
				exp = kkfix[exp/.SOD->SO]/.kkfix -> Identity /. SO->SOD/.SP->SPD;
				exp = FeynCalcExternal[FeynAmpDenominatorSimplify[
																		FeynCalcInternal[exp] ,k]
															];
				exp = FeynCalcExternal[FeynAmpDenominatorSimplify[
																		FeynCalcInternal[exp]]
															] /.  FAD-> fadk /. fadk->fado/.fado->FAD;
				If[ MatchQ[exp, _ (SOD[k]-_)^(em_/;Head[em]=!=Integer)],
					exp = exp /. (SOD[k]-bla_)^pow_ :> (
											PowerSimplify[(-1)^pow]*(bla-SOD[k])^pow)
				];
				FCPrint[2,"entering OPEIntegrate2 with ", exp//FeynCalcForm];
				tt = SelectFree[exp, k] kli[k, SelectNotFree[exp,k]];
				nt = tt /. opeinttable;
				If[ tt =!= nt,
					FeynCalcInternal[nt],
					exp  = FeynCalcInternal[exp];
					If[ SelectNotFree[Cases2[exp,Momentum],k] === {},
						exp,
						FixedPoint[ReleaseHold,
						PowerSimplify[
						(CHeck3 =
						gpowsumit[
						idx[(CHeck2 =
						oi2[CHeck1 = DotSimplify[
						eventualfp[isol[k][
						gpowsub[
						ChangeDimension[exp, D],k
										]//topower2 ],
										k,opt]], k, opt
						])//Contract//DiracTrick, opt
						] /. (FinalSubstitutions /. {opt} /. Options[OPEIntegrate2]) /.
							0^_ ->0])]]
					]
				]
			],
			OPEIntegrate2[dummy exp,k,opt]/.dummy->1
		]
	];

oi2[ex_,k_,opt___Rule] :=
	If[ (Collecting/.{opt}/.Options[OPEIntegrate2])===True,
		oi[Collect2[ex,k,Factoring->False],k,opt],
		oi[ex,k,opt]
	];

oi[a_Plus,k_,opt___Rule] :=
	Map[oi[#,k,opt]&, a];
oi[a_,k_,___] :=
	a /; FreeQ2[a,{k,Pattern}] || (Head[a] =!= Times);

oi[exp_Times, k_, opt___Rule] :=
	SelectFree[exp,k] *
	ilist[ChangeDimension[SelectNotFree[exp, k], D],
				Momentum[k,D],
				If[ (OPEDelta /. {opt} /. Options[OPEIntegrate2])===True,
					0,
					1
				]
				];

denmatch[Power[den_,(*mm*)_], ka_ /; Length[ka]===2] :=
	True /;
	FreeQ[FourDivergence[Expand[ExpandScalarProduct[den]] - Pair[ka,ka],
	Pair[ka,LorentzIndex[dummy,ka[[2]]]]
	], ka[[1]]
	];

getalpha[h_] :=
	If[ MatchQ[SelectFree[h,{Power2,LorentzIndex}], _^_],
		-SelectFree[h,{Power2,LorentzIndex}][[2]],
		False
	];
getl[h_,ka_Momentum] :=
	Block[ {tg,ll, check},
		If[ !MatchQ[SelectFree[h,{Power2,LorentzIndex}], _^_],
			False,
							(*else*)
							(* get the linear part *)
			tg = SelectFree[h, {Power2, LorentzIndex}][[1]] -
					Pair[ka,ka];
			tg = SelectNotFree[ExpandScalarProduct[tg]//Expand,ka[[1]]];
							(* calculate l *)
			ll = D[tg/.Pair[ka, b_Momentum]:>(ka b), ka];
							(* check if it really is o.k. *)
			check = Expand[ExpandScalarProduct[tg-Pair[ll,ka]]];
			If[ check === 0,
				Expand[ll/(-2)],
				False
			]
		]
	];


getm2[h_,ka_Momentum] :=
	If[ !MatchQ[SelectFree[h,{Power2,LorentzIndex}], _^_],
		False,
		SelectFree[Expand[ExpandScalarProduct[
						SelectFree[h,{Power2,LorentzIndex}][[1]]]], ka]
	];
getopem[h_] :=
	(If[ Head[#]===Pair,
		1,
		If[ Head[#]===Power || Head[#]===Power2,
			#[[2]],
			False
		]
	]&
				)@SelectNotFree[h, Power2];

getmu[h_] :=
	Cases2[h, LorentzIndex];

(* I^(m)_alpha *)
match1[h_, km_] :=
	MatchQ[h//Numerator, Power2[
												Pair[Momentum[OPEDelta,___],km ], _]
											] &&
							denmatch[SelectFree[h, Power2]//Denominator,km
											] &&
								MatchQ[SelectNotFree[h, Power2],
												Power2[Pair[Momentum[OPEDelta,___],km ],_]
											];
(* I^(m)_alpha,mu *)
match2[h_, km_] :=
	MatchQ[h//Numerator,
					Power2[Pair[Momentum[OPEDelta,___], km],_]*
					Pair[km, LorentzIndex[_,___]]
				]&& denmatch[SelectFree[h,Power2]//Denominator, km] &&
	MatchQ[SelectNotFree[h,Power2],Power2[Pair[Momentum[OPEDelta,___],km],_]];

(* I^(m)_alpha,mu,nu *)
match3[h_, km_] :=
	MatchQ[h//Numerator,
					Power2[Pair[Momentum[OPEDelta,___], km], _]*
					Pair[km, LorentzIndex[aa_,___]]*
					Pair[km, LorentzIndex[bb_,___]]/;(aa=!=bb)(*VS*)
				]&& denmatch[SelectFree[h,Power2]//Denominator,km] &&
	MatchQ[SelectNotFree[h, Power2],
					Power2[Pair[Momentum[OPEDelta,___],km ],_]];

(* I^(m)_alpha,mu,nu,la *)
match4[h_, km_] :=
	MatchQ[h//Numerator,
					Power2[Pair[Momentum[OPEDelta,___],km ],_]*
					Pair[km, LorentzIndex[aa_,___]]*
					Pair[km, LorentzIndex[bb_,___]]*
					Pair[km, LorentzIndex[cc_,___]]/;(aa=!=bb && bb=!=cc)(*VS*)
				]&& denmatch[SelectFree[h,Power2]//Denominator,km] &&
	MatchQ[SelectNotFree[h,Power2],
					Power2[Pair[Momentum[OPEDelta,___],km ],_]];

(* I^(m)_alpha *)
ilist[ih_, Momentum[k_,n_], od_Integer] :=
	Block[ {al,m2,l,de,m,mu,h,fake,lmu,nu,la,lnu,lla,dmu,dnu,dla,del,l2},
		h = ih /. (a_ /; !FreeQ[a,k])^(w_ /;Head[w]=!=Integer) :>
							Power2[a, w];
		de = Momentum[OPEDelta, n];
		If[ FreeQ[h,Power2[ab_/;!FreeQ[ab,k],w_/;Head[w]=!=Integer]],
			h = topower2[h Power2[Pair[Momentum[k,n], de], fake]]
		];
		al = getalpha[h];
		l = getl[h, Momentum[k, n]];
		l2 = Pair[l,l]//ExpandScalarProduct//Expand;
		m2 = getm2[h, Momentum[k, n]]//ExpandScalarProduct//Expand;
		del = Pair[l,de]//ExpandScalarProduct//Factor2;
		m = getopem[h];
		FCPrint[2,"h = ",h];
		FCPrint[2,"l2 = " ,l2];
		FCPrint[2,"m2 = " ,m2];
		Which[
					match1[h, Momentum[k,n]],
				(
					(-1)^(n/2) I Sn Gamma[al-n/2]/Gamma[al] del^m *
					(m2-l2)^(n/2-al)
				),
					match2[h, Momentum[k,n]],
						mu = getmu[ih][[1]];
						(-1)^(n/2) I Sn /Gamma[al] del^m *
						(Gamma[al-n/2] (m2-l2)^(n/2-al) * Pair[l, mu] od +
						m/2 Gamma[al-1-n/2] (m2-l2)^(n/2-al+1) *
						Pair[de,mu]/del
						),
					match3[h, Momentum[k,n]],
						mu = getmu[ih][[1]];
						nu = getmu[ih][[2]];
						(
							(-1)^(n/2) I Sn /Gamma[al] del^m *
							(Gamma[al-n/2] (m2-l2)^(n/2-al) *
														od Pair[l, mu] * Pair[l, nu] +
							1/2 Gamma[al-1-n/2] (m2-l2)^(n/2-al+1) *
							od (Pair[mu,nu] + m/del (
									Pair[l,mu] Pair[de,nu] + Pair[l,nu] Pair[de,mu]
																		)
									) +
							1/4 m (m-1) Gamma[al-2-n/2] (m2-l2)^(n/2-al+2) *
							Pair[de,mu] Pair[de,nu] / del^2
							)
						),
					match4[h, Momentum[k,n]],
						mu = getmu[ih][[1]];
						nu = getmu[ih][[2]];
						la = getmu[ih][[3]];
						lmu = Pair[l,mu];
						lnu = Pair[l,nu];
						lla = Pair[l,la];
						dmu = Pair[de,mu];
						dnu = Pair[de,nu];
						dla = Pair[de,la];
						(-1)^(n/2) I Sn /Gamma[al] del^m *
						(
						od Gamma[al-n/2] (m2-l2)^(n/2-al) lmu lnu lla  +
							1/2 Gamma[al-1-n/2] (m2-l2)^(n/2-al+1) (
							Pair[mu,nu] Pair[l,la]+Pair[nu,la] Pair[l,mu] +
							Pair[la,mu] Pair[l,nu]                 ) +
						od m/2 Gamma[al-1-n/2] (m2-l2)^(n/2-al+1) (
							lmu lnu dla + lnu lla dmu + lla lmu dnu)/del +
						od 1/4 m (m-1) Gamma[al-2-n/2] (m2-l2)^(n/2-al+2) (
							lmu dnu dla + lnu dla dmu + lla dmu dnu)/del^2 +
						od m/4 Gamma[al-n/2-2] (m2-l2)^(n/2-al+2) (
							Pair[mu,nu] dla + Pair[mu,la] dnu + Pair[nu,la] dmu
																										)/del +
							m/8 (m-1) (m-2) Gamma[al-n/2-3] (m2-l2)^(n/2-al+3)*
							dmu dnu dla/del^3
						) ,
				True, Print["MISTTTTTTTTTTTTTTTTT"];
					Dialog[];
					h
				]/.fake->0 /. Power[aa_,bb_] :> Power[aa,Expand[bb]]
	];

opeinttable = {
kli[k_,FAD[k_,k_Symbol.., (k_)+((*p1*)_),___] (_. + _. SOD[k_])^(OPEm+_.)] :>0
,
kli[k_,FAD[k_Symbol.., (k_)+((*p1*)_), (k_)+((*p2*)_),___] *
	(_. + _. SOD[k_])^(OPEm+_.)] :>0
,
kli[k_,FAD[k_Symbol.., (k_)+((*p1*)_), (k_)+((*p2*)_),___] *
		(_. + _. SOD[k_])^(OPEm+_.) SOD[k_]^_.
	] :>0
,
kli[k_,FAD[k_,(k_)-((*p1*)_),(k_)-((*p3*)_)] (_. + _. SOD[k_])^(OPEm+_.)] :>0
,
kli[k_,FAD[k_,k_,(k_)-((*p1*)_),(k_)-((*p3*)_)] (_. + _. SOD[k_])^(OPEm+_.)]:>0
,
kli[k_,FAD[(k_)-(p3_),(k_)-(p1_)]*SOD[k_]^(OPEm+en_.) SPD[k_,k_]]:>
(-2*I*Sn*SO[p3]^(1 + en + OPEm)*
			(2*SO[p1]*SO[p3]*SP[p1, p1] + (en + OPEm)*SO[p1]*SO[p3]*SP[p1, p1] -
				(en + OPEm)*SO[p3]^2*SP[p1, p1] - 6*SO[p1]*SO[p3]*SP[p1, p3] -
				2*(en + OPEm)*SO[p1]*SO[p3]*SP[p1, p3] + 2*SO[p3]^2*SP[p1, p3] +
				2*(en + OPEm)*SO[p3]^2*SP[p1, p3] + 2*SO[p1]^2*SP[p3, p3] +
				(en + OPEm)*SO[p1]^2*SP[p3, p3] -
				(en + OPEm)*SO[p1]*SO[p3]*SP[p3, p3]))/
		(Epsilon*(1 + en + OPEm)*(2 + en + OPEm)*(-SO[p1] + SO[p3])^3) +
	(2*I*Sn*SO[p1]^(1 + en + OPEm)*
			(-((en + OPEm)*SO[p1]*SO[p3]*SP[p1, p1]) + 2*SO[p3]^2*SP[p1, p1] +
				(en + OPEm)*SO[p3]^2*SP[p1, p1] + 2*SO[p1]^2*SP[p1, p3] +
				2*(en + OPEm)*SO[p1]^2*SP[p1, p3] - 6*SO[p1]*SO[p3]*SP[p1, p3] -
				2*(en + OPEm)*SO[p1]*SO[p3]*SP[p1, p3] -
				(en + OPEm)*SO[p1]^2*SP[p3, p3] + 2*SO[p1]*SO[p3]*SP[p3, p3] +
				(en + OPEm)*SO[p1]*SO[p3]*SP[p3, p3]))/
		(Epsilon*(1 + en + OPEm)*(2 + en + OPEm)*(-SO[p1] + SO[p3])^3)
,
kli[k_,FAD[(k_)-(p1_),(k_)-(p3_)]*SOD[k_]^(OPEm+en_.)]:>
(2*I*Sn*SO[p1]^(OPEm+1+en))/(Epsilon*(OPEm+1+en)*(-SO[p1]+SO[p3]))-
(2*I*Sn*SO[p3]^(OPEm+1+en))/(Epsilon*(OPEm+1+en)*(-SO[p1]+SO[p3]))
,
kli[k_,FAD[k_,(k_)-(p1_)]*SOD[k_]^(OPEm+en_.)]:>
(-2*I*Sn*SO[p1]^(OPEm+en))/(Epsilon*(1+OPEm+en))
,
kli[k_,FAD[k_,(k_)+(p1_)]*SOD[k_]^(OPEm+en_.)]:>
(-2*I*Sn*SO[-p1]^(OPEm+en))/(Epsilon*(1+OPEm+en))
,
kli[k_,FAD[k_,(k_)-(p3_)]*SOD[k_]^(OPEm+en_.)*SPD[k_,p1_]]:>
(I*Sn*SO[p3]^(-1+OPEm+en)*(-2*SO[p3]*SP[p1,p3]-
2*(en+OPEm)*SO[p3]*SP[p1,p3]+(OPEm+en)*SO[p1]*SP[p3,p3]))/
(Epsilon*(1+OPEm+en)*(2+OPEm+en))
,
kli[k_,FAD[k_,k_,(k_)-(p3_)]*SOD[k_]^(OPEm+en_.)* SPD[k_,p1_]]:>
(-I*Sn*SO[p1]*SO[p3]^(OPEm-1+en))/(Epsilon*(1+OPEm+en))
,
kli[k_,FAD[k_,k_,(k_)-(p3_)]*SOD[k_]^(OPEm+en_.)* SPD[k_,p1_]^2]:>
(I*Sn*SO[p3]^(-2+OPEm+en)*(-(SO[p3]^2*SP[p1,p1])-
2*(OPEm+en)*SO[p1]*SO[p3]*SP[p1,p3]-SO[p1]^2*SP[p3,p3]+
(OPEm+en)*SO[p1]^2*SP[p3,p3]))/(Epsilon*(1+en+OPEm)*(2+en+OPEm))
,
kli[k_,FAD[k_,(k_)-(p1_)]*SOD[k_]^(OPEm+en_.)*SPD[k_,p3_]]:>
(-I*Sn*SO[p1]^(-1+en+OPEm)*(-((en+OPEm)*SO[p3]*SP[p1,p1])+
2*SO[p1]*SP[p1,p3]+2*(en+OPEm)*SO[p1]*SP[p1,p3]))/
(Epsilon*(1+en+OPEm)*(2+en+OPEm))
,
kli[k_,FAD[k_,k_,(k_)-(p1_)]*SOD[k_]^(en_.+OPEm)* SPD[k_,p3_]]:>
(-I*Sn*SO[p1]^(OPEm+en-1)*SO[p3])/(Epsilon*(1+OPEm+en))
,
kli[k_,FAD[k_,k_,(k_)-(p1_)]*SOD[k_]^(OPEm+en_.)* SPD[k_,p3_]^2]:>
(-I*Sn*SO[p1]^(-2+en+OPEm)*(SO[p3]^2*SP[p1,p1]-
(en+OPEm)*SO[p3]^2*SP[p1,p1]+2*(en+OPEm)*SO[p1]*SO[p3]*SP[p1,p3]+
SO[p1]^2*SP[p3,p3]))/(Epsilon*(1+en+OPEm)*(2+en+OPEm))
,
kli[k_,FAD[(k_)-(p3_),(k_)-(p1_)]*SOD[k_]^(OPEm+en_.)* SPD[k_,k_]]:>
(
(2*I*Sn*SO[p3]^(1+en+OPEm)*(-(SO[p1]*SO[p3]*SP[p1,p1])-
(1+en+OPEm)*SO[p1]*SO[p3]*SP[p1,p1]-SO[p3]^2*SP[p1,p1]+
(1+en+OPEm)*SO[p3]^2*SP[p1,p1]+4*SO[p1]*SO[p3]*SP[p1,p3]+
2*(1+en+OPEm)*SO[p1]*SO[p3]*SP[p1,p3]-2*(1+en+OPEm)*SO[p3]^2*SP[p1,p3]-
SO[p1]^2*SP[p3,p3]-(1+en+OPEm)*SO[p1]^2*SP[p3,p3]-
SO[p1]*SO[p3]*SP[p3,p3]+(1+en+OPEm)*SO[p1]*SO[p3]*SP[p3,p3]))/
(Epsilon*(1+en+OPEm)*(1+(1+en+OPEm))*(-SO[p1]+SO[p3])^3)+
(2*I*Sn*SO[p1]^(1+en+OPEm)*(SO[p1]*SO[p3]*SP[p1,p1]-
(1+en+OPEm)*SO[p1]*SO[p3]*SP[p1,p1]+SO[p3]^2*SP[p1,p1]+
(1+en+OPEm)*SO[p3]^2*SP[p1,p1]+2*(1+en+OPEm)*SO[p1]^2*SP[p1,p3]-
4*SO[p1]*SO[p3]*SP[p1,p3]-2*(1+en+OPEm)*SO[p1]*SO[p3]*SP[p1,p3]+
SO[p1]^2*SP[p3,p3]-(1+en+OPEm)*SO[p1]^2*SP[p3,p3]+
SO[p1]*SO[p3]*SP[p3,p3]+(1+en+OPEm)*SO[p1]*SO[p3]*SP[p3,p3]))/
(Epsilon*(1+en+OPEm)*(1+(1+en+OPEm))*(-SO[p1]+SO[p3])^3)
)
,
(*STILLTODO*)
kli[k_,FAD[k_,(k_)-(p3_)]*SOD[k_]^(-1+OPEm)* SPD[k_,p1_]]:>
(I*Sn*SO[p3]^(-2+OPEm)*(-2*OPEm*SO[p3]*SP[p1,p3]-
SO[p1]*SP[p3,p3]+OPEm*SO[p1]*SP[p3,p3]))/
(Epsilon*OPEm*(1+OPEm))
,
kli[k_,FAD[(k_)-(p3_),(k_)-(p1_)]*SOD[k_]^(-2+OPEm)* SPD[k_,p3_]]:>
(-I*Sn*SO[p3]^(-1+OPEm)*(OPEm*SO[p1]*SO[p3]*SP[p1,p1]+
2*SO[p3]^2*SP[p1,p1]-OPEm*SO[p3]^2*SP[p1,p1]-
2*SO[p1]*SO[p3]*SP[p1,p3]-2*OPEm*SO[p1]*SO[p3]*SP[p1,p3]-
2*SO[p3]^2*SP[p1,p3]+2*OPEm*SO[p3]^2*SP[p1,p3]+
2*OPEm*SO[p1]^2*SP[p3,p3]+2*SO[p1]*SO[p3]*SP[p3,p3]-
3*OPEm*SO[p1]*SO[p3]*SP[p3,p3]+OPEm*SO[p3]^2*SP[p3,p3]))/
(Epsilon*(-1+OPEm)*OPEm*(-SO[p1]+SO[p3])^3)+
(I*Sn*SO[p1]^(-1+OPEm)*(2*SO[p1]*SO[p3]*SP[p1,p1]-
OPEm*SO[p1]*SO[p3]*SP[p1,p1]+OPEm*SO[p3]^2*SP[p1,p1]-
2*SO[p1]^2*SP[p1,p3]+2*OPEm*SO[p1]^2*SP[p1,p3]-
2*SO[p1]*SO[p3]*SP[p1,p3]-2*OPEm*SO[p1]*SO[p3]*SP[p1,p3]+
2*SO[p1]^2*SP[p3,p3]-OPEm*SO[p1]*SO[p3]*SP[p3,p3]+
OPEm*SO[p3]^2*SP[p3,p3]))/
(Epsilon*(-1+OPEm)*OPEm*(-SO[p1]+SO[p3])^3)
,
kli[k_,FAD[(k_)-(p3_),(k_)-(p1_)]*SOD[k_]^(-2+OPEm)* SPD[k_,p1_]]:>
(I*Sn*SO[p1]^(-1+OPEm)*(OPEm*SO[p1]^2*SP[p1,p1]+
2*SO[p1]*SO[p3]*SP[p1,p1]-3*OPEm*SO[p1]*SO[p3]*SP[p1,p1]+
2*OPEm*SO[p3]^2*SP[p1,p1]-2*SO[p1]^2*SP[p1,p3]+
2*OPEm*SO[p1]^2*SP[p1,p3]-2*SO[p1]*SO[p3]*SP[p1,p3]-
2*OPEm*SO[p1]*SO[p3]*SP[p1,p3]+2*SO[p1]^2*SP[p3,p3]-
OPEm*SO[p1]^2*SP[p3,p3]+OPEm*SO[p1]*SO[p3]*SP[p3,p3]))/
(Epsilon*(-1+OPEm)*OPEm*(-SO[p1]+SO[p3])^3)+
(I*Sn*SO[p3]^(-1+OPEm)*(-(OPEm*SO[p1]^2*SP[p1,p1])+
OPEm*SO[p1]*SO[p3]*SP[p1,p1]-2*SO[p3]^2*SP[p1,p1]+
2*SO[p1]*SO[p3]*SP[p1,p3]+2*OPEm*SO[p1]*SO[p3]*SP[p1,p3]+
2*SO[p3]^2*SP[p1,p3]-2*OPEm*SO[p3]^2*SP[p1,p3]-
OPEm*SO[p1]^2*SP[p3,p3]-2*SO[p1]*SO[p3]*SP[p3,p3]+
OPEm*SO[p1]*SO[p3]*SP[p3,p3]))/
(Epsilon*(-1+OPEm)*OPEm*(-SO[p1]+SO[p3])^3)
,
kli[k_,FAD[k_,k_,(k_)-(p1_)]*SOD[k_]^(-1+OPEm)*
SPD[k_,p3_]]:>(-I*Sn*SO[p1]^(-2+OPEm)*SO[p3])/(Epsilon*OPEm)
,
kli[k_,FAD[k_,k_,(k_)-(p3_)]*SOD[k_]^(-1+OPEm)*
SPD[k_,p1_]]:>(-I*Sn*SO[p1]*SO[p3]^(-2+OPEm))/(Epsilon*OPEm)
,
kli[k_,FAD[(k_)-(p3_),(k_)-(p1_)]*SOD[k_]^(-2+OPEm)]:>
(2*I*Sn*SO[p3]^(-1+OPEm))/(Epsilon*(-1+OPEm)*(SO[p1]-SO[p3]))+
(2*I*Sn*SO[p1]^(-1+OPEm))/(Epsilon*(-1+OPEm)*(-SO[p1]+SO[p3]))
,
kli[k_,FAD[k_,(k_)-(p1_)]*SOD[k_]^(-2+OPEm)]:>
(-2*I*Sn*SO[p1]^(-2+OPEm))/(Epsilon*(-1+OPEm))
,
kli[k_,FAD[k_,(k_)-(p3_)]*SOD[k_]^(-2+OPEm)]:>
(-2*I*Sn*SO[p3]^(-2+OPEm))/(Epsilon*(-1+OPEm))
,
kli[k_,FAD[k_,(k_)-((*p3*)_),(k_)-((*p1*)_)]* SOD[k_]^(-2+OPEm)]:>0
,
kli[k_,FAD[k_,(k_)-(p1_)]*SOD[k_]^(-1+OPEm)]:>
(-2*I*Sn*SO[p1]^(-1+OPEm))/(Epsilon*OPEm)
,
kli[k_,FAD[k_,k_,(k_)-((*p1*)_)]*SOD[k_]^(-1+OPEm)]:>0
,
kli[k_,FAD[k_,(k_)-(p3_)]*SOD[k_]^(-1+OPEm)]:>
(-2*I*Sn*SO[p3]^(-1+OPEm))/(Epsilon*OPEm)
,
kli[k_,FAD[k_,k_,(k_)-((*p3*)_)]*SOD[k_]^(-1+OPEm)]:>0
,
kli[k_,FAD[(k_)-(p3_),(k_)-(p1_)]*SOD[k_]^(-1+OPEm)]:>
(2*I*Sn*SO[p1]^OPEm)/(Epsilon*OPEm*(-SO[p1]+SO[p3]))-
(2*I*Sn*SO[p3]^OPEm)/(Epsilon*OPEm*(-SO[p1]+SO[p3]))
,
kli[k_,FAD[k_,k_,(k_)-(p1_)]*SOD[k_]^OPEm*SPD[k_,p3_]]:>
(-I*Sn*SO[p1]^(-1+OPEm)*SO[p3])/(Epsilon*(1+OPEm))
,
kli[k_,FAD[k_,(k_)-(p1_)]*SOD[k_]^OPEm]:>
(-2*I*Sn*SO[p1]^OPEm)/(Epsilon*(1+OPEm))
,
kli[k_,FAD[k_,k_,(k_)-(p3_)]*SOD[k_]^OPEm*SPD[k_,p1_]]:>
(-I*Sn*SO[p1]*SO[p3]^(-1+OPEm))/(Epsilon*(1+OPEm))
,
kli[k_,FAD[k_,(k_)-(p3_)]*SOD[k_]^(en_.+OPEm)]:>
(-2*I*Sn*SO[p3]^(en+OPEm))/(Epsilon*(1+OPEm+en))
,
kli[k_,FAD[(k_)-(p3_),(k_)-(p1_)]*SOD[k_]^(en_. +OPEm)]:>
(2*I*Sn*SO[p3]^(1+en+OPEm))/(Epsilon*(1+en+OPEm)*(SO[p1]-SO[p3]))+
(2*I*Sn*SO[p1]^(1+en+OPEm))/(Epsilon*(1+en+OPEm)*(-SO[p1]+SO[p3]))
,

kli[k_,FAD[k_,k_,(k_)-((*p1*)_)]*SOD[k_]^(1+OPEm)]:>0,
kli[k_,FAD[k_,k_,(k_)-((*p3*)_)]*SOD[k_]^(1+OPEm)]:>0,
kli[k_,FAD[k_,(k_)-((*p3*)_),(k_)-((*p1*)_)]* SOD[k_]^(1+OPEm)]:>0,
kli[k_,FAD[k_,k_,(k_)-((*p3*)_),(k_)-((*p1*)_)]* SOD[k_]^(2+OPEm)]:>0,
kli[k_,FAD[k_,(k_)-((*p3*)_),(k_)-((*p1*)_)]* SOD[k_]^(-1+OPEm)]:>0,
kli[k_,FAD[k_,k_,(k_)-((*p3*)_),(k_)-((*p1*)_)]* SOD[k_]^(-1+OPEm)]:>0,
kli[k_,FAD[k_,k_,(k_)-((*p1*)_)]*SOD[k_]^OPEm]:>0,
kli[k_,FAD[k_,k_,(k_)-((*p3*)_)]*SOD[k_]^OPEm]:>0,
kli[k_,FAD[k_,(k_)-((*p3*)_),(k_)-((*p1*)_)]*SOD[k_]^OPEm]:>0,
kli[k_,FAD[k_,k_,(k_)-((*p3*)_),(k_)-((*p1*)_)]* SOD[k_]^(1+OPEm)]:>0,
kli[k_,FAD[k_,k_,(k_)-((*p3*)_),(k_)-((*p1*)_)]*SOD[k_]^OPEm]:> 0,

(* newW*)
kli[k_,FAD[k_, -p1_ + p2_ + k_]*(SOD[p2_] + SOD[k_])^(OPEm + en_.)]:>
(-2*I*Sn*(SO[p1]^(OPEm+1+en) - SO[p2]^(OPEm+1+en)))/
	(Epsilon*(OPEm+1+en)*(SO[p1] - SO[p2]))
,
(*MAYBE*)
kli[k_,FAD[k_, k_ - p2_ + p1_]*(-SOD[k_] + SOD[p1_])^(OPEm+en_.)]:>
(-2*I*Sn*(SO[p1]^(1 + OPEm + en) - SO[p2]^(1 + OPEm + en)))/
	(Epsilon*(1 + OPEm + en)*(SO[p1] - SO[p2]))
,
kli[k_,FAD[k_, k_ - p2_ + p1_]*(SOD[k_] - SOD[p1_])^(OPEm+en_.)]:>
(-1)^(OEPm+en) (-2*I*Sn*(SO[p1]^(1 + OPEm + en) - SO[p2]^(1 + OPEm + en)))/
	(Epsilon*(1 + OPEm + en)*(SO[p1] - SO[p2]))
,


kli[k_,FAD[k_, -p2_ + k_] (SOD[p1_]-SOD[k_])^(OPEm+en_.)]:>
(-2*I*Sn*(SO[p1]^(1 + en + OPEm)/((1 + en + OPEm)*SO[p2]) -
(SO[p1] - SO[p2])^(1 + en + OPEm)/((1 + en + OPEm)*SO[p2])))/Epsilon
,

(*new 03*)
kli[k_,FAD[k_, p2_ + k_] (SOD[p1_]-SOD[k_])^(OPEm+en_.)]:>
(-2*I*Sn*(SO[p1]^(1 + en + OPEm)/((1 + en + OPEm)*(-SO[p2])) -
(SO[p1] + SO[p2])^(1 + en + OPEm)/((1 + en + OPEm)*(-SO[p2]))))/Epsilon
,


kli[k_, FAD[k_, -p2_ + (k_)]* (SOD[p1_] - SOD[k_])^(OPEm + (en_.))] :>
		(-2*I*Sn*(SO[p1]^(1 + en + OPEm)/((1 + en + OPEm)*SO[p2]) -
					(SO[p1] - SO[p2])^(1 + en + OPEm)/((1 + en + OPEm)*SO[p2])))/
			Epsilon
,
kli[k_, FAD[k_, -p1_ + (k_), -(*p2*)_ + (k_)]*
			(SOD[p1_] - SOD[k_])^(OPEm + ((*en*)_.))] :> 0
,
kli[k_, FAD[k_, -p1_ + (k_), -p1_ + (k_), -(*p2*)_ + (k_)]*
			(SOD[p1_] - SOD[k_])^(OPEm + ((*en*)_.))] :> 0
,
kli[k_, FAD[k_, k_, -(*p1*)_ + (*p2*)_ + (k_)]*SOD[k_]^(OPEm + ((*en*)_.))] :> 0
,
kli[k_, FAD[k_, k_, -p1_ + p2_ + (k_)]*SOD[k_]^(OPEm + (en_.))*
			SPD[p1_, k_]] :>
		(-I*Sn*SO[p1]*(SO[p1] - SO[p2])^(-1 + en + OPEm))/
			(Epsilon*(1 + en + OPEm))
,
kli[k_, FAD[k_, k_, -p1_ + (k_)]*SOD[k_]^(OPEm + (en_.))*SPD[p2_, k_]] :>
		(I*(en + OPEm)*(-(en + OPEm)^(-1) + (1 + en + OPEm)^(-1))*Sn*
				SO[p1]^(-1 + en + OPEm)*SO[p2])/Epsilon
,
kli[k_, FAD[k_, k_, -p1_ + p2_ + (k_)]*SOD[k_]^(OPEm + (en_.))*
			SPD[p2_, k_]] :>
		(-I*Sn*(SO[p1] - SO[p2])^(-1 + en + OPEm)*SO[p2])/
			(Epsilon*(1 + en + OPEm))
,
kli[k_, FAD[-p2_ + k_, -p1_ + k_]*(SOD[p2_] - SOD[k_])^(en_. + OPEm)] :>
(2*I*(-1)^(1 + en + OPEm)*Sn*(SO[p1] - SO[p2])^(en + OPEm))/
	(Epsilon*(1 + en + OPEm))
,
kli[k_, FAD[k_, -p1_ + (k_)]*(SOD[p2_] - SOD[k_])^(en_. + OPEm)*SOD[k_]
	] :>
(-2*I*Sn*(SO[p2]^(2 + en + OPEm) -
(-SO[p1] + SO[p2])^(1 + en + OPEm)*((1 + en + OPEm)*SO[p1] + SO[p2])))/
	(Epsilon*(1 + en + OPEm)*(2 + en + OPEm)*SO[p1])
,
kli[k_, FAD[k_, -p2_ + (k_)]*(SOD[p2_] - SOD[k_])^(en_. + OPEm)*SOD[k_]] :>
(-2*I*Sn*SO[p2]^(1 + en + OPEm))/(Epsilon*(1 + en + OPEm)*(2 + OPEm+en))
,
kli[k_, FAD[(k_) - (p2_), (k_) - (p1_)]*SOD[k_]*(-SOD[k_] + SOD[p2_])^(e + OPEm)] :>
(2*I*(-1)^(1 + e + OPEm)*Sn*
		((SO[p1] - SO[p2])^(1 + e + OPEm)/(1 + e + OPEm) -
			(SO[p1] - SO[p2])^(1 + e + OPEm)/((1 + e + OPEm)*(2 + e + OPEm)) +
			((SO[p1] - SO[p2])^(e + OPEm)*SO[p2])/(1 + e + OPEm)))/Epsilon
};

(*TODO This is simply not right! One should never mess up with such
	low-level functions as If!!!*)
Unprotect[If];
If[ -Re[OPEi - OPEm] > 1,
	aa_,
	(*bb*)_
] :=
	aa;
If[ -Re[OPEi - OPEm] > 2,
	aa_,
	(*bb*)_
] :=
	aa;

If[ 1 + Re[OPEi] > -1,
	a_,
	_
] :=
	a;
If[ 1 + Re[OPEi] > -2,
	a_,
	_
] :=
	a;

If[ -1 + Re[OPEm] > -3,
	a_,
	_
] :=
	a;
If[ -1 + Re[OPEm] > -2,
	a_,
	_
] :=
	a;
If[ -2 + Re[OPEm] > -1,
	a_,
	_
] :=
	a;
If[ -2 + Re[OPEm] > -2,
	a_,
	_
] :=
	a;
If[ -2 + Re[OPEm] > -3,
	a_,
	_
] :=
	a;
If[ -2 + Re[OPEm] > -1,
	a_,
	_
] :=
	a;
If[ -1 + Re[OPEm] > -1,
	a_,
	_
] :=
	a;
If[ -1 + Re[OPEm] > -1 && -1 + Re[OPEm] > -1,
	a_,
	_
] :=
	a;
If[ -2 + Re[OPEm] > -1 && -2 + Re[OPEm] > -1,
	a_,
	_
] :=
	a;

FCPrint[1,"OPEIntegrate2.m loaded"];
End[]
