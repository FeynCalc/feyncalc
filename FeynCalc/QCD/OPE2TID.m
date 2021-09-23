(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPE2TID *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 September '97 at 9:16 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  special OPE-2loop tensor integral decomposition *)

(* ------------------------------------------------------------------------ *)

OPE2TID::usage =
"OPE2TID[exp, k1, k2, p] does a special tensor integral decomposition of exp.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`OPE2TID`Private`"]

mU::usage="";
nu::usage="";
rho::usage="";

diracsimp[a_] :=
	If[ !FreeQ[a, DiracGamma],
		DiracSimplify[a],
		a
	];
diracsimp2[a_] :=
	If[ !FreeQ[a, DiracGamma],
		DiracSimplify[a//Contract],
		a
	];

Options[OPE2TID] = {Uncontract -> False, Contract -> True,
										Dimension -> D, EpsContract -> False};

(* recursion sometimes necessary *)

OPE2TID[exp_, k1_, k2_, p_, opt___Rule] :=
	Block[ {temp},
		If[ FreeQ2[exp,{k1,k2}],
			exp,
			temp = ApartFF[ope2TID[exp, k1, k2, p, opt],{k1,k2}];
			If[ !FreeQ2[temp, {k1,k2}],
				temp = ope2TID[temp, k1,k2,p,opt];
			];
			temp = ApartFF[OPE1Loop[{k1,k2}, temp], {k1,k2}];

			temp
		]
	];

ope2TID[exp_, k1_, k2_, p_, opt___Rule] :=
(* maybe *)
	ope2TID[exp, k1, k2, p, opt] =
	Block[ {ope2tid,dUMMY = FCGV[ToString[Unique["lI"]]], temp, ntemp, nok1k2factor,
	temp0, n, alL, beT,checkd,muu,k12shift = {},dumi,diramp,
	alpha = FCGV[ToString[Unique["lI"]]], beta = FCGV[ToString[Unique["lI"]]],
	delfactor, temp2, temp3, tempf, temp4, qQQ,epscontract,
	sccon, eeeps,eepcsa, rhoo,phead,fake,mem,
	epscos, epscont, epsc, decrules, decrulespv, checkk, fcheck,
	decrulesspecial, speciallabel = False, dirrramp,contractlabel,kape
	},
		If[ FreeQ[exp,k1] || FreeQ[exp, k2],
			temp  = exp,
			n = Dimension /. {opt} /. Options[OPE2TID];
			contractlabel = Contract /. {opt} /. Options[OPE2TID];
			temp = Expand2[ChangeDimension[exp, n], {k1, k2}];
			dirrramp[ww_, ka1_, ka2_] :=
				If[ Head[ww]=!=Times,
					DiracTrace[ww],
					If[ SelectNotFree[SelectFree[ww,{DOT, DiracGamma}], {ka1,ka2}] *
						SelectFree[SelectFree[ww,{DOT, DiracGamma}], {ka1,ka2}] *
										SelectNotFree[ww,{DOT, DiracGamma}] === ww,
						SelectNotFree[SelectFree[ww,{DOT, DiracGamma}], {ka1,ka2}] *
						DiracTrace[SelectNotFree[ww,{DOT, DiracGamma}] *
												SelectFree[SelectFree[ww,{DOT,DiracGamma}], {ka1,ka2}]
											],
						DiracTrace[ww]
					]
				];
			If[ (Uncontract/.{opt} /.Options[OPE2TID]) === True,
				temp = Uncontract[temp,k1,k2];
				temp = temp /. DiracTrace[ab_] :> dirrramp[ab,k1,k2];
			];
			If[ (Uncontract/.{opt} /.Options[OPE2TID]) === All,
				temp = Uncontract[temp,k1,k2, Pair->{p} ];
				temp = temp /. DiracTrace[ab_] :> dirrramp[ab,k1,k2];
			];
			phead[y_] :=
				If[ Head[y] === Integer,
					If[ y<0,
						True,
						False
					],
					If[ FreeQ2[y, {OPEi, OPEj}],
						True,
						False
					]
				];
			If[ Head[temp] === Plus,
				temp = Map[ope2tid[#, k1,k2,p]&, temp] /. ope2tid -> ope2TID,
				If[ !FreeQ[temp, Power[_,(hh_ /; Head[hh] =!= Integer)]],
					temp = temp /. Power[aa_,(ha_ /; Head[ha] =!= Integer)]:>
									Power2[aa, ha];
				];

				(* careful here; recursion possible ... *)
				If[ FreeQ[temp, Power2[_,(hh_/;phead[hh])]] &&
					!FreeQ[temp, Pair[Momentum[OPEDelta,___], Momentum[k1,___]]],
					If[ !FreeQ[SelectFree[temp, {FeynAmpDenominator,
																		Power2[_,(hhh_/;Head[hhh]=!=Integer)
																					]}], k2
										],
						temp = temp /. Pair[Momentum[OPEDelta,di1___],
																Momentum[k1,di2___]] :>
													(Pair[Momentum[OPEDelta,di1], Momentum[k1,di2]]^
														fake[OPEm])
					]
				];
				If[ FreeQ[temp, Power2[_,(hh_/;phead[hh])]] &&
					!FreeQ[temp, Pair[Momentum[OPEDelta,___], Momentum[k2,___]]],
					If[ !FreeQ[SelectFree[temp, {FeynAmpDenominator,
																		Power2[_,(hhh_/;Head[hhh]=!=Integer)]}], k1
										],
						temp = temp /. Pair[Momentum[OPEDelta,di1___],
																Momentum[k2,di2___]] :>
													(Pair[Momentum[OPEDelta,di1], Momentum[k2,di2]]^
														fake[OPEm])
					]
				];


				(*
				(*NIX shift ; 12/94 *)
				If[(!FreeQ2[temp, {Power2[
														(_. Pair[Momentum[OPEDelta,___], Momentum[k1,___]]
														), (hh_ /; phead[hh])
																],
													Power2[
														(_. Pair[Momentum[OPEDelta,___], Momentum[k1,___]] +
														_. Pair[Momentum[OPEDelta,___], Momentum[p, ___]]
														),(hh_ /; phead[hh])
																]
													}
									]
					) &&
					(!FreeQ[SelectFree[temp, {FeynAmpDenominator,
																Power2[_,(hhh_/;Head[hhh]=!=Integer)]}],
									k2]
					) && (* do only if really necessary (otherwise: recursion danger) *)
					FreeQ[temp,
							Power2[(_. Pair[Momentum[OPEDelta,___], Momentum[k1,___]] +
											_. Pair[Momentum[OPEDelta,___], Momentum[k2,___]] + _.
											),(hh_ /; phead[hh])
										]
							]
					,
					k12shift = {k1 -> (-k1 + k2)}
					];
				If[(!FreeQ2[temp, {Power2[
													(_. Pair[Momentum[OPEDelta,___], Momentum[k2,___]]
													),(hh_ /; phead[hh])
																],
													Power2[
													(_. Pair[Momentum[OPEDelta,___], Momentum[k2,___]] +
													_. Pair[Momentum[OPEDelta,___], Momentum[p, ___]]
													),(hh_ /; phead[hh])
																]
													}
									]
					) &&
					(!FreeQ[SelectFree[temp, {FeynAmpDenominator,
																Power2[_,(hhh_/;Head[hhh]=!=Integer)]}],
									k1]
					) && (* do only if really necessary (otherwise: recursion danger) *)
					FreeQ[temp, Power2[
											(_. Pair[Momentum[OPEDelta,___], Momentum[k1,___]] +
											_. Pair[Momentum[OPEDelta,___], Momentum[k2,___]] + _.
											),(hh_ /; phead[hh])
														]
							]
					,
					k12shift = {k2 -> (-k2 + k1)}
					];
				*)

				(* undo for a special case *)
				If[ !FreeQ[temp, Pair[Momentum[k1,n], Momentum[k2,n]]],
					If[ MatchQ[SelectNotFree[SelectFree[temp,FeynAmpDenominator],{k1,k2}],
											Pair[Momentum[k1,n], Momentum[k2,n]] Pair[
											Momentum[k1|k2,n],LorentzIndex[__]] Power2[Pair[
											Momentum[k1,n],Momentum[OPEDelta,n]],
											(hh_/;Head[hh]=!=Integer)                 ]
										],
						k12shift = {};
						speciallabel = True
					]
				];
				If[ k12shift =!= {},
					FCPrint[2,"shifting ",k12shift[[1]]];
					temp = Collect2[ApartFF[EpsEvaluate[ExpandScalarProduct[
									(temp /. k12shift)/.DiracTrace->Tr2]//diracsimp],{k1,k2}
																	],{k1,k2}
													],
					If[ !FreeQ[temp,DiracGamma],
						temp = Collect2[ApartFF[
											diracsimp[temp]//EpsEvaluate,{k1,k2}],k1,k2],
						temp = EpsEvaluate[temp] /. Pair -> ExpandScalarProduct
					]
				];
				If[ Head[temp] === Plus,
					FCPrint[1,"Map ope2tid"];
					temp = Map[ope2tid[#, k1,k2,p]&, temp] /. ope2tid -> ope2TID;
				(* shift back *)
					FCPrint[2,"shifting back"];
					temp = ExpandScalarProduct[(temp /. k12shift)//diracsimp2];
					If[ contractlabel === True,
						FCPrint[2,"contract "];
						temp = Expand2[temp, LorentzIndex] /. Pair->PairContract3 /.
											PairContract3->Pair;
					];
					temp = EpsEvaluate[temp]//diracsimp2;
					FCPrint[2,"collect in OPE2TID"];
					temp = Collect2[temp, {k1,k2}, Factoring -> False];
						,

					(*  "amputate" DiracTrace ... *)
					If[ True,
						If[ !FreeQ[temp, DiracTrace],
							rhoo = Unique[dumi];
							temp = temp /. { DiracTrace[aa_. DOT[b___,
											DiracGamma[Momentum[k1, ___], ___], c___]
																				] :>
							(* the n  is the Dimension set in the Options *)
											(DiracTrace[aa DOT[b, DiracGamma[LorentzIndex[
																	alpha,n],n],c] ] *
																Pair[Momentum[k1, n], LorentzIndex[alpha,n]]
											) /; FreeQ2[{aa,{b}, {c}}, {k1, k2}],
															DiracTrace[aa_. DOT[b___,
											DiracGamma[Momentum[k1, ___], ___],
											DiracGamma[Momentum[k2, ___], ___], c___]
																				] :>
											(DiracTrace[aa DOT[b, DiracGamma[LorentzIndex[alpha, n], n],
																						DiracGamma[LorentzIndex[beta, n], n
																											],c]
																				] *
																		Pair[Momentum[k1, n], LorentzIndex[alpha,n]] *
																	Pair[Momentum[k2, n], LorentzIndex[beta,n]]
											) /; FreeQ2[{aa,{b}, {c}}, {k1, k2}]
														} /. {
											(DiracTrace[a_] Pair[Momentum[k1, ___],
																					LorentzIndex[alpha, ___]] *
																			Pair[Momentum[k2, ___],
																					LorentzIndex[beta, ___]]*
																			Eps[aaa___,Momentum[k1,___], bbb___]
											) :> (DiracTrace[a] *
																Pair[Momentum[k1, n], LorentzIndex[alpha, n]] *
																Pair[Momentum[k2, n], LorentzIndex[beta, n]]*
																Eps[aaa,LorentzIndex[rhoo, n], bbb] *
																Pair[Momentum[k1, n], LorentzIndex[rhoo, n]]
													),
											(DiracTrace[a_] Pair[Momentum[k1, ___],
																					LorentzIndex[alpha, ___]] *
																			Pair[Momentum[k2, ___],
																					LorentzIndex[beta, ___]]*
																			Eps[aaa___,Momentum[k2,___], bbb___]
											) :> (DiracTrace[a] *
																Pair[Momentum[k1, n], LorentzIndex[alpha, n]] *
																Pair[Momentum[k2, n], LorentzIndex[beta, n]]*
																Eps[aaa,LorentzIndex[rhoo, n], bbb] *
																Pair[Momentum[k2, n], LorentzIndex[rhoo, n]]
													)}
						]
					];
					If[ True,
						If[ !FreeQ[temp, DiracGamma],
							diramp[ka_,en_] :=
								Block[ {uni},
									uni = LorentzIndex[Unique[dumi],en];
									DiracGamma[uni, en] Pair[uni,
									Momentum[ka, en]]
								];
							temp = temp /. {DiracGamma[Momentum[k1,n],n] :> diramp[k1,n],
															DiracGamma[Momentum[k2,n],n] :> diramp[k2,n]
														};
							temp = DotSimplify[temp, Expanding -> False];
						]
					];

					(*  "amputate" a special Eps... *)
					If[ FreeQ[temp, Eps[__]^2] &&
						(!FreeQ[temp, Eps[a___, Momentum[k1, e___], Momentum[k2, e___], b___]]
						) && (FreeQ[temp, Eps[a___, Momentum[k1, e___], Momentum[k2, e___], b___] * Eps[c__]
												]),
						alL = FCGV[ToString[Unique["lI"]]];
						beT = FCGV[ToString[Unique["lI"]]];
						FCPrint[1,"AMPuTATe EPs"];
						temp = temp /. Eps[a___, Momentum[k1, en___],
																		Momentum[k2, en___], b___] :>
									(Pair[LorentzIndex[alL,n], Momentum[k1,n]] *
										Pair[LorentzIndex[beT,n], Momentum[k2,n]] *
										Eps[a, LorentzIndex[alL,n], LorentzIndex[beT,n], b]
									)
					];
					(* another special case ... *)
					If[ MatchQ[temp, _. Pair[Momentum[k1,___],LorentzIndex[__]] *
													Pair[Momentum[k2,___],LorentzIndex[__]] *
													Eps[(*aaa*)___, Momentum[k1,(*didi*)___], b___]],
						rhoo = Unique[dumi];
						temp = temp /. Eps[aaa___, Momentum[k1,(*didi*)___], b___] :>
													(Eps[aaa,LorentzIndex[rhoo,n],b] *
														Pair[Momentum[k1,n], LorentzIndex[rhoo,n]]
													)
					];
					If[ MatchQ[temp, _. Pair[Momentum[k1,___],LorentzIndex[__]] *
													Pair[Momentum[k2,___],LorentzIndex[__]] *
													Eps[(*aaa*)___, Momentum[k2,(*didi*)___], b___]
									],
						rhoo = Unique[dumi];
						temp = temp /. Eps[aaa___,Momentum[k2,(*didi*)___], b___] :>
													(Eps[aaa,LorentzIndex[rhoo,n],b] *
														Pair[Momentum[k2,n], LorentzIndex[rhoo,n]]
													)
					];
					temp0 = temp;

					(* do Levi-Civita -contractions in four or D dimensions
						right away in case no integration momenta are
						around anymore in the Eps.
					*)
					eepcsa[yy__] :=
						Contract[Eps[yy]^2, EpsContract->True];
					epsc[xy__] :=
						If[ (EpsContract /. {opt} ) === False ||
										!FreeQ2[{xy}, {k1,k2}],
							Eps[xy],
							ChangeDimension[eeeps[xy], EpsContract /. {opt} /.
							Options[OPE2TID]] /. {eeeps[w__]^2 :> eepcsa[w]} /.
							eeeps -> Eps
						];
					(*
					epscontract[y_] := y;
					*)
					sccon[yy_] :=
						yy /. Pair -> PairContract /. PairContract->Pair;
					epscontract[y_] :=
						If[ (EpsContract/.{opt})===False,
							EpsEvaluate[sccon[y]],
							If[ Head[y] === Plus,
								Map[epscos, y//sccon],
								epscos[y//sccon]
							]
						];
					epscos[a_] :=
						If[ Head[a] =!= Times,
							a,
							SelectFree[a, LorentzIndex] *
							epscont[SelectNotFree[a, LorentzIndex]]
						];
					epscont[1] = 1;
					epscont[y_] := (*epscont[y] =*)
						Block[ {tt},
							tt = Contract[y, EpsContract -> False];
							If[ FreeQ2[tt, {Eps[___, Momentum[k1, ___], ___],
														Eps[___, Momentum[k2, ___], ___]}
											],
								If[ !FreeQ[tt, Eps],
									tt = tt /. Eps -> epsc /. epsc -> Eps;
									If[ (EpsContract /. {opt} /. Options[OPE2TID]
										) =!= False,
										tt = Contract[tt, EpsContract -> True]
									],
									tt = Collect2[tt, LorentzIndex]
								]
							];
					(*
						If[FreeQ[tt, LorentzIndex], tt = ChangeDimension[tt, 4]];
					*)
							tt
						];
					If[ Head[temp] === Times && !FreeQ2[temp, {k1,k2,OPEDelta}],
					(* canonize *)
						temp = Factor2[temp];
						nok1k2factor = SelectFree[temp, {k1, k2}];
						temp2        = Factor2[temp/nok1k2factor];
					(* there may be no Eps in delfactor *)
						delfactor    = SelectNotFree[SelectNotFree[temp2, Pair], OPEDelta];
						FCPrint[3,"delfactor = ",delfactor // FeynCalcForm];


					(* this function is ESSENTIAL !!! *)
						checkd[yy_] :=
							!FreeQ[delfactor, yy];
						checkd[yy_,zz__] :=
							(!FreeQ[delfactor, yy]) && checkd[zz];
						temp3        = Factor2[temp2/delfactor];
						tempf        = SelectNotFree[temp3, FeynAmpDenominator];
						temp4        = qQQ[SelectFree[temp3, FeynAmpDenominator]];
						checkk = Factor2[temp0] -
										Factor2[nok1k2factor (temp4/.qQQ->Identity) tempf delfactor];
						mem[aa_] :=
							MemberQ[{Pair,Eps},aa];
						mem[aa_,bb__] :=
							mem[aa] && mem[bb];

						(* so :  exp = nok1k2factor temp4 tempf delfactor *)
						If[ checkk =!= 0,
							temp = exp,
							If[ (!FreeQ2[delfactor, {k1, k2}])(* QUark, && (Head[delfactor] =!= Pair)*),
								If[ speciallabel =!= True,
									decrulesspecial = {},
									decrulesspecial = {
									(* special stuff *)
									qQQ[Pair[Momentum[k1,n],Momentum[k2,n]] *
											Pair[Momentum[k1, n], LorentzIndex[mu_,n]]
										] :>
										(
										(Pair[LorentzIndex[mu, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[k1, n]])/
											Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
										(Pair[LorentzIndex[mu, n], Momentum[OPEDelta, n]]*
												(Pair[Momentum[OPEDelta, n], Momentum[k1, n]]*
													Pair[Momentum[p, n], Momentum[p, n]] -
													Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
													Pair[Momentum[p, n], Momentum[k1, n]]))/
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2
										) /; checkd[k1],
									qQQ[Pair[Momentum[k1,n],Momentum[k2,n]] *
											Pair[Momentum[k2, n], LorentzIndex[mu_,n]]
										] :>
										(
										(Pair[LorentzIndex[mu, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[k2, n]])/
											Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
										(Pair[LorentzIndex[mu, n], Momentum[OPEDelta, n]]*
												(Pair[Momentum[OPEDelta, n], Momentum[k2, n]]*
													Pair[Momentum[p, n], Momentum[p, n]] -
													Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
													Pair[Momentum[p, n], Momentum[k2, n]]))/
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2
										) /; checkd[k1]
									};
								];
								decrules = {
								(* k1 k2 ki *)
								qQQ[
								Eps[r___, Momentum[k1,___], Momentum[k2,___], s___]*
								T2_[w___, Momentum[qk_, ___], z___]] :>
								epscontract[
								FCPrint[1,"USING K1K2KIRULE"];
								rho = Unique["lI"];
								( -((Pair[Momentum[k1, D], Momentum[qk, D]]*
											Pair[Momentum[k2, D], Momentum[p, D]]*
											(Eps[r, LorentzIndex[rho, D], Momentum[OPEDelta, D], s]*
													Pair[Momentum[OPEDelta, D], Momentum[p, D]]*
													T2[w, LorentzIndex[rho, D], z] +
												Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
													T2[w, Momentum[OPEDelta, D], z]))/
										((2 - D)*Pair[Momentum[OPEDelta, D], Momentum[p, D]]^2)) +
									(Pair[Momentum[k1, D], Momentum[p, D]]*
										Pair[Momentum[k2, D], Momentum[qk, D]]*
										(Eps[r, LorentzIndex[rho, D], Momentum[OPEDelta, D], s]*
												Pair[Momentum[OPEDelta, D], Momentum[p, D]]*
												T2[w, LorentzIndex[rho, D], z] +
											Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												T2[w, Momentum[OPEDelta, D], z]))/
									((2 - D)*Pair[Momentum[OPEDelta, D], Momentum[p, D]]^2) -
									(Pair[Momentum[k1, D], Momentum[p, D]]*
										Pair[Momentum[k2, D], Momentum[OPEDelta, D]]*
										Pair[Momentum[p, D], Momentum[qk, D]]*
										(Eps[r, LorentzIndex[rho, D], Momentum[OPEDelta, D], s]*
												Pair[Momentum[OPEDelta, D], Momentum[p, D]]*
												T2[w, LorentzIndex[rho, D], z] -
											Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												T2[w, Momentum[OPEDelta, D], z] +
											D*Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												T2[w, Momentum[OPEDelta, D], z]))/
									((2 - D)*Pair[Momentum[OPEDelta, D], Momentum[p, D]]^3) +
									(Pair[Momentum[k1, D], Momentum[OPEDelta, D]]*
										Pair[Momentum[k2, D], Momentum[p, D]]*
										Pair[Momentum[p, D], Momentum[qk, D]]*
										(Eps[r, LorentzIndex[rho, D], Momentum[OPEDelta, D], s]*
												Pair[Momentum[OPEDelta, D], Momentum[p, D]]*
												T2[w, LorentzIndex[rho, D], z] -
											Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												T2[w, Momentum[OPEDelta, D], z] +
											D*Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												T2[w, Momentum[OPEDelta, D], z]))/
									((2 - D)*Pair[Momentum[OPEDelta, D], Momentum[p, D]]^3) -
									(Pair[Momentum[k1, D], Momentum[qk, D]]*
										Pair[Momentum[k2, D], Momentum[OPEDelta, D]]*
										(Eps[r, LorentzIndex[rho, D], Momentum[p, D], s]*
												Pair[Momentum[OPEDelta, D], Momentum[p, D]]*
												T2[w, LorentzIndex[rho, D], z] -
											Eps[r, LorentzIndex[rho, D], Momentum[OPEDelta, D], s]*
												Pair[Momentum[p, D], Momentum[p, D]]*
														T2[w, LorentzIndex[rho, D], z] -
												Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												T2[w, Momentum[p, D], z]))/
									((2 - D)*Pair[Momentum[OPEDelta, D], Momentum[p, D]]^2) +
									(Pair[Momentum[k1, D], Momentum[OPEDelta, D]]*
										Pair[Momentum[k2, D], Momentum[qk, D]]*
										(Eps[r, LorentzIndex[rho, D], Momentum[p, D], s]*
												Pair[Momentum[OPEDelta, D], Momentum[p, D]]*
												T2[w, LorentzIndex[rho, D], z] -
											Eps[r, LorentzIndex[rho, D], Momentum[OPEDelta, D], s]*
												Pair[Momentum[p, D], Momentum[p, D]]*
													T2[w, LorentzIndex[rho, D], z] -
											Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												T2[w, Momentum[p, D], z]))/
									((2 - D)*Pair[Momentum[OPEDelta, D], Momentum[p, D]]^2) +
									(Pair[Momentum[k1, D], Momentum[p, D]]*
										Pair[Momentum[k2, D], Momentum[OPEDelta, D]]*
										Pair[Momentum[OPEDelta, D], Momentum[qk, D]]*
										(Eps[r, LorentzIndex[rho, D], Momentum[p, D], s]*
												Pair[Momentum[OPEDelta, D], Momentum[p, D]]^2*
												T2[w, LorentzIndex[rho, D], z] -
											Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												Pair[Momentum[p, D], Momentum[p, D]]*
												T2[w, Momentum[OPEDelta, D], z] +
											D*Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												Pair[Momentum[p, D], Momentum[p, D]]*
												T2[w, Momentum[OPEDelta, D], z] +
											Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												Pair[Momentum[OPEDelta, D], Momentum[p, D]]*
												T2[w, Momentum[p, D], z] -
											D*Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												Pair[Momentum[OPEDelta, D], Momentum[p, D]]*T2[w, Momentum[p, D], z]
											))/((2 - D)*Pair[Momentum[OPEDelta, D], Momentum[p, D]]^4) -
									(Pair[Momentum[k1, D], Momentum[OPEDelta, D]]*
										Pair[Momentum[k2, D], Momentum[p, D]]*
										Pair[Momentum[OPEDelta, D], Momentum[qk, D]]*
										(Eps[r, LorentzIndex[rho, D], Momentum[p, D], s]*
												Pair[Momentum[OPEDelta, D], Momentum[p, D]]^2*
												T2[w, LorentzIndex[rho, D], z] -
											Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												Pair[Momentum[p, D], Momentum[p, D]]*T2[w, Momentum[OPEDelta, D], z
																] + D*Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												Pair[Momentum[p, D], Momentum[p, D]]*T2[w, Momentum[OPEDelta, D], z
																] + Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												Pair[Momentum[OPEDelta, D], Momentum[p, D]]*T2[w, Momentum[p, D], z
																] - D*Eps[r, Momentum[OPEDelta, D], Momentum[p, D], s]*
												Pair[Momentum[OPEDelta, D], Momentum[p, D]]*T2[w, Momentum[p, D], z])
										)/((2 - D)*Pair[Momentum[OPEDelta, D], Momentum[p, D]]^4)
											)
													] /; FreeQ2[{r,s,z,w}, {k1,k2}] && MemberQ[{k1,k2}, qk] &&
																(checkd[k1] || checkd[k2] || checkd[qk]) && mem[T2]
								(*
																checkd[k1,k2,qk] && mem[T2]
								*)
								,
								(* k2 *)
								qQQ[ T_[r___, Momentum[k2, ___], s___] ] :>
									epscontract[
									(Pair[Momentum[k2, n], Momentum[p, n]]*T[r, Momentum[OPEDelta, n], s])/
										Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
									(Pair[Momentum[k2, n], Momentum[OPEDelta, n]]*
											Pair[Momentum[p, n], Momentum[p, n]]*T[r, Momentum[OPEDelta, n], s])/
										Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 +
									(Pair[Momentum[k2, n], Momentum[OPEDelta, n]]*T[r, Momentum[p, n], s])/
										Pair[Momentum[OPEDelta, n], Momentum[p, n]]
														] /;
								FreeQ2[{r, s}, {k1, k2}] && (checkd[k1] || checkd[k2]) && mem[T],
								(* k2 k2 *)
								(* this is old,  but o.k., checked  24.4.94 *)
									qQQ[T1_[r___, Momentum[k2, ___], s___]^2
											] :>
											epscontract[
								( -((Pair[Momentum[k2, n], Momentum[k2, n]]*
												T1[r, LorentzIndex[dUMMY, n], s]*T1[r, LorentzIndex[dUMMY, n], s])/
											(2 - n)) + (2*Pair[Momentum[k2, n], Momentum[OPEDelta, n]]*
											Pair[Momentum[k2, n], Momentum[p, n]]*
											T1[r, LorentzIndex[dUMMY, n], s]*T1[r, LorentzIndex[dUMMY, n], s])/
										((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]) -
									(Pair[Momentum[k2, n], Momentum[OPEDelta, n]]^2*
											Pair[Momentum[p, n], Momentum[p, n]]*T1[r, LorentzIndex[dUMMY, n], s]*
											T1[r, LorentzIndex[dUMMY, n], s])/
										((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) +
									(Pair[Momentum[k2, n], Momentum[p, n]]^2*T1[r, Momentum[OPEDelta, n], s]*
											T1[r, Momentum[OPEDelta, n], s])/
										Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
									(2*(1 - n)*Pair[Momentum[k2, n], Momentum[OPEDelta, n]]*
											Pair[Momentum[k2, n], Momentum[p, n]]*
											Pair[Momentum[p, n], Momentum[p, n]]*T1[r, Momentum[OPEDelta, n], s]*
											T1[r, Momentum[OPEDelta, n], s])/
										((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) -
									(Pair[Momentum[k2, n], Momentum[k2, n]]*
											Pair[Momentum[p, n], Momentum[p, n]]*T1[r, Momentum[OPEDelta, n], s]*
											T1[r, Momentum[OPEDelta, n], s])/
										((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) +
									((1 - n)*Pair[Momentum[k2, n], Momentum[OPEDelta, n]]^2*
											Pair[Momentum[p, n], Momentum[p, n]]^2*
											T1[r, Momentum[OPEDelta, n], s]*T1[r, Momentum[OPEDelta, n], s])/
										((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) -
									(n*Pair[Momentum[k2, n], Momentum[OPEDelta, n]]*
											Pair[Momentum[k2, n], Momentum[p, n]]*T1[r, Momentum[p, n], s]*
											T1[r, Momentum[OPEDelta, n], s])/
										((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) +
									(Pair[Momentum[k2, n], Momentum[k2, n]]*T1[r, Momentum[p, n], s]*
											T1[r, Momentum[OPEDelta, n], s])/
										((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]) -
									((1 - n)*Pair[Momentum[k2, n], Momentum[OPEDelta, n]]^2*
											Pair[Momentum[p, n], Momentum[p, n]]*T1[r, Momentum[p, n], s]*
											T1[r, Momentum[OPEDelta, n], s])/
										((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) -
									(n*Pair[Momentum[k2, n], Momentum[OPEDelta, n]]*
											Pair[Momentum[k2, n], Momentum[p, n]]*T1[r, Momentum[OPEDelta, n], s]*
											T1[r, Momentum[p, n], s])/
										((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) +
									(Pair[Momentum[k2, n], Momentum[k2, n]]*T1[r, Momentum[OPEDelta, n], s]*
											T1[r, Momentum[p, n], s])/
										((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]) -
									((1 - n)*Pair[Momentum[k2, n], Momentum[OPEDelta, n]]^2*
											Pair[Momentum[p, n], Momentum[p, n]]*T1[r, Momentum[OPEDelta, n], s]*
											T1[r, Momentum[p, n], s])/
										((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) +
									(Pair[Momentum[k2, n], Momentum[OPEDelta, n]]^2*T1[r, Momentum[p, n], s]*
										T1[r, Momentum[p, n], s])/Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2
											)]  /; FreeQ2[{r, s}, {k1, k2}] && (T1 === Eps) && checkd[k2],

								(* ki kj *)
								qQQ[ Eps[r___, Momentum[ki_,___], Momentum[kj_,___], s___] ] :>
								epscontract[
									(Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]*
										Pair[Momentum[ki, n], Momentum[p, n]]*
										Pair[Momentum[kj, n], Momentum[OPEDelta, n]])/
									Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
									(Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]*
										Pair[Momentum[ki, n], Momentum[OPEDelta, n]]*
										Pair[Momentum[kj, n], Momentum[p, n]])/
									Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2
											] /; (Sort[{ki, kj}] === Sort[{k1, k2}]) &&
														(checkd[ki] || checkd[kj]) && FreeQ2[{r,s},{k1,k2}],

								(* XXX *)
								(* qi qj *)
								qQQ[T1_[r___, Momentum[qi_, ___], s___]*
										T2_[t___, Momentum[qj_, ___], u___]
									] :>
								epscontract[
								( (Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]]*T1[r, Momentum[OPEDelta, n], s]*
										T2[t, Momentum[OPEDelta, n], u])/
									Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
									(Pair[Momentum[qi, n], Momentum[qj, n]]*
										(Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												T1[r, LorentzIndex[nu, n], s]*T2[t, LorentzIndex[nu, n], u] +
											Pair[Momentum[p, n], Momentum[p, n]]*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[OPEDelta, n], u] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												T1[r, Momentum[p, n], s]*T2[t, Momentum[OPEDelta, n], u] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[p, n], u]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) +
									(Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]]*
										(Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												T1[r, LorentzIndex[nu, n], s]*T2[t, LorentzIndex[nu, n], u] -
											Pair[Momentum[p, n], Momentum[p, n]]*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[OPEDelta, n], u] +
											n*Pair[Momentum[p, n], Momentum[p, n]]*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[OPEDelta, n], u] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												T1[r, Momentum[p, n], s]*T2[t, Momentum[OPEDelta, n], u] -
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												T1[r, Momentum[p, n], s]*T2[t, Momentum[OPEDelta, n], u] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[p, n], u]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) +
									(Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										Pair[Momentum[p, n], Momentum[qi, n]]*
										(Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												T1[r, LorentzIndex[nu, n], s]*T2[t, LorentzIndex[nu, n], u] -
											Pair[Momentum[p, n], Momentum[p, n]]*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[OPEDelta, n], u] +
											n*Pair[Momentum[p, n], Momentum[p, n]]*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[OPEDelta, n], u] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												T1[r, Momentum[p, n], s]*T2[t, Momentum[OPEDelta, n], u] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[p, n], u] -
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[p, n], u]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) -
									(Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										(Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[p, n], Momentum[p, n]]*T1[r, LorentzIndex[nu, n], s]*
												T2[t, LorentzIndex[nu, n], u] -
											Pair[Momentum[p, n], Momentum[p, n]]^2*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[OPEDelta, n], u] +
											n*Pair[Momentum[p, n], Momentum[p, n]]^2*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[OPEDelta, n], u] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*T1[r, Momentum[p, n], s]*
												T2[t, Momentum[OPEDelta, n], u] -
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*T1[r, Momentum[p, n], s]*
												T2[t, Momentum[OPEDelta, n], u] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[p, n], u] -
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												T1[r, Momentum[OPEDelta, n], s]*T2[t, Momentum[p, n], u] -
											2*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												T1[r, Momentum[p, n], s]*T2[t, Momentum[p, n], u] +
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												T1[r, Momentum[p, n], s]*T2[t, Momentum[p, n], u]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4)
											)
														] /; (Sort[Union[{qi,qj,k1,k2}]] === {k1,k2}) &&
																	FreeQ2[{r, s, t, u}, {k1, k2}] &&
																	(checkd[k1] (*|| check[k2]*)) && mem[T1,T2],
								(*
																	checkd[qi,qj] && mem[T1,T2],
								*)

								(* qi qj qk *)
								qQQ[Pair[Momentum[qi_, ___], LorentzIndex[mu_, ___]]*
										Pair[Momentum[qj_, ___], LorentzIndex[nu_, ___]]*
										Pair[Momentum[qk_, ___], LorentzIndex[rho_, ___]]
									] :> (
								(Pair[LorentzIndex[mu, n], Momentum[p, n]]*
										Pair[LorentzIndex[nu, n], Momentum[p, n]]*
										Pair[LorentzIndex[rho, n], Momentum[p, n]]*
										Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										Pair[Momentum[OPEDelta, n], Momentum[qk, n]]
								)/ Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3 -
									(Pair[LorentzIndex[mu, n], LorentzIndex[nu, n]]*
										Pair[LorentzIndex[rho, n], Momentum[p, n]]*
										Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
										(Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[qi, n], Momentum[qj, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) -
									(Pair[LorentzIndex[mu, n], LorentzIndex[nu, n]]*
										Pair[LorentzIndex[rho, n], Momentum[OPEDelta, n]]*
										(-(Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
													Pair[Momentum[p, n], Momentum[p, n]]) +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]])*
										(Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[qi, n], Momentum[qj, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) -
									(Pair[LorentzIndex[mu, n], LorentzIndex[rho, n]]*
										Pair[LorentzIndex[nu, n], Momentum[p, n]]*
										Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										(Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[qi, n], Momentum[qk, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) +
									(Pair[LorentzIndex[mu, n], LorentzIndex[rho, n]]*
										Pair[LorentzIndex[nu, n], Momentum[OPEDelta, n]]*
										(Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]])*
										(Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[qi, n], Momentum[qk, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) +
									(Pair[LorentzIndex[mu, n], Momentum[OPEDelta, n]]*
										Pair[LorentzIndex[nu, n], Momentum[p, n]]*
										Pair[LorentzIndex[rho, n], Momentum[p, n]]*
										(n*Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[qi, n], Momentum[qj, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[qi, n], Momentum[qk, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) -
									(Pair[LorentzIndex[mu, n], Momentum[p, n]]*
										Pair[LorentzIndex[nu, n], LorentzIndex[rho, n]]*
										Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										(Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[qj, n], Momentum[qk, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) +
									(Pair[LorentzIndex[mu, n], Momentum[OPEDelta, n]]*
										Pair[LorentzIndex[nu, n], LorentzIndex[rho, n]]*
										(Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]])*
										(Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[qj, n], Momentum[qk, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) +
									(Pair[LorentzIndex[mu, n], Momentum[p, n]]*
										Pair[LorentzIndex[nu, n], Momentum[OPEDelta, n]]*
										Pair[LorentzIndex[rho, n], Momentum[p, n]]*
										(n*Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]] -
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[qi, n], Momentum[qj, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[qj, n], Momentum[qk, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) +
									(Pair[LorentzIndex[mu, n], Momentum[p, n]]*
										Pair[LorentzIndex[nu, n], Momentum[p, n]]*
										Pair[LorentzIndex[rho, n], Momentum[OPEDelta, n]]*
										(n*Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] -
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[qi, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[qj, n], Momentum[qk, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) +
									(Pair[LorentzIndex[mu, n], Momentum[p, n]]*
										Pair[LorentzIndex[nu, n], Momentum[OPEDelta, n]]*
										Pair[LorentzIndex[rho, n], Momentum[OPEDelta, n]]*
										(-((1 + n)*Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
													Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
													Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
													Pair[Momentum[p, n], Momentum[p, n]]^2) +
											2*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]] +
											(1 + n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] +
											(1 + n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] -
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[qi, n], Momentum[qj, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3*
												Pair[Momentum[p, n], Momentum[qk, n]]*
												Pair[Momentum[qi, n], Momentum[qj, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[qi, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3*
												Pair[Momentum[p, n], Momentum[qj, n]]*
												Pair[Momentum[qi, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[qj, n], Momentum[qk, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^5) +
									(Pair[LorentzIndex[mu, n], Momentum[OPEDelta, n]]*
										Pair[LorentzIndex[nu, n], Momentum[p, n]]*
										Pair[LorentzIndex[rho, n], Momentum[OPEDelta, n]]*
										(-((1 + n)*Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
													Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
													Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
													Pair[Momentum[p, n], Momentum[p, n]]^2) +
											(1 + n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]] +
											2*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] +
											(1 + n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] -
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[qi, n], Momentum[qj, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3*
												Pair[Momentum[p, n], Momentum[qk, n]]*
												Pair[Momentum[qi, n], Momentum[qj, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[qi, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[qj, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[qj, n], Momentum[qk, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^5) +
									(Pair[LorentzIndex[mu, n], Momentum[OPEDelta, n]]*
										Pair[LorentzIndex[nu, n], Momentum[OPEDelta, n]]*
										Pair[LorentzIndex[rho, n], Momentum[p, n]]*
										(-((1 + n)*Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
													Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
													Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
													Pair[Momentum[p, n], Momentum[p, n]]^2) +
											(1 + n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]] +
											(1 + n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] -
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] +
											2*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[qi, n], Momentum[qj, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[qi, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3*
												Pair[Momentum[p, n], Momentum[qj, n]]*
												Pair[Momentum[qi, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[qj, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[qj, n], Momentum[qk, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^5) +
									(Pair[LorentzIndex[mu, n], Momentum[OPEDelta, n]]*
										Pair[LorentzIndex[nu, n], Momentum[OPEDelta, n]]*
										Pair[LorentzIndex[rho, n], Momentum[OPEDelta, n]]*
										((1 + n)*Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]^3 -
											(1 + n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]^2*
												Pair[Momentum[p, n], Momentum[qi, n]] -
											(1 + n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]^2*
												Pair[Momentum[p, n], Momentum[qj, n]] +
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]] -
											(1 + n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]^2*
												Pair[Momentum[p, n], Momentum[qk, n]] +
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] +
											n*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] +
											(2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qk, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]^2*
												Pair[Momentum[qi, n], Momentum[qj, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qk, n]]*
												Pair[Momentum[qi, n], Momentum[qj, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]^2*
												Pair[Momentum[qi, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qj, n]]*
												Pair[Momentum[qi, n], Momentum[qk, n]] +
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
												Pair[Momentum[p, n], Momentum[p, n]]^2*
												Pair[Momentum[qj, n], Momentum[qk, n]] -
											Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3*
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[qi, n]]*
												Pair[Momentum[qj, n], Momentum[qk, n]]))/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^6)
														) /; (Sort[Union[{qi,qj,qk,k1,k2}]] ===
																	Sort[{k1,k2}]) && (checkd[k1]|| checkd[k2]),

								(* qi qj qi qj*)
								qQQ[Pair[Momentum[qi_, ___], LorentzIndex[mu_, ___]]*
										Pair[Momentum[qj_, ___], LorentzIndex[nu_, ___]]*
										Pair[Momentum[qi_, ___], LorentzIndex[rho_, ___]]*
										Pair[Momentum[qj_, ___], LorentzIndex[si_, ___]]
									] :> (
								Block[ {
								t1 = Pair[LorentzIndex[mu,D],LorentzIndex[nu,D]],
								t2 = Pair[LorentzIndex[mu,D],LorentzIndex[rho,D]],
								t3 = Pair[LorentzIndex[mu,D],LorentzIndex[si,D]],
								t4 = Pair[LorentzIndex[mu,D],Momentum[OPEDelta,D]],
								t5 = Pair[LorentzIndex[mu,D],Momentum[p,D]],
								t6 = Pair[LorentzIndex[nu,D],LorentzIndex[rho,D]],
								t7 = Pair[LorentzIndex[nu,D],LorentzIndex[si,D]],
								t8 = Pair[LorentzIndex[nu,D],Momentum[OPEDelta,D]],
								t9 = Pair[LorentzIndex[nu,D],Momentum[p,D]],
								t10 = Pair[LorentzIndex[rho,D],LorentzIndex[si,D]],
								t11 = Pair[LorentzIndex[rho,D],Momentum[OPEDelta,D]],
								t12 = Pair[LorentzIndex[rho,D],Momentum[p,D]],
								t13 = Pair[LorentzIndex[si,D],Momentum[OPEDelta,D]],
								t14 = Pair[LorentzIndex[si,D],Momentum[p,D]],
								t15 = Pair[Momentum[OPEDelta,D],Momentum[p,D]],
								t16 = Pair[Momentum[OPEDelta,D],Momentum[qi,D]],
								t17 = Pair[Momentum[OPEDelta,D],Momentum[qj,D]],
								t18 = Pair[Momentum[p,D],Momentum[p,D]],
								t19 = Pair[Momentum[p,D],Momentum[qi,D]],
								t20 = Pair[Momentum[p,D],Momentum[qj,D]],
								t21 = Pair[Momentum[qi,D],Momentum[qi,D]],
								t22 = Pair[Momentum[qi,D],Momentum[qj,D]],
								t23 = Pair[Momentum[qj,D],Momentum[qj,D]]
								},
									FCPrint[1,"use G1G1G2G2 rule "];
									(-(t1*t12*t14*t16^2*t17^2*t18)+t1*t12*t14*t15*t16*t17^2*t19+
									t1*t12*t14*t15*t16^2*t17*t20-t1*t12*t14*t15^2*t16*t17*t22)/
									((2-D)*t15^4)+(t1*t10*(-3*t16^2*t17^2*t18^2+D*t16^2*t17^2*t18^2+
									6*t15*t16*t17^2*t18*t19-2*D*t15*t16*t17^2*t18*t19-
									2*t15^2*t17^2*t19^2+D*t15^2*t17^2*t19^2+
									6*t15*t16^2*t17*t18*t20-2*D*t15*t16^2*t17*t18*t20-
									8*t15^2*t16*t17*t19*t20+2*D*t15^2*t16*t17*t19*t20-
									2*t15^2*t16^2*t20^2+D*t15^2*t16^2*t20^2-t15^2*t17^2*t18*t21+
									2*t15^3*t17*t20*t21-4*t15^2*t16*t17*t18*t22+
									2*D*t15^2*t16*t17*t18*t22+4*t15^3*t17*t19*t22-
									2*D*t15^3*t17*t19*t22+4*t15^3*t16*t20*t22-
									2*D*t15^3*t16*t20*t22-2*t15^4*t22^2+D*t15^4*t22^2-
									t15^2*t16^2*t18*t23+2*t15^3*t16*t19*t23-t15^4*t21*t23))/
									((-3+D)*(-2+D)*D*t15^4)+
									(t1*t11*t14*(3*t16^2*t17^2*t18^2+2*D*t16^2*t17^2*t18^2-
									D^2*t16^2*t17^2*t18^2-6*t15*t16*t17^2*t18*t19-
									4*D*t15*t16*t17^2*t18*t19+2*D^2*t15*t16*t17^2*t18*t19+
									2*t15^2*t17^2*t19^2+2*D*t15^2*t17^2*t19^2-
									D^2*t15^2*t17^2*t19^2-6*t15*t16^2*t17*t18*t20-
									D*t15*t16^2*t17*t18*t20+D^2*t15*t16^2*t17*t18*t20+
									8*t15^2*t16*t17*t19*t20+D*t15^2*t16*t17*t19*t20-
									D^2*t15^2*t16*t17*t19*t20+2*t15^2*t16^2*t20^2-
									D*t15^2*t16^2*t20^2+t15^2*t17^2*t18*t21-2*t15^3*t17*t20*t21+
									4*t15^2*t16*t17*t18*t22+D*t15^2*t16*t17*t18*t22-
									D^2*t15^2*t16*t17*t18*t22-4*t15^3*t17*t19*t22-
									D*t15^3*t17*t19*t22+D^2*t15^3*t17*t19*t22-4*t15^3*t16*t20*t22+
									2*D*t15^3*t16*t20*t22+2*t15^4*t22^2-D*t15^4*t22^2+
									t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23+t15^4*t21*t23))/
									((-3+D)*(-2+D)*D*t15^5)+
									(t1*t12*t13*(3*t16^2*t17^2*t18^2+2*D*t16^2*t17^2*t18^2-
									D^2*t16^2*t17^2*t18^2-6*t15*t16*t17^2*t18*t19-
									D*t15*t16*t17^2*t18*t19+D^2*t15*t16*t17^2*t18*t19+
									2*t15^2*t17^2*t19^2-D*t15^2*t17^2*t19^2-
									6*t15*t16^2*t17*t18*t20-4*D*t15*t16^2*t17*t18*t20+
									2*D^2*t15*t16^2*t17*t18*t20+8*t15^2*t16*t17*t19*t20+
									D*t15^2*t16*t17*t19*t20-D^2*t15^2*t16*t17*t19*t20+
									2*t15^2*t16^2*t20^2+2*D*t15^2*t16^2*t20^2-
									D^2*t15^2*t16^2*t20^2+t15^2*t17^2*t18*t21-2*t15^3*t17*t20*t21+
									4*t15^2*t16*t17*t18*t22+D*t15^2*t16*t17*t18*t22-
									D^2*t15^2*t16*t17*t18*t22-4*t15^3*t17*t19*t22+
									2*D*t15^3*t17*t19*t22-4*t15^3*t16*t20*t22-D*t15^3*t16*t20*t22+
									D^2*t15^3*t16*t20*t22+2*t15^4*t22^2-D*t15^4*t22^2+
									t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23+t15^4*t21*t23))/
									((-3+D)*(-2+D)*D*t15^5)+
									(t1*t11*t13*(-3*t16^2*t17^2*t18^3-2*D*t16^2*t17^2*t18^3+
									D^2*t16^2*t17^2*t18^3+6*t15*t16*t17^2*t18^2*t19+
									4*D*t15*t16*t17^2*t18^2*t19-2*D^2*t15*t16*t17^2*t18^2*t19-
									2*t15^2*t17^2*t18*t19^2-2*D*t15^2*t17^2*t18*t19^2+
									D^2*t15^2*t17^2*t18*t19^2+6*t15*t16^2*t17*t18^2*t20+
									4*D*t15*t16^2*t17*t18^2*t20-2*D^2*t15*t16^2*t17*t18^2*t20-
									8*t15^2*t16*t17*t18*t19*t20-7*D*t15^2*t16*t17*t18*t19*t20+
									3*D^2*t15^2*t16*t17*t18*t19*t20+3*D*t15^3*t17*t19^2*t20-
									D^2*t15^3*t17*t19^2*t20-2*t15^2*t16^2*t18*t20^2-
									2*D*t15^2*t16^2*t18*t20^2+D^2*t15^2*t16^2*t18*t20^2+
									3*D*t15^3*t16*t19*t20^2-D^2*t15^3*t16*t19*t20^2-
									t15^2*t17^2*t18^2*t21+2*t15^3*t17*t18*t20*t21-
									4*t15^2*t16*t17*t18^2*t22-D*t15^2*t16*t17*t18^2*t22+
									D^2*t15^2*t16*t17*t18^2*t22+4*t15^3*t17*t18*t19*t22+
									D*t15^3*t17*t18*t19*t22-D^2*t15^3*t17*t18*t19*t22+
									4*t15^3*t16*t18*t20*t22+D*t15^3*t16*t18*t20*t22-
									D^2*t15^3*t16*t18*t20*t22-3*D*t15^4*t19*t20*t22+
									D^2*t15^4*t19*t20*t22-2*t15^4*t18*t22^2+D*t15^4*t18*t22^2-
									t15^2*t16^2*t18^2*t23+2*t15^3*t16*t18*t19*t23-t15^4*t18*t21*t23))/
									((-3+D)*(-2+D)*D*t15^6)+
									((-3*t16^2*t17^2*t18^2+D*t16^2*t17^2*t18^2+6*t15*t16*t17^2*t18*t19-
									2*D*t15*t16*t17^2*t18*t19-2*t15^2*t17^2*t19^2+
									D*t15^2*t17^2*t19^2+6*t15*t16^2*t17*t18*t20-
									2*D*t15*t16^2*t17*t18*t20-8*t15^2*t16*t17*t19*t20+
									2*D*t15^2*t16*t17*t19*t20-2*t15^2*t16^2*t20^2+
									D*t15^2*t16^2*t20^2-t15^2*t17^2*t18*t21+2*t15^3*t17*t20*t21-
									4*t15^2*t16*t17*t18*t22+2*D*t15^2*t16*t17*t18*t22+
									4*t15^3*t17*t19*t22-2*D*t15^3*t17*t19*t22+4*t15^3*t16*t20*t22-
									2*D*t15^3*t16*t20*t22-2*t15^4*t22^2+D*t15^4*t22^2-
									t15^2*t16^2*t18*t23+2*t15^3*t16*t19*t23-t15^4*t21*t23)*t3*t6)/
									((-3+D)*(-2+D)*D*t15^4)+
									(t14*(3*t16^2*t17^2*t18^2+2*D*t16^2*t17^2*t18^2-
									D^2*t16^2*t17^2*t18^2-6*t15*t16*t17^2*t18*t19-
									4*D*t15*t16*t17^2*t18*t19+2*D^2*t15*t16*t17^2*t18*t19+
									2*t15^2*t17^2*t19^2+2*D*t15^2*t17^2*t19^2-
									D^2*t15^2*t17^2*t19^2-6*t15*t16^2*t17*t18*t20-
									D*t15*t16^2*t17*t18*t20+D^2*t15*t16^2*t17*t18*t20+
									8*t15^2*t16*t17*t19*t20+D*t15^2*t16*t17*t19*t20-
									D^2*t15^2*t16*t17*t19*t20+2*t15^2*t16^2*t20^2-
									D*t15^2*t16^2*t20^2+t15^2*t17^2*t18*t21-2*t15^3*t17*t20*t21+
									4*t15^2*t16*t17*t18*t22+D*t15^2*t16*t17*t18*t22-
									D^2*t15^2*t16*t17*t18*t22-4*t15^3*t17*t19*t22-
									D*t15^3*t17*t19*t22+D^2*t15^3*t17*t19*t22-4*t15^3*t16*t20*t22+
									2*D*t15^3*t16*t20*t22+2*t15^4*t22^2-D*t15^4*t22^2+
									t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23+t15^4*t21*t23)*t4*t6)/
									((-3+D)*(-2+D)*D*t15^5)+
									(t13*(-3*t16^2*t17^2*t18^3-2*D*t16^2*t17^2*t18^3+
									D^2*t16^2*t17^2*t18^3+6*t15*t16*t17^2*t18^2*t19+
									4*D*t15*t16*t17^2*t18^2*t19-2*D^2*t15*t16*t17^2*t18^2*t19-
									2*t15^2*t17^2*t18*t19^2-2*D*t15^2*t17^2*t18*t19^2+
									D^2*t15^2*t17^2*t18*t19^2+6*t15*t16^2*t17*t18^2*t20+
									4*D*t15*t16^2*t17*t18^2*t20-2*D^2*t15*t16^2*t17*t18^2*t20-
									8*t15^2*t16*t17*t18*t19*t20-7*D*t15^2*t16*t17*t18*t19*t20+
									3*D^2*t15^2*t16*t17*t18*t19*t20+3*D*t15^3*t17*t19^2*t20-
									D^2*t15^3*t17*t19^2*t20-2*t15^2*t16^2*t18*t20^2-
									2*D*t15^2*t16^2*t18*t20^2+D^2*t15^2*t16^2*t18*t20^2+
									3*D*t15^3*t16*t19*t20^2-D^2*t15^3*t16*t19*t20^2-
									t15^2*t17^2*t18^2*t21+2*t15^3*t17*t18*t20*t21-
									4*t15^2*t16*t17*t18^2*t22-D*t15^2*t16*t17*t18^2*t22+
									D^2*t15^2*t16*t17*t18^2*t22+4*t15^3*t17*t18*t19*t22+
									D*t15^3*t17*t18*t19*t22-D^2*t15^3*t17*t18*t19*t22+
									4*t15^3*t16*t18*t20*t22+D*t15^3*t16*t18*t20*t22-
									D^2*t15^3*t16*t18*t20*t22-3*D*t15^4*t19*t20*t22+
									D^2*t15^4*t19*t20*t22-2*t15^4*t18*t22^2+D*t15^4*t18*t22^2-
									t15^2*t16^2*t18^2*t23+2*t15^3*t16*t18*t19*t23-t15^4*t18*t21*t23)*
									t4*t6)/((-3+D)*(-2+D)*D*t15^6)+
									(t13*(3*t16^2*t17^2*t18^2+2*D*t16^2*t17^2*t18^2-
									D^2*t16^2*t17^2*t18^2-6*t15*t16*t17^2*t18*t19-
									D*t15*t16*t17^2*t18*t19+D^2*t15*t16*t17^2*t18*t19+
									2*t15^2*t17^2*t19^2-D*t15^2*t17^2*t19^2-
									6*t15*t16^2*t17*t18*t20-4*D*t15*t16^2*t17*t18*t20+
									2*D^2*t15*t16^2*t17*t18*t20+8*t15^2*t16*t17*t19*t20+
									D*t15^2*t16*t17*t19*t20-D^2*t15^2*t16*t17*t19*t20+
									2*t15^2*t16^2*t20^2+2*D*t15^2*t16^2*t20^2-
									D^2*t15^2*t16^2*t20^2+t15^2*t17^2*t18*t21-2*t15^3*t17*t20*t21+
									4*t15^2*t16*t17*t18*t22+D*t15^2*t16*t17*t18*t22-
									D^2*t15^2*t16*t17*t18*t22-4*t15^3*t17*t19*t22+
									2*D*t15^3*t17*t19*t22-4*t15^3*t16*t20*t22-D*t15^3*t16*t20*t22+
									D^2*t15^3*t16*t20*t22+2*t15^4*t22^2-D*t15^4*t22^2+
									t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23+t15^4*t21*t23)*t5*t6)/
									((-3+D)*(-2+D)*D*t15^5)+
									(-(t14*t16^2*t17^2*t18*t5*t6)+t14*t15*t16*t17^2*t19*t5*t6+
									t14*t15*t16^2*t17*t20*t5*t6-t14*t15^2*t16*t17*t22*t5*t6)/
									((2-D)*t15^4)+(t2*(-3*t16^2*t17^2*t18^2+D*t16^2*t17^2*t18^2+
									6*t15*t16*t17^2*t18*t19-2*D*t15*t16*t17^2*t18*t19-
									2*t15^2*t17^2*t19^2+6*t15*t16^2*t17*t18*t20-
									2*D*t15*t16^2*t17*t18*t20-8*t15^2*t16*t17*t19*t20+
									4*D*t15^2*t16*t17*t19*t20-2*t15^2*t16^2*t20^2-
									t15^2*t17^2*t18*t21+D*t15^2*t17^2*t18*t21+2*t15^3*t17*t20*t21-
									2*D*t15^3*t17*t20*t21-4*t15^2*t16*t17*t18*t22+
									4*t15^3*t17*t19*t22+4*t15^3*t16*t20*t22-2*t15^4*t22^2-
									t15^2*t16^2*t18*t23+D*t15^2*t16^2*t18*t23+2*t15^3*t16*t19*t23-
									2*D*t15^3*t16*t19*t23-t15^4*t21*t23+D*t15^4*t21*t23)*t7)/
									((-3+D)*(-2+D)*D*t15^4)+
									(t12*(3*t16^2*t17^2*t18^2+2*D*t16^2*t17^2*t18^2-
									D^2*t16^2*t17^2*t18^2-6*t15*t16*t17^2*t18*t19-
									D*t15*t16*t17^2*t18*t19+D^2*t15*t16*t17^2*t18*t19+
									2*t15^2*t17^2*t19^2-6*t15*t16^2*t17*t18*t20-
									4*D*t15*t16^2*t17*t18*t20+2*D^2*t15*t16^2*t17*t18*t20+
									8*t15^2*t16*t17*t19*t20+2*D*t15^2*t16*t17*t19*t20-
									2*D^2*t15^2*t16*t17*t19*t20+2*t15^2*t16^2*t20^2+
									t15^2*t17^2*t18*t21-D*t15^2*t17^2*t18*t21-2*t15^3*t17*t20*t21+
									2*D*t15^3*t17*t20*t21+4*t15^2*t16*t17*t18*t22-
									4*t15^3*t17*t19*t22-4*t15^3*t16*t20*t22+2*t15^4*t22^2+
									t15^2*t16^2*t18*t23+2*D*t15^2*t16^2*t18*t23-
									D^2*t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23-
									D*t15^3*t16*t19*t23+D^2*t15^3*t16*t19*t23+t15^4*t21*t23-
									D*t15^4*t21*t23)*t4*t7)/((-3+D)*(-2+D)*D*t15^5)+
									(t11*(-3*t16^2*t17^2*t18^3-2*D*t16^2*t17^2*t18^3+
									D^2*t16^2*t17^2*t18^3+6*t15*t16*t17^2*t18^2*t19+
									4*D*t15*t16*t17^2*t18^2*t19-2*D^2*t15*t16*t17^2*t18^2*t19-
									2*t15^2*t17^2*t18*t19^2-3*D*t15^2*t17^2*t18*t19^2+
									D^2*t15^2*t17^2*t18*t19^2+6*t15*t16^2*t17*t18^2*t20+
									4*D*t15*t16^2*t17*t18^2*t20-2*D^2*t15*t16^2*t17*t18^2*t20-
									8*t15^2*t16*t17*t18*t19*t20-8*D*t15^2*t16*t17*t18*t19*t20+
									4*D^2*t15^2*t16*t17*t18*t19*t20+6*D*t15^3*t17*t19^2*t20-
									2*D^2*t15^3*t17*t19^2*t20-2*t15^2*t16^2*t18*t20^2-
									t15^2*t17^2*t18^2*t21+D*t15^2*t17^2*t18^2*t21+
									2*t15^3*t17*t18*t20*t21-2*D*t15^3*t17*t18*t20*t21-
									4*t15^2*t16*t17*t18^2*t22+4*t15^3*t17*t18*t19*t22+
									4*t15^3*t16*t18*t20*t22-2*t15^4*t18*t22^2-
									t15^2*t16^2*t18^2*t23-2*D*t15^2*t16^2*t18^2*t23+
									D^2*t15^2*t16^2*t18^2*t23+2*t15^3*t16*t18*t19*t23+
									4*D*t15^3*t16*t18*t19*t23-2*D^2*t15^3*t16*t18*t19*t23-
									3*D*t15^4*t19^2*t23+D^2*t15^4*t19^2*t23-t15^4*t18*t21*t23+
									D*t15^4*t18*t21*t23)*t4*t7)/((-3+D)*(-2+D)*D*t15^6)+
									(t11*(3*t16^2*t17^2*t18^2+2*D*t16^2*t17^2*t18^2-
									D^2*t16^2*t17^2*t18^2-6*t15*t16*t17^2*t18*t19-
									D*t15*t16*t17^2*t18*t19+D^2*t15*t16*t17^2*t18*t19+
									2*t15^2*t17^2*t19^2-6*t15*t16^2*t17*t18*t20-
									4*D*t15*t16^2*t17*t18*t20+2*D^2*t15*t16^2*t17*t18*t20+
									8*t15^2*t16*t17*t19*t20+2*D*t15^2*t16*t17*t19*t20-
									2*D^2*t15^2*t16*t17*t19*t20+2*t15^2*t16^2*t20^2+
									t15^2*t17^2*t18*t21-D*t15^2*t17^2*t18*t21-2*t15^3*t17*t20*t21+
									2*D*t15^3*t17*t20*t21+4*t15^2*t16*t17*t18*t22-
									4*t15^3*t17*t19*t22-4*t15^3*t16*t20*t22+2*t15^4*t22^2+
									t15^2*t16^2*t18*t23+2*D*t15^2*t16^2*t18*t23-
									D^2*t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23-
									D*t15^3*t16*t19*t23+D^2*t15^3*t16*t19*t23+t15^4*t21*t23-
									D*t15^4*t21*t23)*t5*t7)/((-3+D)*(-2+D)*D*t15^5)+
									(-(t12*t16^2*t17^2*t18*t5*t7)+2*t12*t15*t16^2*t17*t20*t5*t7-
									t12*t15^2*t16^2*t23*t5*t7)/((2-D)*t15^4)+
									(t14*t2*(3*t16^2*t17^2*t18^2+2*D*t16^2*t17^2*t18^2-
									D^2*t16^2*t17^2*t18^2-6*t15*t16*t17^2*t18*t19-
									4*D*t15*t16*t17^2*t18*t19+2*D^2*t15*t16*t17^2*t18*t19+
									2*t15^2*t17^2*t19^2-6*t15*t16^2*t17*t18*t20-
									D*t15*t16^2*t17*t18*t20+D^2*t15*t16^2*t17*t18*t20+
									8*t15^2*t16*t17*t19*t20+2*D*t15^2*t16*t17*t19*t20-
									2*D^2*t15^2*t16*t17*t19*t20+2*t15^2*t16^2*t20^2+
									t15^2*t17^2*t18*t21+2*D*t15^2*t17^2*t18*t21-
									D^2*t15^2*t17^2*t18*t21-2*t15^3*t17*t20*t21-
									D*t15^3*t17*t20*t21+D^2*t15^3*t17*t20*t21+
									4*t15^2*t16*t17*t18*t22-4*t15^3*t17*t19*t22-
									4*t15^3*t16*t20*t22+2*t15^4*t22^2+t15^2*t16^2*t18*t23-
									D*t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23+
									2*D*t15^3*t16*t19*t23+t15^4*t21*t23-D*t15^4*t21*t23)*t8)/
									((-3+D)*(-2+D)*D*t15^5)+
									(t13*t2*(-3*t16^2*t17^2*t18^3-2*D*t16^2*t17^2*t18^3+
									D^2*t16^2*t17^2*t18^3+6*t15*t16*t17^2*t18^2*t19+
									4*D*t15*t16*t17^2*t18^2*t19-2*D^2*t15*t16*t17^2*t18^2*t19-
									2*t15^2*t17^2*t18*t19^2+6*t15*t16^2*t17*t18^2*t20+
									4*D*t15*t16^2*t17*t18^2*t20-2*D^2*t15*t16^2*t17*t18^2*t20-
									8*t15^2*t16*t17*t18*t19*t20-8*D*t15^2*t16*t17*t18*t19*t20+
									4*D^2*t15^2*t16*t17*t18*t19*t20-2*t15^2*t16^2*t18*t20^2-
									3*D*t15^2*t16^2*t18*t20^2+D^2*t15^2*t16^2*t18*t20^2+
									6*D*t15^3*t16*t19*t20^2-2*D^2*t15^3*t16*t19*t20^2-
									t15^2*t17^2*t18^2*t21-2*D*t15^2*t17^2*t18^2*t21+
									D^2*t15^2*t17^2*t18^2*t21+2*t15^3*t17*t18*t20*t21+
									4*D*t15^3*t17*t18*t20*t21-2*D^2*t15^3*t17*t18*t20*t21-
									3*D*t15^4*t20^2*t21+D^2*t15^4*t20^2*t21-
									4*t15^2*t16*t17*t18^2*t22+4*t15^3*t17*t18*t19*t22+
									4*t15^3*t16*t18*t20*t22-2*t15^4*t18*t22^2-
									t15^2*t16^2*t18^2*t23+D*t15^2*t16^2*t18^2*t23+
									2*t15^3*t16*t18*t19*t23-2*D*t15^3*t16*t18*t19*t23-
									t15^4*t18*t21*t23+D*t15^4*t18*t21*t23)*t8)/
									((-3+D)*(-2+D)*D*t15^6)+
									(t12*(3*t16^2*t17^2*t18^2+2*D*t16^2*t17^2*t18^2-
									D^2*t16^2*t17^2*t18^2-6*t15*t16*t17^2*t18*t19-
									D*t15*t16*t17^2*t18*t19+D^2*t15*t16*t17^2*t18*t19+
									2*t15^2*t17^2*t19^2-D*t15^2*t17^2*t19^2-
									6*t15*t16^2*t17*t18*t20-4*D*t15*t16^2*t17*t18*t20+
									2*D^2*t15*t16^2*t17*t18*t20+8*t15^2*t16*t17*t19*t20+
									D*t15^2*t16*t17*t19*t20-D^2*t15^2*t16*t17*t19*t20+
									2*t15^2*t16^2*t20^2+2*D*t15^2*t16^2*t20^2-
									D^2*t15^2*t16^2*t20^2+t15^2*t17^2*t18*t21-2*t15^3*t17*t20*t21+
									4*t15^2*t16*t17*t18*t22+D*t15^2*t16*t17*t18*t22-
									D^2*t15^2*t16*t17*t18*t22-4*t15^3*t17*t19*t22+
									2*D*t15^3*t17*t19*t22-4*t15^3*t16*t20*t22-D*t15^3*t16*t20*t22+
									D^2*t15^3*t16*t20*t22+2*t15^4*t22^2-D*t15^4*t22^2+
									t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23+t15^4*t21*t23)*t3*t8)/
									((-3+D)*(-2+D)*D*t15^5)+
									(t11*(-3*t16^2*t17^2*t18^3-2*D*t16^2*t17^2*t18^3+
									D^2*t16^2*t17^2*t18^3+6*t15*t16*t17^2*t18^2*t19+
									4*D*t15*t16*t17^2*t18^2*t19-2*D^2*t15*t16*t17^2*t18^2*t19-
									2*t15^2*t17^2*t18*t19^2-2*D*t15^2*t17^2*t18*t19^2+
									D^2*t15^2*t17^2*t18*t19^2+6*t15*t16^2*t17*t18^2*t20+
									4*D*t15*t16^2*t17*t18^2*t20-2*D^2*t15*t16^2*t17*t18^2*t20-
									8*t15^2*t16*t17*t18*t19*t20-7*D*t15^2*t16*t17*t18*t19*t20+
									3*D^2*t15^2*t16*t17*t18*t19*t20+3*D*t15^3*t17*t19^2*t20-
									D^2*t15^3*t17*t19^2*t20-2*t15^2*t16^2*t18*t20^2-
									2*D*t15^2*t16^2*t18*t20^2+D^2*t15^2*t16^2*t18*t20^2+
									3*D*t15^3*t16*t19*t20^2-D^2*t15^3*t16*t19*t20^2-
									t15^2*t17^2*t18^2*t21+2*t15^3*t17*t18*t20*t21-
									4*t15^2*t16*t17*t18^2*t22-D*t15^2*t16*t17*t18^2*t22+
									D^2*t15^2*t16*t17*t18^2*t22+4*t15^3*t17*t18*t19*t22+
									D*t15^3*t17*t18*t19*t22-D^2*t15^3*t17*t18*t19*t22+
									4*t15^3*t16*t18*t20*t22+D*t15^3*t16*t18*t20*t22-
									D^2*t15^3*t16*t18*t20*t22-3*D*t15^4*t19*t20*t22+
									D^2*t15^4*t19*t20*t22-2*t15^4*t18*t22^2+D*t15^4*t18*t22^2-
									t15^2*t16^2*t18^2*t23+2*t15^3*t16*t18*t19*t23-t15^4*t18*t21*t23)*
									t3*t8)/((-3+D)*(-2+D)*D*t15^6)+
									(t12*t14*(-6*t16^2*t17^2*t18^2-7*D*t16^2*t17^2*t18^2+
									D^3*t16^2*t17^2*t18^2+12*t15*t16*t17^2*t18*t19+
									11*D*t15*t16*t17^2*t18*t19-2*D^2*t15*t16*t17^2*t18*t19-
									D^3*t15*t16*t17^2*t18*t19-4*t15^2*t17^2*t19^2-
									2*D*t15^2*t17^2*t19^2+D^2*t15^2*t17^2*t19^2+
									12*t15*t16^2*t17*t18*t20+11*D*t15*t16^2*t17*t18*t20-
									2*D^2*t15*t16^2*t17*t18*t20-D^3*t15*t16^2*t17*t18*t20-
									16*t15^2*t16*t17*t19*t20-6*D*t15^2*t16*t17*t19*t20+
									D^2*t15^2*t16*t17*t19*t20+D^3*t15^2*t16*t17*t19*t20-
									4*t15^2*t16^2*t20^2-2*D*t15^2*t16^2*t20^2+
									D^2*t15^2*t16^2*t20^2-2*t15^2*t17^2*t18*t21-
									2*D*t15^2*t17^2*t18*t21+D^2*t15^2*t17^2*t18*t21+
									4*t15^3*t17*t20*t21+D*t15^3*t17*t20*t21-D^2*t15^3*t17*t20*t21-
									8*t15^2*t16*t17*t18*t22-7*D*t15^2*t16*t17*t18*t22+
									3*D^2*t15^2*t16*t17*t18*t22+8*t15^3*t17*t19*t22+
									D*t15^3*t17*t19*t22-D^2*t15^3*t17*t19*t22+8*t15^3*t16*t20*t22+
									D*t15^3*t16*t20*t22-D^2*t15^3*t16*t20*t22-4*t15^4*t22^2+
									D*t15^4*t22^2-2*t15^2*t16^2*t18*t23-2*D*t15^2*t16^2*t18*t23+
									D^2*t15^2*t16^2*t18*t23+4*t15^3*t16*t19*t23+
									D*t15^3*t16*t19*t23-D^2*t15^3*t16*t19*t23-2*t15^4*t21*t23+
									D*t15^4*t21*t23)*t4*t8)/((-3+D)*(-2+D)*D*t15^6)+
									(t10*(-3*t16^2*t17^2*t18^3-2*D*t16^2*t17^2*t18^3+
									D^2*t16^2*t17^2*t18^3+6*t15*t16*t17^2*t18^2*t19+
									4*D*t15*t16*t17^2*t18^2*t19-2*D^2*t15*t16*t17^2*t18^2*t19-
									2*t15^2*t17^2*t18*t19^2-2*D*t15^2*t17^2*t18*t19^2+
									D^2*t15^2*t17^2*t18*t19^2+6*t15*t16^2*t17*t18^2*t20+
									4*D*t15*t16^2*t17*t18^2*t20-2*D^2*t15*t16^2*t17*t18^2*t20-
									8*t15^2*t16*t17*t18*t19*t20-7*D*t15^2*t16*t17*t18*t19*t20+
									3*D^2*t15^2*t16*t17*t18*t19*t20+3*D*t15^3*t17*t19^2*t20-
									D^2*t15^3*t17*t19^2*t20-2*t15^2*t16^2*t18*t20^2-
									2*D*t15^2*t16^2*t18*t20^2+D^2*t15^2*t16^2*t18*t20^2+
									3*D*t15^3*t16*t19*t20^2-D^2*t15^3*t16*t19*t20^2-
									t15^2*t17^2*t18^2*t21+2*t15^3*t17*t18*t20*t21-
									4*t15^2*t16*t17*t18^2*t22-D*t15^2*t16*t17*t18^2*t22+
									D^2*t15^2*t16*t17*t18^2*t22+4*t15^3*t17*t18*t19*t22+
									D*t15^3*t17*t18*t19*t22-D^2*t15^3*t17*t18*t19*t22+
									4*t15^3*t16*t18*t20*t22+D*t15^3*t16*t18*t20*t22-
									D^2*t15^3*t16*t18*t20*t22-3*D*t15^4*t19*t20*t22+
									D^2*t15^4*t19*t20*t22-2*t15^4*t18*t22^2+D*t15^4*t18*t22^2-
									t15^2*t16^2*t18^2*t23+2*t15^3*t16*t18*t19*t23-t15^4*t18*t21*t23)*
									t4*t8)/((-3+D)*(-2+D)*D*t15^6)+
									(t11*t13*(3*t16^2*t17^2*t18^4+4*D*t16^2*t17^2*t18^4+
									D^2*t16^2*t17^2*t18^4-6*t15*t16*t17^2*t18^3*t19-
									8*D*t15*t16*t17^2*t18^3*t19-2*D^2*t15*t16*t17^2*t18^3*t19+
									2*t15^2*t17^2*t18^2*t19^2+3*D*t15^2*t17^2*t18^2*t19^2+
									D^2*t15^2*t17^2*t18^2*t19^2-6*t15*t16^2*t17*t18^3*t20-
									8*D*t15*t16^2*t17*t18^3*t20-2*D^2*t15*t16^2*t17*t18^3*t20+
									8*t15^2*t16*t17*t18^2*t19*t20+12*D*t15^2*t16*t17*t18^2*t19*t20+
									4*D^2*t15^2*t16*t17*t18^2*t19*t20-2*D*t15^3*t17*t18*t19^2*t20-
									2*D^2*t15^3*t17*t18*t19^2*t20+2*t15^2*t16^2*t18^2*t20^2+
									3*D*t15^2*t16^2*t18^2*t20^2+D^2*t15^2*t16^2*t18^2*t20^2-
									2*D*t15^3*t16*t18*t19*t20^2-2*D^2*t15^3*t16*t18*t19*t20^2-
									2*D*t15^4*t19^2*t20^2+D^2*t15^4*t19^2*t20^2+
									t15^2*t17^2*t18^3*t21+D*t15^2*t17^2*t18^3*t21-
									2*t15^3*t17*t18^2*t20*t21-2*D*t15^3*t17*t18^2*t20*t21+
									D*t15^4*t18*t20^2*t21+4*t15^2*t16*t17*t18^3*t22+
									4*D*t15^2*t16*t17*t18^3*t22-4*t15^3*t17*t18^2*t19*t22-
									4*D*t15^3*t17*t18^2*t19*t22-4*t15^3*t16*t18^2*t20*t22-
									4*D*t15^3*t16*t18^2*t20*t22+4*D*t15^4*t18*t19*t20*t22+
									2*t15^4*t18^2*t22^2+t15^2*t16^2*t18^3*t23+
									D*t15^2*t16^2*t18^3*t23-2*t15^3*t16*t18^2*t19*t23-
									2*D*t15^3*t16*t18^2*t19*t23+D*t15^4*t18*t19^2*t23+
									t15^4*t18^2*t21*t23)*t4*t8)/((-2+D)*D*t15^8)+
									(t12*t13*(-6*t16^2*t17^2*t18^2-7*D*t16^2*t17^2*t18^2+
									D^3*t16^2*t17^2*t18^2+12*t15*t16*t17^2*t18*t19+
									8*D*t15*t16*t17^2*t18*t19-4*D^2*t15*t16*t17^2*t18*t19-
									4*t15^2*t17^2*t19^2+2*D*t15^2*t17^2*t19^2+
									12*t15*t16^2*t17*t18*t20+14*D*t15*t16^2*t17*t18*t20-
									2*D^3*t15*t16^2*t17*t18*t20-16*t15^2*t16*t17*t19*t20-
									8*D*t15^2*t16*t17*t19*t20+4*D^2*t15^2*t16*t17*t19*t20-
									4*t15^2*t16^2*t20^2-4*D*t15^2*t16^2*t20^2-
									D^2*t15^2*t16^2*t20^2+D^3*t15^2*t16^2*t20^2-
									2*t15^2*t17^2*t18*t21+4*t15^3*t17*t20*t21-
									8*t15^2*t16*t17*t18*t22-8*D*t15^2*t16*t17*t18*t22+
									4*D^2*t15^2*t16*t17*t18*t22+8*t15^3*t17*t19*t22-
									4*D*t15^3*t17*t19*t22+8*t15^3*t16*t20*t22+
									8*D*t15^3*t16*t20*t22-4*D^2*t15^3*t16*t20*t22-4*t15^4*t22^2+
									2*D*t15^4*t22^2-2*t15^2*t16^2*t18*t23-3*D*t15^2*t16^2*t18*t23+
									D^2*t15^2*t16^2*t18*t23+4*t15^3*t16*t19*t23-2*t15^4*t21*t23)*t5*
									t8)/((-3+D)*(-2+D)*D*t15^6)+
									(t10*(3*t16^2*t17^2*t18^2+2*D*t16^2*t17^2*t18^2-
									D^2*t16^2*t17^2*t18^2-6*t15*t16*t17^2*t18*t19-
									D*t15*t16*t17^2*t18*t19+D^2*t15*t16*t17^2*t18*t19+
									2*t15^2*t17^2*t19^2-D*t15^2*t17^2*t19^2-
									6*t15*t16^2*t17*t18*t20-4*D*t15*t16^2*t17*t18*t20+
									2*D^2*t15*t16^2*t17*t18*t20+8*t15^2*t16*t17*t19*t20+
									D*t15^2*t16*t17*t19*t20-D^2*t15^2*t16*t17*t19*t20+
									2*t15^2*t16^2*t20^2+2*D*t15^2*t16^2*t20^2-
									D^2*t15^2*t16^2*t20^2+t15^2*t17^2*t18*t21-2*t15^3*t17*t20*t21+
									4*t15^2*t16*t17*t18*t22+D*t15^2*t16*t17*t18*t22-
									D^2*t15^2*t16*t17*t18*t22-4*t15^3*t17*t19*t22+
									2*D*t15^3*t17*t19*t22-4*t15^3*t16*t20*t22-D*t15^3*t16*t20*t22+
									D^2*t15^3*t16*t20*t22+2*t15^4*t22^2-D*t15^4*t22^2+
									t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23+t15^4*t21*t23)*t5*t8)/
									((-3+D)*(-2+D)*D*t15^5)+
									(t11*t14*(-6*t16^2*t17^2*t18^2-7*D*t16^2*t17^2*t18^2+
									D^3*t16^2*t17^2*t18^2+12*t15*t16*t17^2*t18*t19+
									11*D*t15*t16*t17^2*t18*t19-2*D^2*t15*t16*t17^2*t18*t19-
									D^3*t15*t16*t17^2*t18*t19-4*t15^2*t17^2*t19^2-
									2*D*t15^2*t17^2*t19^2+D^2*t15^2*t17^2*t19^2+
									12*t15*t16^2*t17*t18*t20+11*D*t15*t16^2*t17*t18*t20-
									2*D^2*t15*t16^2*t17*t18*t20-D^3*t15*t16^2*t17*t18*t20-
									16*t15^2*t16*t17*t19*t20-6*D*t15^2*t16*t17*t19*t20+
									D^2*t15^2*t16*t17*t19*t20+D^3*t15^2*t16*t17*t19*t20-
									4*t15^2*t16^2*t20^2-2*D*t15^2*t16^2*t20^2+
									D^2*t15^2*t16^2*t20^2-2*t15^2*t17^2*t18*t21-
									2*D*t15^2*t17^2*t18*t21+D^2*t15^2*t17^2*t18*t21+
									4*t15^3*t17*t20*t21+D*t15^3*t17*t20*t21-D^2*t15^3*t17*t20*t21-
									8*t15^2*t16*t17*t18*t22-7*D*t15^2*t16*t17*t18*t22+
									3*D^2*t15^2*t16*t17*t18*t22+8*t15^3*t17*t19*t22+
									D*t15^3*t17*t19*t22-D^2*t15^3*t17*t19*t22+8*t15^3*t16*t20*t22+
									D*t15^3*t16*t20*t22-D^2*t15^3*t16*t20*t22-4*t15^4*t22^2+
									D*t15^4*t22^2-2*t15^2*t16^2*t18*t23-2*D*t15^2*t16^2*t18*t23+
									D^2*t15^2*t16^2*t18*t23+4*t15^3*t16*t19*t23+
									D*t15^3*t16*t19*t23-D^2*t15^3*t16*t19*t23-2*t15^4*t21*t23+
									D*t15^4*t21*t23)*t5*t8)/((-3+D)*(-2+D)*D*t15^6)+
									(3*t12*t13*t16^2*t17^2*t18^3*t4*t8+
									4*D*t12*t13*t16^2*t17^2*t18^3*t4*t8+
									D^2*t12*t13*t16^2*t17^2*t18^3*t4*t8-
									6*t12*t13*t15*t16*t17^2*t18^2*t19*t4*t8-
									7*D*t12*t13*t15*t16*t17^2*t18^2*t19*t4*t8-
									D^2*t12*t13*t15*t16*t17^2*t18^2*t19*t4*t8+
									2*t12*t13*t15^2*t17^2*t18*t19^2*t4*t8+
									2*D*t12*t13*t15^2*t17^2*t18*t19^2*t4*t8-
									6*t12*t13*t15*t16^2*t17*t18^2*t20*t4*t8-
									8*D*t12*t13*t15*t16^2*t17*t18^2*t20*t4*t8-
									2*D^2*t12*t13*t15*t16^2*t17*t18^2*t20*t4*t8+
									8*t12*t13*t15^2*t16*t17*t18*t19*t20*t4*t8+
									10*D*t12*t13*t15^2*t16*t17*t18*t19*t20*t4*t8+
									2*D^2*t12*t13*t15^2*t16*t17*t18*t19*t20*t4*t8-
									2*D*t12*t13*t15^3*t17*t19^2*t20*t4*t8+
									2*t12*t13*t15^2*t16^2*t18*t20^2*t4*t8+
									3*D*t12*t13*t15^2*t16^2*t18*t20^2*t4*t8+
									D^2*t12*t13*t15^2*t16^2*t18*t20^2*t4*t8-
									2*D*t12*t13*t15^3*t16*t19*t20^2*t4*t8-
									D^2*t12*t13*t15^3*t16*t19*t20^2*t4*t8+
									t12*t13*t15^2*t17^2*t18^2*t21*t4*t8+
									D*t12*t13*t15^2*t17^2*t18^2*t21*t4*t8-
									2*t12*t13*t15^3*t17*t18*t20*t21*t4*t8-
									2*D*t12*t13*t15^3*t17*t18*t20*t21*t4*t8+
									D*t12*t13*t15^4*t20^2*t21*t4*t8+
									4*t12*t13*t15^2*t16*t17*t18^2*t22*t4*t8+
									4*D*t12*t13*t15^2*t16*t17*t18^2*t22*t4*t8-
									4*t12*t13*t15^3*t17*t18*t19*t22*t4*t8-
									2*D*t12*t13*t15^3*t17*t18*t19*t22*t4*t8-
									4*t12*t13*t15^3*t16*t18*t20*t22*t4*t8-
									4*D*t12*t13*t15^3*t16*t18*t20*t22*t4*t8+
									2*D*t12*t13*t15^4*t19*t20*t22*t4*t8+
									2*t12*t13*t15^4*t18*t22^2*t4*t8+
									t12*t13*t15^2*t16^2*t18^2*t23*t4*t8+
									D*t12*t13*t15^2*t16^2*t18^2*t23*t4*t8-
									2*t12*t13*t15^3*t16*t18*t19*t23*t4*t8-
									D*t12*t13*t15^3*t16*t18*t19*t23*t4*t8+t12*t13*t15^4*t18*t21*t23*t4*t8
									)/((2-D)*D*t15^7)+(3*t11*t14*t16^2*t17^2*t18^3*t4*t8+
									4*D*t11*t14*t16^2*t17^2*t18^3*t4*t8+
									D^2*t11*t14*t16^2*t17^2*t18^3*t4*t8-
									6*t11*t14*t15*t16*t17^2*t18^2*t19*t4*t8-
									8*D*t11*t14*t15*t16*t17^2*t18^2*t19*t4*t8-
									2*D^2*t11*t14*t15*t16*t17^2*t18^2*t19*t4*t8+
									2*t11*t14*t15^2*t17^2*t18*t19^2*t4*t8+
									3*D*t11*t14*t15^2*t17^2*t18*t19^2*t4*t8+
									D^2*t11*t14*t15^2*t17^2*t18*t19^2*t4*t8-
									6*t11*t14*t15*t16^2*t17*t18^2*t20*t4*t8-
									7*D*t11*t14*t15*t16^2*t17*t18^2*t20*t4*t8-
									D^2*t11*t14*t15*t16^2*t17*t18^2*t20*t4*t8+
									8*t11*t14*t15^2*t16*t17*t18*t19*t20*t4*t8+
									10*D*t11*t14*t15^2*t16*t17*t18*t19*t20*t4*t8+
									2*D^2*t11*t14*t15^2*t16*t17*t18*t19*t20*t4*t8-
									2*D*t11*t14*t15^3*t17*t19^2*t20*t4*t8-
									D^2*t11*t14*t15^3*t17*t19^2*t20*t4*t8+
									2*t11*t14*t15^2*t16^2*t18*t20^2*t4*t8+
									2*D*t11*t14*t15^2*t16^2*t18*t20^2*t4*t8-
									2*D*t11*t14*t15^3*t16*t19*t20^2*t4*t8+
									t11*t14*t15^2*t17^2*t18^2*t21*t4*t8+
									D*t11*t14*t15^2*t17^2*t18^2*t21*t4*t8-
									2*t11*t14*t15^3*t17*t18*t20*t21*t4*t8-
									D*t11*t14*t15^3*t17*t18*t20*t21*t4*t8+
									4*t11*t14*t15^2*t16*t17*t18^2*t22*t4*t8+
									4*D*t11*t14*t15^2*t16*t17*t18^2*t22*t4*t8-
									4*t11*t14*t15^3*t17*t18*t19*t22*t4*t8-
									4*D*t11*t14*t15^3*t17*t18*t19*t22*t4*t8-
									4*t11*t14*t15^3*t16*t18*t20*t22*t4*t8-
									2*D*t11*t14*t15^3*t16*t18*t20*t22*t4*t8+
									2*D*t11*t14*t15^4*t19*t20*t22*t4*t8+
									2*t11*t14*t15^4*t18*t22^2*t4*t8+
									t11*t14*t15^2*t16^2*t18^2*t23*t4*t8+
									D*t11*t14*t15^2*t16^2*t18^2*t23*t4*t8-
									2*t11*t14*t15^3*t16*t18*t19*t23*t4*t8-
									2*D*t11*t14*t15^3*t16*t18*t19*t23*t4*t8+
									D*t11*t14*t15^4*t19^2*t23*t4*t8+t11*t14*t15^4*t18*t21*t23*t4*t8)/
									((2-D)*D*t15^7)+(t12*t14*t16^2*t17^2*t18*t5*t8+
									D*t12*t14*t16^2*t17^2*t18*t5*t8-2*t12*t14*t15*t16*t17^2*t19*t5*t8-
									2*t12*t14*t15*t16^2*t17*t20*t5*t8-
									D*t12*t14*t15*t16^2*t17*t20*t5*t8+
									2*t12*t14*t15^2*t16*t17*t22*t5*t8+t12*t14*t15^2*t16^2*t23*t5*t8)/
									((2-D)*t15^5)+(3*t11*t13*t16^2*t17^2*t18^3*t5*t8+
									4*D*t11*t13*t16^2*t17^2*t18^3*t5*t8+
									D^2*t11*t13*t16^2*t17^2*t18^3*t5*t8-
									6*t11*t13*t15*t16*t17^2*t18^2*t19*t5*t8-
									7*D*t11*t13*t15*t16*t17^2*t18^2*t19*t5*t8-
									D^2*t11*t13*t15*t16*t17^2*t18^2*t19*t5*t8+
									2*t11*t13*t15^2*t17^2*t18*t19^2*t5*t8+
									2*D*t11*t13*t15^2*t17^2*t18*t19^2*t5*t8-
									6*t11*t13*t15*t16^2*t17*t18^2*t20*t5*t8-
									8*D*t11*t13*t15*t16^2*t17*t18^2*t20*t5*t8-
									2*D^2*t11*t13*t15*t16^2*t17*t18^2*t20*t5*t8+
									8*t11*t13*t15^2*t16*t17*t18*t19*t20*t5*t8+
									10*D*t11*t13*t15^2*t16*t17*t18*t19*t20*t5*t8+
									2*D^2*t11*t13*t15^2*t16*t17*t18*t19*t20*t5*t8-
									2*D*t11*t13*t15^3*t17*t19^2*t20*t5*t8+
									2*t11*t13*t15^2*t16^2*t18*t20^2*t5*t8+
									3*D*t11*t13*t15^2*t16^2*t18*t20^2*t5*t8+
									D^2*t11*t13*t15^2*t16^2*t18*t20^2*t5*t8-
									2*D*t11*t13*t15^3*t16*t19*t20^2*t5*t8-
									D^2*t11*t13*t15^3*t16*t19*t20^2*t5*t8+
									t11*t13*t15^2*t17^2*t18^2*t21*t5*t8+
									D*t11*t13*t15^2*t17^2*t18^2*t21*t5*t8-
									2*t11*t13*t15^3*t17*t18*t20*t21*t5*t8-
									2*D*t11*t13*t15^3*t17*t18*t20*t21*t5*t8+
									D*t11*t13*t15^4*t20^2*t21*t5*t8+
									4*t11*t13*t15^2*t16*t17*t18^2*t22*t5*t8+
									4*D*t11*t13*t15^2*t16*t17*t18^2*t22*t5*t8-
									4*t11*t13*t15^3*t17*t18*t19*t22*t5*t8-
									2*D*t11*t13*t15^3*t17*t18*t19*t22*t5*t8-
									4*t11*t13*t15^3*t16*t18*t20*t22*t5*t8-
									4*D*t11*t13*t15^3*t16*t18*t20*t22*t5*t8+
									2*D*t11*t13*t15^4*t19*t20*t22*t5*t8+
									2*t11*t13*t15^4*t18*t22^2*t5*t8+
									t11*t13*t15^2*t16^2*t18^2*t23*t5*t8+
									D*t11*t13*t15^2*t16^2*t18^2*t23*t5*t8-
									2*t11*t13*t15^3*t16*t18*t19*t23*t5*t8-
									D*t11*t13*t15^3*t16*t18*t19*t23*t5*t8+t11*t13*t15^4*t18*t21*t23*t5*t8
									)/((2-D)*D*t15^7)+(t13*t2*(3*t16^2*t17^2*t18^2+
									2*D*t16^2*t17^2*t18^2-D^2*t16^2*t17^2*t18^2-
									6*t15*t16*t17^2*t18*t19-4*D*t15*t16*t17^2*t18*t19+
									2*D^2*t15*t16*t17^2*t18*t19+2*t15^2*t17^2*t19^2-
									6*t15*t16^2*t17*t18*t20-D*t15*t16^2*t17*t18*t20+
									D^2*t15*t16^2*t17*t18*t20+8*t15^2*t16*t17*t19*t20+
									2*D*t15^2*t16*t17*t19*t20-2*D^2*t15^2*t16*t17*t19*t20+
									2*t15^2*t16^2*t20^2+t15^2*t17^2*t18*t21+
									2*D*t15^2*t17^2*t18*t21-D^2*t15^2*t17^2*t18*t21-
									2*t15^3*t17*t20*t21-D*t15^3*t17*t20*t21+D^2*t15^3*t17*t20*t21+
									4*t15^2*t16*t17*t18*t22-4*t15^3*t17*t19*t22-
									4*t15^3*t16*t20*t22+2*t15^4*t22^2+t15^2*t16^2*t18*t23-
									D*t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23+
									2*D*t15^3*t16*t19*t23+t15^4*t21*t23-D*t15^4*t21*t23)*t9)/
									((-3+D)*(-2+D)*D*t15^5)+
									(t11*(3*t16^2*t17^2*t18^2+2*D*t16^2*t17^2*t18^2-
									D^2*t16^2*t17^2*t18^2-6*t15*t16*t17^2*t18*t19-
									4*D*t15*t16*t17^2*t18*t19+2*D^2*t15*t16*t17^2*t18*t19+
									2*t15^2*t17^2*t19^2+2*D*t15^2*t17^2*t19^2-
									D^2*t15^2*t17^2*t19^2-6*t15*t16^2*t17*t18*t20-
									D*t15*t16^2*t17*t18*t20+D^2*t15*t16^2*t17*t18*t20+
									8*t15^2*t16*t17*t19*t20+D*t15^2*t16*t17*t19*t20-
									D^2*t15^2*t16*t17*t19*t20+2*t15^2*t16^2*t20^2-
									D*t15^2*t16^2*t20^2+t15^2*t17^2*t18*t21-2*t15^3*t17*t20*t21+
									4*t15^2*t16*t17*t18*t22+D*t15^2*t16*t17*t18*t22-
									D^2*t15^2*t16*t17*t18*t22-4*t15^3*t17*t19*t22-
									D*t15^3*t17*t19*t22+D^2*t15^3*t17*t19*t22-4*t15^3*t16*t20*t22+
									2*D*t15^3*t16*t20*t22+2*t15^4*t22^2-D*t15^4*t22^2+
									t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23+t15^4*t21*t23)*t3*t9)/
									((-3+D)*(-2+D)*D*t15^5)+
									(t11*t14*(-6*t16^2*t17^2*t18^2-7*D*t16^2*t17^2*t18^2+
									D^3*t16^2*t17^2*t18^2+12*t15*t16*t17^2*t18*t19+
									14*D*t15*t16*t17^2*t18*t19-2*D^3*t15*t16*t17^2*t18*t19-
									4*t15^2*t17^2*t19^2-4*D*t15^2*t17^2*t19^2-
									D^2*t15^2*t17^2*t19^2+D^3*t15^2*t17^2*t19^2+
									12*t15*t16^2*t17*t18*t20+8*D*t15*t16^2*t17*t18*t20-
									4*D^2*t15*t16^2*t17*t18*t20-16*t15^2*t16*t17*t19*t20-
									8*D*t15^2*t16*t17*t19*t20+4*D^2*t15^2*t16*t17*t19*t20-
									4*t15^2*t16^2*t20^2+2*D*t15^2*t16^2*t20^2-
									2*t15^2*t17^2*t18*t21-3*D*t15^2*t17^2*t18*t21+
									D^2*t15^2*t17^2*t18*t21+4*t15^3*t17*t20*t21-
									8*t15^2*t16*t17*t18*t22-8*D*t15^2*t16*t17*t18*t22+
									4*D^2*t15^2*t16*t17*t18*t22+8*t15^3*t17*t19*t22+
									8*D*t15^3*t17*t19*t22-4*D^2*t15^3*t17*t19*t22+
									8*t15^3*t16*t20*t22-4*D*t15^3*t16*t20*t22-4*t15^4*t22^2+
									2*D*t15^4*t22^2-2*t15^2*t16^2*t18*t23+4*t15^3*t16*t19*t23-
									2*t15^4*t21*t23)*t4*t9)/((-3+D)*(-2+D)*D*t15^6)+
									(t10*(3*t16^2*t17^2*t18^2+2*D*t16^2*t17^2*t18^2-
									D^2*t16^2*t17^2*t18^2-6*t15*t16*t17^2*t18*t19-
									4*D*t15*t16*t17^2*t18*t19+2*D^2*t15*t16*t17^2*t18*t19+
									2*t15^2*t17^2*t19^2+2*D*t15^2*t17^2*t19^2-
									D^2*t15^2*t17^2*t19^2-6*t15*t16^2*t17*t18*t20-
									D*t15*t16^2*t17*t18*t20+D^2*t15*t16^2*t17*t18*t20+
									8*t15^2*t16*t17*t19*t20+D*t15^2*t16*t17*t19*t20-
									D^2*t15^2*t16*t17*t19*t20+2*t15^2*t16^2*t20^2-
									D*t15^2*t16^2*t20^2+t15^2*t17^2*t18*t21-2*t15^3*t17*t20*t21+
									4*t15^2*t16*t17*t18*t22+D*t15^2*t16*t17*t18*t22-
									D^2*t15^2*t16*t17*t18*t22-4*t15^3*t17*t19*t22-
									D*t15^3*t17*t19*t22+D^2*t15^3*t17*t19*t22-4*t15^3*t16*t20*t22+
									2*D*t15^3*t16*t20*t22+2*t15^4*t22^2-D*t15^4*t22^2+
									t15^2*t16^2*t18*t23-2*t15^3*t16*t19*t23+t15^4*t21*t23)*t4*t9)/
									((-3+D)*(-2+D)*D*t15^5)+
									(t12*t13*(-6*t16^2*t17^2*t18^2-7*D*t16^2*t17^2*t18^2+
									D^3*t16^2*t17^2*t18^2+12*t15*t16*t17^2*t18*t19+
									11*D*t15*t16*t17^2*t18*t19-2*D^2*t15*t16*t17^2*t18*t19-
									D^3*t15*t16*t17^2*t18*t19-4*t15^2*t17^2*t19^2-
									2*D*t15^2*t17^2*t19^2+D^2*t15^2*t17^2*t19^2+
									12*t15*t16^2*t17*t18*t20+11*D*t15*t16^2*t17*t18*t20-
									2*D^2*t15*t16^2*t17*t18*t20-D^3*t15*t16^2*t17*t18*t20-
									16*t15^2*t16*t17*t19*t20-6*D*t15^2*t16*t17*t19*t20+
									D^2*t15^2*t16*t17*t19*t20+D^3*t15^2*t16*t17*t19*t20-
									4*t15^2*t16^2*t20^2-2*D*t15^2*t16^2*t20^2+
									D^2*t15^2*t16^2*t20^2-2*t15^2*t17^2*t18*t21-
									2*D*t15^2*t17^2*t18*t21+D^2*t15^2*t17^2*t18*t21+
									4*t15^3*t17*t20*t21+D*t15^3*t17*t20*t21-D^2*t15^3*t17*t20*t21-
									8*t15^2*t16*t17*t18*t22-7*D*t15^2*t16*t17*t18*t22+
									3*D^2*t15^2*t16*t17*t18*t22+8*t15^3*t17*t19*t22+
									D*t15^3*t17*t19*t22-D^2*t15^3*t17*t19*t22+8*t15^3*t16*t20*t22+
									D*t15^3*t16*t20*t22-D^2*t15^3*t16*t20*t22-4*t15^4*t22^2+
									D*t15^4*t22^2-2*t15^2*t16^2*t18*t23-2*D*t15^2*t16^2*t18*t23+
									D^2*t15^2*t16^2*t18*t23+4*t15^3*t16*t19*t23+
									D*t15^3*t16*t19*t23-D^2*t15^3*t16*t19*t23-2*t15^4*t21*t23+
									D*t15^4*t21*t23)*t4*t9)/((-3+D)*(-2+D)*D*t15^6)+
									(t12*t14*t16^2*t17^2*t5*t9)/t15^4+
									(t11*t13*(-6*t16^2*t17^2*t18^2-7*D*t16^2*t17^2*t18^2+
									D^3*t16^2*t17^2*t18^2+12*t15*t16*t17^2*t18*t19+
									11*D*t15*t16*t17^2*t18*t19-2*D^2*t15*t16*t17^2*t18*t19-
									D^3*t15*t16*t17^2*t18*t19-4*t15^2*t17^2*t19^2-
									2*D*t15^2*t17^2*t19^2+D^2*t15^2*t17^2*t19^2+
									12*t15*t16^2*t17*t18*t20+11*D*t15*t16^2*t17*t18*t20-
									2*D^2*t15*t16^2*t17*t18*t20-D^3*t15*t16^2*t17*t18*t20-
									16*t15^2*t16*t17*t19*t20-6*D*t15^2*t16*t17*t19*t20+
									D^2*t15^2*t16*t17*t19*t20+D^3*t15^2*t16*t17*t19*t20-
									4*t15^2*t16^2*t20^2-2*D*t15^2*t16^2*t20^2+
									D^2*t15^2*t16^2*t20^2-2*t15^2*t17^2*t18*t21-
									2*D*t15^2*t17^2*t18*t21+D^2*t15^2*t17^2*t18*t21+
									4*t15^3*t17*t20*t21+D*t15^3*t17*t20*t21-D^2*t15^3*t17*t20*t21-
									8*t15^2*t16*t17*t18*t22-7*D*t15^2*t16*t17*t18*t22+
									3*D^2*t15^2*t16*t17*t18*t22+8*t15^3*t17*t19*t22+
									D*t15^3*t17*t19*t22-D^2*t15^3*t17*t19*t22+8*t15^3*t16*t20*t22+
									D*t15^3*t16*t20*t22-D^2*t15^3*t16*t20*t22-4*t15^4*t22^2+
									D*t15^4*t22^2-2*t15^2*t16^2*t18*t23-2*D*t15^2*t16^2*t18*t23+
									D^2*t15^2*t16^2*t18*t23+4*t15^3*t16*t19*t23+
									D*t15^3*t16*t19*t23-D^2*t15^3*t16*t19*t23-2*t15^4*t21*t23+
									D*t15^4*t21*t23)*t5*t9)/((-3+D)*(-2+D)*D*t15^6)+
									(-(t14*t16^2*t17^2*t18*t2*t9)+2*t14*t15*t16*t17^2*t19*t2*t9-
									t14*t15^2*t17^2*t2*t21*t9)/((2-D)*t15^4)+
									(-(t12*t16^2*t17^2*t18*t3*t9)+t12*t15*t16*t17^2*t19*t3*t9+
									t12*t15*t16^2*t17*t20*t3*t9-t12*t15^2*t16*t17*t22*t3*t9)/
									((2-D)*t15^4)+(t12*t14*t16^2*t17^2*t18*t4*t9+
									D*t12*t14*t16^2*t17^2*t18*t4*t9-2*t12*t14*t15*t16*t17^2*t19*t4*t9-
									D*t12*t14*t15*t16*t17^2*t19*t4*t9-
									2*t12*t14*t15*t16^2*t17*t20*t4*t9+t12*t14*t15^2*t17^2*t21*t4*t9+
									2*t12*t14*t15^2*t16*t17*t22*t4*t9)/((2-D)*t15^5)+
									(3*t11*t13*t16^2*t17^2*t18^3*t4*t9+
									4*D*t11*t13*t16^2*t17^2*t18^3*t4*t9+
									D^2*t11*t13*t16^2*t17^2*t18^3*t4*t9-
									6*t11*t13*t15*t16*t17^2*t18^2*t19*t4*t9-
									8*D*t11*t13*t15*t16*t17^2*t18^2*t19*t4*t9-
									2*D^2*t11*t13*t15*t16*t17^2*t18^2*t19*t4*t9+
									2*t11*t13*t15^2*t17^2*t18*t19^2*t4*t9+
									3*D*t11*t13*t15^2*t17^2*t18*t19^2*t4*t9+
									D^2*t11*t13*t15^2*t17^2*t18*t19^2*t4*t9-
									6*t11*t13*t15*t16^2*t17*t18^2*t20*t4*t9-
									7*D*t11*t13*t15*t16^2*t17*t18^2*t20*t4*t9-
									D^2*t11*t13*t15*t16^2*t17*t18^2*t20*t4*t9+
									8*t11*t13*t15^2*t16*t17*t18*t19*t20*t4*t9+
									10*D*t11*t13*t15^2*t16*t17*t18*t19*t20*t4*t9+
									2*D^2*t11*t13*t15^2*t16*t17*t18*t19*t20*t4*t9-
									2*D*t11*t13*t15^3*t17*t19^2*t20*t4*t9-
									D^2*t11*t13*t15^3*t17*t19^2*t20*t4*t9+
									2*t11*t13*t15^2*t16^2*t18*t20^2*t4*t9+
									2*D*t11*t13*t15^2*t16^2*t18*t20^2*t4*t9-
									2*D*t11*t13*t15^3*t16*t19*t20^2*t4*t9+
									t11*t13*t15^2*t17^2*t18^2*t21*t4*t9+
									D*t11*t13*t15^2*t17^2*t18^2*t21*t4*t9-
									2*t11*t13*t15^3*t17*t18*t20*t21*t4*t9-
									D*t11*t13*t15^3*t17*t18*t20*t21*t4*t9+
									4*t11*t13*t15^2*t16*t17*t18^2*t22*t4*t9+
									4*D*t11*t13*t15^2*t16*t17*t18^2*t22*t4*t9-
									4*t11*t13*t15^3*t17*t18*t19*t22*t4*t9-
									4*D*t11*t13*t15^3*t17*t18*t19*t22*t4*t9-
									4*t11*t13*t15^3*t16*t18*t20*t22*t4*t9-
									2*D*t11*t13*t15^3*t16*t18*t20*t22*t4*t9+
									2*D*t11*t13*t15^4*t19*t20*t22*t4*t9+
									2*t11*t13*t15^4*t18*t22^2*t4*t9+
									t11*t13*t15^2*t16^2*t18^2*t23*t4*t9+
									D*t11*t13*t15^2*t16^2*t18^2*t23*t4*t9-
									2*t11*t13*t15^3*t16*t18*t19*t23*t4*t9-
									2*D*t11*t13*t15^3*t16*t18*t19*t23*t4*t9+
									D*t11*t13*t15^4*t19^2*t23*t4*t9+t11*t13*t15^4*t18*t21*t23*t4*t9)/
									((2-D)*D*t15^7)+(-(t10*t16^2*t17^2*t18*t5*t9)+
									t10*t15*t16*t17^2*t19*t5*t9+t10*t15*t16^2*t17*t20*t5*t9-
									t10*t15^2*t16*t17*t22*t5*t9)/((2-D)*t15^4)+
									(t11*t14*t16^2*t17^2*t18*t5*t9+D*t11*t14*t16^2*t17^2*t18*t5*t9-
									2*t11*t14*t15*t16*t17^2*t19*t5*t9-
									D*t11*t14*t15*t16*t17^2*t19*t5*t9-
									2*t11*t14*t15*t16^2*t17*t20*t5*t9+t11*t14*t15^2*t17^2*t21*t5*t9+
									2*t11*t14*t15^2*t16*t17*t22*t5*t9)/((2-D)*t15^5)+
									(t12*t13*t16^2*t17^2*t18*t5*t9+D*t12*t13*t16^2*t17^2*t18*t5*t9-
									2*t12*t13*t15*t16*t17^2*t19*t5*t9-2*t12*t13*t15*t16^2*t17*t20*t5*t9-
									D*t12*t13*t15*t16^2*t17*t20*t5*t9+2*t12*t13*t15^2*t16*t17*t22*t5*t9+
									t12*t13*t15^2*t16^2*t23*t5*t9)/((2-D)*t15^5)
								]  ) /; (Sort[Union[{qi,qj,k1,k2}]] === Sort[{k1,k2}]) &&
												(checkd[k1]|| checkd[k2]),

								(* (k1 k2)^2 *)

								qQQ[
								Eps[r___, Momentum[qi_,___], Momentum[qj_,___], s___]^2
									] :>
								(
								FCPrint[1,"USING K12SQUAREDRULE"];
								epscontract[
								( (Pair[Momentum[OPEDelta, n], Momentum[qj, n]]^2*
										(2*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											3*n*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 +
											n^2*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											2*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[m[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] +
											2*n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 +
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]])*
										Pair[Momentum[p, n], Momentum[qi, n]]^2)/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) -
									(2*Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										(2*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											3*n*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 +
											n^2*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											2*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] +
											2*n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 +
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]])*
										Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) +
									(Pair[Momentum[OPEDelta, n], Momentum[qi, n]]^2*
										(2*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											3*n*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 +
											n^2*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											2*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] +
											2*n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 +
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]])*
										Pair[Momentum[p, n], Momentum[qj, n]]^2)/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) -
									(Pair[Momentum[OPEDelta, n], Momentum[qj, n]]^2*
										(3*Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] +
											n*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] -
											2*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] +
											2*n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[p, n], Momentum[p, n]] +
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]]^2 -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]]^2)*
										Pair[Momentum[qi, n], Momentum[qi, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) -
									(2*Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										(Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											n*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] +
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] +
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]])*
										Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[qi, n], Momentum[qi, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) -
									(Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
										Pair[Momentum[p, n], Momentum[qj, n]]^2*
										Pair[Momentum[qi, n], Momentum[qi, n]])/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) +
									(2*Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										(3*Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] +
											n*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] -
											2*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] +
											2*n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[p, n], Momentum[p, n]] +
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]]^2 -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]]^2)*
										Pair[Momentum[qi, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) +
									(2*Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										(Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											n*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] +
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] +
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]])*
										Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[qi, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) +
									(2*Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										(Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											n*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] +
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] +
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]])*
										Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[qi, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) +
									(2*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
										Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[qi, n], Momentum[qj, n]])/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) +
									((2*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 +
											4*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											2*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]])*
										Pair[Momentum[qi, n], Momentum[qj, n]]^2)/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) -
									(Pair[Momentum[OPEDelta, n], Momentum[qi, n]]^2*
										(3*Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] +
											n*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] -
											2*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] +
											2*n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]*
												Pair[Momentum[p, n], Momentum[p, n]] -
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2*
												Pair[Momentum[p, n], Momentum[p, n]] +
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]]^2 -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]]^2)*
										Pair[Momentum[qj, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^4) -
									(2*Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										(Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											n*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 -
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] +
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]] +
											n*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]])*
										Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[qj, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) -
									(Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
										Pair[Momentum[p, n], Momentum[qi, n]]^2*
										Pair[Momentum[qj, n], Momentum[qj, n]])/
									((2 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) -
									((2*Eps[r, Momentum[OPEDelta, n], Momentum[p, n], s]^2 +
											4*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]*
												Eps[r, LorentzIndex[mU[1], n], Momentum[p, n], s]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]] -
											Eps[r, LorentzIndex[mU[1], n], LorentzIndex[mU[2], n], s]^2*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2 -
											2*Eps[r, LorentzIndex[mU[1], n], Momentum[OPEDelta, n], s]^2*
												Pair[Momentum[p, n], Momentum[p, n]])*
										Pair[Momentum[qi, n], Momentum[qi, n]]*
										Pair[Momentum[qj, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2)
											) ]
								) /; (Sort[{qi, qj}] === {k1, k2}) &&
															(checkd[k1] || checkd[k2]) && FreeQ[{r,s},{k1,k2}],

								(* k1 k2 k1 k2 *)
									qQQ[
								Eps[LorentzIndex[AL_,___], Momentum[OPEDelta, ___],
										Momentum[qi_, ___], Momentum[qj_, ___]
									]*
								Eps[LorentzIndex[AL_,___], Momentum[p, ___],
										Momentum[qi_, ___], Momentum[qj_, ___]
									]  ]:>
								(
								FCPrint[1,"USING QiQjQiQjRULE"];
								epscontract[
								(* o.k., forgot to do with general n in Eps ... *)
								ChangeDimension[
								( ((Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]], Momentum[OPEDelta],
												Momentum[p]]^2 - n*Eps[LorentzIndex[mU[1]],
													LorentzIndex[mU[2]], Momentum[OPEDelta], Momentum[p]]^2 -
											Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[OPEDelta, n], Momentum[qj, n]]^2*
										Pair[Momentum[p, n], Momentum[qi, n]]^2)/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) -
									(2*(Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												Momentum[OPEDelta], Momentum[p]]^2 -
											n*Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
													Momentum[OPEDelta], Momentum[p]]^2 -
											Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) +
									((Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]], Momentum[OPEDelta],
												Momentum[p]]^2 - n*Eps[LorentzIndex[mU[1]],
													LorentzIndex[mU[2]], Momentum[OPEDelta], Momentum[p]]^2 -
											Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[OPEDelta, n], Momentum[qi, n]]^2*
										Pair[Momentum[p, n], Momentum[qj, n]]^2)/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) -
									((Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]], Momentum[OPEDelta],
												Momentum[p]]^2 - n*Eps[LorentzIndex[mU[1]],
													LorentzIndex[mU[2]], Momentum[OPEDelta], Momentum[p]]^2 -
											Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[OPEDelta, n], Momentum[qj, n]]^2*
										Pair[Momentum[p, n], Momentum[p, n]]*
										Pair[Momentum[qi, n], Momentum[qi, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) -
									((Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]], Momentum[OPEDelta],
												Momentum[p]]^2 + n*Eps[LorentzIndex[mU[1]],
													LorentzIndex[mU[2]], Momentum[OPEDelta], Momentum[p]]^2 +
											2*Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[qi, n], Momentum[qi, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) +
									(2*(Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												Momentum[OPEDelta], Momentum[p]]^2 -
											n*Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
													Momentum[OPEDelta], Momentum[p]]^2 -
											Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										Pair[Momentum[p, n], Momentum[p, n]]*
										Pair[Momentum[qi, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) +
									((Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]], Momentum[OPEDelta],
												Momentum[p]]^2 + n*Eps[LorentzIndex[mU[1]],
													LorentzIndex[mU[2]], Momentum[OPEDelta], Momentum[p]]^2 +
											2*Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[OPEDelta, n], Momentum[qj, n]]*
										Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[qi, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) +
									((Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]], Momentum[OPEDelta],
												Momentum[p]]^2 + n*Eps[LorentzIndex[mU[1]],
													LorentzIndex[mU[2]], Momentum[OPEDelta], Momentum[p]]^2 +
											2*Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[qi, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) -
									((2*Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
													Momentum[OPEDelta], Momentum[p]]^2 +
											Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[qi, n], Momentum[qj, n]]^2)/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]) -
									((Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]], Momentum[OPEDelta],
												Momentum[p]]^2 - n*Eps[LorentzIndex[mU[1]],
													LorentzIndex[mU[2]], Momentum[OPEDelta], Momentum[p]]^2 -
											Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[OPEDelta, n], Momentum[qi, n]]^2*
										Pair[Momentum[p, n], Momentum[p, n]]*
										Pair[Momentum[qj, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^3) -
									((Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]], Momentum[OPEDelta],
												Momentum[p]]^2 + n*Eps[LorentzIndex[mU[1]],
													LorentzIndex[mU[2]], Momentum[OPEDelta], Momentum[p]]^2 +
											2*Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[OPEDelta, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[qj, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]]^2) +
									((2*Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]], Momentum[OPEDelta],
													Momentum[p]]^2 + Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[OPEDelta]]*
												Eps[LorentzIndex[mU[1]], LorentzIndex[mU[2]],
												LorentzIndex[mU[3]], Momentum[p]]*
												Pair[Momentum[OPEDelta, n], Momentum[p, n]])*
										Pair[Momentum[qi, n], Momentum[qi, n]]*
										Pair[Momentum[qj, n], Momentum[qj, n]])/
									((2 - n)*(3 - n)*Pair[Momentum[OPEDelta, n], Momentum[p, n]])
											), n] ]) /; (Sort[{qi, qj}] === {k1, k2}) &&
																	(checkd[qi] || checkd[qj])
								};
								decrules = Join[decrules, Drop[Drop[decrules,-1],1
																							] /. {k2 :> k1, k1 :> k2}];
								FCPrint[1,"OPE2TID checkkkk"];
								If[ speciallabel =!= True,
									temp4 = Expand2[epscontract[temp4 /. decrules]//diracsimp2,
																LorentzIndex];,
									temp4 = Expand2[temp4 /. decrulesspecial, LorentzIndex];
								];
								FCPrint[1,"decrules done"];
								If[ contractlabel === True,
									temp4 = temp4/. Pair -> PairContract3 /. PairContract3 -> Pair;
								];
								temp = nok1k2factor temp4 tempf delfactor;
								If[ k12shift =!= {},
									FCPrint[2,"shifting back"];
									temp = EpsEvaluate[ExpandScalarProduct[temp /. k12shift]];
								];
								If[ !FreeQ[temp,fake[OPEm]],
									temp = temp /. fake[OPEm]->1
								];
								If[ (contractlabel === True) && (!FreeQ[temp, LorentzIndex]),
									For[i = 1, i < 6, i++,
											If[ (EvenQ[#] && (Length[#]>0))&[Position[temp,$MU[i]]],
												temp  = temp /. $MU[i] -> muu[i];
											]
										];
									temp = Expand2[temp, LorentzIndex];
									If[ Head[temp] === Plus,
										ntemp = 0;
										For[j = 1, j <=Length[temp], j++,
													FCPrint[2,"int = ",j, "  out of",Length[temp]];
													ntemp = ntemp + Contract[temp[[j]], Expanding -> True,
																									EpsContract -> False,
																									Rename -> True]
											];
										temp = ntemp,
										temp = Contract[temp, EpsContract -> False, Expanding ->True,
																		Rename -> True]
									];
								];
								If[ True,
									If[ !FreeQ[temp, DiracTrace],
								(*
											temp = Contract[temp, EpsContract->False];
								*)
										FCPrint[1,"DiracTrick"];
										temp = DiracTrick[temp];
										FCPrint[1,"collecting DiracTrace"];
										temp = Collect2[temp, DiracTrace,
																					Factoring -> False
																	]/. DiracTrace -> Tr2
									];
									If[ contractlabel === True,
										temp = Expand2[temp, LorentzIndex];
										FCPrint[1,"PairContract"];
										temp = temp /. Pair -> PairContract3 /.
													PairContract3 -> PairContract  /.
													PairContract -> Pair;
										FCPrint[1,"PairContract done"];
										If[ !FreeQ[temp, LorentzIndex],
											For[j = 1, j < 6, j++,
													If[ (EvenQ[#] && (Length[#]>0))&[Position[temp,$MU[j]]],
														temp  = temp /. $MU[j] -> muu[j];
													]
												];
											FCPrint[1,"contracting agaaaaaaaaaain in OPE2TID"];
											temp = Expand2[temp,LorentzIndex];
											If[ Head[temp] === Plus,
												ntemp = 0;
												For[r = 1, r <=Length[temp], r++,
															FCPrint[2,"int = ",r, "  out of",Length[temp]];
															ntemp = ntemp + Contract[temp[[r]], Expanding -> True,
																											EpsContract -> False,
																											Rename -> True]
													];
												temp = ntemp,
												temp = Contract[temp, EpsContract -> False, Expanding ->True,
																				Rename -> True]
											];
										];
									];
								];
								If[ FreeQ[temp, qQQ],
									FCPrint[2,"ApartFF in OPE2TID"];
									temp = ApartFF[temp /.
										Power2 -> Power/. Power[a_, b_ /; Head[b] =!= Integer] :> Power2[a, b], {k1, k2}];
									FCPrint[2,"ApartFF in OPE2TID done"];
									temp = FeynAmpDenominatorSimplify[Collect2[temp, k1,k2], k1, k2];
									FCPrint[2,"collect after ApartFF in OPE2TID done"];

									(* ZZZ*)
									If[ !FreeQ2[kape = SelectFree[Cases2[temp,Pair], OPEDelta], {k1,k2}],
										If[ Head[temp] =!= Plus,
											temp = ApartFF[OPE1Loop[{k1,k2},temp],{k1,k2},Collecting->False],
											temp = SelectFree[temp,kape] +
											ApartFF[ OPE1Loop[{k1,k2},SelectNotFree[temp, kape]], {k1, k2},Collecting->False]
										]
									];
									temp = PowerSimplify[temp];
									temp = FeynAmpDenominatorSimplify[temp];
									temp = FeynAmpDenominatorSimplify[temp, k1, k2],
									temp  = FeynAmpDenominatorSimplify[temp,k1,k2] /. qQQ->Identity
								],
								(* !FreeQ2  delfactor *)
								(* and now, if there is no delta-factor , let's do a
									decomposition a la Pass. -Velt. : *)

								(* for clarity put tempf into qQQ *)
								(* and eventually also the delfactor if it is just a Pair[] *)
								If[ (Head[delfactor] === Pair) && (!FreeQ2[delfactor, {k1,k2}]),
									temp4 = temp4 /. qQQ[a_] :> qQQ[delfactor tempf a];
									delfactor = 1,
									temp4 = temp4 /. qQQ[a_] :> qQQ[tempf a]
								];
								fcheck[ka_, aa__] :=
									Block[ {ww},
										ww = SelectNotFree[FeynAmpDenominatorSplit[FeynAmpDenominator[aa], Momentum->{ka}], ka];
										If[ (!FreeQ[{aa}, p])(* not necessary; CHANGE 11/94
											&& (!FreeQ[ww, SelectFree[{k1,k2},ka][[1]]])
										*),
											True,
											False
										]
									];

								(*YYY*)
								decrulespv = {
								(* k1 *)
								qQQ[T1_[aa___, Momentum[k1, ___], bb___] *
										FeynAmpDenominator[ped__]
										]:>
										epscontract[ FeynAmpDenominator[ped] *
																	T1[aa, Momentum[p, n], bb] *
																	Pair[Momentum[k1,n], Momentum[p, n]]/
																	Pair[Momentum[p,n] , Momentum[p, n]]
																] /; fcheck[k1, ped] && mem[T1] &&
								(* hier war der Hund begraben .... *)
																		FreeQ2[{aa,bb}, {k1,k2}],
								qQQ[T1_[aa___, Momentum[k2, ___], bb___] *
										FeynAmpDenominator[ped__]
										]:>
										epscontract[ FeynAmpDenominator[ped] *
																	T1[aa, Momentum[p, n], bb] *
																	Pair[Momentum[k2,n], Momentum[p, n]]/
																	Pair[Momentum[p,n] , Momentum[p, n]]
																] /; fcheck[k2, ped] && mem[T1]&&
								(* hier war der Hund begraben .... *)
																		FreeQ2[{aa,bb}, {k1,k2}],
								(* (...ki...)^2 *)
								qQQ[(T1_[a___, Momentum[qi_ /; MemberQ[{k1,k2},qi], ___], b___]
										)^2 * FeynAmpDenominator[ped__]
										] :> (
								FeynAmpDenominator[ped] *
									epscontract[
									((Pair[Momentum[p, n], Momentum[qi, n]]^2 -
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[qi, n], Momentum[qi, n]])*
											T1[a, LorentzIndex[nu, n], b]^2)/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]) -
									((n*Pair[Momentum[p, n], Momentum[qi, n]]^2 -
												Pair[Momentum[p, n], Momentum[p, n]]*
												Pair[Momentum[qi, n], Momentum[qi, n]])*
											T1[a, Momentum[p, n], b]^2)/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2)
														]) /; fcheck[qi, ped] && mem[T1] &&
								(* hier war der Hund begraben .... *)
																		FreeQ2[{a,b}, {k1,k2}],

								qQQ[T1_[a___, Momentum[qi_ /; MemberQ[{k1,k2},qi], ___], b___]*
										T2_[c___, Momentum[qj_ /; MemberQ[{k1,k2},qj], ___], d___]*
										FeynAmpDenominator[ped__]
										] :>
								epscontract[
								FeynAmpDenominator[ped] * (
									(Pair[Momentum[p, n], Momentum[qi, n]]*
											Pair[Momentum[p, n], Momentum[qj, n]]*T1[a, LorentzIndex[nu, n], b]*
											T2[c, LorentzIndex[nu, n], d])/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]) -
									(Pair[Momentum[qi, n], Momentum[qj, n]]*T1[a, LorentzIndex[nu, n], b]*
											T2[c, LorentzIndex[nu, n], d])/(1 - n) -
									(n*Pair[Momentum[p, n], Momentum[qi, n]]*
											Pair[Momentum[p, n], Momentum[qj, n]]*T1[a, Momentum[p, n], b]*
											T2[c, Momentum[p, n], d])/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2) +
									(Pair[Momentum[qi, n], Momentum[qj, n]]*T1[a, Momentum[p, n], b]*
											T2[c, Momentum[p, n], d])/((1 - n)*
															Pair[Momentum[p, n], Momentum[p, n]])
													)] /; fcheck[qi, ped] && fcheck[qj, ped] && mem[T1,T2] &&
																FreeQ2[{a,b,c,d}, {k1,k2}],

								qQQ[Eps[a___, Momentum[qi_ /; MemberQ[{k1,k2},qi], ___],
															Momentum[qj_ /; MemberQ[{k1,k2},qj], ___],
												b___]*
										Eps[c___, Momentum[qk_ /; MemberQ[{k1,k2},qk], ___], d___]*
										FeynAmpDenominator[ped__]
										] :> (FeynAmpDenominator[ped] *
										epscontract[
									-((Eps[c, LorentzIndex[nu, n], d]*
												Eps[a, LorentzIndex[nu, n], Momentum[p, n], b]*
												Pair[Momentum[p, n], Momentum[qj, n]]*
												Pair[Momentum[qi, n], Momentum[qk, n]])/
											((1 - n)*Pair[Momentum[p, n], Momentum[p, n]])) +
									(Eps[c, LorentzIndex[nu, n], d]*
											Eps[a, LorentzIndex[nu, n], Momentum[p, n], b]*
											Pair[Momentum[p, n], Momentum[qi, n]]*
											Pair[Momentum[qj, n], Momentum[qk, n]])/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]])
															]) /; (fcheck[qi, ped] && fcheck[qj, ped])&&
																		FreeQ2[{a,b,c,d}, {k1,k2}],

								qQQ[T1_[a___, Momentum[qi_ /; MemberQ[{k1,k2},qi], ___], b___]*
										T2_[c___, Momentum[qj_ /; MemberQ[{k1,k2},qj], ___], d___]*
										T3_[e___, Momentum[qk_ /; MemberQ[{k1,k2},qk], ___], f___]*
											FeynAmpDenominator[ped__]
										] :> (
								rho = Unique["lI"];
								FeynAmpDenominator[ped] * (
								epscontract[
								( (Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[p, n], Momentum[qk, n]]*T1[a, Momentum[p, n], b]*
										T2[c, LorentzIndex[rho, n], d]*T3[e, LorentzIndex[rho, n], f])/
									((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2) -
									(Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[qj, n], Momentum[qk, n]]*T1[a, Momentum[p, n], b]*
										T2[c, LorentzIndex[rho, n], d]*T3[e, LorentzIndex[rho, n], f])/
									((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]) +
									(Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[p, n], Momentum[qk, n]]*T1[a, LorentzIndex[rho, n], b]*
										T2[c, Momentum[p, n], d]*T3[e, LorentzIndex[rho, n], f])/
									((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2) -
									(Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[qi, n], Momentum[qk, n]]*T1[a, LorentzIndex[rho, n], b]*
										T2[c, Momentum[p, n], d]*T3[e, LorentzIndex[rho, n], f])/
									((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]) +
									(Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[p, n], Momentum[qk, n]]*T1[a, LorentzIndex[nu, n], b]*
										T2[c, LorentzIndex[nu, n], d]*T3[e, Momentum[p, n], f])/
									((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2) -
									(Pair[Momentum[p, n], Momentum[qk, n]]*
										Pair[Momentum[qi, n], Momentum[qj, n]]*T1[a, LorentzIndex[nu, n], b]*
										T2[c, LorentzIndex[nu, n], d]*T3[e, Momentum[p, n], f])/
									((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]) -
									(2*Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[p, n], Momentum[qk, n]]*T1[a, Momentum[p, n], b]*
										T2[c, Momentum[p, n], d]*T3[e, Momentum[p, n], f])/
									((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^3) -
									(n*Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[p, n], Momentum[qk, n]]*T1[a, Momentum[p, n], b]*
										T2[c, Momentum[p, n], d]*T3[e, Momentum[p, n], f])/
									((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^3) +
									(Pair[Momentum[p, n], Momentum[qk, n]]*
										Pair[Momentum[qi, n], Momentum[qj, n]]*T1[a, Momentum[p, n], b]*
										T2[c, Momentum[p, n], d]*T3[e, Momentum[p, n], f])/
									((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2) +
									(Pair[Momentum[p, n], Momentum[qj, n]]*
										Pair[Momentum[qi, n], Momentum[qk, n]]*T1[a, Momentum[p, n], b]*
										T2[c, Momentum[p, n], d]*T3[e, Momentum[p, n], f])/
									((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2) +
									(Pair[Momentum[p, n], Momentum[qi, n]]*
										Pair[Momentum[qj, n], Momentum[qk, n]]*T1[a, Momentum[p, n], b]*
										T2[c, Momentum[p, n], d]*T3[e, Momentum[p, n], f])/
									((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2)
											)
													])) /; (fcheck[qi, ped] && fcheck[qj, ped]&&fcheck[qk, ped])&&
																	mem[T1,T2,T3] && FreeQ2[{a,b,c,d,e,f},{k1,k2}]
								,
								qQQ[T1_[a___, Momentum[qi_ /; MemberQ[{k1,k2},qi], ___], b___]^2*
										T2_[c___, Momentum[qj_ /; MemberQ[{k1,k2},qj], ___], d___] *
										FeynAmpDenominator[ped__]
										] :>
								(
								rho = Unique["lI"];
								nu = Unique["lI"];
								FeynAmpDenominator[ped] *
								epscontract[
									(2*Pair[Momentum[p, n], Momentum[qi, n]]^2*
											Pair[Momentum[p, n], Momentum[qj, n]]*T1[a, LorentzIndex[rho, n], b]*
											T1[a, Momentum[p, n], b]*T2[c, LorentzIndex[rho, n], d])/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2) -
									(2*Pair[Momentum[p, n], Momentum[qi, n]]*
											Pair[Momentum[qi, n], Momentum[qj, n]]*T1[a, LorentzIndex[rho, n], b]*
											T1[a, Momentum[p, n], b]*T2[c, LorentzIndex[rho, n], d])/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]) +
									(Pair[Momentum[p, n], Momentum[qi, n]]^2*
											Pair[Momentum[p, n], Momentum[qj, n]]*T1[a, LorentzIndex[nu, n], b]^2*
											T2[c, Momentum[p, n], d])/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2) -
									(Pair[Momentum[p, n], Momentum[qj, n]]*
											Pair[Momentum[qi, n], Momentum[qi, n]]*
											T1[a, LorentzIndex[nu, n], b]^2*T2[c, Momentum[p, n], d])/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]) -
									(2*Pair[Momentum[p, n], Momentum[qi, n]]^2*
											Pair[Momentum[p, n], Momentum[qj, n]]*T1[a, Momentum[p, n], b]^2*
											T2[c, Momentum[p, n], d])/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^3) -
									(n*Pair[Momentum[p, n], Momentum[qi, n]]^2*
											Pair[Momentum[p, n], Momentum[qj, n]]*T1[a, Momentum[p, n], b]^2*
											T2[c, Momentum[p, n], d])/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^3) +
									(Pair[Momentum[p, n], Momentum[qj, n]]*
											Pair[Momentum[qi, n], Momentum[qi, n]]*T1[a, Momentum[p, n], b]^2*
											T2[c, Momentum[p, n], d])/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2) +
									(2*Pair[Momentum[p, n], Momentum[qi, n]]*
											Pair[Momentum[qi, n], Momentum[qj, n]]*T1[a, Momentum[p, n], b]^2*
											T2[c, Momentum[p, n], d])/
										((1 - n)*Pair[Momentum[p, n], Momentum[p, n]]^2)
													]
								) /; (fcheck[qi, ped] && fcheck[qj, ped])&& mem[T1,T2] &&
													FreeQ2[{a,b,c,d},{k1,k2}]
														};
								temp4 = epscontract[temp4 /. decrulespv] /. qQQ -> Identity;
								If[ !FreeQ[temp4, LorentzIndex]  && contractlabel === True,
									temp4 = Expand2[temp4, LorentzIndex];
								];
								temp = nok1k2factor temp4 delfactor;
								FCPrint[1, "decrulespv done"];

								(*
								temp = temp /. Pair -> PairContract3 /. PairContract -> Pair;
								*)
								If[ !FreeQ[temp, LorentzIndex] &&  contractlabel === True,
									temp = Contract[temp, EpsContract -> False, Rename -> True];
								];
								If[ !FreeQ[temp,DiracGamma],
									temp = DiracTrick[temp]
								];

								(*
								If[!FreeQ[temp, Power[_, (hh_ /; Head[hh] =!= Integer)]],
									temp = temp /. Power[aa_,(ha_ /; Head[ha] =!= Integer)]:>
													Power2[aa, ha];
									];
								*)
								temp = Collect2[ApartFF[temp,k1,k2],{k1,k2}];
								temp = temp /. Power2 -> Power;
								If[ !FreeQ2[Cases2[temp,Pair], {k1,k2}],
									temp = ApartFF[OPE1Loop[{k1,k2},temp],{k1,k2}];
								];
								temp = Collect2[FeynAmpDenominatorSimplify[temp,k1,k2],{k1,k2}]
							];
						(* freeq delfactor k1 k2*)
];
					(* checkkk === 0 *)
					];
				(* head temp times *)
					];
			(* head temp plus *)
				];
		(* head temp plus *)
			]; (* freeq exp k1 or k2*)
		temp/.Power2->Power
	];

FCPrint[1,"OPE2TID.m loaded"];
End[]
