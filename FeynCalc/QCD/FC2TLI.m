(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FC2TLI *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 September '97 at 9:12 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  change 2-loop OPE-integrals into Roelofs
							notation  (eq. (3C.19))
*)

(* ------------------------------------------------------------------------ *)


FC2TLI::usage =
"FC2TLI[exp, k1, k2] transforms all 2-loop OPE-integrals in FeynAmpDenominator
form to the TLI-integrals. The option IncludePair governs the inclusion of
scalar products p.k1, p.k2 and k1.k2 (setting True).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FC2TLI`Private`"];

Options[FC2TLI] = {Dimension -> D, IncludePair -> True,
									Do -> True};

FC2TLI[0,__] :=
	0;
FC2TLI[y_, k1_, k2_, ___Rule] :=
	y /; FreeQ2[y, {k1,k2}];
FC2TLI[y_ /; ((Head[y] =!= Plus) && (Head[y]=!=Times)
						) || FreeQ2[y, {FAD,FeynAmpDenominator}] ,__
			] :=
	y;
FC2TLI[x_Plus, b__] :=
	Map[FC2TLI[#, b]&, x] /;
						!FreeQ2[x,{FAD,FeynAmpDenominator}];

FC2TLI[xy_ /; Head[xy]=!=Plus, k1_, k2_, opts___Rule] :=
	If[ !FreeQ2[xy, {FeynAmpDenominator,FAD}],
		MemSet[FC2TLI[xy, k1,k2,opts],
		Block[ {x, xx,nx, onx, ne,fa, dim, fd, dp, dk1, dk2, dpk1, dpk2, dk1k2,p,
		nk12, nk22, k12,k22,k1k2,k1p,k2p, pk12,pk22,k1k22,mf, mfm,
		fad, inc, c, d, e, doheuristics,power2sub,(*dof,*)pfix,
			vV,wW,xX,yY,zZ, al, be, ga, de, ep, result, check,dummyp,
				lr1, lr2, lork1,lork2, lorfa, lorcheck, qch, pru, anx, mass,
				iall, int, rhi, ncheck},
			dim = Dimension   /. {opts} /. Options[FC2TLI];
			inc = IncludePair /. {opts} /. Options[FC2TLI];
			doheuristics = Do /. {opts} /. Options[FC2TLI];
			x = Expand2[Expand2[xy//FeynCalcInternal, k1], k2
								]//PowerSimplify//ExpandScalarProduct;
			result = x;
			If[ x =!= 0,
				If[ Head[x] === Plus,
					result = Map[FC2TLI[#, k1, k2, opts]&, x],
					If[ doheuristics &&
							(!FreeQ2[x, {Pair[Momentum[k1,___], Momentum[k1,___]],
												Pair[Momentum[k2,___], Momentum[k2,___]] }
									]),
						x = FeynAmpDenominatorCombine[ IFPDOff[ IFPDOn[x, k1, k2],
								k1, k2]]
					];
					If[ x === 0,
						result = 0,

			(* for dimensional reduction *)
						If[ FreeQ[SelectFree[x, Eps],LorentzIndex],
							x = SelectNotFree[x,Eps] ChangeDimension[SelectFree[x,Eps],dim]
						];

						(* include k1^2 and k2^2 *)
						qch = SelectFree[SelectFree[x, {OPEDelta, FeynAmpDenominator}],
													{Pair[Momentum[k1,___], Momentum[k1,___]],
													Pair[Momentum[k2,___], Momentum[k2,___]]
												}];
						If[ ((inc === False) && (!FreeQ2[qch, {k1, k2}])  ) ||
							(!FreeQ2[qch, {Eps[___, Momentum[k1,___], ___],
															Eps[___, Momentum[k2,___], ___]
														}
											]
							)(*||
							(!FreeQ[SelectNotFree[SelectNotFree[x, Pair], {k1,k2}], LorentzIndex])
						*)
							,
							result = x,
							fad = (List@@SelectNotFree[x, FeynAmpDenominator]) /.
										PropagatorDenominator[w_, _] :> w /. Momentum[a_, _] :> a;
							p = SelectFree[Union[Flatten[Map[Variables,fad]]], {k1,k2} /.
												Momentum[a_,_] -> a ];
							If[ p =!= {},
								p = p[[1]],
								p = dummyp
							];
							dp    = Pair[Momentum[OPEDelta, dim], Momentum[p, dim]];
							dk1   = Pair[Momentum[OPEDelta, dim], Momentum[k1, dim]];
							dk2   = Pair[Momentum[OPEDelta, dim], Momentum[k2, dim]];
							dpk1  = dp  - dk1;
							dpk2  = dp  - dk2;
							dk1k2 = dk1 - dk2;
							xx = x;
							If[ !FreeQ[xx, Power2],
								power2sub = Cases2[xx, Power2];
								power2sub = Table[power2sub[[r]] ->
																	(power2sub[[r]]/.Power2->Power),
																	{r,Length[power2sub]}
																	];
								xx = xx /. power2sub;,
								power2sub = {}
							];
							pru = (a_)^(w_ /; Head[w] =!= Integer) :> (PowerExpand[Factor2[one*a]^w] /. one -> 1);
							xx = xx //. pru;
							xx = xx /. { (-dp + dk1)^w_ :> ( (-1)^w (dp - dk1)^w ),
													(-dp + dk2)^w_ :> ( (-1)^w (dp - dk2)^w ),
													(-dk1 + dk2)^w_ :> ( (-1)^w (dk1 - dk2)^w )
												};
							lr1 = Cases2[SelectNotFree[xx, k1], LorentzIndex];
							If[ Length[lr1] > 0,
								lork1 = Map[First, lr1],
								lork1 = {}
							];
							lr2 = Cases2[SelectNotFree[xx,k2], LorentzIndex];
							If[ Length[lr2] > 0,
								lork2 = Map[First, lr2],
								lork2 = {}
							];
							lorfa = If[ Length[lork1] === 0,
										1,
										Times@@(Pair[Momentum[k1,dim], LorentzIndex[#,dim]]& /@ lork1)
									] *
												If[ Length[lork2] === 0,
													1,
													Times@@(Pair[Momentum[k1,dim], LorentzIndex[#,dim]]& /@ lork1)
												];
							If[ SelectNotFree[SelectNotFree[xx,{k1,k2}],LorentzIndex] =!= lorfa,
								lorcheck = False,
								lorcheck = True
							];
							If[ lorcheck === False,
								result = x,
								nx = SelectNotFree[xx, {k1,k2}];
								fa = xx / nx;
								fa = fa /. {(-1)^w_ :> Expand[(-1)^w]} /.
													{(-1)^(2 OPEm) :> 1, (-1)^(2 OPEi) :> 1,(-1)^(2 OPEj) :> 1};

								(*
								nx = FeynAmpDenominatorSimplify[nx, k1, k2] /. pru;
								*)
								If[ (!FreeQ[nx, (a_ /; Head[a]===Plus && Length[a]===3)^m_ ]
									) || (!FreeQ[nx, PropagatorDenominator[Momentum[a__] +
																													Momentum[b__], _]
															]
												)
										|| (!FreeQ[nx, PropagatorDenominator[-Momentum[a__] -
																													Momentum[b__], _]
															]
												) || (!FreeQ[nx, PropagatorDenominator[_.Momentum[a__] +
																					_. Momentum[b__] + _. Momentum[c__], _]
																		]
														),
									If[ doheuristics === True,
										dof[yy_] :=
											dof[yy] = FC2TLI[FeynAmpDenominatorSimplify[
																yy,
															k1, k2,FC2TLI->False], k1,k2, Do -> False
																						];
										(* this is heuristic ... *)
										FCPrint[1,"heuristics "];
										nx = Catch[
																If[ FreeQ[nx, PropagatorDenominator[Momentum[a__] +
																														Momentum[b__], 0]
																				] &&
																	FreeQ[nx, PropagatorDenominator[-Momentum[a__] -
																														Momentum[b__], 0]
																				],
																	ne = nx /. {k1 :> k2, k2 :> k1};
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k1 -> (-k1 + k2)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k1 -> (-k1 + p)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k2 -> (-k2 + p)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k1 -> (-k1+p) /. k2 -> (- k2+p)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k2 -> (k2 + k1) /. k1 -> (k1 - k2)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k2 -> (k2 - k1) /. k1 -> (k1 + k2)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k2 -> (-k2 + k1)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	],
																	ne = ExpandScalarProduct[
																				nx /. k1 -> (-k1+p)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k2 -> (-k2+p)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k1 -> (-k1)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k2 -> (-k2)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k1 -> (-k1) /. k2 -> (-k2)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																	ne = ExpandScalarProduct[
																				nx /. k1 -> (k1-p) /. k2 -> (-k2+k1)];
																	If[ FreeQ[ne = dof[ne], FeynAmpDenominator],
																		Throw[ne]
																	];
																];
																nx
															]
									];
								];
								If[ FreeQ[nx, FeynAmpDenominator],
									result = fa nx,
									nx = nx /. FeynAmpDenominator -> fd;
									anx = nx;
									nx = nx /.
										{Pair[Momentum[k1,dim], Momentum[k1,dim]]    :> nk12,
										Pair[Momentum[k2,dim], Momentum[k2,dim]]    :> nk22,
										Pair[Momentum[k1,dim], Momentum[k2,dim]]    :> k1k2,
										Pair[Momentum[k1,dim], Momentum[p,dim]]     :> k1p,
										Pair[Momentum[k2,dim], Momentum[p,dim]]     :> k2p,
										PropagatorDenominator[Momentum[k1, dim], m_] :> k12[m],
										PropagatorDenominator[Momentum[k2, dim], m_] :> k22[m],
										PropagatorDenominator[ Momentum[p, dim] - Momentum[k1, dim], m_]
											:> pk12[m],
										PropagatorDenominator[-Momentum[p, dim] + Momentum[k1, dim], m_]
											:> pk12[m],
										PropagatorDenominator[ Momentum[p, dim] - Momentum[k2, dim], m_]
											:> pk22[m],
										PropagatorDenominator[-Momentum[p, dim] + Momentum[k2, dim], m_]
											:> pk22[m],
										PropagatorDenominator[ Momentum[k1, dim] - Momentum[k2, dim], m_]
											:> k1k22[m],
										PropagatorDenominator[-Momentum[k1, dim] + Momentum[k2, dim], m_]
											:> k1k22[m]
										};
									mass = Union[Select[
																			Map[#[[2]]&, Cases2[anx, PropagatorDenominator]],
																				Head[#]=!=Integer&
																		]
															];
									onx = nx;
									(*NN*)
									If[ Length[mass] === 1,
										mass = mass[[1]];
		(*NN*)
										mf[aa_] :=
											If[ !FreeQ[onx,aa[mass]],
												aa[mass],
												aa[0]
											];
							(*NN*)
										mfm[aa_] :=
											If[ !FreeQ[onx,aa[mass]],
												{aa[mass],mass},
												{aa[0],0}
											];,
										If[ Length[mass] === 0,
											mf[aa_] :=
												aa[0];
											mfm[aa_] :=
												{aa[0],0},
											mf[aa_] :=
												Catch[Do[If[ !FreeQ[onx,aa[mass[[j]]]],
															Throw[aa[mass[[j]]]]
														],
																			{j, Length[mass]}];
													aa[0]
																	];
											mfm[aa_] :=
												Catch[Do[If[ !FreeQ[onx,aa[mass[[j]]]],
															Throw[{aa[mass[[j]]],mass[[j]]}]
														],
																			{j, Length[mass]}];
													{aa[0],0}
																	];
										];
									];
									nx = nx /. fd[y__] :> (1/Apply[Times, {y}]);
									check = nx;

									(* this is (3C.19) + scalar products in the numerator *)
									iall = nk12^vV nk22^wW k1p^xX k2p^yY k1k2^zZ *
												dk1^a dk2^b dpk1^c dpk2^d dk1k2^e/
												(mf[k12]^al mf[k22]^be mf[pk12]^ga mf[pk22]^de mf[k1k22]^ep);


									(* in order to fool the pattern matcher *)
									int = nx iall;
									se[y_] :=
										(If[ #===1,
											0,
											If[ Head[#] === Power,
												If[ Length[#[[1]]]===1,
													{#[[2]],#[[1,1]]},
													#[[2]]
												],
												y
											]
										]&
												)[ SelectNotFree[int, y^h_] ];
									rhi = Map[se,
														{nk12,nk22,k1p,k2p,k1k2,
														dk1, dk2, dpk1, dpk2, dk1k2,
														mfm[k12], mfm[k22], mfm[pk12], mfm[pk22], mfm[k1k22]}
													] - {vV, wW, xX, yY, zZ,  a,b,c,d,e,
																{-al,0}, {-be,0}, {-ga,0}, {-de,0}, {-ep,0}
															};

									(* check if this is really correct *)
									ncheck = nk12^rhi[[1]] nk22^rhi[[2]] k1p^rhi[[3]] k2p^rhi[[4]] *
													k1k2^rhi[[5]]*
													dk1^rhi[[6]] dk2^rhi[[7]] dpk1^rhi[[8]] dpk2^rhi[[9]] *
													dk1k2^rhi[[10]] *
													(k12[rhi[[11,2]]]^rhi[[11,1]] *
														k22[rhi[[12,2]]]^rhi[[12,1]] *
														pk12[rhi[[13,2]]]^rhi[[13,1]] *
														pk22[rhi[[14,2]]]^rhi[[14,1]] *
														k1k22[rhi[[15,2]]]^rhi[[15,1]]
													) lorfa;
									pfix[{a_,0}] :=
										-a;
									pfix[{a_,b_ /; b=!=0}] :=
										{-a,b};
									If[ ncheck === check,
										If[ lorfa === 1,
											result = fa TLI[Take[rhi, 5], Take[rhi,{6,10}],
															Map[pfix, Drop[rhi,10]]],
											result = fa TLI[lork1,lork2][
																			Take[rhi, 5], Take[rhi,{6,10}],
																			Map[pfix,Drop[rhi,10]]
																									]
										],
										result = x
									];
									result = PowerSimplify[result];
									If[ power2sub =!= {} && !FreeQ[result,k1],
										result = result /. Map[Reverse, power2sub]
									];
								];
							];
						(*lorcheck*)
							];
					];
				];
			];
			result
		]],
		xy
	];

FCPrint[1,"FC2TLI.m loaded"];
End[]
