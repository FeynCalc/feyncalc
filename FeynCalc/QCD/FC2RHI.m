(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FC2RHI *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  change 2-loop OPE-integrals into Roelofs
							notation  (eq. (3C.19))
*)

(* ------------------------------------------------------------------------ *)

FC2RHI::usage =
"FC2RHI[exp, k1, k2] transforms all 2-loop OPE-integrals
in FeynAmpDenominator form to the RHI-integrals.
FC2RHI[exp] is equivalent to FC2RHI[exp,q1,q2].
The option IncludePair governs the inclusion  of scalar products
p.k1, p.k2 and k1.k2 (setting True).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FC2RHI`Private`"]

Options[FC2RHI] = {Dimension -> D, IncludePair -> True,
									Do -> True};

FC2RHI[exp_, opt___Rule] :=
	FC2RHI[exp, FCGV["q1"], FCGV["q2"], opt];

FC2RHI[0,__] :=
	0;
FC2RHI[y_, k1_, k2_, ___Rule] :=
	y /; FreeQ2[y, {k1,k2}];
FC2RHI[y_ /; ((Head[y] =!= Plus) && (Head[y]=!=Times)
						) || FreeQ[y, FeynAmpDenominator] ,__
			] :=
	y;
FC2RHI[x_Plus, b__] :=
	Map[FC2RHI[#, b]&, x] /;
						 !FreeQ[x,FeynAmpDenominator];

FC2RHI[xy_ /; Head[xy]=!=Plus, k1_, k2_, opts___Rule] :=
	If[ !FreeQ[xy, FeynAmpDenominator] &&
		FreeQ[xy, PropagatorDenominator[_, ma_ /; ma=!=0]],
		MemSet[FC2RHI[xy, k1,k2,opts],
		Block[ {x, xx,nx, ne,fa, dim, fd, dp, dk1, dk2, dpk1, dpk2, dk1k2,p,
		nk12, nk22, k12,k22,k1k2,k1p,k2p, pk12,pk22,k1k22,
		fad, inc, a, b, c, d, e, doheuristics,power2sub,(*dof,*)
			vV,wW,xX,yY,zZ, al, be, ga, de, ep, result, check,dummyp},
			dim = Dimension   /. {opts} /. Options[FC2RHI];
			inc = IncludePair /. {opts} /. Options[FC2RHI];
			doheuristics = Do /. {opts} /. Options[FC2RHI];
			x = Expand2[Expand2[xy, k1], k2]//PowerSimplify;
			result = x;
			If[ x =!= 0,
				If[ Head[x] === Plus,
					result = Map[FC2RHI[#, k1, k2, opts]&, x],
					If[ !FreeQ2[x, {Pair[Momentum[k1,___], Momentum[k1,___]],
												Pair[Momentum[k2,___], Momentum[k2,___]] }
									],
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
							) ||
							(!FreeQ[SelectNotFree[SelectNotFree[x, Pair], {k1,k2}], LorentzIndex]),
							result = x,
							fad = (List@@SelectNotFree[x, FeynAmpDenominator]) /.
										PropagatorDenominator[w_, 0] -> w /. Momentum[a_, _] -> a;
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
								power2sub = Table[power2sub[[ij]] ->
																	(power2sub[[ij]]/.Power2->Power),
																	{ij,Length[power2sub]}
																	];
								xx = xx /. power2sub;,
								power2sub = {}
							];
							pru = (a_)^(w_ /; Head[w] =!= Integer) :>
										(PowerExpand[Factor2[one*a]^w] /. one -> 1);
							xx = xx //. pru;
							xx = xx /. { (-dp + dk1)^w_ :> ( (-1)^w (dp - dk1)^w ),
													(-dp + dk2)^w_ :> ( (-1)^w (dp - dk2)^w ),
													(-dk1 + dk2)^w_ :> ( (-1)^w (dk1 - dk2)^w )
												};
							nx = SelectNotFree[xx, {k1,k2}];
							fa = xx / nx;
							fa = fa /. {(-1)^w_ :> Expand[(-1)^w]} /.
												{(-1)^(2 OPEm) :> 1, (-1)^(2 OPEi) :> 1,(-1)^(2 OPEj) :> 1};

							(*
							nx = FeynAmpDenominatorSimplify[nx, k1, k2] /. pru;
							*)
							If[ (!FreeQ[nx, (a_ /; Head[a]===Plus && Length[a]===3)^m_ ]
								) || (!FreeQ[nx, PropagatorDenominator[Momentum[a__] +
																												Momentum[b__], 0]
														]
											)
									|| (!FreeQ[nx, PropagatorDenominator[-Momentum[a__] -
																												Momentum[b__], 0]
														]
											) || (!FreeQ[nx, PropagatorDenominator[_.Momentum[a__] +
																				_. Momentum[b__] + _. Momentum[c__], 0]
																	]
													),
								If[ doheuristics === True,
									dof[yy_] :=
										dof[yy] = FC2RHI[FeynAmpDenominatorSimplify[
															yy,
														k1, k2,FC2RHI->False], k1,k2, Do -> False
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
								nx = nx /.
									{Pair[Momentum[k1,dim], Momentum[k1,dim]]    :> nk12,
									Pair[Momentum[k2,dim], Momentum[k2,dim]]    :> nk22,
									Pair[Momentum[k1,dim], Momentum[k2,dim]]    :> k1k2,
									Pair[Momentum[k1,dim], Momentum[p,dim]]     :> k1p,
									Pair[Momentum[k2,dim], Momentum[p,dim]]     :> k2p,

									PropagatorDenominator[Momentum[k1, dim], 0] :> k12,
									PropagatorDenominator[Momentum[k2, dim], 0] :> k22,
									PropagatorDenominator[ Momentum[p, dim] - Momentum[k1, dim], 0] :> pk12,
									PropagatorDenominator[-Momentum[p, dim] + Momentum[k1, dim], 0] :> pk12,
									PropagatorDenominator[ Momentum[p, dim] - Momentum[k2, dim], 0] :> pk22,
									PropagatorDenominator[-Momentum[p, dim] + Momentum[k2, dim], 0] :> pk22,
									PropagatorDenominator[ Momentum[k1, dim] - Momentum[k2, dim], 0] :> k1k22,
									PropagatorDenominator[-Momentum[k1, dim] + Momentum[k2, dim], 0] :> k1k22
									};
								nx = nx /. fd[y__] :> (1/Apply[Times, {y}]);
								check = nx;

								(* this is (3C.19) + scalar products in the numerator *)
								iall = nk12^vV nk22^wW k1p^xX k2p^yY k1k2^zZ *
											dk1^a dk2^b dpk1^c dpk2^d dk1k2^e/
											(k12^al k22^be pk12^ga pk22^de k1k22^ep);

								(* in order to fool the pattern matcher *)
								int = nx iall;
								se[y_] :=
									(If[ #===1,
										 0,
										 If[ Head[#] === Power,
											 #[[2]],
											 y
										 ]
									 ]&
											  )[SelectNotFree[int, y^h_]];
								rhi = Map[se,
													{nk12,nk22,k1p,k2p,k1k2,
													dk1, dk2, dpk1, dpk2, dk1k2, k12, k22, pk12, pk22, k1k22}
												] - {vV,wW,xX,yY,zZ, a,b,c,d,e,-al,-be,-ga,-de,-ep};

								(* check if this is really correct *)
								ncheck = nk12^rhi[[1]] nk22^rhi[[2]] k1p^rhi[[3]] k2p^rhi[[4]] *
												k1k2^rhi[[5]]*
												dk1^rhi[[6]] dk2^rhi[[7]] dpk1^rhi[[8]] dpk2^rhi[[9]] *
												dk1k2^rhi[[10]] (k12^rhi[[11]] k22^rhi[[12]] pk12^rhi[[13]] *
																				pk22^rhi[[14]] k1k22^rhi[[15]]);
								If[ ncheck === check,
									result = fa RHI[Take[rhi, 5], Take[rhi,{6,10}], -Drop[rhi,10]],
									result = x
								];
								result = PowerSimplify[result];
								If[ power2sub =!= {} && !FreeQ[result,k1],
									result = result /. Map[Reverse, power2sub]
								];
							];
						];
					];
				];
			];
			result
		]],
		xy
	];

FCPrint[1,"FC2RHI.m loaded"];
End[]
