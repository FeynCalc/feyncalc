(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CalculateCounterTerm *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 September '97 at 9:14 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

CalculateCounterTerm::usage =
"CalculateCounterTerm[exp, k] calculates the residue of exp.This is a rather
special function designed for some specific OPE calculations. Not a universal
routine for daily use.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`CalculateCounterTerm`Private`"]

Options[CalculateCounterTerm] = {
	Chisholm -> False,
	FinalSubstitutions -> {D -> 4}
};


mapart[y_Plus] :=
	mapart /@ y;
mapart[y_Times] :=
	SelectFree[y, OPEm] *
	SelectNotFree[SelectNotFree[y, OPEm], {(-1)^(_. + OPEm), Pair}] *
	Apart[SelectFree[SelectNotFree[y, OPEm], {(-1)^(_. + OPEm), Pair}]];

topower2[y_] :=
	y /. Power2 -> Power /. (a_ /; !FreeQ[a,OPEDelta])^
	(p_ /; Head[p] =!= Integer) :> Power2[a, p];

opback[y_] :=
	If[ Head[y] === Plus,
		opback/@y,
		If[ FreeQ[y, OPESum],
			y,
			If[ Head[y] =!= Times,
				y,
				opsel[y]
			]
		]
	];
opsel[z_ OPESum[{opi_, a_, b_}]] :=
	SelectFree[z,opi] opsav[OPESum[SelectNotFree[z,opi],{opi,a,b}]];

opsav[OPESum[a_,{ii_,0,em_}]] :=
	opsav[OPESum[a,{ii,0,em}]] =
	Block[ {tt,sums,null1,sumsavs},
(* l = b-a *)
		sums[1/(aa_ - i_),{i_, 0, bb_}] :=
			(PolyGamma[0,1+aa] - PolyGamma[0,aa-bb]
			) /; Variables[aa] === Variables[bb];
		sumsavs[z__] :=
			sumsavs[z] = Sum[z];
		tt = Apart[a, ii] + null1;
		tt = (SelectFree[#,ii] sums[SelectNotFree[#,ii], {ii,0,em}])& /@ tt;
		If[ $VersionNumber>2.2,
			FullSimplify[tt/.null1->0/.sums->sumsavs],
			Factor2[SimplifyPolyGamma[tt/.null1->0/.sums->sumsavs]]
		]
	];

(* shift eventually *)
fixpower2[y_, k_, {a_,b__}] :=
	fixpower2[fixpower2[y,k,{a}], k, {b}];

fixpower2[y_Plus, k_,po_] :=
	Map[fixpower2[#,k,po]&, y];

fixpower2[y_Times, k_,
			{Power2[m_. Pair[Momentum[OPEDelta,___], Momentum[k_,___]] +
			n_. Pair[Momentum[OPEDelta,___], Momentum[p_,___]], _]
			}
		] :=
	If[ FreeQ[y, Power2[_. Pair[Momentum[OPEDelta,___],
								Momentum[k,___]] +
					_. Pair[Momentum[OPEDelta,___],
								Momentum[p,___]], _
					]
			],
		y,
		Expand[(SelectFree[y, k] *
			EpsEvaluate[Contract[DiracSimplify[ExpandScalarProduct[
			FeynAmpDenominatorSimplify[
			SelectNotFree[y, k] /. k -> (-(k/m) -(n/m) p),k]]]]
					])/.Power2[-1,OPEm]:> (-1)^OPEm]//PowerSimplify
	];



CalculateCounterTerm[exp_, k_, saveit_:D, opt___Rule] :=
	Block[ {t0 = exp, chish, ta, sunt,pow2,opvar,finsub,
	lt, t1,t2,t3,t4,t5,t6,t7,ht7,t8,t9,t10,t11,t12,t13,nt7,lnt7, pr, sut8,
	fsut8, fa1, fa2},
		chish = Chisholm /. {opt} /. Options[CalculateCounterTerm];
		t1 = Collect2[ChangeDimension[t0, 4] // Trick,
						SUNIndex,Factoring->False];
		finsub =
		(FinalSubstitutions/. {opt} /. Options[CalculateCounterTerm]);
		FCPrint[1,"color algebra"];
		t2 = SUNSimplify[t1, Explicit -> False, SUNTrace -> False];
		If[ Length[Cases2[t2,SUNT]] === 1,
			ta = Cases2[t2,SUNT][[1]];
			t2 = Trick[t2 /. ta -> sunt]
		];
		FCPrint[1,"contraction"];
		t3 = Contract[FeynAmpDenominatorSimplify[t2,k]]//EpsEvaluate;
		FCPrint[1,"insertion of operators"];
		t4 = t3 /. {Twist2GluonOperator[ww__] :>
					Twist2GluonOperator[ww, Explicit -> True, Dimension->4],
					Twist2QuarkOperator[ww__] :>
					Twist2QuarkOperator[ww, Explicit -> True, Dimension->4],
					GluonVertex[aa__] :> GluonVertex[aa, Explicit->True,
											Dimension -> 4]
					};
	(* do a special sum *)
		t4 = topower2[t4] /. OPESum[em_. Power2[a_ /;!FreeQ[a,k],pa_] *
										Power2[b_ /;!FreeQ[b,k],pb_],
									{oi_,0,emm_}
									] :>
			topower2[Apart[OPESumExplicit[OPESum[em a^pa b^pb, {oi,0,emm}]
											]
							]
					];
		t4 = t4 /.OPESum[a_,b__List]:> ( OPESum[b] a );
		If[ !FreeQ[t4, DiracGamma],
			FCPrint[1,"contraction and DiracSimplify "];
			t5 = Contract[t4]//DiracSimplify//ExpandScalarProduct;
			dord[y__] :=
				dord[y] = DiracOrder[DOT[y]];
			t5 = DotSimplify[t5 /. DOT -> dord]//Contract,
			FCPrint[1,"contraction "];
			t5 = Contract[t4]//ExpandScalarProduct
		];
		t5 = PowerSimplify[t5];
		FCPrint[1,"cancel scalar products"];
		t6 = ApartFF[t5//EpsEvaluate, {k}] /. Power2->Power /.
			Power[a_, b_/;Head[b]=!=Integer] :> Power2[a, b];
		t6 = Collect2[ChangeDimension[t6,4], k];
		pow2 = SelectNotFree[SelectNotFree[Cases2[t6, Power2],k], Power2[_Plus,_]];
		If[ pow2 =!= {},
			FCPrint[1,"fixpower2"];
			t6 = fixpower2[t6, k, pow2]/.fixpower2[aa_,__]:>aa
		];
		t6 = FeynAmpDenominatorSimplify[t6,k];
		If[ chish === True,
			FCPrint[1,"CHISHOLM"];
			t6 = Collect2[t6,DiracGamma,Eps, Factoring->False];
			doc[y__] :=
				doc[y] = Chisholm[DOT[y]];
			t6 = Contract[t6 /. DOT -> doc];
			t6 = Contract[t6,EpsContract->False, Rename -> True];
			t6 = ApartFF[t6,{k}];
		];
		FCPrint[1,"collect w.r.t. integration momentum"];
		t7 = Collect2[ChangeDimension[t6/.Power2->Power,4],
						k, Factoring -> True
					];
		t7 = PowerSimplify[DiracTrick[FeynAmpDenominatorSimplify[t7,k]]];
		t7 = Collect2[t7, k, Factoring->False];

		(*
		FCPrint[1,"entering OneLoopSimplify"];
		t7 = OneLoopSimplify[t7,k, Dimension -> 4, Collecting -> False];
		FCPrint[1,"exiting OneLoopSimplify"];
		*)
		nt7 = 0;
		If[ Head[t7] =!= Plus,
			nt7 = ChangeDimension[TID[t7, k, Isolate -> True], 4],
			lnt7 =  Length[t7];
			For[r = 1, r <= lnt7, r++,
				FCPrint[2,"ijn = ",r,"  out of", lnt7,
				" ", InputForm[SelectNotFree[t7[[r]], k]]];
				nt7 = nt7 + (( ( SelectFree[dummy t7[[r]],k]
											) /.dummy -> 1
							) *
					ChangeDimension[
					FixedPoint[ReleaseHold,
					TID[SelectNotFree[dummy t7[[r]],k], k,
										Contract -> True,
										Isolate -> True
						]     ], 4]
							)
				]
		];
		t7 = Collect2[ nt7//DiracSimplify, k, Factoring -> True];

		(*
			kmunu = SelectNotFree[SelectNotFree[Cases2[t7,Pair],k],LorentzIndex];
		If[Length[kmunu] >0,
			t7 = t7+null1 + null2;
			FCPrint[1,"oneloopsimplify   ",kmunu];
			t7 = Collect2[SelectFree[t7,kmunu] +
							ChangeDimension[
							OneLoopSimplify[SelectNotFree[t7, kmunu], k], 4], k
						]
			];
		*)
		FCPrint[1,"doing the integrals "];
		t7 = ChangeDimension[t7, D];
		If[ StringQ[saveit],
			Write2 @@ {ToString[saveit], TT == Isolate[t7] };
			t12 = FixedPoint[ReleaseHold, t7];
		];
		If[ !StringQ[saveit],
			If[ Head[t7] =!= Plus,
				t8 = OPEIntegrate2[t7,k,Integratedx -> True,
										OPEDelta    -> False,
										Collecting  -> False
								],
				(* else *)
				t8 = 0;
				lt = Length[t7];
				ht7 = Hold@@{t7};
				For[i = 1, i <= lt, i++, pr@@{"i ======== ", i," out of ",lt};
										t8 = t8 + (
										SelectFree[FCPrint[1,". "];
													ht7[[1,i]],k] Collect2[FCPrint[1,".. "];
																		PowerSimplify[FCPrint[1,"... "];
																						OPEIntegrate2[FCPrint[1,".... "];
																									SelectNotFree[FCPrint[1,"..... "];
																													ht7[[1, i]], k], k,
																										Integratedx -> True,
																										OPEDelta    -> False,
																										Collecting  -> False
																										]/.(FCPrint[1,"...... "];
																											finsub)
																					] , LorentzIndex
																		]
												)
					];
			];
			t8 = ChangeDimension[t8,4] /. finsub;
			t8 = t8 /. z_^(em_/;Head[em]=!=Integer) /(a_ (a_ - z_)) :>
					(z^(em-1) (1/(a-z)-1/a));
			t8 = t8 /. (a_ b_^(em_/;Head[em]=!=Integer)/(a_ - b_)) :>
					(b^em +  b^(em+1)/(a-b));
			If[ !FreeQ[t8, Eps],
				t8 = EpsEvaluate[t8]
			];
			t8 = PowerSimplify[Expand2[t8,OPEm]]/.Power2->Power;
			t8 = t8 /. finsub;
			If[ !FreeQ[t8, OPESum],
				FCPrint[1,"sum it back"];
				opvar = Map[First, Cases2[t8, OPESum]/.
						OPESum->Identity]//Union;
				FCPrint[1," there are ",opvar];
				t8 = Collect2[t8, opvar, Factoring -> False];
				If[ Head[t8]===Plus,
					sut8 = SelectNotFree[t8,opvar];
					FCPrint[1,"CHECK if the spurious sums cancel"];
					fsut8 = Factor[sut8];
					If[ fsut8===0,
						FCPrint[1,"YEAHHH! indeed it cancels; even without an explicit human mind"];
						t8 = t8-sut8,
						t8 = t8-sut8 + opback[Collect2[fsut8,opvar]];
					]
				];
			];
			t8 = t8 /. PolyGamma[0, em_ +1] :> (1/em + PolyGamma[0, em]);
			If[ FreeQ[t8, DOT],
				t8 = t8,
				If[ (!FreeQ[t8, Eps]) && (!FreeQ[t8, DiracGamma]),
					t8 = Contract[Collect2[t8,{Eps,DiracGamma},Factoring->False
											], Rename -> True
									] /. {(DiracGamma[LorentzIndex[mu3_]] .
										DiracGamma[LorentzIndex[mu1_]] .
										DiracGamma[LorentzIndex[mu2_]] *
										Eps[LorentzIndex[mu1_],
											LorentzIndex[mu2_],
											LorentzIndex[mu3_],
											Momentum[OPEDelta]]
										) :> FeynCalcInternal[
											GA[mu1,mu2,mu3]*
											LC[mu1,mu2,mu3][OPEDelta]
															]
										}
				]
			];
			If[ LeafCount[t8]<220,
				If[ $VersionNumber>2.2,
					FCPrint[1,"FullSimplify at the end "];
					t9  = FullSimplify[Factor2[Expand2[t8,OPEm]//PowerSimplify
									]]//Factor2,
					FCPrint[1,"Factor2 at the end "];
					t9  = Factor2[Expand2[t8,OPEm]//PowerSimplify]
				];
				If[ t9 =!= 0,
					fa1 = SelectNotFree[t9, {(-1)^_, CA, CF}];
					t9  = t9/fa1;
					fa1 = fa1 / 2;
					t9  = 2 t9;
					fa2 = Numerator[SelectFree[t9,Plus]]/Epsilon;
					t9  = t9/fa2;
				];,
				fa1 = fa2 = 1;
				FCPrint[1,"Collect2 at the end "];
				t9 = Collect2[t8, LorentzIndex, Expanding -> False]
			];
			If[ Head[t9] === Times,
				t10 = SelectFree[t9,{Pair,Eps,OPEm}] *
					Collect2[SelectNotFree[t9,{Pair,Eps,OPEm}], {Eps,Pair},
							Factoring -> Factor2],
	(*
				t10 = Collect2[t9, Pair, Factoring -> Factor2]
	*)
				t10 = t9
			];
			If[ Head[t10] === Times,
				t11 = NumericalFactor[t10] mapart[t10/NumericalFactor[t10]];
				t12 = t11 fa1 fa2 /. sunt -> ta;,
				t12 = t9
			];
		(*StringQ*)
		];
		t12
	];

FCPrint[1,"CalculateCounterTerm.m loaded."];
End[]
