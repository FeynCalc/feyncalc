(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TID *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 4 December '98 at 14:21 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: TID *)

(* ------------------------------------------------------------------------ *)

TID::usage =
"TID[amp, q] does a 1-loop tensor integral decomposition, transforming the
Lorentz indices away from the integration momentum q.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`TID`Private`"]

procanonical[l_][y_,m_] :=
	PropagatorDenominator[y /.
	(-Momentum[l,di___] + a_.) :>
	(Momentum[l,di] - a), m];

Options[TID] = {Collecting -> True,
				Contract -> False,
				Dimension -> D,
				ChangeDimension -> D,
				DimensionalReduction -> False,
				FeynAmpDenominatorCombine -> True,
				FeynAmpDenominatorSimplify -> False,
				Isolate -> False,
				ScalarProductCancel -> False};


(* maybe not ... *)
TID[a_Plus,b__] :=
	Map[TID[#,b]&, a];




TID[FeynCalc`OneLoopSimplify`Private`null1 , q_, opt___Rule]:=
	FeynCalc`OneLoopSimplify`Private`null1;

TID[FeynCalc`OneLoopSimplify`Private`null2 , q_, opt___Rule]:=
	FeynCalc`OneLoopSimplify`Private`null2;

TID[am_ , q_, opt___Rule] :=
	Block[ {n, t0, t1, t2, t3, t4, t5, t6, null1, null2, qrule, mudum,
	nudum, fdp, qQQ, qQQprepare, getfdp,res,nres,irrelevant = 0,
	contractlabel, diditlabel, famp,chd,fds,tid, tidinternal,
	originallistoflorentzindices, dimred, disi, massless=False,nPoint,vanishingMoms,
	vanishingGramDet=False,masses,limitto4=$LimitTo4
	},
		t0 = am;
		(* do some special hack ... *)
		If[ !FreeQ[t0, Polarization],
			If[ Head[t0] === Times,
				If[ MatchQ[SelectNotFree[t0,q],
				Pair[Momentum[q,di_],Momentum[Polarization[ka_,_],di_]]*
				FeynAmpDenominator[PropagatorDenominator[Momentum[q,di_],_].. ,
				PropagatorDenominator[_. Momentum[ka_,di_]+Momentum[q,di_],_]..]
						],
					t0 = 0
				]
			]
		];
		dimred = DimensionalReduction /. {opt} /. Options[TID];
		n = Dimension /. {opt} /. Options[TID];
		contractlabel = Contract /. {opt} /. Options[TID];
		fds = FeynAmpDenominatorSimplify /. {opt} /. Options[TID];
		chd = ChangeDimension /. {opt} /. Options[TID];
		If[ dimred =!= True,
			t5 = ChangeDimension[t5, chd];
			$LimitTo4=False
		];
		(*
			t5 = t5 /. {LorentzIndex[aa_, en_Symbol] :> LorentzIndex[aa],
						Momentum[b_, en_Symbol]      :> Momentum[b]
						};
		*)
		If[ FeynAmpDenominatorCombine /. {opt} /. Options[TID],
			t0 = fspec[FeynAmpDenominatorCombine[t0], q];
		];
		If[ t0 === 0,
			t0,
			originallistoflorentzindices = Cases[t0, LorentzIndex];
			t1 = Uncontract[t0, q, Pair -> All, DimensionalReduction -> dimred,
							(*Added 17/9-2000, F.Orellana*) Dimension -> n] /.
				PropagatorDenominator -> procanonical[q];

			(* RM20110622: Uncommented the above again and commented the below.
			t1 = t0 /.  PropagatorDenominator -> procanonical[q];
			*)
			If[ Head[t1] =!= Plus,
				irrelevant = 0,
				irrelevant = SelectFree[t1, Pair[Momentum[q,n],LorentzIndex[__]]];
				t1 = t1 - irrelevant
			];
			If[ (Collecting /. {opt} /. Options[TID])===True,
				t2 = Collect2[t1, q, Factoring -> False],
				t2 = t1
			];
			If[ Head[t2] =!= Plus,
				t2 = t2 + null1 + null2
			];

			(* ne ne
			fdp[] = 1;
			*)
			fdp[a___,0,b___] :=
				fdp[a,b];
			(* if there are same momenta but different masses *)
			fdp[a___, b_, b_, c___] :=
				fdp[a,b,c]; (* CCC *)
			getfdp[w__] :=
				( (fdp@@(First /@ (
				MomentumCombine[{w},LeafCount -> 1000] /. q->0
						))) /. Momentum[a_,___] :> a
				) /; FreeQ[{w},   PropagatorDenominator[
							fa_ Momentum[q, ___] + _., _]
						];

			(* get the momenta on which the integral depends *)
			qQQprepare[FeynAmpDenominator[any__] f_ /; (!FreeQ[f, Momentum[q,___]])
						] :=
				(FeynAmpDenominator[any] qQQ[getfdp[any] f]
				) /; FreeQ[f, OPEDelta];
			qQQprepare[FeynAmpDenominator[any__] f_ /; (!FreeQ[f, Momentum[q,___]])
						] :=
				(FeynAmpDenominator[any] SelectNotFree[SelectNotFree[f,q],OPEDelta]*
				qQQ[Append[getfdp[any],OPEDelta] *
					f/SelectNotFree[SelectNotFree[f,q],OPEDelta]]
															(* avoid tadpoles *)
				) /; !FreeQ[SelectNotFree[f,q], OPEDelta] && (getfdp[any]=!=1);

			(* no need to uncontract, but then select for q and LorentzIndex here *)
			t3 = Map[(SelectFree[SelectFree[#, Pair[LorentzIndex[__],Momentum[q,___]]],
								FeynAmpDenominator]*
						qQQprepare[SelectNotFree[#, FeynAmpDenominator]*
								SelectNotFree[#, Pair[LorentzIndex[__],Momentum[q,___]]]]
					)&, t2
					] /. {null1 :> 0, null2 :> 0, qQQprepare:>Identity};

			(* Check if the integral has any masses in the propagators *)
			If[MatchQ[t3, _ FeynAmpDenominator[PropagatorDenominator[_, 0] ..]],
				massless=True;
			];

			(* Build a list of masses in the propagators*)

			masses= Replace[t3, _. FeynAmpDenominator[xx__] :> Map[(# /. PropagatorDenominator[_, bb_] :> bb^2) &, {xx}],{0}];
			nPoint = Replace[t3, _. FeynAmpDenominator[props__]:>Length[{props}],{0}];
			If[Length[masses]=!=nPoint,
				FCPrint[0,"Error! The number of legs doesn't match the number of masses:", masses, nPoint];
				Abort[]
			];
			FCPrint[1,"t3=",t3];
			FCPrint[1,"masses: ", masses];
			FCPrint[1,"t3 has no masses in propagators? ", massless];
			tdeclist[{vecs__}, {moms___}] :=
				{{vecs} /. {Pair[LorentzIndex[aa_, nn_], Momentum[bb_, nn_]] :> {bb, aa}}, {moms}};
			vanishingMoms[{moms___}]:=
				(FCPrint[3,Map[ExpandScalarProduct[ScalarProduct[#]]&, {moms}]];
				MatchQ[Map[ExpandScalarProduct[ScalarProduct[#]]& ,{moms}], {0 ..} | {}]);
			breturn[expr_,mom_,dim_]:=
				expr/.{B0[ExpandScalarProduct[ScalarProduct[mom]],m1_,m2_]->
					FeynAmpDenominator[PropagatorDenominator[Momentum[q,dim],PowerExpand[Sqrt[m1]]],PropagatorDenominator[Momentum[q,dim]+
						MomentumExpand[Momentum[mom,dim]],PowerExpand[Sqrt[m2]]]],
						A0[m_]:>FeynAmpDenominator[PropagatorDenominator[Momentum[q,dim],PowerExpand[Sqrt[m]]]]

						};
			pavePrepare[ex_,np_Integer?Positive,{moms__},{ms__}]:=
				Which[np===1 || np===2,
					ex/.FCGV["PaVe"][{nums__}]:>(*PaVeReduce@*)(I Pi^2)PaVe[nums,
						ExpandScalarProduct /@ (ScalarProduct /@ {moms}), {ms}],
					np===3,
					ex/.FCGV["PaVe"][{nums__}]:>(*PaVeReduce@*)(I Pi^2)PaVe[nums,
						ExpandScalarProduct /@ (ScalarProduct /@ {
							{moms}[[1]],
							{moms}[[1]]-{moms}[[2]],
							{moms}[[2]]
						}), {ms}],
					np===4,
					ex/.FCGV["PaVe"][{nums__}]:>(*PaVeReduce@*)(I Pi^2)PaVe[nums,
						ExpandScalarProduct /@ (ScalarProduct /@ {
							{moms}[[1]],
							{moms}[[1]]-{moms}[[2]],
							{moms}[[2]]-{moms}[[3]],
							{moms}[[3]],
							{moms}[[2]],
							{moms}[[1]]-{moms}[[3]]
						}), {ms}]
				]/; (Length[{moms}]+1)===Length[{ms}] && Length[{ms}]===np && np<=4;

			If[Replace[t3, _. qQQ[_. fdp[xx___],___]:>{xx},{0}]=!={},
				FCPrint[1, "Checking Gram determinant..."];
					If[ExpandScalarProduct[Det[(Replace[t3, _. qQQ[_. fdp[xx___],___]:>{xx},{0}])//Table[2 ScalarProduct[#[[i]], #[[j]]], {i, 1, Length[#]}, {j, 1, Length[#]}] &]]===0,
						vanishingGramDet = True
					]
			];
			FCPrint[1,"Vanishing Gram determinants? ", vanishingGramDet];
			(* if there is something to substitute then ... *)
			If[ FreeQ[t3, qQQ],
				res = t3,
				qrule =
				{
				(* general tensor integral, which has some scales, i.e. the Gram determinant doesn't vanish *)
				qQQ[fdp[moms___] (vecs : Pair[LorentzIndex[_, nn_], Momentum[_, nn_]] ..)]/;
				!vanishingGramDet :>
					(FCPrint[3,"General case", massless,vanishingMoms[{moms}]]; Tdec[(Sequence @@ tdeclist[{vecs}, {moms}]), Dimension -> n, List -> False,
					FeynCalcExternal->False]),
				(*
				(* general tensor integral, which is scaleless, i.e. all external momenta
				and masses vanish *)
				qQQ[fdp[moms___] (vecs : Pair[LorentzIndex[_, nn_], Momentum[_, nn_]] ..)]/;
				(massless && vanishingMoms[{moms}]) :>
					(FCPrint[3, t3, "is scaleless (no masses in propagators
						and vanishing external momenta), hence it vanishes in DR."]; 0),
				*)
				(* one-point function of arbitrary rank with non-vanishing mass *)
				(*
				qQQ[fdp[] (vecs : Pair[LorentzIndex[_, nn_], Momentum[_, nn_]] ..)]*
				FeynAmpDenominator[PropagatorDenominator[q,_]]/;
				!massless :>
					Tdec[(Sequence @@ tdeclist[{vecs}, {}]), Dimension -> n, List -> False,
					FeynCalcExternal->False

					],*)
				(* General tensor reduction formulas up to 4-point functions for vanishing Gram determinants *)
				qQQ[fdp[moms___] (vecs : Pair[LorentzIndex[_, nn_], Momentum[_, nn_]] ..)]*
				FeynAmpDenominator[__]/;
				vanishingGramDet :>
					(FCPrint[3,"Trying to handle vanishing Gram determinants"];
					Tdec[(Sequence @@ tdeclist[{vecs}, {moms}]), Dimension -> n, BasisOnly -> True,
					FeynCalcExternal->False]/.FCGV["PaVe"][xx_]:>pavePrepare[FCGV["PaVe"][xx],nPoint,{moms},masses])
				};
				t5 = t3 /. qrule;
				If[t5===t3,
					FCPrint[3,"Nothing matched"!]
				];
				FCPrint[1,"t5=",StandardForm[t5]];
				scsav[a_Momentum,b_Momentum] :=
					scsav[a,b] = ExpandScalarProduct[a,b]//Expand;
				t5 = t5 /. Pair -> scsav /. scsav -> Pair;
				FCPrint[1,"t5=",t5];
				diditlabel = (t5 =!= t4);
				If[ diditlabel === True,
					t5 = t5 /. qQQ -> Identity /. fdp[__] :> 1;
					If[ !FreeQ[t5, LorentzIndex] && contractlabel===True,
						FCPrint[1, "simple contracting  in TID "];
						t5 = Expand2[t5, LorentzIndex] /. Pair -> PairContract /.
							PairContract -> Pair;
					];
					If[ !FreeQ[t5, Eps],
						t5 = EpsEvaluate[t5]
					];
					If[ !FreeQ[t5, LorentzIndex] && contractlabel===True,
						FCPrint[1, "contracting  in TID "];
						t5 = Contract[t5, EpsContract -> False, Rename -> True]
					];
					If[ !FreeQ[t5, DiracGamma],
						FCPrint[1, "DiracSimplify (dotsav) in TID "];
						dotsav[z__] :=
							dotsav[z] = DiracSimplify[DOT[z]];
						t5 = t5 /. DOT -> dotsav
					];,
					t5 = t5 /. qQQ -> Identity /. fdp[___] :> 1;
				];
				res = t5 /. fdp[___] :> 1;
				FCPrint[1,"DIDITLABEL = ",diditlabel];
				If[ diditlabel === True,
					FCPrint[1,"collecting (1)  in TID  "];
					If[ FreeQ[res, DiracGamma],
						res = Collect2[res, q, Factoring -> Factor, Expanding -> False],
						FCPrint[1,"special disi in TID  "];
						disi[z_] := (*disi[z] = *)
							If[ FreeQ[FixedPoint[ReleaseHold,z], q],
								Collect2[DiracSimplify[Collect2[
								ChangeDimension[z,n],DiracGamma,Factoring->False]],
									DiracGamma, Factoring->Factor],
								z
							];
						res = Collect2[res, q, Factoring -> disi, Expanding->False]
					];
					(*
					res = Collect2[res, q, Factoring -> False, Expanding -> False];
					*)
					If[ Isolate /. {opt} /. Options[TID],
						res = Isolate[res, {q, DOT},
											IsolateNames -> tidinternal,
											IsolateSplit -> 4444I]
					];
					If[ !FreeQ[res, DOT],
						res = DiracTrick[res]
					];
					If[ fds,
						res = FeynAmpDenominatorSimplify[res, q]
					];
					If[ (ScalarProductCancel /. {opt} /. Options[TID]) === True,
						FCPrint[1,"ScalarProductCancel in TID "];
						res = ScalarProductCancel[res, q,
												FeynAmpDenominatorSimplify -> fds,
												FeynAmpDenominatorCombine -> False,
												Collecting -> False
												];
						FCPrint[1,"collecting (2)  in TID "];
					(* res = Collect2[res, q, Factoring -> True]; *)
						res = Collect2[res, q, Factoring -> False];
						FCPrint[1,"collecting (2)  in TID done"];
					];
					(*CHANGE Feb. 97 *)
					If[ !FreeQ[res, Pair[Momentum[q, n], Momentum[q, n]]], (*repeat *)
						If[ (ScalarProductCancel /. {opt} /. Options[TID]) === True,
							FCPrint[1,"again (!) ScalarProductCancel in TID "];
							res = ScalarProductCancel[res, q,
													FeynAmpDenominatorSimplify -> fds,
													FeynAmpDenominatorCombine -> False,
													Collecting -> False,(*Added 17/9-2000, F.Orellana*)
							ChangeDimension -> chd
													];
							FCPrint[1,"collecting (3)  in TID "];
							(* res = Collect2[res, q, Factoring -> True]; *)
							If[ !FreeQ[res, DiracGamma],
								res = Collect2[res, q, Factoring -> disi],
								res = Collect2[res, q, Factoring -> False]
							];
						];
					];
					If[ (Head[res] === Plus) && (!FreeQ[res, Pair[Momentum[q, n], _]]), (*repeat *)
						FCPrint[1,"again (XX) ScalarProductCancel in TID "];
						res = SelectFree[res, Pair[Momentum[q,n],_]] +
						(* CHANGE 12/97 *)
								ScalarProductCancel[SelectNotFree[res,Pair[Momentum[q,n],_]],q,
													FeynAmpDenominatorSimplify -> fds,
													FeynAmpDenominatorCombine -> False,
													Collecting -> False,
								(*Added 17/9-2000, F.Orellana*)
								ChangeDimension -> chd
													];
					(*
						TID[Isolate[SelectNotFree[res, Pair[Momentum[q,n],_]],q,
							IsolateNames ->
							tidinternal], q, opt];
					tid /: HoldForm[tid[w__]] := tid[w];
					res = FixedPoint[((# /. tidinternal->tid)/.tid->tidinternal)&, res, 13];
					Clear[tidinternal];
				*)
					];
					If[ !FreeQ[res, tidinternal],
						res = res //. HoldForm[tidinternal[a_]] :> tidinternal[a];
					];
				]
			];
			If[ FeynAmpDenominatorCombine /. {opt} /. Options[TID],
				If[ !FreeQ2[res, (FeynAmpDenominator[xxx__]^_.) *
								(FeynAmpDenominator[xyx__]^_.)
						],
					res = FeynAmpDenominatorCombine[res]
				];
			];
			res = fspec[res, q];
			If[ Head[res]===Plus && !FreeQ[res, FeynAmpDenominator],
				res = SelectNotFree[res, FeynAmpDenominator]
			];
			If[ Cases2[res, LorentzIndex] =!= originallistoflorentzindices,
				res = If[ $VersionNumber>2.2,
						Expand[res,  LorentzIndex],
						Expand2[res, LorentzIndex]
					] /. Pair -> PairContract /. PairContract -> Pair
			];
			(*
			If[Cases2[res, LorentzIndex] =!= originallistoflorentzindices,
				res = Contract[res]
				];
			*)
			If[ dimred === True,
				res = res /. Momentum[aa_,n] :> Momentum[aa]
			];
			If[ (Collecting /. {opt} /. Options[TID])===True,
				res = Collect2[res, q, Factoring -> False]
			];
			$LimitTo4=limitto4;
			irrelevant + res
		]
	];

(* some speciality for on-shell stuff *)
(* in dim. reg. *)

fspec[y_,k_] :=
	y;
(*
fspec[y_,k_] := y /. FeynAmpDenominator :> fadd[k] /. fadd[k] :>
					FeynAmpDenominator;
*)

fadd[k_][PropagatorDenominator[Momentum[k_,  D], 0],
		PropagatorDenominator[Momentum[k_,  D] - Momentum[p_, D], 0]
		] :=
	0 /; Pair[Momentum[p, D], Momentum[p, D]] === 0;

fadd[k_][PropagatorDenominator[Momentum[k_,  D], 0],
		PropagatorDenominator[Momentum[k_,  D] + Momentum[p_, D], 0]
		] :=
	0 /; Pair[Momentum[p,D], Momentum[p,D]] === 0;

(*
somehow this does not work, even though it should

fspec[y_,k_] :=
	y //. {FeynAmpDenominator[PropagatorDenominator[Momentum[k,  D], 0],
							PropagatorDenominator[Momentum[k,  D] -
													Momentum[p_, D], 0]
							] :> 0 /;
						(Pair[Momentum[p,D], Momentum[p,D]] === 0
		};

fspec[y_,k_] :=
	y//. {FeynAmpDenominator[PropagatorDenominator[Momentum[k,  D], 0],
							PropagatorDenominator[Momentum[k,  D] +
												Momentum[p_, D], 0]
							] :> 0 /;
		Pair[Momentum[p, D], Momentum[p, D]] === 0
		};

*)

FCPrint[1,"TID.m loaded."];
End[]
