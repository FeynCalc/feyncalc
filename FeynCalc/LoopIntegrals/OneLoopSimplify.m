(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OneLoopSimplify *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 January '99 at 1:11 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: OneLoopSimplify *)

(* ------------------------------------------------------------------------ *)

OneLoopSimplify::usage =
"OneLoopSimplify[amp, q] simplifies the one-loop amplitude amp.
The second argument denotes the integration momentum.
If the first argument has head FeynAmp then
OneLoopSimplify[FeynAmp[name, k, expr], k] tranforms to
OneLoopSimplify[expr, k].";

OneLoopSimplify::nivar =
"The integration variable is not found in the integrand.
Please check that the name of the second argument is
correct.";

OneLoopSimplify::numerators =
"OneLoopSimplify failed to eliminate all loop-momentum dependent numerators in
then give loop integral(s). Please examine the output carfeully.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`OneLoopSimplify`Private`"]

Options[OneLoopSimplify] = {Collecting -> False,
							Dimension -> D,
							DimensionalReduction -> False,
							DiracSimplify -> True,
							FinalSubstitutions -> {},
							IntegralTable -> {},
							OPE1Loop -> False,
							ScalarProductCancel -> True,
							SUNNToCACF -> True,
							SUNTrace -> False
							};


OneLoopSimplify[qu_ /; Head[qu]=== Symbol, amp_ /;Head[amp]=!=Symbol,
				opt___Rule] :=
	OneLoopSimplify[amp,qu,opt] /;
	!FreeQ[amp,qu];

OneLoopSimplify[FeynAmp[_, qu_Symbol, expr_], ___, opts___Rule] :=
	OneLoopSimplify[ expr, qu, opts];

OneLoopSimplify[amp_, qu_, opt___Rule] :=
	If[ FreeQ[amp, qu],
		Message[OneLoopSimplify::nivar, qu];
		amp,
		Block[ {q, dim, sunntocacf, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, spc,
		substis, pae, dimred,lt6, lnt6,dirsimplify, nt6,dirsimp,
		ope1loop, integraltable,loopisolate,time},
			q = qu;
			dim = Dimension /. {opt} /. Options[OneLoopSimplify];
			dirsimplify = DiracSimplify/. {opt} /. Options[OneLoopSimplify];
			dimred  = DimensionalReduction /. {opt} /. Options[OneLoopSimplify];
			sunntocacf = SUNNToCACF /. {opt} /. Options[OneLoopSimplify];
			suntrace = SUNTrace /. {opt} /. Options[OneLoopSimplify];
			ope1loop = OPE1Loop /. {opt} /. Options[OneLoopSimplify];
			substis = FinalSubstitutions /. {opt} /. Options[OneLoopSimplify];
			spc = ScalarProductCancel /. {opt} /. Options[OneLoopSimplify];
			integraltable = IntegralTable /. {opt} /. Options[OneLoopSimplify];
			FCPrint[2,"Using dimension ",dim];
			If[ dimred =!= True,
				t1  = Trick[ChangeDimension[FeynCalcInternal[amp], dim]],
				t1  = Trick[FeynCalcInternal[amp]]
			];
			If[ dimred =!= True,
				substis = ChangeDimension[substis, dim]
			];
			t1 = Collect2[Isolate[t1,q,IsolateNames->loopisolate],FeynAmpDenominator[__]];
			time = AbsoluteTime[];
			t1 = FeynAmpDenominatorCombine[t1//Explicit];
			FCPrint[2,"FeynAmpDenominatorCombine done. t1 = ",t1];
			t1 = FeynAmpDenominatorSimplify[Collect2[t1,q,Factoring->False], q,
											IntegralTable -> integraltable];
			t1 = FRH[t1,IsolateNames->loopisolate];
			If[!FreeQ[t1,loopisolate],
				Print["Something went wrong: ", t1];
				Abort[];
			];
			FCPrint[1,"OneLoopSimplify: Time spent on FeynAmpDenominatorSimplify: ", N[AbsoluteTime[] - time, 4]];
			FCPrint[2,"OneLoopSimplify: FeynAmpDenominatorCombine done. t1 = ",t1];
			t1 = Isolate[t1,{SUNIndex,SUNFIndex},IsolateNames->loopisolate]/. DOT->holdDOT/. holdDOT[x__]/;
				!FreeQ2[x,{SUNIndex,SUNFIndex}]:>DOT[FRH[x,IsolateNames->loopisolate]]/. holdDOT->DOT;
			FCPrint[2,"OneLoopSimplify: Doing SUNSimplify on ",t1];
			time = AbsoluteTime[];
			If[ !FreeQ2[t1, {SUNIndex,SUNFIndex}],
				t2 = SUNSimplify[t1,  SUNNToCACF   -> sunntocacf,
									SUNTrace     -> suntrace,
									SUNFToTraces -> False],
				t2 = t1
			];
			t2 = FRH[t2,IsolateNames->loopisolate];
			FCPrint[1,"OneLoopSimplify: Time spent on SUNSimplify: ", N[AbsoluteTime[] - time, 4]];
			FCPrint[1,"OneLoopSimplify: After SUNSimplify: ", t2];
			If[ !FreeQ[t2, DiracGamma],
				t2 = DiracTrick[t2];
				t2 =  t2 /. DiracTrace -> TR
			];
			FCPrint[1,"OneLoopSimplify: After DiracTrick and DiracTrace: ", t2];
			If[ FreeQ2[t2, {DiracGamma, Eps}],
				t3 = Contract3[t2],
				t3 = Contract[t2]
			];
			FCPrint[1,"OneLoopSimplify: After contractions: ", t3];
			t3 = Isolate[Collect2[DiracGammaExpand[t3],DiracGamma[__]],{DiracGamma},IsolateNames->loopisolate]/.
			DOT->holdDOT/. holdDOT[x__]/;
				!FreeQ2[x,{DiracGamma}]:>DOT[FRH[x,IsolateNames->loopisolate]]/. holdDOT->DOT;
			time = AbsoluteTime[];
			If[ (!FreeQ[t3, DiracGamma]) && (dirsimplify === True),
				FCPrint[1,"OneLoopSimplify: Applying DiracSimplify on ", t3];
				t3 = DiracSimplify[Collect2[t3, DiracGamma, Factoring -> False]];
				t3 =  t3 /. DiracTrace -> TR
			];
			If[ (!FreeQ[t3, DiracGamma]) &&  (dirsimplify === True),
				FCPrint[1,"DiracOrdering ", t3];
				t3 = Collect2[t3, DiracGamma, Factoring->False];
				If[ Head[t3] =!= Plus,
					t3 = DiracSimplify[DiracOrder[t3]],
					t3 = Map[DiracSimplify[DiracOrder[#]]&, t3]
				];
				FCPrint[1,"DiracOrdering done: ", t3];
			];
			t3 = FRH[t3,IsolateNames->loopisolate];
			FCPrint[1,"OneLoopSimplify: Time spent on Dirac algebra: ", N[AbsoluteTime[] - time, 4]];
			FCPrint[1,"OneLoopSimplify: After doing Dirac algebra: ", t3];

			t3= Isolate[Collect2[ ExpandScalarProduct[t3], {FeynAmpDenominator, q, OPEDelta}],
				{q, OPEDelta},IsolateNames->loopisolate];
			time = AbsoluteTime[];
			If[ (Collecting /. {opt} /. Options[OneLoopSimplify]) === True,
				t4 = Collect2[ExpandScalarProduct[t3], q, Factoring->Factor],
				t4 = ExpandScalarProduct[t3]
			];
			FCPrint[1,"t4=", t4];
			If[ spc =!= True,
				t5 = t4,
				t5 = ScalarProductCancel[t4, q, FeynAmpDenominatorSimplify->True];
				t5 = ScalarProductCancel[t5, q, FeynAmpDenominatorSimplify->True];
				If[ !FreeQ[t5, DiracGamma],
					dirsimp[z__] :=
						dirsimp[z] = If[ !FreeQ[{z}, DiracGamma],
										DiracSimplify[DOT[z]],
										DOT[z]
									];
					t5 = t5 /. DOT -> dirsimp
				]
			];

			(* Need to reisolate things here *)
			t5 = FRH[t5,IsolateNames->loopisolate];
			t5= Isolate[Collect2[ ExpandScalarProduct[t5], {FeynAmpDenominator, q, OPEDelta}],
				{q, OPEDelta},IsolateNames->loopisolate];

			t5 = Collect2[t5, q, Factoring->Factor];
			FCPrint[1,"t5=", t5];
			FCPrint[1,"\n\n after cancelling scalar products \n\n", t5];




			If[ (!FreeQ[t5, OPEDelta]) && (ope1loop === True),
				t5 = Collect2[ OPE1Loop[q, t5, SUNNToCACF -> sunntocacf,
											SUNFToTraces -> False
									] /. dummyhead->Identity, q,
								Factoring->Factor
						]
			];
			(* XXX *)
			t6 = t5;
			FCPrint[1,"OneLoopSimplify: Time spent on cancelling scalar products: ", N[AbsoluteTime[] - time, 4]];
			time = AbsoluteTime[];


			FCPrint[2,"doing TID on",t6];
			If[ Head[t6] =!= Plus,
				t6 = t6 + null1+null2
			];
			nt6 = 0;
			lnt6 = Length[t6];
			Do[FCPrint[1,"TIDPART ",ij," out of ",lnt6," le = ",Length[nt6]];
			nt6 = nt6 + TID[t6[[ij]], q, ScalarProductCancel -> spc,
							DimensionalReduction -> dimred,
							FeynAmpDenominatorSimplify -> True,
							Isolate -> True,
							Contract -> True,
							Collecting -> True,
				(*Added 7/8-2000, F.Orellana*)
				Dimension -> dim,
				ChangeDimension -> dim
							];
				,{ij, lnt6}
				];
			t6 = nt6 /. {null1 :> 0, null2 :>0};
			FCPrint[1,"OneLoopSimplify: First TID done, expression is now ", t6];
			t6 = Collect2[t6, q, Factoring->False, Expanding->False];
			FCPrint[1,"collect2 after first TID done ",Length[t6]];
			FCPrint[2,"expression is now ",t6];
			If[ Head[t6] === Plus && (!FreeQ[Cases2[t6, Pair], q]),
				lt6 = Length[t6];
				t6  = Sum[FCPrint[1,"TID 2 #" , j," out of " , lt6];
						FCPrint[1,"dimension ",dim, " on: ", StandardForm[SelectNotFree[t6[[j]],q]]];
						tmpres = SelectFree[t6[[j]], q]  *
							TID[SelectNotFree[t6[[j]],q],  q,
								ScalarProductCancel -> spc,
								DimensionalReduction -> dimred,
								FeynAmpDenominatorSimplify -> True,
								Contract -> True,
								Collecting -> True,
						(*Added 7/8-2000, F.Orellana*)
						Dimension -> dim,
						ChangeDimension -> dim
								];
						FCPrint[1,"got: ", tmpres,q];
						tmpres,
				{j, lt6}];
				FCPrint[2,"expression is now ",t6];
				t6 = Collect2[t6, q, Factoring -> Factor];
			];
			FCPrint[2,"expression is now ",t6];
			If[ Head[t6] === Plus && (!FreeQ[Cases2[t6, Pair], q]),
				t6 = FixedPoint[(FCPrint[2,"Applying TID on",#];
								TID[#,q, ScalarProductCancel -> spc,
									DimensionalReduction -> dimred,
									FeynAmpDenominatorSimplify -> True,
									Contract -> True,
									Collecting -> True,
								(*Added 7/8-2000, F.Orellana*)
								Dimension -> dim])&, t6, 3
							];
			];
			FCPrint[1,"TID  returned: ",t6];
			If[ !FreeQ[t6,FeynAmpDenominator],
				t6 = Expand2[t6+null1+null2,FeynAmpDenominator]
			];
			FCPrint[1,"doing TID  done ", t6];
			t6 = t6 /. substis /. {null1 :> 0, null2 :> 0};



			t6 = FRH[t6,IsolateNames->loopisolate];
			FCPrint[1,"OneLoopSimplify: Time spent on tensor reduction: ", N[AbsoluteTime[] - time, 4]];



			If[ !FreeQ[t6,DiracGamma],
				t6 = FRH[t6,IsolateNames->loopisolate];
				t6 = Isolate[Collect2[DiracGammaExpand[t6],DiracGamma[__]],{DiracGamma},IsolateNames->loopisolate]/.
					DOT->holdDOT/. holdDOT[x__]/; !FreeQ2[x,{DiracGamma}]:>DOT[FRH[x,IsolateNames->loopisolate]]/.
					holdDOT->DOT;
				time = AbsoluteTime[];
				FCPrint[1,"collecing after FRH in TID",t6];
				t6 = Collect2[t6, DiracGamma, Factoring -> False];
				FCPrint[1,"collecing after FRH in TID done",t6];
				If[ Head[t6] === Plus,
					t6 = Map[ExpandScalarProduct[DiracOrder[DiracSimplify[#]]]&,
						t6];
					t6 = Collect2[t6, q];
					t6 = ScalarProductCancel[t6, q]
				];
				FCPrint[1,"OneLoopSimplify: Time spent on Dirac algebra: ", N[AbsoluteTime[] - time, 4]];
				t6 = FRH[t6,IsolateNames->loopisolate];
			];


			(* Need to reisolate things here *)
			t6 = FRH[t6,IsolateNames->loopisolate];
			t6= Isolate[Collect2[ ExpandScalarProduct[t6], {FeynAmpDenominator, q, OPEDelta}],
				{q, OPEDelta},IsolateNames->loopisolate];
			time = AbsoluteTime[];
			If[ dimred =!= True,
				t6 = PowerSimplify[ChangeDimension[t6,dim]/.substis],
				t6 = PowerSimplify[t6]/.substis
			];
			FCPrint[1,"OneLoopSimplify: Time spent on PowerSimplify: ", N[AbsoluteTime[] - time, 4]];

			time = AbsoluteTime[];
			t6 = Expand2[t6,q];
			If[ Head[t6] =!= Plus,
				t6 = FeynAmpDenominatorSimplify[t6, q, IntegralTable -> integraltable],
				fdisav[za_] :=
					fdisav[za] = FeynAmpDenominatorSimplify[za,q,
						IntegralTable -> integraltable];
				t6 = Sum[SelectFree[t6[[i]],q] fdisav[fdisav[SelectNotFree[t6[[i]],q]]],
						{i,Length[t6]}];
			];

			FCPrint[1,"OneLoopSimplify: Time spent on FeynAmpDenominatorSimplify: ", N[AbsoluteTime[] - time, 4]];

			t7 = PowerSimplify[Collect2[t6, qu, Expanding->False,
								Factoring -> False]/.substis
								];
			pae[a_,b_] :=
				MemSet[pae[a,b], ExpandScalarProduct[a,b]];
			t7=FRH[t7,IsolateNames->loopisolate];
			If[!FreeQ[t7/.FeynAmpDenominator[___]:>1,q],
				Message[OneLoopSimplify::numerators]
			];
			t7 = t7 /. Pair -> pae /. Power2->Power;
			If[ LeafCount[t7]<1000,
				t7 = Collect2[t7, qu, Expanding->False],
				t7 = Collect2[t7,qu,Factoring->False,Expanding->False];
			];
			t7
		]
	];

FCPrint[1,"OneLoopSimplify.m loaded."];
End[]
