(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OneLoop *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 January '99 at 20:46 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

CombineGraphs::usage =
"CombineGraphs is an option for OneLoopSum.";

DenominatorOrder::usage =
"DenominatorOrder is an option for OneLoop, if set to True the \
PropagatorDenominator will be ordered in a standard way.";

FinalFunction::usage = "FinalFunction is an option for OneLoopSum.";

ExtraVariables::usage =
"ExtraVariables is an option for OneLoopSum; it may be set \
to a list of variables which are also bracketed out in the result, \
just like B0, C0, D0 and  PaVe.";

OneLoop::usage =
"OneLoop[q, amplitude] calculates the one-loop Feynman diagram \
amplitude (n-point, where n<=4 and the highest tensor rank of the \
integration momenta (after cancellation of scalar products) may be 3; \
unless OneLoopSimplify is used). \n
The argument q denotes the integration variable, i.e., \
the loop momentum. \n
OneLoop[name, q, amplitude] has as first argument a name of \
the amplitude. If the second argument has head FeynAmp then \
OneLoop[q, FeynAmp[name, k, expr]] and \
OneLoop[FeynAmp[name, k, expr]] tranform to OneLoop[name, k, expr].";

OneLoopSum::usage =
"OneLoopSum[ FeynAmp[ ... ], FeynAmp[ ... ] , ...] \
will calculate a list of Feynman amplitudes by replacing \
FeynAmp step by step by OneLoop.";

Prefactor::usage =
"Prefactor is an option for OneLoop and OneLoopSum. \
If set as option of OneLoop, the amplitude is multiplied by \
Prefactor before calculation; if specified as option of OneLoopSum, \
after calculation in the final result as a global factor.";

SelectGraphs::usage =
"SelectGraphs is an option for OneLoopSum indicating that only a \
slected set of graphs of the list provided to OneLoopSum is to \
be calculated. \
Possible settings are: SelectGraphs -> { i, j,  ... } \
or SelectGraphs -> { a, {b, c}, ...  } \
which indicates the graphs to be taken from the list provided \
to OneLoopSum. In the second setting the list {b, c} indicates that \
all amplitudes from b to c should be taken.";

ReduceGamma::usage =
"ReduceGamma is an option of OneLoop. If set to True all \
DiracMatrix[6] and DiracMatrix[7] (i.e. all ChiralityProjector) \
are reduced to Gamma5.";

ReduceToScalars::usage =
"ReduceToScalars is an option for OneLoop  and OneLoopSum that \
specifies whether the result will be reduced to scalar A0, B0, C0 \
and D0 scalar integrals.";

SmallVariables::usage =
"SmallVariables is an option for OneLoop. \
\"SmallVariables->{Melectron}\" i.e. will \
substitute \"SmallVariable[Melectron]\" \
for all Melectron's in the calculation.";

StandardMatrixElement::usage =
"StandardMatrixElement[ ... ] is the head for matrix element abbreviations.";

SetStandardMatrixElements::usage =
"SetStandardMatrixElements[{sm1 -> abb1}, {sm2 -> abb2}, ...]. Set abbreviations \
abb1, abb2, ... for matrix elements sm1, sm2, ... \\ \
SetStandardMatrixElements[{sm1 -> abb1}, {sm2 -> abb2}, ..., cons]. Set abbreviations \
abb1, abb2, ... for matrix elements sm1, sm2, ... using energy-momentum conservation cons, \
e.g. k2 -> p1 + p2 - k1";

OneLoop::failmsg =
"Error! OneLoop has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]


Begin["`OneLoop`Private`"]

oneloopVerbose::usage="";

StandardMatrixElement /:
MakeBoxes[StandardMatrixElement[x_], TraditionalForm
		] :=
RowBox[{"\[LeftDoubleBracketingBar]",TBox[x],"\[RightDoubleBracketingBar]"}];

StandardMatrixElement[0] :=
	0;

	If[ StandardMatrixElement =!= Identity,
		StandardMatrixElement[x_Plus] :=
			Map[StandardMatrixElement,x]
	];

(* ********************************************************************* *)
(*                          oneloop10                                    *)
(* ********************************************************************* *)
	coneins[ x_ ] :=
		MemSet[coneins[x],
		x/.Pair -> PairContract/. PairContract -> Pair];
	dotlin[x_] :=
		DotSimplify[x, Expanding -> False];

(* contractlidef *)
	contractli[x_] :=
		Contract[ x, Expanding->True, Factoring->False,
					EpsContract->False
			] // Expand;
	conall[ x_ ] :=
		Contract[ x,                               (*conalldef*)
		Expanding->True, EpsContract->True, Factoring->False
				] // Expand;

(*
collin[ expr_, varh_, expa_]:= Collect[expr,Cases2[expr,varh]];
*)

collin[ expr_, varh_, expa_] :=
	Block[ {nx,se,i,co,new, null,res,frex,totvarh},
		new = 0;
		nx = expr;
		If[ expa =!= True,
			frex[yy__] :=
				Plus[yy]/;!FreeQ2[{yy}, varh];
			nx = nx/.Plus -> frex
		];
		nx = Expand[nx];
		totvarh = Select[ Variables[ nx ], (!FreeQ2[#,varh])&];
		If[ (Length[nx] < 2142) && (LeafCount[nx]< 10^6),
			res = Collect[ nx, totvarh ],
			For[ i = 1, i <= Length[totvarh], i++,
				co = Coefficient[ nx, totvarh[[i]] ];
				nx = nx - Expand[ co totvarh[[i]] ];
				new = new + totvarh[[i]] co ];
			res = nx + new
		];
		If[ expa =!= True,
			res = res/.frex -> Plus
		];
		res
	];


	spinorchainevaluate[x_] :=
		Expand[DiracOrder[DiracSimplify[x]],q];
(*
	spinorchainevaluate[x_]:=DiracOrder[
		Expand[Contract[ SpinorChainEvaluate[x] ], DiracGamma],{q}
										];
*)

(* ********************************************************************* *)
(*                          oneloop11                                    *)
(* ********************************************************************* *)


Options[OneLoop] = {
					DenominatorOrder           -> False,
					Dimension                  -> D,
					FinalSubstitutions         -> {},
					Factoring                  -> False,
					FCVerbose 					-> False,
					FormatType                 -> InputForm,
					InitialSubstitutions       -> {},
					IntermediateSubstitutions  -> {},
					IsolateNames                -> False,
					Mandelstam                 -> {},
					OneLoopSimplify            -> False,
					PaVeAutoOrder 				-> True,
					PaVeAutoReduce				-> True,
					Prefactor                  -> 1,
					ReduceGamma                -> False,
					ReduceToScalars            -> False,
					SmallVariables             -> {},
					WriteOut                   -> False,
					WriteOutPaVe               -> False,
					Sum                       -> True
				};
(* setting WriteOut to "" retrieves also previously calculated results *)

(* OneLoopdef *)

(*New Jan 1999*)
OneLoop[FeynAmp[name_, k_, expr_], opts___] :=
	OneLoop[name, k, expr, opts];

OneLoop[_, FeynAmp[name_, k_, expr_], opts___] :=
	OneLoop[name, k, expr, opts];

OneLoop[qq_,amp_] :=
	OneLoop[False, qq,amp]/; qq=!=False;

OneLoop[qq_,amp_, opts:OptionsPattern[]] :=
	OneLoop[False, qq,amp,opts]/;
	qq=!=False;

OneLoop[grname_,q_,integ_,opts:OptionsPattern[]] :=
	Block[ {oneamp = FeynCalcInternal[integ],iv,onemandel,
	denf, denorder, denprop, isolatehead,tric,
	var2,smallv,finsubst,fact,qpcancel,pvabbrev,
	writeout,prop,dnq,dfsor,dfsorb, denomOrder,dfli,
	vcid,intcan,de, oneoptga67,vin3, sqB,usual,tostandmat,
	vint4,ret,vret,fmas,vva,smalist,isol,i, $higherpoint,
	pvlist, pva,pvar,arglist,npref, nfact, nre,formattype,pLu,
	prode,
	collpav,simpit,prefactor,in3p, in3q, resin3q,newprefactor,
	newnewprefactor,
	newret, facto,ret2,defs,dim,breakdown,options, name = grname,DDim,
	newoneamp,ip,lenneu, lenneu2, neuamp,paone,paone2,oneselect,fsub,
	intermedsubst,writeoutrecover = False, oldfile, ftemp,
	writeoutpav, uvpart,to4dim, oneampresult, null1, null2,
	oneloopVerbose,nonLoopTerms,loopTerms, oneloopsimplify,inisubs,
	reducegamma67,oneopt,tim,smav, smdaemon, smalldirac
	},

		If [!FreeQ[$ScalarProducts, q],
			Message[OneLoop::failmsg, "The loop momentum " <> ToString[q,InputForm] <> " has scalar product rules attached to it."];
			Abort[]
		];


		If[	$KeepLogDivergentScalelessIntegrals,
			Message[OneLoop::failmsg, "OneLoop does not support the option $KeepLogDivergentScalelessIntegrals!."];
			Abort[]
		];

		If[	$BreitMaison || $Larin,
			Message[OneLoop::failmsg, "OneLoop cannot be used for computations in the BMHV or Larin schemes. Use TID instead."];
			Abort[]
		];

		options = {opts};
		(* for FA2.0 *)
		If[ Length[options] > 0,
			If[ MatchQ[options[[1]] ,(a_List -> b_List)],
				FA2Info = options[[1]];
				options = Rest[options]
			]
		];
		( oneopt    = Join[ options,Options[OneLoop] ];
		onemandel  = Mandelstam/.oneopt;
		denorder   = DenominatorOrder/.oneopt;
		dim        = Dimension/.oneopt;
		If[ dim===True,
			dim = D
		];
		formattype = FormatType/.oneopt;
		isolatehead = IsolateNames/.oneopt;
		oneloopsimplify = OneLoopSimplify/.oneopt;
		prefactor  =  Prefactor/.oneopt;
		qpcancel   = True;
		smallv     =  Flatten[{ SmallVariables/.oneopt }];
		inisubs    =  InitialSubstitutions/.oneopt;
		finsubst   = FinalSubstitutions/.oneopt;
		intermedsubst = IntermediateSubstitutions/.oneopt;
		fact       = Factoring/.oneopt;
		writeoutpav = WriteOutPaVe /. oneopt;
		uvpart = False;
		reducegamma67 = ReduceGamma/.oneopt;
		writeout   = WriteOut/.oneopt;

		paveautoorder = PaVeAutoOrder /. oneopt;
		paveautoreduce = PaVeAutoReduce /. oneopt;
		If[ writeout === True || writeout === " ",
			writeout = "";
			writeoutrecover = False
		];
		If[ writeout === True || writeout === "",
			writeoutrecover = True
		];
		breakdown  = ReduceToScalars/.oneopt;
		If[ (breakdown===True) && ($LimitTo4 === False),
			SetOptions[PaVeReduce, Dimension -> dim]
		];
		);
		If[ uvpart === True,
			breakdown = True;
			$LimitTo4 = True;
			SetOptions[PaVeReduce, Dimension -> True];
			dim = 4
		];
		If[ (breakdown===True) && ( (writeoutpav===False) || (writeoutpav===True) ),
			writeoutpav = ""
		];

		If [OptionValue[FCVerbose]===False,
			oneloopVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				oneloopVerbose=OptionValue[FCVerbose]
			];
		];

		tim = Timing[
		If[ StringQ[ name ] && StringQ[writeout],
			name = StringJoin[writeout, name]
		];
		If[ Head[name]===GraphName,
			name = StringJoin @@ (ToString/@{First[name], Last[name]}),
			If[ (name=!=False) && (Head[name]=!=String),
				name = ToString[name]
			]
		];
		If[ Head[name]===String,
			Which[ formattype === InputForm,   name = StringJoin[name, ".m"],
					formattype === FortranForm, name = StringJoin[name, ".for"],
					formattype === MacsymaForm, name = StringJoin[name, ".mac"],
					formattype === MapleForm,   name = StringJoin[name, ".map"]
			]
		];
		If[ StringQ[name],
		(*Mac fix, 18/9-2000, F.Orellana. Ditto for FileType's below*)
			oldfile = FileType[name];
			If[ oldfile === File,
				FCPrint[1, "oldfile  =", name, FCDoControl->oneloopVerbose];
				ftemp = ( Get[name] );
				If[ ValueQ[OneLoopResult[grname]] && FreeQ[ftemp, FeynAmpDenominator],
					oneamp = OneLoopResult[grname]
				];
			]
		];
		oneamp = ChangeDimension[FeynCalcInternal[oneamp],dim];

		(* in case oneamp has no FeynAmpDenominator: write oneamp out *)
		If[ FreeQ2[ oneamp , {FeynAmpDenominator,FAD}],
			oneampresult = oneamp,

		(* ********************************************************************* *)
		(*                          oneloop12                                    *)
		(* ********************************************************************* *)


		(* * * * ** * * * * * * * * * * * * * * * * * * * * * * * * * * *)
		(* Starting  the game:  *)
		(* * * * ** * * * * * * * * * * * * * * * * * * * * * * * * * * *)

		(*epschisholmdef*)
			epschisholm[x_] :=
				x/;FreeQ[x, Eps] || FreeQ[x, DiracGamma];
			epschisholm[x_Plus] :=
				Map[epschisholm,x];
			epschisholm[x_] :=
				If[ reducegamma67,
					EpsChisholm[x],
					EpsChisholm[x]/.DiracGamma[5] -> (DiracGamma[6]-DiracGamma[7])
				] /; Head[x]=!=Plus;
			If[ FreeQ[oneamp, DOT[Spinor[p1__] , a__ , Spinor[p2__] *
							Spinor[p3__] , b__ , Spinor[p4__]]
					],
				FeynCalc`DiracSimplify`Private`$sirlin = False
			];
			FCPrint[3, "FeynCalc`DiracSimplify`Private`$sirlin = ", FeynCalc`DiracSimplify`Private`$sirlin, FCDoControl->oneloopVerbose];
			(* smallv *)
			smav = Table[smallv[[iv]]->SmallVariable[ smallv[[iv]] ],
						{iv,1,Length[smallv]} ];
		(* do the initial substitutions *)
			oneamp = ( oneamp/.smav )/.inisubs;
		(* neglect any small variable in the numerators of the fermion propagators  *)
			smalldirac /: smalldirac[_] + DiracGamma[a__]:=
				DiracGamma[a];

		(* and in the spinors *)
			smalldirac /: Spinor[ pe_, smalldirac[_], op___]:=
				Spinor[pe,0,op];

			oneamp = oneamp /. SmallVariable -> smalldirac /.
								smalldirac -> SmallVariable;
		(* put heads on the momenta in the denominators *)
			denf[x_,y_] :=
				denprop[Momentum[x],y]/;FreeQ[x,Momentum];
			denf[x_,y_] :=
				denprop[x,y]/;!FreeQ[x,Momentum];
		(* if a propagator has no integration momentum *)
			denprop[a_, b_] :=
				(1/TrickMandelstam[(Pair[a,a]//ExpandScalarProduct)
				- b^2, onemandel] /. SmallVariable[_]->0)/; FreeQ[a, q];
		(* ********************************************************************* *)
		(*                          oneloop13                                    *)
		(* ********************************************************************* *)

		(* ONEAMPCHANGE: denominators *)
			If[ Cases[x, DOT[Spinor[a_,_,_] , (___) , Spinor[b_,_,_] *
										Spinor[c_,_,_] , (___) , Spinor[d_,_,_]
									]
						] =!= {},
				$fourfermion = True
			];

			oneamp = FeynAmpDenominatorSplit[oneamp//Trick//FeynAmpDenominatorCombine, Momentum->{q}];
			oneamp = oneamp /. FeynAmpDenominator[xyz__] :> PropagatorDenominatorExplicit[FeynAmpDenominator[xyz]] /;FreeQ[{xyz}, q];

			oneamp = oneamp/.PropagatorDenominator -> denf /.
					denprop -> PropagatorDenominator;

			If[ oneloopsimplify=== True,
				oneamp = OneLoopSimplify[oneamp, q, Dimension -> dim]
			];
			FCPrint[2, "length of oneamp = ", Length[oneamp], FCDoControl->oneloopVerbose];
			oneamp = FDS[oneamp,q];
			oneamp = FDS[Expand[ApartFF[oneamp,{q}], FeynAmpDenominator],q,FCI->True,ApartFF->False];

			(* reduce N-point to 4-point,  still temporary  *)
			SetAttributes[ Y, Orderless ];
			smn[a_] :=
				0;
			smdaemon[x_] :=
				x/.SmallVariable->smn;
			Y[{pi_, mi_}, {pj_, mj_}] :=
				Expand[smdaemon[ mi^2 + mj^2 -ScalarProduct[pi - pj, pi - pj]//ExpandScalarProduct]];

			(* pli is the list of momenta (without q), being one less than mli *)

			(* ********************************************************************* *)
			(*                          oneloop14                                    *)
			(* ********************************************************************* *)
			(* Reducing N-=point to 4 point *)
			(* ********************************************************************* *)
			gr454[mli_List, pli_List] :=
				gr454[mli, pli] =
				Block[ {pl = Prepend[pli, 0], n = Length[mli]},
	(* create a list of the columns of eq. (4.54) *)
					columnli = Prepend[ Table[ Y[{pl[[i]], mli[[i]]}, {pl[[j]], mli[[j]]}],
												{j,1,n}, {i,1,n}],
										Array[#^0&, n]
										];
					columnli
				];

			(*define this such that sgr[{...},{...}, -1  ] is the Y_{ij} determinant *)
			sgr[mli_, pli_, i_] :=
				sgr[mli, pli, i] =
				Factor2[Drop[gr454[mli, pli], {i+2,i+2}]//Det];
			redamp[x_Plus,qu_] :=
				redamp[#,qu]&/@x;
			redamp[x_,qu_] :=
				Block[ {pl, ml, fm, pp,res = x},
					fd = PartitHead[x, FeynAmpDenominator];
					If[ Length[fd[[2]]] > 4 &&  FreeQ[(x/.FeynAmpDenominator[___]:>1),qu],
						fm[pe_,___] :=
							pe;
						pp[a_, b_] :=
							Expand[(a/.Momentum->fm) - qu];
						mm[a_, b_] :=
							b;
						pl = List @@ (fd[[2]]/.PropagatorDenominator->pp );
						pl = Rest[pl];
						ml =  List @@ (fd[[2]]/.PropagatorDenominator->mm);
(*Extract the determinant *)
						res = {1/sgr[ml, pl, -1] ,
								Sum[ (-1)^(j+1) subdethold[sgr[ml,pl, j]
														] fd[[1]] *
									Drop[fd[[2]], {j+1,j+1}],
									{j, 0, Length[fd[[2]]]-1}
									]
								}
					];
					res
				] /; Head[x] =!= Plus;
			subdethold[x_Plus] :=
				x/;Length[x] < 4;
			subdethold[x_] :=
				ReleaseHold[Isolate[x, IsolateSplit->Infinity,
								IsolateNames->SUB
							] /. SUB -> SUBDET
							];
			$higherpoint = False;
			fdhigh[xx__] :=
				If[ Length[Union[{xx}]] < 5,
					0,
					FeynAmpDenominator[xx]
				];

			If[ !FreeQ[oneamp /. FeynAmpDenominator -> fdhigh, FeynAmpDenominator],
				$higherpoint = True;
				namp = redamp[ oneamp,q ];
				If[ Head[namp]===List,
					prefactor = prefactor namp[[1]];
					oneamp = namp[[2]],
					oneamp = namp
				]
			];



		(* Put here the i pi^2 from the integrals *)
			newprefactor = prefactor I Pi^2;

		(* ONEAMPCHANGE: extract coupling constants *)
			If[ Head[oneamp] === Times,
				oneselect = Select[oneamp,
							FreeQ2[#, {Pair, PropagatorDenominator,Eps,
										dim,
										Momentum,LorentzIndex, SUNF,
										SUNDelta, SUNT,
										DiracGamma, Spinor}
									]&
								];
				oneamp = oneamp/oneselect;
				newprefactor =  Factor2[ newprefactor oneselect ];
			];
			If[ (!FreeQ[ newprefactor, dim ]) || (!FreeQ[newprefactor, LorentzIndex])
				|| (!FreeQ[newprefactor, DiracGamma]),
				newnewprefactor = Select[newprefactor,
										!FreeQ2[#, {dim, LorentzIndex, DiracGamma}]&];
				If[ !FreeQ[newnewprefactor, DiracGamma],
					oneamp = DOT[newnewprefactor , oneamp],
					oneamp = oneamp newnewprefactor
				];
				newprefactor = smalld[newprefactor / newnewprefactor + nUUUl
									]/. nUUUl -> 0;
				If[ newprefactor === 0,
					oneamp = 0
				];
			];

			(* ********************************************************************* *)
			(*                          oneloop15                                    *)
			(* ********************************************************************* *)


			(* change the dimension ,   to4dimdef*)
			to4dim[x_] :=
				x/.{Momentum[v_,_]:>Momentum[v],
					LorentzIndex[w_,_]:>LorentzIndex[w]};
			oneamp0 = oneamp;

			FCPrint[2, "check", FCDoControl->oneloopVerbose];
			(* ONEAMPCHANGE: make dimensions right *)
			If[ dim===4,
				oneamp = SUNSimplify[to4dim[ oneamp ], Explicit -> False],
				oneamp = oneamp + null;
				newone = SUNSimplify[ ChangeDimension[oneamp, dim],
									Explicit -> False ];
				oneamp = newone/.null -> 0
			];
			FCPrint[3, " oneamp = ", oneamp, FCDoControl->oneloopVerbose];
			If[ (!FreeQ[oneamp, SUNF]) || (!FreeQ[oneamp, SUNDelta]) ||
				(!FreeQ[oneamp, SUNT]),
				oneamp = oneamp /. SUNF -> sUNF /. SUNDelta -> sUNDelta;
				AppendTo[finsubst, {sUNF -> SUNF, sUNDelta -> SUNDelta}];
			];

			(* ********************************************************************* *)
			(*                          oneloop16                                    *)
			(* ********************************************************************* *)

			(* for propagators without the integration variable q *)
			dnq[a___,PropagatorDenominator[pe_,ma_],b___] :=
				ExpandScalarProduct[
				(1/Factor2[ smalld[TrickMandelstam[ ExpandScalarProduct[Pair[pe,pe]]-ma^2,
								onemandel ]//Expand] ] ) dnq[a,b] ] /; FreeQ[pe,q];
			prode[a_,b_] :=
				PropagatorDenominator[a//MomentumExpand,b] /; !FreeQ[a,q];
			prode[ppe_,mm_] :=
				ExpandScalarProduct[
				(1/Factor2[ smalld[TrickMandelstam[
									ExpandScalarProduct[Pair[ppe,ppe]]-mm^2,
								onemandel ]//Expand] ] ) ]  /; FreeQ[ppe,q];

		(* ONEAMPCHANGE: extract denominators without q's *)
			oneamp = oneamp/.FeynAmpDenominator->dnq/.dnq->FeynAmpDenominator/.
							PropagatorDenominator->prode;
			dfsor[ve_,ma_] :=
				defs[ma][ve]//MomentumExpand;
			dfsorb[a_][b_] :=
				PropagatorDenominator[b,a];
		(* denomOrder orders the PropagatorDenominator         , denomOrderdef *)
		(* the eventually necessary translation of q is done with intcan.       *)
			denomOrder[ a__ ] :=
				Block[ { dfli = {a},ru },
(* this is checked always *)
					ru = {PropagatorDenominator[c_. Momentum[ce_. q ,di___],ma0_]:>
						PropagatorDenominator[Sqrt[c^2] Momentum[Sqrt[ce^2] q,di],ma0],
						PropagatorDenominator[x_+be_ Momentum[q,di___],mA_] :>
						PropagatorDenominator[Expand[-x+Momentum[q,di] ],mA]/;(be===-1)
						};
					dfli = dfli//.ru;
					If[ !MatchQ[ First[dfli], PropagatorDenominator[Momentum[q,___],_] ],
						denorder = True
					];
					If[ denorder === True,
						dfli = Sort[ dfli/.PropagatorDenominator->dfsor ]/.defs->dfsorb;
				(* For boxes: bring a evtl. 0 at position 3 *)
						If[ Length[ dfli ] === 4,
							dfli = RotateRight[dfli,2]
						]
					];
					If[ Length[dfli]===3 && denorder === True,
						dfli = dfli/.{den1_, PropagatorDenominator[pe2_,ma1_],
												PropagatorDenominator[pe3_,ma2_]}:>
										{den1,  PropagatorDenominator[pe3,ma2],
												PropagatorDenominator[pe2,ma1]}/;
										Length[ pe2 ] > Length[ pe3 ]
					];
					If[ denorder === True,
						dfli = dfli/.{den1_,PropagatorDenominator[pe2_,ma2_],den3_,
											PropagatorDenominator[pe3_,ma2_]}:>
										{den1, PropagatorDenominator[pe3,ma2],den3,
											PropagatorDenominator[pe2,ma2]}/;
											Length[pe2]>Length[pe3];
						FCPrint[3, "after denomOrdering : ", dfli, FCDoControl->oneloopVerbose];
					];
					FeynAmpDenominator@@dfli
				](* endBlock *);
		(* ********************************************************************* *)
		(*                          oneloop17                                    *)
		(* ********************************************************************* *)
			vcid[pe_,___] :=
				pe;
		(* for translating the integration variable q in the first propagator *)
		(* intcandef *)
			intcan[x_] :=
				x/;FreeQ[x,q];
			intcan[x_Plus] :=
				intcan/@x;
			intcan[ a_ b_ ] :=
				a intcan[b]/; FreeQ[a,q];
			intcan[x_Times] :=
				(SelectFree[x, q] intcan[SelectNotFree[x,q]]) /;
				SelectFree[x, q] =!= 1;
			intcan[any_. FeynAmpDenominator[
						PropagatorDenominator[p_ + Momentum[q,dim], m1_],dfrest___ ]
				] :=
				denomExpand[ ( any FeynAmpDenominator[
					PropagatorDenominator[ p + Momentum[q,dim],m1], dfrest]
							)/.q->( q - (p/.Momentum->vcid) )
							];
		(*  bringing the denominator in a canonical form                         *)

		(* ONEAMPCHANGE : fixing all denominators , evtl. ordering *)
			oneamp = denomExpand[ oneamp ];
			oneamp = denomExpand[ oneamp ]/.FeynAmpDenominator->denomOrder;
			oneamp = (intcan[oneamp//MomentumExpand]//MomentumExpand)/.
					intcan->Identity;
			oneamp = Map[ (Numerator[#]/Factor2[Denominator[#]])&, oneamp + null ];
			oneamp = oneamp/.null->0;
			FCPrint[3, "oneamp after ordering = ", oneamp, FCDoControl->oneloopVerbose];

		(* ONEAMPCHANGE : bringing all denominator in standard order *)
			consum[x_] :=
				conall[x]/;Head[x]=!=Plus;    (* consumdef *)
			consum[x_Plus] :=
				Block[ {nx = 0,i},
					For[i = 1, i<=Length[x], i++,
						FCPrint[2, "contracting # ", i, " out of ", Length[x], FCDoControl->oneloopVerbose];
						nx = nx + conall[x[[i]]]
						];
					nx
				];
			(* consum2def *)
			consum2[x_] :=
				smalld[ExpandScalarProduct[conall[x]]]/;Head[x]=!=Plus;
			consum2[x_Plus] :=
				Block[ {nx = 0},
					For[i = 1, i<=Length[x], i++,
						FCPrint[2, "contracting # ", i, " out of ", Length[x], FCDoControl->oneloopVerbose];
						nx = nx + (Expand[
							ExpandScalarProduct[conall[x[[i]]]]]//smalld)
						];
					nx
				];
		(* ********************************************************************* *)
		(*                          oneloop18                                    *)
		(* ********************************************************************* *)
			oneamp = Contract[ oneamp, Expanding -> False];
			oneampc = oneamp;
			oneamp = DiracSimplify[oneamp, Expanding -> False];
			oneampd = oneamp;
			FCPrint[2, "OneLoop: before collin ", oneamp, FCDoControl->oneloopVerbose];
			oneamp = collin[ consum[oneamp]//smalld, FeynAmpDenominator, False ];
		(* ONEAMPCHANGE : contracting Lorentz indices and expanding *)
		(* now a canonized form is achieved *)
		(* and all Lorentzindices which are not part of a DiracGamma contracted *)
			If[ !FreeQ[oneamp,DiracGamma],
				FCPrint[1, "Simplification of Dirac structures", FCDoControl->oneloopVerbose]
			];
			If[ !FreeQ[oneamp,DiracTrace],
				FCPrint[1, "and calculation of Traces", FCDoControl->oneloopVerbose]
			];

		(* ONEAMPCHANGE : contracting Lorentz indices and trace calculation *)
		(*oneampe = oneamp;*)
			If[ !FreeQ[oneamp, DiracTrace],
				neuamp = 0;
				oneamp = collin[ oneamp, DiracTrace, False];
				If[ Head[oneamp] =!= Plus,
					oneamp = Expand[oneamp /. DiracTrace->TR],
					For[i = 1, i<=Length[oneamp], i++,
						FCPrint[1, "calculating trace # ", i, "  out of ", Length[oneamp], FCDoControl->oneloopVerbose];
						timi = Timing[
										neuamp = neuamp + Expand[ oneamp[[i]]/.DiracTrace->TR]
									][[1]];
						FCPrint[1, "time needed = ", timi, FCDoControl->oneloopVerbose];
						];
					oneampf = oneamp;
					oneamp = Collect2[neuamp, FeynAmpDenominator, Factoring -> False]
				]
			];
			(*oneampzero = oneamp;*)

		(* ONEAMPCHANGE : bringing gamma5, gamma6 and gamma7 into standard
			way according to the setting of the option ReduceGamma
		*)
			If[ !reducegamma67,
				oneamp = oneamp /.
				{DOT[ Spinor[p1__],(a___),Spinor[p2__]] :>
					(DOT[Spinor[p1],a,DiracGamma[6],Spinor[p2]] + DOT[Spinor[p1],a,DiracGamma[7],Spinor[p2]])};
			];
			If[ reducegamma67,
				oneamp = oneamp /. DiracGamma[6] -> (1/2 + DiracGamma[5]/2)/. DiracGamma[7] -> (1/2 - DiracGamma[5]/2)
			];
		(* ********************************************************************* *)
		(*                          oneloop19                                    *)
		(* ********************************************************************* *)
			FCPrint[1, "contraction, etc. ", FCDoControl->oneloopVerbose];
			diracorder[x_] :=
				x /; FreeQ[ x, DiracGamma ];
			diracorder[x_] :=
				Collect2[DiracOrder[x(*, {q}*)], DOT,Factoring->False
						] /; Head[x]=!=Plus;
			diracorder[x_Plus] :=
				Collect2[ Map[DiracOrder[#(*, {q}*)]&, x], DOT,
							Factoring->False];
			$Test = True;
			paex[x_,y_] :=
				If[ !FreeQ[{x,y}, Eps],
					ExpandScalarProduct[Pair[x,y]],
					Pair[x,y]
				];
			epsit[x_] :=
				If[ $Test === True,
					epschisholm[Collect2[x/.Pair->paex, {Eps,Spinor},
													Factoring -> False ]],
					x
				];
			simpit[x_ /; FreeQ[x, DiracGamma]] :=
				(FCPrint[3, "checkkkk", FCDoControl->oneloopVerbose];
				consum2[x]);
			simpit[x_] :=
				(FCPrint[3, "OneLoop: simpit", FCDoControl->oneloopVerbose];
				diracorder[ Collect2[ consum2[x]//epsit, DOT,
									Factoring->False
								] // DiracSimplify]) /; !FreeQ[x, DiracGamma];

		(* we want to keep the distinction in different graphs *)
		(* ONEAMPCHANGE *)
		(*oneamp1 = oneamp; *)
			FCPrint[1, "oneamp = ", oneamp, FCDoControl->oneloopVerbose];
			neuamp = 0;
			If[ Head[oneamp] === Plus,
				lenneu = Length[oneamp],
				lenneu = 1
			];
			For[ ip = 1, ip <= lenneu, ip++,
				FCPrint[2, "working with part # ", ip, " out of ", lenneu, FCDoControl->oneloopVerbose];
				If[ lenneu > 1,
					parf = PartitHead[oneamp[[ip]], FeynAmpDenominator]//smalld,
					parf = PartitHead[oneamp, FeynAmpDenominator]//smalld
				];
				neuamp = neuamp + ((Expand[ If[parf[[2]]=!=0,parf[[2]],1] simpit[parf[[1]]]
											]) //smalld)
				];
			FCPrint[2, "Length of neuamp = ", Length[neuamp], FCDoControl->oneloopVerbose];
			If[ FreeQ[neuamp, DOT],
				oneamp = neuamp,
				oneamp = Collect2[neuamp, DOT, Factoring->False];
			];
			oneamp2 = oneamp;
			FCPrint[3, "oneamp2 = ", oneamp2, FCDoControl->oneloopVerbose];

(* ONEAMPCHANGE *)
			If[ intermedsubst =!= {},
				oneamp = oneamp /. intermedsubst /. intermedsubst;
				neuamp = 0;
				If[ Head[oneamp] === Plus,
					lenneu = Length[oneamp],
					lenneu = 1
				];
				For[ iip = 1, iip <= lenneu, iip++,
					FCPrint[2, "working with (substituted) part # ", iip, " out of ", lenneu, FCDoControl->oneloopVerbose];
					If[ lenneu > 1,
						parf = PartitHead[oneamp[[iip]], FeynAmpDenominator],
						parf = PartitHead[oneamp, FeynAmpDenominator]
					];
					neuamp = neuamp + ((Expand[ parf[[2]] simpit[parf[[1]]]
												]) //smalld)
					];
			];
			oneamp = neuamp;
			FCPrint[2, "check1", FCDoControl->oneloopVerbose];
			FCPrint[3, "neuamp = ", neuamp, FCDoControl->oneloopVerbose];

		(* Zwischenspiel... *)
			smadot[] = 1;
			standard /: standard[a_] standard[b_] := standard[a b];
			smadot[x___] :=
				standard[ dotlin[DOT[x]]/.dim->4 ] /.
				standard -> StandardMatrixElement;
			If[ StandardMatrixElement =!= Identity,
				StandardMatrixElement[x_Plus] :=
					Map[StandardMatrixElement,x]
			];

		(* ********************************************************************* *)
		(*                          oneloop20                                    *)
		(* ********************************************************************* *)

		(* tostandmatdef *)
			tostandmat[xy_] :=
				Block[ {te, standa},
					standa[a__] :=
						dotlin[DOT[a]] /;
						!FreeQ2[{a}, {DOT,Pair,Spinor}];
					standa[a_,b__] :=
						StandardMatrixElement[a,b]/;
						FreeQ2[a, {DOT,Pair,Spinor}];
					te = tempstandmat[Expand[xy]]//Expand;
					If[ !FreeQ[te, DiracGamma[6]],
						te = te/.StandardMatrixElement -> standa/.
												standa -> spinorsandpairs/.
												spinorsandpairs->smadot/.
												DiracGamma[6] ->
						(1/2 + DiracGamma[5]/2);
						te = spinorchainevaluate[te]//Expand;
						te = tempstandmat[te]//Expand
					];
					If[ !FreeQ[te, DiracGamma[7]],
						te = te/.StandardMatrixElement -> standa/.
												standa -> spinorsandpairs/.
												spinorsandpairs->smadot/.
												DiracGamma[7] ->
						(1/2 - DiracGamma[5]/2);
						te = DiracSimplify[te]//DiracOrder//Expand;
						te = tempstandmat[te//Contract]//Expand
					];
					te = te /.DOT->spinorsandpairs/.
							spinorsandpairs->smadot;
					te
				] /; reducegamma67 === True;
			tostandmat[xy_] :=
				Block[ {te, standa, pluh, pluh2},
					standa[xx_] :=
						xx/;!FreeQ2[xx,
							{DOT,Pair,Polarization}];
					standa[a_,b__] :=
						StandardMatrixElement[a,b];
					pluh[x__] :=
						Plus[x] /; !FreeQ2[{x},
						{Spinor,GellMannMatrix, SUNIndex,
						Polarization,Pair}];
					te = xy /. Plus -> pluh /. pluh->pluh2;
					te = tempstandmat[Expand[te]]//Expand;
					te = te /. pluh2 -> Plus;
					te = te/.StandardMatrixElement -> standa/.
						DiracGamma[5] ->
						(DiracGamma[6] - DiracGamma[7]);
					te = te//dotlin;
			(* 1 = gamma6 + gamma7 *)
					If[ !FreeQ[te, Spinor],
						te = te //. {
						DOT[
						Spinor[p1__],(a___),Spinor[p2__] ]:>
						(DOT[Spinor[p1],a,DiracGamma[6],Spinor[p2]] +
							DOT[Spinor[p1],a,DiracGamma[7],Spinor[p2]]
						) /; FreeQ2[{a}, {DiracGamma[6],
											DiracGamma[7]} ]
											};
						te = Expand[DiracSimplify[te]//DiracOrder,
									Spinor]//Contract;
						te = Expand[ te, Pair ]
					];
					te = tempstandmat[te];
					te = te /.DOT->spinorsandpairs/.
								spinorsandpairs->smadot;
					te
				] /; reducegamma67 =!= True;

		(* ********************************************************************* *)
		(*                          oneloop21                                    *)
		(* ********************************************************************* *)

		(* ONEAMPCHANGE : spinor stuff and matrixelements *)
			FCPrint[2, " Dirac-Algebra again", FCDoControl->oneloopVerbose];
			FCPrint[2, "before spinorch: oneamp = ", oneamp//Length, FCDoControl->oneloopVerbose];
		(*
			oneamp =  conall[spinorchainevaluate[oneamp//smalld ]]//
					ExpandScalarProduct;
		*)
			If[ Head[oneamp]===Plus,
				FCPrint[2, "substituting", FCDoControl->oneloopVerbose];
				If[ Length[oneamp]<10,
					oneamp = oneamp/.{DiracGamma[Momentum[pe_,di_Symbol],di_Symbol]:>
										DiracGamma[Momentum[pe]]/;FreeQ[pe,q] };
					oneamp =  conall[oneamp//smalld ]// ExpandScalarProduct,
					newamp = 0;
					For[ij = 1, ij <= Length[oneamp], ij++,
						If[ IntegerQ[ij/100],
							FCPrint[2, "ij = ", ij, FCDoControl->oneloopVerbose]
						];
						temp =  (oneamp[[ij]]/.
							{DiracGamma[Momentum[pe_,di_Symbol],di_Symbol]:>
							DiracGamma[Momentum[pe]]/;FreeQ[pe,q] });
						temp = conall[temp//smalld] // ExpandScalarProduct;
						newamp = newamp + Expand[temp, q]
						];
					oneamp = newamp
				];
			];

		(*
			oneamp = Collect2[oneamp/.subdethold->Identity, q, Factoring-> False];
		*)
			FCPrint[2, "collect w.r.t. ", q, FCDoControl->oneloopVerbose];
			oneamp = Collect2[oneamp, q, Factoring -> False];
			FCPrint[2, "oneamp ", oneamp, FCDoControl->oneloopVerbose];

		(* ********************************************************************* *)
		(*                          oneloop22                                    *)
		(* ********************************************************************* *)


			If[ (oneamp =!= 0),
				(*This is a good point to isolate possible non-loop terms in the input expression *)
				oneamp=Collect2[oneamp,q];
				nonLoopTerms = Select[oneamp+ null1+ null2, FreeQ[#, q]&]/. {null1|null2 -> 0};
				loopTerms = Select[oneamp+ null1+ null2, !FreeQ[#, q]&]/. {null1|null2 -> 0};
				If[loopTerms+nonLoopTerms=!=oneamp,
					FCPrint[1,"Splitting the expression " <> ToString[oneamp,InputForm] <>
					"into loop and non-loop pieces in OneLoop failed."];
					Abort[]
				];
				oneamp=loopTerms;

		(* ONEAMPCHANGE : cancelling q p 's *)
				If[ qpcancel === True,
					FCPrint[1, "cancelling qp's", FCDoControl->oneloopVerbose];
					qpcanc[b_,qu_] :=
						Select[null1 + null2 +
						Collect2[ApartFF[b,{qu}]//smalld, qu],
						!FreeQ[#,FeynAmpDenominator]&];
					oneamp = qpcanc[qpcanc[oneamp, q],q];
					FCPrint[1, "OneLoop: cancelling qp's done, oneamp= ",oneamp, FCDoControl->oneloopVerbose];
					If[ !FreeQ[oneamp,Pair[Momentum[q,_:4],Momentum[q,_:4]]],
						FCPrint[0,"Something went wrong in the cancelling of scalar products. Evaluation aborted!"];
						Abort[]
					];

				];
				(* order the denominators again *)
				If[ denorder === True,
					oneamp = denomExpand[ oneamp ]/.FeynAmpDenominator->denomOrder
				];
				oneamp = (intcan[oneamp//MomentumExpand]//ExpandScalarProduct)/.
						intcan->Identity;
			];
		(*oneamp1 = oneamp;*)
			FCPrint[1, "simplifying again in ", oneamp, FCDoControl->oneloopVerbose];
			If[ !FreeQ[oneamp, Spinor],
				oneamp = Map[spinorchainevaluate, oneamp +  nUUl]/.nUUl->0;
				oneamp = Expand[ ExpandScalarProduct[ oneamp ], q]
			];
			FCPrint[1, "collecting w.r.t.", q, "  ", Length[oneamp], " terms", FCDoControl->oneloopVerbose];
			oneamp = FixedPoint[ ReleaseHold, smalld[oneamp] ];
			timecoll = $FactorTime;
			$FactorTime = 360;
			tim = Timing[

			If[ $ToughStuff =!=True,
				oneamp = Collect2[ oneamp, q ],
				oneamp = Collect2[ oneamp, q, Factoring -> False];
			];
			oneamp2 = oneamp;
			If[ Head[oneamp] === Plus,
				newoneamp = 0;
				lenneu2 = Length[oneamp];
				If[ dim=!=4,
					qdi = {q,dim},
					qdi = {q}
				];
				For[ii = 1, ii<=lenneu2, ii++,
					FCPrint[2, "isolating; ii  = ", ii, " out of ", lenneu2, FCDoControl->oneloopVerbose];
					newoneamp = newoneamp + Isolate[oneamp[[ii]], qdi,
													IsolateNames ->fFC,
													IsolateSplit->Infinity]
					];
				oneamp = newoneamp
			];
					];
			$FactorTime =  timecoll;
			FCPrint[1, "time for collect2  and isolate : ", tim[[1]]//FeynCalcForm, FCDoControl->oneloopVerbose];
			FCPrint[1, "isolated and collected, before tensorintegraldecomposition:/n
				length of isolated graph = ", Length[oneamp], FCDoControl->oneloopVerbose];
			FCPrint[1, "memory in use = ", MemoryInUse[], FCDoControl->oneloopVerbose];
			FCPrint[3, "oneamp = ", oneamp, FCDoControl->oneloopVerbose];

		(* ONEAMPCHANGE : tensorintegraldecomposition *)
		(* relax this here. RM 20110622 *)
		(*
			If[!FreeQ2[oneamp, {Pair[x_,y_Plus] , DiracGamma[x_Plus,___]}],
		*)
			FCPrint[1, "cheCK", FCDoControl->oneloopVerbose];
			oneamp = DiracSimplify[oneamp]//ExpandScalarProduct;
	(*
		];
	*)

	(* bug (found by Christian Bauer, 07/97)
		fixed here for objects like eps[al,nu,si,p-q]  (can occur
		after canonicalization of denominators; need then to expand the eps;
		this is done with EpsEvaluate)
	*)
			If[ !FreeQ[oneamp, Eps],
				oneamp = Collect2[EpsEvaluate[oneamp],q],
(* add this here for safety, since otherwise tensint might now work correctly. RM 20110622*)
				oneamp = Collect2[oneamp,q]
			];

		(*XXX*)
			sirlinsave = FeynCalc`DiracSimplify`Private`$sirlin;
			FeynCalc`DiracSimplify`Private`$sirlin = False;
			FCPrint[1, "q = ", q, FCDoControl->oneloopVerbose];
			oneamp = tensint[ oneamp,dim,q,{Mandelstam->onemandel} ];
			oneamp = oneamp /.{B1 :> bB1, B00 :> bB00, B11 :> bB11};
			oneamp = oneamp//smalld;
			oneamp = oneamp /.{bB1 :> B1, bB00 :> B00, bB11 :> B11};
			FeynCalc`DiracSimplify`Private`$sirlin = sirlinsave;
			oneamp = FixedPoint[ReleaseHold, oneamp];
			FCPrint[1, "after tensint ", FCDoControl->oneloopVerbose];
			FCPrint[3, "after tensint : oneamp = ", oneamp, FCDoControl->oneloopVerbose];
			If[ !FreeQ[oneamp, Spinor],
				oneamp = Collect2[oneamp, Spinor, Factoring -> False],
				If[ !FreeQ[oneamp, Pair],
					oneamp = Collect2[oneamp, {Pair, Polarization},
										Factoring -> False],
					oneamp = Expand[oneamp]
				]
			];

		(* StandardMatrixElement[ Spinor[] ... ] -> Spinor[] ... *)
			standback[x_] :=
				x /; !FreeQ[ x, Spinor ];
			oneamp = oneamp /. StandardMatrixElement -> standback /.
								standback -> StandardMatrixElement;
			oneampsave = oneamp;
			FCPrint[3, "oneampsave = ", oneampsave, FCDoControl->oneloopVerbose];

			(* ********************************************************************* *)
			(*                          oneloop23                                    *)
			(* ********************************************************************* *)
			FCPrint[1, "after collecting ", FCDoControl->oneloopVerbose];

		(* ONEAMPCHaNGE : spinor stuff and matrixelements *)
			If[ (!FreeQ[ oneamp,Spinor ]),
				If[ Length[oneamp]>42,
					oneamp = Isolate[ oneamp, {Spinor, DiracGamma, Pair},
									IsolateNames-> fFCT,
									IsolateSplit -> Infinity];
				];
				FCPrint[1, "length of oneamp now: ", Length[oneamp], FCDoControl->oneloopVerbose];
				If[ (reducegamma67 === True) ||
					(!FreeQ[oneamp,
							DOT[Spinor[p1__] , a___ , Spinor[p2__]] *
							DOT[Spinor[p3__] , b___ , Spinor[p4__]]
							]),
					oneamp = oneamp/. DiracGamma[7]->(1/2 - DiracGamma[5]/2)/.
									DiracGamma[6]->(1/2 + DiracGamma[5]/2);
				];
				oneamp = Expand[DiracSimplify[oneamp],DOT]//DiracOrder//
				ExpandScalarProduct;
				oneamp = DiracSimplify[oneamp];
				If[ (reducegamma67 =!= True)  &&
					(!FreeQ[oneamp,
							DOT[Spinor[p1__] , a___ , Spinor[p2__]] *
							DOT[Spinor[p3__] , b___ , Spinor[p4__]]
						]),
					oneamp = oneamp /.
					{DOT[ Spinor[p1__],(a___),Spinor[p2__]] :>
						(DOT[Spinor[p1],a,DiracGamma[6],Spinor[p2]] +
						DOT[Spinor[p1],a,DiracGamma[7],Spinor[p2]]
						)
					};
					oneamp = DiracSimplify[oneamp]
				];
				If[ $higherpoint===False,
					oneamp = FixedPoint[ReleaseHold, oneamp]
				];
				FCPrint[1, "after spinorchainevaluate", FCDoControl->oneloopVerbose]
			];
			oneamp = oneamp /.{B1 :> bB1, B00 :> bB00, B11 :> bB11};
			oneamp = oneamp//smalld;
			oneamp = oneamp /.{bB1 :> B1, bB00 :> B00, bB11 :> B11};

		(* If something changed, we have to order again *)
			If[ oneamp =!= oneampsave,
				If[ !FreeQ[oneamp, Spinor],
					oneamp = Collect2[oneamp, Spinor, Factoring -> False],
					If[ !FreeQ[oneamp, Pair],
						oneamp = Collect2[oneamp, {Pair, Polarization},
											Factoring -> False],
						oneamp = Expand[oneamp]
					]
				]
			];
			FCPrint[1, "after Collecting ", FCDoControl->oneloopVerbose];
			standmatd[xxx__] :=
				StandardMatrixElement[dotdotlin[xxx]];
			standmatd[] = 1;
			(* this may take a lot of time ... *)
			If[ (StandardMatrixElement =!= Identity),
				If[ !FreeQ2[ oneamp, {DOT, DiracGamma, Polarization,
									SUNF, SUNDelta, SUNT
									} ] && FreeQ[ oneamp, Spinor ],
					If[ Head[oneamp]===Plus,
						oneamp = Sum[ (oneamp[[iii]] spip[])/.
										spip -> spinorsandpairs /.
										spinorsandpairs -> standmatd,
										{iii,1,Length[oneamp]}],
						oneamp = Expand[oneamp spip[], Polarization]/.
									spip->spinorsandpairs /. spinorsandpairs ->
									standmatd
					]
				]
			];
			If[ (StandardMatrixElement =!= Identity) &&
			(!FreeQ2[oneamp, {Spinor, Polarization, SUNIndex}]),
				FCPrint[1, "before tostandmat", FCDoControl->oneloopVerbose];
				If[ Head[oneamp] === Plus,
					paone = Select[oneamp, !FreeQ2[#,
											{Spinor, Polarization, SUNIndex}]&];
					paone = {oneamp - paone, paone},
					paone = {0, oneamp}
				];
				paone2 = Isolate[Collect2[paone[[2]], {Spinor,Polarization,SUNIndex},
											Factoring -> False],
											{Spinor,Polarization,SUNIndex},
											IsolateNames -> tempFC,
											IsolateSplit->Infinity];
				oneamp = FixedPoint[ReleaseHold, tostandmat[paone2] ] + paone[[1]];
				FCPrint[1, "after tostandmat", FCDoControl->oneloopVerbose];
				FCPrint[3, "after tostandmat : oneamp = ", oneamp, FCDoControl->oneloopVerbose];
			];
		(* ********************************************************************* *)
		(*                          oneloop25                                    *)
		(* ********************************************************************* *)


		(* ONEAMPCHANGE : spinor stuff and matrixelements *)
			If[ !FreeQ[oneamp, StandardMatrixElement],
				FCPrint[1, "collecting w.r.t. standard matrixelements ", FCDoControl->oneloopVerbose];
				oneamp = collin[oneamp/.inisubs, StandardMatrixElement, True
								];
				FCPrint[1, "collecting done", FCDoControl->oneloopVerbose];
			];

		(* ONEAMPCHANGE : inserting the subdeterminants again *)
			If[ !FreeQ[ oneamp, SUBDET ],
				oneamp = oneamp /. SUBDET -> SUB;
				FCPrint[1, "subdeterminants reinserted", FCDoControl->oneloopVerbose]
			];
			tric[y_Plus] :=
				tric /@ y;
			tric[x_] :=
				TrickMandelstam[x, onemandel]/;Length[onemandel]>0;
			tric[x_] :=
				x/;onemandel==={};
			fma[xx_] :=
				True/;Head[xx]===StandardMatrixElement;
			vva[xx_] :=
				True/;!FreeQ[{xx},PaVe];
			If[ LeafCount[oneamp]<1000,
				voneampsma =  Variables[oneamp]/.SmallVariable->Identity;
				smalist = Select[ voneampsma, fma ];
				pvlist = Select[ voneampsma, vva ]//Union;
				arglist = Union[pvlist/.PaVe->pvar],
				arglist = {}
			];
			collpav[x_Symbol] :=
				x;
			collpav[a_StandardMatrixElement b_] :=
				a collpav[b];
			collpav[x_] :=
				tric[ Collect2[x,{A0,B0,B1,B00,B11,C0,D0,PaVe},
						Factoring -> True] ];
		(* ********************************************************************* *)
		(*                          oneloop26                                    *)
		(* ********************************************************************* *)

		(* substituting the final substitutions *)
			fsub[x_, su_] :=
				Block[ {nx = x,ij},
					For[ij = 1, ij<=Length[su], ij++,
						nx = nx/.su[[ij]]
						];
					nx
				];
			oneamp = fsub[ oneamp, finsubst ];
			If[ !FreeQ[oneamp, SUNIndex],
				oneamp = SUNSimplify[oneamp, Explicit -> True];
			];

		(* getting a common factor *)
			oneamp = oneamp + nUl1 + nUl2;
			If[ fact===True,
				factor3[x_] :=
					Factor2[x, FactorFull -> False];
				npref[0] = 0;
				npref[w_ v_] :=
					(factor3[w]/.Plus->pluS) npref[v]/;
					FreeQ2[w,{A0, B0,B1, C0, D0, B00, B11, PaVe}];
				oneamp = factor3[(npref /@ Map[factor3,oneamp ])/.nUl1->0/.nUl2->0
									]/.pluS->Plus/.npref->collpav,
				oneamp = Map[collpav, oneamp] /. nUl1 ->0 /.nUl2 -> 0;
			];

		(* Isolating *)
			If[ isolatehead=!=False,
				isol[x_] :=
					Isolate[x,IsolateNames->isolatehead];
				sh[he_][x__] :=
					isol[he[x]];
				scaliso = {A0->sh[A0], B0->sh[B0], B1->sh[b1], B00->sh[B00],
							B11->sh[B11], C0->sh[C0], D0->sh[D0]};
				isoplu[x__] :=
					isol[Plus[x]];
				oneamp = isol[  oneamp/.D0->sh[D0]/.C0->sh[C0]/.
									B11->sh[B11]/.B00->sh[B00]/.B1->sh[b1]/.
									B0->sh[B0]/.A0->sh[A0]/.Plus->isoplu/.
					isoplu->Plus ];
				isol[x_] :=
					x
			];

		(* putting everything together again, including the prefactor *)
			If[ FreeQ[oneamp, q],
				oneamp = ChangeDimension[oneamp, 4],
				newprefactor = newprefactor/I/Pi
			];
			oneampresult = oneamp;
			oneampresult = fsub[newprefactor, finsubst] oneampresult;
			(* Here we add back the non-loop terms *)
			oneampresult = nonLoopTerms + oneampresult;
			FCPrint[3, "oneampresult = ", oneampresult, FCDoControl->oneloopVerbose];
			If[ isolatehead=!=False,
				oneampresult = isol[oneampresult]
			];
		](* end of If FreeQ oneamp, FeynAmpDenomiantor, FAD *) ;

		(* ********************************************************************* *)
		(*                          oneloop27                                    *)
		(* ********************************************************************* *)

		(* writing the result in a file specified by grname *)
		If[ (writeout=!=False )&&  StringQ[ name ] &&
			(!ValueQ[OneLoopResult[grname]]),
			If[ formattype===FortranForm,
				wri[ name, Hold[grname  = oneampresult],
						FormatType -> FortranForm
					]/.wri -> Write2,
				wri[ name, Hold[OneLoopResult[ grname ] = oneampresult],
				FormatType->formattype]/.wri -> Write2;
				If[ Length[FA2Info] > 0,
					wri[ name, Hold[OneLoopInfo[ grname ] = FA2Info],
					FormatType->InputForm]/.wri -> Write2;
				];
			]
		];
		If[ (!ValueQ[ OneLoopResult[ grname ]]) && (grname=!=False),
			set[ OneLoopResult[grname], oneampresult ]/.set->Set
		];
		If[ (!ValueQ[ OneLoopInfo[ grname ]]) && (grname=!=False),
			set[ OneLoopInfo[grname], FA2Info]/.set->Set
		];

	(* ********************************************************************* *)
	(*                          oneloop28                                    *)
	(* ********************************************************************* *)


	(* --------------------------------------------------------------------- *)
	(* some cosmetics for print1 *)
	(* --------------------------------------------------------------------- *)
	(* only if no abbreviations are introduced: print eventually *)
	(* for printing purposes abbreviations are useful,but  *)
	(* this may actually under certain circumstances be incorrect! *)
	(* Though the result returned by OneLoop is of course correct *)
		If[ isolatehead===False,
			PaVeAbbreviate[x_] :=
				x/.PaVe->paVeabbrevi/.paVeabbrevi->PaVe;
			paVeabbrevi[x__,{y__},{m1_,m2_,m3_}, OptionsPattern[]] :=
				ToExpression[ StringJoin@@Prepend[Map[ToString,{x}],"C"] ];
			paVeabbrevi[x__,{y__},{m1_,m2_,m3_,m4_}, OptionsPattern[]] :=
				ToExpression[ StringJoin@@Prepend[Map[ToString,{x}],"D"] ];
			pva[xxx_] :=
				xxx//PaVeAbbreviate;
			pvar[xx__,li1_{},li2_List, OptionsPattern[]] :=
				"As"[li2[[1]]]/;Length[li2]===1;
			pvar[xx__,li1_List,li2_List, OptionsPattern[]] :=
				"Bs"@@Flatten[{li1,li2}]/;Length[li2]===2;
			pvar[xx__,li1_List,li2_List, OptionsPattern[]] :=
				"Cs"@@Flatten[{li1,li2}]/;Length[li2]===3;
			pvar[xx__,li1_List,li2_List, OptionsPattern[]] :=
				"Ds"@@Flatten[{li1,li2}]/;Length[li2]===4;
			FCPrint[2, " ", FCDoControl->oneloopVerbose];
			FCPrint[2, " * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *", FCDoControl->oneloopVerbose];
			FCPrint[2, " ", FCDoControl->oneloopVerbose];
			If[ grname=!=False,
				FCPrint[1, " The result for ", grname, " is ", oneampresult//LeafCount, " leafcount  long "
					(*
						Shallow[ oneampresult,{6, 20} ]
					 *), FCDoControl->oneloopVerbose];
				FCPrint[3, LeafCount[oneampresult], FCDoControl->oneloopVerbose];
			];
			If[ Length[ arglist ]>0 && !breakdown===True,
				FCPrint[2, "Arguments:  ", arglist//Union, FCDoControl->oneloopVerbose]
			];
			FCPrint[2, " ", FCDoControl->oneloopVerbose];
			FCPrint[2, " * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *", FCDoControl->oneloopVerbose];
			FCPrint[2, " ", FCDoControl->oneloopVerbose];
		](* endIf IsolateNames *);

		(* ----------------------------------------------------------------- *)
		]; (* end Timing *)
		If[ grname=!=False,
			FCPrint[1, "CPU - time for ", grname, ": ", tim[[1]]//FeynCalcForm, FCDoControl->oneloopVerbose],
			FCPrint[1, "CPU - time : ", tim[[1]]//FeynCalcForm, FCDoControl->oneloopVerbose]
		];
		oneampresult /.

				(*New 31/7-2002. Support for FeynArts 3.0 SumOver. F.Orellana*)
				If[ (Sum /. Flatten[{opts}] /. Options[OneLoop]) === False,
					FeynArts`SumOver[___] -> 1,
					{}
				] //.
				If[ (Sum /. Flatten[{opts}] /. Options[OneLoop]) === Explicit,
					Times[f__, FeynArts`SumOver[i_, r_, ___]] :>
					(If[ Head[r] =!= List,
						FCPrint[2, "Summing ", i, " from 1 to ", r, FCDoControl->oneloopVerbose];
						rr = Range[1, r],
						FCPrint[2, "Summing ", i, " over ", r, FCDoControl->oneloopVerbose];
						rr = r
					];
					Plus @@ ((Times[f] /. i -> #)& /@ rr)),
					{}
				]
	] /; FreeQ[q,Rule] && FreeQ[q,Plus];

(* ******************************************************************* *)



(* ********************************************************************* *)
(*                          oneloop30                                    *)
(* ********************************************************************* *)


(* OneLoopSumdef *)

Options[OneLoopSum] = {
(*
multiply with the Born diagrams
						Born               -> 1,
*)
						CombineGraphs      -> {},
						Dimension          -> True,
						ExtraVariables      -> {},
						FinalFunction       ->Identity,
						FinalSubstitutions -> {},
						FormatType         -> InputForm,
						InitialSubstitutions  -> {},
						IntermediateSubstitutions -> {},
						IsolateNames        -> KK,
(*
						KeepOnly           -> False,
*)
						Mandelstam         -> {},
						Prefactor          -> 1,
						ReduceToScalars    -> True,
(*
						Scaling            -> {},
*)
						SelectGraphs       -> All,
						WriteOutPaVe       -> False
					};
(*
KeepOnly::usage=
"KeepOnly is an option of OneLoop.
It may be set to B0, C0, D0 keeping only the corresponding
coefficients. The default setting is False. If KeepOnly is set
to {} then the part of the amplitude which is not coefficient
of B0, C0, D0 is kept.";

*)

OneLoopSum[ex_, ops___] :=
	Block[ {mand,reduce,na,i,exx,nsres,sres,
	len0,len,jj, vars, varpave,isolatehead,alreadysummed,
	filename, formattype,selectgraphs,combinegraphs,pres,
	aa0,bb0,cc0,dd0,ddb0,finfunc,inisuB,extravars,
	acdc, lnw, nres, j, nvd, set, npi3, newpa, simp,nplin,
	mandelspec,sumcol,colll,prefactor,feynli,dims,fsub,keeponly,
	d0multiply, c0multiply, d0scalIsolate,c0scalIsolate,vsm,
	intermedsub,isol2,combinelist,np,nnp,npavopt,iiv,lres,mmsu,iim,
	feynAmpden1, feynAmpden2,masss1, masss2, fim,checklabel },
		exx = ex;
		sres = 0;
		SetOptions[DiracTrace,  DiracTraceEvaluate -> False ];
		exx = FixedPoint[ ReleaseHold, exx ];
		opsli         = Join[ {ops}//ReleaseHold, Options[OneLoopSum] ];

		(*
		born          = Born /. opsli;
		*)
		born          = 1;
		combinegraphs = CombineGraphs/.opsli;
		extravars     = ExtraVariables/.opsli;
		isolatehead   = IsolateNames/.opsli;
		If[ isolatehead === True,
			isolatehead = IsolateNames/.Options[Isolate]
		];
		dims           = Dimension /.opsli;
		finalsubst    = FinalSubstitutions/.opsli;
		finfunc       = FinalFunction/.opsli;
		formattype    = FormatType/.opsli;
		inisuB        = InitialSubstitutions /. opsli;
		intermedsub   = IntermediateSubstitutions /. opsli;
		(*
		keeponly      = KeepOnly /. opsli;
		*)
		keeponly = False;
		mandelspec    = Mandelstam/.opsli;
		prefactor     = Prefactor/.opsli;
		reduce        = ReduceToScalars/.opsli;
		(*
		scaling       = Scaling /. opsli;
		*)
		scaling = {};
		writeoutpave  = WriteOutPaVe/.opsli;
		If[ intermedsub =!= {},
			SetOptions[ OneLoop, IntermediateSubstitutions -> intermedsub ]
		];

		(* ********************************************************************* *)
		(*                          oneloop31                                    *)
		(* ********************************************************************* *)
		If[ inisuB =!= {},
			exx = exx /. inisuB
		];
		If[ !FreeQ[exx, FeynAmp],
			(* For FA2.0 *)
			If[ (Head[Head[exx]] =!= FeynAmpList) &&
				(Length[exx] === 1              ) &&
				(Head[Head[exx[[1]]]] === FeynAmpList),
				exx = exx[[1]]
			];

		(* bringing selectgraphs and combinegraphs in a standard form *)
			selectgraphs  = SelectGraphs/.Join[ {ops}, Options[OneLoopSum] ];
			FCPrint[1, "selectghraphs = ", selectgraphs, FCDoControl->oneloopVerbose];
			If[ selectgraphs =!= All,
				selectgraphs  = selectgraphs //. { a___, {i_, j_}, b___} :>
												{ a, i, j, b} /; ( (j-i)^2 === 1);
				selectgraphs  = selectgraphs //.
									{ a___, {i_Integer, j_Integer}, b___ } :>
												{a, Range[i,j],b};
				selectgraphs  = Flatten[ {selectgraphs} ] // Sort;
			];
			len0 = Length[exx];
			If[ combinegraphs === All,
				combinegraphs = Range[len0]
			];
			If[ selectgraphs === All,
				selectgraphs = Range[len0]
			];
			combinegraphs = combinegraphs //. { a___, {b___, {i_,j_}, c___}, d___} :>
												{ a, Join[{b}, Range[i,j], {c}], d};
			If[ MatchQ[combinegraphs, {i___Integer}],
				combinegraphs = {combinegraphs}
			];
			combinegraphs = Map[ Sort, combinegraphs ];
			If[ combinegraphs =!= False,
				ncombine = {};
				For[ ii = 1, ii <= Length[combinegraphs], ii++,
					If[ Length[ Intersection[selectgraphs, combinegraphs[[ii]]] ] > 0,
						AppendTo[ncombine,
								Intersection[selectgraphs,combinegraphs[[ii]]]]
					]   ];
				combinegraphs = ncombine;
				combinelist   = Flatten[combinegraphs];
			];
			FCPrint[1, "Selecting graphs # ", selectgraphs, FCDoControl->oneloopVerbose];
			FCPrint[1, "Combining graphs # ", combinegraphs, FCDoControl->oneloopVerbose];
			If[ Head[exx]=!=FeynAmp,
				feynli[___][xx__] :=
					{xx};
				feynli[xx__] :=
					{xx};
				exx = {List@@(exx/.FeynAmpList->feynli)}//Flatten,
				exx = {exx}
			];
			If[ (Head[selectgraphs]===List && Length[selectgraphs]>0) ||
				(Head[combinegraphs]===List && Length[combinegraphs]>0),
				sumit[exli_, numli_List] :=
					Block[ {nam,lq = First[exli][[2]]},
						nam = GraphName[exli[[1,1,1]],
										ToExpression[ StringJoin@@ Map[ ToString,
													Table[First[ exli[[numli[[jj]]]]]//
												Last, {jj,Length[numli]} ]
										]            ]      ];
						amps = Sum[ exli[[numli[[ij]]]]//Last, {ij,Length[numli]} ];
						FeynAmp[nam,lq,amps]
					];
				nex = {};
				alreadysummed = {};
				For[ ii = 1, ii <= len0, ii++,
					If[ MemberQ[selectgraphs, ii] && FreeQ[alreadysummed,ii],
						If[ !FreeQ[combinelist, ii],
							sumli = sumit[exx, Select[combinegraphs, !FreeQ[#,ii]&][[1]]];
							AppendTo[nex, sumli];
							AppendTo[alreadysummed,
										Select[combinegraphs, !FreeQ[#,ii]&][[1]]],
							AppendTo[nex, exx[[ii]]];
							AppendTo[alreadysummed, ii]
						]
					]
					];
				exx = nex
			];
			len = Length[exx];
			fim[a_] :=
				If[ a==={},
					False,
					a[[1]]
				];
			For[ i = 1, i<=len, i++,
				na = exx[[i,1]];
				If[ na=!=False,
					FCPrint[1, "Calculating ", na, " ; # ", i, " out of ", len, FCDoControl->oneloopVerbose]
				];
				checklabel = False;
				If[ (i > 1) && !FreeQ[exx[[i]], DiracTrace] &&
					(Last[exx[[i-1]]] =!= 0) && (Last[exx[[i]]] =!= 0),
					feynAmpden1 = Select[Last[exx[[i-1]]],
										!FreeQ[#, FeynAmpDenominator]&];
					feynAmpden2 = Select[Last[exx[[i]]],
										!FreeQ[#, FeynAmpDenominator]&];
					If[ (Head[feynAmpden2] === FeynAmpDenominator) &&
						(Head[feynAmpden1] === FeynAmpDenominator) &&
						Length[feynAmpden1] > 2,
						masss1 = Union[ #[[2]]& /@ feynAmpden1];
						masss2 = Union[ #[[2]]& /@ feynAmpden2];
						If[ (Length[masss1] === 1) && (Length[masss2] === 1) &&
							(((!FreeQ[SmallVariables/.Options[OneLoop], masss1//fim]) &&
							(!FreeQ[SmallVariables/.Options[OneLoop], masss2//fim])
							) ||
							((FreeQ[SmallVariables/.Options[OneLoop], masss1//fim]) &&
							(FreeQ[SmallVariables/.Options[OneLoop], masss2//fim])
							)),
							masss1 = masss1[[1]];
							masss2 = masss2[[1]];
							raTio = (Last[exx[[i-1]]] /. masss1 -> masss2
									)/Last[exx[[i]]];
							If[ NumberQ[raTio],
								pres = OneLoop[exx[[i, 1]], exx[[i, 2]],
												(OneLoopResult[exx[[i-1, 1]]]/ raTio
												) /. masss1 -> masss2];
								checklabel = True
							]
						]
					]
				];
			(*dat gaat niet ...
				If[ (i > 2 ) && !FreeQ[exx[[i]], DiracTrace],
					feynAmpden1 = Select[Last[exx[[i-2]]],
										!FreeQ[#, FeynAmpDenominator]&];
					feynAmpden2 = Select[Last[exx[[i]]], !FreeQ[#, FeynAmpDenominator]&];
					If[ (Head[feynAmpden2] === FeynAmpDenominator) &&
						(Head[feynAmpden1] === FeynAmpDenominator) &&
						(Length[feynAmpden1] === Length[feynAmpden2]) &&
						(Length[feynAmpden1] > 2),
						masss1 = Select[Union[ #[[2]]& /@ feynAmpden1], FreeQ[#, 0]&];
						masss2 = Select[Union[ #[[2]]& /@ feynAmpden2], FreeQ[#, 0]&];
						If[ (Length[masss1] === Length[masss2]) && (Length[masss1]>0),
							(((!FreeQ[SmallVariables/.Options[OneLoop], masss1//fim]) &&
							(!FreeQ[SmallVariables/.Options[OneLoop], masss2//fim])
							) ||
							((FreeQ[SmallVariables/.Options[OneLoop], masss1//fim]) &&
							(FreeQ[SmallVariables/.Options[OneLoop], masss2//fim])
							)),
							masssub =  Table[ masss1[[iji]] -> masss2[[iji]],
												{iji, Length[masss1]}];
							raTio = (Last[exx[[i-2]]] /. masssub)/Last[exx[[i]]];
							If[NumberQ[raTio],
								pres = OneLoop[exx[[i, 1]], exx[[i, 2]],
												(OneLoopResult[exx[[i-2, 1]]]/ raTio ) /.
										masssub];
								checklabel = True
								]
					] ]
					];
			*)
				If[ checklabel === False,
					pres = exx[[i]]/.FeynAmp -> OneLoop
				];
				If[ !FreeQ[pres, StandardMatrixElement],
					pres = Expand[pres, StandardMatrixElement]
				];
				sres =  sres + pres
				],
		(*If FreeQ FeynAmp*)

		(* ********************************************************************* *)
		(*                          oneloop32                                    *)
		(* ********************************************************************* *)
			sres = exx;

			(*
			If[ keeponly =!= False,
				SetOptions[B0, B0Unique -> True];
				SetOptions[A0, A0ToB0   -> True];
				Which[ keeponly === D0,
						B0[__] := 0; C0[__] := 0; PaVe[__,{_,_,_},{_,_,_}] := 0,
						keeponly === C0,
						D0[__] := 0; B0[__] := 0,
						keeponly === B0,
						C0[__] := 0; D0[__] := 0,
						keeponly === {},
						C0[__] := 0;  D0[__] :=0
					]
				];
			*)
			dsi[x__] :=
				dsi[x] = DiracOrder[ DiracSimplify[ DOT[x] ] ];
			sres = sres /. DOT -> dsi;
		(*
			If[!FreeQ[sres, StandardMatrixElement],
				sres = Expand[ sres, StandardMatrixElement]
				]
			*)
			](*If FreeQ FeynAmp*);
		If[ combinegraphs === False,
			nres = sres,
			If[ reduce === True,
				mand = Mandelstam/.Options[OneLoop],
				mand = {}
			];
			If[ (mand === {}) && (mandelspec=!={}),
				mand = mandelspec
			];
			FCPrint[1, "mand = ", mand, FCDoControl->oneloopVerbose];
			If[ Length[mand]===4,
				mansu = {mand[[3]]->( mand[[4]] - mand[[1]] - mand[[2]] )},
				mansu = {}
			];
			collp[x_] :=
				Block[ {temp,ntemp,iit},
					temp = x/.mansu;
					If[ reduce =!= False,
						FCPrint[2, "collecting w.r.t. PaVe ", FCDoControl->oneloopVerbose];
						If[ !FreeQ[temp, PaVe],
							If[ Head[temp] =!= Plus,
								temp = Collect2[temp, {A0,B0,C0,D0,PaVe}, Factoring -> True],
								ntemp = 0;
								For[iit = 1, iit <= Length[temp], iit++,
									FCPrint[2, "collecting #  ", iit, " out of ", Length[temp], FCDoControl->oneloopVerbose];
									ntemp = ntemp + Collect2[temp[[iit]],  {A0,B0,C0,D0,PaVe},
															Factoring-> True];
									];
								temp = Collect2[ntemp, {A0,B0,C0,D0,PaVe}, Factoring -> True]
							];
						];
						FCPrint[2, "PaVe-collection done", FCDoControl->oneloopVerbose];
					];
					temp
				];
			If[ FreeQ[sres, StandardMatrixElement],
				vsm = {},
				FCPrint[1, "searching StandardMatrixElement", FCDoControl->oneloopVerbose];
				vsm = Variables[ sres /. {a_StandardMatrixElement _. :> a} ];
				vsm = Select[vsm, (Head[#] === StandardMatrixElement) &];
			];
			If[ (!ValueQ[$SMECollect]) || ($SMECollect === True),
				If[ vsm=!={},
					FCPrint[1, "collect with respect to StandardMatrixElement", FCDoControl->oneloopVerbose];
					nsres = 0;
					For[ij = 1, ij<=Length[vsm], ij++,
					FCPrint[1, "ij = ", ij, "  out of ", Length[vsm], FCDoControl->oneloopVerbose];
					dif =  D[ sres, vsm[[ij]] ];
					nsres = nsres + collp[ dif ] vsm[[ij]];
					sres = sres /. vsm[[ij]] -> 0
						];
					sres = nsres + sres;
					FCPrint[1, "collecting done", FCDoControl->oneloopVerbose],
					sres = collp[sres]
				];
			];
			npavopt = PaVeOrderList/.Options[PaVeOrder];
			paveorder[xxx_] :=
				PaVeOrder[xxx, PaVeOrderList -> npavopt];
			(* insert here eventually previously calculated PaVe's *)
			sres = sres /. PaVe->pavesave /. pavesave -> PaVe;

			(* ********************************************************************* *)
			(*                          oneloop33                                    *)
			(* ********************************************************************* *)
			pavvar[y_] :=
				Block[ {alt,arr,ia,new},
					alt = Drop[#,-1]& /@ Position[y,PaVe];
					arr = {};
					For[ia = 1, ia<=Length[alt], ia++,
						new = Part @@ Prepend[alt[[ia]], y];
						If[ !MemberQ[arr, new],
							AppendTo[arr,new]
						]
						];
					arr
				];
			If[ !FreeQ[sres, PaVe],
				varpave = {};
				If[ Head[sres]===Plus,
					lres = Length[sres];
					For[iiv = 1, iiv <= lres, iiv++,
						FCPrint[2, "searching for PaVe;  iiv = ", iiv, " out of ", lres, FCDoControl->oneloopVerbose];
						varpave = Union[varpave, pavvar[sres[[iiv]]]];
						],
					varpave = pavvar[sres]
				];
				varpave = FixedPoint[ ReleaseHold, varpave ];
				lenpa = Length[varpave];
				pavit[xXX_PaVe, dir_, prev_:False] :=
					Block[ {nx, file, temp, set,xxxa,abbs},
						paV[xy__, p_List, m_List] :=
							PaVe[xy,C,p,C,m, PaVeAutoOrder->paveautoorder,PaVeAutoReduce->paveautoreduce];
						xxx = paV@@xXX;
						(*Changed 18/9-2000, F.Orellana*)
						abbs = DownValues[Abbreviation] /. Abbreviation -> Identity /.
								HoldPattern -> Identity;
						nx = StringReplace[ ToString[InputForm[xxx/.abbs], PageWidth -> 222],
											$Abbreviations
										];
						(**)
						(*nx = StringReplace[ ToString[InputForm[xxx], PageWidth -> 222],
											{", "->"","^"->"","{"->"", "/" -> "",
											"}"->"", "["->"", "]"->"", "*" -> "", " " -> "" ,
								(*Added 18/9-2000, F.Orellana*)"\n" -> "", "\r" -> "",
								"Momentum" -> "", "Pair" -> "", "RenormalizationState" -> "",
								"ParticleMass" -> "m", "PseudoScalar" -> "PS", "Scalar" -> "S",
								"Vector" -> "V", "AxialVector" -> "AV"}
										];*)
						nx = StringJoin[dir, nx, ".s"];
						FCPrint[1, "nx = ", nx, FCDoControl->oneloopVerbose];
						file = FileType[nx];
						FCPrint[1, "file  =", file, FCDoControl->oneloopVerbose];
						If[ file === File,
							temp = ( Get @@ {nx} ) // paveorder;
	(* If something went wrong in writing the file *)
							If[ Head[temp]=!=Plus,
								file = None
							]
						];
						If[ (file === None) && (keeponly === False),
							tim = Timing[
													If[ prev === False,
														temp = PaVeReduce[xXX, Dimension -> dims,
														(*Added 19/9-2000. F.Orellana*)WriteOutPaVe->dir(**)]//paveorder,
														temp = paveorder[prev]
													];
			][[1]];
							FCPrint[1, "Time needed = ", tim//FeynCalcForm, FCDoControl->oneloopVerbose];
							OpenWrite @@ {nx};
							WriteString @@ {nx, "( "};
							Write @@ {nx, temp};
							WriteString @@ {nx, "  ) "};
							Close @@ {nx}
						];
						temp
					](* pavitend *);
				If[ reduce === True,
					For[ j = 1,j<=lenpa,j++,
							FCPrint[1, "working with # ", j, " out of ", lenpa, FCDoControl->oneloopVerbose];
							FCPrint[1, "calculating ", InputForm[ varpave[[j]] ], FCDoControl->oneloopVerbose];
							If[ writeoutpave===True,
								writeoutpave = ""
							];
							If[ !StringQ[writeoutpave],
								tii = Timing[
											nvd = PaVeReduce[ varpave[[j]], IsolateNames ->False,
															Dimension -> dims
															] // paveorder
											];
								FCPrint[1, tii[[1]], " needed", FCDoControl->oneloopVerbose]
							];

							(* ********************************************************************* *)
							(*                          oneloop34                                    *)
							(* ********************************************************************* *)
							SQR[xxx_] :=
								PowerExpand[Sqrt[xxx]];
							If[ StringQ[writeoutpave],
								(* Check if the difference w.r.t. the previous PaVe
								is only in the mass arguments. *)
								nvd = False;
								If[ j > 1,
									If[ (Take[varpave[[j-1]],{-2,-2}]===
										Take[varpave[[j]], {-2,-2}]) &&
										((FreeQ[{Last[varpave[[j-1]]],
												Last[varpave[[j]]]}, SmallVariable]
										) ||
										( (Union[(!FreeQ[#,SmallVariable])& /@
												Last[varpave[[j-1]]]] === {True}) &&
										(Union[(!FreeQ[#,SmallVariable])& /@
												Last[varpave[[j]]]] === {True})
										)),
										mmsu = Table[SQR[Last[varpave[[j-1]]][[iim]]] ->
													SQR[Last[varpave[[j]]][[iim]]],
													{iim, Length[Last[varpave[[j]]]]}
													];
										If[ (varpave[[j-1]] /. mmsu) === varpave[[j]],
											nvd = pavit[varpave[[j]], writeoutpave,
														(varpave[[j-1]]/.PaVe->pavesave
														) /. mmsu
														];
										]
									]
								];
								If[ nvd === False,
									nvd = pavit[varpave[[j]], writeoutpave]
								]
							];
							set[ varpave[[j]]/.PaVe->pavesave ,nvd ]/.set->Set
						];
					sres = sres /. PaVe->pavesave /. pavesave -> PaVe;
				]
			]; (* If !FreeQ[ sres, PaVe ] *)
			isol2[a_?NumberQ] :=
				a;
			isol2[a_?NumberQ b_] :=
				a isol2[b];
			isol2[isol2[a_]] :=
				isol2[a];
			acdc = Join[extravars,{A0,B0,B1,B00,B11,DB0,C0,D0,PaVe}];
			acdc = Union[acdc, acdc /. finalsubst];
			FCPrint[1, "acdc = ", acdc, FCDoControl->oneloopVerbose];
			(* partdef *)
			part[a_Times] :=
				Block[ {pAA},
					pAA = Select[a, !FreeQ2[#, acdc]&];
					pAA part[a/pAA]
				] /; !FreeQ2[a, acdc];
			tog[y_] :=
				Combine[ReleaseHold[y]/.mansu/.scaling/.scaling];
			If[ Length[mand]===4,
				sumcol[xx_] :=
					xx/.Plus->colll/.colll->Plus;
				colll[yy__] :=
					isol2[Collect2[Plus[yy], Variables[Take[mand/.
								scaling/.scaling,3]],
								Factoring->True]
					] /; FreeQ2[{yy},acdc];
		(*simpdef*)
				simp[y_] :=
					sumcol[Factor2[ y /. scaling /. scaling, FactorFull->False
								]//smalld ],
				colll[yy__] :=
					isol2[Plus[yy]];
				sumcol[xx_] :=
					xx/.Plus->colll/.colll->Plus;
				simp[y_] :=
					Factor2[ y/. scaling /. scaling, FactorFull -> False
						]//smalld
			];
			If[ Length[mandelspec] === 4,
				simp[y_] :=
					sumcol[TrickMandelstam[
							Factor2[ (y /. scaling /. scaling)//smalld,
										FactorFull -> False],
										mandelspec/.scaling]
						]
			];
			born = simp[born /. scaling /. scaling];
			lnw = Length[sres];
			If[ Head[sres]=!=Plus,
				lnw = 1
			];
			nres = 0;
			FCPrint[1, "substituting ", FCDoControl->oneloopVerbose];
			sres =  sres/.mansu;
			FCPrint[1, "done", FCDoControl->oneloopVerbose];
			(* here we have the loop over the StandardMatrixElement *)
			If[ FreeQ[sres, StandardMatrixElement],
				lnw = 1
			];
			For[jj = 1, jj<=lnw, jj++,
				If[ lnw === 1,
					If[ FreeQ[sres, StandardMatrixElement],
						newpa = {sres, 1},
						newpa =  PartitHead[ Expand[sres, StandardMatrixElement],
											StandardMatrixElement ]
					],
					newpa = PartitHead[ sres[[jj]],StandardMatrixElement ]
				];
				FCPrint[1, " # ", jj, " out of ", lnw, "  ", newpa[[2]], FCDoControl->oneloopVerbose];
						(* Collect wrt. all the scalar integrals *)
				FCPrint[1, "Shallow  ", Shallow[newpa[[1]]], FCDoControl->oneloopVerbose];
				np = newpa[[1]];
				FCPrint[1, "leafcount of np = ", LeafCount[np], FCDoControl->oneloopVerbose];
				If[ Global`$Special === True,
					oldnp = np;
					np = Collect2[np, acdc, Factoring -> False];
				];
				tinp = Timing[
								np = Collect2[ np, acdc, Factoring -> True];
								nnp = paveorder[np];
								If[ np =!= nnp,
									np = Collect2[nnp, acdc, Factoring -> True];
								];
							];
				FCPrint[1, "timing for collecting = ", tinp[[1]], FCDoControl->oneloopVerbose];
				zero[__] :=
					0;
								(* combine the terms without PaVe's *)
				nplin = np/.A0->zero;
				nplin = nplin/.B0->zero;
				nplin = nplin/.C0->zero;
				nplin = nplin/.D0->zero;
				nplin = nplin/.DB0->zero;
				nplin = nplin/.B1->zero;
				nplin = nplin/.B00->zero;
				nplin = nplin/.B11->zero;
				nplin = nplin/.PaVe->zero;
				If[ extravars =!= {},
					For[iext = 1, iext <= Length[extravars], iext++,
						nplin = nplin /. extravars[[iext]] -> 0
						]
				];
				If[ nplin === 0,
					FCPrint[1, "nplin = 0", FCDoControl->oneloopVerbose],
					FCPrint[1, "leafcount of nplin = ", LeafCount[nplin], FCDoControl->oneloopVerbose];
				];
				If[ nplin =!= 0,
					If[ keeponly === False,
						np = np - nplin,
						If[ keeponly === {},
							np = 0,
							If[ keeponly === B0 || keeponly === C0 ||
								keeponly === D0,
								np = np - nplin;
								nplin = 0
							]
						]
					]
				];
				pres = 0;
				If[ Head[np]===Plus,
					lnp = Length[np];
					FCPrint[1, "combining coefficients of B0, C0, ...", FCDoControl->oneloopVerbose];
							(* This ist the loop of A0B0C0D0 *)
								(* putting it now over a common denominator *)
					For[i3 = 1, i3<=lnp, i3++,
						FCPrint[1, "i3 = ", i3, "   out of ", lnp, "
							LeafCount = ", LeafCount[np[[i3]]], FCDoControl->oneloopVerbose];
						npi3 = finfunc @@ {part[ np[[i3]]
												]/.part->simp};
						If[ born =!= 1,
							npi3 = part[npi3 born]/.part->simp;
						];
						FCPrint[2, "npi3 = ", npi3, FCDoControl->oneloopVerbose];
						pres = pres + npi3
						],
					pres = part[np ]/.part->simp;
					If[ born=!=1,
						pres = part[ pres born ] /. part -> simp
					]
				];
				FCPrint[2, "factoring nplin, LeafCount =  ", LeafCount[nplin], FCDoControl->oneloopVerbose];
				nplin = Cancel[simp[ Factor2[finfunc[nplin]]  ] *
								simp[ Factor2[finfunc[born]]]//finfunc ];
				FCPrint[2, "nplin = ", nplin, FCDoControl->oneloopVerbose];
				nres = nres +  newpa[[2]] (pres + nplin)
				](* endFor*);

		(* ********************************************************************* *)
		(*                          oneloop35                                    *)
		(* ********************************************************************* *)
			fsub[x_] :=
				Block[ {nx = x,su,ij},
					su = finalsubst;
					For[ij = 1, ij<=Length[su], ij++,
						nx = nx/.su[[ij]]
						];
					nx
				];
			mand = fsub[mand];
			nres = fsub[ FixedPoint[tog, prefactor, 5] nres];
			check = nres;
			{aa0, bb0, bb1, bb00, bb11, ddb0, cc0, dd0} =
			{A0, B0,   B1,  B00,  B11,  DB0,  C0,  D0} // fsub;
			If[ isolatehead=!=False,
				FCPrint[1, "isolating now ", FCDoControl->oneloopVerbose];
				plupp0[x__] :=
					Plus[x] /; !FreeQ[{x},plupp0];
				plupp1[x__] :=
					Factor2[ains Plus[x]];
				isolfact[x_] :=
					isol2[x/.Plus->plupp0/.plupp0->plupp1]/.ains->1;
				If[ Length[mand]===4,
					isolmand[x_] :=
						Isolate[x, {mand[[1]],mand[[2]],mand[[3]]},
										IsolateNames->isolatehead],
					isolmand[x_] :=
						Isolate[x,IsolateNames->isolatehead ]
				];
				isolate0[x_] :=
					Isolate[x, IsolateNames->isolatehead ];
				isc[x_][y__] :=
					isol1[x][(TrickMandelstam[fsub[x[y]]/.dd0->D0,mand
											]//paveorder)/.
							D0 -> dd0, IsolateNames->isolatehead];
				If[ Length[mand]===4,
					isol1[_][x_, y_] :=
						Isolate[x, y] /; FreeQ2[x, Take[mand, 3]]
				];
				(* for scaling *)
				d0multiply = (D0 /. scaling) /. D0 -> 1;
				c0multiply = (C0 /. scaling) /. C0 -> 1;
				db0multiply = (DB0 /. scaling) /. DB0 -> 1;
				d0scalIsolate[x_,he_] :=
					Isolate[x d0multiply, he];
				c0scalIsolate[x_,he_] :=
					Isolate[x c0multiply, he];
				db0scalIsolate[x_,he_] :=
					Isolate[x db0multiply, he];
				nres = isolate0[( nres )/.
										dd0    -> isc[dd0]/.
										cc0    -> isc[cc0]/.
										bb11   -> isc[bb11]/. bb00  -> isc[bb00]/.
										bb1    -> isc[bb1]/.  bb0   -> isc[bb0]/.
										ddb0   -> isc[ddb0]/.
										aa0    -> isc[aa0]/.  PaVe -> isc[PaVe]/.
										isol1[dd0] -> d0scalIsolate/.
										isol1[cc0] -> c0scalIsolate/.
										isol1[ddb0] -> db0scalIsolate/.
										isol1[bb1] -> Isolate/.
										isol1[bb00] -> Isolate/.
										isol1[bb11] -> Isolate/.
										isol1[bb0] -> Isolate/.
										isol1[aa0] -> Isolate/.
										isol1[PaVe] -> Isolate/.
										isol2 -> isolfact/.
										isol2 -> isolmand
								],
			(* ********************************************************************* *)
			(*                          oneloop36                                    *)
			(* ********************************************************************* *)

			(* If isolatehead .. *)
			(*Only if the option Factoring of OneLoop is True, factor also here *)
				If[ (Factoring/.Options[OneLoop]) === True,
					specrule = {(a_Symbol - b_Symbol) (a_Symbol+b_Symbol)->(a^2-b^2)};
					factor3[x_] :=
						Factor2[x]/.specrule;
					isol22[a_ b_] :=
						isol22[a] isol22[b];
					isol22[a_] :=
						a/;Head[a]=!=Plus;
					isc2[x_][y__] :=
						(TrickMandelstam[x[y]/.dd0->D0,mand]//paveorder)/.
						D0 -> dd0;
					nres = nres/.dd0 -> isc2[dd0]/.cc0    -> isc2[cc0]/.
								bb11-> isc2[bb11]/. bb00 -> isc2[bb00]/.
								bb1 -> isc2[bb1]/. bb0   -> isc2[bb0]/.
								dbb0 -> isc2[ddb0] /.
								aa0 -> isc2[aa0]/.  PaVe -> isc2[PaVe];
					nres = nres/.isol2 -> isol22;
					nres = Map[factor3, nres + nuLL]/.specrule;
					colp[x__] :=
						Map[TrickMandelstam[#,mand]&,
							Collect2[Plus[x], {aa0,bb0,bb00,bb11,bb1,ddb0,
											cc0,dd0,PaVe},
									Factoring -> True]
						];
					nres = factor3[ Map[(#/.Plus->hoLdP)&, nres]/.nuLL->0/.
									hoLdP[0]->0 ] /. hoLdP -> colp,
					nres = nres/.isol2->Identity
				];
				nres = nres/.isol22->Identity;
				disc[y__] :=
					TrickMandelstam[ D0[y],mand ]//paveorder;
				cisc[y__] :=
					TrickMandelstam[ C0[y],mand ]//paveorder;
				dbisc[y__] :=
					TrickMandelstam[ DB0[y],mand ]//paveorder;
				nres = nres /. D0->disc /. C0->cisc /. DB0 -> dbisc;
			];
		](*If combinegraphs ... *);
		FCPrint[2, "The result of OneLoopSum is ", nres, FCDoControl->oneloopVerbose];
		nres
	];
(*endOneLoopSum *)


(* ******************************************************************* *)

	(*smallddef *)
small2/: small2[x_]^n_ := small2[x^2] /; n > 0;
small2/: small2[_] a_ :=0;
small3/: small3[_] + a_ :=a;
small4[x_^m_] :=
	SmallVariable[x]^m;
	smalld[x_] :=
		x/;FreeQ[x,SmallVariable];
	smalld[x_] :=
		x/.SmallVariable->small2/.small2->small3/.
				small3->small4/.small4->SmallVariable;

(* ******************************************************************* *)
(* ********************************************************************* *)
(*                          oneloop37                                    *)
(* ********************************************************************* *)

												(*spinorsandpairsdef*)
	dotdotlin[x___] :=
		dotlin[DOT[x]];
(*tempstandmatdef*)
	standma[x_] :=
		dotdotlin[x]/;!FreeQ2[x, {Polarization, DOT}];
	tempstandmat[x_] :=
		Block[ {ttt},
			ttt = x;
			If[ StandardMatrixElement =!= Identity,
				If[ FreeQ[ttt, Spinor] &&
					!FreeQ2[ttt,{SUNF,SUNDelta, SUNT, Pair}],
					If[ LeafCount[ttt]>500,
						FCPrint[2, "expanding in tempstandmat", FCDoControl->oneloopVerbose]
					];
					ttt = Expand[ttt spinorsandpairs[]];
					If[ LeafCount[ttt]>500,
						FCPrint[2, "expanding in tempstandmat done", FCDoControl->oneloopVerbose]
					];
					ttt = ttt /. spinorsandpairs -> dotsp
				];
				If[ (Length[DownValues[spinorsandpairs]]>1) ||
					ValueQ[StandardMatrixElement],
					ttt = x/.DOT->spinorsandpairs/.
							spinorsandpairs->StandardMatrixElement/.
							StandardMatrixElement->standma/.standma->
							StandardMatrixElement;
				];
			(*
					If[$fourfermion =!= True,
						If[FreeQ[ttt, Polarization] && !FreeQ[ttt,Spinor],
							ttt = SpecificPolarization[ttt]
						]
						];
*)
	];
			ttt
		];



	spinorsandpairs[a_,b__] :=
		dotdotlin[a,b]//spinorsandpairs;
	dotsp[] = 1;
	dotsp[x_] :=
		x;

	spinorsandpairs/:
		spinorsandpairs[x___] Pair[ Momentum[a__], Momentum[b__]]^n_. :=
			spinorsandpairs[dotsp[x] Pair[Momentum[a],Momentum[b]]^n]/; !FreeQ[{a,b},Polarization];

	spinorsandpairs/: spinorsandpairs[x___] Eps[w__] :=
		spinorsandpairs[dotsp[x] Eps[w]]/; !FreeQ[{w}, Polarization];

	spinorsandpairs/:
		spinorsandpairs[x___] a_SUNT:= spinorsandpairs[dotsp[x], a];

	spinorsandpairs/:
		spinorsandpairs[x___] a_SUNF:= spinorsandpairs[dotsp[x] a];

	spinorsandpairs/:
		spinorsandpairs[x___] a_SUNDelta:= spinorsandpairs[dotsp[x] a];

	spinorsandpairs/: spinorsandpairs[x___] spinorsandpairs[y___] :=
		spinorsandpairs[dotsp[x] dotsp[y]];

(* ********************************************************************* *)
(*                          oneloop38                                    *)
(* ********************************************************************* *)

(* *************************************************************** *)
(* Tensorintegraldecomposition *)
(* *************************************************************** *)

denomExpand[y__] :=
	y/.FeynAmpDenominator->denexp; (*denomExpanddef*)
denexp[z__] :=
	Expand //@ MomentumExpand[ FeynAmpDenominator[z] ];
(* *************************************************************** *)
(* suind substitutes for qu dummy indices for the tensor integral  *)
(* decomposition, the first argument of suind is a sumand *)
(* *************************************************************** *)
	(* Hier das RICHTIGE  suind ::: *)
	suind[ y_,qu_,dim_,md_] :=
		Block[ {i,res = y, posli,
				currentposli},          (*suinddef*)
			posli = Position[y,Momentum[qu,___] ];
			If[ posli=!={},
				For[i = 1, i <= Length[posli],i++,
					currentposli = Position[res, Momentum[qu,___] ];
					res = ReplacePart[res, LorentzIndex[md[i],dim],
									currentposli[[1]] ]
				]
			];
			res
		];


(* *************************************************************** *)
(* for the divergent parts  "epsilon - substitution"               *)
(* *************************************************************** *)
	epst[x__] :=
		If[ uvpart === True,
			0,
			epst2[x]
		];
	epst2[gr_,x_,4,resid_] :=
		to4dim[ gr ];       (*epstdef*)
	epst2[gr_,x_,d_Symbol,_] :=
		to4dim[ gr ]/;FreeQ[x,d];
	epst2[gr_,x_,d_Symbol,resid_] :=
		Block[ {epstresul,epstin,epseps,epx},
			epx = to4dim[x];
			epstin = Expand[(epx/.d->(4-epseps))-(epx/.d->4)
							]//spinorchainevaluate;
			epstresul = to4dim[gr + Normal[ Series[epstin,{epseps,0,1}]
										]/.epseps->resid ]//Expand;
			epstresul
		];

(* ********************************************************************* *)
(*                          oneloop39                                    *)
(* ********************************************************************* *)

(* *************************************************************** *)
(* A useful evaluation function ( for  tensint )                   *)
(* *************************************************************** *)
	to4d2[x_] :=
		x /;  $LimitTo4 =!= True;
	to4d2[x_] :=
		(x/.{Momentum[fope_,_]:>Momentum[fope]/;FreeQ[fope,q],
			LorentzIndex[muu_,_] :> LorentzIndex[muu]}
		) /; $LimitTo4 === True;
	dirsim[a_ b_] :=
		a dirsim[b] /; FreeQ2[a, {Spinor,DiracGamma}];
SetAttributes[eval,Listable];                            (*evaldef*)
	eval[evy_] :=
		MemSet[ eval[evy],
		Block[ {evalte,nul1,nul2,ie,neval,nt},
			FCPrint[3, "OneLoop: eval: Entering with ", evy ,FCDoControl->oneloopVerbose];
			evalte = to4d2[ evy/.NonCommutativeMultiply->Times ];
			If[ !FreeQ[ evalte, LorentzIndex ],
				evalte = contractli[ evalte ];
			];
			evalte = FixedPoint[ReleaseHold,evalte]//to4d2;
			evalte = Expand[ dotlin[ evalte ]//ExpandScalarProduct, DOT ];
			If[ (Length[evalte]>5) && !FreeQ[evalte, DOT],
				evalte = Collect2[ evalte, DOT, Factoring -> False]
			];
			If[ !FreeQ[ evalte, DiracGamma],
				evalte = Map[ dirsim, evalte + nul1 ]/.
						dirsim->DiracSimplify/.nul1 -> 0;
				If[ LeafCount[evalte]>100,
					evalte = Collect2[evalte,DOT, Factoring -> False];
				];
				evalte = evalte + nul1 + nul2;
				neval = 0;
				For[ie = 1, ie<=Length[evalte], ie++,
					If[ Length[evalte[[ie]]]>0,
						FCPrint[3, "ie = ", ie, " out of ", Length[evalte], FCDoControl->oneloopVerbose]
					];
					nt = Contract[DiracOrder[evalte[[ie]]]
								]//ExpandScalarProduct;
					nt = DiracSimplify[nt]//DiracOrder;
					nt = Expand[nt, DOT];
					FCPrint[3, "length of nt = ", nt//Length, FCDoControl->oneloopVerbose];
					neval = neval + nt
					];
				evalte = neval/.nul1->0/.nul2->0;
			];
			If[ !FreeQ[evalte, Eps],
				evalte = evalte//EpsEvaluate//epschisholm;
					(* The default is that Eps's will be contracted away!*)
				evalte = Expand[ conall[ evalte ]//ExpandScalarProduct];
				evalte = epschisholm[ evalte ]//DiracSimplify//DiracOrder;
				evalte = Contract[ evalte ];
				evalte = Expand[ evalte//ExpandScalarProduct ]
			];
			evalte = tempstandmat[ evalte ];
			evalte = Expand[evalte];
			FCPrint[3, "OneLoop: eval: Leaving with ", evalte ,FCDoControl->oneloopVerbose];
			evalte
		]];

(* ********************************************************************* *)
(* ********************************************************************* *)
(*                          oneloop40                                    *)
(* ********************************************************************* *)
	(*tensintdef*)
tensint[x_,dim_,q_,options___] := (*tensint[x,dim,q,options]=*)
	Block[ {(*tensj,tensi,tensic,*)tensg = 0,(*mandel,*)tensx = x(*,tensdnp,tensdnp1,*)
		(*tenslnt,tensldn,tensqmax,tenslep,tensdnqq,tensdnqqb,*)
		(*tensqc,tensjq*)(*,tensfq,ltx*)
		},
		tensg = Catch[
		FCPrint[2, "OneLoop: tensint: entering with ", tensx ,FCDoControl->oneloopVerbose];
		FCPrint[3, "OneLoop: tensint: entering ", q, "  dimension  ", dim, "   ", x//FeynCalcForm, FCDoControl->oneloopVerbose];
		(* diracSimplify must have been used previously              *)
		mandel =  Mandelstam /.Join[ options,Options[ tensint ] ];
	(* tensor integral decomposition *)
		(*ltx = nterms[tensx];*)
		If[ Head[tensx]===Plus,
			tenslnt = Length[tensx],
			tenslnt = 1
		];

		(* The tensj - loop runs over all different loop integrals *)
		Clear[tensqc];
		For[ tensj = 1, tensj <= tenslnt, tensj++,
			FCPrint[1, "OneLoop: tensint: tensorintegral # ", tensj, " / ", tenslnt, FCDoControl->oneloopVerbose];
			FCPrint[3, "OneLoop: tensint: tensorintegral ", If[ tenslnt===1, tensx, tensx[[tensj]]], FCDoControl->oneloopVerbose];
			tensqc[tensj][any_] :=
				0;
			(* splitting into q^i_1 ... q^i_j and FeynAmpDenominator for each term *)
			If[ tenslnt===1,
				tensdnp = PartitHead[ tensx,FeynAmpDenominator ],
				tensdnp = PartitHead[ tensx[[tensj]],FeynAmpDenominator ]
			];
			FCPrint[3,"OneLoop: tensint: tensdnp1 ",tensdnp, FCDoControl->oneloopVerbose];
			(* Collect according to the number of q's *)
			tensdnp1 = Collect2[ tensdnp[[1]],q, Factoring -> False];

			FCPrint[3,"OneLoop: tensint: tensdnp1 ",tensdnp1, FCDoControl->oneloopVerbose];

			(*This for dealing with things like (q.x)^n *)
			pairpow/: pairpow[a___,Momentum[q,di___],b___]^n_Integer?Positive :=
					(pairpow[a,Momentum[q,di],b]^(n-1))**pairpow[a,Momentum[q,di],b];

			tensdnp1 = tensdnp1/.Pair->pairpow/.pairpow->Pair;

			FCPrint[3,"OneLoop: tensint: tensdnp1 after pairpow ",tensdnp1, FCDoControl->oneloopVerbose];
			FCPrint[1, "OneLoop: tensint: Checking rank of ", tensdnp1, FCDoControl->oneloopVerbose];
			If[ Head[tensdnp1]===Plus,
				tensldn = Length[tensdnp1],
				tensldn = 1
			];
			FCPrint[3, "OneLoop: tensint: tensldn: ", tensldn, FCDoControl->oneloopVerbose];
			tensqmax[tensj] = 0;
			FCPrint[3, "OneLoop: tensint: Entering 1st nested loop", FCDoControl->oneloopVerbose];
			For[ tensic = 1, tensic <= tensldn, tensic++,
				If[ tensldn===1,
					(*one term in the numerator *)

					(* Determines the tensor rank *)
					tenslep = Length[Position[tensdnp1,q ] ];
					(* BREAK EVENTUALLY *)
					If[ tenslep>3,
						Print["FYI: Tensor integrals of rank higher than 3 encountered; Please use the option CancelQP -> True or OneLoopSimplify->True or use another program."];
						Throw[x]
					];
					FCPrint[3, "OneLoop: tensint: tensdnp1 ", tensdnp1, FCDoControl->oneloopVerbose];
					tensqc[tensj][tenslep] += suind[ tensdnp1,q,dim,mud ],
					(*more terms in the numerator *)
					(* Determines the tensor rank *)
					tenslep = Length[ Position[tensdnp1[[tensic]],q ] ];
					tensqc[tensj][tenslep] += suind[tensdnp1[[tensic]],q,dim,mud]
				];
				FCPrint[3, "OneLoop: tensqc[tensj][tenslep]: ", tensqc[tensj][tenslep], FCDoControl->oneloopVerbose];

				If[ tenslep > tensqmax[tensj],
					tensqmax[tensj] = tenslep
				]
			];
			FCPrint[3, "OneLoop: tensint: 1st nested loop done", FCDoControl->oneloopVerbose];

			FCPrint[3, "OneLoop: tensint: tensdnp1 ", tensdnp1, FCDoControl->oneloopVerbose];
			FCPrint[3, "OneLoop: tensint: tensqmax[tensj] ", tensqmax[tensj], FCDoControl->oneloopVerbose];
			FCPrint[3, "OneLoop: tensint: tensg before the nested loop ", tensg, FCDoControl->oneloopVerbose];



			FCPrint[3, "OneLoop: tensint: Entering 2nd nested loop", FCDoControl->oneloopVerbose];
			For[ tensjq = 0, tensjq <= tensqmax[tensj], tensjq++,
				tdenlen = Length[ tensdnp[[2]] ];
				FCPrint[2, "OneLoop: tensint: Tensorintegral (N = ", tdenlen, ") : # of q's = ", tensjq, " decomposing ", Length[tensqc[tensj][tensjq]], " term(s)", FCDoControl->oneloopVerbose];
				(* tdec is the function that is actually doing the decomposition !*)
				FCPrint[3, "OneLoop: tensint: tensqc[tensj][tensjq] ", tensqc[tensj][tensjq], "", FCDoControl->oneloopVerbose];
				tensg += tdec[ tensqc[tensj][tensjq], tensdnp[[2]],q,
								tensjq,dim,mud,mandel
							]/.NonCommutativeMultiply->Times
				];(*tensjq - loop*)
				FCPrint[3, "OneLoop: tensint: 2nd nested loop done", FCDoControl->oneloopVerbose];
				FCPrint[3, "OneLoop: tensint: tensg after the 2nd nested loop ", tensg, FCDoControl->oneloopVerbose];
			];
		tensg =  Expand[tensg]

		];
		FCPrint[2, "OneLoop: tensint: Leaving with tensg= ", tensg, FCDoControl->oneloopVerbose];
		tensg
	](* end tensint *);

(* ********************************************************************* *)
(*                          oneloop41                                    *)
(* ********************************************************************* *)

(* *************************************************************** *)
(* tensor integrals; "qn" denotes the number of "q's"  *)
(* *************************************************************** *)
pavremember[x__] :=
	MemSet[pavremember[x], PaVeReduce[x]];

	tdec[0,___] :=          (*tdecdef*)
		0;

	tdec[ expr_,props_,Q_,qn_ ,di_,mudu_,mand_] :=          (*tdecdef*)
		MemSet[ tdec[ expr,props,Q,qn,di,mudu,mand],
		Block[ {spl0, mande,tensps = {},tdecnew,tdec0j,tdectij,
				tensdf2,tensdf1, pav0,
				tdecex = expr/.Pair-> PairContract,
				tdi,tdecti,tdectj,tdectk,tdectl,tdectm,tdecr = 0,
				tdecpl,tdecml,tdeclpl,
				rul,spl,add
				},
				FCPrint[3, "entering tdec with expr = ", expr//FeynCalcForm, FCDoControl->oneloopVerbose];
				FCPrint[3, "props =  ", props, FCDoControl->oneloopVerbose];
				FCPrint[3, "Q =  ", Q, FCDoControl->oneloopVerbose];
				(* number of vectors in the numerator *)
				FCPrint[3, "qn =  ", qn, FCDoControl->oneloopVerbose];
				FCPrint[3, "di =  ", di, FCDoControl->oneloopVerbose];
				FCPrint[3, "mudu =  ", mudu, FCDoControl->oneloopVerbose];
				FCPrint[3, "mand =  ", mand, FCDoControl->oneloopVerbose];
				tensdf2[_,b_] :=
					b;
				tensdf1[a_,_] :=
					Expand[ to4dim[
							MomentumExpand[a-Momentum[Q]]]
						];
			If[ tdecex===0,
				tdecr = 0,
				(* Here it goes *)

				(* define add *)
				add[gra_,pva_,exp_] :=
					Block[ {addre, pv = pva},
						FCPrint[3, "OneLoop: tdec: add: gra: ", gra, FCDoControl->oneloopVerbose];
						FCPrint[3, "OneLoop: tdec: add: pv: ", pv, FCDoControl->oneloopVerbose];
						FCPrint[3, "OneLoop: tdec: add: exp: ", exp, FCDoControl->oneloopVerbose];
						If[ breakdown === True,
							pv = pavremember[pv, WriteOutPaVe -> writeoutpav ];
							If[ uvpart === True,
								pv = pv /. C0[__] -> 0 /. D0[__]->0 /. B0[__]-> UVDELTA;
								If[ FreeQ2[pv, {B1,B00,B11}],
									FCPrint[2, "pv = ", pv//FeynCalcForm, FCDoControl->oneloopVerbose];
									pv = Factor2[D[ pv, UVDELTA]],
									FCPrint[2, "pv = ", pv//FeynCalcForm, FCDoControl->oneloopVerbose];
									Print["problems with uvcheck in OneLoop!!!", Dialog[]]
								];
								FCPrint[2, "uvcheck ", pv//FeynCalcForm, FCDoControl->oneloopVerbose];
							];
						];
						If[ $LimitTo4 === True,
							addre = gra + (Expand[ExpandScalarProduct[ pv to4dim[ exp/.di->4 ]]]),
							addre = gra + (Expand[ExpandScalarProduct[ pv exp]])
						];
						addre
					];

				(* calculate the List of scalar products needed as arguments *)

				(* get the list of p's from the propagators, ignoring the first propagator *)
				tdecpl = Drop[ Expand[ props//MomentumExpand]/.
								PropagatorDenominator->tensdf1/.
								FeynAmpDenominator->List,1 ]//DiracGammaCombine;
				tdecpl = Expand[tdecpl];
				FCPrint[2, "tdecpl = ", tdecpl, FCDoControl->oneloopVerbose];

				(* get the list of m's from the propagators *)
				tdecml = props/.PropagatorDenominator->tensdf2/. FeynAmpDenominator->List;
				FCPrint[3, "tdecml = ", tdecml, FCDoControl->oneloopVerbose];
				tdecml = #^2& /@ tdecml;
				FCPrint[3, "tdecml = ", tdecml, FCDoControl->oneloopVerbose];

				(* number of external momenta *)
				tdeclpl = Length[ tdecpl ];

				(* D_0, D_mu, D_munu and D_munuro have no UV poles. Therefore, is
				$LimitTo4 is true, one can directly set D=4 *)
				If[ ($LimitTo4===True) && (tdeclpl===3) && (qn<4),
					tdecex = tdecex/.di->4;
					tdi = 4,
					tdi = di
				];

				(* calculation of (N (N-1)/2) scalar pipj - arguments *)
				spl0[a_,b_,man_] :=
					(TrickMandelstam@@Prepend[{man},Expand[Pair[a-b,a-b]]//ExpandScalarProduct])//smalld;

				spl[aa__] :=
					spl0[aa,mand]//ExpandAll;

				FCPrint[3, "OneLoop: tdec: covnerting FADs to PaVe functions.", FCDoControl->oneloopVerbose];

			tensps = ExpandScalarProduct[FeynCalc`Package`momentumRoutingDenner[tdecpl,spl[#,0]&]];

			FCPrint[3, "OneLoop: tdec: tensps ", tensps," ", FCDoControl->oneloopVerbose];
			FCPrint[3, "OneLoop: tdec: tdecr ", tdecr, FCDoControl->oneloopVerbose];
			FCPrint[3, "OneLoop: tdec: tdecex ", tdecex, FCDoControl->oneloopVerbose];

			(* scalar integrals *)
			If[ qn==0,

				(* new prefactor *)
				tdecnew = eval[ tdecex ];

				If[ $LimitTo4 === True,
					Which[
						tdeclpl == 0,
							(* e A0 = 2m^2 *)
							tdecr = epst[ tdecr,tdecnew,tdi, 2 tdecml[[1]] ],
						tdeclpl == 1,
							(* e B0 = 2 *)
							tdecr = epst[ tdecr,tdecnew,tdi, 2 ];
					]
				];

				FCPrint[3, "OneLoop: tdec: tdecenew ", tdecnew, FCDoControl->oneloopVerbose];

				(* if the option DenominatorOrder is True, then order here again *)
				If[ denomOrder === True,
					pav0 = PaVeOrder[PaVe[0,tensps,tdecml,PaVeAutoOrder->paveautoorder,PaVeAutoReduce->paveautoreduce]],
					pav0 = PaVe[0,tensps,tdecml,PaVeAutoOrder->paveautoorder,PaVeAutoReduce->paveautoreduce]
				];

				FCPrint[3, "OneLoop: tdec: pav0: ", pav0, FCDoControl->oneloopVerbose];
				(* Reduction of PaVe functions into simpler ones... *)
				tdecr = add[ tdecr, pav0, tdecnew ];
				FCPrint[3, "OneLoop: tdec: qn==0, tdecr=", tdecr, FCDoControl->oneloopVerbose]
			];

			(* q^mu (...) *)
			If[ qn==1,
				(* new prefactor *)
				tdecnew = Table[ eval[ tdecex/.LorentzIndex[mudu[1],___]-> tdecpl[[tdecti]] ], {tdecti,1,tdeclpl}];

				FCPrint[3, "OneLoop: tdec: qn==1, tdecnew=", tdecnew, FCDoControl->oneloopVerbose];
				If[ ($LimitTo4 === True) && (tdeclpl === 1),   (* e B1 = -1 *)
					tdecr = epst[ tdecr,tdecnew[[1]], tdi,-1 ]
				];
				FCPrint[3, "OneLoop: tdec: qn==1, tdecr=", tdecr, FCDoControl->oneloopVerbose];
				For[ tdectj = 1,tdectj<=tdeclpl,tdectj++,
					tdecr = add[ tdecr, PaVe[tdectj,tensps,tdecml,PaVeAutoOrder->paveautoorder,PaVeAutoReduce->paveautoreduce],
									tdecnew[[tdectj]]
					]            ];
				FCPrint[3, "OneLoop: tdec: qn==1, tdecr=", tdecr, FCDoControl->oneloopVerbose]
			];

			(* q^mu q^nu (...) *)
			If[ qn==2,
				tdecnew = eval[ tdecex/.LorentzIndex[mudu[1],dime___] :> LorentzIndex[mudu[2],dime]];
				FCPrint[3, "OneLoop: tdec: qn==2, tdecnew=", tdecnew, FCDoControl->oneloopVerbose];

				If[ $LimitTo4 === True,
					Which[
						tdeclpl == 0,        (* e A00 = m^4/2 *)
										tdecr = epst[  tdecr,tdecnew,tdi,
														tdecml[[1]]^2/2
													],
						tdeclpl == 1,        (* e B00  *)
										tdecr = epst[  tdecr,tdecnew,tdi,
														(-1/3 spl[tdecpl[[1]],0] +
														tdecml[[1]] +
														tdecml[[2]] )/2
													] ,
						tdeclpl == 2,       (* e C00 = 1/2 *)
										tdecr = epst[ tdecr, tdecnew, tdi, 1/2 ]
						]
				];

				FCPrint[3, "OneLoop: tdec: qn==2, tdecr=", tdecr, FCDoControl->oneloopVerbose];
				tdecr = add[ tdecr, PaVe[0,0,tensps,tdecml,PaVeAutoOrder->paveautoorder,PaVeAutoReduce->paveautoreduce], tdecnew ];
				FCPrint[3, "OneLoop: tdec: qn==2, tdecr=", tdecr, FCDoControl->oneloopVerbose];
				FCPrint[3, "OneLoop: tdec: qn==2, tdecex=", tdecex, FCDoControl->oneloopVerbose];
				FCPrint[3, "OneLoop: tdec: qn==2, tdeclpl=", tdeclpl, FCDoControl->oneloopVerbose];
				tdecnew = Table[{Sort[{tdecti,tdectj}],
						tdecex/.LorentzIndex[mudu[1],___]->tdecpl[[tdecti]]/.
								LorentzIndex[mudu[2],___]->tdecpl[[tdectj]]
								},{tdectj,1,tdeclpl},{tdecti,1,tdeclpl}
								];

				FCPrint[3, "OneLoop: tdec: qn==2, tdecnew=", FullForm[tdecnew], FCDoControl->oneloopVerbose];
				tdecnew = eval[ Flatten[ tdecnew,1 ] ];
				FCPrint[3, "OneLoop: tdec: qn==2, tdecnew=", tdecnew, FCDoControl->oneloopVerbose];
				If[ ($LimitTo4 === True) && (tdeclpl == 1),  (* e B11 = 2/3 *)
					tdecr = epst[ tdecr,tdecnew[[1,2]],tdi, 2/3 ]
				];
				For[ tdectj = 1,tdectj<=Length[tdecnew],tdectj++,
					tdecr = add[ tdecr,
							PaVe@@Join[tdecnew[[tdectj,1]],{tensps},{tdecml},{PaVeAutoOrder->paveautoorder,PaVeAutoReduce->paveautoreduce}],
								tdecnew[[tdectj,2]]
								];
					If[ $LimitTo4 === True,
						tdecr = tdecr /.tdi->4
					];
					];
				FCPrint[3, "OneLoop: tdec: qn==2, tdecr=", tdecr, FCDoControl->oneloopVerbose]
			];


			(* q^mu q^nu q^rho (...) *)
				If[ qn == 3,               (* The  00i - terms *)
					tdecnew = {};
					For[ tdectij = 1, tdectij <= tdeclpl, tdectij++,
						tdecnew = Append[ tdecnew, eval[
					conall[ tdecex/.NonCommutativeMultiply->Times/.
							{LorentzIndex[mudu[1],dime___]->
							LorentzIndex[mudu[2],dime],
							LorentzIndex[mudu[3],___]->tdecpl[[tdectij]]}]
					+ conall[ tdecex/.NonCommutativeMultiply->Times/.
							{LorentzIndex[mudu[2],dime___]->
							LorentzIndex[mudu[3],dime],
							LorentzIndex[mudu[1],___]->tdecpl[[tdectij]]}]
					+ conall[ tdecex/.NonCommutativeMultiply->Times/.
							{LorentzIndex[mudu[1],dime___]->
							LorentzIndex[mudu[3],dime],
							LorentzIndex[mudu[2],___]->tdecpl[[tdectij]]}]
														]
										]
						];
					If[ ($LimitTo4 === True) && (tdeclpl==2),  (* C001 = -1/6 *)
						tdecr = epst[ tdecr,tdecnew[[1]], tdi,-1/6 ];
						tdecr = epst[ tdecr,tdecnew[[2]], tdi,-1/6 ]
					];
					For[ tdec0j = 1, tdec0j <= tdeclpl, tdec0j++,
						tdecr = add[  tdecr, PaVe[0,0,tdec0j,tensps,tdecml,PaVeAutoOrder->paveautoorder,PaVeAutoReduce->paveautoreduce],
										tdecnew[[tdec0j]]  ]
						];
					For[ tdecti = 1, tdecti <= tdeclpl, tdecti++,
						For[ tdectj = 1, tdectj <= tdeclpl, tdectj++,
							For[ tdectk = 1, tdectk <= tdeclpl, tdectk++,
									tdecr = add[ tdecr,
											PaVe@@Join[Sort[{tdecti,tdectj,tdectk}],
															{tensps},{tdecml}
														],
										(eval[ tdecex/.LorentzIndex[mudu[1],___]->
														tdecpl[[tdecti]]/.
														LorentzIndex[mudu[2],___]->
														tdecpl[[tdectj]]/.
														LorentzIndex[mudu[3],___]->
														tdecpl[[tdectk]]
											]
										)
											];
									If[ $LimitTo4 === True,
										tdecr = tdecr/.tdi->4
									]
								]        ]                   ];
				FCPrint[3, "OneLoop: tdec: qn==3, tdecr=", tdecr, FCDoControl->oneloopVerbose]
				];
				If[ qn>3,
					tdecr = expr
				];
				tdecr = Expand[tdecr];

			(* end if tdecex == 0 *) ];
			FCPrint[3, "exiting tdec with ", tdecr, FCDoControl->oneloopVerbose];
			If[ !FreeQ[tdecr, Null],
				Print["Null encountered in tdec; entering DIALOG"];
				Dialog[{expr,props,Q,qn ,di,mudu,mand}]
			];
			tdecr
		]]/; expr =!= 0;

(* ************************************************************** *)

(* ********************************************************************* *)
(*                          oneloop42                                    *)
(* ********************************************************************* *)

(* ---------------------------------------------------------------- *)
(* ---------------------------------------------------------------- *)
	nterms[x_Plus] :=
		Length[x];    (*ntermsdef *)
	nterms[x_] :=
		Block[ {ntermslex = Expand[x]},
			If[ Head[ntermslex]===Plus,
				ntermslex = Length[ntermslex],
				If[ x===0,
					ntermslex = 0,
					ntermslex = 1
				]
			];
			ntermslex
		];
(* ------------------------------------------------------------ *)
(* ********************************************************************* *)
(* ********************************************************************* *)
(* ********************************************************************* *)
(*                          oneloop43                                    *)
(* ********************************************************************* *)


	dndummy[x__] :=
		dummy FeynAmpDenominator[x];
	vcid[x_,___] :=
		x;


(* ********************************************************************* *)
(*                          oneloop44                                    *)
(* ********************************************************************* *)

(* SetStandardMatrixElementdef *)
Options[SetStandardMatrixElements] = {WriteOut -> False};
SetStandardMatrixElements[rx_List,en_:{}, op___Rule] :=
	Block[ {links = {},nmat,mat,ix,i,ii,j,sup,newli = {}, ops, enm,
				savmem,neweq,mati,set,isos,isolspc,nullll,x,x2,sumand,
				mati1, set2, filename, temp},
		ops = {op};
		enm = en;
		If[ {ops}==={} || (!FreeQ[enm, WriteOut]),
			ops = en;
			enm = {}
		];
		filename = WriteOut /. ops /. Options[SetStandardMatrixElements];
		If[ StringQ[filename],
			file = FileType[filename]
		];
		If[ ValueQ[file] && (file === File),
			temp = Get[filename];
			temp = Select[temp, !FreeQ[#,Spinor]&]
		];
		If[ Length[temp]>0,
			temp/.Literal->Identity/.RuleDelayed->Set;
			FCPrint[2, "loading old matrixelementdefinitions from ", filename, FCDoControl->oneloopVerbose];
			FCPrint[3, "lold matrixelementdefinitions: ", temp, FCDoControl->oneloopVerbose],
			savmem = $MemoryAvailable;
			$MemoryAvailable = 0;
			x = {};
			For[ix = 1, ix <= Length[rx], ix ++,
				If[ FreeQ[rx[[ix,1]], Plus],
					x = Prepend[x,{rx[[ix,1]],
									StandardMatrixElement@@Flatten[{rx[[ix,2]]}]
								} ],
					x = Append[x, {rx[[ix,1]],
									StandardMatrixElement@@Flatten[{rx[[ix,2]]}]
								} ]
				]
			];
			x = Flatten[x, 1];
			If[ Cases[x, DOT[Spinor[a_,_,_] , (___) , Spinor[b_,_,_]] *
							DOT[Spinor[c_,_,_] , (___) , Spinor[d_,_,_]]
						] =!= {},
				x = x /. DiracGamma[6] -> (1/2 + 1/2 DiracGamma[5]);
				x = x /. DiracGamma[7] -> (1/2 - 1/2 DiracGamma[5])
			];
			FCPrint[2, Length[x], FCDoControl->oneloopVerbose];
			FCPrint[2, "enm = ", enm, FCDoControl->oneloopVerbose];
			FCPrint[2, "ops= ", ops, FCDoControl->oneloopVerbose];
			If[ enm==={},
				mat = DiracSimplify[ x ]//Expand//DiracOrder//Contract,
				mat = DiracSimplify[ x/.enm ]//Expand//DiracOrder//Contract
			];
			nmat = Expand[ ExpandScalarProduct[mat] ]//smalld;
			nmat = nmat /. DOT -> spinorsandpairs;
			isos[b_spinorsandpairs] :=
				b;
			isos[a_?NumberQ] :=
				a;
			isos[a_ b_spinorsandpairs] :=
				b isos[a];
			isolspc[xx_] :=
				Map[ isos,
				collin[ xx, spinorsandpairs, True] + nullll
				]/.nullll->0;
			mat = Table[ {isolspc[ nmat[[2 ii - 1]] ],
						nmat[[2 ii]]},{ii,1,Length[nmat]/2}
						];
			pat[x_,_] :=
				x;
		(* Need this for pattern in the SME's, e.g., 4-fermion processes *)
			set2[a_, b_] :=
				Set @@ {a, b/.Pattern->pat};
			For[ i = 1, i<=Length[mat],i++,
			FCPrint[2, "i = ", i, " out of ", Length[mat], FCDoControl->oneloopVerbose];
			mat = Expand[ (mat//ExpandScalarProduct)/.spinorsandpairs->DOT ];
			mat = mat /. DOT -> spinorsandpairs;
			mati1 = Expand[isolspc[mat[[i,1]]]];
			For[j = 1,j<=nterms[mati1],j++,
				If[ nterms[mati1] === 1,
					sumand = mati1,
					sumand = mati1[[j]]
				];
				FCPrint[2, "sumand = ", sumand, FCDoControl->oneloopVerbose];
				If[ !(FreeQ[sumand,spinorsandpairs]),
					sup = PartitHead[ sumand,spinorsandpairs]//Expand;
					If[ (!MemberQ[ links,sup[[2]]/.spinorsandpairs->DOT/.
													Pair -> bier ]) &&
						Head[ sup[[2]] ] === spinorsandpairs,
						FCPrint[2, "o.k1", FCDoControl->oneloopVerbose];
						links =  Append[ links, sup[[2]]/.spinorsandpairs->DOT/.
															Pair -> bier
										]//Expand;
						neweq = set[ sup[[2]], Together/@(collin[
									Expand[
										((mat[[i,2]]-mati1+sumand )/sup[[1]]
											)/.isos->Identity
											],spinorsandpairs ,True
													]
											)
									];
			(* Avoid things like  a=a *)
						If[ (neweq[[1]] - neweq[[2]]) =!= 0,
							FCPrint[2, "setting", FCDoControl->oneloopVerbose];
							newli = Append[ newli,neweq/.set->set2 ]//Expand;
							j = nterms[mati1]+1
						];
					]
				]
			]
					](* i - loop *);
			FCPrint[2, "Solving the system of linear equations for standard
				matrix elements", FCDoControl->oneloopVerbose];

			(*
			(* This takes care of the fact that  1 = Gamma6 + Gamma7 *)
			If[ (!FreeQ[ rx, DiracGamma[6] ]) || (!FreeQ[ rx, DiracGamma[7] ]),
				SetStandardMatrixElements[
					rx/.DiracGamma[6]->(1/2 + DiracGamma[5]/2)/.
						DiracGamma[7]->(1/2 - DiracGamma[5]/2), enm]
				];
			*)
			$MemoryAvailable = savmem;
			If[ StringQ[filename],
				Put @@ {DownValues[spinorsandpairs], filename}
			]
		];
		newli
	];
(* ------------------------------------------------------------ *)

GetOneLoopResult[x_, li_List] :=
	Block[ {name, list,new, lenli},
		name = ToString[x];
		list = Table[StringJoin[name, "N",li[[i]]//ToString, ".m"], {i, Length[li]}];
		none[__,y_] :=
			ToExpression[ StringJoin[ Rest[ Characters[ToString[y]] ] ]];
		new = 0;
		lenli = Length[li];
		For[j = 1, j <= lenli, j++,
			FCPrint[1, "loading #  ", j, " out of ", lenli, FCDoControl->oneloopVerbose];
			Get[list[[j]]];
			];
		For[j = 1, j <= lenli, j++,
			FCPrint[1, "summing # ", j, " out of ", lenli, FCDoControl->oneloopVerbose];
			new = new + Expand[ DownValues[OneLoopResult][[j,2]],
								StandardMatrixElement ];
			];
		new
	];

FCPrint[1, "OneLoop.m loaded.", FCDoControl->oneloopVerbose];
End[]
