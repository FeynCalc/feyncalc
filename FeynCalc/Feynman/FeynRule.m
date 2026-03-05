(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynRule *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 12 March '98 at 0:04 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: derivation of feynman rules via functional differentiation *)

(* ------------------------------------------------------------------------ *)




FeynRule::usage =
"FeynRule[lag, {fields}] derives the Feynman rule corresponding to the field
configuration fields of the Lagrangian lag.

FeynRule does not calculate propagator Feynman rules.

The option ZeroMomentumInsertion can be used for twist-2 and higher twist
operators.

FeynRule is not very versatile and was primarily developed for QCD
calculations. It is often more useful when dealing with bosonic fields than
with fermions. If you need a more powerful and universal solution for deriving
Feynman rules, have a look at the standalone Mathematica Package FeynRules
(not related to FeynCalc).";

InitialFunction::usage =
"InitialFunction is an option of FeynRule the setting of which is applied to
the first argument of FeynRule before anything else.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynRule`Private`"]

assumptions::usage="";
frVerbose::usage="";
schouten::usage="";
opes::usage="";
localSUND::usage="";
localSUNF::usage="";
sdummy::usage="";

(* Functions that are applied to the expression first.
Added 3/8-2000 by Frederik Orellana to allow interoperability with Phi:
InitialFunction could e.g. be set to PhiToFC *)

(* ******************************************************************** *)
lorunique[a_] :=
	lorunique[a] = LorentzIndex[FCGV[ToString[Unique["li"]]]];
lorunique[a_,dim_] :=
	lorunique[a,dim] = LorentzIndex[FCGV[ToString[Unique["li"]]],dim];
sununique[a_] :=
	sununique[a] = SUNIndex[FCGV[ToString[Unique["si"]]]];

(*Added ExplicitSUNIndex. F.Orellana, 16/9-2002*)

	pluc[xx__] :=
		If[ !FreeQ[{xx}, SUNIndex],
			Map[(#/.Plus->((Factor1 /@ Collect[Plus[##],Variables[Plus[##]]] )&))&,
				Factor1 /@ Collect2[Plus[xx], SUNIndex,ExplicitSUNIndex, Factoring -> False]],
			Map[Factor1,
				Collect2[Plus[xx], {Pair[LorentzIndex[_], LorentzIndex[_]] },
						Factoring->False] ]
		];

frex[nl_] :=
	frex[nl] = Block[ {nla = DotSimplify[nl],sdum, flag, ff, fm,ff1, ff2, cli,tem,
	nlafirst, newlorlist, lorindlist, sunindlist, newsunlist, uniquelist},


				FCPrint[1, "FeynRule: frex: Entering with ", nla, FCDoControl->frVerbose];

				sdum = SUNIndex[ToExpression[StringJoin@@(ToString /@ {Unique[System`D], "k"})]];
				flag = Select[Expand2[Select[nla, FreeQ[#, DOT]&], SUNF] + null1 + null2,
							(Count[#, SUNF[__]] === 2)&
							] /. null1 ->1 /. null2 ->0;
							If[ flag =!= 0,
					If[ Head[flag] === Times,
						ff = Select[flag, !FreeQ[#, SUNF]&],
						If[ Head[flag] === Plus,
							ff = Select[flag[[1]], !FreeQ[#, SUNF]&];
						];
					];
					If[ Length[ff] === 2,
						ff1 = List @@ ff[[1]];
						ff2 = List@@ff[[2]];
						cli = Complement[ff1, Complement[ff1, ff2]];
						If[ Length[cli] > 0,
							sdum = cli[[1]]
						];
					];
							];
				FCPrint[1, "FeynRule: frex: flag ", flag, FCDoControl->frVerbose];
			(* change 05/94 *)
				nlafirst = If[ Head[nla]===Plus,
								nla[[1]],
								nla
							];
				(* get a list of all LorentzIndex *)
				lorindlist = Cases2[nla, LorentzIndex];
				sunindlist = Cases2[nla, SUNIndex,ExplicitSUNIndex];
				(* select those which occur an even number of times *)
				newlorlist = {};
				newsunlist = {};
				For[r = 1, r <= Length[lorindlist], r++,
					If[ EvenQ[Length[Position[nlafirst, lorindlist[[r]]]]],
						AppendTo[newlorlist, lorindlist[[r]]]
					];
				];
				For[r = 1, r <= Length[sunindlist], r++,
					If[ EvenQ[Length[Position[nlafirst, sunindlist[[r]]]]],
						AppendTo[newsunlist, sunindlist[[r]]]
					]
					];
				FCPrint[1, "FeynRule: frex: newlorlist ", newlorlist, FCDoControl->frVerbose];
				FCPrint[1, "FeynRule: frex: newsunlist ", newsunlist, FCDoControl->frVerbose];

				uniquelist = Join[Table[newlorlist[[i]] ->
										(newlorlist[[i]]/.LorentzIndex -> lorunique),
										{i, Length[newlorlist]}
										],
									Table[newsunlist[[j]] ->
										(newsunlist[[j]]/.SUNIndex -> sununique),
										{j, Length[newsunlist]}
										]
								];
				FCPrint[1, "FeynRule: frex: uniquelist = ", uniquelist, FCDoControl->frVerbose];
				nla = nla /. uniquelist;
				nla = DotSimplify[nla, Expanding -> True];
				FCPrint[1, "FeynRule: frex: nla = ", nla, FCDoControl->frVerbose];
				tem = Contract[Expand2[nla /. QuantumField -> (QuantumField[##][]&) ]] + null1;

				FCPrint[1, "FeynRule: frex: tem = ", tem, FCDoControl->frVerbose];

				fm/: (fm[aa___][bb___] * fm[xx___][yy___] )   := fm[aa][bb]**fm[xx][yy];
				fm/: fm[aa___][bb___]^n_Integer?Positive :=
					(fm[aa][bb]^(n-1))**fm[aa][bb];
				tem = tem /. QuantumField -> fm /. fm -> QuantumField;
				{tem, sdum}
			];

fcis[x_] :=
	fcis[x] = FeynCalcInternal[x];

getpes[__][pe_] :=
	pe/. Momentum -> Identity;



dirdot[yy_] :=
	If[ FreeQ[yy, DOT],
		yy,
		If[ FreeQ[yy, DiracGamma],
			DotSimplify[yy, Expanding -> False],
			DiracTrick[yy]
		]
	];

Options[FeynRule] = {Anti5 -> -Infinity,
					Assumptions -> Automatic,
					Contract -> False,
					Factor1 -> False,
					FinalSubstitutions -> {},
					FCPartialD -> RightPartialD,
					FCVerbose -> False,
					Schouten -> False,
					InitialFunction -> Identity
					};

(*FeynRuledef*)
FeynRule[a_,b_ /; Head[b] =!=Rule && Head[b]=!= List, c___,
			d_ /; Head[d] =!= Rule && Head[d] =!= List, e___Rule
		] :=
	FeynRule[a, {b,c,d}, e];

FeynRule[lag_, fii_List, ru___Rule] :=
	If[ Length[lag] === 0,
		Print[lag, " does not look like a lagrangian"],
		Block[ {(*InitialFunction stuff added by F.Orellana 3/8-2000*)
		initf, nlag, temp1, temp,
				fili = fii, lfili, qli,specope,
				groupindices,
				result,fields,tfields,plist,
				vert, getsu,gsu,subs,
				qfi, qqq, oldnoncomm, onepm, onemm,

				indd, anti5,
				coup,cdp,cedepe,
				partiald,fcVerbose,
				puref, nres, nee
				},


			initf = InitialFunction /. {ru} /. Options[FeynRule];
			nlag = ExpandAll[fcis[initf[lag]]];

			If[	!FreeQ2[{lag,fii}, FeynCalc`Package`NRStuff],
				Message[FeynCalc::nrfail];
				Abort[]
			];


			FCPrint[1, "FeynRule: Entering with ", lag, FCDoControl->frVerbose];

			nlag = nlag /. SUND -> localSUND /. SUNF -> localSUNF;
			anti5    = Anti5 /. {ru} /. Options[FeynRule];
			subs     = FinalSubstitutions /. {ru} /. Options[FeynRule];
			schouten = Schouten /. {ru} /. Options[FeynRule];
			partiald = FCPartialD /. {ru} /. Options[FeynRule];
			assumptions = Assumptions /. {ru} /. Options[FeynRule];
			fcVerbose = FCVerbose /. {ru} /. Options[FeynRule];

			If [fcVerbose===False,
				frVerbose=$VeryVerbose,
				If[MatchQ[fcVerbose, _Integer?Positive | 0],
					frVerbose=fcVerbose
				];
			];

			SetOptions[CovariantD, FCPartialD -> partiald];

			$NONZERO = False;


			fields = Map[ ( QuantumField[___, #,
					Pattern @@ {Unique["dm"], ___}][___])&, #[[0, 1]]& /@ fili];

			FCPrint[1, "non-commutative expansion", FCDoControl->frVerbose];

			nlag = nlag /. FieldStrength[a__] :> FieldStrength[a, Explicit->True];

			If[ False,
				nlag = DotSimplify[nlag],
				If[ !FreeQ[nlag, CovariantD[w__/;FreeQ[{w}, Rule]
											]^hh_ /; Head[hh]=!=Integer],
					cdp /: cdp[aa__]^(h_ /; Head[h]=!=Integer) :=
						cedepe[aa, {h, Length[fii] - 2}];
					nlag = nlag /. CovariantD -> cdp /.
							{cdp[aa__] :> CovariantD[aa, Explicit->True],
							cedepe :> CovariantD
							};
					coup = CouplingConstant /. Options[CovariantD];
					nlag = DotSimplify[nlag] /.coup^(nn_ /; nn>(Length[fii]-2)) :> 0;,
					nlag = DotSimplify[nlag];
				]
			];

			FCPrint[1, "FeynRule: After DotSimplify ", nlag, FCDoControl->frVerbose];

			If[ !FreeQ[nlag, SUNDelta],
				nlag = Expand2[nlag, {SUNIndex,ExplicitSUNIndex}]/.SUNDelta-> SUNDeltaContract/.
				SUNDeltaContract->SUNDelta
			];

			FCPrint[1, "FeynRule: After SUNDeltaContract ", nlag, FCDoControl->frVerbose];

			nlag = ExpandPartialD[nlag](* /. DOT -> dotsunt /. dotsunt -> DOT*);

			FCPrint[1, "FeynRule: After applying Leibniz rule ", nlag, FCDoControl->frVerbose];

			(* check for Leibniz - sums *) (* trick17 *)

			nlag = ExpandPartialD[nlag];

			FCPrint[1, "FeynRule: After ExpandPartialD: ", nlag, FCDoControl->frVerbose];

			temp1 = Expand2[frex[nlag],QuantumField];

			FCPrint[1, "FeynRule: After frex: ", temp1, FCDoControl->frVerbose];

			temp = temp1[[1]];
			sdummy = temp1[[2]];



			vert = Select[temp, (Length[Position[#, QuantumField]]===
								Length[fields]) &];

			FCPrint[1, "FeynRule: temp: ", temp, FCDoControl->frVerbose];
			FCPrint[1, "FeynRule: sdummy: ", sdummy, FCDoControl->frVerbose];
			FCPrint[1, "FeynRule: 1st vert: ", vert, FCDoControl->frVerbose];

			tfields = fields;
			vert = vert + null1 + null2;
			While[(Length[tfields] > 0) && (Head[vert] === Plus),
					vert = Select[vert, !FreeQ[#, First[tfields]]&];
					tfields = Rest[tfields];
				];
			FCPrint[1, "FeynRule: 2nd vert: ", vert, FCDoControl->frVerbose];

			FCPrint[1, "FeynRule: vert: ", vert/.QuantumField->qfi, FCDoControl->frVerbose];


			(* there might be still a sum ... *)
			If[ Head[vert] === Plus,
				qfi[___FCPartialD, fiii_, ___LorentzIndex, ___SUNIndex|___ExplicitSUNIndex][___] :=
					qqq[fiii];
				qfi[___FCPartialD, fiii_, ___Momentum, ___SUNIndex|___ExplicitSUNIndex][___] :=
					qqq[fiii];
				qfi[___BlankNullSequence, fiii_, ___Pattern][___] :=
					qqq[fiii];
				puref = (Sort[Select[Variables[# /.   QuantumField -> qfi /. DOT -> Times /.
															NonCommutativeMultiply -> Times
												]//Flatten//Union,
													Head[#]===qqq&
										]      ] ===
											Sort[Variables[fields /. QuantumField -> qfi]]
									)&;
				vert = Select[vert, puref];
			];
			FCPrint[1, "FeynRule: vert: ", vert, FCDoControl->frVerbose];
			If[ vert === 0,
				result = 0,
				FCPrint[1, "FeynRule: vert is not zero!", FCDoControl->frVerbose];
				vert = vert /. NonCommutativeMultiply -> Times;
				vert = Expand[ SUNSimplify[dirdot[vert],Explicit->False] ];
				FCPrint[1, "functional differentiation ", FCDoControl->frVerbose];
				groupindices = Map[First,Cases2[{vert,fili},{SUNIndex,ExplicitSUNIndex,LorentzIndex}]];
				UnDeclareNonCommutative[groupindices];
				If[ Head[vert] === Plus,
					result = 0;
					For[j = 1, j <= Length[vert], j++,
							FCPrint[2, "iij of FunctionalD = ", j, " out of ", Length[vert], FCDoControl->frVerbose];
							result = result + DotSimplify[FunctionalD[vert[[j]], fili], Expanding -> False];
						],
					result = DotSimplify[FunctionalD[vert, fili],Expanding -> False];
				];
				FCPrint[1, "FeynRule: After functional differentiation ", result, FCDoControl->frVerbose];
				qli[__,el__LorentzIndex, ___][_] :=
					{el};
				lfili  = Flatten[fili /. QuantumField  -> qli];
				result = Expand2[result/.SUNDelta->SUNDeltaContract/.SUNDeltaContract->SUNDelta];
				FCPrint[1, "there are now ", Length[result], " terms", FCDoControl->frVerbose];
				result = result /. Pair -> PairContract /. Pair -> PairContract /.
							PairContract -> Pair;
				FCPrint[1, "FeynRule: After simple contractions ", result, FCDoControl->frVerbose];
				If[ !FreeQ[result, Eps],
					result = EpsEvaluate[result];
					FCPrint[1, "FeynRule: After EpsEvaluate ", result, FCDoControl->frVerbose];
				];
				If[ !FreeQ[result, DOT],
					result = dirdot[result];
					FCPrint[1, "FeynRule: After dirdot ", result, FCDoControl->frVerbose];
				];

				FCPrint[1, "FeynRule: After Cases ", result, FCDoControl->frVerbose];

				If[ Union[Cases[result, LorentzIndex[__], Infinity]] =!= Sort[lfili],
					If[ (Contract /. Options[FeynRule]) === True,
						result = result// Contract//ExpandAll;
						FCPrint[1, "FeynRule: After another contraction ", result, FCDoControl->frVerbose];
					]
				];

				If[ !FreeQ[result, Eps],
					result = EpsEvaluate[result]
				];
				If[ !FreeQ[result, DOT],
					result = DotSimplify[result, Expanding -> False]
				];
				FCPrint[1, "there are ", Length[result], " terms ", FCDoControl->frVerbose];
				If[ !FreeQ[result, SUNDelta],
					result = result /. SUNDelta -> SUNDeltaContract /. SUNDeltaContract -> SUNDelta;
				];
				FCPrint[1, "FeynRule: After serveral simplifications ", result, FCDoControl->frVerbose];
				plist =  fii /. QuantumField -> getpes /. Momentum -> Identity;

				result = I result;

				result = Expand[result];


				If[ !FreeQ[result, SUNIndex],
					result = SUNSimplify[result,Explicit->False];
					If[ !FreeQ[result, SUNT],
						result = SUNSimplify[result,Explicit->False]
					]
				];
				FCPrint[1, "FeynRule: After color simplifications ", result, FCDoControl->frVerbose];
				result = result//Expand;
				FCPrint[1, "FeynRule: After another Expand ", result, FCDoControl->frVerbose];

				(* in case the incoming momenta are a sum *)
				If[ !FreeQ[fii, Plus],
					result = ExpandScalarProduct[result]
				];
				If[ !FreeQ[result, DiracGamma[5]],
					result = Anti5[result, anti5]
				];
				result = Expand[result] /. subs;

				FCPrint[1, "FeynRule: After more simplifications ", result, FCDoControl->frVerbose];

				If[ (!FreeQ2[result, {SUNT, DiracGamma}]),

					result = ExpandScalarProduct[result /. plist[[1]] :> (-(Plus @@ Rest[plist]))]//Factor1;

					If[ !FreeQ2[result, {SUNIndex,ExplicitSUNIndex}],
						result = Collect2[result, SUNIndex,ExplicitSUNIndex, Factoring -> False, Expanding -> False];
					];
					FCPrint[1, "collect2ing done; ", FCDoControl->frVerbose];

					If[ (Length[plist]<4),
						result = Factor1[result];
					];

					result = result /. Plus :> pluc;

					FCPrint[1, "feinarbeit ", FCDoControl->frVerbose];
					If[ ((Factor1 /. {ru} /. Options[FeynRule]) === True),
						If[ (Head[result] === Plus),
							result  = Factor1[result]
						];
					];
				];

			];

			FCPrint[1, "FeynRule: Preliminary result ", result, FCDoControl->frVerbose];

			result
		]/. {localSUND :> SUND, localSUNF :> SUNF}
	];

FCPrint[1, "FeynRule.m loaded.", FCDoControl->frVerbose];
End[]
