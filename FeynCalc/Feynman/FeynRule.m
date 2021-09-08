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

enm[h_Plus, pel_List] :=
	FixedPoint[enm2[#, pel]&, h, 5];

enm2[ha_, pl_List] :=
	MemSet[enm2[ha,pl], ha] /; Head[ha] =!= Plus;
enm2[ha_Plus, pl_List] :=
	MemSet[enm2[ha,pl],
	Block[ {ttt,nn, nnli, pall = Plus@@pl, i},
		ttt = Catch[
		(* check for 1-term *)
					For[i = 1, i<=Length[pl], i++,
						If[ !FreeQ[ha, pl[[i]]],
							nn = Expand2[ExpandScalarProduct[ha /. pl[[i]]->
											(-(pall-pl[[i]]))]
										];
							If[ Head[nn] =!= Plus,
								Throw[nn]
							];
						]
						];
		(* check for smaller length *)
					nnli = {};
					For[i = 1, i<=Length[pl], i++,
						If[ !FreeQ[ha, pl[[i]]],
							nn = Expand2[ExpandScalarProduct[ha /. pl[[i]] ->
														(-(pall-pl[[i]]))]
										];
							If[ (NTerms[nn] < NTerms[ha]),
								AppendTo[nnli, nn];
							]
						];
				];
					If[ Length[nnli] > 0,
						Throw[Sort[nnli][[1]]]
					];
		(* check for equal length *)
					nnli = {ha};
					For[i = 1, i<=Length[pl], i++,
						If[ !FreeQ[ha, pl[[i]]],
							nn = Expand2[ExpandScalarProduct[ha /. pl[[i]] ->
														(-(pall-pl[[i]]))]
										];
							If[ (NTerms[nn] ===  NTerms[ha]),
								AppendTo[nnli, nn];
							]
						];
				];
					Throw[Last[Sort[nnli]]];
	(*
				If[SelectNotFree[nnli,-1] =!= {},
					Throw[Sort[SelectNotFree[nnli,-1]][[1]]],
					Throw[Sort[nnli][[1]]]
					];
	*)
					ha
					];
		ttt
	]];
enm3[ha_Plus, pl_List] :=
	If[ $NONZERO === True,
		ha,
		MemSet[enm3[ha,pl],
			Block[ {ttt,nn, nnli, pall = Plus@@pl, i},
				ttt = Catch[
				(* check for equal length *)
							nnli = {ha};
							For[i = 1, i<=Length[pl], i++,
								If[ !FreeQ[ha, pl[[i]]],
									nn = Expand2[ExpandScalarProduct[ha /. pl[[i]] ->
																(-(pall-pl[[i]]))]
												];
									If[ (NTerms[nn] === NTerms[ha]),
										AppendTo[nnli, nn];
									]
								];
				];
							If[ SelectNotFree[nnli,-1] =!= {},
								Throw[Sort[SelectNotFree[nnli,-1]][[1]]],
								Throw[Sort[nnli][[1]]]
							];
							ha
							];
				ttt
			]]
	];


enmomcon[aa_, pli_List] :=
	If[ $NONZERO === True,
		aa,
		PowerSimplify[
		aa /. {Power2[h_Plus,w_] :> Power2[enm2[h,pli],w]} /.
				{Power2[-1,po_] :> (-1)^po} /.
				{Power[h_Plus,w_ /; Head[w] =!= Integer] :>
				Power[enm2[h,pli],w]},Assumptions->assumptions]
	];

enmomcon3[aa_, pli_List] :=
	aa /. Power2[h_Plus,w_] :>
	PowerSimplify[Power2[enm3[h,pli],w],Assumptions->assumptions];

sumtrick1[ex_, {i_,0,j_}, {j_, 0, n_}] :=
	sumtrick1[ex, {j,0,n}, {i,0,j}];

sumtrick1[ex_, {j_, 0, n_}, {i_, 0, j_}] :=
	MemSet[sumtrick1[ex, {j,0,n}, {i,0,j}],
	OPESumSimplify[
	OPESum[
	PowerSimplify[
		PowerSimplify[ex,Assumptions->assumptions] /. { ((-1)^(i+em_.)*
							pow_[(a_+b_) , w_ /; (w===(j-i))] *
							pow_[a_ /; !FreeQ[a, OPEDelta],
								v_ /; (v === (n-j))] *
							pow_[c_ /; !FreeQ[c, OPEDelta],
								i]
							) :>
							((-1)^(n+em+j) pow[a+b, j-i] pow[a, i] pow[c, n-j]
							) /; ( ((pow === Power) || (pow === Power2)) &&
									FreeQ[em, i]
								)
							},Assumptions->assumptions
				], {j,0,n}, {i, 0, j}],Assumptions->assumptions ]];

sumtrick1[ex_, {i_, 0, n_}] :=
	MemSet[sumtrick1[ex, {i,0,n}],
	OPESumSimplify[ OPESum[
	PowerSimplify[
	PowerSimplify[ex,Assumptions->assumptions] /. {(-1)^(i+em_.)*
	(
	pow_[a_ /; !FreeQ[a, OPEDelta],
	v_ /;(Variables[v]==={i})] *
	pow_[b_ /; !FreeQ[b, OPEDelta],
	w_/;(Variables[w] === Variables[{i,n}])]
	) :> (((-1)^n (-1)^(i+em) pow[a, v + n - 2 i]*
					pow[b, w - n + 2 i]
	) /. (-1)^(y_ + 2 z_) :> ((-1)^y /;
		MemberQ[{OPEi,OPEj,OPEk,OPEl,OPEm},z]
							)
	) /; (!OrderedQ[{a,b}]) &&
	((pow === Power) || (pow === Power2)) &&
	FreeQ[em, i]
	},Assumptions->assumptions], {i,0,n}],Assumptions->assumptions]];

suback[a_,___] :=
	a;
(*
sutr[_][x_,_,__] := x;
sutr[pli_List][x_, {i_, 0, j_}] :=
*)

sutr[pli_List][x_, {i_, 0, j_}, b__] :=
	MemSet[sutr[pli][x,{i,0,j}, b],
	Block[ {xx = x, te},
		te =  If[ FreeQ[x, (-1)^(_. i + _.)],
				enmomcon[x, pli],
				x
			];
		(*
		If[te =!= x,
		Print["sutred ", xx//FeynCalcForm , "  -->  ",te//FeynCalcForm];
		];*)
		te
	]];

sutr[pli_List][x_, {i_, 0, j_}] :=
	MemSet[sutr[pli][x,{i,0,j}],
	Block[ {xx = x, te},
		te =  If[ FreeQ[x, (-1)^(_. i + _.)],
				enmomcon3[x, pli],
				x
			];
		(*
		If[te =!= x,
		Print["sutred ", xx//FeynCalcForm , "  -->  ",te//FeynCalcForm];
		];*)
		te
	]];


opesback[y_Times, fi_List] :=
	SelectFree[y, {Power2, OPEi, OPEj, OPEk, OPEl, OPEm}] *
	opesbac3[SelectNotFree[y, {Power2, OPEi, OPEj, OPEk, OPEl, OPEm}], fi];

opesbac3[a__] :=
	MemSet[opesbac3[a], opesbac2[a]];

opesbac2[1,__] = 1;

opesbac2[y_ opes[b__List], fi_List] :=
	( sumtrick1[sutr[fi /. QuantumField -> getpes
					][enmomcon[y, fi /. QuantumField -> getpes], b]
						, b
				] /. OPESum -> sumtrick1 /. sumtrick1 -> suback
	) /; FreeQ[y, opes];

opesbac2[y_ opes[b__List] opes[c__List], fi_List] :=
	(
	enmomcon[
	sutr[fi /. QuantumField -> getpes
		][
			sumtrick1[ enmomcon[y, fi /. QuantumField -> getpes
							], b, c
					] /. OPESum -> sumtrick1 /. sumtrick1 -> suback,
					b, c
		],  fi /. QuantumField -> getpes
			] /. OPESum -> sumtrick1 /. sumtrick1 -> suback
	) /; FreeQ[y, opes];

opesbac2[y_ opes[a__List] opes[b__List] opes[c__List], fi_List] :=
	( sumtrick1[enmomcon[y, fi /. QuantumField -> getpes], a, b, c
				] /. OPESum -> sumtrick1 /. sumtrick1 -> suback
	) /; FreeQ[y, opes];

opesbac2[y_ /; FreeQ[y, opes], fi_List] :=
	enmomcon5[enmomcon[y, fi/.QuantumField -> getpes],
						fi/.QuantumField -> getpes
			];
(* only for Eps *)
enmomcon5[xx_, pli_List] :=
	If[ ((Plus@@pli) === 0) || (Head[Expand[Last[pli]]] === Plus) ||
	FreeQ[xx, Eps],
		PowerSimplify[xx,Assumptions->assumptions],
		PowerSimplify[ExpandScalarProduct[xx /. Last[pli] :>
						(-(Plus@@Drop[pli,-1]))],Assumptions->assumptions]
	];

dirdot[yy_] :=
	If[ FreeQ[yy, DOT],
		yy,
		If[ FreeQ[yy, DiracGamma],
			DotSimplify[yy, Expanding -> False],
			DiracTrick[yy]
		]
	];
opsum[y_] :=
	y //. {OPESum[w_ /; FreeQ[SelectFree[w, OPESum], Binomial], b__List] :>
		(w opes[b]) /; (Head[w] === Times || Head[w] === OPESum ||
						Head[w] === DOT),
		OPESum[b__List] :> opes[b]
		};

Options[FeynRule] = {Anti5 -> -Infinity,
					Assumptions -> Automatic,
					Contract -> False,
					Factor1 -> False,
					FinalSubstitutions -> {},
					FCPartialD -> RightPartialD,
					FCVerbose -> False,
					Schouten -> False,
					ZeroMomentumInsertion -> True,
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
				plho,opsumb,opsumb2,
				$binindices, indd, oplei,anti5,
				leib,coup,cdp,cedepe,opexbin,
				zeromomentum,partiald,fcVerbose,
				sumBinomial, puref, nres, nee
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
			zeromomentum = ZeroMomentumInsertion /. {ru} /. Options[FeynRule];
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

			(* somewhat hacky, ... *)
			If[ zeromomentum === False,
				$NONZERO = True,
				$NONZERO = False
			];


			(* remeber which indices are around *)
			$binindices = {};
			oplei[Binomial[up_, ind_] w_, {ind_, 0, up_}] :=
				( If[ !MemberQ[$binindices, ind],
					AppendTo[$binindices, ind]
				];
				sumBinomial[up, ind] w);
			leib[y_] :=
				y /. OPESum -> oplei /. oplei -> OPESum;
			fields = Map[ ( QuantumField[___, #,
					Pattern @@ {Unique["dm"], ___}][___])&, #[[0, 1]]& /@ fili];
			If[ !FreeQ[nlag, Sum],
				nlag = nlag /. Sum -> OPESum
			];
			FCPrint[1, "non-commutative expansion", FCDoControl->frVerbose];
			If[ !FreeQ[nlag, OPESum],
				nlag = opsum[nlag]
			];




			If[ !FreeQ[nlag, OPEDelta],
				nlag = nlag /. FCPartialD[Momentum[OPEDelta]]^(mm_/; Head[mm]=!=Integer):>
								FCPartialD[Momentum[OPEDelta]^mm]
			];
			If[ True,
				nlag = nlag /. FieldStrength[a__] :>
								FieldStrength[a, Explicit->True];
			];
			(* CHANGE 28.6.94 *)


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
			If[ !FreeQ[nlag, Binomial],
				nlag  = leib[nlag]//opsum
			];
			nlag = ExpandPartialD[nlag];

			FCPrint[1, "FeynRule: After ExpandPartialD: ", nlag, FCDoControl->frVerbose];

			temp1 = frex[nlag];

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
				puref = (Sort[Select[Variables[# /.   QuantumField -> qfi /.
															(*Plus-> List /.*)
															{OPESum[aaa_,__]:>aaa} /.
															DOT -> Times /.
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
							result = result +
							DotSimplify[FunctionalD[vert[[j]], fili],
										Expanding -> False
										] /. (a_ /; (a =!= (-1)))^
							(w_ /; (Head[w] =!= Integer)) :> Power2[a, w];
						];,
					result = DotSimplify[FunctionalD[vert, fili],Expanding -> False
										] /. (a_ /; (a =!= (-1)))^
							(w_ /; (Head[w] =!= Integer)) :> Power2[a, w];
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
						result = result// Contract;
						FCPrint[1, "FeynRule: After another contraction ", result, FCDoControl->frVerbose];
					]
				];
				result = PowerSimplify[result /. subs,Assumptions->assumptions];
				FCPrint[1, "FeynRule: After PowerSimplify ", result, FCDoControl->frVerbose];

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
				(*
				If[ Length[fili] === 2,
					result = dirdot[result];
					If[!FreeQ[result, DiracGamma] ]
					]
				*);
				(*
				If[Length[fili] > 2, result = I result];
				result = I result;
				*)
				result = I result;

				(* FISHY!!!!!!!!!!!!*)
				If[ (!FreeQ[result, OPEDelta])(* && (Length[plist] < 4)*),
					result = -(*I*) result;
				];
				result = Expand[result] /. Power2 -> Power;
				(*change Nov 2003*)
				If[ !FreeQ[result, SUNIndex],
					result = SUNSimplify[result,Explicit->False];
					If[ !FreeQ[result, SUNT],
						result = SUNSimplify[result,Explicit->False]
					]
				];
				FCPrint[1, "FeynRule: After color simplifications ", result, FCDoControl->frVerbose];
				result = PowerSimplify[result,Assumptions->assumptions]//Expand;
				FCPrint[1, "FeynRule: After another PowerSimplify ", result, FCDoControl->frVerbose];
				If[ !FreeQ[result, sumBinomial],
					$binindices = Reverse[ Sort[$binindices] ];
					opexbin[a_, b_] :=
						Factor2[PowerSimplify[SymbolicSum[a,b],Assumptions->assumptions
							]             ] /; !FreeQ[a,Binomial];
					For[r = 1, r <= Length[$binindices], r++,
					FCPrint[1, "r = ", r, " out of ", Length[$binindices], FCDoControl->frVerbose];
					indd = $binindices[[r]];
					If[ !FreeQ[result, sumBinomial],
						Clear[opsumb, opsumb2];
						opsumb[xx_Plus] :=
							Map[opsumb, xx];
						opsumb[xx_] :=
							If[ FreeQ[xx, indd],
								xx,
								SelectFree[xx, indd] *
								opsumb2[SelectNotFree[xx,indd]]
							];
						opsumb2[xx_. sumBinomial[up_, indd]] :=
							SymbolicSum2[xx Binomial[up,indd], {indd,0,up}
										] /. SymbolicSum2 -> SymbolicSum /.
							SymbolicSum ->  OPESum;
						result = opsumb[result] /. opsumupv -> OPESum;
						If[ Length[result] < 500,
							FCPrint[1, "before OPESumSimplify", FCDoControl->frVerbose];
							result = OPESumSimplify[result, Assumptions->assumptions];
						];
						result = result /. OPESum -> opexbin /. opexbin -> OPESum;
					];
					];
					If[ !FreeQ[result, opes],
						result = Collect2[result, opes, Factoring->False]
					];
				];
				FCPrint[1, "FeynRule: After working out sumBinomial ", result, FCDoControl->frVerbose];
				result = result /. (a_ /; (a =!= (-1)))^
						(w_ /; (Head[w] =!= Integer)) :> Power2[a, w];
				If[ zeromomentum === True,
					If[ !FreeQ[result, OPEDelta],
						powsu[aa_Plus] :=
							Power2[ExpandScalarProduct[Plus[aa] /.
								plist[[1]] :> (-(Plus @@ Rest[plist]))
								]];
						result = PowerSimplify[result /. Power2 -> powsu /. powsu -> Power2,Assumptions->assumptions];
					];
				];
				FCPrint[1, "FeynRule: After another PowerSimplify ", result, FCDoControl->frVerbose];
				If[ !FreeQ[result, Power2],
					If[ Head[result] =!= Plus,
						result = opesback[result, fii],
						nres = 0;
						For[r = 1, r<=Length[result], r++,
						FCPrint[1, "r = ", r, FCDoControl->frVerbose];
						nee = opesback[result[[r]], fii];
						nres = nres  + nee;
							];
						result = nres;
					]
				];
				FCPrint[1, "FeynRule: After opesback ", result, FCDoControl->frVerbose];

				(* in case the incoming momenta are a sum *)
				If[ !FreeQ[fii, Plus],
					result = ExpandScalarProduct[result]
				];
				If[ !FreeQ[result, DiracGamma[5]],
					result = Anti5[result, anti5]
				];
				result = Expand[result] /. {(-1)^a_      :> PowerSimplify[(-1)^a,Assumptions->assumptions],
											Power2[-1,a] :> PowerSimplify[(-1)^a,Assumptions->assumptions]
											} /. subs;

				FCPrint[1, "FeynRule: After more simplifications ", result, FCDoControl->frVerbose];

				If[ (!FreeQ2[result, {SUNT, OPESum, DiracGamma}]) && Length[Cases2[result, OPESum]] <= 1,
					If[ zeromomentum === True,
						result = Collect2[ExpandScalarProduct[result /.
								plist[[1]] :> (-(Plus @@ Rest[plist]))],
										OPESum]//Factor1;
					];
					result = result /. Power2 -> Power,

					If[ !FreeQ2[result, {SUNIndex,ExplicitSUNIndex}],
						result = Collect2[result, SUNIndex,ExplicitSUNIndex, Factoring -> False, Expanding -> False];
					];
					FCPrint[1, "collect2ing done; ", FCDoControl->frVerbose];
					If[ LeafCount[result]<1000,
						result = OPESumSimplify[result,Assumptions->assumptions];
					];
					If[ (Length[plist]<4) && FreeQ[result, OPEDelta],
						result = Factor1[result];
					];
					If[ FreeQ[result, OPEDelta],
						result = result /. Plus :> pluc;
					];
					FCPrint[1, "feinarbeit ", FCDoControl->frVerbose];
					If[ ((Factor1 /. {ru} /. Options[FeynRule]) === True),
						If[ (Head[result] === Plus),
							result  = Factor1[Map[#/.Plus->plho&,
												Map[feinarbeit[#, plist]&, result]
												]] /. plho -> Plus,
							result  = feinarbeit[result, plist]
						];
					];
				];

			];

			FCPrint[1, "FeynRule: Preliminary result ", result, FCDoControl->frVerbose];

			If[ LeafCount[result]<10^4,
				result//PowerSimplify[#,Assumptions->assumptions]&//OPESumSimplify[#,Assumptions->assumptions]&,
				result
			]/.Power2->Power
		]/. {localSUND :> SUND, localSUNF :> SUNF}
	];
(* ******************************************************************** *)

feinarbeit[fey_Times, pl_List] :=
	SelectNotFree[fey, SUNIndex,ExplicitSUNIndex]  *
	feinarbeit[SelectFree[fey,  SUNIndex,ExplicitSUNIndex], pl] /; !FreeQ2[fey,
	{SUNIndex,ExplicitSUNIndex}];

feinarbeit[fey_ /; FreeQ2[fey, {SUNIndex,ExplicitSUNIndex}], pli_List] :=
	Block[ {uniqli,onepm, onemm, resu, legs, fop, foop, ores, nopres,
	ple,pleps,power3,schau},
		If[ FreeQ[fey, OPEDelta],
			resu = fey,
			resu = fey;
			legs = Length[pli];
			If[ FreeQ2[resu, {DiracGamma, Eps}],
				onepm /: onepm^2 = onepm;
				resu = resu /. {(-1)^OPEm :> (2 onepm -1)},
				onemm /: onemm^2 = onemm;
				resu = resu /. {(-1)^OPEm :> (-2 onemm +1)}
			];
			If[ !FreeQ[resu, OPESum] && Head[resu] === Plus && legs < 5,
				nopres = SelectFree[resu, OPESum];
				ores   = resu - nopres;
				nopres = nopres /. Power2 -> Power;
				ple[a__] :=
					Collect2[Plus[a] /. Power -> power3 /.
					{power3[ab_ /; !FreeQ[ab, OPEDelta], OPEm-1] :>
					(ab power3[ab, OPEm -2]) }, power3] /. power3 -> Power;
				pleps[xy_] :=
					xy /. Plus -> ple;
				resu = pleps[nopres//Expand] +
						Collect2[ores, OPESum, Factoring -> Factor2,
												Expanding -> True];
				If[ (!FreeQ[resu, Eps]) && schouten == True,
					FCPrint[1, "using Schouten identity ", FCDoControl->frVerbose];
					schau[y__] :=
						MemSet[schau[y],
						If[ FreeQ2[{y},{(aa_ /; !FreeQ[aa, Pair])^
									(vv_ /; Head[vv] =!= Integer),
								Power2[aaa_ /; !FreeQ[aaa, Pair],
										vvv_ /; Head[vvv] =!= Integer
									]
									}
							],
							Factor1[Schouten[Plus[y]]],
							Plus[y]
						]];
					resu  = resu /. Plus -> schau;
				];
				fop[yy_] :=
					If[ Head[yy] =!= Times,
						yy,
						SelectFree[yy, OPEDelta] *
						foop[SelectNotFree[yy, OPEDelta]]
					];
				resu = Map[fop, resu];
			];
			If[ legs<5,
				resu = Factor2[resu /. Complex[0,be_]->(be y)
								] /. {(onepm (-1)^OPEm) :>   onepm } /.
									{(onemm (-1)^OPEm) :> (-onemm)} /.
									{y^(2 a_)  :> (-1)^a} /.
									{ y :> I, (*Plus :> pluc, *)
										onepm :> ((1+(-1)^OPEm)/2),
										onemm :> ((1-(-1)^OPEm)/2)
									},
				resu = Expand[resu /. {onepm :> ((1+(-1)^OPEm)/2),
										onemm :> ((1-(-1)^OPEm)/2)
										}
								];
			];
			FCPrint[1, "Factoring done; ", FCDoControl->frVerbose];
			If[ !FreeQ[resu, $SU],
				uniqli = {};
				getsu[xx_] :=
					(AppendTo[uniqli, xx];
					gsu[xx]);
				resu = resu /. $SU -> getsu;
				uniqli = Union[uniqli];
				resu = resu /.gsu[uniqli[[1]]] -> sdummy /. gsu -> Identity;
			];
			If[ (legs < 4) && FreeQ[resu, OPEDelta],
				resu = FeynCalcExternal[resu] /. Power2 -> Power /. I^(2 a_) :> (-1)^a;
			];
			resu =  resu /. Power2 -> Power;
			FCPrint[1, "last global factoring ", FCDoControl->frVerbose];
			resu = PowerSimplify[resu /. foop -> Identity,Assumptions->assumptions];
			If[ Head[resu] === Times && !FreeQ[resu, OPEDelta],
				resu = PowerSimplify[SelectFree[resu, OPEDelta] /.
				{(-1)^m_ (1+(-1)^m_) :> (1+(-1)^m) /; MemberQ[{OPEi,OPEj,OPEm},m]},Assumptions->assumptions
									] (resu/SelectFree[resu, OPEDelta])
			];
			If[ legs < 3,
				resu = Factor2[resu]
			];
		];
		If[ !FreeQ[resu, _Complex^_],
			resu = PowerSimplify[resu,Assumptions->assumptions]
		];
		resu
	];


(*
InvertScalar[exp_ /; FreeQ2[exp, {SUNDeltaContract, DiracGamma, lor}]
			] := 1/exp;

InvertFermion[n_. DiracGamma[Momentum[pe_]] + m_.] :=
	-I (n GS[pe] - m)/(n^2 ScalarProduct[pe, pe] - m^2);

InvertBoson[exp_, k_, m_, n_] := Block[{geteq, la},
geteq[x_ ] := ({#[[1]]==0, #[[2]]==0}& @
					Collect2[Contract[x], lor]) /.
						Pair[___, lor[__], ___] -> 1;
(A MT[m, n] + B FV[k, m] FV[k, n]) /.
	Solve[geteq[exp (A MT[m, la] +
					B FV[k, m] FV[k, la]) -
							MT[la, n]], {A, B}][[1]]
										];
*)

FCPrint[1, "FeynRule.m loaded.", FCDoControl->frVerbose];
End[]
