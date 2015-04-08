(* ::Package:: *)



(* :Title: DiracTrace *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 February '99 at 0:06 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac trace calculation *)

(* ------------------------------------------------------------------------ *)


DiracTrace::usage =
"DiracTrace[expr] is the head of Dirac traces. \
Whether the trace is  evaluated depends on the option \
DiracTraceEvaluate. See also TR. \
The argument expr may be a product of Dirac matrices or slashes \
separated by the Mathematica Dot \".\" (assuming DOT has been set to Dot).
The option Factoring determines the final function to be applied. If
it is set to False no simplification is done.
It might be set to, e.g., Factor or Factor2 to get simpler results.
With the default setting Factoring -> Automatic factorization is performed on
not too long (LeafCount[ ] < 5000 ) expressions.
";

DiracTrace::noncom =
"Wrong syntax! The Dirac trace of `1` contains Dirac matrices multiplied via \
Times (commutative multiplication) instead of DOT (non-commutative multiplication). \
Evaluation aborted!";


DiracTrace::ndranomaly =
"You are using naive dimensional regularization (NDR), such that in D dimensions \
gamma^5 anticommutes with all other Dirac matrices. In this scheme \
(without additional prescriptions) it is not possible to compute traces with an \
odd number of gamma^5 unambiguously. The trace of `1` is illegal in NDR. \
Evaluation aborted!";

DiracTrace::ilsch =
"The settings $BreitMaison=`1`, $Larin=`2` and $West=`3` do not describe a valid scheme for treating gamma^5 \
in D dimensions. Evaluation aborted!.";

DiracTrace::fail =
"DiracTrace failed to compute the trace of `1`. Evaluation aborted!.";


(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

(*spursav is also used in TR*)
spursav

End[]

Begin["`DiracTrace`Private`"]

scev[a__] :=
	scev[a] = ExpandScalarProduct[a];

Options[DiracTrace] = {
	EpsContract -> False,
	Factoring -> Automatic,
	FeynCalcExternal -> False,
	Mandelstam    -> {},
	PairCollect    -> True,
	DiracTraceEvaluate-> False,
	Schouten-> 0,
	LeviCivitaSign:> $LeviCivitaSign,
	TraceOfOne -> 4
};


DiracTrace /:
	MakeBoxes[DiracTrace[expr__, OptionsPattern[]], TraditionalForm]:=
	RowBox[{"tr","(",TBox[expr], ")"}]


(* gamma67backdef: reinsertion of gamma6 and gamm7 *)
gamma67back[x_] :=
	x/.DiracGamma[6]->( 1/2 + DiracGamma[5]/2 )/. DiracGamma[7]->( 1/2 - DiracGamma[5]/2 );

DiracTrace[x:Except[_HoldAll], opts:OptionsPattern[]] :=
	(Message[DiracTrace::noncom, InputForm[x]];
	DiracTrace[HoldAll[x]])/;
	!FreeQ2[FCI[x], {
	(*Times instead of DOT between two Dirac or SU(N) matrices*)
	(DiracGamma | DiracGammaT)[a__]*(DiracGamma | DiracGammaT)[b__],
	SUNT[a__]*SUNT[b__],
	(*Two DOT objects multiplied with each other via Times, unless those are closed spinor chains*)
	DOT[a:Except[_Spinor]...,(DiracGamma | DiracGammaT )[b__],c:Except[_Spinor]...]*
	DOT[d:Except[_Spinor]...,(DiracGamma | DiracGammaT)[e__],f:Except[_Spinor]...],
	(*Open spinor chains*)
	DOT[a_Spinor,b:Except[_Spinor]...],
	(*DOT object multiplied by a Dirac or SU(N) matrix via Times*)
	DOT[a:Except[_Spinor]...,(DiracGamma | DiracGammaT)[b__],c:Except[_Spinor]...]*
	(DiracGamma | DiracGammaT)[d__]
	}] && OptionValue[FilterRules[{opts}, Options[DiracTrace]],DiracTraceEvaluate];


DiracTrace[0,___] :=
	0;

DiracTrace[a:Except[_HoldAll] /; (FreeQ[a, DiracGamma] && !FreeQ[a, DiracGammaT]), b___?OptionQ] :=
	DiracTrace[(a//Transpose)//Reverse, b];

DiracTrace[a:Except[_HoldAll]..., x_,y_, z___] :=
	DiracTrace[a,x.y,z]/;FreeQ2[y,{Rule,BlankNullSequence}]&& FreeQ2[x,{Rule,BlankNullSequence}];

(*DiracTracedef*)
fcit[y_] :=
	If[ True,
		FeynCalcInternal[DiracSigmaExplicit[y]]//DiracGammaExpand,
		FeynCalcInternal[y]//DiracGammaExpand
	];

fcex[ops___?OptionQ][z_] :=
	If[ (FeynCalcExternal /. {ops} /. Options[DiracTrace]),
		FeynCalcExternal[z],
		z
	];

(*
DiracTrace[x_,op___?OptionQ] := fcex[op][
						( diractraceevsimple[
					fcit[x] ,Flatten[{op}]   ] /. diractraceevsimple -> diractraceev /.
						diractraceev->diractraceev2
						)              ]/;
(DiracTraceEvaluate/.{op} /. (Join[{op},Options[DiracTrace]]//Flatten)
) === True;
*)
(* 2005-02-05 *)
(* There is a problem with passing the options (e.g. for LeviCivitaSign),
	so instead of passing them all through the global options will get changed
	temporarily
*)

DiracTrace[x:Except[_HoldAll], op___?OptionQ] :=
	Block[ {diTres, globalstartops = Options[DiracTrace] },
		SetOptions[DiracTrace,FilterRules[{op},Options[DiracTrace]]];
		diTres = (fcex[op][( diractraceevsimple[fcit[x] ,Flatten[{op}]] /.
			diractraceevsimple -> diractraceev /. diractraceev -> diractraceev2)]);
		SetOptions[DiracTrace, Sequence@@globalstartops];
		If[ !FreeQ[diTres,DiracGamma],
			Print["Problem!!!"];
		];
		diTres
	]/;((DiracTraceEvaluate/.{op} /. (Join[{op},Options[DiracTrace]]//Flatten)) === True && FreeQ[x,SUNT]);

diractraceevsimple[x_,{opt___}] :=
	(x(TraceOfOne /. {opt} /. Options[DiracTrace]))/; FreeQ[x, DiracGamma];

diractraceevsimple[y_ DOT[x_,z__],{opt___}] :=
	(y diractraceevsimple[DOT[x,z],{opt}])/; FreeQ[y, DiracGamma];

diractraceevsimple[x_Plus , {opt___}] :=
	Map[diractraceevsimple[#,{opt}]&, x];

diractraceevsimple[DOT[x___], {___}] :=
	(If[ FreeQ[#,LorentzIndex],
		#,
		#/.Pair->PairContract/.PairContract->Pair
	]&[diractraceev@@DOT[x]])/;
	(MatchQ[List@@DOT[x], { DiracGamma[(LorentzIndex | Momentum)[_,_],_]..}] ||
	MatchQ[List@@DOT[x], { DiracGamma[(LorentzIndex | Momentum)[_]]..}] ||
	MatchQ[List@@DOT[x], { DiracGamma[(LorentzIndex | Momentum)[_,_],_]..,
		DiracGamma[5 | 6 | 7]} ] ||
	MatchQ[List@@DOT[x], { DiracGamma[(LorentzIndex | Momentum)[_]]..,
		DiracGamma[5 | 6 | 7]}]);

dirli[LorentzIndex[xx_, ___],___] :=
	xx;

diractraceev[DiracGamma[LorentzIndex[a1_,dii_],dii_],
			DiracGamma[LorentzIndex[a2_,dii_],dii_],
			DiracGamma[LorentzIndex[a3_,dii_],dii_],
			a4:DiracGamma[LorentzIndex[_,dii_],dii_]..,
			DiracGamma[LorentzIndex[a1_,dii_],dii_],
			DiracGamma[LorentzIndex[a2_,dii_],dii_],
			DiracGamma[LorentzIndex[a3_,dii_],dii_],
			a4:DiracGamma[LorentzIndex[_,dii_],dii_]..
			] :=
	(TraceOfOne /. Options[DiracTrace]) dcs[dii]@@Join[{a1,a2,a3}, {a4}/.DiracGamma->dirli,
	{a1,a2,a3}, {a4}/.DiracGamma->dirli];

dcs[dim_][x___] :=
	dcs[dim][x] = (dics[dim][x] /. dics->dc);

dc[_][] =
	1;

dics[_][] =
	1;

dics[dI_][a___, n_, n_, b___] :=
	dI dics[dI][a, b];

dics[dI_][a___, n_, z_, n_, b___ ] :=
	(2-dI) dics[dI][a, z, b];

dics[dI_][a___, n_, v_, w_, n_, b___] :=
	(dI-4) dics[dI][a, v,w, b] + 4 (dics[dI]@@({a, b}/. v -> w));

dics[dI_][a___, n_, v_, w_, z_, n_, b___] :=
	(4-dI) dics[dI][a, v,w,z, b] - 2 dics[dI][a, z,w,v,b];

dics[dI_][a___, n_, mu_, nu_, ro_,si_, n_, b___] :=
	(dI-4) dics[dI][a, mu,nu,ro,si, b] +
	2 dics[dI][a, ro,nu,mu,si,b] + 2 dics[dI][a, si,mu,nu,ro,b];

dics[dI_][a___, n_, mu_, nu_, ro_, si_, de_, n_, b___] :=
	(4-dI) * dics[dI][a, mu,nu,ro,si,de, b] - 2 dics[dI][a, mu,de,nu,ro,si, b] -
	2 dics[dI][a, mu,si,ro,nu,de, b] + 2 dics[dI][a, nu,ro,si,de,mu, b];

dicsav[dd_][x___] :=
	dicsav[dd][x] = dics[dd][x];

dc[di_][a___, mu_, lim__, mu_, b___] :=
	Expand[
		Block[ {m = Length[{lim}], i, j},
			(-1)^m ( (di-2 m) dicss[di][a,lim,b] -
			4 Sum[(-1)^(j-i) *
			If[ {lim}[[j]] === {lim}[[i]],
				di (dicss[di] @@Join[{a}, Delete[{lim}, {{i},{j}}], {b}]),
				dicss[di] @@(Join[{a}, Delete[{lim}, {{i},{j}}], {b}]/.
				({lim}[[j]]) -> ({lim}[[i]]))
			],     {i,1,m-1}, {j,i+1,m}])
		] /. dicss -> dicsav//. dics -> dcs];
(* ****************************************************** *)
							(*conalldef*)
conall[ x_,opt_:{}] :=
	Contract[x, Expanding->True, EpsContract-> (EpsContract /. opt /. Options[DiracTrace]),
	Factoring->False ];

fr567[x__] :=
	FreeQ2[FixedPoint[ReleaseHold,{x}], {DiracGamma[5],DiracGamma[6],DiracGamma[7]}];

coneins[ x_ ] :=
	MemSet[coneins[x], x/.Pair->PairContract/.PairContract->Pair ];

diractraceev[x_, opt___] :=
	Block[ {trfa = 1, enx = x},
		If[ Head[x] === Times,
			trfa = Select[x, FreeQ2[#, {DiracGamma, LorentzIndex, Eps}]&];
			enx = x / trfa;
		];
		diractraceev2[conall[enx], opt] trfa
	];

diractraceev2[x_,opt_:{}] :=
	(TraceOfOne /. opt /. Options[DiracTrace] ) * x /; FreeQ[x,DiracGamma];

diractraceev2[a_DiracGamma,b__DiracGamma] :=
	diractraceev2[ DOT @@ {a,b} ];

diractraceev2[nnx_,in_:{}] :=
	Block[ {diractrjj, diractrlnx, diractrres, diractrny = 0, mand, diractrfact, nx,
		diractrcoll, traceofone, schoutenopt, diractrnyjj, opt},
		opt = Join[ Flatten[{in}], Options[DiracTrace] ];
		mand = Mandelstam/.opt;
		diractrfact = Factoring/.opt;
		If[ diractrfact === Automatic,
			diractrfact = Function[x, If[ LeafCount[x] <  5000,
										Factor[x],
										x
									]];
		];
		diractrcoll = PairCollect/.opt;
		schoutenopt = Schouten /. opt;
		traceofone = TraceOfOne /.  opt;
		nx = Collect2[coneins[nnx], DOT, Factoring -> False];
		nx = DiracGammaCombine[nx];
		If[ Head[nx]===Plus && Length[nx] > 142,
			(*Long sums of traces*)
			diractrlnx = Length[nx];
			diractrjj = 0;
			While[ diractrjj < diractrlnx, diractrjj++;
										FCPrint[2,"diractrjj = ", diractrjj," out of ",diractrlnx];
										diractrny = diractrny +
										If[ FreeQ[nx,DiracGamma],
											diractrnyjj = nx[[diractrjj]],
											diractrnyjj = Expand2[ DiracSimplify[ nx[[diractrjj]],
														InsideDiracTrace->True, Factoring->False,
														FeynCalcInternal -> True, DiracCanonical->False], Pair];
											If[ !FreeQ[diractrnyjj, DiracGamma],
													(*DotSimplify added 16/10-2002, F.Orellana*)
												diractrnyjj = Expand2[DotSimplify[diractrnyjj,Expanding -> False] /.
												DOT->spursav, Pair];
											];
											(* the summand to be added ...*)
											diractrnyjj
										]
			],
			If[ FreeQ[nx,DiracGamma],
				(*Expression is free of Dirac matrices*)
				diractrny = nx,
				(* Standard case: First simplify as much as possible through DiracSimplify *)
				diractrny = Expand2[ DiracSimplify[ nx, InsideDiracTrace->True, Factoring->False,
				FeynCalcInternal -> True, DiracCanonical->False ], Pair];
				If[ !FreeQ[diractrny, DiracGamma],
					(* If the output of DiracSimplify still contains Dirac matrices, apply DotSimplify and try
					to evaluate the traces of Dirac matric chains via spursav *)
					diractrny = Expand2[DotSimplify[diractrny,
								Expanding -> True] /. DOT->spursav, Pair];
				];
			]
		];
		(* Apply DiracSimplify one more time *)
		If[ !FreeQ[diractrny, DiracGamma],
			diractrny = DiracSimplify[ diractrny, InsideDiracTrace->True,
			Factoring->False, FeynCalcInternal -> True, DiracCanonical->False]
		];
		If[ !FreeQ[diractrny, DiracGamma],
			Print["Problem!", SelectNotFree[diractrny,DiracGamma]];
		];
		FCPrint[2,"CH2 ", TimeUsed[]];
		(* If the output of the second DiracSimplify contains Lorentz indices, try
		to contract them *)
		If[ !FreeQ[diractrny, LorentzIndex],
			(* Special contractions for expressions that contain Levi-Civita tensors*)
			If[ !FreeQ[diractrny, Eps],
				diractrny = diractrny //. {
					Pair[LorentzIndex[a_,D], b_] Eps[c___,LorentzIndex[a_],d___] :> Eps[c,b,d],
					Pair[LorentzIndex[a_,D], b_]  Eps[c___,LorentzIndex[a_,D],d___] :> Eps[c,b,d]
				}
			];
			diractrny = diractrny /. Pair -> PairContract /. PairContract -> scev
		];
		FCPrint[2,"CH3", TimeUsed[]];
		(* Special expansion for expressions that contain Levi-Civita tensors*)
		If[ !FreeQ[diractrny, Eps],
			diractrny = EpsEvaluate[diractrny]//Expand;
			diractrny = Contract[ diractrny, EpsContract -> (EpsContract /. in /. Options[DiracTrace]),
								Schouten->schoutenopt, Expanding -> False ];
		];
		(* Factor the result, if requested *)
		If[ diractrfact===True,
			diractrres = Factor2[traceofone diractrny],
			If[ diractrfact===False,
				diractrres = traceofone diractrny,
				diractrres = diractrfact[traceofone diractrny]
			]
		];
		(* If the result should contain 2 -> 2 Mandelstam variable *)
		If[ Length[mand] > 0,
			diractrres = TrickMandelstam @@ Prepend[{mand}, diractrres]
		];
		diractrpc[x__] :=
			Plus[x]/;FreeQ[{x},Pair];
		(* If the result should be collected w.r.t Pairs *)
		If[ diractrcoll===True,
			diractrpc[x__] :=
				Collect2[ Plus[x],Pair ,Factoring -> False];
			diractrres = diractrres/. Plus -> diractrpc
		];
		diractrres
	]/; !FreeQ2[nnx,{DOT,DiracGamma}];

spursav[0 ..] :=
	0;

spursavg[x___, LorentzIndex[a_, dim_ : 4], LorentzIndex[a_, dim_ : 4], y___] :=
	(dim spursavg[x, y]) /. spursavg -> spug;

(* calculation of traces (recursively) --  up to a factor of 4 *)
spursav[x_DiracGamma,y_DiracGamma,r_DiracGamma,z_DiracGamma, DiracGamma[5]] :=
	Block[ {dirsign},
		(* this was not right: the option does actually get inherited from TR into the options of DiracTrace;
		and now (2005-02-05), this gets passed locally to Options[DiracTrace]  *)
		dirsign = LeviCivitaSign /. Options[DiracTrace];
		dirsign I Apply[ Eps, {x,y,r,z}/. DiracGamma[vl_[mp_,di___],
			di___]->vl[mp,di]]//EpsEvaluate
	];

(* there is the problem with different Gamma5-schemes ... *)
spursav[x__DiracGamma] :=
	spur[x];
	(*Added 28/2-2001 by F.Orellana. Fix to bug reported by A.Kyrielei*)
spursav[x : ((DiracGamma[__] | HoldPattern[Plus[__DiracGamma]]) ..)] :=
	MemSet[spursav[x], spur[x]];

diracga[DiracGamma[h_Integer]] :=
	DiracGamma[h];

diracga[LorentzIndex[mu_, dii_]] :=
	diracga[LorentzIndex[mu,dii],dii];

diracga[Momentum[p_, dii_]] :=
	diracga[Momentum[p, dii],dii];

spug[x___] :=
	spursav@@(Map[diracga, {x}] /. diracga -> DiracGamma);

spur[] =
	1;
spur[DiracGamma[5]] =
	0;

spur[x___,DiracGamma[5],y__] :=
	DiracSimplify[DOT[x,DiracGamma[5],y],InsideDiracTrace->True]/;(!FreeQ[{x},DiracGamma] || !FreeQ[{y},DiracGamma]);

spur[x___,DiracGamma[5],y___] :=
	0/; FreeQ[{x},DiracGamma] && FreeQ[{y},DiracGamma];

spur[x_[y__],DiracGamma[5]] :=
	0;

spur[a_[b__],x_[y__],DiracGamma[5]] :=
	0;

spur[a_[b__],c_[d__],x_[y__],DiracGamma[5]] :=
	0;

spur[a_[b__],c_[d__],x_[y__], _[__], odd__, DiracGamma[5]] :=
	0/; OddQ[Length[{odd}]];

spur[a__] :=
	(spur @@ Reverse[Transpose[{a}]]) /; (!FreeQ[{a}, DiracGammaT]) && FreeQ[{a},DiracGamma];

(* This is a definition of   Trace( 1.2.3.4. gamma[5] ) *)
spur[x_,y_,r_,z_,DiracGamma[5]] :=
	Block[ {dirsign},
		dirsign = LeviCivitaSign /. Options[DiracTrace];
		dirsign I Apply[Eps, {x,y,r,z}/.DiracGamma[vl_[mp_,dii___],___]->vl[mp,dii]]//EpsEvaluate
	];

	(* Dropped by F.Orellana, 14/1-2002.
		Dropped Kreimer scheme. According to Rolf it's wrong *)
	(*spur[m_,n_,r_,s_,l_,t_,DiracGamma[5]]:= Block[{dirsign, sres, ltr},
	If[($Kreimer === True) && (!OrderedQ[{m,n,r,s,l,t}]),
			TR[1/(TraceOfOne/.Options[DiracTrace]) DiracOrder[ DOT[m,n,r,s,
												l,t,DiracGamma[5]] ]
			],
		If[$Larin === True &&
			!FreeQ[{m,n,r,s,l,t}, DiracGamma[LorentzIndex[_,_],_]]
			,
			ltr[a1_, a2_, a3_, a4_, a5_][
				DiracGamma[LorentzIndex[in_,di___], di___]
										] :=
	Block[{f1, f2, f3,drsi},
			drsi = LeviCivitaSign /. Options[DiracTrace];
			drsi = drsi/(TraceOfOne/.Options[DiracTrace]);
(*drsi is usually -1/4 *)
			{f1, f2, f3} = LorentzIndex[#, D]& /@ Unique[{"L","L","L"}];
			TR[drsi I/6 Eps[LorentzIndex[in, di], f1, f2, f3] *
			DOT[a1,a2,a3,a4,a5,DiracGamma[f1, D] , DiracGamma[f2, D] ,
							DiracGamma[f3, D]]
			]
		];
			Which[ MatchQ[t, DiracGamma[ LorentzIndex[__], ___]],
					ltr[m,n,r,s,l][t],
					MatchQ[l, DiracGamma[ LorentzIndex[__], ___]],
					-ltr[m,n,r,s,t][l],
					MatchQ[s, DiracGamma[ LorentzIndex[__], ___]],
					ltr[m,n,r,t,l][s],
					MatchQ[r, DiracGamma[ LorentzIndex[__], ___]],
					-ltr[m,n,s,t,l][r],
					MatchQ[n, DiracGamma[ LorentzIndex[__], ___]],
					ltr[m,r,s,t,l][n],
					MatchQ[m, DiracGamma[ LorentzIndex[__], ___]],
					-ltr[n,r,s,t,l][m]
				]
		,(* nix Larin *)
		dirsign = LeviCivitaSign /. Options[DiracTrace];
		Expand[ + dirsign I (
		scev[ m//gc,n//gc ]  Apply[ Eps, {l,r,s,t}//gc ] -
		scev[ m//gc,r//gc ]  Apply[ Eps, {l,n,s,t}//gc ] +
		scev[ n//gc,r//gc ]  Apply[ Eps, {l,m,s,t}//gc ] +
		scev[ s//gc,l//gc ]  Apply[ Eps, {m,n,r,t}//gc ] +
		scev[ l//gc,t//gc ]  Apply[ Eps, {m,n,r,s}//gc ] +
		scev[ s//gc,t//gc ]  Apply[ Eps, {l,m,n,r}//gc ]
														)//EpsEvaluate
												] ] ]
		] /; $West =!= True; *)     (*spurdef*)

(* this trace has been calculated according to Larin,
	i.e. expression DiracMatrix[w8].DiracGamma[5] by
	(-I/6) LeviCivita[w8,mu,nu,la] DiracMatrix[mu,nu,la] *)
spur[w1_,w2_,w3_,w4_,w5_,w6_,w7_,w8_,DiracGamma[5]] :=
	Block[ {trsign,z1,z2,z3,z4,z5,z6,z7,z8},
		{z1,z2,z3,z4,z5,z6,z7,z8} =
		{w1,w2,w3,w4,w5,w6,w7,w8} /.DiracGamma[vl_[mp_,dii___],___]->vl[mp,dii];
		(*TODO: vl -> (vl :LorentzIndex | Momentum) *)
		trsign = LeviCivitaSign /. Options[DiracTrace];
		(* trsign is usually  =  -1 *)
		(* factor 4 is put later *)
		trsign*I*(Eps[z5, z6, z7, z8]*Pair[z1, z4]*Pair[z2, z3] -
			Eps[z4, z6, z7, z8]*Pair[z1, z5]*Pair[z2, z3] +
			Eps[z4, z5, z7, z8]*Pair[z1, z6]*Pair[z2, z3] -
			Eps[z4, z5, z6, z8]*Pair[z1, z7]*Pair[z2, z3] -
			Eps[z5, z6, z7, z8]*Pair[z1, z3]*Pair[z2, z4] +
			Eps[z3, z6, z7, z8]*Pair[z1, z5]*Pair[z2, z4] -
			Eps[z3, z5, z7, z8]*Pair[z1, z6]*Pair[z2, z4] +
			Eps[z3, z5, z6, z8]*Pair[z1, z7]*Pair[z2, z4] +
			Eps[z4, z6, z7, z8]*Pair[z1, z3]*Pair[z2, z5] -
			Eps[z3, z6, z7, z8]*Pair[z1, z4]*Pair[z2, z5] +
			Eps[z3, z4, z7, z8]*Pair[z1, z6]*Pair[z2, z5] -
			Eps[z3, z4, z6, z8]*Pair[z1, z7]*Pair[z2, z5] -
			Eps[z4, z5, z7, z8]*Pair[z1, z3]*Pair[z2, z6] +
			Eps[z3, z5, z7, z8]*Pair[z1, z4]*Pair[z2, z6] -
			Eps[z3, z4, z7, z8]*Pair[z1, z5]*Pair[z2, z6] +
			Eps[z3, z4, z5, z8]*Pair[z1, z7]*Pair[z2, z6] +
			Eps[z4, z5, z6, z8]*Pair[z1, z3]*Pair[z2, z7] -
			Eps[z3, z5, z6, z8]*Pair[z1, z4]*Pair[z2, z7] +
			Eps[z3, z4, z6, z8]*Pair[z1, z5]*Pair[z2, z7] -
			Eps[z3, z4, z5, z8]*Pair[z1, z6]*Pair[z2, z7] +
			Eps[z5, z6, z7, z8]*Pair[z1, z2]*Pair[z3, z4] -
			Eps[z2, z6, z7, z8]*Pair[z1, z5]*Pair[z3, z4] +
			Eps[z2, z5, z7, z8]*Pair[z1, z6]*Pair[z3, z4] -
			Eps[z2, z5, z6, z8]*Pair[z1, z7]*Pair[z3, z4] +
			Eps[z1, z6, z7, z8]*Pair[z2, z5]*Pair[z3, z4] -
			Eps[z1, z5, z7, z8]*Pair[z2, z6]*Pair[z3, z4] +
			Eps[z1, z5, z6, z8]*Pair[z2, z7]*Pair[z3, z4] -
			Eps[z4, z6, z7, z8]*Pair[z1, z2]*Pair[z3, z5] +
			Eps[z2, z6, z7, z8]*Pair[z1, z4]*Pair[z3, z5] -
			Eps[z2, z4, z7, z8]*Pair[z1, z6]*Pair[z3, z5] +
			Eps[z2, z4, z6, z8]*Pair[z1, z7]*Pair[z3, z5] -
			Eps[z1, z6, z7, z8]*Pair[z2, z4]*Pair[z3, z5] +
			Eps[z1, z4, z7, z8]*Pair[z2, z6]*Pair[z3, z5] -
			Eps[z1, z4, z6, z8]*Pair[z2, z7]*Pair[z3, z5] +
			Eps[z4, z5, z7, z8]*Pair[z1, z2]*Pair[z3, z6] -
			Eps[z2, z5, z7, z8]*Pair[z1, z4]*Pair[z3, z6] +
			Eps[z2, z4, z7, z8]*Pair[z1, z5]*Pair[z3, z6] -
			Eps[z2, z4, z5, z8]*Pair[z1, z7]*Pair[z3, z6] +
			Eps[z1, z5, z7, z8]*Pair[z2, z4]*Pair[z3, z6] -
			Eps[z1, z4, z7, z8]*Pair[z2, z5]*Pair[z3, z6] +
			Eps[z1, z4, z5, z8]*Pair[z2, z7]*Pair[z3, z6] -
			Eps[z4, z5, z6, z8]*Pair[z1, z2]*Pair[z3, z7] +
			Eps[z2, z5, z6, z8]*Pair[z1, z4]*Pair[z3, z7] -
			Eps[z2, z4, z6, z8]*Pair[z1, z5]*Pair[z3, z7] +
			Eps[z2, z4, z5, z8]*Pair[z1, z6]*Pair[z3, z7] -
			Eps[z1, z5, z6, z8]*Pair[z2, z4]*Pair[z3, z7] +
			Eps[z1, z4, z6, z8]*Pair[z2, z5]*Pair[z3, z7] -
			Eps[z1, z4, z5, z8]*Pair[z2, z6]*Pair[z3, z7] +
			Eps[z3, z6, z7, z8]*Pair[z1, z2]*Pair[z4, z5] -
			Eps[z2, z6, z7, z8]*Pair[z1, z3]*Pair[z4, z5] +
			Eps[z2, z3, z7, z8]*Pair[z1, z6]*Pair[z4, z5] -
			Eps[z2, z3, z6, z8]*Pair[z1, z7]*Pair[z4, z5] +
			Eps[z1, z6, z7, z8]*Pair[z2, z3]*Pair[z4, z5] -
			Eps[z1, z3, z7, z8]*Pair[z2, z6]*Pair[z4, z5] +
			Eps[z1, z3, z6, z8]*Pair[z2, z7]*Pair[z4, z5] +
			Eps[z1, z2, z7, z8]*Pair[z3, z6]*Pair[z4, z5] -
			Eps[z1, z2, z6, z8]*Pair[z3, z7]*Pair[z4, z5] -
			Eps[z3, z5, z7, z8]*Pair[z1, z2]*Pair[z4, z6] +
			Eps[z2, z5, z7, z8]*Pair[z1, z3]*Pair[z4, z6] -
			Eps[z2, z3, z7, z8]*Pair[z1, z5]*Pair[z4, z6] +
			Eps[z2, z3, z5, z8]*Pair[z1, z7]*Pair[z4, z6] -
			Eps[z1, z5, z7, z8]*Pair[z2, z3]*Pair[z4, z6] +
			Eps[z1, z3, z7, z8]*Pair[z2, z5]*Pair[z4, z6] -
			Eps[z1, z3, z5, z8]*Pair[z2, z7]*Pair[z4, z6] -
			Eps[z1, z2, z7, z8]*Pair[z3, z5]*Pair[z4, z6] +
			Eps[z1, z2, z5, z8]*Pair[z3, z7]*Pair[z4, z6] +
			Eps[z3, z5, z6, z8]*Pair[z1, z2]*Pair[z4, z7] -
			Eps[z2, z5, z6, z8]*Pair[z1, z3]*Pair[z4, z7] +
			Eps[z2, z3, z6, z8]*Pair[z1, z5]*Pair[z4, z7] -
			Eps[z2, z3, z5, z8]*Pair[z1, z6]*Pair[z4, z7] +
			Eps[z1, z5, z6, z8]*Pair[z2, z3]*Pair[z4, z7] -
			Eps[z1, z3, z6, z8]*Pair[z2, z5]*Pair[z4, z7] +
			Eps[z1, z3, z5, z8]*Pair[z2, z6]*Pair[z4, z7] +
			Eps[z1, z2, z6, z8]*Pair[z3, z5]*Pair[z4, z7] -
			Eps[z1, z2, z5, z8]*Pair[z3, z6]*Pair[z4, z7] +
			Eps[z3, z4, z7, z8]*Pair[z1, z2]*Pair[z5, z6] -
			Eps[z2, z4, z7, z8]*Pair[z1, z3]*Pair[z5, z6] +
			Eps[z2, z3, z7, z8]*Pair[z1, z4]*Pair[z5, z6] -
			Eps[z2, z3, z4, z8]*Pair[z1, z7]*Pair[z5, z6] +
			Eps[z1, z4, z7, z8]*Pair[z2, z3]*Pair[z5, z6] -
			Eps[z1, z3, z7, z8]*Pair[z2, z4]*Pair[z5, z6] +
			Eps[z1, z3, z4, z8]*Pair[z2, z7]*Pair[z5, z6] +
			Eps[z1, z2, z7, z8]*Pair[z3, z4]*Pair[z5, z6] -
			Eps[z1, z2, z4, z8]*Pair[z3, z7]*Pair[z5, z6] +
			Eps[z1, z2, z3, z8]*Pair[z4, z7]*Pair[z5, z6] -
			Eps[z3, z4, z6, z8]*Pair[z1, z2]*Pair[z5, z7] +
			Eps[z2, z4, z6, z8]*Pair[z1, z3]*Pair[z5, z7] -
			Eps[z2, z3, z6, z8]*Pair[z1, z4]*Pair[z5, z7] +
			Eps[z2, z3, z4, z8]*Pair[z1, z6]*Pair[z5, z7] -
			Eps[z1, z4, z6, z8]*Pair[z2, z3]*Pair[z5, z7] +
			Eps[z1, z3, z6, z8]*Pair[z2, z4]*Pair[z5, z7] -
			Eps[z1, z3, z4, z8]*Pair[z2, z6]*Pair[z5, z7] -
			Eps[z1, z2, z6, z8]*Pair[z3, z4]*Pair[z5, z7] +
			Eps[z1, z2, z4, z8]*Pair[z3, z6]*Pair[z5, z7] -
			Eps[z1, z2, z3, z8]*Pair[z4, z6]*Pair[z5, z7] +
			Eps[z3, z4, z5, z8]*Pair[z1, z2]*Pair[z6, z7] -
			Eps[z2, z4, z5, z8]*Pair[z1, z3]*Pair[z6, z7] +
			Eps[z2, z3, z5, z8]*Pair[z1, z4]*Pair[z6, z7] -
			Eps[z2, z3, z4, z8]*Pair[z1, z5]*Pair[z6, z7] +
			Eps[z1, z4, z5, z8]*Pair[z2, z3]*Pair[z6, z7] -
			Eps[z1, z3, z5, z8]*Pair[z2, z4]*Pair[z6, z7] +
			Eps[z1, z3, z4, z8]*Pair[z2, z5]*Pair[z6, z7] +
			Eps[z1, z2, z5, z8]*Pair[z3, z4]*Pair[z6, z7] -
			Eps[z1, z2, z4, z8]*Pair[z3, z5]*Pair[z6, z7] +
			Eps[z1, z2, z3, z8]*Pair[z4, z5]*Pair[z6, z7])
	] /; $Larin && !$BreitMaison;

	spur[x__,DiracGamma[6]] :=
		1/2 spur[x] + 1/2 spur[x,DiracGamma[5]];

	spur[x__,DiracGamma[7]] :=
		1/2 spur[x] - 1/2 spur[x,DiracGamma[5]];

	spur[x__] :=
		(DiracTrace@@(gamma67back[ {x} ]))/; !FreeQ2[{x},{DiracGamma[6],DiracGamma[7]}];

	gc[x_] :=
		x/.DiracGamma->gach;
	gach[ex_,___] :=
		ex /; Length[ex]>0;                     (*gachdef*)
	gach[n_Integer] =
		DiracGamma[n];
	(* This function handles general  Dirac traces *)
	spur[y__] :=
		Block[ {spx,le = Length[{y}],tempres,i,spurjj,tempr,
			temp2 = 0, fi,spt, resp,scx,dirsign},
			spx = ( {y}//DiracGammaExpand )/.DiracGamma->gach;
			scx[a_,b_] :=
				scev[spx[[a]],spx[[b]]];
			temp2 = Hold[spur][spx];
			FCPrint[1, "Entering spur with ", FullForm[spx]];
			resp =
			Which[
				(*Trace of an odd number of Dirac matrices without gamma^5 *)
				OddQ[le] && fr567[spx],
					0,
				(* Trace of g^i1 g^i2 *)
				le===2,
					scev[spx[[1]],spx[[2]]]/.Pair->PairContract/.PairContract->Pair,
				(* Trace of g^i1 g^i2 g^i3 g^i4 *)
				le===4,
					(scx[1,2] scx[3,4]-scx[1,3] scx[2,4]+scx[1,4] scx[2,3]
				)//Expand,
				(* Trace of g^i1 g^i2 g^i3 g^i4 g^i5 g^i6 *)
				le===6,
					(
					scx[1,6] scx[2,5] scx[3,4] - scx[1,5] scx[2,6] scx[3,4] -
					scx[1,6] scx[2,4] scx[3,5] + scx[1,4] scx[2,6] scx[3,5] +
					scx[1,5] scx[2,4] scx[3,6] - scx[1,4] scx[2,5] scx[3,6] +
					scx[1,6] scx[2,3] scx[4,5] - scx[1,3] scx[2,6] scx[4,5] +
					scx[1,2] scx[3,6] scx[4,5] - scx[1,5] scx[2,3] scx[4,6] +
					scx[1,3] scx[2,5] scx[4,6] - scx[1,2] scx[3,5] scx[4,6] +
					scx[1,4] scx[2,3] scx[5,6] - scx[1,3] scx[2,4] scx[5,6] +
					scx[1,2] scx[3,4] scx[5,6]
					)//Expand ,
				(* For traces with a higher even number of Dirac matrices without gamma^5
				use the trace reduction equation from Veltman's Gammatrica (p.255) *)
				FreeQ[spx,DiracGamma[5]],
					temp2 = 0;
					For[i = 2, i<le+1, i++,
						temp2 += ((-1)^i) * (*coneins[*)
								scev[spx[[1]],spx[[i]]] spt@@Rest[Drop[spx,{i,i}]]
												(* ] *)
						];
					Expand[ temp2/.spt->spursavg/.spursavg->spug],
				(* Here we handle traces with of type g^i1 .... g^in g^5 with n>=6*)
				FreeQ[Drop[spx,-1], DiracGamma[5]] && Length[spx] > 6,
					FCPrint[2,"Computing the chiral trace ", spx];
					Which[
						(* NDR *)
						!$Larin && !$BreitMaison,
							If[ MatchQ[SelectFree[spx,{(LorentzIndex | Momentum)[_],DiracGamma[5]}], {} ],
								(* If the trace is purely four dimensional, NDR is ok here. *)
								If[ $West,
									(* Apply West's formula (c.f. Eq 3.10 of T. H. West,
										Comp. Phys. Commun., 77 (1993) ) *)
									FCPrint[3,"The chiral trace", spx, "is computed using West's formula in NDR" ];
									temp2 = Expand[2/(Length[spx]-5) Sum[(-1)^(i+j+1) scev[spx[[i]], spx[[j]]]*
									spt@@Delete[spx,{{j},{i}}], {i,2,Length[spx]-1},{j,1,i-1}]],
									(*Apply the standard anomalous trace formula (c.f. Eq 2.18 of R. Mertig, M. Boehm,
									A. Denner. Comp. Phys. Commun., 64 (1991)) *)
									FCPrint[3,"The chiral trace", spx, "is computed using standard recursion formula in NDR" ];
									fi = LorentzIndex[Unique[]];
									temp2 = scev[spx[[le-3]],spx[[le-2]]] spt@@Append[Drop[Drop[spx,{le-3,le-2}], -1 ],
									DiracGamma[5]]- scev[spx[[le-3]],spx[[le-1]]] spt@@Append[Drop[Drop[spx,{le-3,le-3}], -2],
									DiracGamma[5]]+ scev[spx[[le-2]],spx[[le-1]]] spt@@Append[Drop[spx,-3], DiracGamma[5]] +
									( I Eps[spx[[le-3]],spx[[le-2]],spx[[le-1]],fi] *spt @@ Append[Drop[spx,-4],fi]);
								];
								temp2/.spt->spursavg/.spursavg->spug,
								(* Otherwise abort the computation, since NDR cannot handle anomalous traces without an
								additional prescription*)
								Message[DiracTrace::ndranomaly, InputForm[DOT[y]]];
								Abort[];
							],
						(* Larin *)
						$Larin && !$BreitMaison && !$West,
							FCPrint[3,"The chiral trace", spx, "is computed in Larin's scheme" ];
							{fi1, fi2, fi3} = LorentzIndex[#,D]& /@ Unique[{"a","b","c"}];
							drsi = LeviCivitaSign /. Options[DiracTrace];
							drsi = drsi/(TraceOfOne/.Options[DiracTrace]);
							(*drsi is usually -1/4 *)
							temp2 = spx /. {a___, lomo_[mUU_,di___], DiracGamma[5]} :>
							TR[ drsi I/6 Eps[lomo[mUU,di], fi1, fi2, fi3] *
							DOT @@ Map[DiracGamma[#,D]&, {a,fi1,fi2,fi3}], EpsContract->True];
							temp2/.spt->spursavg/.spursavg->spug,
						(* BMHV, standard (slow!) trace formula *)
						!$Larin && $BreitMaison && !$West,
							FCPrint[3,"The chiral trace", spx, "is computed in the BMHV scheme using the slow formula" ];
							dirsign = LeviCivitaSign /. Options[DiracTrace];
							fi = Table[LorentzIndex[ Unique[] ],{spurjj,1,4}];
							DiracTrace @@ ({y}/.DiracGamma[5]->
							(dirsign I/24 (DOT[DiracGamma[fi[[1]]],DiracGamma[fi[[2]]],
							DiracGamma[fi[[3]]],DiracGamma[fi[[4]]]]) (Eps@@fi))),
						(* BMHV West's trace formula *)
						!$Larin && $BreitMaison && !West,
							FCPrint[3,"The chiral trace", spx, "is computed in the BMHV scheme using West's formula" ];
							temp2 = Expand[2/(Length[spx]-5) Sum[(-1)^(i+j+1) *
							scev[spx[[i]], spx[[j]]] spt@@Delete[spx,{{j},{i}}],
								{i,2,Length[spx]-1},{j,1,i-1}]];
							temp2/.spt->spursavg/.spursavg->spug,
							(* Any other combination of $Larin, $BreitMaison and $West doesn't describe
							a valid scheme *)
						True,
							Message[DiracTrace::ilsch, $BreitMaison,$Larin,$West]
					],
			True,
			Message[DiracTrace::fail, FullForm[spx]]
			];
			resp
		];

FCPrint[1,"DiracTrace.m loaded."];
End[]
