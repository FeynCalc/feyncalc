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
The option Factoring determines the final function to be applied. If \
it is set to False no simplification is done. \
It might be set to, e.g., Factor or Factor2 to get simpler results. \
With the default setting Factoring -> Automatic factorization is performed on \
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
"The settings $BreitMaison=`1`, $Larin=`2` do not describe a valid \
scheme for treating gamma^5 in D dimensions. Evaluation aborted!.";

DiracTrace::fail =
"DiracTrace failed to compute the trace of `1`. Evaluation aborted!.";

DiracTrace::rem =
"Error! The trace of the original expression still contains Dirac matrices. \
Evaluation aborted!.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

(*spursav is also used in TR*)
spursav

End[]

Begin["`DiracTrace`Private`"]

diTrVerbose::usage="";
west::usage="";
unitMatrixTrace::usage="";
traceNo5Fun::usage="";
trace5Fun::usage="";

Options[DiracTrace] = {
	Contract -> 400000,
	EpsContract -> False,
	Factoring -> Automatic,
	FeynCalcExternal -> False,
	Mandelstam    -> {},
	PairCollect    -> False,
	DiracTraceEvaluate-> False,
	Schouten-> 0,
	TraceOfOne -> 4,
	FCVerbose -> False,
	West -> True
};


DiracTrace /:
	MakeBoxes[DiracTrace[expr__, OptionsPattern[]], TraditionalForm]:=
	RowBox[{"tr","(",TBox[expr], ")"}]


(* gamma67backdef: reinsertion of gamma6 and gamm7 *)
gamma67back[x_] :=
	x/.DiracGamma[6]->( 1/2 + DiracGamma[5]/2 )/. DiracGamma[7]->( 1/2 - DiracGamma[5]/2 );

DiracTrace[0, OptionsPattern[]] :=
	0;

DiracTrace[a_ /; (FreeQ[a, DiracGamma] && !FreeQ[a, DiracGammaT]), b:OptionsPattern[]] :=
	DiracTrace[(a//Transpose)//Reverse, b];

DiracTrace[a:Except[_HoldAll]..., x_,y_, z___] :=
	DiracTrace[a,x.y,z]/;FreeQ2[y,{Rule,BlankNullSequence}]&& FreeQ2[x,{Rule,BlankNullSequence}];


(*DiracTracedef*)
fcit[y_] :=
	If[ True,
		FeynCalcInternal[DiracSigmaExplicit[y]]//DiracGammaExpand,
		FeynCalcInternal[y]//DiracGammaExpand
	];

DiracTrace[x_, op:OptionsPattern[]] :=
	Block[ {diTres, expr, tr1,tr2,tr3,time},


		If [OptionValue[FCVerbose]===False,
			diTrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				diTrVerbose=OptionValue[FCVerbose]
			];
		];

		unitMatrixTrace = OptionValue[TraceOfOne];

		FCPrint[1, "DiracTrace. Entering.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrace: Entering with ", x, FCDoControl->diTrVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "DiracTrace. Checking the syntax.", FCDoControl->diTrVerbose];
		If [ !FreeQ2[FCI[x],
			{
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
			(DiracGamma | DiracGammaT)[d__]}],
			Message[DiracTrace::noncom, InputForm[x]];
			Abort[]
		];

		FCPrint[1,"DiracTrace: Syntax check done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->diTrVerbose];



		time=AbsoluteTime[];
		FCPrint[1, "DiracTrace. Applying diractraceevsimple.", FCDoControl->diTrVerbose];
		tr1 = diractraceevsimple[fcit[x], Flatten[Join[{op}, FilterRules[Options[DiracTrace], Except[{op}]]]]];
		FCPrint[1,"DiracTrace: diractraceevsimple done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrace: After diractraceevsimple ", tr1, FCDoControl->diTrVerbose];


		time=AbsoluteTime[];
		FCPrint[1, "DiracTrace. Applying diractraceev.", FCDoControl->diTrVerbose];
		tr2  = tr1  /. diractraceevsimple -> diractraceev;
		FCPrint[1,"DiracTrace: diractraceev done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrace: After diractraceev ", tr2, FCDoControl->diTrVerbose];


		time=AbsoluteTime[];
		FCPrint[1, "DiracTrace. Applying diractraceev2.", FCDoControl->diTrVerbose];
		tr3  = tr2  /. diractraceev -> diractraceev2;
		FCPrint[1,"DiracTrace: diractraceev2 done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->diTrVerbose];

		FCPrint[3, "DiracTrace: After diractraceev2 ", tr3, FCDoControl->diTrVerbose];

		If [OptionValue[FeynCalcExternal],
			diTres = FCE[tr3],
			diTres = tr3
		];

		FCPrint[1, "DiracTrace: Leaving.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrace: Leaving with", diTres, FCDoControl->diTrVerbose];

		If[ !FreeQ[diTres,DiracGamma],
			Message[DiracTrace::rem];
			Abort[]
		];

		diTres
	]/; OptionValue[DiracTraceEvaluate] && FreeQ[x,SUNT]

diractraceevsimple[x_, OptionsPattern[]] :=
	unitMatrixTrace x/; FreeQ[x, DiracGamma];

diractraceevsimple[y_ DOT[x_,z__], opts:OptionsPattern[]] :=
	(y diractraceevsimple[DOT[x,z], opts])/; FreeQ[y, DiracGamma];

diractraceevsimple[x_Plus , opts:OptionsPattern[]] :=
	Map[diractraceevsimple[#,{opts}]&, x];

diractraceevsimple[DOT[x___], opts:OptionsPattern[]] :=
	(If[ FreeQ[#,LorentzIndex],
		#,
		#/.Pair->PairContract/.PairContract->Pair
	]&[diractraceev[Sequence@@DOT[x],opts]])/;
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
			a4:DiracGamma[LorentzIndex[_,dii_],dii_]..,
			OptionsPattern[]
			] :=
	unitMatrixTrace dcs[dii]@@Join[{a1,a2,a3}, {a4}/.DiracGamma->dirli,
	{a1,a2,a3}, {a4}/.DiracGamma->dirli];

dcs[dim_][x___] :=
	(dics[dim][x] /. dics->dc);

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
	dics[dd][x];

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
conall[ x_,opts:OptionsPattern[]] :=
	Contract[x, Expanding->True, EpsContract-> OptionValue[DiracTrace,{opts},EpsContract],
	Factoring->False ];

fr567[x__] :=
	FreeQ2[FixedPoint[ReleaseHold,{x}], {DiracGamma[5],DiracGamma[6],DiracGamma[7]}];

diractraceev[x_, opts:OptionsPattern[]] :=
	Block[ {trfa = 1, enx = x},
		If[ Head[x] === Times,
			trfa = Select[x, FreeQ2[#, {DiracGamma, LorentzIndex, Eps}]&];
			enx = x / trfa;
		];
		diractraceev2[conall[enx], opts] trfa
	];

(* Tr(1) *)
diractraceev2[x_, OptionsPattern[]] :=
	unitMatrixTrace  x /; FreeQ[x,DiracGamma];

diractraceev2[a_DiracGamma,b__DiracGamma, opts:OptionsPattern[]] :=
	diractraceev2[ DOT @@ {a,b}, opts];

diractraceev2[nnx_,opts:OptionsPattern[]] :=
	Block[ {diractrjj, diractrlnx, diractrres, diractrny = 0, mand, diractrfact, nx,
		diractrcoll, schoutenopt, diractrnyjj,
		dtmp,dWrap,dtWrap,wrapRule,prepSpur,time,time2,contract},

		wrapRule = {dWrap[5]->0, dWrap[6]->1/2, dWrap[7]->1/2, dWrap[LorentzIndex[_,_:4],___]->0,
					dWrap[_. Momentum[_,_:4]+_:0,___]->0};

		mand 		= OptionValue[DiracTrace,{opts},Mandelstam];
		diractrfact = OptionValue[DiracTrace,{opts},Factoring];
		diractrcoll = OptionValue[DiracTrace,{opts},PairCollect];
		schoutenopt = OptionValue[DiracTrace,{opts},Schouten];
		contract  	= OptionValue[DiracTrace,{opts},Contract];
		west		= OptionValue[DiracTrace,{opts},West];

		If[ diractrfact === Automatic,
			diractrfact = Function[x, If[ LeafCount[x] <  5000,
										Factor[x],
										x
									]];
		];

		FCPrint[1,"DiracTrace: diractraceev2: Entering", FCDoControl->diTrVerbose];
		FCPrint[2,"DiracTrace: diractraceev2: Entering with: ",nnx, FCDoControl->diTrVerbose];

		time=AbsoluteTime[];

		FCPrint[1,"DiracTrace: diractraceev2: Applying Collect2 and DiracGammaCombine.", FCDoControl->diTrVerbose];
		nx = Collect2[(nnx/.Pair->PairContract/.PairContract->Pair), DOT, Factoring -> False];
		nx = DiracGammaCombine[nx];

		FCPrint[1,"DiracTrace: diractraceev2: Collect2 and DiracGammaCombine done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->diTrVerbose];


		time=AbsoluteTime[];
		FCPrint[1,"DiracTrace: diractraceev2: Entering the main loop.", FCDoControl->diTrVerbose];

		If[ Head[nx]===Plus && Length[nx] > 142,
			FCPrint[1,"DiracTrace: diractraceev2: Long sum of traces.", FCDoControl->diTrVerbose];
			diractrlnx = Length[nx];
			diractrjj = 0;
			While[ diractrjj < diractrlnx,
				diractrjj++;
				FCPrint[2,"diractrjj = ", diractrjj," out of ",diractrlnx, FCDoControl->diTrVerbose];
				diractrny = diractrny +
				If[ FreeQ[nx,DiracGamma],
					diractrnyjj = nx[[diractrjj]],
					diractrnyjj = Expand2[ DiracSimplify[ nx[[diractrjj]],
								InsideDiracTrace->True, Factoring->False,
								FeynCalcInternal -> True, DiracCanonical->False], Pair];
					If[ !FreeQ[diractrnyjj, DiracGamma],
						(*	We need to consider standalone Dirac matrices separately
							With the following all of them will  be wrapped inside
							dWrap or dtWrap
						*)
						diractrnyjj = Expand2[DotSimplify[diractrnyjj,Expanding -> False] /.
							{DiracGamma -> dWrap,DiracGammaT -> dtWrap} /.
							DOT -> prepSpur /.
							prepSpur[zzz__] :> spursav@@({zzz} /.
							{dWrap -> DiracGamma,dtWrap->DiracGammaT}), Pair];
					];
					(* The trace of any standalone Dirac matrix is zero,
					g^6 and g^7 are of course special *)
					diractrnyjj = diractrnyjj/.wrapRule;
					If[ !FreeQ2[diractrnyjj,{dWrap,dtWrap}],
						Message[DiracTrace::rem];
						Abort[]
					];
					diractrnyjj
				]
			],
			FCPrint[1,"DiracTrace: diractraceev2: Small number of traces.", FCDoControl->diTrVerbose];
			If[ FreeQ[nx,DiracGamma],
				(*Expression is free of Dirac matrices*)
				diractrny = nx,
				(* Standard case: First simplify as much as possible through DiracSimplify *)
				time2=AbsoluteTime[];
				FCPrint[1,"DiracTrace: diractraceev2: Doing DiracSimplify in the main loop.", FCDoControl->diTrVerbose];
				diractrny = Expand2[ DiracSimplify[ nx, InsideDiracTrace->True, Factoring->False,
				FeynCalcInternal -> True, DiracCanonical->False ], Pair];
				FCPrint[1,"DiracTrace: diractraceev2: DiracSimplify done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->diTrVerbose];
				If[ !FreeQ[diractrny, DiracGamma],
					(* If the output of DiracSimplify still contains Dirac matrices, apply DotSimplify and try
					to evaluate the traces of Dirac matric chains via spursav *)
					(*	We need to consider standalone Dirac matrices separately
													With the following all of them will  be wrapped inside
													dWrap or dtWrap *)
					time2=AbsoluteTime[];
					FCPrint[1,"DiracTrace: diractraceev2: Calculating the trace.", FCDoControl->diTrVerbose];
					diractrny = (DotSimplify[diractrny, Expanding -> True] /.  {DiracGamma -> dWrap,DiracGammaT -> dtWrap} /.
								DOT -> prepSpur /. prepSpur[zzz__] :> spursav@@({zzz} /.
								{dWrap -> DiracGamma,dtWrap->DiracGammaT}));
					FCPrint[1,"DiracTrace: diractraceev2: Done calculating the trace, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->diTrVerbose];
					time2=AbsoluteTime[];
					FCPrint[1,"DiracTrace: diractraceev2: Expanding the result w.r.t Pairs", FCDoControl->diTrVerbose];
					diractrny=Expand2[diractrny,Pair];

					FCPrint[1,"DiracTrace: diractraceev2: Done expanding the result, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->diTrVerbose];

					(* The trace of any standalone Dirac matrix is zero,
					g^6 and g^7 are of course special *)
					diractrny = diractrny/. wrapRule;
					If[ !FreeQ2[diractrny,{dWrap,dtWrap}],
						Message[DiracTrace::rem];
						Abort[]
					]
				];
			]
		];


		FCPrint[1,"DiracTrace: diractraceev2: Main loop finished, timing:",N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];


		(* Apply DiracSimplify one more time *)

		If[ !FreeQ[diractrny, DiracGamma],
			time=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diractraceev2: Applying DiracSimplify again.", FCDoControl->diTrVerbose];
			diractrny = DiracSimplify[ diractrny, InsideDiracTrace->True,
			Factoring->False, FeynCalcInternal -> True, DiracCanonical->False];
			FCPrint[1,"DiracTrace: diractraceev2: DiracSimplify done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->diTrVerbose]
		];

		If[ !FreeQ[diractrny,DiracGamma],
			Message[DiracTrace::rem];
			Abort[]
		];
		FCPrint[2,"DiracTrace: diractraceev2: Contracting Lorentz indices. Time used: ", TimeUsed[], FCDoControl->diTrVerbose];

		(* If the output of the second DiracSimplify contains Lorentz indices, try
		to contract them *)
		If[ (contract===True || (NumberQ[contract] && LeafCount[diractrny] < contract)) && !FreeQ[diractrny, LorentzIndex],
			time=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diractraceev2: Contracting Lorentz indices. ", FCDoControl->diTrVerbose];
			diractrny=Contract[diractrny];
			(* Special contractions for expressions that contain Levi-Civita tensors*)
			(*If[ !FreeQ[diractrny, Eps],
				diractrny = diractrny //. {
					Pair[LorentzIndex[a_,D], b_] Eps[c___,LorentzIndex[a_],d___] :> Eps[c,b,d],
					Pair[LorentzIndex[a_,D], b_]  Eps[c___,LorentzIndex[a_,D],d___] :> Eps[c,b,d]
				}
			];
			diractrny = diractrny /. Pair -> PairContract /. PairContract -> scev*)
			FCPrint[1,"DiracTrace: diractraceev2: Contract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose]
		];


		(* Special expansion for expressions that contain Levi-Civita tensors*)
		If[ !FreeQ[diractrny, Eps],
			time=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diractraceev2: Treating Eps tensors.", FCDoControl->diTrVerbose];
			diractrny = EpsEvaluate[diractrny]//Expand;
			diractrny = Contract[ diractrny, EpsContract -> OptionValue[DiracTrace,{opts},EpsContract],
								Schouten->schoutenopt, Expanding -> False ];
			FCPrint[1,"DiracTrace: diractraceev2: Done with Eps tensors, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose]
		];

		(* Factor the result, if requested *)
		(* This is where we put back the prefactor of 4! *)
		time=AbsoluteTime[];
		FCPrint[1,"DiracTrace: diractraceev2: Factoring the result.", FCDoControl->diTrVerbose];
		If[ diractrfact===True,
			diractrres = Factor2[unitMatrixTrace diractrny],
			If[ diractrfact===False,
				diractrres = unitMatrixTrace diractrny,
				diractrres = diractrfact[unitMatrixTrace diractrny]
			]
		];
		FCPrint[1,"DiracTrace: diractraceev2: Factoring done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];


		time=AbsoluteTime[];
		FCPrint[2,"DiracTrace: diractraceev2: Applying TrickMandelstam.",FCDoControl->diTrVerbose];
		(* If the result should contain 2 -> 2 Mandelstam variable *)
		If[ Length[mand] > 0,
			diractrres = TrickMandelstam @@ Prepend[{mand}, diractrres]
		];
		diractrpc[x__] :=
			Plus[x]/;FreeQ[{x},Pair];
		(* If the result should be collected w.r.t Pairs *)
		If[ diractrcoll===True,
			time=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diractraceev2: Collecting the result w.r.t Pairs.", FCDoControl->diTrVerbose];
			diractrpc[x__] :=
				Collect2[ Plus[x],Pair ,Factoring -> False];
			diractrres = diractrres/. Plus -> diractrpc;
			FCPrint[1,"DiracTrace: diractraceev2: Collecting done, timing", N[AbsoluteTime[] - time, 4],  FCDoControl->diTrVerbose];


		];

		FCPrint[1,"DiracTrace: diractraceev2: Leaving.", FCDoControl->diTrVerbose];

		diractrres
	]/; !FreeQ2[nnx,{DOT,DiracGamma}];

spursav[0 ..] :=
	0;

(* calculation of traces (recursively) --  up to a factor of 4 *)
(*	Trace of g^mu g^nu g^rho g^si g^5	*)

(*
spursav[x_DiracGamma,y_DiracGamma,r_DiracGamma,z_DiracGamma, DiracGamma[5]] :=
	$LeviCivitaSign I Apply[ Eps, {x,y,r,z}/. DiracGamma[vl_[mp_,di___],di___]->vl[mp,di]]//EpsEvaluate
*)

(* 	All Dirac matrices are 4-dim. Simple case. *)
spursav[x_DiracGamma,y_DiracGamma,r_DiracGamma,z_DiracGamma, DiracGamma[5]] :=
	(EpsEvaluate[$LeviCivitaSign I Eps[x[[1]],y[[1]],r[[1]],z[[1]]]])/;
		FCGetDimensions[{x,y,r,z}]==={4};

(* 	For all other cases special treatment is needed... *)
spursav[x_DiracGamma,y_DiracGamma,r_DiracGamma,z_DiracGamma, DiracGamma[5]] :=
	Block[{dims,tmp},
		dims=FCGetDimensions[{x,y,r,z}];
		Which[

			(* D-dims, BMHV -> gets converted to 4 Dims*)
			MatchQ[dims, {_Symbol}] && !$Larin && $BreitMaison,
				tmp = Eps[x[[1]],y[[1]],r[[1]],z[[1]]],
			(* D-dims, Larin -> remains in Dims*)
			MatchQ[dims, {_Symbol}] && $Larin && !$BreitMaison,
				tmp = Eps[x[[1]],y[[1]],r[[1]],z[[1]],Dimension->dims[[1]]],
			(* 4-dims, D-dims and D-4 dims mixtures, BMHV -> gets converted to 4 Dims*)
			(MatchQ[dims, {4,_Symbol}] || MatchQ[dims, {4,_Symbol-4}] || MatchQ[dims, {s_Symbol-4,s_Symbol}] || MatchQ[dims, {4, s_Symbol-4,s_Symbol}])	&& !$Larin && $BreitMaison,
				tmp = Eps[x[[1]],y[[1]],r[[1]],z[[1]]],
			(* any other combination is most likely an error*)
			True,
			Message[DiracTrace::fail, FullForm[{x,y,r,z,DiracGamma[5]}]];
			Abort[]
		];
		EpsEvaluate[$LeviCivitaSign I tmp]
	]/;FCGetDimensions[{x,y,r,z}]=!={4};

(* there is the problem with different Gamma5-schemes ... *)
spursav[x__DiracGamma] :=
	spur[x];
	(*Added 28/2-2001 by F.Orellana. Fix to bug reported by A.Kyrielei*)

spursav[x : ((DiracGamma[__] | HoldPattern[Plus[__DiracGamma]]) ..)] :=
	spur[x];

spursavg[x___, LorentzIndex[a_, dim_ : 4], LorentzIndex[a_, dim_ : 4], y___] :=
	(dim spursavg[x, y]) /. spursavg -> spug;

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
(*
spur[x___,DiracGamma[5],y__] :=
	DiracSimplify[DOT[x,DiracGamma[5],y],InsideDiracTrace->True]/;
	(!FreeQ[{x},DiracGamma] || !FreeQ[{y},DiracGamma]) && Length[{x,y}>2];
*)
spur[x___,DiracGamma[5],y___] :=
	0/; FreeQ[{x},DiracGamma] && FreeQ[{y},DiracGamma];

spur[x_[y__],DiracGamma[5]] :=
	0;

spur[DiracGamma[_,_:4],DiracGamma[_,_:4],DiracGamma[5]] :=
	0;

spur[a_[b__],c_[d__],x_[y__],DiracGamma[5]] :=
	0;

spur[a_[b__],c_[d__],x_[y__], _[__], odd__, DiracGamma[5]] :=
	0/; OddQ[Length[{odd}]];

spur[a__] :=
	(spur @@ Reverse[Transpose[{a}]]) /; (!FreeQ[{a}, DiracGammaT]) && FreeQ[{a},DiracGamma];

(* This is a definition of   Trace( 1.2.3.4. gamma[5] ) *)
spur[x_,y_,r_,z_,DiracGamma[5]] :=
	$LeviCivitaSign I Apply[Eps, {x,y,r,z}/.DiracGamma[vl_[mp_,dii___],___]->vl[mp,dii]]//EpsEvaluate


(* this trace has been calculated according to Larin,
	i.e. expression DiracMatrix[w8].DiracGamma[5] by
	(-I/6) LeviCivita[w8,mu,nu,la] DiracMatrix[mu,nu,la] *)
spur[w1_,w2_,w3_,w4_,w5_,w6_,w7_,w8_,DiracGamma[5]] :=
	Block[ {z1,z2,z3,z4,z5,z6,z7,z8},
		{z1,z2,z3,z4,z5,z6,z7,z8} =
		{w1,w2,w3,w4,w5,w6,w7,w8} /.DiracGamma[vl_[mp_,dii___],___]->vl[mp,dii];
		(*TODO: vl -> (vl :LorentzIndex | Momentum) *)
		(* trsign is usually  =  -1 *)
		(* factor 4 is put later *)
		$LeviCivitaSign*I*(Eps[z5, z6, z7, z8]*Pair[z1, z4]*Pair[z2, z3] -
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
		ex /; Length[ex]>0;
	gach[n_Integer] =
		DiracGamma[n];
	(* This function handles general  Dirac traces *)

	spur[y__] :=
		Block[ {spx,le = Length[{y}],tempres,i,spurjj,tempr,
			temp2 = 0, fi,spt, resp,dirsign,time,fi1,fi2,fi3,drsi},
			spx = ( {y}//DiracGammaExpand )/.DiracGamma->gach;
			temp2 = Hold[spur][spx];
			time = AbsoluteTime[];
			FCPrint[1, "DiracTrace: spur: Entering.", FCDoControl->diTrVerbose];
			FCPrint[2, "DiracTrace: spur: Entering with ", spx, FCDoControl->diTrVerbose];

			resp =
			Which[
				(*Trace of an odd number of Dirac matrices without gamma^5 *)
				OddQ[le] && fr567[spx],
					0,
				(* For traces with an even number of Dirac matrices without gamma^5
				use the trace reduction equation from Veltman's Gammatrica (p.255) plus
				a tweaked version of Thomas Hahn's Trace4 with some memoization magic *)
				FreeQ[spx,DiracGamma[5]],
					traceNo5Wrap@@spx,
				(* Here we handle traces with of type g^i1 .... g^in g^5 with n>=6*)
				FreeQ[Drop[spx,-1], DiracGamma[5]] && Length[spx] > 6,
					FCPrint[2,"Computing the chiral trace ", spx, FCDoControl->diTrVerbose];
					Which[
						(* NDR *)
						!$Larin && !$BreitMaison,
							If[ MatchQ[SelectFree[spx,{(LorentzIndex | Momentum)[_],DiracGamma[5]}], {} ],
								(* If the trace is purely four dimensional, NDR is ok here. *)
								(*Apply the standard anomalous trace formula (c.f. Eq 2.18 of R. Mertig, M. Boehm,
								A. Denner. Comp. Phys. Commun., 64 (1991)) *)
								FCPrint[3,"The chiral trace", spx, "is computed using the standard recursion formula in NDR", FCDoControl->diTrVerbose];
								trace5Wrap@@(spx//.DiracGamma[5]->5),
								(* Otherwise abort the computation, since NDR cannot handle anomalous traces without an
								additional prescription*)
								Message[DiracTrace::ndranomaly, InputForm[DOT[y]]];
								Abort[];
							],
						(* Larin *)
						$Larin && !$BreitMaison,
							FCPrint[3,"The chiral trace", spx, "is computed in Larin's scheme", FCDoControl->diTrVerbose];
							{fi1, fi2, fi3} = LorentzIndex[#,D]& /@ Unique[{"a","b","c"}];
							drsi = $LeviCivitaSign/(TraceOfOne/.Options[DiracTrace]);
							(*drsi is usually -1/4 *)
							temp2 = spx /. {a___, lomo_[mUU_,di___], DiracGamma[5]} :>
							TR[ drsi I/6 Eps[lomo[mUU,di], fi1, fi2, fi3] *
							DOT @@ Map[DiracGamma[#,D]&, {a,fi1,fi2,fi3}], EpsContract->True];
							temp2/.spt->spursavg/.spursavg->spug,
						(* BMHV, standard (slow!) trace formula *)
						!$Larin && $BreitMaison && !west,
							FCPrint[3,"The chiral trace", spx, "is computed in the BMHV scheme using the slow formula", FCDoControl->diTrVerbose];
							fi = Table[LorentzIndex[ Unique[] ],{spurjj,1,4}];
							drsi = $LeviCivitaSign/(TraceOfOne/.Options[DiracTrace]);
							(tmp @@ ({y}/.DiracGamma[5]->
							(drsi I/24 (DOT[DiracGamma[fi[[1]]],DiracGamma[fi[[2]]],
							DiracGamma[fi[[3]]],DiracGamma[fi[[4]]]]) (Eps@@fi))))/. tmp[arg__] :> DiracTrace[arg,DiracTraceEvaluate->True],
						(* BMHV West's trace formula *)
						!$Larin && $BreitMaison && west,
							FCPrint[3,"The chiral trace", spx, "is computed in the BMHV scheme using West's formula", FCDoControl->diTrVerbose];
							temp2 = Expand[2/(Length[spx]-5) Sum[(-1)^(i+j+1) *
							FCUseCache[ExpandScalarProduct,{spx[[i]],spx[[j]]},{}] spt@@Delete[spx,{{j},{i}}],
								{i,2,Length[spx]-1},{j,1,i-1}]];
							temp2/.spt->spursavg/.spursavg->spug,
							(* Any other combination of $Larin, $BreitMaison doesn't describe
							a valid scheme *)
						True,
							Message[DiracTrace::ilsch, $BreitMaison,$Larin]
					],
			True,
			Message[DiracTrace::fail, FullForm[spx]]
			];


			FCPrint[1,"DiracTrace: spur: Finished, timing:",N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrace: spur: Leaving with:",resp, FCDoControl->diTrVerbose];
			FCPrint[1,"DiracTrace: spur: Leaving.", FCDoControl->diTrVerbose];

			resp
		];




fastExpand[xx_] :=
	Replace[xx, p_. Times[a__, x_Plus] :> Distribute[p a*x, Plus], 1];

(*	traceNo5Wrap is a higher level function that handles the computation of traces without gamma 5,
	all indices different.  The trick here 	is that as soon as we compute a trace for a given number of Dirac matrices,
	we define it is a function (traceNo5fun) so that the result can be retrieved very fast. Combined with the the fast expansion
	using fastExpand this provides a rather quick way to obtain Dirac traces. The bottlenecks here are the amount of RAM required
	for caching and the general slowness of Mathematica on very large expressions. Traces with up to 14 Dirac matrices should be fine,
	after that it becomes too slow *)

traceNo5Wrap[SI1_, SI2__] :=
	Block[{res, repRule, tab, set, SI, args, setDel, tmpRes, finalRes},

		tab = Table[ ToExpression["MySI" <> ToString[i]], {i, 1, Length[{SI1, SI2}]}];
		finalRes = traceNo5Fun @@ {SI1, SI2};

		If[Head[finalRes] === traceNo5Fun,
			(* The trace needs to be computed *)
			tmpRes = traceNo5 @@ tab;
			If[	($MemoryAvailable - MemoryInUse[]/1000000.) >1. ,
				(* If there is enough memory, we save the computed result as a function *)
				args = Sequence @@ (Pattern[#, _] & /@ tab);
				setDel[traceNo5Fun[args], fastExpand[tmpRes]] /. setDel -> SetDelayed;
				res = traceNo5Fun @@ {SI1, SI2},
				(* No memoization if we have not enough memory *)
				res = tmpRes /. Thread[Rule[tab, {SI1,SI2}]]

			],
			(* The trace has already been computed *)
			res = finalRes
		];

		res
	];

traceNo5Wrap[] =
	1;

(* 	traceNo5 is the lower level function that computes only indices of type S[1],S[2],... and
	remembers its values. It's based on Thomas Hahn's famous Trace4  function *)
traceNo5[SI1_, SI2__] :=
	Block[{head, s = -1, res},
		res = Plus @@ MapIndexed[((s = -s) Pair[SI1, #1] Drop[head[SI2], #2]) &, {SI2}];
		res = res /. head -> traceNo5Wrap;
		res
	];

(* 	trace5Wrap computes a 4-dimensional trace of Dirac matrices with one gamma 5 using
	similar tricks as traceNo5Wrap. *)
trace5Wrap[SI1__, 5] :=
	Block[{res, repRule, tab, set, args, setDel, tmpRes, realRes},
		tab = Table[ToExpression["MySI" <> ToString[i]], {i, 1, Length[{SI1}]}];

		realRes = trace5Fun @@ {SI1, 5};

		If[Head[realRes] === trace5Fun,
			(* The trace needs to be computed *)
			tmpRes = trace5 @@ (Join[tab, {5}]);
			If[	($MemoryAvailable - MemoryInUse[]/1000000.) >1. ,
				(* If there is enough memory, we save the computed result as a function *)
				args = Sequence @@ Join[(Pattern[#, _] & /@ tab), {5}];
				setDel[trace5Fun[args], fastExpand[tmpRes]] /. setDel -> SetDelayed;
				res = trace5Fun @@ {SI1, 5},
				(* No memoization if we have not enough memory *)
				res = tmpRes /. Thread[Rule[tab, {SI1}]]

			],
			(* The trace has already been computed *)
			res = realRes
		];
		res
	];

trace5[SI1_, SI2__, mu_, nu_, rho_, 5] :=
	Pair[mu, nu] trace5[SI1, SI2, rho, 5] -
	Pair[mu, rho] trace5[SI1, SI2, nu, 5] +
	Pair[nu, rho] trace5[SI1, SI2, mu, 5] -
	$LeviCivitaSign I traceEpsNo5[mu, nu, rho, SI1, SI2];


(*
trace5[mu_, nu_, rho_, SI1_, SI2__, 5] :=
	Pair[mu, nu] trace5[rho, SI1, SI2, 5] -
	Pair[mu, rho] trace5[nu, SI1, SI2, 5] +
	Pair[nu, rho] trace5[mu, SI1, SI2, 5] +
	epsTensorSign I traceEpsNo5[mu, nu, rho, SI1, SI2]
*)

trace5[a_, b_, c_, d_, 5]:=
	$LeviCivitaSign I Eps[a, b, c, d];

traceEpsNo5[mu_, nu_, rho_, SI2__] :=
	Block[{head, s = -1, res},
		res = Plus @@ MapIndexed[((s = -s) Eps[mu, nu, rho, #1] Drop[head[SI2], #2]) &, {SI2}];
		res = res /. head -> traceNo5Wrap;
		res
	];

FCPrint[1,"DiracTrace.m loaded."];
End[]
