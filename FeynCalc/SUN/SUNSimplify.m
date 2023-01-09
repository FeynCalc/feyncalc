(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNSimplify     													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Color algebra simplifications									*)

(* ------------------------------------------------------------------------ *)

SUNSimplify::usage =
"SUNSimplify[exp] simplifies color algebraic expressions involving color
matrices with implicit (SUNT) or explicit fundamental indices (SUNTF) as well
as structure constants (SUND, SUNF) and Kronecker deltas (SD, SDF).

If the option Explicit is set to True (default is False), the structure
constants will be rewritten in terms of traces. However, since traces with 2
or 3 color matrices are by default converted back into structure constants,
you must also set the option SUNTraceEvaluate to False (default is Automatic)
in order to have unevaluated color traces in the output.";

SUNFJacobi::usage=
"SUNFJacobi is an option for SUNSimplify, indicating whether the Jacobi
identity should be used.";

SUNSimplify::failmsg =
"Error! SUNSimplify has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

sunTrace;
colorSimplifyGeneric;
colorSimplifyToSUNF2SUND2;
colorSimplifyFromSUNF2SUND2;
sunf;
sund;
holdDOTColor;

End[]

Begin["`SUNSimplify`Private`"]

sunSiVerbose::usage="";
dummyInd::usage="";
optSUNTraceEvaluate::usage="";

SetAttributes[SUNSimplify, Listable];

Options[SUNSimplify] = {
	Collecting			-> True,
	Explicit			-> False,
	FCI 				-> False,
	FCE 				-> False,
	FCVerbose 			-> False,
	Factoring 			-> {Factor, 5000},
	SUNFIndexNames		-> {},
	SUNFJacobi			-> False,
	SUNIndexNames		-> {},
	SUNNToCACF			-> True,
	SUNTraceEvaluate	-> Automatic,
	TimeConstrained		-> 3
};

SUNSimplify[expr_, OptionsPattern[]] :=
	Block[{	ex, temp, optSUNNToCACF, optExplicit, optFactoring, time, sunsiIso, listColoredObjects,
			listColoredObjectsEval, finalRepRule, optSUNFJacobi, optCollecting, res, optTimeConstrained,
			listColorFactor, listColorFactorEval},

		optCollecting		= OptionValue[Collecting];
		optFactoring		= OptionValue[Factoring];
		optSUNNToCACF		= OptionValue[SUNNToCACF];
		optExplicit			= OptionValue[Explicit];
		optSUNTraceEvaluate	= OptionValue[SUNTraceEvaluate];
		optSUNFJacobi		= OptionValue[SUNFJacobi];
		optTimeConstrained	= OptionValue[TimeConstrained];

		If [OptionValue[FCVerbose]===False,
			sunSiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				sunSiVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];
		temp = ex;

		FCPrint[1, "SUNSimplify: Entering.", FCDoControl->sunSiVerbose];
		FCPrint[3, "SUNSimplify: Entering with ", temp, FCDoControl->sunSiVerbose];

		(* Isolate everything except for the color structures that we are interested in*)
		time=AbsoluteTime[];
		FCPrint[1, "SUNSimplify: Collecting terms w.r.t. colored objects.", FCDoControl->sunSiVerbose];
		temp = FCColorIsolate[temp, FCI->True,Isolate->True, IsolateFast->True, IsolateNames->sunsiIso, Head->sunObj, ClearHeads->{sunObj}, ExceptHeads->{SUNN,CA,CF}, "ExpandNestedDOTs" -> True];
		FCPrint[1,"SUNSimplify: collecting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
		FCPrint[3, "SUNSimplify: After collecting terms w.r.t. colored objects: ",temp, FCDoControl->sunSiVerbose];

		(* It is better to canonicalize the indices at the very beginning. FCCanonicalizeDummyIndices can handle this automatically*)
		time=AbsoluteTime[];
		FCPrint[1, "SUNSimplify: Renaming.", FCDoControl->sunSiVerbose];
		temp = FCCanonicalizeDummyIndices[temp, FCI -> True, Head ->{SUNIndex,SUNFIndex}];
		FCPrint[1, "SUNSimplify: Renaming done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
		FCPrint[3, "SUNSimplify: After renaming, ", temp, FCDoControl->sunSiVerbose];

		ex = temp;
		listColoredObjects = Cases2[temp, sunObj];


		(* The show begins *)
		listColoredObjectsEval = listColoredObjects /. sunObj->Identity;

		(* Now the expression should be free of SUNDelta/SUNFDelta with dummy indices *)

		listColoredObjectsEval = listColoredObjectsEval /. SUNTrace -> sunTrace /. {
			DOT -> holdDOTColor,
			SUNF[args__, OptionsPattern[]] :> sunf[args],
			SUND[args__, OptionsPattern[]] :> sund[args]
		};

		FCPrint[3, "SUNSimplify: Unique colored objects: ", listColoredObjectsEval, FCDoControl->sunSiVerbose];

		listColoredObjectsEval = Expand2[#,{sunTrace, holdDOTColor, SUNT,sunf,sund,sunf2,sund2,SUNDelta,SUNFDelta}]&/@listColoredObjectsEval;
		listColoredObjectsEval = colorIndexContract/@listColoredObjectsEval;

		FCPrint[3, "SUNSimplify: After initial contractions of color deltas: ", listColoredObjectsEval, FCDoControl->sunSiVerbose];

		listColoredObjectsEval = listColoredObjectsEval /. colorIndexContract->colorSimplifyGeneric;
		FCPrint[3, "SUNSimplify: After colorSimplifyGeneric: ", listColoredObjectsEval, FCDoControl->sunSiVerbose];

		(*Switching to sunf2/sund2*)
		If[	!FreeQ2[listColoredObjectsEval,{sunf,sund}],
			listColoredObjectsEval = listColoredObjectsEval/. colorSimplifyGeneric->colorSimplifyToSUNF2SUND2 /. colorSimplifyToSUNF2SUND2->colorSimplifyGeneric;
		];

		FCPrint[3, "SUNSimplify: After simplifying terms with SUNFs and SUNDs: ", listColoredObjectsEval, FCDoControl->sunSiVerbose];


		If[ optSUNTraceEvaluate===True,
			listColoredObjectsEval = listColoredObjectsEval /. sunTrace[z_] :> SUNTrace[z /. holdDOTColor[] -> 1 /. holdDOTColor -> DOT, SUNTraceEvaluate->True];

			listColoredObjectsEval = listColoredObjectsEval /. SUNTrace -> sunTrace /. {
				DOT -> holdDOTColor,
				SUNF[args__, OptionsPattern[]] :> sunf[args],
				SUND[args__, OptionsPattern[]] :> sund[args]
			};
			listColoredObjectsEval= listColoredObjectsEval/. colorSimplifyGeneric[x_] :> Expand2[x,{sunTrace, holdDOTColor, SUNT,sunf,sund,sunf2,sund2}];
			FCPrint[3, "SUNSimplify: After taking care of color traces: ", listColoredObjectsEval, FCDoControl->sunSiVerbose];
		];

		(* Back substitution of the remaining sunf2/sund2 symbols *)
		If[	!FreeQ2[listColoredObjectsEval,{sunf2,sund2}],
			listColoredObjectsEval = listColoredObjectsEval /. colorSimplifyGeneric -> colorSimplifyFromSUNF2SUND2 /. colorSimplifyFromSUNF2SUND2 -> colorSimplifyGeneric;
		];

		(* Possibly insert explicit expressions for SUNFs and SUNDs*)
		If[ TrueQ[optExplicit],
			listColoredObjectsEval = listColoredObjectsEval /. colorSimplifyGeneric -> insertColorTraces /. insertColorTraces -> colorSimplifyGeneric;
			FCPrint[3, "SUNSimplify: After reinserting SUNF/SUND symbols: ", listColoredObjectsEval, FCDoControl->sunSiVerbose];
			listColoredObjectsEval = Expand2[#,{colorSimplifyGeneric}]&/@listColoredObjectsEval
		];

		listColoredObjectsEval = listColoredObjectsEval /. {sunf -> SUNF, sund -> SUND};

		FCPrint[3, "SUNSimplify: After reinserting SUNF/SUND symbols: ", listColoredObjectsEval, FCDoControl->sunSiVerbose];

		listColoredObjectsEval = listColoredObjectsEval/. colorSimplifyGeneric->Identity;
		listColoredObjectsEval = listColoredObjectsEval /. holdDOTColor[] -> 1 /. holdDOTColor -> DOT /. SUNTrace -> sunTrace;



		(*Final replacement rule*)
		finalRepRule = Thread[Rule[listColoredObjects, listColoredObjectsEval]];

		FCPrint[3, "SUNSimplify: Final replacement rule: ", finalRepRule, FCDoControl->sunSiVerbose];

		temp = ex /. Dispatch[finalRepRule] /. sunObj->Identity;

		(* Canonicalize indices once again to account for the introduced dummy indices *)

		time=AbsoluteTime[];
		FCPrint[1, "SUNSimplify: Renaming.", FCDoControl->sunSiVerbose];
		temp = FCCanonicalizeDummyIndices[temp, FCI -> True, Head ->{SUNIndex,SUNFIndex}, SUNIndexNames->OptionValue[SUNIndexNames], SUNFIndexNames->OptionValue[SUNFIndexNames]];
		FCPrint[1, "SUNSimplify: Renaming done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
		FCPrint[3, "SUNSimplify: After renaming, ", temp, FCDoControl->sunSiVerbose];

		(*Remove isolations*)
		temp = FRH[temp, IsolateNames->sunsiIso];


		If[ optSUNFJacobi && !FreeQ[temp, SUNF],
			time=AbsoluteTime[];
			FCPrint[1, "SUNSimplify: Applying the Jacobi identity for SUNFs.", FCDoControl->sunSiVerbose];
				temp = temp /. SUNF -> sunf;
				temp = temp /. (sunf[a_, b_, c_] sunf[d_, c_, e_] ) :> (- sunf[a, b, c] sunf[d, e, c]);
				temp = temp /. (sunf[a_, b_, c_] sunf[c_, d_, e_] ) :> (sunf[a, b, c] sunf[d, e, c]);

				temp = temp /. {
					sunf[a_SUNIndex, c_SUNIndex, e_SUNIndex] sunf[b_SUNIndex, d_SUNIndex, e_SUNIndex]/;
					Sort[{ {a,c,e, b,d,e}, {a,b,e, c,d,e}, {b,c,e, a,d,e}}][[1]] === {a,c,e, b,d,e} :>
						sunf[a,b,e] sunf[c,d,e] + sunf[b,c,e] sunf[a,d,e]
				};
				temp = temp /. sunf -> SUNF;

			FCPrint[1, "SUNSimplify: Done applying Jacobi identity for SUNFs, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
		];

		temp = temp /. SUNTrace-> sunTrace /. DOT->holdDOTColor /. sunTrace -> sunTraceOrder /. holdDOTColor->DOT /. sunTrace->SUNTrace;

		FCPrint[3, "SUNSimplify: After reordering color traces: ", temp, FCDoControl->sunSiVerbose];

		(*
			Introducing SU(N) structure constants, this must be the last operation, since any kind of factoring would rewrite structure
			constants in a weird way!
		*)

		time=AbsoluteTime[];
		FCPrint[1, "SUNSimplify: Introducing SU(N) structure constants.", FCDoControl->sunSiVerbose];
		If[	!FreeQ2[temp,{SUNIndex,SUNFIndex}],
			temp = Collect2[temp,{SUNIndex,SUNFIndex}, Factoring->optFactoring, TimeConstrained->optTimeConstrained,Head->{Identity,colorFactor}],
			temp = colorFactor[temp,1]
		];

		temp  = temp /. {
			colorFactor[a_, b_]/;!FreeQ2[a,{SUNN,CA,CF}] :> colorFactor[a] b,
			colorFactor[a_, b_]/;FreeQ2[a,{SUNN,CA,CF}] :> a b
		};

		If[ optSUNNToCACF,
			temp = temp /. colorFactor[a_]/; Internal`SyntacticNegativeQ[a] :> - colorFactor[ExpandAll[-a]]
		];

		listColorFactor = Cases2[temp,colorFactor];


		If[ optSUNNToCACF,

			FCPrint[3, "SUNSimplify: Prefactors containing SUNN:", listColorFactor, FCDoControl->sunSiVerbose];
			listColorFactorEval = Factor2/@(listColorFactor/. colorFactor->Identity/. {CA ->SUNN, CF -> (SUNN^2-1)/(2 SUNN)});
			listColorFactorEval = listColorFactorEval /. (1-SUNN^2) -> (-CF 2 CA) /. SUNN -> CA /. (-1 + CA^2)->(2 CA CF);
			listColorFactorEval = listColorFactorEval /. (((2 - CA^2) CF )/CA ) ->(CF (CA - 4 CF));
			listColorFactorEval = listColorFactorEval /. (1-CA^2) -> (-2 CA CF) /. (1/CA) -> (CA - 2 CF) /. ((1 - CA^2)*(CA - 2*CF)) -> (-2*CF) /. (CA (CA-2 CF)) -> 1,
			listColorFactorEval = (listColorFactor/. colorFactor->Identity) /. CA -> SUNN /. CF -> ((SUNN^2-1)/(2 SUNN));
		];

		FCPrint[1, "SUNSimplify: Done introducing SU(N) structure constants, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sunSiVerbose];
		FCPrint[3, "SUNSimplify: Rewritten prefactors:", listColorFactor, FCDoControl->sunSiVerbose];

		finalRepRule = Thread[Rule[listColorFactor,listColorFactorEval]];

		temp = temp /. Dispatch[finalRepRule];

		If[ !FreeQ[temp, CA],
			temp = temp /. (CA*(CA -2*CF)) -> 1
		];


		res = temp;

		If [OptionValue[FeynCalcExternal],
			res = FCE[res]
		];

		res

	];

(* SUND -> Color trace *)
insertColorTraces[rest_. sund[i_,j_,k_]]:=
	2 insertColorTraces[rest sunTrace[holdDOTColor[SUNT[i],SUNT[j],SUNT[k]]]] + 2 insertColorTraces[rest sunTrace[holdDOTColor[SUNT[j], SUNT[i], SUNT[k]]]];

(* SUNF -> Color trace *)
insertColorTraces[rest_. sunf[i_,j_,k_]]:=
	2 I insertColorTraces[rest sunTrace[holdDOTColor[SUNT[i],SUNT[k],SUNT[j]]]] - 2 I insertColorTraces[rest sunTrace[holdDOTColor[SUNT[i],SUNT[j],SUNT[k]]]];


(* Cyclicity of the color trace *)
sunTraceOrder[holdDOTColor[ts__SUNT]]:=
	Block[{inds},
		inds = First/@{ts};
		inds = First[Sort[NestList[RotateLeft, inds, Length[inds]-1]]];
		sunTrace[holdDOTColor@@(SUNT/@inds)]
	];


(*Index contractions*)
colorIndexContract[rest_. SUNDelta[x_SUNIndex, x_SUNIndex]]:=
	(SUNN^2 - 1) colorIndexContract[rest];

colorIndexContract[rest_. SUNDelta[j_ExplicitSUNIndex, _SUNIndex]^2]:=
	SUNDelta[ExplicitSUNIndex[j], ExplicitSUNIndex[j]] colorIndexContract[rest];

colorIndexContract[rest_. SUNDelta[i_SUNIndex, j_SUNIndex]^2]:=
	(SUNN^2 - 1) colorIndexContract[rest]/; (i =!= j);

colorIndexContract[rest_ SUNDelta[i_SUNIndex, j_SUNIndex ]]:=
	colorIndexContract[rest /. {i -> j}]/; !FreeQ[rest, i] && FreeQ[rest, _FeynArts`SumOver]

colorIndexContract[rest_. SUNFDelta[x_SUNFIndex, x_SUNFIndex]]:=
	SUNN colorIndexContract[rest];

colorIndexContract[rest_. SUNFDelta[j_ExplicitSUNFIndex, _SUNFIndex]^2]:=
	SUNFDelta[ExplicitSUNFIndex[j], ExplicitSUNFIndex[j]] colorIndexContract[rest];

colorIndexContract[rest_. SUNFDelta[i_SUNFIndex, j_SUNFIndex]^2]:=
	SUNN colorIndexContract[rest]/; (i =!= j);

colorIndexContract[rest_ SUNFDelta[i_SUNFIndex, j_SUNFIndex ]]:=
	colorIndexContract[rest /. {i -> j}]/; !FreeQ[rest, i] && FreeQ[rest, _FeynArts`SumOver]

(* ---------------------------------------------------------------------- *)
(*	Conversions between sunf/sund and sunf2/sund2			  			  *)
(* ---------------------------------------------------------------------- *)


(* f^iab f^icd = f_2^abcd *)
colorSimplifyToSUNF2SUND2[rest_. sunf[r1___, a_SUNIndex, r2___] sunf[r3___, a_SUNIndex, r4___]] :=
	colorSimplifyToSUNF2SUND2[rest sunf2[r2,r1,r4,r3]]/; Signature[{r1,r2,r3,r4}]=!=0;

(* d^iab d^icd = d_2^abcd *)
colorSimplifyToSUNF2SUND2[rest_. sund[r1___, a_SUNIndex, r2___] sund[r3___, a_SUNIndex, r4___]] :=
	colorSimplifyToSUNF2SUND2[rest sund2[r2,r1,r4,r3]]/; Signature[{r1,r2,r3,r4}]=!=0;


colorSimplifyFromSUNF2SUND2[rest_. sunf2[a_SUNIndex,b_SUNIndex,c_SUNIndex,d_SUNIndex]] :=
	(
	dummyInd=SUNIndex[FCGV[ToString[Unique["sun"]]]];
	colorSimplifyFromSUNF2SUND2[rest sunf[dummyInd,a,b] sunf[dummyInd,c,d]]
	);

colorSimplifyFromSUNF2SUND2[rest_. sund2[a_SUNIndex,b_SUNIndex,c_SUNIndex,d_SUNIndex]] :=
	(
	dummyInd=SUNIndex[FCGV[ToString[Unique["sun"]]]];
	colorSimplifyFromSUNF2SUND2[rest sund[dummyInd,a,b] sund[dummyInd,c,d]]
	);



colorSimplifyGeneric[ex_Plus]:=
	colorSimplifyGeneric/@ex;

colorSimplifyGeneric[0]:=
	0;

colorSimplifyGeneric[rest_]:=
	rest/; FreeQ2[rest,{SUNIndex,SUNFIndex,sunTrace}]

(* ---------------------------------------------------------------------- *)
(*	Evaluations of simple traces 							  			  *)
(* ---------------------------------------------------------------------- *)

colorSimplifyGeneric[rest_. sunTrace[holdDOTColor[]]^n_.]:=
	SUNN^n colorSimplifyGeneric[rest];

(* Tr(T^a) *)
colorSimplifyGeneric[_. sunTrace[holdDOTColor[SUNT[_SUNIndex]]]]:=
	0;

colorSimplifyGeneric[_. sunTrace[SUNT[_SUNIndex]]]:=
	0;

(* Tr(T^a T^b) *)
colorSimplifyGeneric[rest_. sunTrace[holdDOTColor[SUNT[x_SUNIndex] , SUNT[y_SUNIndex]]]]:=
	1/2 colorSimplifyGeneric[rest SUNDelta[x, y]]/; optSUNTraceEvaluate===Automatic;


(* Tr(T^a T^b T^c) *)
colorSimplifyGeneric[rest_. sunTrace[holdDOTColor[SUNT[a_SUNIndex] , SUNT[b_SUNIndex] , SUNT[c_SUNIndex]]]] :=
	(
	1/4 colorSimplifyGeneric[rest sund[a, b, c]] + I/4 colorSimplifyGeneric[rest sunf[a,b,c]]
	)/; optSUNTraceEvaluate===Automatic;

(* ---------------------------------------------------------------------- *)
(*	Index contractions using Kronecker deltas 							  *)
(* ---------------------------------------------------------------------- *)

colorSimplifyGeneric[rest_. SUNDelta[x_SUNIndex, x_SUNIndex]]:=
	(SUNN^2 - 1) colorSimplifyGeneric[rest];

colorSimplifyGeneric[rest_. SUNDelta[j_ExplicitSUNIndex, _SUNIndex]^2]:=
	SUNDelta[ExplicitSUNIndex[j], ExplicitSUNIndex[j]] colorSimplifyGeneric[rest];

colorSimplifyGeneric[rest_. SUNDelta[i_SUNIndex, j_SUNIndex]^2]:=
	(SUNN^2 - 1) colorSimplifyGeneric[rest]/; (i =!= j);

colorSimplifyGeneric[rest_ SUNDelta[i_SUNIndex, j_SUNIndex ]]:=
	colorSimplifyGeneric[rest /. {i -> j}]/; !FreeQ[rest, i] && FreeQ[rest, _FeynArts`SumOver]

colorSimplifyGeneric[rest_. SUNFDelta[x_SUNFIndex, x_SUNFIndex]]:=
	SUNN colorSimplifyGeneric[rest];

colorSimplifyGeneric[rest_. SUNFDelta[j_ExplicitSUNFIndex, _SUNFIndex]^2]:=
	SUNFDelta[ExplicitSUNFIndex[j], ExplicitSUNFIndex[j]] colorSimplifyGeneric[rest];

colorSimplifyGeneric[rest_. SUNFDelta[i_SUNFIndex, j_SUNFIndex]^2]:=
	SUNN colorSimplifyGeneric[rest]/; (i =!= j);

colorSimplifyGeneric[rest_ SUNFDelta[i_SUNFIndex, j_SUNFIndex ]]:=
	colorSimplifyGeneric[rest /. {i -> j}]/; !FreeQ[rest, i] && FreeQ[rest, _FeynArts`SumOver]


(* ---------------------------------------------------------------------- *)
(*	Cvitanovic's algorithm to simplify chains that contain common indices *)
(* ---------------------------------------------------------------------- *)


colorSimplifyGeneric[rest_. SUNTF[{},i_,j_]]:=
	colorSimplifyGeneric[rest SUNFDelta[i,j]];

(* ... T^a T^a ... *)
colorSimplifyGeneric[rest_. holdDOTColor[xx___, SUNT[a_SUNIndex], SUNT[a_SUNIndex], yy___]]:=
	(SUNN^2 -1)/(2 SUNN) colorSimplifyGeneric[rest holdDOTColor[xx,yy]];

(* [... T^a T^a ...]_ij *)
colorSimplifyGeneric[rest_. SUNTF[{xx___,a_SUNIndex,a_SUNIndex,yy___},i_,j_]]:=
	(SUNN^2 -1)/(2 SUNN) colorSimplifyGeneric[rest SUNTF[{xx,yy},i,j]];

(* ... T^a T^b T^a ... *)
colorSimplifyGeneric[rest_. holdDOTColor[xx___, SUNT[a_SUNIndex], SUNT[b_SUNIndex], SUNT[a_SUNIndex], yy___]] :=
		(-1)/(2 SUNN) colorSimplifyGeneric[rest holdDOTColor[xx, SUNT[b], yy]];
(* [... T^a T^b T^a ...]_ij *)
colorSimplifyGeneric[rest_. SUNTF[{xx___,a_SUNIndex,b_SUNIndex,a_SUNIndex,yy___},i_,j_]] :=
		(-1)/(2 SUNN) colorSimplifyGeneric[rest SUNTF[{xx,b,yy}, i,j]];

(* Tr[... T^a T^b T^a ...] *)
colorSimplifyGeneric[rest_. sunTrace[holdDOTColor[xx___, SUNT[a_SUNIndex], SUNT[b_SUNIndex], SUNT[a_SUNIndex], yy___]]] :=
		(-1)/(2 SUNN) colorSimplifyGeneric[rest sunTrace[holdDOTColor[xx, SUNT[b], yy]]];

(* ... T^a T^b ... T^a ... *)
colorSimplifyGeneric[rest_. holdDOTColor[A___, SUNT[i_SUNIndex], SUNT[a_SUNIndex], B:SUNT[_SUNIndex].. , SUNT[i_SUNIndex], C___]] :=
			1/2 colorSimplifyGeneric[rest holdDOTColor[A,C] sunTrace[holdDOTColor[SUNT[a],B]]] - 1/(2 SUNN) colorSimplifyGeneric[rest holdDOTColor[A,SUNT[a],B,C]];

(* [... T^a T^b ... T^a ...]_jk *)
colorSimplifyGeneric[rest_. SUNTF[{A___,i_SUNIndex,a_SUNIndex,B__SUNIndex,i_SUNIndex,C___},j_,k_]] :=
			1/2 colorSimplifyGeneric[rest SUNTF[{A,C}, j,k] sunTrace[holdDOTColor[SUNT[a],Sequence@@(SUNT/@{B})]]] - 1/(2 SUNN) colorSimplifyGeneric[rest SUNTF[{A,a,B,C},j,k]];

(* ... Tr[... T^a T^b ... T^a ...] ... *)
colorSimplifyGeneric[rest_. sunTrace[holdDOTColor[A___, SUNT[i_SUNIndex], SUNT[a_SUNIndex], B:SUNT[_SUNIndex].. , SUNT[i_SUNIndex], C___]]] :=
			1/2 colorSimplifyGeneric[rest sunTrace[holdDOTColor[A,C]] sunTrace[holdDOTColor[SUNT[a],B]]] - 1/(2 SUNN) colorSimplifyGeneric[rest sunTrace[holdDOTColor[A,SUNT[a],B,C]]];

(* T^a Tr[ ... T^a ... ] *)
colorSimplifyGeneric[rest_. SUNT[a_SUNIndex] sunTrace[holdDOTColor[xc___, SUNT[a_SUNIndex], xd___]]] :=
		1/2 colorSimplifyGeneric[rest holdDOTColor[xd,xc]] - 1/(2 SUNN) colorSimplifyGeneric[rest sunTrace[holdDOTColor[xc,xd]]];

(* T^a_ij Tr[ ... T^a ... ] *)
colorSimplifyGeneric[rest_. SUNTF[{a_SUNIndex},i_,j_] sunTrace[holdDOTColor[xc___, SUNT[a_SUNIndex], xd___]]] :=
		1/2 colorSimplifyGeneric[rest SUNTF[(First /@ {xd,xc}),i,j]] - 1/(2 SUNN) colorSimplifyGeneric[rest SUNFDelta[i,j] sunTrace[holdDOTColor[xc,xd]]];

(* ... T^a ... Tr[ ... T^a ... ] *)
colorSimplifyGeneric[rest_. holdDOTColor[xa___, SUNT[a_SUNIndex], xb___] sunTrace[holdDOTColor[xc___, SUNT[a_SUNIndex], xd___]]] :=
		1/2 colorSimplifyGeneric[rest holdDOTColor[xa,xd,xc,xb]] - 1/(2 SUNN) colorSimplifyGeneric[rest holdDOTColor[xa,xb] sunTrace[holdDOTColor[xc,xd]]];

(* ... T^a ... Tr[ ... T^a ... ] *)
colorSimplifyGeneric[rest_. SUNTF[{xa___, a_SUNIndex, xb___},i_,j_] sunTrace[holdDOTColor[xc___, SUNT[a_SUNIndex], xd___]]] :=
		1/2 colorSimplifyGeneric[rest SUNTF[Flatten[{xa,First/@{xd},First/@{xc},xb}],i,j]] - 1/(2 SUNN) colorSimplifyGeneric[rest SUNTF[{xa,xb},i,j] sunTrace[holdDOTColor[xc,xd]]];


(* Tr[... T^a ...] Tr[ ... T^a ... ] *)
colorSimplifyGeneric[rest_. sunTrace[holdDOTColor[xa___, SUNT[a_SUNIndex], xb___]] sunTrace[holdDOTColor[xc___, SUNT[a_SUNIndex], xd___]]] :=
		1/2 colorSimplifyGeneric[rest sunTrace[holdDOTColor[xa,xd,xc,xb]]] - 1/(2 SUNN) colorSimplifyGeneric[rest sunTrace[holdDOTColor[xa,xb]] sunTrace[holdDOTColor[xc,xd]]];

colorSimplifyGeneric[rest_. sunTrace[holdDOTColor[xa___, SUNT[_SUNIndex], xb___]]^2] :=
		1/2 colorSimplifyGeneric[rest sunTrace[holdDOTColor[xa,xb,xa,xb]]] - 1/(2 SUNN) colorSimplifyGeneric[rest sunTrace[holdDOTColor[xa,xb]]^2];


(* ---------------------------------------------------------------------- *)
(*	Various contractions of structure constants							  *)
(* ---------------------------------------------------------------------- *)

(* (f^abc)^2 = 2 CA^2 CF *)
colorSimplifyGeneric[rest_. sunf[_SUNIndex,_SUNIndex,_SUNIndex]^2]:=
	2 CA^2 CF colorSimplifyGeneric[rest];

(* (d^abc)^2 = - 2 (4 - CA^2) CF *)
colorSimplifyGeneric[rest_. sund[_SUNIndex,_SUNIndex,_SUNIndex]^2]:=
	-2 (4-CA^2) CF colorSimplifyGeneric[rest];

(* f^abc f^abd = N d^{cd} *)
colorSimplifyGeneric[rest_. sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___] sunf[r4___,a_SUNIndex,r5___,b_SUNIndex,r6___]] :=
			SUNN (-1)^(Length[{r2}] + Length[{r5}]) colorSimplifyGeneric[rest SUNDelta[r2,r3,r1,r5,r6,r4]];

colorSimplifyGeneric[rest_. sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___] sunf[r4___,b_SUNIndex,r5___,a_SUNIndex,r6___]] :=
			SUNN (-1)^(Length[{r2}] + Length[{r5}]+1) colorSimplifyGeneric[rest SUNDelta[r2,r3,r1,r5,r6,r4]];

(* d^abc d^abd = (N^2-4)/N d^{cd} *)
colorSimplifyGeneric[rest_. sund[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___] sund[s1___,a_SUNIndex,s2___,b_SUNIndex,s3___]] :=
			(SUNN^2 - 4)/SUNN colorSimplifyGeneric[rest SUNDelta[r1,r2,r3,s1,s2,s3]];

(* d^abc d^abd = (N^2-4)/N d^{cd} *)
colorSimplifyGeneric[rest_. sund[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___] sund[s1___,b_SUNIndex,s2___,a_SUNIndex,s3___]] :=
			(SUNN^2 - 4)/SUNN colorSimplifyGeneric[rest SUNDelta[r1,r2,r3,s1,s2,s3]];

(*
			f^abc d^abe = 0
			Since we intend to catch all such SUNF*SUND products here, there is no need to consider them
			later when dealing with SUND2/SUNF2 symbols
*)
colorSimplifyGeneric[_. sunf[___,a_SUNIndex,___,b_SUNIndex,___] sund[___,a_SUNIndex,___,b_SUNIndex,___]]:=
		0;

colorSimplifyGeneric[_. sunf[___,a_SUNIndex,___,b_SUNIndex,___] sund[___,b_SUNIndex,___,a_SUNIndex,___]]:=
		0;

(* ---------------------------------------------------------------------- *)
(* Products of a single SUNF/SUND with a SUNT 							  *)
(* ---------------------------------------------------------------------- *)

(* f^abc T^a *)
colorSimplifyGeneric[rest_. sunf[r1___,a_SUNIndex, r2___] SUNT[a_SUNIndex]]:=
	(
	I colorSimplifyGeneric[rest holdDOTColor[SUNT[Last[{r2,r1}]], SUNT[First[{r2,r1}]]]] -
	I colorSimplifyGeneric[rest holdDOTColor[SUNT[First[{r2,r1}]], SUNT[Last[{r2,r1}]]]]
	)/; Length[{r2,r1}]===2;

(* f^abc (... T^a ...) *)
colorSimplifyGeneric[rest_. sunf[r1___,a_SUNIndex, r2___] holdDOTColor[r3___,SUNT[a_SUNIndex],r4___]]  :=
	(
	I colorSimplifyGeneric[rest holdDOTColor[r3,SUNT[Last[{r2,r1}]], SUNT[First[{r2,r1}]],r4]] -
	I colorSimplifyGeneric[rest holdDOTColor[r3,SUNT[First[{r2,r1}]], SUNT[Last[{r2,r1}]],r4]]
	)/; Length[{r2,r1}]===2;

(* f^abc Tr(... T^a ...) *)
colorSimplifyGeneric[rest_. sunf[r1___,a_SUNIndex, r2___] sunTrace[holdDOTColor[r3___,SUNT[a_SUNIndex],r4___]]]  :=
	(
	I colorSimplifyGeneric[rest sunTrace[holdDOTColor[r3,SUNT[Last[{r2,r1}]], SUNT[First[{r2,r1}]],r4]]] -
	I colorSimplifyGeneric[rest sunTrace[holdDOTColor[r3,SUNT[First[{r2,r1}]], SUNT[Last[{r2,r1}]],r4]]]
	)/; Length[{r2,r1}]===2;

(* d^abc T^a *)
colorSimplifyGeneric[rest_. sund[r1___,a_SUNIndex, r2___] SUNT[a_SUNIndex]] :=
	(
	colorSimplifyGeneric[rest holdDOTColor[SUNT[Last[{r2,r1}]], SUNT[First[{r2,r1}]]]] +
	colorSimplifyGeneric[rest holdDOTColor[SUNT[First[{r2,r1}]], SUNT[Last[{r2,r1}]]]] -
	(1/SUNN) colorSimplifyGeneric[rest SUNDelta[r1,r2]]
	) /; Length[{r2,r1}]===2;

(* d^abc (... T^a ...) *)
colorSimplifyGeneric[rest_. sund[r1___,a_SUNIndex, r2___] holdDOTColor[r3___,SUNT[a_SUNIndex],r4___]]  :=
	(
	colorSimplifyGeneric[rest holdDOTColor[r3,SUNT[Last[{r2,r1}]], SUNT[First[{r2,r1}]],r4]] +
	colorSimplifyGeneric[rest holdDOTColor[r3,SUNT[First[{r2,r1}]], SUNT[Last[{r2,r1}]],r4]] -
	(1/SUNN) colorSimplifyGeneric[rest SUNDelta[r1,r2] holdDOTColor[r3,r4]]
	)/; Length[{r2,r1}]===2;

(* d^abc Tr (... T^a ...) *)
colorSimplifyGeneric[rest_. sund[r1___,a_SUNIndex, r2___] sunTrace[holdDOTColor[r3___,SUNT[a_SUNIndex],r4___]]]  :=
	(
	colorSimplifyGeneric[rest sunTrace[holdDOTColor[r3,SUNT[Last[{r2,r1}]] , SUNT[First[{r2,r1}]],r4]]] +
	colorSimplifyGeneric[rest sunTrace[holdDOTColor[r3,SUNT[First[{r2,r1}]] , SUNT[Last[{r2,r1}]],r4]]] -
	(1/SUNN) colorSimplifyGeneric[rest SUNDelta[r1,r2] sunTrace[holdDOTColor[r3,r4]]]
	)/; Length[{r2,r1}]===2;



(* ---------------------------------------------------------------------- *)
(* Explicit fundamental color indices  							  		  *)
(* ---------------------------------------------------------------------- *)

(* SUNTFs *)
colorSimplifyGeneric[rest_. SUNTF[{x__}, i_, j_SUNFIndex] SUNTF[{y__}, j_SUNFIndex, k_]] :=
	colorSimplifyGeneric[rest SUNTF[{x,y}, i, k]];

colorSimplifyGeneric[rest_. SUNTF[{x__}, i_SUNFIndex, i_SUNFIndex]] :=
	colorSimplifyGeneric[rest sunTrace[holdDOTColor @@ (SUNT /@ {x})]];


(* ---------------------------------------------------------------------- *)
(*	More complicated contractions of structure constants
	(requires colorSimplifyToSUNF2SUND2)						  		  *)
(* ---------------------------------------------------------------------- *)

(* f_2^abcd f^abe = N f^ecd *)
colorSimplifyGeneric[rest_. sunf2[a_SUNIndex, b_SUNIndex, c_, d_] sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___]] :=
	SUNN (-1)^(Length[{r2}]) colorSimplifyGeneric[rest sunf[r2,r3,r1,c,d]];

(* f_2^bacd f^abe = - N f^ecd *)
colorSimplifyGeneric[rest_. sunf2[b_SUNIndex, a_SUNIndex, c_, d_] sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___]] :=
	SUNN (-1)^(Length[{r2}]+1) colorSimplifyGeneric[rest sunf[r2,r3,r1,c,d]];

(* f_2^abcd f^cde = N f^eab *)
colorSimplifyGeneric[rest_. sunf2[c_, d_, a_SUNIndex, b_SUNIndex] sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___]] :=
	SUNN (-1)^(Length[{r2}]) colorSimplifyGeneric[rest sunf[r2,r3,r1,c,d]];

(* f_2^abdc f^cde = - N f^eab *)
colorSimplifyGeneric[rest_. sunf2[c_, d_, b_SUNIndex, a_SUNIndex] sunf[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___]] :=
	SUNN (-1)^(Length[{r2}]+1) colorSimplifyGeneric[rest sunf[r2,r3,r1,c,d]];

(* f_2^abcd f^ace = N/2 f^bde *)
colorSimplifyGeneric[rest_. sunf2[a_SUNIndex, b_, c_SUNIndex, d_] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___]] :=
	SUNN/2 (-1)^(Length[{r2}]) colorSimplifyGeneric[rest sunf[b,d,r2,r3,r1]];

(* f_2^cbad f^ace = - N/2 f^bde *)
colorSimplifyGeneric[rest_. sunf2[c_SUNIndex, b_, a_SUNIndex, d_] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___]] :=
	SUNN/2 (-1)^(Length[{r2}]+1) colorSimplifyGeneric[rest sunf[b,d,r2,r3,r1]];

(* f_2^abdc f^ace = - N/2 f^bde *)
colorSimplifyGeneric[rest_. sunf2[a_SUNIndex, b_, d_, c_SUNIndex] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___]] :=
	SUNN/2 (-1)^(Length[{r2}]+1) colorSimplifyGeneric[rest sunf[b,d,r2,r3,r1]];

(* f_2^cbda f^ace = - N/2 f^bde *)
colorSimplifyGeneric[rest_. sunf2[c_SUNIndex, b_, d_, a_SUNIndex] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___]] :=
	SUNN/2 (-1)^(Length[{r2}]) colorSimplifyGeneric[rest sunf[b,d,r2,r3,r1]];

(* f_2^bacd f^ace = - N/2 f^bde *)
colorSimplifyGeneric[rest_. sunf2[b_, a_SUNIndex, c_SUNIndex, d_] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___]] :=
	SUNN/2 (-1)^(Length[{r2}]+1) colorSimplifyGeneric[rest sunf[b,d,r2,r3,r1]];

(* f_2^bcad f^ace = - N/2 f^bde *)
colorSimplifyGeneric[rest_. sunf2[b_, a_SUNIndex, c_SUNIndex, d_] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___]] :=
	SUNN/2 (-1)^(Length[{r2}]) colorSimplifyGeneric[rest sunf[b,d,r2,r3,r1]];

(* f_2^badc f^ace = - N/2 f^bde *)
colorSimplifyGeneric[rest_. sunf2[b_, a_SUNIndex, d_, c_SUNIndex] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___]] :=
	SUNN/2 (-1)^(Length[{r2}]) colorSimplifyGeneric[rest sunf[b,d,r2,r3,r1]];

(* f_2^bcda f^ace = - N/2 f^bde *)
colorSimplifyGeneric[rest_. sunf2[b_, c_SUNIndex, d_, a_SUNIndex] sunf[r1___,a_SUNIndex,r2___,c_SUNIndex,r3___]] :=
	SUNN/2 (-1)^(Length[{r2}]+1) colorSimplifyGeneric[rest sunf[b,d,r2,r3,r1]];

(*
	Instead of trying to work out all possible products of two sunf2 symbols, it is easier
	to convert one of them into a product of two sunf symbols.
*)
colorSimplifyGeneric[rest_. sunf2[x__] sunf2[a_SUNIndex,b_SUNIndex,c_SUNIndex,d_SUNIndex]] :=
	(
	dummyInd=SUNIndex[FCGV[ToString[Unique["sun"]]]];
	colorSimplifyGeneric[rest sunf2[x] sunf[dummyInd,a,b] sunf[dummyInd,c,d]]
	);


(* d_2^abci d_2^idea *)
colorSimplifyGeneric[rest_. sund2[b1___, a_SUNIndex, b2___, c1___, i_SUNIndex, c2___] sund2[d1___,i_SUNIndex, d2___, e1___, a_SUNIndex, e2___]] :=
	(
	(SUNN/4-4/SUNN) colorSimplifyGeneric[rest sund2[c1,c2,d1,d2,b1,b2,e1,e2]] -
	SUNN/4 colorSimplifyGeneric[rest sund2[b1,b2,d1,d2,c1,c2,e1,e2]] +
	(SUNN/4-4/SUNN) colorSimplifyGeneric[rest sund2[d1,d2,e1,e2,b1,b2,c1,c2]] +
	(1 - 4/SUNN^2) colorSimplifyGeneric[rest SUNDelta[b1,b2,e1,e2] SUNDelta[c1,c2,d1,d2]] +
	(1 - 4/SUNN^2) colorSimplifyGeneric[rest SUNDelta[b1,b2,c1,c2] SUNDelta[d1,d2,e1,e2]]
	)/; Length[{b1,b2}]==1 && Length[{c1,c2}]==1 && Length[{d1,d2}]==1 && Length[{e1,e2}]==1;


(* d_2^abci d_2^adei *)
colorSimplifyGeneric[rest_. sund2[b1___, a_SUNIndex, b2___, c1___, i_SUNIndex, c2___] sund2[d1___, a_SUNIndex, d2___, e1___, i_SUNIndex, e2___]] :=
	(
	(SUNN/4-4/SUNN) colorSimplifyGeneric[rest sund2[b1,b2,d1,d2,c1,c2,e1,e2]] -
	SUNN/4 colorSimplifyGeneric[rest sund2[c1,c2,d1,d2,b1,b2,e1,e2]] +
	(SUNN/4-4/SUNN) colorSimplifyGeneric[rest sund2[d1,d2,e1,e2,b1,b2,c1,c2]] +
	(1 - 4/SUNN^2) colorSimplifyGeneric[rest SUNDelta[b1,b2,d1,d2] SUNDelta[c1,c2,e1,e2]] +
	(1 - 4/SUNN^2) colorSimplifyGeneric[rest SUNDelta[b1,b2,c1,c2] SUNDelta[d1,d2,e1,e2]]
	)/; Length[{b1,b2}]==1 && Length[{c1,c2}]==1 && Length[{d1,d2}]==1 && Length[{e1,e2}]==1;

(*
	d_2^iabj d^abk = (N/2 - 6/N) d^ijk + permutations
	Can be derived using SUNSimplify[SUND[i, a, b] SUND[j, b, c] SUND[k, c, a],
		Explicit -> True, SUNNToCACF -> False, SUNTrace -> True]
*)
colorSimplifyGeneric[rest_. sund2[l1___,a_SUNIndex,l2_,b_SUNIndex,l3___] sund[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___]] :=
	(SUNN/2 - 6/SUNN) colorSimplifyGeneric[rest sund[l1,l2,l3,r1,r2,r3]] /; Length[{l1,l2,l3}]===2;

colorSimplifyGeneric[rest_. sund2[l1___,a_SUNIndex,l2_,b_SUNIndex,l3___] sund[r1___,b_SUNIndex,r2___,a_SUNIndex,r3___]] :=
	(SUNN/2 - 6/SUNN) colorSimplifyGeneric[rest sund[l1,l2,l3,r1,r2,r3]] /; Length[{l1,l2,l3}]===2;

colorSimplifyGeneric[rest_. sund2[l1_,a_SUNIndex,b_SUNIndex,l2_] sund[r1___,a_SUNIndex,r2___,b_SUNIndex,r3___]] :=
	(SUNN/2 - 6/SUNN) colorSimplifyGeneric[rest sund[l1,l2,r1,r2,r3]];

colorSimplifyGeneric[rest_. sund2[l1_,a_SUNIndex,b_SUNIndex,l2_] sund[r1___,b_SUNIndex,r2___,a_SUNIndex,r3___]] :=
	(SUNN/2 - 6/SUNN) colorSimplifyGeneric[rest sund[l1,l2,r1,r2,r3]];

colorSimplifyGeneric[rest_. sund2[a_SUNIndex,b_SUNIndex,a_SUNIndex,b_SUNIndex]] :=
	-2 (4-CA^2) CF colorSimplifyGeneric[rest];


(* ---------------------------------------------------------------------- *)
(* Properties of sunf, sund, sunf2 and sund2 							  *)
(* ---------------------------------------------------------------------- *)

sund[a_SUNIndex,b_SUNIndex,c_SUNIndex]:=
	0/; (Signature[{a,b,c}] === 0) && FCPatternFreeQ[{a,b,c}];

sund[a_SUNIndex,b_SUNIndex,c_SUNIndex]:=
	sund@@Sort[{a,b,c}]/; !OrderedQ[{a,b,c}] && FCPatternFreeQ[{a,b,c}];

sunf[a_SUNIndex,b_SUNIndex,c_SUNIndex]:=
	0/; (Signature[{a,b,c}] === 0) && FCPatternFreeQ[{a,b,c}];


(*

	f^abc f^ade = f_2^bcde

	properties of f_2:

		f_2^abcd = - f_2^bacd
		f_2^abcd = - f_2^abdc
		f_2^abcd = + f_2^cdab

		f_2^aabc = 0
		f_2^abcc = 0

		f_2^abac = N d^bc
		f_2^abcb = N d^ac

		f_2^abca = -N d^bc
		f_2^abbc = -N d^ac

*)

sunf2[a_SUNIndex,a_SUNIndex,_,_]:=
	0;

sunf2[_,_,a_SUNIndex,a_SUNIndex]:=
	0;

sunf2[a_,b_,c_,d_]:=
	-sunf2[b,a,c,d]/; !OrderedQ[{a,b}] && FCPatternFreeQ[{a,b,c,d}];

sunf2[a_,b_,c_, d_]:=
	-sunf2[a,b,d,c]/; !OrderedQ[{c,d}] && FCPatternFreeQ[{a,b,c,d}];

sunf2[a_,b_,c_,d_]:=
	-sunf2[c,d,a,b]/; !OrderedQ[{{a,b},{c,d}}] && FCPatternFreeQ[{a,b,c,d}];

sunf2[a_SUNIndex,b_,a_SUNIndex,c_]:=
	SUNN SUNDelta[b,c];

sunf2[a_,b_SUNIndex,c_,b_SUNIndex]:=
	SUNN SUNDelta[a,c];

sunf2[a_SUNIndex,b_,c_,a_SUNIndex]:=
	- SUNN SUNDelta[b,c];

sunf2[a_,b_SUNIndex,b_SUNIndex,c_]:=
	- SUNN SUNDelta[a,c];

(*

	d^abc d^ade = d_2^bcde

	properties of d_2:

		d_2^abcd =  d_2^bacd
		d_2^abcd =  d_2^abdc
		d_2^abcd =  d_2^cdab

	*)

sund2[a_,a_,b__]:=
	0/; FCPatternFreeQ[{a,b}];

sund2[a__,b_,b_]:=
	0/; FCPatternFreeQ[{a,b}];

sund2[a_,b_,c_,d_]:=
	sund2[b,a,c,d]/; !OrderedQ[{a,b}] && FCPatternFreeQ[{a,b,c,d}];

sund2[a_,b_,c_, d_]:=
	sund2[a,b,d,c]/; !OrderedQ[{c,d}] && FCPatternFreeQ[{a,b,c,d}];

sund2[a_,b_,c_,d_]:=
	sund2[c,d,a,b]/; !OrderedQ[{{a,b},{c,d}}] && FCPatternFreeQ[{a,b,c,d}];


FCPrint[1,"SUNSimplify.m loaded."];
End[]
