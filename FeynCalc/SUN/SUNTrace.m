(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNTrace *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 March '98 at 9:07 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: calculation of traces of SUNT-matrices (color factors)*)

(* ------------------------------------------------------------------------ *)

SUNTrace::usage=
"SUNTrace[exp] calculates the color-trace.";
(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SUNTrace`Private`"]

Options[SUNTrace] = {
	Explicit -> False
};

HoldPattern[SUNTrace[n_, ___Rule]] :=
	SUNN n /; FCPatternFreeQ[{n}, {SUNT}];

(* this is the function which puts everything together ********* *)
SUNTrace[expr_Plus, op___Rule] :=
	Map[SUNTrace[#, op]&, expr];

(*new; suggested by Frederik Orellana *)
SUNTrace[expr_Times,op___Rule] :=
	(Select[expr, FreeQ2[#, {SUNT,SUNF,SUND,SUNDelta}]&] *
	SUNTrace[Select[expr, !FreeQ2[#, {SUNT,SUNF,SUND,SUNDelta}]&], op])/;
	(expr === fcis[expr]) &&
	(Select[expr, !FreeQ2[#, {SUNT,SUNF,SUND,SUNDelta}]&] =!= 1) &&
	(*To avoid infinite recursion. 3/9-2002. Frederik Orellana*)
	(Select[expr, FreeQ2[#, {SUNT,SUNF,SUND,SUNDelta}]&] =!= 1);

HoldPattern[SUNTrace[1 .., ___Rule]] =
	SUNN;

HoldPattern[SUNTrace[a_, ___Rule]] :=
	0 /; MatchQ[a, SUNT[SUNIndex[_]]] && nopat[a] && (fcis[a] === a);

HoldPattern[SUNTrace[a_, o___Rule]] :=
	SUNTrace[fcis[a],o] /; (fcis[a]=!=a) && nopat[a];

SUNTrace[DOT[SUNT[x_SUNIndex] , SUNT[y_SUNIndex]], ___Rule] :=
	SUNDelta[x, y]/2;

SUNTrace[DOT[SUNT[a_SUNIndex] , SUNT[b_SUNIndex] , SUNT[c_SUNIndex]], opt___Rule] :=
	(SUND[a, b, c]/4 + I SUNF[a,b,c]/4) /; Length[Union[{a,b,c}]]===3 &&
	(Explicit /. {opt} /. Options[SUNTrace]) === True;

(* recursion suggested by Peter Cho *)
SUNTrace[DOT[SUNT[a_] , SUNT[b_] , SUNT[c_] , SUNT[d_] , (more__SUNT)],	opt___Rule] :=
	Block[ {f},
		f = Unique["c"];
		SUNDelta[a,b]/2/SUNN SUNTrace[DOT[SUNT[c],SUNT[d],more],opt] +
			1/2 SUND[a,b,f] SUNTrace[DOT[SUNT[f],SUNT[c],SUNT[d],more],opt] +
			I/2 SUNF[a,b,f] SUNTrace[DOT[SUNT[f],SUNT[c],SUNT[d],more],opt]
	]/;
	(Union[Head /@ {a,b,c,d}] === {SUNIndex}) &&
	((Explicit /. {opt} /. Options[SUNTrace]) === True );

SUNTrace[DOT[SUNT[a_], SUNT[b_], SUNT[c_], SUNT[d_]], opt___Rule] :=
	Block[ {e = FCGV[ToString[Unique["e"]]]},
		Expand[1/4/SUNN(SUNDelta[a, b] SUNDelta[c, d] -
						SUNDelta[a, c] SUNDelta[b, d] +
						SUNDelta[a, d] SUNDelta[b, c]) +
				1/8(SUND[a,b,e] SUND[c,d,e] -
					SUND[a,c,e] SUND[b,d,e] +
					SUND[a,d,e] SUND[b,c,e]
					) +
				I/8(SUND[a,d,e] SUNF[b,c,e] -
					SUNF[a,d,e] SUND[b,c,e])
		]
	] /; (Union[Head /@ {a,b,c,d}] === {SUNIndex}) &&
				(Explicit /. {opt} /. Options[SUNTrace]) === True;

HoldPattern[SUNTrace[ SUNTrace[x__] y_., op___Rule ]] :=
	SUNTrace[x] SUNTrace[y, op];

SUNTrace/:  SUNTrace[DOT[(A___), SUNT[x_SUNIndex], B___],___Rule] *
			SUNTrace[DOT[(a___), SUNT[x_SUNIndex], b___],___Rule] :=
		FixedPoint[cvit,  (gmcyc[DOT[A,SUNT[x],B]] gmcyc[DOT[a,SUNT[x],b]])/.
		SUNTrace->gellm1/. DOT->gm2lambdaT/.gellm1->gellm2/. SUNF->f2tr
		]/.lambdaT->SUNT/.gellm2->SUNTrace;

SUNTrace /: HoldPattern[SUNTrace[x_,o___Rule]^2] :=
	SUNTrace[x,o] * SUNTrace[x,o];

SUNTrace[ a_, ___Rule ] :=
	fixgell[a]/; NumberQ[fixgell[a]] &&	FreeQ[a, Pattern] && (fcis[a] === a);

SUNTrace[ expr_, OptionsPattern[] ] :=
	(fixgell[expr]/. gellm2->SUNTrace)/;
	((Head[expr] =!= Times) || (Select[expr, !FreeQ[#, SUNT]&] ===1 )) &&
	(expr=!=(fixgell[expr] /. gellm2->Identity)) && FCPatternFreeQ[{expr}] &&
	(fcis[expr]===expr);

SUNTrace[x_?externQ] :=
	SUNTrace[fcis[x]];

(*	typesetting	*)
SUNTrace /:
	MakeBoxes[SUNTrace[a_,___Rule], TraditionalForm] :=
	TBox["tr","(",a,")"];

fcis[z_ /; FreeQ[z, Pattern]] :=
	(fcis[z] = DotSimplify[FeynCalcInternal[z]]);
(* change SUNT' which are multiplied with each other to lambdaT's *)
lambdaT[1] =
	1;
gm2lambdaT[x__] :=
	(gmlin@@( {x}/.SUNT->lambdaT ) )/.gmlin->DOT;
(********************* linearity  ********************************* *)
(* noncomQdef : checking non-commutativity *)
noncomQ[z_] :=
	TrueQ[noncQ[z]];

noncQ[_ ?NumberQ] :=
	True;

noncQ[_SUNTrace] :=

	True;
noncQ[x_] :=

	If[ FreeQ2[FixedPoint[ReleaseHold, x], $NonComm],
		True,
		False
	];

gmlin/: HoldPattern[gmlin[gmlin[x__]]] :=
	gmlin[x];

gmlin[ a___, b_ c_, d___ ] :=
	b gmlin[a,c,d]/;FreeQ[b, lambdaT] && noncomQ[b];

gmlin[ a___, b_ , d___ ] :=

	b gmlin[a,d]/;FreeQ[b, lambdaT] && noncomQ[b];
gmlin[] =
	1;
gellm1[x_, y__] :=
	gellm1[DOT[x, y]];

gellm2[x_, y__] :=
	gellm2[DOT[x, y]];
(******************* cyclicity *********************************** *)
gmcyc[x__] :=
	gellm1 @@ First[NestList[RotateLeft, {x}, Length[{x}]-1]//Sort];
(************* define the properties of trace of T-matrices *)
gellm2[ ] =
	gmcyc[ ] = SUNN;         (* unit trace  *)
(************** each single T-matrix has vanishing trace *)
gellm2[ lambdaT[_] ] :=
	0;
(************** Cvitanovic - rules ******************************* *)
gellm2[DOT[a___, HoldPattern[lambdaT[i_]], b___, HoldPattern[lambdaT[i_]], c___]] :=
	(1/2 gmcyc[b] gmcyc[a, c] - 1/2/SUNN gmcyc[a, b, c]) /; (Head[i] === SUNIndex) && !IntegerQ[i];

gellm2/: gellm2[DOT[a___, HoldPattern[lambdaT[i_]], b___]]^2 :=
	(1/2 gmcyc[a, b, a, b] - 1/2/SUNN gmcyc[a, b]^2) /; (Head[i] === SUNIndex) && !IntegerQ[i];

gellm2/:gellm2[DOT[a___, HoldPattern[lambdaT[i_]], b___]] gellm2[DOT[c___, HoldPattern[lambdaT[i_]], d___]]:=
	(1/2 gmcyc[a, d, c, b] - 1/2/SUNN gmcyc[a, b] gmcyc[c, d]) /; (Head[i] === SUNIndex) && !IntegerQ[i];

f2tr[i_,j_,k_,___] :=
	2 I (gmcyc @@ lambdaT/@{i,k,j} - gmcyc @@ lambdaT/@{i,j,k});
(* do the application of the Cvitanovic - relations step by step *)
cvit[x_Plus] :=
	cvit/@x;
cvit[x_] :=
	(cvit[x] = ExpandAll[ x /. gellm1 -> gellm2 ]);



nopat[x_] :=
	FreeQ2[x, {Pattern, Blank, BlankSequence, BlankNullSequence}];



gellm1[x_Plus] :=
	gellm1 /@ x;

gellm1/:
	gellm1[x_ y_] :=
		x gellm1[y]/;FreeQ[x,lambdaT];
gellm1/:
	gellm1[DOT[x_,x1__] gellm1[y___]]:=
		gellm1[y] gellm1[DOT[x,x1]];

gellex[z_] :=
	gellm1[ExpandAll[z]];

fixgell[x_] :=
	(fixgell[x] =
	FixedPoint[cvit, ( gellm1[ExpandAll[x/.SUNTrace->gellex/.
	DOT->gm2lambdaT/.SUNF->f2tr]]
	)/.gellm1->gellm2, 19]/.lambdaT->SUNT)/; FreeQ2[x, {Pattern, Blank, BlankSequence, BlankNullSequence}];

externQ[xx_] :=
	If[ !FreeQ[xx, Pattern],
		False,
		If[ fcis[xx] === xx,
			False,
			True
		]
	];

FCPrint[1,"SUNTrace.m loaded."];
End[]
