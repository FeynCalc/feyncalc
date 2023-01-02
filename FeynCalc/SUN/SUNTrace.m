(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SUNTrace     													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:  Calculation of color traces 									*)

(* ------------------------------------------------------------------------ *)

SUNTrace::usage=
"SUNTrace[exp] is the head of color traces. By default the trace is not
evaluated. The evaluation occurs only when the option SUNTraceEvaluate is set
to True. It is recommended to use SUNSimplify, which will automatically
evaluate all color traces involving 2 or 3 matrices in the input expression.";

SUNTraceEvaluate::usage=
"SUNTraceEvaluate is an option for SUNTrace, SUNSimplify and some other
functions. If set to False, color traces remain unevaluated. Automatic implies
evaluation of traces with 2 or 3 color matrices. Setting this option to True
will force every trace to be evaluated. For more details, see the
documentation for SUNTrace and SUNSimplify.";

SUNTrace::failmsg =
"Error! SUNTrace has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"


(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SUNTrace`Private`"]

sTrVerbose::usage="";

Options[SUNTrace] = {
	FCColorIsolate		-> True,
	FCVerbose			-> False,
	FeynCalcExternal	-> False,
	FeynCalcInternal	-> False,
	SUNIndexNames 		-> {},
	SUNTraceEvaluate	-> False
};


SUNTrace /:
	MakeBoxes[SUNTrace[expr__, OptionsPattern[]], TraditionalForm]:=
	RowBox[{"tr","(",TBox[expr], ")"}]


SUNTrace[0, OptionsPattern[]] :=
	0;


SUNTrace[expr_, OptionsPattern[]] :=
	Block[{	ex, res, time, sunHead, sunObjects,
			sunObjectsEval, null1, null2, freePart, sunPart, repRule,
			optSUNIndexNames},


		If [OptionValue[FCVerbose]===False,
			sTrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				sTrVerbose=OptionValue[FCVerbose]
			];
		];


		FCPrint[1, "SUNTrace: Entering.", FCDoControl->sTrVerbose];
		FCPrint[3, "SUNTrace: Entering with ", expr, FCDoControl->sTrVerbose];

		optSUNIndexNames = OptionValue[SUNIndexNames];


		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	OptionValue[FCColorIsolate],
			FCPrint[1, "SUNTrace: Standard mode.", FCDoControl->sTrVerbose];
			(* 	First of all we need to extract all the Color structures inside the trace. *)

			ex = FCColorIsolate[ex,FCI->True,Head->sunHead, SUNTrace->False, "ExpandNestedDOTs" -> True];

			ex = ex /. sunHead[x_]/;FreeQ[x,SUNT] :> x;

			ex = ex /. SUNTrace -> FeynCalc`Package`sunTrace;


			{freePart,sunPart} = FCSplit[ex,{sunHead,SUNT}];
			FCPrint[3,"SUNTrace: sunPart: ", sunPart , FCDoControl->sTrVerbose];
			FCPrint[3,"SUNTrace: freePart: ", freePart , FCDoControl->sTrVerbose];
			If [ sunPart=!=0,
				(* Check that there is only one sunHead per term and no nested sunHeads *)
				Scan[
					If[	!MatchQ[#, a_. sunHead[b_]/; (FreeQ[{a,b}, sunHead] && !FreeQ[b,SUNT])],
						Message[SUNTrace::failmsg, "Irregular trace structure in", InputForm[#]];
						Print[#];
						Abort[]
				]&, sunPart+sunHead[SUNT] ];
			];

			(* 	Now it is guaranteed that sunPart is of the form a*sunHead[x]+b*sunHead[y]+c*sunHead[z]+...
				So it is safe to extract all the sunHead objects and handle them separately	*)
			sunObjects = Cases[sunPart+null1+null2, sunHead[_], Infinity]//Union,


			FCPrint[1, "SUNTrace: Fast mode.", FCDoControl->sTrVerbose];
			(*	Fast mode for simple traces	*)
				If[	!FreeQ[ex,SUNT],
					freePart=0;

					sunPart=sunHead[ex]/. SUNTrace -> FeynCalc`Package`sunTrace;
					sunObjects = {sunPart},

					freePart=ex;
					sunPart=0;
					sunObjects = {}
				];
		];


		time=AbsoluteTime[];
		FCPrint[1,"SUNTrace: Applying colorSimplifyGeneric.", FCDoControl->sTrVerbose];

		sunObjectsEval = (FeynCalc`Package`colorSimplifyGeneric/@(sunObjects/.sunHead->Identity/. DOT->FeynCalc`Package`holdDOTColor)) /. FeynCalc`Package`colorSimplifyGeneric -> Identity;

		FCPrint[1,"SUNTrace: colorSimplifyGeneric finished, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sTrVerbose];
		FCPrint[3,"SUNTrace: After colorSimplifyGeneric: ", sunObjectsEval, FCDoControl->sTrVerbose];


		time=AbsoluteTime[];
		FCPrint[1,"SUNTrace: Applying sunTraceEvaluate.", FCDoControl->sTrVerbose];
		sunObjectsEval = Expand2[#,{SUNT}]&/@sunObjectsEval;
		sunObjectsEval = (sunTraceEvaluate/@sunObjectsEval) /. sunTraceEvaluate -> FeynCalc`Package`sunTrace;

		FCPrint[1,"SUNTrace: sunTraceEvaluate finished, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sTrVerbose];
		FCPrint[3,"SUNTrace: After sunTraceEvaluate: ", sunObjectsEval, FCDoControl->sTrVerbose];


		time=AbsoluteTime[];
		(*	We use colorSimplifyGeneric only for the contractions of SUNDeltas	*)
		FCPrint[1,"SUNTrace: Applying colorSimplifyGeneric again.", FCDoControl->sTrVerbose];

		sunObjectsEval = Expand2[#,{FeynCalc`Package`sunTrace,FeynCalc`Package`holdDOTColor,SUNT,FeynCalc`Package`sund,FeynCalc`Package`sunf,SUNDelta,SUNFDelta}]&/@(sunObjectsEval/.{
			SUND->FeynCalc`Package`sund,
			SUNF->FeynCalc`Package`sunf
		});

		sunObjectsEval = (FeynCalc`Package`colorSimplifyGeneric/@(sunObjectsEval)) /.
		FeynCalc`Package`colorSimplifyGeneric -> FeynCalc`Package`colorSimplifyToSUNF2SUND2 /.
		FeynCalc`Package`colorSimplifyToSUNF2SUND2 -> FeynCalc`Package`colorSimplifyGeneric;

		sunObjectsEval = sunObjectsEval/.
		FeynCalc`Package`colorSimplifyGeneric -> FeynCalc`Package`colorSimplifyFromSUNF2SUND2 /. FeynCalc`Package`colorSimplifyFromSUNF2SUND2 -> Identity;

		sunObjectsEval = sunObjectsEval/. {FeynCalc`Package`sund->SUND, FeynCalc`Package`sunf->SUNF} /. FeynCalc`Package`holdDOTColor[] ->1;

		FCPrint[1,"SUNTrace: colorSimplifyGeneric finished, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sTrVerbose];
		FCPrint[3,"SUNTrace: After 2nd colorSimplifyGeneric: ", sunObjectsEval, FCDoControl->sTrVerbose];


		repRule = Thread[Rule[sunObjects,sunObjectsEval]];
		FCPrint[3,"SUNTrace: Final replacement rule: ", repRule , FCDoControl->sTrVerbose];

		If[	!FreeQ[freePart,DOT],
			freePart = freePart /. DOT->FeynCalc`Package`holdDOTColor /. FeynCalc`Package`holdDOTColor[a__]/;NonCommFreeQ[{a}] :> Times[a] /. FeynCalc`Package`holdDOTColor -> DOT
		];

		res = (SUNN freePart) + ( sunPart /. Dispatch[repRule]);

		FCPrint[3,"SUNTrace: Preliminary result: ", res, FCDoControl->sTrVerbose];

		If[optSUNIndexNames=!={},
			res = FCCanonicalizeDummyIndices[res, FCI -> True, Head ->{SUNIndex}, SUNIndexNames->optSUNIndexNames];
		];


		If [OptionValue[FeynCalcExternal],
			res = FCE[res]
		];

		If[ !FreeQ[res/. FeynCalc`Package`sunTrace[_]:>1, SUNT],
			Message[SUNTrace::failmsg,"The output still contains color matrices"];
			Abort[]
		];

		res = res /. FeynCalc`Package`sunTrace->SUNTrace;

		FCPrint[1, "SUNTrace: Leaving.", FCDoControl->sTrVerbose];
		FCPrint[3, "SUNTrace: Leaving with ", res, FCDoControl->sTrVerbose];

		res
	]/; OptionValue[SUNTraceEvaluate];


sunTraceEvaluate[ex_Plus]:=
	sunTraceEvaluate/@ex;

sunTraceEvaluate[0]:=
	0;

sunTraceEvaluate[rest_]:=
	SUNN rest/; FreeQ[rest,SUNT];

(* Tr(T^a) *)
sunTraceEvaluate[rest_. SUNT[_SUNIndex] ]:=
	0/; FreeQ[rest,SUNT];

(* Tr(T^a T^b) *)
sunTraceEvaluate[rest_. FeynCalc`Package`holdDOTColor[SUNT[x_SUNIndex] , SUNT[y_SUNIndex]]]:=
	rest SUNDelta[x, y]/2/; FreeQ[rest,SUNT];


(* Tr(T^a T^b T^c) *)
sunTraceEvaluate[rest_. FeynCalc`Package`holdDOTColor[SUNT[a_SUNIndex] , SUNT[b_SUNIndex] , SUNT[c_SUNIndex]]] :=
	rest (SUND[a, b, c]/4 + I SUNF[a,b,c]/4)/; FreeQ[rest,SUNT];


(* Tr(T^a T^b T^c T^d) *)
sunTraceEvaluate[rest_. FeynCalc`Package`holdDOTColor[SUNT[a_SUNIndex], SUNT[b_SUNIndex], SUNT[c_SUNIndex], SUNT[d_SUNIndex]]] :=
	Block[{e},
		e = SUNIndex[FCGV[ToString[Unique["sun"]]]];
		rest (
		1/4/SUNN(SUNDelta[a, b] SUNDelta[c, d] - SUNDelta[a, c] SUNDelta[b, d] + SUNDelta[a, d] SUNDelta[b, c]) +
		1/8(SUND[a,b,e] SUND[c,d,e] - SUND[a,c,e] SUND[b,d,e] + SUND[a,d,e] SUND[b,c,e]) +
		I/8(SUND[a,d,e] SUNF[b,c,e] - SUNF[a,d,e] SUND[b,c,e])
		)
	]/; FreeQ[rest,SUNT];

(* Tr(T^a T^b T^c T^d ...) *)
sunTraceEvaluate[rest_. FeynCalc`Package`holdDOTColor[SUNT[a_SUNIndex] , SUNT[b_SUNIndex] , SUNT[c_SUNIndex] , SUNT[d_SUNIndex] , (more__SUNT)]]:=
	Block[{f},
			f = SUNIndex[FCGV[ToString[Unique["sun"]]]];
			rest (SUNDelta[a,b]/2/SUNN sunTraceEvaluate[FeynCalc`Package`holdDOTColor[SUNT[c],SUNT[d],more]] +
				1/2 SUND[a,b,f] sunTraceEvaluate[FeynCalc`Package`holdDOTColor[SUNT[f],SUNT[c],SUNT[d],more]] +
				I/2 SUNF[a,b,f] sunTraceEvaluate[FeynCalc`Package`holdDOTColor[SUNT[f],SUNT[c],SUNT[d],more]])
		]/; FreeQ[rest,SUNT];


FCPrint[1,"SUNTrace.m loaded."];
End[]
