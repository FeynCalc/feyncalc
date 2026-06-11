(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynRule															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Derivation of feynman rules via functional differentiation	*)

(* ------------------------------------------------------------------------ *)


FeynRule::usage =
"FeynRule[lag, {fields}] derives the Feynman rule corresponding to the field
configuration fields of the Lagrangian lag.

FeynRule does not calculate propagator Feynman rules.

FeynRule is not very versatile and was primarily developed for QCD
calculations. It is often more useful when dealing with bosonic fields than
with fermions. If you need a more powerful and universal solution for deriving
Feynman rules, have a look at the standalone Mathematica Package FeynRules
(not related to FeynCalc).";

InitialFunction::usage =
"InitialFunction is an option of FeynRule the setting of which is applied to
the first argument of FeynRule before anything else.";


FeynRule::failmsg =
"Error! FeynRule has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";


(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynRule`Private`"]

Options[FeynRule] = {
	"MomentumConservation"	-> False,
	Anti5 					-> -Infinity,
	Collecting				-> True,
	Contract 				-> False,
	FCE						-> False,
	FCI						-> False,
	FCFactorOut				-> 1,
	FCPartialD 				-> RightPartialD,
	FCVerbose 				-> False,
	Factoring				-> {Factor2, 5000},
	FinalSubstitutions		-> {},
	InitialFunction 		-> Identity,
	LorentzIndexNames		-> {},
	SUNIndexNames			-> {},
	SUNFIndexNames			-> {},
	TimeConstrained 		-> 3
};


FeynRule[a_,b_ /; Head[b] =!=Rule && Head[b]=!= List, c___,
			d_ /; Head[d] =!= Rule && Head[d] =!= List, e___Rule] :=
	FeynRule[a, {b,c,d}, e];

FeynRule[expr_, fieldsRaw_List/;!OptionQ[fieldsRaw], OptionsPattern[]] :=
	Block[{	fieldMultiply, null1, null2, ex, lfili, qli, result, fields,
			tfields, vert, qfi, qqq, anti5, partiald, fcVerbose, plist,
			puref, optVerbose, time, optLorentzIndexNames, optSUNIndexNames,
			optSUNFIndexNames, optFCCanonicalizeDummyIndices=False, optCollecting},

			anti5    	= OptionValue[Anti5];
			partiald 	= OptionValue[FCPartialD];
			fcVerbose	= OptionValue[FCVerbose];

			optCollecting			= OptionValue[Collecting];
			optLorentzIndexNames 	= OptionValue[LorentzIndexNames];
			optSUNIndexNames 		= OptionValue[SUNIndexNames];
			optSUNFIndexNames		= OptionValue[SUNFIndexNames];


			If[ OptionValue[LorentzIndexNames]=!={} || OptionValue[SUNIndexNames]=!={}|| OptionValue[SUNFIndexNames]=!={},
				optFCCanonicalizeDummyIndices = True
			];

			If [OptionValue[FCVerbose]===False,
				optVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					optVerbose=OptionValue[FCVerbose]
				];
			];

			If[ OptionValue[FCI],
				ex = expr,
				ex = FCI[expr]
			];

			FCPrint[3, "FeynRule: Entering.", FCDoControl->optVerbose];
			FCPrint[3, "FeynRule: Entering with ", ex, FCDoControl->optVerbose];

			ex = OptionValue[InitialFunction][ex];

			ex = ExpandAll[ex];

			If[	!FreeQ2[{ex,fieldsRaw}, FeynCalc`Package`NRStuff],
				Message[FeynCalc::nrfail];
				Abort[]
			];

			fields = Map[ ( QuantumField[___, #, Pattern @@ {Unique["dm"], ___}][___])&, #[[0, 1]]& /@ fieldsRaw];

			FCPrint[2, "FeynRule: Fields: ", fields, FCDoControl->optVerbose];

			If[	!FreeQ[ex,FieldStrength],
				ex = ex /. FieldStrength[a__] :> FieldStrength[a, Explicit->True];
			];
			ex = ex /. CovariantD[args__] :> CovariantD[args, FCPartialD->partiald,Explicit->True];

			If[	!FreeQ[ex,CovariantD],
				Message[FeynRule::failmsg,"Failed to eliminate all occurrences of CovariantD"];
				Abort[]
			];

			time=AbsoluteTime[];
			FCPrint[1, "FeynRule: Applying DotSimplify.", FCDoControl->optVerbose];
			ex = DotSimplify[ex];
			FCPrint[1,"FeynRule: DotSimplify done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			FCPrint[3, "FeynRule: After DotSimplify: ", ex, FCDoControl->optVerbose];

			If[ !FreeQ2[ex, {SUNDelta,SUNFDelta}],
				time=AbsoluteTime[];
				FCPrint[1, "FeynRule: Applying color delta contractions.", FCDoControl->optVerbose];
				ex = Expand2[ex, {SUNIndex,SUNFIndex,ExplicitSUNIndex,ExplicitSUNFIndex}];
				ex = ex /.{SUNDelta-> SUNDeltaContract,SUNFDelta-> SUNFDeltaContract}/. {SUNDeltaContract->SUNDelta, SUNFDeltaContract->SUNFDelta};
				FCPrint[1,"FeynRule: Color delta contractions done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
				FCPrint[3, "FeynRule: After color delta contractions: ", ex, FCDoControl->optVerbose];
			];

			time=AbsoluteTime[];
			FCPrint[1, "FeynRule: Applying ExpandPartialD.", FCDoControl->optVerbose];
			ex = ExpandPartialD[ex,FCI->True];
			FCPrint[1,"FeynRule: ExpandPartialD done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			FCPrint[3, "FeynRule: After ExpandPartialD: ", ex, FCDoControl->optVerbose];


			time=AbsoluteTime[];
			FCPrint[1, "FeynRule: Applying FCRenameDummyIndices.", FCDoControl->optVerbose];
			ex = FCRenameDummyIndices[ex,FCI->True,DotSimplify->True,Expanding->True];
			FCPrint[1,"FeynRule: FCRenameDummyIndices done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			FCPrint[3, "FeynRule: After FCRenameDummyIndices: ", ex, FCDoControl->optVerbose];

			time=AbsoluteTime[];
			FCPrint[1, "FeynRule: Applying Contract.", FCDoControl->optVerbose];
			ex = Contract[ex /. QuantumField -> (QuantumField[##][]&), FCI->True];
			FCPrint[1,"FeynRule: Contract done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			FCPrint[3, "FeynRule: After Contract: ", ex, FCDoControl->optVerbose];

			time=AbsoluteTime[];
			FCPrint[1, "FeynRule: Applying fieldMultiply.", FCDoControl->optVerbose];
			fieldMultiply /: (fieldMultiply[aa___][bb___] * fieldMultiply[xx___][yy___] ):=
				fieldMultiply[aa][bb]**fieldMultiply[xx][yy];
			ex = Expand2[Expand2[ex /. QuantumField -> fieldMultiply,QuantumField] /. fieldMultiply -> QuantumField,QuantumField];
			FCPrint[1,"FeynRule: fieldMultiply done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			FCPrint[3, "FeynRule: After fieldMultiply: ", ex, FCDoControl->optVerbose];


			(* Now we select vertices of suitable length. For that aim the expression must be expanded in QuantumField beforehand.*)

			vert = Select[ex + null1+null2, (Length[Position[#, QuantumField]]===Length[fields]) &];
			FCPrint[3, "FeynRule: Raw selected vertices: ", vert, FCDoControl->optVerbose];

			(* Keep only vertices that depend on the fields w.r.t which we are differentiating .*)
			tfields = fields;
			vert = vert + null1 + null2;
			While[(Length[tfields] > 0) && (Head[vert] === Plus),
					vert = Select[vert, !FreeQ[#, First[tfields]]&];
					tfields = Rest[tfields];
				];
			FCPrint[3, "FeynRule: Prefinal selected vertices: ", vert, FCDoControl->optVerbose];

			(* There might be still a sum ... *)
			If[ Head[vert] === Plus,

				qfi[___FCPartialD, fiii_, ___LorentzIndex, ___SUNIndex|___ExplicitSUNIndex][___] :=
					qqq[fiii];

				qfi[___FCPartialD, fiii_, ___Momentum, ___SUNIndex|___ExplicitSUNIndex][___] :=
					qqq[fiii];

				qfi[___BlankNullSequence, fiii_, ___Pattern][___] :=
					qqq[fiii];

				puref = (Sort[Select[Variables[# /. QuantumField -> qfi /. DOT -> Times /. NonCommutativeMultiply -> Times]//Flatten//Union,Head[#]===qqq&]] ===
					Sort[Variables[fields /. QuantumField -> qfi]])&;
				vert = Select[vert, puref];
			];

			FCPrint[3, "FeynRule: Final selected vertices: ", vert, FCDoControl->optVerbose];

			If[	vert===0,
				Return[0]
			];

			vert = vert /. NonCommutativeMultiply -> Times;

			If[	!FreeQ[vert, DOT],
				time=AbsoluteTime[];
				FCPrint[1, "FeynRule: Applying noncommutative simplifications.", FCDoControl->optVerbose];
				If[ FreeQ[vert, DiracGamma],
					vert = DotSimplify[vert, Expanding -> False],
					vert = DiracTrick[vert,FCI->True]
				];
				FCPrint[1,"FeynRule: Noncommutative simplifications done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			];

			If[ !FreeQ2[result, {SUNDelta,SUNFDelta}],
				time=AbsoluteTime[];
				FCPrint[1, "FeynRule: Applying SUNSimplify.", FCDoControl->optVerbose];
				vert = SUNSimplify[vert,FCI->True];
				FCPrint[1,"FeynRule: SUNSimplify done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
				FCPrint[3, "FeynRule: After SUNSimplify: ", vert, FCDoControl->optVerbose];
			];

			time=AbsoluteTime[];
			FCPrint[1, "FeynRule: Applying FunctionalD.", FCDoControl->optVerbose];
			result = DotSimplify[Map[FunctionalD[#, fieldsRaw]&,vert+null1+null2], Expanding -> False];
			FCPrint[1,"FeynRule: FunctionalD done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			FCPrint[3, "FeynRule: After FunctionalD: ", result, FCDoControl->optVerbose];

			If[ !FreeQ2[result, {SUNDelta,SUNFDelta}],
				time=AbsoluteTime[];
				FCPrint[1, "FeynRule: Applying color delta contractions.", FCDoControl->optVerbose];
				result = Expand2[result, {SUNIndex,SUNFIndex,ExplicitSUNIndex,ExplicitSUNFIndex}];
				result = result /.{SUNDelta-> SUNDeltaContract,SUNFDelta-> SUNFDeltaContract}/. {SUNDeltaContract->SUNDelta, SUNFDeltaContract->SUNFDelta};
				FCPrint[1,"FeynRule: Color delta contractions done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
				FCPrint[3, "FeynRule: After color delta contractions: ", result, FCDoControl->optVerbose];
			];

			FCPrint[2, "FeynRule: Number of terms: ", Length[result], FCDoControl->optVerbose];

			If[ !FreeQ2[result, {LorentzIndex}],
				FCPrint[1, "FeynRule: Applying PairContract.", FCDoControl->optVerbose];
				result = result /. Pair -> PairContract /. Pair -> PairContract /. PairContract -> Pair;
				FCPrint[3, "FeynRule: After PairContract: ", result, FCDoControl->optVerbose];
			];

			If[ !FreeQ[result, Eps],
				FCPrint[1, "FeynRule: Applying EpsEvaluate.", FCDoControl->optVerbose];
				result = EpsEvaluate[result,FCI->True];
				FCPrint[3, "FeynRule: After EpsEvaluate ", result, FCDoControl->optVerbose];
			];

			If[	!FreeQ[result, DOT],
				time=AbsoluteTime[];
				FCPrint[1, "FeynRule: Applying noncommutative simplifications.", FCDoControl->optVerbose];
				If[ FreeQ[result, DiracGamma],
					result = DotSimplify[result, Expanding -> False],
					result = DiracTrick[result,FCI->True]
				];
				FCPrint[1,"FeynRule: Noncommutative simplifications done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			];


			lfili  = Flatten[fieldsRaw /. QuantumField[__,el__LorentzIndex, ___][_]  -> {el}];
			If[ Union[Cases[result, LorentzIndex[__], Infinity]] =!= Sort[lfili],
				If[ OptionValue[Contract],
					result = Contract[result,FCI->True]//ExpandAll;
					FCPrint[1, "FeynRule: After another contraction ", result, FCDoControl->optVerbose];
				]
			];

			If[ !FreeQ[result, Eps],
				FCPrint[1, "FeynRule: Applying EpsEvaluate.", FCDoControl->optVerbose];
				result = EpsEvaluate[result,FCI->True];
				FCPrint[3, "FeynRule: After EpsEvaluate ", result, FCDoControl->optVerbose];
			];

			If[ !FreeQ[result, DOT],
				time=AbsoluteTime[];
				FCPrint[1, "FeynRule: Applying DotSimplify.", FCDoControl->optVerbose];
				result = DotSimplify[result, Expanding -> False];
				FCPrint[1,"FeynRule: DotSimplify done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			];

			FCPrint[2, "FeynRule: Number of terms: ", Length[result], FCDoControl->optVerbose];


			If[ !FreeQ2[result, {SUNIndex,SUNFIndex}],
				time=AbsoluteTime[];
				FCPrint[1, "FeynRule: Applying SUNSimplify.", FCDoControl->optVerbose];
				result = SUNSimplify[result,Explicit->False];
				FCPrint[1,"FeynRule: SUNSimplify done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			];

			(* in case the incoming momenta are a sum *)
			If[ !FreeQ[fieldsRaw, Plus],
				time=AbsoluteTime[];
				FCPrint[1, "FeynRule: Applying ExpandScalarProduct.", FCDoControl->optVerbose];
				result = ExpandScalarProduct[result];
				FCPrint[1,"FeynRule: ExpandScalarProduct done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			];

			If[ !FreeQ[result, DiracGamma[5]],
				time=AbsoluteTime[];
				FCPrint[1, "FeynRule: Applying Anti5.", FCDoControl->optVerbose];
				result = Anti5[result, anti5];
				FCPrint[1,"FeynRule: Anti5 done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			];

			(* Momentum conservation, eliminate first momentum*)
			If[OptionValue["MomentumConservation"],
				plist =  fieldsRaw /. QuantumField[__][pe_] -> pe /. Momentum -> Identity;
				result = ExpandScalarProduct[result /. plist[[1]] :> (-(Plus @@ Rest[plist]))];
			];

			result = Expand[I result] /. OptionValue[FinalSubstitutions];

			If[	optFCCanonicalizeDummyIndices,
				time=AbsoluteTime[];
				FCPrint[1, "FeynRule: Applying FCCanonicalizeDummyIndices.", FCDoControl->optVerbose];
				result = FCCanonicalizeDummyIndices[result, FCI->True, LorentzIndexNames->optLorentzIndexNames,
					SUNIndexNames->optSUNIndexNames, SUNFIndexNames->optSUNFIndexNames];
				FCPrint[1,"FeynRule: FCCanonicalizeDummyIndices done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			];

			If[	OptionValue[Collecting]=!=False,
				time=AbsoluteTime[];
				FCPrint[1, "FeynRule: Applying Collect2.", FCDoControl->optVerbose];
				If[	TrueQ[Head[optCollecting]===List],
					result = Collect2[result, optCollecting, Factoring->OptionValue[Factoring],
						TimeConstrained->OptionValue[TimeConstrained], FCFactorOut->OptionValue[FCFactorOut]],
					result = Collect2[result, {LorentzIndex,SUNIndex,SUNFIndex}, Factoring->OptionValue[Factoring],
						TimeConstrained->OptionValue[TimeConstrained], FCFactorOut->OptionValue[FCFactorOut]];
				];
				FCPrint[1,"FeynRule: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			];


			If[OptionValue[FCE],
				result = FCE[result];
			];


			FCPrint[1, "FeynRule: Preliminary result ", result, FCDoControl->optVerbose];

			result
		]

FCPrint[1, "FeynRule.m loaded."];
End[]
